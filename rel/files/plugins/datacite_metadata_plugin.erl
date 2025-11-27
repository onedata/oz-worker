%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Implementation of the onezone_plugin_behaviour and the handle_metadata_plugin_behaviour
%%% for handling DataCite metadata format ("datacite" in OAI-PMH).
%%%
%%% NOTE: two variants of DataCite schema are commonly used: "oai_datacite" and "datacite".
%%% The main difference is that the former includes a wrapper element with additional
%%% information, as specified here: https://support.datacite.org/docs/oai-pmh-schema-documentation
%%% The official DataCite OAI-PMH endpoint supports both schemas:
%%% * https://oai.datacite.org/oai?verb=GetRecord&metadataPrefix=oai_datacite&identifier=doi:10.5061/dryad.7q0nq
%%% * https://oai.datacite.org/oai?verb=GetRecord&metadataPrefix=datacite&identifier=doi:10.5061/dryad.7q0nq
%%%
%%% This plugin covers "datacite"; @see oai_datacite_metadata_plugin for the other one.
%%%
%%% @see handle_metadata_plugin_behaviour for general information about metadata plugins.
%%%
%%% Metadata revision step:
%%%   * remove preexisting identifier element(s) (to be overwritten in the next step)
%%%   * add an alternateIdentifier element with the value equal to the public share URL
%%%
%%% Public handle insertion step:
%%%   * insert an identifier element (serving as primary) with the value equal to the public handle
%%%
%%% Adaptation for OAI-PMH step:
%%%   * none
%%% @end
%%%-------------------------------------------------------------------
-module(datacite_metadata_plugin).
-author("Lukasz Opiola").

-behavior(onezone_plugin_behaviour).
-behaviour(handle_metadata_plugin_behaviour).

-include("http/handlers/oai.hrl").


%% onezone_plugin_behaviour callbacks
-export([type/0]).

%% handle_metadata_plugin_behaviour callbacks
-export([metadata_prefix/0, schema_URL/0, main_namespace/0]).
-export([revise_for_publication/3, insert_public_handle/2, adapt_for_oai_pmh/1]).
-export([encode_xml/1]).
-export([validation_examples/0]).

%% API related to validation examples
-export([invalid_examples/0]).
-export([correct_input_raw_xml/0]).
-export([correct_exp_revised_metadata_generator/2]).
-export([correct_exp_final_metadata_generator/3]).
-export([correct_exp_oai_pmh_metadata_generator/3]).


-define(identifier_element(Type, Value), #xmlElement{
    name = identifier,
    attributes = [#xmlAttribute{name = identifierType, value = Type}],
    content = [#xmlText{value = Value}]
}).

-define(alternate_url_identifier_element(Value), #xmlElement{
    name = alternateIdentifier,
    attributes = [#xmlAttribute{name = alternateIdentifierType, value = "URL"}],
    content = [#xmlText{value = Value}]
}).

-define(DATACENTRE_SYMBOL, oz_worker:get_env(datacite_datacentre_symbol, oz_worker:get_domain())).


%%%===================================================================
%%% onezone_plugin_behaviour callbacks
%%%===================================================================


%% @doc {@link onezone_plugin_behaviour} callback type/0
-spec type() -> handle_metadata_plugin.
type() ->
    handle_metadata_plugin.


%%%===================================================================
%%% handle_metadata_plugin_behaviour callbacks
%%%===================================================================


%% @doc {@link handle_metadata_plugin_behaviour} callback metadata_prefix/0
-spec metadata_prefix() -> binary().
metadata_prefix() ->
    ?OAI_DATACITE_METADATA_PREFIX.


%% @doc {@link handle_metadata_plugin_behaviour} callback schema_URL/0
-spec schema_URL() -> binary().
schema_URL() ->
    <<"http://schema.datacite.org/meta/kernel-4/metadata.xsd">>.


%% @doc {@link handle_metadata_plugin_behaviour} callback main_namespace/0
-spec main_namespace() -> {atom(), binary()}.
main_namespace() ->
    {'xmlns', <<"http://datacite.org/schema/kernel-4">>}.


%% @doc {@link handle_metadata_plugin_behaviour} callback revise_for_publication/3
-spec revise_for_publication(od_handle:parsed_metadata(), od_share:id(), od_share:record()) ->
    {ok, od_handle:parsed_metadata()} | error.
revise_for_publication(#xmlElement{name = resource} = ResourceXml0, ShareId, _ShareRecord) ->
    % TODO VFS-12975 improve the behaviour based on options if the pid should be reused
    % and possibly change the primary identifier to alternate one if needed
    ResourceXml1 = remove_primary_identifier(ResourceXml0),
    ResourceXml2 = ensure_alternate_url_identifier(
        binary_to_list(od_share:build_public_url(ShareId)),
        ResourceXml1
    ),
    {ok, ResourceXml2};

revise_for_publication(_InvalidXml, _ShareId, _ShareRecord) ->
    error.


%% @doc {@link handle_metadata_plugin_behaviour} callback insert_public_handle/1
-spec insert_public_handle(od_handle:parsed_metadata(), od_handle:public_handle()) ->
    od_handle:parsed_metadata().
insert_public_handle(#xmlElement{name = resource} = ResourceXml, PublicHandle) ->
    % TODO VFS-12975 this has to reworked for the internal handle service type so as not to
    % duplicate primary and alternate identifiers
    case PublicHandle of
        ?DOI_IDENTIFIER(DoiHandle) ->
            insert_primary_identifier("DOI", binary_to_list(DoiHandle), ResourceXml);
        _ ->
            insert_primary_identifier("URL", binary_to_list(PublicHandle), ResourceXml)
    end.


%% @private
-spec insert_primary_identifier(string(), string(), od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
insert_primary_identifier(Type, Value, #xmlElement{name = resource, content = Content} = ResourceXml) ->
    case ?find_matching_element(#xmlElement{name = identifier}, Content) of
        {ok, Found} ->
            ResourceXml#xmlElement{content = lists_utils:replace(
                Found,
                ?identifier_element(Type, Value),
                Content
            )};
        error ->
            ResourceXml#xmlElement{
                content = oai_xml:prepend_element_with_indent(4, ?identifier_element(Type, Value), Content)
            }
    end.


%% @private
-spec remove_primary_identifier(od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
remove_primary_identifier(#xmlElement{name = resource, content = Content} = ResourceXml) ->
    case ?find_matching_element(#xmlElement{name = identifier}, Content) of
        {ok, Found} ->
            NextElement = lists:nth(lists_utils:index_of(Found, Content) + 1, Content),
            ContentWithoutWhitespace = case NextElement of
                #xmlText{value = Text} ->
                    case re:run(Text, "^[\\s]*$", [{capture, none}]) of
                        match -> lists:delete(NextElement, Content);
                        _ -> Content
                    end;
                _ ->
                    Content
            end,
            % just in case, remove all identifier elements if there's more than one
            % (though it's technically not allowed by the schema, it won't hurt to safeguard this)
            remove_primary_identifier(
                ResourceXml#xmlElement{content = lists:delete(Found, ContentWithoutWhitespace)}
            );
        error ->
            ResourceXml
    end.


%% @private
-spec ensure_alternate_url_identifier(string(), od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
ensure_alternate_url_identifier(Value, #xmlElement{name = resource, content = Content} = ResourceXml) ->
    case ?find_matching_element(#xmlElement{name = alternateIdentifiers}, Content) of
        {ok, AlternateIdentifiersXml} ->
            ResourceXml#xmlElement{content = lists_utils:replace(
                AlternateIdentifiersXml,
                ensure_alternate_url_identifier(Value, AlternateIdentifiersXml),
                Content
            )};
        error ->
            ensure_alternate_url_identifier(Value, ResourceXml#xmlElement{
                content = oai_xml:prepend_element_with_indent(4, #xmlElement{
                    name = alternateIdentifiers,
                    content = []
                }, Content)
            })
    end;
ensure_alternate_url_identifier(Value, #xmlElement{name = alternateIdentifiers, content = Content} = AIXml) ->
    case ?find_matching_element(?alternate_url_identifier_element(Value), Content) of
        {ok, _} ->
            AIXml;
        error ->
            AIXml#xmlElement{
                content = oai_xml:prepend_element_with_indent(8, ?alternate_url_identifier_element(Value), Content)
            }
    end.


%% @doc {@link handle_metadata_plugin_behaviour} callback adapt_for_oai_pmh/1
-spec adapt_for_oai_pmh(od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
adapt_for_oai_pmh(#xmlElement{name = resource} = ResourceXml) ->
    ResourceXml.


%% @doc {@link handle_metadata_plugin_behaviour} callback encode_xml/1
-spec encode_xml(od_handle:parsed_metadata()) -> od_handle:raw_metadata().
encode_xml(Metadata) ->
    oai_xml:encode(Metadata).


%% @doc {@link handle_metadata_plugin_behaviour} callback validation_examples/0
-spec validation_examples() -> [handle_metadata_plugin_behaviour:validation_example()].
validation_examples() ->
    lists:flatten([
        % TODO VFS-7454 add better validation of the XML (schema)
        invalid_examples(),

        #handle_metadata_plugin_validation_example{
            input_raw_xml = correct_input_raw_xml(),
            input_qualifies_for_publication = true,
            exp_revised_metadata_generator = fun correct_exp_revised_metadata_generator/2,
            exp_final_metadata_generator = fun correct_exp_final_metadata_generator/3,
            exp_oai_pmh_metadata_generator = fun correct_exp_oai_pmh_metadata_generator/3
        }
    ]).


invalid_examples() ->
    [
        #handle_metadata_plugin_validation_example{
            input_raw_xml = <<
                "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
                "<valid-xml>but no resource tag</valid-xml>"
            >>,
            input_qualifies_for_publication = false
        },

        #handle_metadata_plugin_validation_example{
            input_raw_xml = <<
                "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
                "<creators>\n"
                "   <creator>\n"
                "       <creatorName>John Doe</creatorName>\n"
                "   </creator>\n"
                "</creators>"
            >>,
            input_qualifies_for_publication = false
        }
    ].


%% @private
-spec correct_input_raw_xml() -> binary().
correct_input_raw_xml() ->
    <<
        "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
        "<resource xmlns=\"http://datacite.org/schema/kernel-4\" xsi:schemaLocation=\"http://datacite.org/schema/kernel-4 http://schema.datacite.org/meta/kernel-4.3/metadata.xsd\">\n"
        "    <identifier>preexisting-identifier-to-be-deleted</identifier>\n"
        "    <alternateIdentifiers>\n"
        "        <alternateIdentifier alternateIdentifierType=\"oai\">oai:example.com:1234567</alternateIdentifier>\n"
        "    </alternateIdentifiers>\n"
        "    <creators>\n"
        "        <creator>\n"
        "            <creatorName nameType=\"Personal\">John Doe</creatorName>\n"
        "            <familyName>Doe</familyName>\n"
        "            <affiliation>University X</affiliation>\n"
        "        </creator>\n"
        "    </creators>\n"
        "    <titles>\n"
        "        <title>Example dataset</title>\n"
        "    </titles>\n"
        "    <publisher>Onedata</publisher>\n"
        "    <publicationYear>2025</publicationYear>\n"
        "    <dates>\n"
        "        <date dateType=\"Issued\">2025-01-11</date>\n"
        "        <date dateType=\"Updated\">2025-01-12</date>\n"
        "    </dates>\n"
        "    <resourceType resourceTypeGeneral=\"Software\"/>\n"
        "    <identifier>doi:10.5061/superflouous-identifier</identifier>\n"
        "    <descriptions>\n"
        "        <description descriptionType=\"Abstract\">This is an example dataset</description>\n"
        "    </descriptions>\n"
        "</resource>"
    >>.


%% @private
-spec correct_exp_revised_metadata_generator(od_share:id(), od_share:record()) -> binary().
correct_exp_revised_metadata_generator(ShareId, _ShareRecord) ->
    <<
        "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
        "<resource xmlns=\"http://datacite.org/schema/kernel-4\" xsi:schemaLocation=\"http://datacite.org/schema/kernel-4 http://schema.datacite.org/meta/kernel-4.3/metadata.xsd\">\n"
        "    <alternateIdentifiers>\n"
        "        <alternateIdentifier alternateIdentifierType=\"URL\">", (od_share:build_public_url(ShareId))/binary, "</alternateIdentifier>\n"
    "        <alternateIdentifier alternateIdentifierType=\"oai\">oai:example.com:1234567</alternateIdentifier>\n"
    "    </alternateIdentifiers>\n"
    "    <creators>\n"
    "        <creator>\n"
    "            <creatorName nameType=\"Personal\">John Doe</creatorName>\n"
    "            <familyName>Doe</familyName>\n"
    "            <affiliation>University X</affiliation>\n"
    "        </creator>\n"
    "    </creators>\n"
    "    <titles>\n"
    "        <title>Example dataset</title>\n"
    "    </titles>\n"
    "    <publisher>Onedata</publisher>\n"
    "    <publicationYear>2025</publicationYear>\n"
    "    <dates>\n"
    "        <date dateType=\"Issued\">2025-01-11</date>\n"
    "        <date dateType=\"Updated\">2025-01-12</date>\n"
    "    </dates>\n"
    "    <resourceType resourceTypeGeneral=\"Software\"/>\n"
    "    <descriptions>\n"
    "        <description descriptionType=\"Abstract\">This is an example dataset</description>\n"
    "    </descriptions>\n"
    "</resource>"
    >>.


%% @private
-spec correct_exp_final_metadata_generator(od_share:id(), od_share:record(), od_handle:public_handle()) -> binary().
correct_exp_final_metadata_generator(ShareId, _ShareRecord, PublicHandle) ->
    <<
        "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
        "<resource xmlns=\"http://datacite.org/schema/kernel-4\" xsi:schemaLocation=\"http://datacite.org/schema/kernel-4 http://schema.datacite.org/meta/kernel-4.3/metadata.xsd\">\n",
        "    ", (exp_primary_identifier(PublicHandle))/binary, "\n"
    "    <alternateIdentifiers>\n"
    "        <alternateIdentifier alternateIdentifierType=\"URL\">", (od_share:build_public_url(ShareId))/binary, "</alternateIdentifier>\n"
    "        <alternateIdentifier alternateIdentifierType=\"oai\">oai:example.com:1234567</alternateIdentifier>\n"
    "    </alternateIdentifiers>\n"
    "    <creators>\n"
    "        <creator>\n"
    "            <creatorName nameType=\"Personal\">John Doe</creatorName>\n"
    "            <familyName>Doe</familyName>\n"
    "            <affiliation>University X</affiliation>\n"
    "        </creator>\n"
    "    </creators>\n"
    "    <titles>\n"
    "        <title>Example dataset</title>\n"
    "    </titles>\n"
    "    <publisher>Onedata</publisher>\n"
    "    <publicationYear>2025</publicationYear>\n"
    "    <dates>\n"
    "        <date dateType=\"Issued\">2025-01-11</date>\n"
    "        <date dateType=\"Updated\">2025-01-12</date>\n"
    "    </dates>\n"
    "    <resourceType resourceTypeGeneral=\"Software\"/>\n"
    "    <descriptions>\n"
    "        <description descriptionType=\"Abstract\">This is an example dataset</description>\n"
    "    </descriptions>\n"
    "</resource>"
    >>.


%% @private
-spec correct_exp_oai_pmh_metadata_generator(od_share:id(), od_share:record(), od_handle:public_handle()) -> binary().
correct_exp_oai_pmh_metadata_generator(ShareId, _ShareRecord, PublicHandle) ->
    <<
        "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
        "<resource xmlns=\"http://datacite.org/schema/kernel-4\" xsi:schemaLocation=\"http://datacite.org/schema/kernel-4 http://schema.datacite.org/meta/kernel-4.3/metadata.xsd\">\n",
        "    ", (exp_primary_identifier(PublicHandle))/binary, "\n"
    "    <alternateIdentifiers>\n"
    "        <alternateIdentifier alternateIdentifierType=\"URL\">", (od_share:build_public_url(ShareId))/binary, "</alternateIdentifier>\n"
    "        <alternateIdentifier alternateIdentifierType=\"oai\">oai:example.com:1234567</alternateIdentifier>\n"
    "    </alternateIdentifiers>\n"
    "    <creators>\n"
    "        <creator>\n"
    "            <creatorName nameType=\"Personal\">John Doe</creatorName>\n"
    "            <familyName>Doe</familyName>\n"
    "            <affiliation>University X</affiliation>\n"
    "        </creator>\n"
    "    </creators>\n"
    "    <titles>\n"
    "        <title>Example dataset</title>\n"
    "    </titles>\n"
    "    <publisher>Onedata</publisher>\n"
    "    <publicationYear>2025</publicationYear>\n"
    "    <dates>\n"
    "        <date dateType=\"Issued\">2025-01-11</date>\n"
    "        <date dateType=\"Updated\">2025-01-12</date>\n"
    "    </dates>\n"
    "    <resourceType resourceTypeGeneral=\"Software\"/>\n"
    "    <descriptions>\n"
    "        <description descriptionType=\"Abstract\">This is an example dataset</description>\n"
    "    </descriptions>\n"
    "</resource>"
    >>.


%% @private
-spec exp_primary_identifier(od_handle:public_handle()) -> binary().
exp_primary_identifier(<<"doi:", DoiHandle/binary>>) ->
    <<"<identifier identifierType=\"DOI\">", DoiHandle/binary, "</identifier>">>;
exp_primary_identifier(PublicHandle) ->
    <<"<identifier identifierType=\"URL\">", PublicHandle/binary, "</identifier>">>.
