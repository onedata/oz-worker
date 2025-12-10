%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Implementation of the onezone_plugin_behaviour and the handle_metadata_plugin_behaviour
%%% for handling DataCite metadata schema ("oai_datacite").
%%% @see handle_metadata_plugin_behaviour for general information about metadata plugins.
%%%
%%% fixme
%%% @end
%%%-------------------------------------------------------------------
-module(oai_openaire_metadata_plugin).
-author("Lukasz Opiola").

-behavior(onezone_plugin_behaviour).
-behaviour(handle_metadata_plugin_behaviour).

-include("http/public_data/oai.hrl").


%% onezone_plugin_behaviour callbacks
-export([type/0]).

%% handle_metadata_plugin_behaviour callbacks
-export([metadata_schema/0, supported_oai_pmh_metadata_prefixes/0, schema_URL/1, main_namespace/1]).
-export([revise_for_publication/3, insert_public_handle/2, adapt_for_oai_pmh/2]).
-export([encode_xml/1]).
-export([validation_examples/0]).


-define(identifier_element(Type, Value), #xmlElement{
    name = 'datacite:identifier',
    attributes = [#xmlAttribute{name = 'datacite:identifierType', value = Type}],
    content = [#xmlText{value = Value}]
}).

-define(alternate_url_identifier_element(Value), #xmlElement{
    name = 'datacite:alternateIdentifier',
    attributes = [#xmlAttribute{name = 'datacite:alternateIdentifierType', value = "URL"}],
    content = [#xmlText{value = Value}]
}).


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


%% @doc {@link handle_metadata_plugin_behaviour} callback metadata_schema/0
-spec metadata_schema() -> od_handle:metadata_schema().
metadata_schema() ->
    ?OAI_OPENAIRE_METADATA_PREFIX.


%% @doc {@link handle_metadata_plugin_behaviour} callback supported_oai_pmh_metadata_prefixes/0
-spec supported_oai_pmh_metadata_prefixes() -> od_handle:metadata_schema().
supported_oai_pmh_metadata_prefixes() ->
    [?OAI_OPENAIRE_METADATA_PREFIX].


%% @doc {@link handle_metadata_plugin_behaviour} callback schema_URL/1
-spec schema_URL(oai_metadata:prefix()) -> binary().
schema_URL(?OAI_OPENAIRE_METADATA_PREFIX) ->
    <<"http://schema.datacite.org/fixme">>.


%% @doc {@link handle_metadata_plugin_behaviour} callback main_namespace/1
-spec main_namespace(oai_metadata:prefix()) -> {atom(), binary()}.
main_namespace(?OAI_OPENAIRE_METADATA_PREFIX) ->
    {'xmlns', <<"http://schema.datacite.org/fixme">>}.


%% @doc {@link handle_metadata_plugin_behaviour} callback revise_for_publication/3
-spec revise_for_publication(od_handle:parsed_metadata(), od_share:id(), od_share:record()) ->
    {ok, od_handle:parsed_metadata()} | error.
revise_for_publication(#xmlElement{name = 'oaire:resource'} = ResourceXml0, ShareId, _ShareRecord) ->
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
insert_public_handle(#xmlElement{name = 'oaire:resource'} = ResourceXml, PublicHandle) ->
    % TODO VFS-12975 this has to be reworked for the internal handle service type so as not to
    % duplicate primary and alternate identifiers
    case PublicHandle of
        ?DOI_IDENTIFIER(DoiHandle) ->
            insert_primary_identifier("DOI", binary_to_list(DoiHandle), ResourceXml);
        <<"http://hdl.handle.net/", _/binary>> ->
            % TODO VFS-12975 improve this heuristic and allow provision of identifier type client-side
            insert_primary_identifier("Handle", binary_to_list(PublicHandle), ResourceXml);
        _ ->
            insert_primary_identifier("URL", binary_to_list(PublicHandle), ResourceXml)
    end.


%% @private
-spec insert_primary_identifier(string(), string(), od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
insert_primary_identifier(Type, Value, #xmlElement{name = 'oaire:resource', content = Content} = ResourceXml) ->
    case ?find_matching_element(#xmlElement{name = 'datacite:identifier'}, Content) of
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
remove_primary_identifier(#xmlElement{name = 'oaire:resource', content = Content} = ResourceXml) ->
    case ?find_matching_element(#xmlElement{name = 'datacite:identifier'}, Content) of
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
ensure_alternate_url_identifier(Value, #xmlElement{name = 'oaire:resource', content = Content} = ResourceXml) ->
    case ?find_matching_element(#xmlElement{name = 'datacite:alternateIdentifiers'}, Content) of
        {ok, AlternateIdentifiersXml} ->
            ResourceXml#xmlElement{content = lists_utils:replace(
                AlternateIdentifiersXml,
                ensure_alternate_url_identifier(Value, AlternateIdentifiersXml),
                Content
            )};
        error ->
            ensure_alternate_url_identifier(Value, ResourceXml#xmlElement{
                content = oai_xml:prepend_element_with_indent(4, #xmlElement{
                    name = 'datacite:alternateIdentifiers',
                    content = []
                }, Content)
            })
    end;
ensure_alternate_url_identifier(Value, #xmlElement{name = 'datacite:alternateIdentifiers', content = Content} = AIXml) ->
    case ?find_matching_element(?alternate_url_identifier_element(Value), Content) of
        {ok, _} ->
            AIXml;
        error ->
            AIXml#xmlElement{
                content = oai_xml:prepend_element_with_indent(8, ?alternate_url_identifier_element(Value), Content)
            }
    end.


%% @doc {@link handle_metadata_plugin_behaviour} callback adapt_for_oai_pmh/2
-spec adapt_for_oai_pmh(oai_metadata:prefix(), od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
adapt_for_oai_pmh(?OAI_OPENAIRE_METADATA_PREFIX, #xmlElement{name = 'oaire:resource'} = ResourceXml) ->
    ResourceXml.


%% @doc {@link handle_metadata_plugin_behaviour} callback encode_xml/1
-spec encode_xml(od_handle:parsed_metadata()) -> od_handle:raw_metadata().
encode_xml(Metadata) ->
    oai_xml:encode(Metadata).


%% @doc {@link handle_metadata_plugin_behaviour} callback validation_examples/0
-spec validation_examples() -> [handle_metadata_plugin_behaviour:validation_example()].
validation_examples() -> [
    % TODO VFS-7454 add better validation of the XML (schema)
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
            "<datacite:creators>\n"
            "   <datacite:creator>\n"
            "       <datacite:creatorName>Jane Doe</datacite:creatorName>\n"
            "   </datacite:creator>\n"
            "</datacite:creators>"
        >>,
        input_qualifies_for_publication = false
    },

    #handle_metadata_plugin_validation_example{
        input_raw_xml = <<
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            "<oaire:resource xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
            " xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
            " xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
            " xmlns:datacite=\"http://datacite.org/schema/kernel-4\""
            " xmlns:vc=\"http://www.w3.org/2007/XMLSchema-versioning\""
            " xmlns:oaire=\"http://namespace.openaire.eu/schema/oaire/\""
            " xsi:schemaLocation=\"http://namespace.openaire.eu/schema/oaire/ https://www.openaire.eu/schema/repo-lit/4.0/openaire.xsd\">\n"
            "    <datacite:identifier>preexisting-identifier-to-be-deleted</datacite:identifier>\n"
            "    <datacite:alternateIdentifiers>\n"
            "        <datacite:alternateIdentifier datacite:alternateIdentifierType=\"oai\">oai:example.com:1234567</datacite:alternateIdentifier>\n"
            "    </datacite:alternateIdentifiers>\n"
            "    <datacite:titles>\n"
            "        <datacite:title>${title}</datacite:title>\n"
            "    </datacite:titles>\n"
            "    <datacite:creators>\n"
            "        <datacite:creator>\n"
            "            <datacite:creatorName>${creator}</datacite:creatorName>\n"
            "        </datacite:creator>\n"
            "    </datacite:creators>\n"
            "    <dc:language>eng</dc:language>\n"
            "    <datacite:dates>\n"
            "        <datacite:date dateType=\"Issued\">${year}</datacite:date>\n"
            "    </datacite:dates>\n"
            "    <oaire:resourceType resourceTypeGeneral=\"literature\" uri=\"http://purl.org/coar/resource_type/c_93fc\">report</oaire:resourceType>\n"
            "    <datacite:rights rightsURI=\"http://purl.org/coar/access_right/c_abf2\">open access</datacite:rights>\n"
            "</oaire:resource>"
        >>,
        input_qualifies_for_publication = true,
        exp_revised_metadata_generator = fun(ShareId, _ShareRecord) ->
            <<
                "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
                "<oaire:resource xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
                " xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
                " xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
                " xmlns:datacite=\"http://datacite.org/schema/kernel-4\""
                " xmlns:vc=\"http://www.w3.org/2007/XMLSchema-versioning\""
                " xmlns:oaire=\"http://namespace.openaire.eu/schema/oaire/\""
                " xsi:schemaLocation=\"http://namespace.openaire.eu/schema/oaire/ https://www.openaire.eu/schema/repo-lit/4.0/openaire.xsd\">\n"
                "    <datacite:alternateIdentifiers>\n"
                "        <datacite:alternateIdentifier datacite:alternateIdentifierType=\"URL\">", (od_share:build_public_url(ShareId))/binary, "</datacite:alternateIdentifier>\n"
                "        <datacite:alternateIdentifier datacite:alternateIdentifierType=\"oai\">oai:example.com:1234567</datacite:alternateIdentifier>\n"
                "    </datacite:alternateIdentifiers>\n"
                "    <datacite:titles>\n"
                "        <datacite:title>${title}</datacite:title>\n"
                "    </datacite:titles>\n"
                "    <datacite:creators>\n"
                "        <datacite:creator>\n"
                "            <datacite:creatorName>${creator}</datacite:creatorName>\n"
                "        </datacite:creator>\n"
                "    </datacite:creators>\n"
                "    <dc:language>eng</dc:language>\n"
                "    <datacite:dates>\n"
                "        <datacite:date dateType=\"Issued\">${year}</datacite:date>\n"
                "    </datacite:dates>\n"
                "    <oaire:resourceType resourceTypeGeneral=\"literature\" uri=\"http://purl.org/coar/resource_type/c_93fc\">report</oaire:resourceType>\n"
                "    <datacite:rights rightsURI=\"http://purl.org/coar/access_right/c_abf2\">open access</datacite:rights>\n"
                "</oaire:resource>"
            >>
        end,
        exp_final_metadata_generator = fun(ShareId, _ShareRecord, PublicHandle) ->
            <<
                "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
                "<oaire:resource xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
                " xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
                " xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
                " xmlns:datacite=\"http://datacite.org/schema/kernel-4\""
                " xmlns:vc=\"http://www.w3.org/2007/XMLSchema-versioning\""
                " xmlns:oaire=\"http://namespace.openaire.eu/schema/oaire/\""
                " xsi:schemaLocation=\"http://namespace.openaire.eu/schema/oaire/ https://www.openaire.eu/schema/repo-lit/4.0/openaire.xsd\">\n"
                "    ", (exp_primary_identifier(PublicHandle))/binary, "\n"
                "    <datacite:alternateIdentifiers>\n"
                "        <datacite:alternateIdentifier datacite:alternateIdentifierType=\"URL\">", (od_share:build_public_url(ShareId))/binary, "</datacite:alternateIdentifier>\n"
                "        <datacite:alternateIdentifier datacite:alternateIdentifierType=\"oai\">oai:example.com:1234567</datacite:alternateIdentifier>\n"
                "    </datacite:alternateIdentifiers>\n"
                "    <datacite:titles>\n"
                "        <datacite:title>${title}</datacite:title>\n"
                "    </datacite:titles>\n"
                "    <datacite:creators>\n"
                "        <datacite:creator>\n"
                "            <datacite:creatorName>${creator}</datacite:creatorName>\n"
                "        </datacite:creator>\n"
                "    </datacite:creators>\n"
                "    <dc:language>eng</dc:language>\n"
                "    <datacite:dates>\n"
                "        <datacite:date dateType=\"Issued\">${year}</datacite:date>\n"
                "    </datacite:dates>\n"
                "    <oaire:resourceType resourceTypeGeneral=\"literature\" uri=\"http://purl.org/coar/resource_type/c_93fc\">report</oaire:resourceType>\n"
                "    <datacite:rights rightsURI=\"http://purl.org/coar/access_right/c_abf2\">open access</datacite:rights>\n"
                "</oaire:resource>"
            >>
        end,
        exp_oai_pmh_metadata_generator = fun(?OAI_OPENAIRE_METADATA_PREFIX, ShareId, _ShareRecord, PublicHandle) ->
                <<
                    "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
                    "<oaire:resource xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
                    " xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
                    " xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
                    " xmlns:datacite=\"http://datacite.org/schema/kernel-4\""
                    " xmlns:vc=\"http://www.w3.org/2007/XMLSchema-versioning\""
                    " xmlns:oaire=\"http://namespace.openaire.eu/schema/oaire/\""
                    " xsi:schemaLocation=\"http://namespace.openaire.eu/schema/oaire/ https://www.openaire.eu/schema/repo-lit/4.0/openaire.xsd\">\n"
                    "    ", (exp_primary_identifier(PublicHandle))/binary, "\n"
                    "    <datacite:alternateIdentifiers>\n"
                    "        <datacite:alternateIdentifier datacite:alternateIdentifierType=\"URL\">", (od_share:build_public_url(ShareId))/binary, "</datacite:alternateIdentifier>\n"
                    "        <datacite:alternateIdentifier datacite:alternateIdentifierType=\"oai\">oai:example.com:1234567</datacite:alternateIdentifier>\n"
                    "    </datacite:alternateIdentifiers>\n"
                    "    <datacite:titles>\n"
                    "        <datacite:title>${title}</datacite:title>\n"
                    "    </datacite:titles>\n"
                    "    <datacite:creators>\n"
                    "        <datacite:creator>\n"
                    "            <datacite:creatorName>${creator}</datacite:creatorName>\n"
                    "        </datacite:creator>\n"
                    "    </datacite:creators>\n"
                    "    <dc:language>eng</dc:language>\n"
                    "    <datacite:dates>\n"
                    "        <datacite:date dateType=\"Issued\">${year}</datacite:date>\n"
                    "    </datacite:dates>\n"
                    "    <oaire:resourceType resourceTypeGeneral=\"literature\" uri=\"http://purl.org/coar/resource_type/c_93fc\">report</oaire:resourceType>\n"
                    "    <datacite:rights rightsURI=\"http://purl.org/coar/access_right/c_abf2\">open access</datacite:rights>\n"
                    "</oaire:resource>"
                >>
        end
    }
].


%% @private
-spec exp_primary_identifier(od_handle:public_handle()) -> binary().
exp_primary_identifier(<<"doi:", DoiHandle/binary>>) ->
    <<"<datacite:identifier datacite:identifierType=\"DOI\">", DoiHandle/binary, "</datacite:identifier>">>;
exp_primary_identifier(<<"http://hdl.handle.net/", _/binary>> = PublicHandle) ->
    <<"<datacite:identifier datacite:identifierType=\"Handle\">", PublicHandle/binary, "</datacite:identifier>">>;
exp_primary_identifier(PublicHandle) ->
    <<"<datacite:identifier datacite:identifierType=\"URL\">", PublicHandle/binary, "</datacite:identifier>">>.
