%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Implementation of the onezone_plugin_behaviour and the handle_metadata_plugin_behaviour
%%% for handling DataCite metadata format ("oai_datacite" in OAI-PMH).
%%%
%%% NOTE: two variants of DataCite schema are commonly used: "oai_datacite" and "datacite".
%%% The main difference is that the former includes a wrapper element with additional
%%% information, as specified here: https://support.datacite.org/docs/oai-pmh-schema-documentation
%%% The official DataCite OAI-PMH endpoint supports both schemas:
%%% * https://oai.datacite.org/oai?verb=GetRecord&metadataPrefix=oai_datacite&identifier=doi:10.5061/dryad.7q0nq
%%% * https://oai.datacite.org/oai?verb=GetRecord&metadataPrefix=datacite&identifier=doi:10.5061/dryad.7q0nq
%%% This plugin covers "oai_datacite"; @see datacite_metadata_plugin for the other one.
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
%%%   * wrap in an additional oai_datacite element (see the NOTE above)
%%% @end
%%%-------------------------------------------------------------------
-module(oai_datacite_metadata_plugin).
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


-define(DATACENTRE_SYMBOL, case oz_worker:get_env(datacite_datacentre_symbol, undefined) of
    undefined -> oz_worker:get_domain();
    Symbol -> Symbol
end).


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
    <<"http://schema.datacite.org/oai/oai-1.1/oai.xsd">>.


%% @doc {@link handle_metadata_plugin_behaviour} callback main_namespace/0
-spec main_namespace() -> {atom(), binary()}.
main_namespace() ->
    {'xmlns', <<"http://schema.datacite.org/oai/oai-1.1/">>}.


%% @doc {@link handle_metadata_plugin_behaviour} callback revise_for_publication/3
-spec revise_for_publication(od_handle:parsed_metadata(), od_share:id(), od_share:record()) ->
    {ok, od_handle:parsed_metadata()} | error.
revise_for_publication(ResourceXml, ShareId, ShareRecord) ->
    datacite_metadata_plugin:revise_for_publication(ResourceXml, ShareId, ShareRecord).


%% @doc {@link handle_metadata_plugin_behaviour} callback insert_public_handle/1
-spec insert_public_handle(od_handle:parsed_metadata(), od_handle:public_handle()) ->
    od_handle:parsed_metadata().
insert_public_handle(ResourceXml, PublicHandle) ->
    datacite_metadata_plugin:insert_public_handle(ResourceXml, PublicHandle).


%% @doc {@link handle_metadata_plugin_behaviour} callback adapt_for_oai_pmh/1
-spec adapt_for_oai_pmh(od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
adapt_for_oai_pmh(#xmlElement{name = resource} = ResourceXml) ->
    {MainNamespaceName, MainNamespaceValue} = main_namespace(),
    SchemaLocation = str_utils:format("~ts ~ts", [MainNamespaceValue, schema_URL()]),
    #xmlElement{
        name = oai_datacite,
        attributes = [
            #xmlAttribute{name = MainNamespaceName, value = str_utils:to_list(MainNamespaceValue)},
            #xmlAttribute{name = 'xsi:schemaLocation', value = SchemaLocation}
        ],
        content = oai_xml:indent_content_in_newline(4, [
            #xmlElement{
                name = schemaVersion,
                content = [#xmlText{value = str_utils:to_list(infer_schema_version(ResourceXml))}]
            },
            #xmlElement{
                name = datacentreSymbol,
                content = [#xmlText{value = str_utils:to_list(?DATACENTRE_SYMBOL)}]
            },
            #xmlElement{name = payload, content = [
                #xmlText{value = "\n"},
                ResourceXml
            ]}
        ]) ++ [#xmlText{value = "\n"}]
    }.


%% @private
-spec infer_schema_version(od_handle:parsed_metadata()) -> string().
infer_schema_version(#xmlElement{attributes = Attrs}) ->
    case ?find_matching_element(#xmlAttribute{name = xmlns}, Attrs) of
        {ok, #xmlAttribute{value = "http://datacite.org/schema/kernel-" ++ Version}} ->
            Version;
        {ok, _} ->
            "0";
        error ->
            "0"
    end.


%% @doc {@link handle_metadata_plugin_behaviour} callback encode_xml/1
-spec encode_xml(od_handle:parsed_metadata()) -> od_handle:raw_metadata().
encode_xml(Metadata) ->
    oai_xml:encode(Metadata).


%% @doc {@link handle_metadata_plugin_behaviour} callback validation_examples/0
-spec validation_examples() -> [handle_metadata_plugin_behaviour:validation_example()].
validation_examples() ->
    lists:flatten([
        % TODO VFS-7454 add better validation of the XML (schema)
        datacite_metadata_plugin:invalid_examples(),

        #handle_metadata_plugin_validation_example{
            input_raw_xml = datacite_metadata_plugin:correct_input_raw_xml(),
            input_qualifies_for_publication = true,
            exp_revised_metadata_generator = fun datacite_metadata_plugin:correct_exp_revised_metadata_generator/2,
            exp_final_metadata_generator = fun datacite_metadata_plugin:correct_exp_final_metadata_generator/3,
            exp_oai_pmh_metadata_generator = fun(ShareId, ShareRecord, PublicHandle) ->
                OaiPmhEntry = datacite_metadata_plugin:correct_exp_oai_pmh_metadata_generator(
                    ShareId, ShareRecord, PublicHandle
                ),
                [_PrologLine, ExpDataCiteMetadata] = binary:split(OaiPmhEntry, <<"\n">>),
                <<
                    "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
                    "<oai_datacite xmlns=\"http://schema.datacite.org/oai/oai-1.1/\" xsi:schemaLocation=\"http://schema.datacite.org/oai/oai-1.1/ http://schema.datacite.org/oai/oai-1.1/oai.xsd\">\n"
                    "    <schemaVersion>4</schemaVersion>\n"
                    "    <datacentreSymbol>", (oz_worker:get_domain())/binary, "</datacentreSymbol>\n"
                    "    <payload>\n",
                    ExpDataCiteMetadata/binary, "\n",
                    "    </payload>\n"
                    "</oai_datacite>"
                >>
            end
        }
    ]).
