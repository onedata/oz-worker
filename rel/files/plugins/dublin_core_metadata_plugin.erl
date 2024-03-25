%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia, Lukasz Opiola
%%% @copyright (C) 2016-2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Implementation of a onezone plugin and the handle_metadata_plugin_behaviour
%%% for handling DC (Dublin Core) metadata format.
%%%
%%% @see handle_metadata_plugin_behaviour for general information about metadata plugins.
%%%
%%% Metadata revision step:
%%%   * dc:identifier element is added, with the value equal to
%%%     the public share URL
%%%
%%% Public handle insertion step:
%%%   * dc:identifier element is added, with the value equal to
%%%     the public handle
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dublin_core_metadata_plugin).
-author("Jakub Kudzia").
-author("Lukasz Opiola").

-behavior(onezone_plugin_behaviour).
-behaviour(handle_metadata_plugin_behaviour).

-include("http/handlers/oai.hrl").


%% onezone_plugin_behaviour callbacks
-export([type/0]).

%% handle_metadata_plugin_behaviour callbacks
-export([metadata_prefix/0, schema_URL/0, main_namespace/0]).
-export([revise_for_publication/3, insert_public_handle/2, adapt_for_oai_pmh/1]).
-export([validation_examples/0]).


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
metadata_prefix() -> ?OAI_DC_METADATA_PREFIX.


%% @doc {@link handle_metadata_plugin_behaviour} callback schema_URL/0
-spec schema_URL() -> binary().
schema_URL() -> <<"http://www.openarchives.org/OAI/2.0/oai_dc.xsd">>.


%% @doc {@link handle_metadata_plugin_behaviour} callback main_namespace/0
-spec main_namespace() -> {atom(), binary()}.
main_namespace() ->
    {'xmlns:oai_dc', <<"http://www.openarchives.org/OAI/2.0/oai_dc/">>}.


%% @doc {@link handle_metadata_plugin_behaviour} callback revise_for_publication/3
-spec revise_for_publication(od_handle:parsed_metadata(), od_share:id(), od_share:record()) ->
    {ok, od_handle:parsed_metadata()} | error.
revise_for_publication(#xmlElement{name = metadata, content = Content} = MetadataXml, ShareId, _ShareRecord) ->
    {ok, MetadataXml#xmlElement{
        content = Content ++ [#xmlElement{
            name = 'dc:identifier',
            content = [#xmlText{value = binary_to_list(share_logic:build_public_url(ShareId))}]
        }]
    }}.


%% @doc {@link handle_metadata_plugin_behaviour} callback insert_public_handle/1
-spec insert_public_handle(od_handle:parsed_metadata(), od_handle:public_handle()) ->
    od_handle:parsed_metadata().
insert_public_handle(#xmlElement{name = metadata, content = Content} = MetadataXml, PublicHandle) ->
    MetadataXml#xmlElement{
        content = Content ++ [#xmlElement{
            name = 'dc:identifier',
            content = [#xmlText{value = binary_to_list(PublicHandle)}]
        }]
    }.


%% @doc {@link handle_metadata_plugin_behaviour} callback adapt_for_oai_pmh/1
-spec adapt_for_oai_pmh(od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
adapt_for_oai_pmh(#xmlElement{name = metadata, content = Content}) ->
    {MainNamespaceName, MainNamespaceValue} = main_namespace(),
    #xmlElement{
        name = 'oai_dc:dc',
        attributes = [
            #xmlAttribute{name = MainNamespaceName, value = str_utils:to_list(MainNamespaceValue)},
            #xmlAttribute{name = 'xmlns:dc', value = "http://purl.org/dc/elements/1.1/"},
            ?OAI_XML_SCHEMA_NAMESPACE,
            #xmlAttribute{name = 'xsi:schemaLocation', value = str_utils:format("~s ~s", [MainNamespaceValue, schema_URL()])}
        ],
        content = Content
    }.


%% @doc {@link handle_metadata_plugin_behaviour} callback validation_examples/0
-spec validation_examples() -> [handle_metadata_plugin_behaviour:validation_example()].
validation_examples() -> [
    % TODO VFS-11365 add better validation of the DC XML; currently, any XML is accepted
    #handle_metadata_plugin_validation_example{
        input_raw_xml = <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>",
            "<metadata>",
            "<dc:title xml:lang=\"en\">Metadata Example Record Tier A</dc:title>",
            "<dc:type>book</dc:type>",
            "</metadata>"
        >>,
        input_qualifies_for_publication = true,
        exp_revised_metadata_generator = fun(ShareId, _ShareRecord) -> <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>",
            "<metadata>",
            "<dc:title xml:lang=\"en\">Metadata Example Record Tier A</dc:title>",
            "<dc:type>book</dc:type>",
            "<dc:identifier>", (share_logic:build_public_url(ShareId))/binary, "</dc:identifier>",
            "</metadata>"
        >> end,
        exp_final_metadata_generator = fun(ShareId, _ShareRecord, PublicHandle) -> <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>",
            "<metadata>",
            "<dc:title xml:lang=\"en\">Metadata Example Record Tier A</dc:title>",
            "<dc:type>book</dc:type>",
            "<dc:identifier>", (share_logic:build_public_url(ShareId))/binary, "</dc:identifier>",
            "<dc:identifier>", PublicHandle/binary, "</dc:identifier>",
            "</metadata>"
        >> end
    },
    #handle_metadata_plugin_validation_example{
        input_raw_xml = <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>",
            "<dc:contributor>John Doe</dc:contributor>"
        >>,
        input_qualifies_for_publication = true,
        exp_revised_metadata_generator = fun(ShareId, _ShareRecord) -> <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>",
            "<metadata>",
            "<dc:contributor>John Doe</dc:contributor>",
            "<dc:identifier>", (share_logic:build_public_url(ShareId))/binary, "</dc:identifier>",
            "</metadata>"
        >> end,
        exp_final_metadata_generator = fun(ShareId, _ShareRecord, PublicHandle) -> <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>",
            "<metadata>",
            "<dc:contributor>John Doe</dc:contributor>",
            "<dc:identifier>", (share_logic:build_public_url(ShareId))/binary, "</dc:identifier>",
            "<dc:identifier>", PublicHandle/binary, "</dc:identifier>",
            "</metadata>"
        >> end
    }
].