%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia, Lukasz Opiola
%%% @copyright (C) 2016-2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Implementation of the onezone_plugin_behaviour and the handle_metadata_plugin_behaviour
%%% for handling Dublin Core metadata schema ("oai_dc").
%%%
%%% @see handle_metadata_plugin_behaviour for general information about metadata plugins.
%%%
%%% Metadata revision step:
%%%   * add a dc:identifier element with the value equal to the public share URL
%%%
%%% Public handle insertion step:
%%%   * add a dc:identifier element with the value equal to the public handle
%%%
%%% Adaptation for OAI-PMH step:
%%%   * change the outermost "metadata" element to "oai_dc:dc"
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(oai_dc_metadata_plugin).
-author("Jakub Kudzia").
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
    ?OAI_DC_METADATA_PREFIX.


%% @doc {@link handle_metadata_plugin_behaviour} callback supported_oai_pmh_metadata_prefixes/0
-spec supported_oai_pmh_metadata_prefixes() -> od_handle:metadata_schema().
supported_oai_pmh_metadata_prefixes() ->
    [?OAI_DC_METADATA_PREFIX].


%% @doc {@link handle_metadata_plugin_behaviour} callback schema_URL/1
-spec schema_URL(oai_metadata:prefix()) -> binary().
schema_URL(?OAI_DC_METADATA_PREFIX) ->
    <<"http://www.openarchives.org/OAI/2.0/oai_dc.xsd">>.


%% @doc {@link handle_metadata_plugin_behaviour} callback main_namespace/1
-spec main_namespace(oai_metadata:prefix()) -> {atom(), binary()}.
main_namespace(?OAI_DC_METADATA_PREFIX) ->
    {'xmlns:oai_dc', <<"http://www.openarchives.org/OAI/2.0/oai_dc/">>}.


%% @doc {@link handle_metadata_plugin_behaviour} callback revise_for_publication/3
-spec revise_for_publication(od_handle:parsed_metadata(), od_share:id(), od_share:record()) ->
    {ok, od_handle:parsed_metadata()} | error.
revise_for_publication(#xmlElement{name = metadata} = MetadataXml, ShareId, _ShareRecord) ->
    {ok, ensure_dc_identifier(binary_to_list(od_share:build_public_url(ShareId)), MetadataXml)};

revise_for_publication(_InvalidXml, _ShareId, _ShareRecord) ->
    error.


%% @doc {@link handle_metadata_plugin_behaviour} callback insert_public_handle/1
-spec insert_public_handle(od_handle:parsed_metadata(), od_handle:public_handle()) ->
    od_handle:parsed_metadata().
insert_public_handle(#xmlElement{name = metadata} = MetadataXml, PublicHandle) ->
    ensure_dc_identifier(binary_to_list(PublicHandle), MetadataXml).


%% @private
-spec ensure_dc_identifier(string(), od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
ensure_dc_identifier(Value, #xmlElement{content = Content} = MetadataXml) ->
    case ?find_matching_element(#xmlElement{name = 'dc:identifier', content = [#xmlText{value = Value}]}, Content) of
        {ok, _} ->
            MetadataXml;
        error ->
            MetadataXml#xmlElement{
                content = oai_xml:prepend_element_with_indent(4, #xmlElement{
                    name = 'dc:identifier',
                    content = [#xmlText{value = Value}]
                }, Content)
            }
    end.


%% @doc {@link handle_metadata_plugin_behaviour} callback adapt_for_oai_pmh/2
-spec adapt_for_oai_pmh(oai_metadata:prefix(), od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
adapt_for_oai_pmh(?OAI_DC_METADATA_PREFIX, #xmlElement{name = metadata, content = Content}) ->
    {MainNamespaceName, MainNamespaceValue} = main_namespace(?OAI_DC_METADATA_PREFIX),
    SchemaLocation = str_utils:format("~ts ~ts", [MainNamespaceValue, schema_URL(?OAI_DC_METADATA_PREFIX)]),
    #xmlElement{
        name = 'oai_dc:dc',
        attributes = [
            #xmlAttribute{name = MainNamespaceName, value = str_utils:to_list(MainNamespaceValue)},
            #xmlAttribute{name = 'xmlns:dc', value = "http://purl.org/dc/elements/1.1/"},
            ?OAI_XML_SCHEMA_NAMESPACE,
            #xmlAttribute{name = 'xsi:schemaLocation', value = SchemaLocation}
        ],
        content = Content
    }.


%% @doc {@link handle_metadata_plugin_behaviour} callback encode_xml/1
-spec encode_xml(od_handle:parsed_metadata()) -> od_handle:raw_metadata().
encode_xml(Metadata) ->
    oai_xml:encode(Metadata).


%% @doc {@link handle_metadata_plugin_behaviour} callback validation_examples/0
-spec validation_examples() -> [handle_metadata_plugin_behaviour:validation_example()].
validation_examples() -> [
    % TODO VFS-7454 add better validation of the DC XML (schema)
    #handle_metadata_plugin_validation_example{
        input_raw_xml = <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<valid-xml>but no metadata tag</valid-xml>"
        >>,
        input_qualifies_for_publication = false
    },

    #handle_metadata_plugin_validation_example{
        input_raw_xml = <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<dc:contributor>John Doe</dc:contributor>"
        >>,
        input_qualifies_for_publication = false
    },

    #handle_metadata_plugin_validation_example{
        input_raw_xml = <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<metadata\n"
            "    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
            "    xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n"
            "    <dc:title>Test dataset</dc:title>\n",
            "    <dc:creator>John Johnson</dc:creator>\n",
            "    <dc:creator>Jane Doe</dc:creator>\n",
            "    <dc:subject>Test of datacite</dc:subject>\n",
            "    <dc:description>Lorem ipsum</dc:description>\n",
            "    <dc:publisher>Onedata</dc:publisher>\n",
            "    <dc:publisher>EGI</dc:publisher>\n",
            "    <dc:date>2016</dc:date>\n",
            "    <dc:format>application/pdf</dc:format>\n",
            "    <dc:identifier>some/preexisting/identifier/1234567</dc:identifier>\n",
            "    <dc:language>eng</dc:language>\n",
            "    <dc:rights>CC-0</dc:rights>\n",
            "</metadata>"
        >>,
        input_qualifies_for_publication = true,
        exp_revised_metadata_generator = fun(ShareId, _ShareRecord) -> <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<metadata\n"
            "    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
            "    xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n"
            "    <dc:identifier>", (od_share:build_public_url(ShareId))/binary, "</dc:identifier>\n",
            "    <dc:title>Test dataset</dc:title>\n",
            "    <dc:creator>John Johnson</dc:creator>\n",
            "    <dc:creator>Jane Doe</dc:creator>\n",
            "    <dc:subject>Test of datacite</dc:subject>\n",
            "    <dc:description>Lorem ipsum</dc:description>\n",
            "    <dc:publisher>Onedata</dc:publisher>\n",
            "    <dc:publisher>EGI</dc:publisher>\n",
            "    <dc:date>2016</dc:date>\n",
            "    <dc:format>application/pdf</dc:format>\n",
            "    <dc:identifier>some/preexisting/identifier/1234567</dc:identifier>\n",
            "    <dc:language>eng</dc:language>\n",
            "    <dc:rights>CC-0</dc:rights>\n",
            "</metadata>"
        >> end,
        exp_final_metadata_generator = fun(ShareId, _ShareRecord, PublicHandle) -> <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<metadata\n"
            "    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
            "    xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n"
            "    <dc:identifier>", PublicHandle/binary, "</dc:identifier>\n",
            "    <dc:identifier>", (od_share:build_public_url(ShareId))/binary, "</dc:identifier>\n",
            "    <dc:title>Test dataset</dc:title>\n",
            "    <dc:creator>John Johnson</dc:creator>\n",
            "    <dc:creator>Jane Doe</dc:creator>\n",
            "    <dc:subject>Test of datacite</dc:subject>\n",
            "    <dc:description>Lorem ipsum</dc:description>\n",
            "    <dc:publisher>Onedata</dc:publisher>\n",
            "    <dc:publisher>EGI</dc:publisher>\n",
            "    <dc:date>2016</dc:date>\n",
            "    <dc:format>application/pdf</dc:format>\n",
            "    <dc:identifier>some/preexisting/identifier/1234567</dc:identifier>\n",
            "    <dc:language>eng</dc:language>\n",
            "    <dc:rights>CC-0</dc:rights>\n",
            "</metadata>"
        >> end,
        exp_oai_pmh_metadata_generator = fun(?OAI_DC_METADATA_PREFIX, ShareId, _ShareRecord, PublicHandle) -> <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<oai_dc:dc\n"
            "    xmlns:oai_dc=\"http://www.openarchives.org/OAI/2.0/oai_dc/\"\n",
            "    xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n",
            "    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n",
            "    xsi:schemaLocation=\"http://www.openarchives.org/OAI/2.0/oai_dc/ http://www.openarchives.org/OAI/2.0/oai_dc.xsd\">\n"
            "    <dc:identifier>", PublicHandle/binary, "</dc:identifier>\n",
            "    <dc:identifier>", (od_share:build_public_url(ShareId))/binary, "</dc:identifier>\n",
            "    <dc:title>Test dataset</dc:title>\n",
            "    <dc:creator>John Johnson</dc:creator>\n",
            "    <dc:creator>Jane Doe</dc:creator>\n",
            "    <dc:subject>Test of datacite</dc:subject>\n",
            "    <dc:description>Lorem ipsum</dc:description>\n",
            "    <dc:publisher>Onedata</dc:publisher>\n",
            "    <dc:publisher>EGI</dc:publisher>\n",
            "    <dc:date>2016</dc:date>\n",
            "    <dc:format>application/pdf</dc:format>\n",
            "    <dc:identifier>some/preexisting/identifier/1234567</dc:identifier>\n",
            "    <dc:language>eng</dc:language>\n",
            "    <dc:rights>CC-0</dc:rights>\n",
            "</oai_dc:dc>"
        >> end
    },

    #handle_metadata_plugin_validation_example{
        input_raw_xml = <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<metadata>\n",
            "    <dc:contributor>John Doe</dc:contributor>\n",
            "</metadata>"
        >>,
        input_qualifies_for_publication = true,
        exp_revised_metadata_generator = fun(ShareId, _ShareRecord) -> <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<metadata>\n",
            "    <dc:identifier>", (od_share:build_public_url(ShareId))/binary, "</dc:identifier>\n",
            "    <dc:contributor>John Doe</dc:contributor>\n",
            "</metadata>"
        >> end,
        exp_final_metadata_generator = fun(ShareId, _ShareRecord, PublicHandle) -> <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<metadata>\n",
            "    <dc:identifier>", PublicHandle/binary, "</dc:identifier>\n",
            "    <dc:identifier>", (od_share:build_public_url(ShareId))/binary, "</dc:identifier>\n",
            "    <dc:contributor>John Doe</dc:contributor>\n",
            "</metadata>"
        >> end,
        exp_oai_pmh_metadata_generator = fun(?OAI_DC_METADATA_PREFIX, ShareId, _ShareRecord, PublicHandle) -> <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<oai_dc:dc\n"
            "    xmlns:oai_dc=\"http://www.openarchives.org/OAI/2.0/oai_dc/\"\n",
            "    xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n",
            "    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n",
            "    xsi:schemaLocation=\"http://www.openarchives.org/OAI/2.0/oai_dc/ http://www.openarchives.org/OAI/2.0/oai_dc.xsd\">\n",
            "    <dc:identifier>", PublicHandle/binary, "</dc:identifier>\n",
            "    <dc:identifier>", (od_share:build_public_url(ShareId))/binary, "</dc:identifier>\n",
            "    <dc:contributor>John Doe</dc:contributor>\n",
            "</oai_dc:dc>"
        >> end
    }
].