%%%-------------------------------------------------------------------
%%% @author Katarzyna Such, Lukasz Opiola
%%% @copyright (C) 2023-2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Implementation of the onezone_plugin_behaviour and the handle_metadata_plugin_behaviour
%%% for handling EDM (Europeana Data Model) metadata format in the scope of the Eureka3D project.
%%%
%%% @see handle_metadata_plugin_behaviour for general information about metadata plugins.
%%%
%%% Metadata revision step:
%%%   * check if the metadata is wrapped in rdf:RDF tags
%%%   * remove the rdf:about attr from edm:ProvidedCHO
%%%     (to be added when public handle is known)
%%%   * remove the rdf:about attr from ore:Aggregation
%%%     (to be added when public handle is known)
%%%   * remove the edm:AggregatedCHO element
%%%     (to be added when public handle is known)
%%%   * insert (and overwrite if exists) edm:isShownBy element,
%%%     pointing to a resource based on root FileId
%%%   * if there is a WebResource element without any specified rdf:about
%%%     attribute, insert the attribute with the same value as the
%%%     rdf:resource in edm:isShownBy.
%%%
%%% Public handle insertion step:
%%%   * add an rdf:about attr to edm:ProvidedCHO
%%%     (the value equal to the public handle)
%%%   * add an rdf:about attr to ore:Aggregation
%%%     (the value equal to the public handle plus "_AGG" suffix)
%%%   * add an the edm:AggregatedCHO element
%%%     (the rdf:resource attr value equal to the public handle)
%%%
%%% Adaptation for OAI-PMH step:
%%%   * no changes needed, return the metadata in the "rdf:RDF" tags
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(edm_eureka3d_metadata_plugin).
-author("Katarzyna Such").
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


-define(rdf_about_attr(Value), #xmlAttribute{name = 'rdf:about', value = Value}).
-define(rdf_resource_attr(Value), #xmlAttribute{name = 'rdf:resource', value = Value}).

-define(is_shown_by_value(FileId), str_utils:format("https://eureka3d.vm.fedcloud.eu/3d/~ts", [FileId])).


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


%% @doc {@link metadata_format_behaviour} callback metadata_prefix/0
-spec metadata_prefix() -> binary().
metadata_prefix() ->
    ?EDM_METADATA_PREFIX.


%% @doc {@link metadata_format_behaviour} callback schema_URL/0
-spec schema_URL() -> binary().
schema_URL() ->
    <<"https://www.europeana.eu/schemas/edm/EDM.xsd">>.


%% @doc {@link metadata_format_behaviour} callback main_namespace/0
-spec main_namespace() -> {atom(), binary()}.
main_namespace() ->
    {'xmlns:edm', <<"http://www.europeana.eu/schemas/edm/">>}.


%% @TODO VFS-7454 check the cardinality of submitted elements (e.g. ProvidedCHO 1, Aggregation 1, WebResource 0..N)
%% @doc {@link handle_metadata_plugin_behaviour} callback revise_for_publication/3
-spec revise_for_publication(od_handle:parsed_metadata(), od_share:id(), od_share:record()) ->
    {ok, od_handle:parsed_metadata()} | error.
revise_for_publication(#xmlElement{
    name = 'rdf:RDF', content = MetadataElements
} = RdfXml, _ShareId, ShareRecord) ->
    ShareRootFileId = ?check(file_id:guid_to_objectid(ShareRecord#od_share.root_file)),
    IsShownByValue = ?is_shown_by_value(ShareRootFileId),

    MetadataElementsWithPublicHandles = lists:map(fun
        (#xmlElement{name = 'edm:ProvidedCHO', attributes = CHOAttrs} = CHOElement) ->
            CHOElement#xmlElement{
                attributes = remove_rdf_about_attr(CHOAttrs)
            };
        (#xmlElement{name = 'ore:Aggregation', content = AggContent0, attributes = AggAttrs} = AggElement) ->
            AggContent1 = remove_rdf_resource_attr_from_aggregated_cho_element(AggContent0),
            AggContent2 = insert_element(AggContent1, 'edm:isShownBy', [?rdf_resource_attr(IsShownByValue)]),
            AggElement#xmlElement{
                attributes = remove_rdf_about_attr(AggAttrs),
                content = AggContent2
            };
        (#xmlElement{name = 'edm:WebResource', attributes = WRAttrs} = WRElement) ->
            WRElement#xmlElement{
                attributes = insert_rdf_about_attr(WRAttrs, honour_existing, IsShownByValue)
            };
        (Other) ->
            Other
    end, MetadataElements),

    {ok, RdfXml#xmlElement{content = MetadataElementsWithPublicHandles}};

revise_for_publication(_InvalidXml, _ShareId, _ShareRecord) ->
    error.


%% @doc {@link handle_metadata_plugin_behaviour} callback insert_public_handle/1
-spec insert_public_handle(od_handle:parsed_metadata(), od_handle:public_handle()) ->
    od_handle:parsed_metadata().
insert_public_handle(#xmlElement{
    name = 'rdf:RDF', content = MetadataElements
} = RdfXml, PublicHandle) ->
    MetadataElementsWithPublicHandles = lists:map(fun
        (#xmlElement{name = 'edm:ProvidedCHO', attributes = CHOAttrs} = CHOElement) ->
            CHOElement#xmlElement{
                attributes = insert_rdf_about_attr(CHOAttrs, overwrite, PublicHandle)
            };
        (#xmlElement{name = 'ore:Aggregation', content = AggContent, attributes = AggAttrs} = AggElement) ->
            AggElement#xmlElement{
                attributes = insert_rdf_about_attr(AggAttrs, overwrite, <<PublicHandle/binary, <<"_AGG">>/binary>>),
                content = insert_element(AggContent, 'edm:aggregatedCHO', [?rdf_resource_attr(PublicHandle)])
            };
        (Other) ->
            Other
    end, MetadataElements),

    RdfXml#xmlElement{content = MetadataElementsWithPublicHandles}.


%% @doc {@link handle_metadata_plugin_behaviour} callback adapt_for_oai_pmh/1
-spec adapt_for_oai_pmh(od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
adapt_for_oai_pmh(RdfXml) ->
    RdfXml.


%% @doc {@link handle_metadata_plugin_behaviour} callback encode_xml/1
-spec encode_xml(od_handle:parsed_metadata()) -> od_handle:raw_metadata().
encode_xml(Metadata) ->
    RawMetadata = oai_xml:encode(Metadata),
    % format the namespace attributes nicely (each in a new, indented line)
    iolist_to_binary(re:replace(RawMetadata, <<" xmlns:">>, <<"\n    xmlns:">>, [global])).


%% @doc {@link handle_metadata_plugin_behaviour} callback validation_examples/0
-spec validation_examples() -> [handle_metadata_plugin_behaviour:validation_example()].
validation_examples() ->
    gen_validation_examples().


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec remove_rdf_resource_attr_from_aggregated_cho_element([#xmlElement{}]) -> [#xmlElement{}].
remove_rdf_resource_attr_from_aggregated_cho_element(Elements) ->
    case ?find_matching_element(#xmlElement{name = 'edm:aggregatedCHO'}, Elements) of
        {ok, FoundElement = #xmlElement{attributes = Attrs}} ->
            lists_utils:replace(FoundElement, FoundElement#xmlElement{
                attributes = case ?find_matching_element(?rdf_resource_attr(_), Attrs) of
                    {ok, FoundAttr} -> lists:delete(FoundAttr, Attrs);
                    error -> Attrs
                end
            }, Elements);
        error ->
            Elements
    end.


%% @private
%% @doc existing attrs (if any) are always overwritten with the provided ones
-spec insert_element([#xmlElement{}], atom(), [#xmlAttribute{}]) -> [#xmlElement{}].
insert_element(Elements, Name, Attrs) ->
    case ?find_matching_element(#xmlElement{name = Name}, Elements) of
        {ok, Found} ->
            lists_utils:replace(Found, Found#xmlElement{attributes = Attrs}, Elements);
        error ->
            oai_xml:prepend_element_with_indent(8, #xmlElement{name = Name, attributes = Attrs}, Elements)
    end.


%% @private
-spec remove_rdf_about_attr([#xmlAttribute{}]) -> [#xmlAttribute{}].
remove_rdf_about_attr(Attrs) ->
    case ?find_matching_element(?rdf_about_attr(_), Attrs) of
        {ok, Found} -> lists:delete(Found, Attrs);
        error -> Attrs
    end.


%% @private
-spec insert_rdf_about_attr([#xmlAttribute{}], overwrite | honour_existing, binary()) -> [#xmlAttribute{}].
insert_rdf_about_attr(Attrs, Strategy, Identifier) ->
    case ?find_matching_element(?rdf_about_attr(_), Attrs) of
        {ok, Found} when Strategy == honour_existing ->
            Attrs;
        {ok, Found} when Strategy == overwrite ->
            lists_utils:replace(Found, ?rdf_about_attr(Identifier), Attrs);
        error ->
            [?rdf_about_attr(Identifier) | Attrs]
    end.


%%%===================================================================
%%% Validation examples
%%%===================================================================


% auxiliary macro to help build validation examples with different combinations of
% pre-existing properties
-record(validation_example_builder_ctx, {
    provided_cho_about_attr :: attr_not_provided | binary(),
    ore_aggregation_about_attr :: attr_not_provided | binary(),
    aggregated_cho_resource_attr :: element_not_provided | attr_not_provided | binary(),
    is_shown_by_resource_attr :: element_not_provided | attr_not_provided | binary()
}).


-define(ALL_NAMESPACES, [
    "xmlns:dc=\"http://purl.org/dc/elements/1.1/\"",
    "xmlns:dcterms=\"http://purl.org/dc/terms/\"",
    "xmlns:edm=\"http://www.europeana.eu/schemas/edm/\"",
    "xmlns:ore=\"http://www.openarchives.org/ore/terms/\"",
    "xmlns:owl=\"http://www.w3.org/2002/07/owl#\"",
    "xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"",
    "xmlns:foaf=\"http://xmlns.com/foaf/0.1/\"",
    "xmlns:skos=\"http://www.w3.org/2004/02/skos/core#\"",
    "xmlns:rdaGr2=\"http://rdvocab.info/ElementsGr2/\"",
    "xmlns:wgs84_pos=\"http://www.w3.org/2003/01/geo/wgs84_pos#\"",
    "xmlns:crm=\"http://www.cidoc-crm.org/cidoc--crm/\"",
    "xmlns:cc=\"http://creativecommons.org/ns#\""
]).

-define(RAND_ELEMENT(L), lists_utils:random_element(L)).


%% @private
-spec gen_validation_examples() -> [handle_metadata_plugin_behaviour:validation_example()].
gen_validation_examples() -> lists:flatten([
    #handle_metadata_plugin_validation_example{
        input_raw_xml = <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<valid-xml>but not a proper EDM metadata object</valid-xml>"
        >>,
        input_qualifies_for_publication = false
    },
    #handle_metadata_plugin_validation_example{
        input_raw_xml = <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
            "<edm:ProvidedCHO>\n",
            "   <dc:title xml:lang=\"en\">Metadata Example Record Tier A</dc:title>\n",
            "   <dc:type>book</dc:type>\n",
            "</edm:ProvidedCHO>"
        >>,
        input_qualifies_for_publication = false  % the top level <rdf:RDF> is required
    },
    % do not generate all the combinations, otherwise validation may take a substantial amount of time
    lists_utils:generate(fun() ->
        gen_validation_example(#validation_example_builder_ctx{
            provided_cho_about_attr = ?RAND_ELEMENT([attr_not_provided, <<"oai:325sa/ffa72790ef7">>]),
            ore_aggregation_about_attr = ?RAND_ELEMENT([attr_not_provided, <<"oai:kv8ds/kjf7ahi13f">>]),
            aggregated_cho_resource_attr = ?RAND_ELEMENT([attr_not_provided, element_not_provided, <<"oai:ks72a/bma9w8hfdalb">>]),
            is_shown_by_resource_attr = ?RAND_ELEMENT([attr_not_provided, element_not_provided, <<"https://example.com/about/oai:83jjd:sdfnb1m9x98">>])
        })
    end, 30)
]).


%% @private
-spec gen_validation_example(#validation_example_builder_ctx{}) ->
    handle_metadata_plugin_behaviour:validation_example().
gen_validation_example(Ctx) ->
    OpeningRdfTag = case str_utils:join_as_binaries(lists_utils:random_sublist(?ALL_NAMESPACES), <<"\n    ">>) of
        <<"">> -> <<"<rdf:RDF>">>;
        NamespaceAttrs -> <<"<rdf:RDF\n    ", NamespaceAttrs/binary, ">">>
    end,
    #handle_metadata_plugin_validation_example{
        input_raw_xml = gen_input_raw_xml_example(OpeningRdfTag, Ctx),
        input_qualifies_for_publication = true,
        exp_revised_metadata_generator = fun(_ShareId, ShareRecord) ->
            gen_exp_metadata(revised, OpeningRdfTag, ShareRecord, undefined, Ctx)
        end,
        exp_final_metadata_generator = fun(_ShareId, ShareRecord, PublicHandle) ->
            gen_exp_metadata(final, OpeningRdfTag, ShareRecord, PublicHandle, Ctx)
        end,
        exp_oai_pmh_metadata_generator = fun(_ShareId, ShareRecord, PublicHandle) ->
            gen_exp_metadata(oai_pmh, OpeningRdfTag, ShareRecord, PublicHandle, Ctx)
        end
    }.


%% @private
-spec gen_input_raw_xml_example(binary(), #validation_example_builder_ctx{}) -> binary().
gen_input_raw_xml_example(OpeningRdfTag, #validation_example_builder_ctx{
    provided_cho_about_attr = ProvidedChoAboutAttr,
    ore_aggregation_about_attr = OreAggregationAboutAttr,
    aggregated_cho_resource_attr = AggregatedChoResourceAttr,
    is_shown_by_resource_attr = IsShownByResourceAttr
}) ->
    BuildAboutAttrStr = fun
        (attr_not_provided) -> <<"">>;
        (Value) -> <<" rdf:about=\"", Value/binary, "\"">>
    end,

    BuildLineWithElementAndResource = fun
        (_ElementName, element_not_provided) ->
            <<"">>;
        (ElementName, attr_not_provided) ->
            case rand:uniform(2) of
                1 -> <<"        <", ElementName/binary, "/>\n">>;
                2 -> <<"        <", ElementName/binary, " rdf:resource=\"\"/>\n">>
            end;
        (ElementName, ResourceValue) ->
            <<"        <", ElementName/binary, " rdf:resource=\"", ResourceValue/binary, "\"/>\n">>
    end,

    <<
        "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
        OpeningRdfTag/binary, "\n",
        "    <edm:ProvidedCHO", (BuildAboutAttrStr(ProvidedChoAboutAttr))/binary, ">\n",
        "        <dc:title xml:lang=\"en\">Metadata Example Record Tier A</dc:title>\n",
        "        <dc:type>book</dc:type>\n",
        "        <dc:language>deu</dc:language>\n",
        "        <edm:type>TEXT</edm:type>\n",
        "        <dcterms:isPartOf>Europeana Foundation Example Records</dcterms:isPartOf>\n",
        "        <dc:identifier>some/internal/identifier/123456</dc:identifier>\n",
        "    </edm:ProvidedCHO>\n",
        "    <ore:Aggregation", (BuildAboutAttrStr(OreAggregationAboutAttr))/binary, ">\n",
        (BuildLineWithElementAndResource(<<"edm:aggregatedCHO">>, AggregatedChoResourceAttr))/binary,
        "        <edm:dataProvider>Europeana Foundation</edm:dataProvider>\n",
        (BuildLineWithElementAndResource(<<"edm:isShownBy">>, IsShownByResourceAttr))/binary,
        "        <edm:provider>Europeana Foundation</edm:provider>\n",
        "        <edm:isShownAt rdf:resource=\"https://example.com/show/oai:zxpp0/dfgs22bma9w\"/>\n",
        "        <edm:rights rdf:resource=\"http://rightsstatements.org/vocab/NoC-OKLR/1.0/\"/>\n",
        "    </ore:Aggregation>\n",
        "    <edm:WebResource rdf:about=\"https://example.com/image.jpg\">\n",
        "        <dc:description>Image representing the CHO</dc:description>\n",
        "        <dc:type>JPG</dc:type>\n",
        "        <edm:rights rdf:resource=\"http://creativecommons.org/licenses/by-nc-sa/4.0/\"/>\n",
        "    </edm:WebResource>\n",
        "    <edm:WebResource>\n",
        "        <dc:description>3D visualization of the CHO</dc:description>\n",
        "        <dc:type>3D</dc:type>\n",
        "        <edm:rights rdf:resource=\"http://creativecommons.org/licenses/by-nc-sa/4.0/\"/>\n",
        "    </edm:WebResource>\n",
        "</rdf:RDF>"
    >>.


%% @private
-spec gen_exp_metadata(
    revised | final | oai_pmh,
    binary(),
    od_share:record(),
    od_handle:public_handle(),
    #validation_example_builder_ctx{}
) ->
    binary().
gen_exp_metadata(MetadataType, OpeningRdfTag, ShareRecord, PublicHandle, #validation_example_builder_ctx{
    is_shown_by_resource_attr = IsShownByResourceAttr,
    aggregated_cho_resource_attr = AggChoResourceAttr
}) ->
    {ExpProvChoRdfAboutStr, ExpOreAggRdfAboutStr, ExpAggChoRdfResourceStr} = case MetadataType of
        revised -> {<<"">>, <<"">>, <<"">>};
        _ -> {
            <<" rdf:about=\"", PublicHandle/binary, "\"">>,
            <<" rdf:about=\"", PublicHandle/binary, "_AGG\"">>,
            <<" rdf:resource=\"", PublicHandle/binary, "\"">>
        }
    end,

    ShareRootFileId = ?check(file_id:guid_to_objectid(ShareRecord#od_share.root_file)),
    ExpIsShownByUrl = <<"https://eureka3d.vm.fedcloud.eu/3d/", ShareRootFileId/binary>>,
    ExpIsShownByLine = <<"        <edm:isShownBy rdf:resource=\"", ExpIsShownByUrl/binary, "\"/>\n">>,
    ExpAggChoLine = <<"        <edm:aggregatedCHO", ExpAggChoRdfResourceStr/binary, "/>\n">>,

    <<
        "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
        OpeningRdfTag/binary, "\n",
        "    <edm:ProvidedCHO", ExpProvChoRdfAboutStr/binary, ">\n",
        "        <dc:title xml:lang=\"en\">Metadata Example Record Tier A</dc:title>\n",
        "        <dc:type>book</dc:type>\n",
        "        <dc:language>deu</dc:language>\n",
        "        <edm:type>TEXT</edm:type>\n",
        "        <dcterms:isPartOf>Europeana Foundation Example Records</dcterms:isPartOf>\n",
        "        <dc:identifier>some/internal/identifier/123456</dc:identifier>\n",
        "    </edm:ProvidedCHO>\n",
        "    <ore:Aggregation", ExpOreAggRdfAboutStr/binary, ">\n",
        (case {MetadataType, AggChoResourceAttr} of {revised, _} -> <<"">>; {_, element_not_provided} -> ExpAggChoLine; _ -> <<"">> end)/binary,
        (case IsShownByResourceAttr of element_not_provided -> ExpIsShownByLine; _ -> <<"">> end)/binary,
        (case {MetadataType, AggChoResourceAttr} of {_, element_not_provided} -> <<"">>; _ -> ExpAggChoLine end)/binary,
        "        <edm:dataProvider>Europeana Foundation</edm:dataProvider>\n",
        (case IsShownByResourceAttr of element_not_provided -> <<"">>; _ -> ExpIsShownByLine end)/binary,
        "        <edm:provider>Europeana Foundation</edm:provider>\n",
        "        <edm:isShownAt rdf:resource=\"https://example.com/show/oai:zxpp0/dfgs22bma9w\"/>\n",
        "        <edm:rights rdf:resource=\"http://rightsstatements.org/vocab/NoC-OKLR/1.0/\"/>\n",
        "    </ore:Aggregation>\n",
        "    <edm:WebResource rdf:about=\"https://example.com/image.jpg\">\n",
        "        <dc:description>Image representing the CHO</dc:description>\n",
        "        <dc:type>JPG</dc:type>\n",
        "        <edm:rights rdf:resource=\"http://creativecommons.org/licenses/by-nc-sa/4.0/\"/>\n",
        "    </edm:WebResource>\n",
        "    <edm:WebResource rdf:about=\"", (ExpIsShownByUrl)/binary, "\">\n",
        "        <dc:description>3D visualization of the CHO</dc:description>\n",
        "        <dc:type>3D</dc:type>\n",
        "        <edm:rights rdf:resource=\"http://creativecommons.org/licenses/by-nc-sa/4.0/\"/>\n",
        "    </edm:WebResource>\n",
        "</rdf:RDF>"
    >>.
