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
%%%   * ensure edm:isShownAt element with a defined rdf:resource attribute;
%%%     do not change if exists, otherwise, add one pointing to
%%%     a resource based on root FileId
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
-export([validation_examples/0]).


-define(find_matching_element(Pattern, List), lists_utils:find(fun(Pattern) -> true; (_) -> false end, List)).

-define(rdf_about_attr(Value), #xmlAttribute{name = 'rdf:about', value = Value}).
-define(rdf_resource_attr(Value), #xmlAttribute{name = 'rdf:resource', value = Value}).

-define(is_shown_by_value(FileId), str_utils:format("https://eureka3d.vm.fedcloud.eu/test/~s", [FileId])).
-define(is_shown_at_value(FileId), str_utils:format("https://eureka3d.vm.fedcloud.eu/test/~s", [FileId])).


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


%% @TODO VFS-11906 check the cardinality of submitted elements (e.g. ProvidedCHO 1, Aggregation 1, WebResource 0..N)
%% @doc {@link handle_metadata_plugin_behaviour} callback revise_for_publication/3
-spec revise_for_publication(od_handle:parsed_metadata(), od_share:id(), od_share:record()) ->
    {ok, od_handle:parsed_metadata()} | error.
revise_for_publication(#xmlElement{
    name = 'rdf:RDF', content = MetadataElements
} = RdfXml, _ShareId, ShareRecord) ->
    MetadataElementsWithPublicHandles = lists:map(fun
        (#xmlElement{name = 'edm:ProvidedCHO', attributes = CHOAttrs} = CHOElement) ->
            CHOElement#xmlElement{
                attributes = remove_rdf_about_attribute(CHOAttrs)
            };
        (#xmlElement{name = 'ore:Aggregation', content = AggContent0, attributes = AggAttrs} = AggElement) ->
            ShareRootFileId = ?check(file_id:guid_to_objectid(ShareRecord#od_share.root_file)),
            AggContent1 = remove_aggregated_cho_element(AggContent0),
            AggContent2 = insert_is_shown_by_element(AggContent1, ShareRootFileId),
            AggContent3 = ensure_is_shown_at_element(AggContent2, ShareRootFileId),
            AggElement#xmlElement{
                attributes = remove_rdf_about_attribute(AggAttrs),
                content = AggContent3
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
                attributes = insert_rdf_about_attribute(CHOAttrs, PublicHandle)
            };
        (#xmlElement{name = 'ore:Aggregation', content = AggContent, attributes = AggAttrs} = AggElement) ->
            AggElement#xmlElement{
                attributes = insert_rdf_about_attribute(AggAttrs, <<PublicHandle/binary, <<"_AGG">>/binary>>),
                content = insert_aggregated_cho_element(AggContent, PublicHandle)
            };
        (Other) ->
            Other
    end, MetadataElements),

    RdfXml#xmlElement{content = MetadataElementsWithPublicHandles}.


%% @doc {@link handle_metadata_plugin_behaviour} callback adapt_for_oai_pmh/1
-spec adapt_for_oai_pmh(od_handle:parsed_metadata()) -> od_handle:parsed_metadata().
adapt_for_oai_pmh(RdfXml) ->
    RdfXml.


%% @doc {@link handle_metadata_plugin_behaviour} callback validation_examples/0
-spec validation_examples() -> [handle_metadata_plugin_behaviour:validation_example()].
validation_examples() ->
    gen_validation_examples().


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec remove_rdf_about_attribute([#xmlAttribute{}]) -> [#xmlAttribute{}].
remove_rdf_about_attribute(Attrs) ->
    case ?find_matching_element(?rdf_about_attr(_), Attrs) of
        {ok, Found} -> lists:delete(Found, Attrs);
        error -> Attrs
    end.


%% @private
-spec remove_aggregated_cho_element([#xmlElement{}]) -> [#xmlElement{}].
remove_aggregated_cho_element(Elements) ->
    case ?find_matching_element(#xmlElement{name = 'edm:aggregatedCHO'}, Elements) of
        {ok, Found} -> lists:delete(Found, Elements);
        error -> Elements
    end.


%% @private
-spec insert_is_shown_by_element([#xmlElement{}], file_id:objectid()) -> [#xmlElement{}].
insert_is_shown_by_element(Elements, FileId) ->
    case ?find_matching_element(#xmlElement{name = 'edm:isShownBy'}, Elements) of
        {ok, Found} ->
            lists_utils:replace(Found, Found#xmlElement{attributes = [
                ?rdf_resource_attr(?is_shown_by_value(FileId))
            ]}, Elements);
        error ->
            Elements ++ [#xmlElement{name = 'edm:isShownBy', attributes = [
                ?rdf_resource_attr(?is_shown_by_value(FileId))
            ]}]
    end.


%% @private
-spec ensure_is_shown_at_element([#xmlElement{}], file_id:objectid()) -> [#xmlElement{}].
ensure_is_shown_at_element(Elements, FileId) ->
    case ?find_matching_element(#xmlElement{name = 'edm:isShownAt'}, Elements) of
        {ok, #xmlElement{attributes = Attributes} = Found} ->
            case ?find_matching_element(?rdf_resource_attr(_), Attributes) of
                {ok, ?rdf_resource_attr(Value)} when length(Value) > 0 ->
                    Elements;
                _ ->
                    lists_utils:replace(Found, Found#xmlElement{attributes = [
                        ?rdf_resource_attr(?is_shown_at_value(FileId))
                    ]}, Elements)
            end;
        error ->
            Elements ++ [#xmlElement{name = 'edm:isShownAt', attributes = [
                ?rdf_resource_attr(?is_shown_at_value(FileId))
            ]}]
    end.


%% @private
-spec insert_rdf_about_attribute([#xmlAttribute{}], binary()) -> [#xmlAttribute{}].
insert_rdf_about_attribute(Attrs, Identifier) ->
    case ?find_matching_element(?rdf_about_attr(_), Attrs) of
        {ok, Found} ->
            lists_utils:replace(Found, ?rdf_about_attr(Identifier), Attrs);
        error ->
            [?rdf_about_attr(Identifier) | Attrs]
    end.


%% @private
-spec insert_aggregated_cho_element([#xmlElement{}], binary()) -> [#xmlElement{}].
insert_aggregated_cho_element(Elements, Identifier) ->
    case ?find_matching_element(#xmlElement{name = 'edm:aggregatedCHO'}, Elements) of
        {ok, Found} ->
            lists_utils:replace(Found, Found#xmlElement{attributes = [?rdf_resource_attr(Identifier)]}, Elements);
        error ->
            Elements ++ [#xmlElement{name = 'edm:aggregatedCHO', attributes = [?rdf_resource_attr(Identifier)]}]
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
    is_shown_by_resource_attr :: element_not_provided | attr_not_provided | binary(),
    is_shown_at_resource_attr :: element_not_provided | attr_not_provided | binary()
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
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"
            "<valid-xml>but not a proper EDM metadata object</valid-xml>"
        >>,
        input_qualifies_for_publication = false
    },
    #handle_metadata_plugin_validation_example{
        input_raw_xml = <<
            "<?xml version=\"1.0\" encoding=\"utf-8\" ?>",
            "<edm:ProvidedCHO>",
            "<dc:title xml:lang=\"en\">Metadata Example Record Tier A</dc:title>",
            "<dc:type>book</dc:type>",
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
            is_shown_by_resource_attr = ?RAND_ELEMENT([attr_not_provided, element_not_provided, <<"https://example.com/about/oai:83jjd:sdfnb1m9x98">>]),
            is_shown_at_resource_attr = ?RAND_ELEMENT([attr_not_provided, element_not_provided, <<"https://example.com/show/oai:zxpp0/dfgs22bma9w">>])
        })
    end, 30)
]).


%% @private
-spec gen_validation_example(#validation_example_builder_ctx{}) ->
    handle_metadata_plugin_behaviour:validation_example().
gen_validation_example(Ctx) ->
    OpeningRdfTag = case str_utils:join_as_binaries(lists_utils:random_sublist(?ALL_NAMESPACES), <<" ">>) of
        <<"">> -> <<"<rdf:RDF>">>;
        SpaceSeparatedReferences -> <<"<rdf:RDF ", SpaceSeparatedReferences/binary, ">">>
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
    is_shown_by_resource_attr = IsShownByResourceAttr,
    is_shown_at_resource_attr = IsShownAtResourceAttr
}) ->
    BuildAboutAttrStr = fun
        (attr_not_provided) -> <<"">>;
        (Value) -> <<" rdf:about=\"", Value/binary, "\"">>
    end,

    BuildElementWithResource = fun
        (_ElementName, element_not_provided) ->
            <<"">>;
        (ElementName, attr_not_provided) ->
            case rand:uniform(2) of
                1 -> <<"<", ElementName/binary, "/>">>;
                2 -> <<"<", ElementName/binary, " rdf:resource=\"\"/>">>
            end;
        (ElementName, ResourceValue) ->
            <<"<", ElementName/binary, " rdf:resource=\"", ResourceValue/binary, "\"/>">>
    end,

    <<
        "<?xml version=\"1.0\" encoding=\"utf-8\" ?>",
        OpeningRdfTag/binary,
        "<edm:ProvidedCHO", (BuildAboutAttrStr(ProvidedChoAboutAttr))/binary, ">",
        "<dc:title xml:lang=\"en\">Metadata Example Record Tier A</dc:title>",
        "<dc:type>book</dc:type>",
        "<dc:language>deu</dc:language>",
        "<edm:type>TEXT</edm:type>",
        "<dcterms:isPartOf>Europeana Foundation Example Records</dcterms:isPartOf>",
        "<dc:identifier>some/internal/identifier/123456</dc:identifier>",
        "</edm:ProvidedCHO>",
        "<ore:Aggregation", (BuildAboutAttrStr(OreAggregationAboutAttr))/binary, ">",
        (BuildElementWithResource(<<"edm:aggregatedCHO">>, AggregatedChoResourceAttr))/binary,
        "<edm:dataProvider>Europeana Foundation</edm:dataProvider>",
        (BuildElementWithResource(<<"edm:isShownBy">>, IsShownByResourceAttr))/binary,
        "<edm:provider>Europeana Foundation</edm:provider>",
        "<edm:rights rdf:resource=\"http://rightsstatements.org/vocab/NoC-OKLR/1.0/\"/>",
        (BuildElementWithResource(<<"edm:isShownAt">>, IsShownAtResourceAttr))/binary,
        "</ore:Aggregation>",
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
    is_shown_at_resource_attr = IsShownAtResourceAttr
}) ->
    {ExpProvChoRdfAboutStr, ExpOreAggRdfAboutStr, ExpAggChoElement} = case MetadataType of
        revised -> {<<"">>, <<"">>, <<"">>};
        _ -> {
            <<" rdf:about=\"", PublicHandle/binary, "\"">>,
            <<" rdf:about=\"", PublicHandle/binary, "_AGG\"">>,
            <<"<edm:aggregatedCHO rdf:resource=\"", PublicHandle/binary, "\"/>">>
        }
    end,

    ShareRootFileId = ?check(file_id:guid_to_objectid(ShareRecord#od_share.root_file)),
    ExpIsShownAtResource = case IsShownAtResourceAttr of
        Value when is_binary(Value) -> Value;
        _ -> <<"https://eureka3d.vm.fedcloud.eu/test/", ShareRootFileId/binary>>
    end,
    ExpIsShownAtElement = <<"<edm:isShownAt rdf:resource=\"", ExpIsShownAtResource/binary, "\"/>">>,
    ExpIsShownByElement = <<
        "<edm:isShownBy rdf:resource=\"https://eureka3d.vm.fedcloud.eu/test/", ShareRootFileId/binary, "\"/>"
    >>,

    <<
        "<?xml version=\"1.0\" encoding=\"utf-8\" ?>",
        OpeningRdfTag/binary,
        "<edm:ProvidedCHO", ExpProvChoRdfAboutStr/binary, ">",
        "<dc:title xml:lang=\"en\">Metadata Example Record Tier A</dc:title>",
        "<dc:type>book</dc:type>",
        "<dc:language>deu</dc:language>",
        "<edm:type>TEXT</edm:type>",
        "<dcterms:isPartOf>Europeana Foundation Example Records</dcterms:isPartOf>",
        "<dc:identifier>some/internal/identifier/123456</dc:identifier>",
        "</edm:ProvidedCHO>",
        "<ore:Aggregation", ExpOreAggRdfAboutStr/binary, ">",
        "<edm:dataProvider>Europeana Foundation</edm:dataProvider>",
        (case IsShownByResourceAttr of element_not_provided -> <<"">>; _ -> ExpIsShownByElement end)/binary,
        "<edm:provider>Europeana Foundation</edm:provider>",
        "<edm:rights rdf:resource=\"http://rightsstatements.org/vocab/NoC-OKLR/1.0/\"/>",
        (case IsShownAtResourceAttr of element_not_provided -> <<"">>; _ -> ExpIsShownAtElement end)/binary,
        (case IsShownByResourceAttr of element_not_provided -> ExpIsShownByElement; _ -> <<"">> end)/binary,
        (case IsShownAtResourceAttr of element_not_provided -> ExpIsShownAtElement; _ -> <<"">> end)/binary,
        ExpAggChoElement/binary,
        "</ore:Aggregation>",
        "</rdf:RDF>"
    >>.
