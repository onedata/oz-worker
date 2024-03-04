%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This module is responsible for encoding metadata to Europeana Data Model format.
%%% It implements metadata_format_behaviour.
%%% @end
%%%-------------------------------------------------------------------
-module(europeana_data_model).
-author("Katarzyna Such").

-include("http/handlers/oai.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-behaviour(metadata_format_behaviour).

-define(find_matching_element(Pattern, List), lists_utils:find(fun(Pattern) -> true; (_) -> false end, List)).

%% API
-export([elements/0, sanitize_metadata/1, encode/2, metadata_prefix/0, schema_URL/0,
    main_namespace/0, resolve_additional_identifiers/1]).


%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback metadata_prefix/0
%%% @end
%%%-------------------------------------------------------------------
-spec metadata_prefix() -> binary().
metadata_prefix() -> ?EDM_METADATA_PREFIX.

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback schema_URL/0
%%% @end
%%%-------------------------------------------------------------------
-spec schema_URL() -> binary().
schema_URL() -> <<"https://www.europeana.eu/schemas/edm/EDM.xsd">>.

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback main_namespace/0
%%% @end
%%%-------------------------------------------------------------------
-spec main_namespace() -> {atom(), binary()}.
main_namespace() ->
    {'xmlns:edm', <<"http://www.europeana.eu/schemas/edm/">>}.

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback elements/0
%%% @end
%%%-------------------------------------------------------------------
-spec elements() -> [binary()].
elements() -> [

    %% @TODO VFS-7454 implement schema validation
    <<"title">>,
    <<"creator">>,
    <<"subject">>,
    <<"description">>,
    <<"publisher">>,
    <<"contributor">>,
    <<"date">>,
    <<"type">>,
    <<"format">>,
    <<"identifier">>,
    <<"source">>,
    <<"language">>,
    <<"relation">>,
    <<"coverage">>,
    <<"rights">>
].


%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback sanitize_metadata/1
%%% @end
%%%-------------------------------------------------------------------
-spec sanitize_metadata(MetadataPrefix :: od_handle:metadata_prefix()) ->
    ok | errors:error().
sanitize_metadata(Metadata) ->
    try
        {#xmlElement{name = 'rdf:RDF',  content = Content}, _} = xmerl_scan:string(
            binary_to_list(Metadata), [{quiet, true}]),
        {ok, _}= ?find_matching_element(#xmlElement{name = 'edm:ProvidedCHO'}, Content),
        {ok, _}= ?find_matching_element(#xmlElement{name = 'ore:Aggregation'}, Content),
        ok
    catch Class:Reason:Stacktrace ->
        ?debug_exception(
            "Cannot parse dublin core metadata",
            Class, Reason, Stacktrace
        ),
        throw(?ERROR_BAD_VALUE_XML(Metadata))
    end.


%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback encode/2
%%% @end
%%%-------------------------------------------------------------------
-spec encode(binary(), AdditionalIdentifiers :: [binary()]) -> #xmlElement{}.
encode(Metadata, [Identifier]) ->
    %% @TODO VFS-7454 currently bare xml is saved in  handle
    %%    MetadataXML = lists:flatmap(fun(Key) ->
    %%        case maps:get(Key, Metadata, undefined) of
    %%            undefined -> [];
    %%            Value -> [#xmlElement{
    %%                name=binary_to_atom(<<"edm:", Key/binary>>, latin1),
    %%                content=[str_utils:to_list(Value)]}]
    %%        end
    %%    end, elements()),

    {#xmlElement{name = 'rdf:RDF', content = Content}, _} = xmerl_scan:string(
        binary_to_list(Metadata), [{quiet, true}]),
    %% Xmerl works on strings in UTF8 (essentially the result of binary_to_list(<<_/utf8>>),
    %% not unicode erlang-strings! However, its output IS expressed in unicode erlang-strings!
    %% This is why we need to transform the resulting unicode strings to UTF8
    %% strings before encoding and sending back to the client.
    MetadataContent = lists:map(fun
        (#xmlElement{content = [#xmlText{value = Value} = Text]} = Element) when is_list(Value) ->
            Element#xmlElement{content = [
                Text#xmlText{value = binary_to_list(str_utils:unicode_list_to_binary(Value))}
            ]};
        (Other) ->
            Other
    end, Content),

    MetadataContentWithIdentifiers = lists:map(fun
        (#xmlElement{
            name = 'edm:ProvidedCHO', content = CHOContent, attributes = CHOAttributes
        } = CHOElement) ->
            NewCHOAttributes = ensure_rdf_about_attribute(CHOAttributes, Identifier),
            NewCHOContent = ensure_dc_identifier_element(CHOContent, Identifier),
            CHOElement#xmlElement{content = NewCHOContent, attributes = NewCHOAttributes};
        (#xmlElement{
            name = 'ore:Aggregation', content = AggContent, attributes = AggAttributes
        } = AggElement) ->
            NewAggAttributes = ensure_rdf_about_attribute(
                AggAttributes, <<Identifier/binary, <<"_AGG">>/binary>>
            ),
            NewAggContent = ensure_aggregated_element(AggContent, Identifier),
            AggElement#xmlElement{content = NewAggContent, attributes = NewAggAttributes};
        (Other) ->
            Other
    end, MetadataContent),

    #xmlElement{
        name = 'rdf:RDF',
        attributes = extra_namespaces_attr(),
        content = MetadataContentWithIdentifiers
    }.


%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback resolve_additional_identifiers/1
%%% @end
%%%-------------------------------------------------------------------
-spec resolve_additional_identifiers(Handle :: od_handle:record()) -> [od_handle:public_handle()].
resolve_additional_identifiers(Handle) ->
    [Handle#od_handle.public_handle].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Returns list of extra XML namespaces for given metadata format.
%%% @end
%%%-------------------------------------------------------------------
-spec extra_namespaces() -> [{atom(), binary()}].
extra_namespaces() -> [
    {'xmlns:dc', <<"http://purl.org/dc/elements/1.1/">>},
    {'xmlns:dcterms', <<"http://purl.org/dc/terms/">>},
    {'xmlns:edm', <<"http://www.europeana.eu/schemas/edm/">>},
    {'xmlns:ore', <<"http://www.openarchives.org/ore/terms/">>},
    {'xmlns:owl', <<"http://www.w3.org/2002/07/owl#">>},
    {'xmlns:rdf', <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>},
    {'xmlns:foaf', <<"http://xmlns.com/foaf/0.1/">>},
    {'xmlns:skos', <<"http://www.w3.org/2004/02/skos/core#">>},
    {'xmlns:rdaGr2', <<"http://rdvocab.info/ElementsGr2/">>},
    {'xmlns:wgs84_pos', <<"http://www.w3.org/2003/01/geo/wgs84_pos#">>},
    {'xmlns:crm', <<"http://www.cidoc-crm.org/cidoc--crm/">>},
    {'xmlns:cc', <<"http://creativecommons.org/ns#">>}

].

%% @private
-spec ensure_rdf_about_attribute([#xmlAttribute{}], binary()) -> [#xmlAttribute{}].
ensure_rdf_about_attribute(Attributes, Identifier) ->
    FindElementResult = ?find_matching_element(#xmlAttribute{name = 'rdf:about'}, Attributes),
    case FindElementResult of
        error ->
            NewAttr = #xmlAttribute{
                name = 'rdf:about',
                value = Identifier
            },
            lists:append([NewAttr], Attributes);
        {ok, Found} ->
            lists_utils:replace(Found, Found#xmlAttribute{value = Identifier}, Attributes)
    end.

%% @private
-spec ensure_dc_identifier_element([#xmlElement{}], binary()) -> [#xmlElement{}].
ensure_dc_identifier_element(CHOContent, Identifier) ->
    FindElementResult = ?find_matching_element(#xmlElement{name = 'dc:identifier'}, CHOContent),
    case FindElementResult of
        error ->
            lists:append(CHOContent, [#xmlElement{name = 'dc:identifier', content = [#xmlText{value = Identifier}]}]);
        {ok, Found} ->
            lists_utils:replace(Found, Found#xmlElement{content = [#xmlText{value = Identifier}]}, CHOContent)
    end.


%% @private
-spec ensure_aggregated_element([#xmlElement{}], binary()) -> [#xmlElement{}].
ensure_aggregated_element(Content, Identifier) ->
    FindElementResult = ?find_matching_element(#xmlElement{name = 'edm:aggregatedCHO'}, Content),
    NewAttr = #xmlAttribute{
        name = 'rdf:resource',
        value = Identifier
    },
    case FindElementResult of
        error ->
            lists:append(Content, [#xmlElement{name = 'edm:aggregatedCHO', attributes = [NewAttr]}]);
        {ok, Found} ->
            lists_utils:replace(Found, Found#xmlElement{attributes = [NewAttr]}, Content)
    end.


%% @private
-spec extra_namespaces_attr() -> [#xmlAttribute{}].
extra_namespaces_attr() ->
    lists:map(fun({Name, Value}) ->
        #xmlAttribute{
            name = Name,
            value = str_utils:to_list(Value)}
    end, extra_namespaces()).