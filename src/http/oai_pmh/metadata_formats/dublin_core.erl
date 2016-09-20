%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This module is responsible for encoding metadata to Dublin Core format.
%%% It implements metadata_format_behaviour.
%%% @end
%%%-------------------------------------------------------------------
-module(dublin_core).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").

-behaviour(metadata_format_behaviour).

%% API
-export([elements/0, encode/1, metadata_prefix/0, schema_URL/0,
        extra_namespaces/0, schema_location/0, main_namespace/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback metadata_prefix/0
%%% @end
%%%-------------------------------------------------------------------
-spec metadata_prefix() -> binary().
metadata_prefix() -> <<"oai_dc">>.

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback schema_URL/0
%%% @end
%%%-------------------------------------------------------------------
-spec schema_URL() -> binary().
schema_URL() -> <<"http://www.openarchives.org/OAI/2.0/oai_dc.xsd">>.

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback main_namespace/0
%%% @end
%%%-------------------------------------------------------------------
-spec main_namespace() -> {atom(), binary()}.
main_namespace() ->
    {'xmlns:oai_dc', <<"http://www.openarchives.org/OAI/2.0/oai_dc/">>}.

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback extra_namespaces/0
%%% @end
%%%-------------------------------------------------------------------
-spec extra_namespaces() -> [{atom(), binary()}].
extra_namespaces() -> [
    {'xmlns:dc', <<"http://purl.org/dc/elements/1.1/">>}
].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback schema_location/0
%%% @end
%%%-------------------------------------------------------------------
-spec schema_location() -> binary().
schema_location() ->
    {_, MainNamespace} = main_namespace(),
    str_utils:format_bin("~s ~s", [MainNamespace, schema_URL()]).

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link metadata_format_behaviour} callback elements/0
%%% @end
%%%-------------------------------------------------------------------
-spec elements() -> [binary()].
elements() -> [
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
%%% {@link metadata_format_behaviour} callback encode/0
%%% @end
%%%-------------------------------------------------------------------
-spec encode(#{} | binary()) -> #xmlElement{}.
encode(Metadata) ->

    %todo currently bare xml is saved
    {#xmlElement{content=MetadataContent}, _} = xmerl_scan:string(binary_to_list(Metadata)),
    %%    MetadataXML = lists:flatmap(fun(Key) ->
    %%        case maps:get(Key, Metadata, undefined) of
    %%            undefined -> [];
    %%            Value -> [#xmlElement{
    %%                name=binary_to_atom(<<"dc:", Key/binary>>, latin1),
    %%                content=[str_utils:to_list(Value)]}]
    %%        end
    %%    end, elements()),

    #xmlElement{
        name = 'oai_dc:dc',
        attributes = get_attributes(),
        content = MetadataContent}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Gets XML attributes for metadata element.
%%% @end
%%%-------------------------------------------------------------------
-spec get_attributes() -> [#xmlAttribute{}].
get_attributes() ->
    [main_namespace_attr()] ++
        extra_namespaces_attr() ++
        [schema_url_attr()] ++
        [schema_location_attr()]
.

-spec main_namespace_attr() -> #xmlAttribute{}.
main_namespace_attr() ->
    {Name, Value} = main_namespace(),
    #xmlAttribute{
        name = Name,
        value = str_utils:to_list(Value)
    }.


-spec extra_namespaces_attr() -> [#xmlAttribute{}].
extra_namespaces_attr() ->
    lists:map(fun({Name, Value}) ->
        #xmlAttribute{
            name = Name,
            value = str_utils:to_list(Value)}
    end, extra_namespaces()).

-spec schema_url_attr() -> #xmlAttribute{}.
schema_url_attr() ->
    ?OAI_XML_SCHEMA_NAMESPACE.

-spec schema_location_attr() -> #xmlAttribute{}.
schema_location_attr() ->
    Value = schema_location(),
    #xmlAttribute{
        name = 'xsi:schemaLocation',
        value = str_utils:to_list(Value)
    }.
