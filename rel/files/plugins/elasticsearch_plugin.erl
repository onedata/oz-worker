%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements onezone_plugin_behaviour and harvester_plugin_behaviour 
%%% and is called by harvester_logic_plugin to handle operations on Elasticsearch.
%%% This plugin conforms to Elasticsearch 6.x
%%% @end
%%%-------------------------------------------------------------------
-module(elasticsearch_plugin).
-author("Michal Stanisz").

-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/logging.hrl").

-export([type/0]).
-export([submit_entry/4, delete_entry/3, query/3, query_validator/0]).

-behaviour(onezone_plugin_behaviour).
-behaviour(harvester_plugin_behaviour).

-define(INDEX_ID(HarvesterId), string:lowercase(HarvesterId)).


%%--------------------------------------------------------------------
%% @doc
%% {@link onezone_plugin_behaviour} callback type/0.
%% @end
%%--------------------------------------------------------------------
type() ->
    harvester_plugin.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback submit_entry/4.
%% @end
%%--------------------------------------------------------------------
-spec submit_entry(Endpoint :: binary(), HarvesterId :: binary(), 
    Id :: binary(), Data :: binary()) -> ok | {error, term()}.
submit_entry(Endpoint, HarvesterId, Id, Data) ->
    case do_request(Endpoint, ?INDEX_ID(HarvesterId), Id, Data, put, [{200,300}]) of
        {ok,_,_,_} -> ok;
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback delete_entry/3.
%% @end
%%--------------------------------------------------------------------
-spec delete_entry(Endpoint :: binary(), HarvesterId :: binary(), 
    Id :: binary()) -> ok | {error, term()}.
delete_entry(Endpoint, HarvesterId, Id) ->
    case do_request(Endpoint, ?INDEX_ID(HarvesterId), Id, <<>>, delete, [{200,300}, 404]) of
        {ok,_,_,_} -> ok;
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback query/3.
%% @end
%%--------------------------------------------------------------------
-spec query(Endpoint :: binary(), HarvesterId :: binary(), Data :: #{}) ->
    {ok, maps:map()} | {error, term()}.
query(Endpoint, HarvesterId, Data) ->
    #{
        <<"method">> := Method, 
        <<"path">> := Path
    } = Data,
    Body = maps:get(<<"body">>, Data, <<>>),
    case do_request(Endpoint, ?INDEX_ID(HarvesterId), Path, Body, Method) of
        {ok, Code, Headers, ResponseBody} ->
            {ok, #{
                <<"code">> => Code,
                <<"headers">> => Headers,
                <<"body">> => ResponseBody
            }};
        {error, _} = Error -> 
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback query_validator/0.
%% @end
%%--------------------------------------------------------------------
-spec query_validator() -> #{}.
query_validator() -> #{
    required => #{
        <<"method">> => {atom, [post, get]},
        <<"path">> => {binary, non_empty}
    },
    optional => #{
        <<"body">> => {binary, non_empty}
    }
}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes request to elastic search.
%% Received status code will NOT be checked.
%% @end
%%--------------------------------------------------------------------
-spec do_request(Endpoint :: binary(), IndexId :: binary(), Path :: binary(), 
    Data :: binary(), Request :: http_client:method()) -> ok.
do_request(Endpoint, IndexId, Path, Data, Method) ->
    do_request(Endpoint, IndexId, Path, Data, Method, undefined).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes request to elastic search.
%% When ExpectedCodes lists is provided returns error if response has wrong status code. 
%% ExpectedCodes list must conform to specification in is_code_expected/2.
%% @end
%%--------------------------------------------------------------------
-spec do_request(Endpoint :: binary(), IndexId :: binary(), Path :: binary(), 
    Data :: binary(), Request :: http_client:method(), 
    ExpectedCodes :: [integer() | {integer(), integer()}] | undefined) -> ok.
do_request(Endpoint, IndexId, Path, Data, Method, ExpectedCodes) ->
    Url = <<Endpoint/binary, "/", IndexId/binary, "/_doc/", Path/binary>>,
    case http_client:Method(Url, #{<<"content-type">> => <<"application/json">>}, Data) of
        {ok, Code, Headers, Body} = Response when is_list(ExpectedCodes) ->
            case is_code_expected(Code, ExpectedCodes) of
                true -> 
                    Response;
                _ ->
                    ?debug("~p ~p in harvester ~p returned unexpected response ~p:~n ~p~n~p",
                        [Method, Url, IndexId, Code, Headers, json_utils:decode(Body)]),
                    ?ERROR_BAD_DATA(<<"payload">>)
            end;
        {ok,_,_,_} = Response ->
            Response;
        {error, _} = Error ->
            ?error("~p in harvester ~p was unsuccessful due to ~p", [Method, IndexId, Error]),
            ?ERROR_TEMPORARY_FAILURE
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Predicate saying whether given Code is expected.
%% Each element of ExpectedCodes list can be in form of:
%% * ExpectedCode - represents that given status code is expected;
%% * {Begin, End} - represents range from Begin(inclusive) to End(exclusive) 
%%                  of expected status codes.
%% @end
%%--------------------------------------------------------------------
-spec is_code_expected(integer(), [integer() | {integer(), integer()}]) -> boolean().
is_code_expected(Code, ExpectedCodes) ->
    lists:any(fun({B, E}) -> (Code >= B) and (Code < E);
                 (ECode) -> Code =:= ECode
    end, ExpectedCodes).