%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements onezone_plugin_behaviour and harvester_plugin_behaviour 
%%% and is called by harvester_logic_plugin to handle operations on Elastic Search.
%%% This plugin conforms to Elastic Search 6.x
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
-spec submit_entry(Endpoint :: binary(), IndexId :: binary(), Id :: binary(), Data :: binary()) -> ok.
submit_entry(Endpoint, IndexId, Id, Data) ->
    do_request(Endpoint, IndexId, Id, Data, put, true),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback delete_entry/3.
%% @end
%%--------------------------------------------------------------------
-spec delete_entry(Endpoint :: binary(), IndexId :: binary(), Id :: binary()) -> ok.
delete_entry(Endpoint, IndexId, Id) ->
    do_request(Endpoint, IndexId, Id, <<>>, delete, true),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback query/3.
%% @end
%%--------------------------------------------------------------------
-spec query(Endpoint :: binary(), IndexId :: binary(), Data :: #{}) ->
    {ok, binary()} | {error, term()}.
query(Endpoint, IndexId, Data) ->
    Method = maps:get(<<"method">>, Data),
    Path = maps:get(<<"path">>, Data),
    Body = maps:get(<<"body">>, Data, <<>>),
    case do_request(Endpoint, IndexId, Path, Body, Method, false) of
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
%% When verbose logs responses with status code other than 2xx.
%% @end
%%--------------------------------------------------------------------
-spec do_request(Endpoint :: binary(), IndexId :: binary(), Path :: binary(), 
    Data :: binary(), Request :: http_client:method(), Verbose :: boolean()) -> ok.
do_request(Endpoint, IndexId, Path, Data, Method, Verbose) ->
    Url = <<Endpoint/binary, "/", IndexId/binary, "/_doc/", Path/binary>>,
    case http_client:Method(Url, #{<<"content-type">> => <<"application/json">>}, Data) of
        {ok, Code, Headers, Body} = Response when Verbose and (((Code < 200) or (Code >= 300))) ->
            ?warning("~p ~p in harvester ~p ended with status code ~p:\n ~p\n~p", 
                [Method, Url, IndexId, Code, Headers, json_utils:decode(Body)]),
            Response;
        {ok, _,_,_} = Response ->
            Response;
        {error, _} = Error ->
            ?error("~p in harvester ~p was unsuccessful due to ~p", [Method, IndexId, Error]),
            Error
    end.
