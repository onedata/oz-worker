%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements onezone_plugin_behaviour and harvester_plugin_behaviour 
%%% and is called by harvester_logic_plugin to handle application specific to harvester logic.
%%% @end
%%%-------------------------------------------------------------------
-module(elastic_search_plugin).
-author("Michal Stanisz").

-include_lib("ctool/include/logging.hrl").

-export([type/0]).
-export([submit_entry/4, delete_entry/3, query/3]).

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
-spec submit_entry(Endpoint :: binary(), IndexId :: binary(), Id :: binary(), Data :: binary()) -> 
    ok | {error, term()}.
submit_entry(Endpoint, IndexId, Id, Data) ->
    do_request(Endpoint, IndexId, Id, Data, put).


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback delete_entry/3.
%% @end
%%--------------------------------------------------------------------
-spec delete_entry(Endpoint :: binary(), IndexId :: binary(), Id :: binary()) ->
    ok | {error, term()}.
delete_entry(Endpoint, IndexId, Id) ->
    do_request(Endpoint, IndexId, Id, <<>>, delete).


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback query/3.
%% @end
%%--------------------------------------------------------------------
-spec query(Endpoint :: binary(), IndexId :: binary(), Req :: binary()) ->
    {ok, binary()} | {error, term()}.
query(Endpoint, IndexId, Req) ->
    case http_client:get(<<Endpoint/binary, "/", IndexId/binary, "/", Req/binary>>) of
        {ok, Code, Headers, Body} ->
            {ok, json_utils:encode(#{
                <<"code">> => Code,
                <<"headers">> => Headers,
                <<"body">> => Body
            })};
        Error -> 
            Error
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes request to elastic search.
%% @end
%%--------------------------------------------------------------------
-spec do_request(Endpoint :: binary(), IndexId :: binary(), Id :: binary(), 
    Data :: binary(), Request :: put | delete) -> ok.
do_request(Endpoint, IndexId, Id, Data, Request) ->
    Url = <<Endpoint/binary, "/", IndexId/binary, "/_doc/", Id/binary>>,
    case http_client:Request(Url, #{<<"content-type">> => <<"application/json">>}, Data) of
        {ok, Code, _Headers, _Body} when ((Code >= 200) and (Code < 300)) ->
            ok;
        {ok, Code, _Headers, Body} ->
            ?warning("~p ~p in ~p ended with status code ~p: ~p", [Request, Url, ?MODULE, Code, Body]);
        Error ->
            ?error("~p in ~p was unsuccessful due to ~p", [Request, ?MODULE, Error])
    end.