%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions used in oz-worker CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt).
-author("Lukasz Opiola").

-include("ozt.hrl").

-define(OZT_MODULES, [
    oz_test_utils, % Load the old utils until the migration is complete
    ozt, ozt_http, ozt_gs, ozt_tokens, ozt_mocks,
    ozt_users, ozt_groups, ozt_spaces, ozt_providers, ozt_handle_services,
    ozt_clusters, ozt_harvesters
]).

-type ct_test_config() :: term().

%% API
-export([init_per_suite/1, get_test_config/0]).
-export([rpc/3, rpc/4]).
-export([cluster_time_seconds/0]).
-export([reconcile_entity_graph/0]).
-export([delete_all_entities/0]).
-export([get_env/1, get_env/2, set_env/2, set_app_env/3]).
-export([get_domain/0, get_nodes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% NOTE: must be called in SUITE's init_per_suite in order to use the ozt*
%% modules in CT tests. Stores the test Config so that it doesn't have to be
%% passed around in test code and adds options to load of the ozt* modules.
%% @end
%%--------------------------------------------------------------------
-spec init_per_suite(ct_test_config()) -> ct_test_config().
init_per_suite(Config) ->
    Posthook = fun(NewConfig) ->
        simple_cache:put(ct_test_config, NewConfig),
        NewConfig
    end,
    ModulesToLoad = ?OZT_MODULES ++ proplists:get_value(?LOAD_MODULES, Config, []),
    [{?ENV_UP_POSTHOOK, Posthook}, {?LOAD_MODULES, ModulesToLoad} | proplists:delete(?LOAD_MODULES, Config)].


-spec get_test_config() -> ct_test_config().
get_test_config() ->
    case simple_cache:get(ct_test_config) of
        {ok, Config} -> Config;
        _ -> error(str_utils:format("Call ~s:init_per_suite/1 at the beggining of the test.", [?MODULE]))
    end.


-spec rpc(module(), Function :: atom(), Args :: [term()]) -> term().
rpc(Module, Function, Args) ->
    Node = lists_utils:random_element(get_nodes()),
    rpc(Node, Module, Function, Args).

-spec rpc(node() | [node()], module(), Function :: atom(), Args :: [term()]) -> term().
rpc(Nodes, Module, Function, Args) when is_list(Nodes) ->
    [rpc(Node, Module, Function, Args) || Node <- Nodes];
rpc(Node, Module, Function, Args) ->
    FunWrapper = fun() ->
        try
            erlang:apply(Module, Function, Args)
        catch Type:Reason ->
            {crash, Type, Reason, lager:pr_stacktrace(erlang:get_stacktrace())}
        end
    end,
    case rpc:call(Node, erlang, apply, [FunWrapper, []]) of
        {crash, Type, Reason, Stacktrace} ->
            ct:pal(
                "RPC call to oz-worker crashed!~n"
                "Node: ~p~n"
                "Module: ~p~n"
                "Function: ~p~n"
                "Args: ~p~n"
                "Error: ~p:~p~n"
                "Stacktrace: ~s",
                [Node, Module, Function, Args, Type, Reason, Stacktrace]
            ),
            {error, {badrpc, Reason}};
        {badrpc, Reason} ->
            ct:pal(
                "badrpc in call to oz-worker!~n"
                "Node: ~p~n"
                "Module: ~p~n"
                "Function: ~p~n"
                "Args: ~p~n"
                "Reason: ~p~n",
                [Node, Module, Function, Args, Reason]
            ),
            {error, {badrpc, Reason}};
        Result ->
            Result
    end.


-spec cluster_time_seconds() -> time_utils:seconds().
cluster_time_seconds() ->
    rpc(time_utils, cluster_time_seconds, []).


-spec reconcile_entity_graph() -> true.
reconcile_entity_graph() ->
    ?assertMatch(true, rpc(entity_graph, ensure_up_to_date, []), 60).


-spec delete_all_entities() -> ok.
delete_all_entities() ->
    oz_test_utils:delete_all_entities(get_test_config()).


-spec get_env(Name :: atom()) -> term().
get_env(Name) ->
    rpc(oz_worker, get_env, [Name]).


-spec get_env(Name :: atom(), Default :: term()) -> term().
get_env(Name, Default) ->
    rpc(oz_worker, get_env, [Name, Default]).


-spec set_env(Name :: atom(), Value :: term()) -> ok.
set_env(Name, Value) ->
    rpc(get_nodes(), oz_worker, set_env, [Name, Value]),
    ok.


-spec set_app_env(App :: atom(), Name :: atom(), Value :: term()) -> ok.
set_app_env(App, Name, Value) ->
    rpc(get_nodes(), application, set_env, [App, Name, Value]),
    ok.


-spec get_domain() -> binary().
get_domain() ->
    rpc(oz_worker, get_domain, []).


-spec get_nodes() -> [node()].
get_nodes() ->
    ?config(oz_worker_nodes, get_test_config()).