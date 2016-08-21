%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(location_test_SUITE).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([
    restarts_port_after_failure/1,
    claim_given_id_test/1,
    claim_any_id_test/1
]).

%% appends function name to id (atom) and yields binary
-define(ID(Id), list_to_binary(
    atom_to_list(Id) ++ "_" ++
        atom_to_list(element(2, element(2, process_info(self(), current_function))))
)).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    claim_given_id_test,
    claim_any_id_test,
    restarts_port_after_failure
]).

restarts_port_after_failure(Config) ->
    %% given
    [Node1, Node2, Node3 | _] = Nodes = ?config(oz_worker_nodes, Config),
    Host1 = get_hostname(Node1),
    Host2 = get_hostname(Node2),
    ID1 = ?ID('id1'),
    ID2 = ?ID('id2'),

    ?assertMatch({ok, ID1}, claim_id(Node1, ID1)),
    [?assertMatch({ok, Host1}, resolve_id(Node, ID1)) || Node <- Nodes],
    [?assertMatch({error, _}, resolve_id(Node, ID2)) || Node <- Nodes],

    [rpc:call(Node, os, cmd, "pkill node") || Node <- Nodes],

    ?assertMatch({ok, ID2}, claim_id(Node2, ID2)),
    [?assertMatch({ok, Host1}, resolve_id(Node, ID1)) || Node <- Nodes],
    [?assertMatch({ok, Host2}, resolve_id(Node, ID2)) || Node <- Nodes],
    ok.

claim_given_id_test(Config) ->
    %% given
    [Node1, Node2, Node3 | _] = ?config(oz_worker_nodes, Config),
    Host1 = get_hostname(Node1),
    Host2 = get_hostname(Node2),
    Host3 = get_hostname(Node3),
    ID1 = ?ID('id1'),
    ID2 = ?ID('id2'),
    ID3 = ?ID('id3'),

    %% when
    ?assertMatch({ok, ID1}, claim_id(Node1, ID1)),
    ?assertMatch({ok, ID2}, claim_id(Node2, ID2)),
    ?assertMatch({ok, ID3}, claim_id(Node3, ID3)),

    %% then - subsequent claim succeeds
    ?assertMatch({ok, ID1}, claim_id(Node1, ID1)),
    ?assertMatch({ok, ID2}, claim_id(Node2, ID2)),
    ?assertMatch({ok, ID3}, claim_id(Node3, ID3)),

    %% then - claim conflict detected
    ?assertMatch({error, _}, claim_id(Node2, ID1)),
    ?assertMatch({error, _}, claim_id(Node3, ID1)),
    ?assertMatch({error, _}, claim_id(Node1, ID2)),
    ?assertMatch({error, _}, claim_id(Node3, ID2)),
    ?assertMatch({error, _}, claim_id(Node1, ID3)),
    ?assertMatch({error, _}, claim_id(Node2, ID3)),

    %% then - coherent state
    ?assertMatch({ok, Host1}, resolve_id(Node1, ID1)),
    ?assertMatch({ok, Host1}, resolve_id(Node2, ID1)),
    ?assertMatch({ok, Host1}, resolve_id(Node3, ID1)),
    ?assertMatch({ok, Host2}, resolve_id(Node1, ID2)),
    ?assertMatch({ok, Host2}, resolve_id(Node2, ID2)),
    ?assertMatch({ok, Host2}, resolve_id(Node3, ID2)),
    ?assertMatch({ok, Host3}, resolve_id(Node1, ID3)),
    ?assertMatch({ok, Host3}, resolve_id(Node2, ID3)),
    ?assertMatch({ok, Host3}, resolve_id(Node3, ID3)),
    ok.

claim_any_id_test(Config) ->
    %% given
    [Node1, Node2, Node3 | _] = ?config(oz_worker_nodes, Config),
    Host1 = get_hostname(Node1),
    Host2 = get_hostname(Node2),
    Host3 = get_hostname(Node3),

    %% when
    Claim1 = claim_id(Node1),
    Claim2 = claim_id(Node2),
    Claim3 = claim_id(Node3),
    ?assertMatch({ok, _}, Claim1),
    ?assertMatch({ok, _}, Claim2),
    ?assertMatch({ok, _}, Claim3),
    {ok, ID1} = Claim1,
    {ok, ID2} = Claim2,
    {ok, ID3} = Claim3,

    %% then - subsequent claim succeeds
    ?assertMatch({ok, ID1}, claim_id(Node1, ID1)),
    ?assertMatch({ok, ID2}, claim_id(Node2, ID2)),
    ?assertMatch({ok, ID3}, claim_id(Node3, ID3)),

    %% then - claim conflict detected
    ?assertMatch({error, _}, claim_id(Node2, ID1)),
    ?assertMatch({error, _}, claim_id(Node3, ID1)),
    ?assertMatch({error, _}, claim_id(Node1, ID2)),
    ?assertMatch({error, _}, claim_id(Node3, ID2)),
    ?assertMatch({error, _}, claim_id(Node1, ID3)),
    ?assertMatch({error, _}, claim_id(Node2, ID3)),

    %% then - coherent state
    ?assertMatch({ok, Host1}, resolve_id(Node1, ID1)),
    ?assertMatch({ok, Host1}, resolve_id(Node2, ID1)),
    ?assertMatch({ok, Host1}, resolve_id(Node3, ID1)),
    ?assertMatch({ok, Host2}, resolve_id(Node1, ID2)),
    ?assertMatch({ok, Host2}, resolve_id(Node2, ID2)),
    ?assertMatch({ok, Host2}, resolve_id(Node3, ID2)),
    ?assertMatch({ok, Host3}, resolve_id(Node1, ID3)),
    ?assertMatch({ok, Host3}, resolve_id(Node2, ID3)),
    ?assertMatch({ok, Host3}, resolve_id(Node3, ID3)),
    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    EnvDescFile = ?TEST_FILE(Config, "env_desc.json"),
    Temp = utils:mkdtemp(),
    Adjusted = location_service_test_utils:adjust_env_desc(Temp, EnvDescFile),
    location_service_test_utils:start(),

    RunningConfig = ?TEST_INIT(Config, Adjusted),
    utils:rmtempdir(Temp),
    RunningConfig.

init_per_testcase(_, _Config) ->
    _Config.

end_per_testcase(_, _Config) ->
    ok.

end_per_suite(Config) ->
    location_service_test_utils:stop(),
    test_node_starter:clean_environment(Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================

claim_id(Node) ->
    rpc:call(Node, locations, claim_id, []).

claim_id(Node, ID) ->
    rpc:call(Node, locations, claim_id, [ID]).

resolve_id(Node, ID) ->
    rpc:call(Node, locations, resolve_id, [ID]).

get_hostname(Node) ->
    {ok, Host} = rpc:call(Node, application, get_env, [oz_worker, http_domain]),
    list_to_binary(Host).