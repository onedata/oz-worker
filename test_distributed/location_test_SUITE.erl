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
-export([all/0]).
-export([
    restarts_port_after_failure/1,
    location_of_records_are_set_upon_creation/1,
    resolve_is_consistent_with_claim/1
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
    resolve_is_consistent_with_claim,
    location_of_records_are_set_upon_creation,
    restarts_port_after_failure
]).

restarts_port_after_failure(Config) ->
    %% given
    [Node1, Node2, Node3 | _] = Nodes = ?config(oz_worker_nodes, Config),
    Host1 = get_hostname(Node1),
    Host2 = get_hostname(Node2),
    ID1 = ?ID('id1'),
    ID2 = ?ID('id2'),

    ?assertMatch(ok, claim(Node1, ID1)),
    [?assertMatch({ok, Host1}, resolve(Node, ID1)) || Node <- Nodes],
    [?assertMatch({error, _}, resolve(Node, ID2)) || Node <- Nodes],

    [rpc:call(Node, os, cmd, "pkill node") || Node <- Nodes],

    ?assertMatch(ok, claim(Node2, ID2)),
    [?assertMatch({ok, Host1}, resolve(Node, ID1)) || Node <- Nodes],
    [?assertMatch({ok, Host2}, resolve(Node, ID2)) || Node <- Nodes],
    ok.

resolve_is_consistent_with_claim(Config) ->
    %% given
    [Node1, Node2, Node3 | _] = ?config(oz_worker_nodes, Config),
    Host1 = get_hostname(Node1),
    Host2 = get_hostname(Node2),
    Host3 = get_hostname(Node3),
    ID1 = ?ID('id1'),
    ID2 = ?ID('id2'),
    ID3 = ?ID('id3'),

    %% when
    ?assertMatch(ok, claim(Node1, ID1)),
    ?assertMatch(ok, claim(Node2, ID2)),
    ?assertMatch(ok, claim(Node3, ID3)),

    %% then - subsequent claim succeeds
    ?assertMatch(ok, claim(Node1, ID1)),
    ?assertMatch(ok, claim(Node2, ID2)),
    ?assertMatch(ok, claim(Node3, ID3)),

    %% then - claim conflict detected
    ?assertMatch({error, {'CONFLICT', _}}, claim(Node2, ID1)),
    ?assertMatch({error, {'CONFLICT', _}}, claim(Node2, ID1)),
    ?assertMatch({error, {'CONFLICT', _}}, claim(Node1, ID2)),
    ?assertMatch({error, {'CONFLICT', _}}, claim(Node3, ID2)),
    ?assertMatch({error, {'CONFLICT', _}}, claim(Node1, ID3)),
    ?assertMatch({error, {'CONFLICT', _}}, claim(Node2, ID3)),

    %% then - coherent state
    ?assertMatch({ok, Host1}, resolve(Node1, ID1)),
    ?assertMatch({ok, Host1}, resolve(Node2, ID1)),
    ?assertMatch({ok, Host1}, resolve(Node3, ID1)),
    ?assertMatch({ok, Host2}, resolve(Node1, ID2)),
    ?assertMatch({ok, Host2}, resolve(Node2, ID2)),
    ?assertMatch({ok, Host2}, resolve(Node3, ID2)),
    ?assertMatch({ok, Host3}, resolve(Node1, ID3)),
    ?assertMatch({ok, Host3}, resolve(Node2, ID3)),
    ?assertMatch({ok, Host3}, resolve(Node3, ID3)),
    ok.

location_of_records_are_set_upon_creation(Config) ->
    %% given
    [Node1, Node2, Node3 | _] = ?config(oz_worker_nodes, Config),
    Host1 = get_hostname(Node1),
    Host2 = get_hostname(Node2),
    Host3 = get_hostname(Node3),

    %% when
    ID1 = create(Node1, #od_user{}),
    ID2 = create(Node2, #od_group{}),
    ID3 = create(Node3, #od_provider{}),
    ID4 = create(Node1, #od_space{}),

    %% then
    ?assertMatch({ok, Host1}, resolve(Node1, od_user, ID1)),
    ?assertMatch({ok, Host1}, resolve(Node2, od_user, ID1)),
    ?assertMatch({ok, Host1}, resolve(Node3, od_user, ID1)),
    ?assertMatch({ok, Host2}, resolve(Node1, od_group, ID2)),
    ?assertMatch({ok, Host2}, resolve(Node2, od_group, ID2)),
    ?assertMatch({ok, Host2}, resolve(Node3, od_group, ID2)),
    ?assertMatch({ok, Host3}, resolve(Node1, od_provider, ID3)),
    ?assertMatch({ok, Host3}, resolve(Node2, od_provider, ID3)),
    ?assertMatch({ok, Host3}, resolve(Node3, od_provider, ID3)),
    ?assertMatch({ok, Host1}, resolve(Node1, od_space, ID4)),
    ?assertMatch({ok, Host1}, resolve(Node2, od_space, ID4)),
    ?assertMatch({ok, Host1}, resolve(Node3, od_space, ID4)),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

create(Node, Record) ->
    Result = rpc:call(Node, element(1, Record), create, [#document{value = Record}]),
    ?assertMatch({ok, _}, Result),
    {ok, ID1} = Result,
    ID1.

claim(Node, ID) ->
    rpc:call(Node, locations, claim, [test, ID]).

resolve(Node, ID) ->
    resolve(Node, test, ID).
resolve(Node, Namespace, ID) ->
    rpc:call(Node, locations, resolve, [Namespace, ID]).

get_hostname(Node) ->
    {ok, Host} = rpc:call(Node, application, get_env, [oz_worker, http_domain]),
    list_to_binary(Host).