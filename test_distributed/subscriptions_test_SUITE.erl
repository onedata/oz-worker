%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions_test_SUITE).
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
-export([space_changes_after_subscription/1]).

-define(getFirstSeq(W, Config),
    begin
        {_, LastSeqInDb, _} = ?assertMatch(
            {ok, _, _},
            rpc:call(W, couchdb_datastore_driver, db_run,
                [couchbeam_changes, follow_once, [], 3])
        ),
        binary_to_integer(LastSeqInDb)
    end).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([space_changes_after_subscription]).

space_changes_after_subscription(Config) ->
    [Node1, _] = ?config(oz_worker_nodes, Config),
    ProviderID = <<"provider_id_1">>,

    subscribe(Node1, ProviderID, 0, "provider_endpoint_1"),

    SpaceKey = <<"space_key">>,
    SpaceDoc = #document{key = SpaceKey, value = #space{name = <<"space">>, providers = [ProviderID]}},
    ?assertEqual({ok, SpaceKey}, rpc:call(Node1, space, save, [SpaceDoc])),

    SpaceKey2 = <<"space_key2">>,
    SpaceDoc2 = #document{key = SpaceKey2, value = #space{name = <<"space2">>, providers = [ProviderID]}},
    ?assertEqual({ok, SpaceKey2}, rpc:call(Node1, space, save, [SpaceDoc2])),

    verify_sent(Node1, "provider_endpoint_1", <<"{\"seq\":2,\"space\":{\"id\":\"space_key\",\"name\":\"space\",\"groups\":[],\"users\":[]}}">>),
    verify_sent(Node1, "provider_endpoint_1", <<"{\"seq\":3,\"space\":{\"id\":\"space_key2\",\"name\":\"space2\",\"groups\":[],\"users\":[]}}">>),
    ok.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    NewConfig.

init_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, http_client),
    test_utils:mock_expect(Nodes, http_client, post, fun(_Address, _Options, _Body) -> ok end),
    Config.

end_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, http_client),
    ok.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================

subscribe(Node1, ProviderID, LastSeen, Endpoint) ->
    ?assertNotMatch({badrpc, _}, rpc:call(Node1, provider_subscriptions, renew, [ProviderID, LastSeen, Endpoint])).

verify_sent(Node, Endpoint, Body) ->
    verify_received(Node, Endpoint, Body, 50).

verify_received(Node, Endpoint, Body, 0) ->
    ?assert({error, timeout_for, Node, Endpoint, Body});

verify_received(Node, Endpoint, Body, Retries) ->
    Result = mock_capture(Node, [first, http_client, post, [Endpoint, '_', Body], 3]),
    case Result of
        {badrpc, _} ->
            timer:sleep(500),
            verify_received(Node, Endpoint, Body, Retries - 1);
        _ ->
            ok
    end.

mock_capture(Node, Args) ->
    rpc:call(Node, meck, capture, Args).