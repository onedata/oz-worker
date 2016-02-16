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
-export([space_changes_after_subscription/1, space_changes_before_subscription/1]).

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

all() -> ?ALL([space_changes_after_subscription, space_changes_before_subscription]).

space_changes_after_subscription(Config) ->
    [Node1, _] = ?config(oz_worker_nodes, Config),
    ProviderID1 = <<"provider1">>,
    ProviderID2 = <<"provider2">>,
    First = ?getFirstSeq(Node1, Config),

    subscribe(Node1, ProviderID1, 0, "endpoint1"),
    subscribe(Node1, ProviderID2, 0, "endpoint2"),

    SpaceKey1 = <<"spacekey1">>,
    SpaceDoc1 = #document{key = SpaceKey1, value = #space{name = <<"space1">>, providers = [ProviderID1]}},
    ?assertEqual({ok, SpaceKey1}, rpc:call(Node1, space, save, [SpaceDoc1])),

    SpaceKey2 = <<"spacekey2">>,
    SpaceDoc2 = #document{key = SpaceKey2, value = #space{name = <<"space2">>, providers = [ProviderID1, ProviderID2]}},
    ?assertEqual({ok, SpaceKey2}, rpc:call(Node1, space, save, [SpaceDoc2])),

    SpaceKey3 = <<"spacekey3">>,
    SpaceDoc3 = #document{key = SpaceKey3, value = #space{name = <<"space3">>, providers = []}},
    ?assertEqual({ok, SpaceKey3}, rpc:call(Node1, space, save, [SpaceDoc3])),

    SpaceKey4 = <<"spacekey4">>,
    SpaceDoc4 = #document{key = SpaceKey4, value = #space{name = <<"space4">>, providers = [ProviderID1, ProviderID2]}},
    ?assertEqual({ok, SpaceKey4}, rpc:call(Node1, space, save, [SpaceDoc4])),

    SpaceKey5 = <<"spacekey5">>,
    SpaceDoc5 = #document{key = SpaceKey5, value = #space{name = <<"space5">>, providers = [ProviderID1], groups = [<<"g1">>, <<"g2">>]}},
    ?assertEqual({ok, SpaceKey5}, rpc:call(Node1, space, save, [SpaceDoc5])),

    SpaceKey6 = <<"spacekey6">>,
    SpaceDoc6 = #document{key = SpaceKey6, value = #space{name = <<"space6">>, providers = [ProviderID1], users = [<<"u1">>, <<"u2">>]}},
    ?assertEqual({ok, SpaceKey6}, rpc:call(Node1, space, save, [SpaceDoc6])),

    verify_sent(Node1, "endpoint1", "{\"seq\":" ++ integer_to_list(First + 1) ++ ",\"space\":{\"id\":\"spacekey1\",\"name\":\"space1\",\"groups\":[],\"users\":[]}}"),
    verify_sent(Node1, "endpoint1", "{\"seq\":" ++ integer_to_list(First + 2) ++ ",\"space\":{\"id\":\"spacekey2\",\"name\":\"space2\",\"groups\":[],\"users\":[]}}"),
    verify_sent(Node1, "endpoint2", "{\"seq\":" ++ integer_to_list(First + 2) ++ ",\"space\":{\"id\":\"spacekey2\",\"name\":\"space2\",\"groups\":[],\"users\":[]}}"),
    verify_sent(Node1, "endpoint1", "{\"seq\":" ++ integer_to_list(First + 4) ++ ",\"space\":{\"id\":\"spacekey4\",\"name\":\"space4\",\"groups\":[],\"users\":[]}}"),
    verify_sent(Node1, "endpoint2", "{\"seq\":" ++ integer_to_list(First + 4) ++ ",\"space\":{\"id\":\"spacekey4\",\"name\":\"space4\",\"groups\":[],\"users\":[]}}"),
    verify_sent(Node1, "endpoint1", "{\"seq\":" ++ integer_to_list(First + 5) ++ ",\"space\":{\"id\":\"spacekey5\",\"name\":\"space5\",\"groups\":[\"g1\",\"g2\"],\"users\":[]}}"),
    verify_sent(Node1, "endpoint1", "{\"seq\":" ++ integer_to_list(First + 6) ++ ",\"space\":{\"id\":\"spacekey6\",\"name\":\"space6\",\"groups\":[],\"users\":[\"u1\",\"u2\"]}}"),
    ok.

space_changes_before_subscription(Config) ->
    [Node1, _] = ?config(oz_worker_nodes, Config),
    ProviderID1 = <<"provider1">>,
    ProviderID2 = <<"provider2">>,
    First = ?getFirstSeq(Node1, Config),

    SpaceKey1 = <<"spacekey1">>,
    SpaceDoc1 = #document{key = SpaceKey1, value = #space{name = <<"space1">>, providers = [ProviderID1]}},
    ?assertEqual({ok, SpaceKey1}, rpc:call(Node1, space, save, [SpaceDoc1])),

    SpaceKey2 = <<"spacekey2">>,
    SpaceDoc2 = #document{key = SpaceKey2, value = #space{name = <<"space2">>, providers = [ProviderID1, ProviderID2]}},
    ?assertEqual({ok, SpaceKey2}, rpc:call(Node1, space, save, [SpaceDoc2])),

    SpaceKey3 = <<"spacekey3">>,
    SpaceDoc3 = #document{key = SpaceKey3, value = #space{name = <<"space3">>, providers = []}},
    ?assertEqual({ok, SpaceKey3}, rpc:call(Node1, space, save, [SpaceDoc3])),

    SpaceKey4 = <<"spacekey4">>,
    SpaceDoc4 = #document{key = SpaceKey4, value = #space{name = <<"space4">>, providers = [ProviderID1, ProviderID2]}},
    ?assertEqual({ok, SpaceKey4}, rpc:call(Node1, space, save, [SpaceDoc4])),

    SpaceKey5 = <<"spacekey5">>,
    SpaceDoc5 = #document{key = SpaceKey5, value = #space{name = <<"space5">>, providers = [ProviderID1], groups = [<<"g1">>, <<"g2">>]}},
    ?assertEqual({ok, SpaceKey5}, rpc:call(Node1, space, save, [SpaceDoc5])),

    SpaceKey6 = <<"spacekey6">>,
    SpaceDoc6 = #document{key = SpaceKey6, value = #space{name = <<"space6">>, providers = [ProviderID1], users = [<<"u1">>, <<"u2">>]}},
    ?assertEqual({ok, SpaceKey6}, rpc:call(Node1, space, save, [SpaceDoc6])),

    subscribe(Node1, ProviderID1, 0, "endpoint1"),
    subscribe(Node1, ProviderID2, 0, "endpoint2"),

    verify_sent(Node1, "endpoint1", "{\"seq\":" ++ integer_to_list(First + 1) ++ ",\"space\":{\"id\":\"spacekey1\",\"name\":\"space1\",\"groups\":[],\"users\":[]}}"),
    verify_sent(Node1, "endpoint1", "{\"seq\":" ++ integer_to_list(First + 2) ++ ",\"space\":{\"id\":\"spacekey2\",\"name\":\"space2\",\"groups\":[],\"users\":[]}}"),
    verify_sent(Node1, "endpoint2", "{\"seq\":" ++ integer_to_list(First + 2) ++ ",\"space\":{\"id\":\"spacekey2\",\"name\":\"space2\",\"groups\":[],\"users\":[]}}"),
    verify_sent(Node1, "endpoint1", "{\"seq\":" ++ integer_to_list(First + 4) ++ ",\"space\":{\"id\":\"spacekey4\",\"name\":\"space4\",\"groups\":[],\"users\":[]}}"),
    verify_sent(Node1, "endpoint2", "{\"seq\":" ++ integer_to_list(First + 4) ++ ",\"space\":{\"id\":\"spacekey4\",\"name\":\"space4\",\"groups\":[],\"users\":[]}}"),
    verify_sent(Node1, "endpoint1", "{\"seq\":" ++ integer_to_list(First + 5) ++ ",\"space\":{\"id\":\"spacekey5\",\"name\":\"space5\",\"groups\":[\"g1\",\"g2\"],\"users\":[]}}"),
    verify_sent(Node1, "endpoint1", "{\"seq\":" ++ integer_to_list(First + 6) ++ ",\"space\":{\"id\":\"spacekey6\",\"name\":\"space6\",\"groups\":[],\"users\":[\"u1\",\"u2\"]}}"),
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
    verify_sent(Node, Endpoint, Body, 50).

verify_sent(Node, Endpoint, Body, 0) ->
    ?assert({error, timeout_for, Node, Endpoint, Body});

verify_sent(Node, Endpoint, Body, Retries) ->
    Result = mock_capture(Node, [first, http_client, post, [Endpoint, '_', list_to_binary(Body)], 3]),
    case Result of
        {badrpc, _} ->
            timer:sleep(500),
            verify_sent(Node, Endpoint, Body, Retries - 1);
        _ ->
            ok
    end.

mock_capture(Node, Args) ->
    rpc:call(Node, meck, capture, Args).