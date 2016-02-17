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
-export([space_changes_after_subscription/1, space_changes_before_subscription/1, node_for_subscription_changes/1]).

-define(CONTENT_TYPE_HEADER, [{<<"content-type">>, <<"application/json">>}]).
-define(COMMUNICATION_WAIT, 500).
-define(ALLOWED_FAILURES, 20).

-define(URLS1, [<<"127.0.0.1">>]).
-define(URLS2, [<<"127.0.0.2">>]).
-define(REDIRECTION_POINT1, <<"https://127.0.0.1:443">>).
-define(REDIRECTION_POINT2, <<"https://127.0.0.2:443">>).
-define(CLIENT_NAME1, <<"provider1">>).
-define(CLIENT_NAME2, <<"provider2">>).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([space_changes_after_subscription, space_changes_before_subscription, node_for_subscription_changes]).

space_changes_after_subscription(Config) ->
    [Node1, _] = ?config(oz_worker_nodes, Config),
    [Address1, _] = ?config(restAddresses, Config),
    RegisterParams = {Address1, ?CONTENT_TYPE_HEADER, []},

    % given
    {ProviderID1, SubscribeParams1} = rest_utils:register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, RegisterParams),
    {ProviderID2, SubscribeParams2} = rest_utils:register_provider(?URLS2, ?REDIRECTION_POINT2, ?CLIENT_NAME2, RegisterParams),
    First = getFirstSeq(Node1),

    % when
    subscribe(First, <<"endpoint1">>, SubscribeParams1),
    subscribe(First, <<"endpoint2">>, SubscribeParams2),

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

    % then
    verify_communication(Node1, <<"endpoint1">>, [
        space_msg(SpaceDoc1),
        space_msg(SpaceDoc2),
        space_msg(SpaceDoc4),
        space_msg(SpaceDoc5),
        space_msg(SpaceDoc6)
    ], [
        space_msg(SpaceDoc3)
    ]),
    verify_communication(Node1, <<"endpoint2">>, [
        space_msg(SpaceDoc2),
        space_msg(SpaceDoc4)
    ], [
        space_msg(SpaceDoc1),
        space_msg(SpaceDoc3),
        space_msg(SpaceDoc5),
        space_msg(SpaceDoc6)
    ]),
    ok.

space_changes_before_subscription(Config) ->
    [Node1, _] = ?config(oz_worker_nodes, Config),
    [Address1, _] = ?config(restAddresses, Config),
    RegisterParams = {Address1, ?CONTENT_TYPE_HEADER, []},

    % given
    {ProviderID1, SubscribeParams1} = rest_utils:register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, RegisterParams),
    {ProviderID2, SubscribeParams2} = rest_utils:register_provider(?URLS2, ?REDIRECTION_POINT2, ?CLIENT_NAME2, RegisterParams),
    First = getFirstSeq(Node1),

    % when
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

    subscribe(First, <<"endpoint1">>, SubscribeParams1),
    subscribe(First, <<"endpoint2">>, SubscribeParams2),

    % then
    verify_communication(Node1, <<"endpoint1">>, [
        space_msg(SpaceDoc1),
        space_msg(SpaceDoc2),
        space_msg(SpaceDoc4),
        space_msg(SpaceDoc5),
        space_msg(SpaceDoc6)
    ], [
        space_msg(SpaceDoc3)
    ]),
    verify_communication(Node1, <<"endpoint2">>, [
        space_msg(SpaceDoc2),
        space_msg(SpaceDoc4)
    ], [
        space_msg(SpaceDoc1),
        space_msg(SpaceDoc3),
        space_msg(SpaceDoc5),
        space_msg(SpaceDoc6)
    ]),
    ok.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(ssl2),
    hackney:start(),
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),

    [Node1, Node2] = ?config(oz_worker_nodes, NewConfig),
    [OZ_IP_1, OZ_IP_2] = [rest_utils:get_node_ip(Node1), rest_utils:get_node_ip(Node2)],
    RestPort = rest_utils:get_rest_port(Node1),
    RestAddress1 = "https://" ++ OZ_IP_1 ++ ":" ++ integer_to_list(RestPort),
    RestAddress2 = "https://" ++ OZ_IP_2 ++ ":" ++ integer_to_list(RestPort),
    Addresses = [RestAddress1, RestAddress2],
    [{restAddresses, Addresses} | NewConfig].

init_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, http_client),
    test_utils:mock_expect(Nodes, http_client, post, fun(_Address, _Options, _Body) -> ok end),
    Config.

end_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, http_client).

end_per_suite(Config) ->
    hackney:stop(),
    application:stop(ssl2),
    test_node_starter:clean_environment(Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================

subscribe(LastSeen, Endpoint, SubscribeParams) ->
    {Address, Headers, Options} = SubscribeParams,
    Data = json_utils:encode([
        {<<"last_seq">>, LastSeen},
        {<<"endpoint">>, Endpoint}
    ]),
    RestAddress = Address ++ "/subscription",
    Result = rest_utils:do_request(RestAddress, Headers, post, Data, Options),
    ?assertEqual(204, rest_utils:get_response_status(Result)).

await_communication(Node, Endpoint, ExpectedChange) ->
    verify_communication(Node, Endpoint, [ExpectedChange], [], 1, ?ALLOWED_FAILURES).

verify_communication(Node, Endpoint, ExpectedChanges, ForbiddenChanges) ->
    verify_communication(Node, Endpoint, ExpectedChanges, ForbiddenChanges, 1, ?ALLOWED_FAILURES).

verify_communication(_, _, [], _, _, _) ->
    ok;
verify_communication(_, _, PresentBodies, _, _, 0) ->
    ?assertMatch([], PresentBodies);
verify_communication(Node, Endpoint, ExpectedChanges, ForbiddenChanges, Request, AllowedFailures) ->
    Body = mock_capture(Node, [Request, http_client, post, [Endpoint, '_', '_'], 3]),
    case Body of
        {badrpc, _} ->
            timer:sleep(?COMMUNICATION_WAIT),
            verify_communication(Node, Endpoint, ExpectedChanges, ForbiddenChanges, Request, AllowedFailures - 1);
        _ ->
            Data = json_utils:decode(Body),
            ChangeData = lists:last(proplists:delete(<<"seq">>, Data)),
            lists:foreach(fun(Forbidden) -> ?assertNotMatch(Forbidden, ChangeData) end, ForbiddenChanges),
            Remaining = ExpectedChanges -- [ChangeData],
            verify_communication(Node, Endpoint, Remaining, ForbiddenChanges, Request + 1, ?ALLOWED_FAILURES)
    end.

space_msg(SpaceDoc) ->
    #document{key = ID, value = #space{name = Name, groups = Groups, users = Users}} = SpaceDoc,
    {<<"space">>, [{<<"id">>, ID}, {<<"name">>, Name}, {<<"groups">>, Groups}, {<<"users">>, Users}]}.

mock_capture(Node, Args) ->
    rpc:call(Node, meck, capture, Args).

getFirstSeq(Node) ->
    {_, LastSeqInDb, _} = ?assertMatch(
        {ok, _, _},
        rpc:call(Node, couchdb_datastore_driver, db_run,
            [couchbeam_changes, follow_once, [], 3])
    ),
    binary_to_integer(LastSeqInDb).