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

    SpaceDoc1 = save(Node1, <<"spacekey1">>, #space{name = <<"space1">>, providers = [ProviderID1]}),
    SpaceDoc2 = save(Node1, <<"spacekey2">>, #space{name = <<"space2">>, providers = [ProviderID1, ProviderID2]}),
    SpaceDoc3 = save(Node1, <<"spacekey3">>, #space{name = <<"space3">>, providers = []}),
    SpaceDoc4 = save(Node1, <<"spacekey4">>, #space{name = <<"space4">>, providers = [ProviderID1, ProviderID2]}),
    SpaceDoc5 = save(Node1, <<"spacekey5">>, #space{name = <<"space5">>, providers = [ProviderID1], groups = [<<"g1">>, <<"g2">>]}),
    SpaceDoc6 = save(Node1, <<"spacekey6">>, #space{name = <<"space6">>, providers = [ProviderID1], users = [<<"u1">>, <<"u2">>]}),

    % then
    verify_communication(Node1, <<"endpoint1">>,
        [SpaceDoc1, SpaceDoc2, SpaceDoc4, SpaceDoc5, SpaceDoc6],
        [SpaceDoc3]
    ),
    verify_communication(Node1, <<"endpoint2">>,
        [SpaceDoc2, SpaceDoc4],
        [SpaceDoc1, SpaceDoc3, SpaceDoc5, SpaceDoc6]
    ),
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
    SpaceDoc1 = save(Node1, <<"spacekey1">>, #space{name = <<"space1">>, providers = [ProviderID1]}),
    SpaceDoc2 = save(Node1, <<"spacekey2">>, #space{name = <<"space2">>, providers = [ProviderID1, ProviderID2]}),
    SpaceDoc3 = save(Node1, <<"spacekey3">>, #space{name = <<"space3">>, providers = []}),
    SpaceDoc4 = save(Node1, <<"spacekey4">>, #space{name = <<"space4">>, providers = [ProviderID1, ProviderID2]}),
    SpaceDoc5 = save(Node1, <<"spacekey5">>, #space{name = <<"space5">>, providers = [ProviderID1], groups = [<<"g1">>, <<"g2">>]}),
    SpaceDoc6 = save(Node1, <<"spacekey6">>, #space{name = <<"space6">>, providers = [ProviderID1], users = [<<"u1">>, <<"u2">>]}),

    subscribe(First, <<"endpoint1">>, SubscribeParams1),
    subscribe(First, <<"endpoint2">>, SubscribeParams2),

    % then
    verify_communication(Node1, <<"endpoint1">>,
        [SpaceDoc1, SpaceDoc2, SpaceDoc4, SpaceDoc5, SpaceDoc6],
        [SpaceDoc3]
    ),
    verify_communication(Node1, <<"endpoint2">>,
        [SpaceDoc2, SpaceDoc4],
        [SpaceDoc1, SpaceDoc3, SpaceDoc5, SpaceDoc6]
    ),
    ok.

node_for_subscription_changes(Config) ->
    [Node1, Node2] = ?config(oz_worker_nodes, Config),
    [Address1, Address2] = ?config(restAddresses, Config),
    RegisterParams = {Address1, ?CONTENT_TYPE_HEADER, []},

    % given
    {ProviderID1, SubscribeParams1} = rest_utils:register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, RegisterParams),
    SubscribeParams2 = rest_utils:update_req_params(SubscribeParams1, Address2, address),

    % when
    First = getFirstSeq(Node1),
    subscribe(First, <<"endpoint1">>, SubscribeParams1),
    SpaceDoc1 = save(Node1, <<"spacekey1">>, #space{name = <<"space1">>, providers = [ProviderID1], groups = []}),

    await_communication(Node1, <<"endpoint1">>, SpaceDoc1),
    Later = getFirstSeq(Node1),
    subscribe(Later, <<"endpoint1">>, SubscribeParams2),
    SpaceDoc2 = save(Node1, <<"spacekey2">>, #space{name = <<"space2">>, providers = [ProviderID1], groups = []}),

    % then
    verify_communication(Node1, <<"endpoint1">>, [SpaceDoc1], [SpaceDoc2]),
    verify_communication(Node2, <<"endpoint1">>, [SpaceDoc2], [SpaceDoc1]),
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

await_communication(Node, Endpoint, ExpectedDoc) ->
    verify_communication(Node, Endpoint, [ExpectedDoc], []).

verify_communication(Node, Endpoint, ExpectedDocs, ForbiddenDocs) ->
    verify_communication(Node, Endpoint, as_changes(ExpectedDocs), as_changes(ForbiddenDocs), 1, ?ALLOWED_FAILURES).

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

as_changes(Docs) ->
    lists:map(fun as_change/1, Docs).

as_change(#document{key = ID, value = #space{name = Name, groups = Groups, users = Users}}) ->
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

save(Node, Key, Value) ->
    Doc = #document{key = Key, value = Value},
    ?assertEqual({ok, Key}, rpc:call(Node, space, save, [Doc])),
    Doc.