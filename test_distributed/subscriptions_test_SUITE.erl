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
-module(subscriptions_test_SUITE).
-author("Michal Zmuda").

-include("subscriptions_test_utils.hrl").
-include("registered_names.hrl").
-include("subscriptions/subscriptions.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([
    space_update_through_support_test/1,
    space_update_through_users_test/1,
    space_update_through_groups_test/1,
    no_space_update_test/1,
    user_update_test/1,
    only_public_user_update_test/1,
    group_update_through_users_test/1,
    ancestor_group_update_through_users_test/1,
    no_group_update_test/1,
    multiple_updates_test/1,
    updates_for_added_user_test/1,
    updates_for_added_user_have_revisions_test/1,
    updates_have_revisions_test/1,
    fetches_changes_older_than_in_cache/1,
    fetches_changes_from_both_cache_and_db/1,
    fetches_changes_when_cache_has_gaps/1,
    simple_delete_test/1,
    provider_connection_checks_test/1,
    all_data_in_space_update_test/1,
    all_data_in_user_update_test/1,
    all_data_in_group_update_test/1,
    all_data_in_provider_update_test/1,
    updates_for_with_providers_test/1,
    child_group_update_through_users_test/1]).


%%%===================================================================
%%% API functions
%%%===================================================================
%%TODO move init_messages befor save
all() -> ?ALL([
    provider_connection_checks_test,
    multiple_updates_test,
    space_update_through_support_test,
    space_update_through_users_test,
    space_update_through_groups_test,
    all_data_in_space_update_test,
    all_data_in_user_update_test,
    all_data_in_group_update_test,
    all_data_in_provider_update_test,
    user_update_test,
    simple_delete_test,
    group_update_through_users_test,
    updates_for_with_providers_test,
    child_group_update_through_users_test,
    ancestor_group_update_through_users_test,
    updates_for_added_user_test,
    updates_have_revisions_test,
    updates_for_added_user_have_revisions_test,
    fetches_changes_older_than_in_cache,
    fetches_changes_from_both_cache_and_db,
    fetches_changes_when_cache_has_gaps,
    no_space_update_test,
    only_public_user_update_test,
    no_group_update_test
]).


provider_connection_checks_test(Config) ->
    [Node, OtherNode | _] = ?config(oz_worker_nodes, Config),
    StubConnFun = fun() -> receive _ -> ok after 5000 -> ok end end,
    P1 = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),

    %% no connection
    ?assertNot(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% local connection
    LocalConn = proc_lib:spawn(Node, StubConnFun),
    subscriptions_test_utils:call_worker(Node, {add_connection, P1, LocalConn}),
    ?assert(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% broken local connection
    exit(LocalConn, killed),
    ?assertNot(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% other local connection
    NewLocalConn = proc_lib:spawn(Node, StubConnFun),
    subscriptions_test_utils:call_worker(Node, {add_connection, P1, NewLocalConn}),
    ?assert(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% connections removed
    subscriptions_test_utils:call_worker(Node, {remove_connection, P1, LocalConn}),
    subscriptions_test_utils:call_worker(Node, {remove_connection, P1, NewLocalConn}),
    ?assertNot(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% connection at different worker
    RemoteConn = proc_lib:spawn(OtherNode, StubConnFun),
    subscriptions_test_utils:call_worker(Node, {add_connection, P1, RemoteConn}),
    ?assert(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% addition of broken connection
    NewRemoteConn = proc_lib:spawn(OtherNode, StubConnFun),
    exit(NewRemoteConn, killed),
    subscriptions_test_utils:call_worker(Node, {add_connection, P1, NewRemoteConn}),
    ?assert(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% all connections broken
    exit(RemoteConn, killed),
    ?assertNot(rpc:call(Node, subscriptions, any_connection_active, [P1])),
    ok.

% Checks if update won't appear if provider does not support space
no_space_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),
    S1 = #space{name = <<"initial">>, providers_supports = []},
    subscriptions_test_utils:save(Node, ?ID(s1), S1),

    % when
    Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:update_document(Node, space, ?ID(s1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_absent(Context, [
        subscriptions_test_utils:expectation(?ID(s1), S1#space{name = <<"initial">>}),
        subscriptions_test_utils:expectation(?ID(s1), S1#space{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers that support changed space
space_update_through_support_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), [?ID(s1)]),
    S1 = #space{name = <<"initial">>, providers_supports = [{PID, 0}]},
    subscriptions_test_utils:save(Node, ?ID(s1), S1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, []),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(s1), S1)),
    subscriptions_test_utils:update_document(Node, space, ?ID(s1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(s1), S1#space{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers where user that have access to space is logged-in
space_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),
    S1 = #space{name = <<"initial">>, users = [{?ID(u1), []}]},
    U1 = #onedata_user{name = <<"u1">>},

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(s1), S1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(s1), S1)),
    subscriptions_test_utils:update_document(Node, space, ?ID(s1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(s1), S1#space{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers where user that have access to space is logged-n
% and user is connected with space through group
space_update_through_groups_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),
    S1 = #space{name = <<"initial">>, groups = [{?ID(g1), []}]},
    U1 = #onedata_user{name = <<"u1">>},

    G1 = #user_group{
        name = <<"g1">>,
        users = [{?ID(u1), privileges:group_admin()}],
        spaces = [?ID(s1)]
    },

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(s1), S1),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(s1), S1)),
    subscriptions_test_utils:update_document(Node, space, ?ID(s1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(s1), S1#space{name = <<"updated">>})
    ]),
    ok.

all_data_in_space_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), [?ID(s1)]),
    PID2 = subscriptions_test_utils:create_provider(Node, ?ID(p2), [?ID(s1)]),
    PID3 = subscriptions_test_utils:create_provider(Node, ?ID(p3), [?ID(s1)]),
    User = #onedata_user{name = <<"user">>},
    Group = #user_group{name = <<"group">>},

    subscriptions_test_utils:save(Node, ?ID(u1), User),
    subscriptions_test_utils:save(Node, ?ID(u2), User),
    subscriptions_test_utils:save(Node, ?ID(g1), Group),
    subscriptions_test_utils:save(Node, ?ID(g2), Group),


    % when
    Space = #space{
        name = <<"space">>,
        users = [{?ID(u1), privileges:space_manager()}, {?ID(u2), []}],
        groups = [{?ID(g1), privileges:space_admin()}, {?ID(g2), []}],
        providers_supports = [{PID, 0}, {PID2, 100}, {PID3, 1000}]
    },
    Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:save(Node, ?ID(s1), Space),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(s1), Space)
    ]),
    ok.

all_data_in_user_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),

    % when
    User = #onedata_user{
        name = <<"user">>,
        groups = [?ID(g1), ?ID(g2)],
        spaces = [?ID(s1), ?ID(s2)],
        default_space = <<"s1">>,
        effective_groups = [?ID(g1), ?ID(g3), ?ID(g3)]
    },
    Context = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    subscriptions_test_utils:save(Node, ?ID(u1), User),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(u1), User)
    ]),
    ok.

all_data_in_group_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),
    User = #onedata_user{name = <<"user">>, groups = [?ID(g1)]},
    subscriptions_test_utils:save(Node, ?ID(u1), User),

    % when
    Group = #user_group{
        name = <<"Group">>,
        users = [{?ID(u1), privileges:group_admin()},
            {?ID(u2), privileges:group_user()}],
        effective_users = [{?ID(u1), privileges:group_admin()},
            {?ID(u3), privileges:group_privileges()}],
        nested_groups = [{?ID(g1), privileges:group_admin()},
            {?ID(g3), privileges:group_privileges()}],
        parent_groups = [?ID(g1), ?ID(g2)],
        spaces = [?ID(s1), ?ID(s2)]
    },
    Context = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    subscriptions_test_utils:save(Node, ?ID(g1), Group),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(g1), Group)
    ]),
    ok.

all_data_in_provider_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    Spaces = [?ID(s1), ?ID(s2), ?ID(s3)],
    Urls = [<<"url1">>, <<"url2">>],
    Name = <<"name">>,
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), Spaces, Urls),

    % when
    Context =subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:update_document(Node, provider, PID, #{client_name => Name}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(PID, #provider{spaces = Spaces, urls = Urls, client_name = Name})
    ]).

updates_for_with_providers_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), [?ID(s1), ?ID(s2)], [<<"p1-url1">>]),
    P2 = #provider{spaces = [?ID(s2)], urls = [<<"p2-url1">>, <<"p2-url2">>], client_name = <<"p2">>},
    P3 = #provider{spaces = [?ID(s1)], urls = [<<"p3-url1">>], client_name = <<"p3">>},


    % when
    Context =subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:save(Node, ?ID(p2), P2),
    subscriptions_test_utils:save(Node, ?ID(p3), P3),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(PID, #provider{spaces = [?ID(s1), ?ID(s2)], urls = [<<"p1-url1">>], client_name = ?ID(p1)}),
        subscriptions_test_utils:public_only_provider_expectation(?ID(p2), P2#provider.client_name, P2#provider.urls),
        subscriptions_test_utils:public_only_provider_expectation(?ID(p3), P3#provider.client_name, P3#provider.urls)
    ]),
    ok.

only_public_user_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), [?ID(s1)]),
    U1 = #onedata_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),

    % when
    Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages(Context, [
        subscriptions_test_utils:public_only_user_expectation(?ID(u1), <<"updated">>)
    ], [
        subscriptions_test_utils:expectation(?ID(u1), U1),
        subscriptions_test_utils:expectation(?ID(u1), U1#onedata_user{name = <<"updated">>})
    ]),
    ok.

user_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), [?ID(s1)]),
    U1 = #onedata_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(u1), U1)),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(u1), U1#onedata_user{name = <<"updated">>})
    ]),
    ok.

simple_delete_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), [?ID(s1)]),
    U1 = #onedata_user{name = <<"u1">>, groups = [?ID(g1)]},
    S1 = #space{name = <<"s1">>, providers_supports = [{PID, 0}],
        users = [{?ID(u1), []}]},
    G1 = #user_group{name = <<"g1">>, users = [{?ID(u1), []}]},

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(s1), S1),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(u1), U1)),
    subscriptions_test_utils:delete_document(Node, onedata_user, ?ID(u1)),
    subscriptions_test_utils:delete_document(Node, user_group, ?ID(g1)),
    subscriptions_test_utils:delete_document(Node, space, ?ID(s1)),

    % then
    subscriptions_test_utils:verify_messages(Context, [
        [{<<"id">>, ?ID(g1)}, {<<"group">>, <<"delete">>}],
        [{<<"id">>, ?ID(u1)}, {<<"user">>, <<"delete">>}],
        [{<<"id">>, ?ID(s1)}, {<<"space">>, <<"delete">>}]
    ], []),
    ok.

% checks if no update is lost in case of many updates in short time
multiple_updates_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), [?ID(s1)]),
    U1 = #onedata_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(u1), U1)),

    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>, groups => [<<"gr1">>]}),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>, groups => [<<"gr2">>], spaces => [<<"sp1">>]}),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(u1), U1#onedata_user{name = <<"updated4">>, groups = [<<"gr2">>], spaces = [<<"sp1">>]})
    ]),
    ok.

no_group_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),
    G1 = #user_group{name = <<"g1">>},
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    subscriptions_test_utils:update_document(Node, user_group, ?ID(g1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_absent(Context, [
        subscriptions_test_utils:expectation(?ID(g1), G1#user_group{name = <<"updated">>})
    ]),
    ok.

group_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),
    U1 = #onedata_user{name = <<"u1">>},
    G1 = #user_group{name = <<"g1">>, users = [{?ID(u1), []}]},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(g1), G1)),
    subscriptions_test_utils:update_document(Node, user_group, ?ID(g1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(g1), G1#user_group{name = <<"updated">>})
    ]),
    ok.

ancestor_group_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),
    U1 = #onedata_user{name = <<"u1">>,
        effective_groups = [?ID(g1), ?ID(g2), ?ID(g3)]},
    G1 = #user_group{name = <<"g1">>,
        users = [{?ID(u1), []}],
        parent_groups = [?ID(g2)],
        effective_users = [{?ID(u1), []}]},
    G2 = #user_group{name = <<"g2">>,
        users = [],
        parent_groups = [?ID(g3)],
        nested_groups = [{?ID(g1), []}],
        effective_users = [{?ID(u1), []}]},
    G3 = #user_group{name = <<"g3">>,
        users = [],
        nested_groups = [{?ID(g2), []}],
        effective_users = [{?ID(u1), []}]},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),
    subscriptions_test_utils:save(Node, ?ID(g2), G2),
    subscriptions_test_utils:save(Node, ?ID(g3), G3),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(g3), G3)),
    subscriptions_test_utils:update_document(Node, user_group, ?ID(g1), #{name => <<"updated">>}),
    subscriptions_test_utils:update_document(Node, user_group, ?ID(g2), #{name => <<"updated">>}),
    subscriptions_test_utils:update_document(Node, user_group, ?ID(g3), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(g1), G1#user_group{name = <<"updated">>}),
        subscriptions_test_utils:expectation(?ID(g2), G2#user_group{name = <<"updated">>}),
        subscriptions_test_utils:expectation(?ID(g3), G3#user_group{name = <<"updated">>})
    ]),
    ok.

child_group_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),

    U1 = #onedata_user{name = <<"u1">>,
        effective_groups = [?ID(g1)]},
    U2 = #onedata_user{name = <<"u2">>,
        effective_groups = [?ID(g2)]},

    G1 = #user_group{name = <<"g1">>,
        users = [{?ID(u1), []}],
        parent_groups = [?ID(g2)],
        effective_groups = [?ID(g2), ?ID(g1)],
        effective_users = [{?ID(u1), []}]},
    G2 = #user_group{name = <<"g2">>,
        users = [{?ID(u2), []}],
        nested_groups = [{?ID(g1), []}],
        effective_groups = [?ID(g1)],
        effective_users = [{?ID(u1), []}, {?ID(u2), []}]},

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(u2), U2),
    subscriptions_test_utils:save(Node, ?ID(g2), G2),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(g1), G1)),
    subscriptions_test_utils:update_document(Node, user_group, ?ID(g1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(g1), G1#user_group{name = <<"updated">>})
    ]),
    ok.

updates_for_added_user_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), [?ID(s1), ?ID(s2)]),
    U1 = #onedata_user{name = <<"u1">>, groups = [?ID(g1)],
        effective_groups = [?ID(g1), ?ID(g2)], spaces = [?ID(s2)]},
    G1 = #user_group{name = <<"g1">>, users = [{?ID(u1), []}],
        spaces = [?ID(s1)], effective_groups = [?ID(g1), ?ID(g2)],
        parent_groups = [?ID(g2)]},
    G2 = #user_group{name = <<"g1">>, users = [],
        spaces = [], effective_groups = [?ID(g2)],
        nested_groups = [{?ID(g1), []}, {?ID(g3), []}]},
    G3 = #user_group{name = <<"g1">>, users = [],
        spaces = [], effective_groups = [?ID(g3), ?ID(g2)],
        parent_groups = [?ID(g2)]},
    S1 = #space{name = <<"s1">>, providers_supports = [{PID, 0}], groups = [{?ID(g1), []}]},
    S2 = #space{name = <<"s2">>, providers_supports = [{PID, 0}], users = [{?ID(u1), []}]},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),
    subscriptions_test_utils:save(Node, ?ID(g2), G2),
    subscriptions_test_utils:save(Node, ?ID(g3), G3),
    subscriptions_test_utils:save(Node, ?ID(s1), S1),
    subscriptions_test_utils:save(Node, ?ID(s2), S2),


    Context1 = subscriptions_test_utils:init_messages(Node, PID, []),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(s2), S2)),

    % when & then
    subscriptions_test_utils:verify_messages_present(Context#subs_ctx{users = [?ID(u1)]}, [
        subscriptions_test_utils:expectation(?ID(u1), U1),
        subscriptions_test_utils:expectation(?ID(g1), G1),
        subscriptions_test_utils:expectation(?ID(g2), G2),
        subscriptions_test_utils:expectation(?ID(g3), G3),
        subscriptions_test_utils:expectation(?ID(s1), S1),
        subscriptions_test_utils:expectation(?ID(s2), S2)
    ]),
    ok.

updates_for_added_user_have_revisions_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),
    U1 = #onedata_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    Rev0 = subscriptions_test_utils:get_rev(Node, onedata_user, ?ID(u1)),

    % when
    Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>}),
    Rev1 = subscriptions_test_utils:get_rev(Node, onedata_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    Rev2 = subscriptions_test_utils:get_rev(Node, onedata_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>}),
    Rev3 = subscriptions_test_utils:get_rev(Node, onedata_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),
    Rev4 = subscriptions_test_utils:get_rev(Node, onedata_user, ?ID(u1)),

    % then
    subscriptions_test_utils:verify_messages_present(Context#subs_ctx{users = [(?ID(u1))]}, [
        subscriptions_test_utils:expectation_with_rev(
            [Rev4, Rev3, Rev2, Rev1, Rev0],
            subscriptions_test_utils:expectation(?ID(u1), U1#onedata_user{name = <<"updated4">>}))
    ]),
    ok.

updates_have_revisions_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),
    U1 = #onedata_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    Rev0 = subscriptions_test_utils:get_rev(Node, onedata_user, ?ID(u1)),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [(?ID(u1))]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(u1), U1)),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>}),
    Rev1 = subscriptions_test_utils:get_rev(Node, onedata_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    Rev2 = subscriptions_test_utils:get_rev(Node, onedata_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>}),
    Rev3 = subscriptions_test_utils:get_rev(Node, onedata_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),
    Rev4 = subscriptions_test_utils:get_rev(Node, onedata_user, ?ID(u1)),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation_with_rev(
            [Rev4, Rev3, Rev2, Rev1, Rev0],
            subscriptions_test_utils:expectation(?ID(u1), U1#onedata_user{name = <<"updated4">>}))
    ]),
    ok.

fetches_changes_older_than_in_cache(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),
    U1 = #onedata_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>}),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>}),

    Context = subscriptions_test_utils:init_messages(Node, PID, [(?ID(u1))]),
    subscriptions_test_utils:update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),

    % when
    _ForgottenContext = subscriptions_test_utils:flush_messages(Context,
        subscriptions_test_utils:expectation(?ID(u1), U1#onedata_user{name = <<"updated4">>})),

    subscriptions_test_utils:empty_cache(Node),
    subscriptions_test_utils:save(Node, ?ID(u2), #onedata_user{name = <<"u2">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(u1), U1#onedata_user{name = <<"updated4">>})
    ]),
    ok.


fetches_changes_from_both_cache_and_db(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), [
        ?ID(s1), ?ID(s2), ?ID(s3), ?ID(s4), ?ID(s5), ?ID(s6), ?ID(s7), ?ID(s8), ?ID(s9)
    ]),
    Space = #space{name = <<"initial">>, providers_supports = [{PID, 0}]},
    subscriptions_test_utils:save(Node, ?ID(s1), Space),
    subscriptions_test_utils:save(Node, ?ID(s3), Space),
    subscriptions_test_utils:save(Node, ?ID(s2), Space),
    subscriptions_test_utils:save(Node, ?ID(s4), Space),
    subscriptions_test_utils:save(Node, ?ID(s5), Space),
    subscriptions_test_utils:save(Node, ?ID(s6), Space),
    subscriptions_test_utils:save(Node, ?ID(s7), Space),
    subscriptions_test_utils:save(Node, ?ID(s8), Space),
    subscriptions_test_utils:save(Node, ?ID(s9), Space),

    % when
    Copy = Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:flush_messages(Context, subscriptions_test_utils:expectation(?ID(s9), Space)),
    empty_first_half_of_cache(Node),

    % then
    subscriptions_test_utils:verify_messages_present(Copy, [
        subscriptions_test_utils:expectation(?ID(s1), Space),
        subscriptions_test_utils:expectation(?ID(s2), Space),
        subscriptions_test_utils:expectation(?ID(s3), Space),
        subscriptions_test_utils:expectation(?ID(s4), Space),
        subscriptions_test_utils:expectation(?ID(s5), Space),
        subscriptions_test_utils:expectation(?ID(s6), Space),
        subscriptions_test_utils:expectation(?ID(s7), Space),
        subscriptions_test_utils:expectation(?ID(s8), Space),
        subscriptions_test_utils:expectation(?ID(s9), Space)
    ]),
    ok.

fetches_changes_when_cache_has_gaps(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), [
        ?ID(s1), ?ID(s2), ?ID(s3), ?ID(s4), ?ID(s5), ?ID(s6), ?ID(s7), ?ID(s8), ?ID(s9)
    ]),
    Space = #space{name = <<"initial">>, providers_supports = [{PID, 0}]},
    subscriptions_test_utils:save(Node, ?ID(s1), Space),
    subscriptions_test_utils:save(Node, ?ID(s2), Space),
    subscriptions_test_utils:save(Node, ?ID(s3), Space),
    subscriptions_test_utils:save(Node, ?ID(s4), Space),
    subscriptions_test_utils:save(Node, ?ID(s5), Space),
    subscriptions_test_utils:save(Node, ?ID(s6), Space),
    subscriptions_test_utils:save(Node, ?ID(s7), Space),
    subscriptions_test_utils:save(Node, ?ID(s8), Space),
    subscriptions_test_utils:save(Node, ?ID(s9), Space),

    % when
    Copy = Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:flush_messages(Context, subscriptions_test_utils:expectation(?ID(s9), Space)),
    empty_odd_seqs_in_cache(Node),

    % then
    subscriptions_test_utils:verify_messages_present(Copy, [
        subscriptions_test_utils:expectation(?ID(s1), Space),
        subscriptions_test_utils:expectation(?ID(s2), Space),
        subscriptions_test_utils:expectation(?ID(s3), Space),
        subscriptions_test_utils:expectation(?ID(s4), Space),
        subscriptions_test_utils:expectation(?ID(s5), Space),
        subscriptions_test_utils:expectation(?ID(s6), Space),
        subscriptions_test_utils:expectation(?ID(s7), Space),
        subscriptions_test_utils:expectation(?ID(s8), Space),
        subscriptions_test_utils:expectation(?ID(s9), Space)
    ]),
    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).

init_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, group_graph),
    test_utils:mock_expect(Nodes, group_graph, refresh_effective_caches, fun() -> ok end),
    Config.

end_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes),
    subscriptions_test_utils:flush(),
    ok.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================

empty_first_half_of_cache(Node) ->
    {ok, #document{value = #subscriptions_state{cache = Cache}}} =
        rpc:call(Node, subscriptions_state, get, [?SUBSCRIPTIONS_STATE_KEY]),

    AsList = gb_trees:to_list(Cache),
    {MedianSeq, _} = lists:nth(length(AsList) div 2, AsList),
    Filtered = lists:filter(fun({Seq, _}) -> Seq > MedianSeq end, AsList),

    UpdatedCache = lists:foldl(fun({Seq, Val}, Acc) ->
        gb_trees:enter(Seq, Val, Acc)
    end, gb_trees:empty(), Filtered),

    subscriptions_test_utils:update_document(Node, subscriptions_state, ?SUBSCRIPTIONS_STATE_KEY, #{
        cache => UpdatedCache
    }).

empty_odd_seqs_in_cache(Node) ->
    {ok, #document{value = #subscriptions_state{cache = Cache}}} =
        rpc:call(Node, subscriptions_state, get, [?SUBSCRIPTIONS_STATE_KEY]),

    AsList = gb_trees:to_list(Cache),
    Filter = fun({Seq, _}) -> (Seq rem 2) =:= 0 end,
    Filtered = lists:filter(Filter, AsList),

    UpdatedCache = lists:foldl(fun({Seq, Val}, Acc) ->
        gb_trees:enter(Seq, Val, Acc)
    end, gb_trees:empty(), Filtered),

    subscriptions_test_utils:update_document(Node, subscriptions_state, ?SUBSCRIPTIONS_STATE_KEY, #{
        cache => UpdatedCache
    }).
