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
-export([all/0]).
-export([
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    space_update_through_support_test/1,
    space_update_through_users_test/1,
    space_update_through_groups_test/1,
    no_space_update_test/1,
    share_update_through_support_test/1,
    no_share_update_test/1,
    handle_update_through_users_test/1,
    handle_update_through_groups_test/1,
    handle_service_update_through_users_test/1,
    handle_service_update_through_groups_test/1,
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
    child_group_update_through_users_test/1,
    fetches_changes_older_than_in_cache_without_save/1]).


%%%===================================================================
%%% API functions
%%%===================================================================
%% TODO move init_messages before save
all() -> ?ALL([
    provider_connection_checks_test,
    multiple_updates_test,
    space_update_through_support_test,
    space_update_through_users_test,
    space_update_through_groups_test,
    no_space_update_test,
    share_update_through_support_test,
    no_share_update_test,
    handle_update_through_users_test,
    handle_update_through_groups_test,
    handle_service_update_through_users_test,
    handle_service_update_through_groups_test,
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
%%    fetches_changes_older_than_in_cache_without_save,
    fetches_changes_from_both_cache_and_db,
    fetches_changes_when_cache_has_gaps,
    only_public_user_update_test,
    no_group_update_test
]).


provider_connection_checks_test(Config) ->
    [Node, OtherNode | _] = ?config(oz_worker_nodes, Config),
    StubConnFun = fun() -> receive _ -> ok after 5000 -> ok end end,
    P1 = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),

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
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    S1 = #od_space{name = <<"initial">>, providers = #{}},
    subscriptions_test_utils:save(Node, ?ID(s1), S1),

    % when
    Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:update_document(Node, od_space, ?ID(s1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_absent(Context, [
        subscriptions_test_utils:expectation(?ID(s1), S1#od_space{name = <<"initial">>}),
        subscriptions_test_utils:expectation(?ID(s1), S1#od_space{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers that support changed space
space_update_through_support_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), [?ID(s1)]),
    S1 = #od_space{name = <<"initial">>, providers = #{PID => 0}},
    subscriptions_test_utils:save(Node, ?ID(s1), S1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, []),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(s1), S1)),
    subscriptions_test_utils:update_document(Node, od_space, ?ID(s1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(s1), S1#od_space{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers where user that have access to space is logged-in
space_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    S1 = #od_space{name = <<"initial">>, users = #{?ID(u1) => []}},
    U1 = #od_user{name = <<"u1">>},

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(s1), S1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(s1), S1)),
    subscriptions_test_utils:update_document(Node, od_space, ?ID(s1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(s1), S1#od_space{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers where user that have access to space is logged-n
% and user is connected with space through group
space_update_through_groups_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    S1 = #od_space{name = <<"initial">>, groups = #{?ID(g1) => []}},
    U1 = #od_user{name = <<"u1">>},

    G1 = #od_group{
        name = <<"g1">>,
        users = #{?ID(u1) => privileges:group_admin()},
        spaces = [?ID(s1)]
    },

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(s1), S1),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(s1), S1)),
    subscriptions_test_utils:update_document(Node, od_space, ?ID(s1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(s1), S1#od_space{name = <<"updated">>})
    ]),
    ok.

% Checks if share update won't appear if provider does not support its space
no_share_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    Sp1 = #od_space{name = <<"whatever">>, providers = #{}, shares = [?ID(sh1)]},
    Sh1 = #od_share{name = <<"initial">>, space = ?ID(sp1)},
    subscriptions_test_utils:save(Node, ?ID(sp1), Sp1),
    subscriptions_test_utils:save(Node, ?ID(sh1), Sh1),

    % when
    Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:update_document(Node, od_share, ?ID(sh1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_absent(Context, [
        subscriptions_test_utils:expectation(?ID(sh1), Sh1#od_share{name = <<"initial">>}),
        subscriptions_test_utils:expectation(?ID(sh1), Sh1#od_share{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers where user that have access to handle is logged-in
handle_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    Timestamp = od_handle:actual_timestamp(),
    H1 = #od_handle{metadata = <<"initial">>, users = #{?ID(u1) => []}, timestamp = Timestamp},
    U1 = #od_user{name = <<"u1">>},

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(h1), H1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(h1), H1)),
    subscriptions_test_utils:update_document(Node, od_handle, ?ID(h1), #{metadata => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(h1), H1#od_handle{metadata = <<"updated">>, timestamp = Timestamp})
    ]),
    ok.

% Checks if update is pushed to providers where user that have access to handle is logged-n
% and user is connected with handle through group
handle_update_through_groups_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    Timestamp = od_handle:actual_timestamp(),
    H1 = #od_handle{metadata = <<"initial">>, groups = #{?ID(g1) => []}, timestamp = Timestamp},
    U1 = #od_user{name = <<"u1">>},

    G1 = #od_group{
        name = <<"g1">>,
        users = #{?ID(u1) => privileges:group_admin()},
        handles = [?ID(h1)]
    },

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(h1), H1),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(h1), H1)),
    subscriptions_test_utils:update_document(Node, od_handle, ?ID(h1), #{metadata => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(h1), H1#od_handle{metadata = <<"updated">>, timestamp = Timestamp})
    ]),
    ok.

% Checks if update is pushed to providers where user that have access to handle_service is logged-in
handle_service_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    HS1 = #od_handle_service{name = <<"initial">>, users = #{?ID(u1) => []}},
    U1 = #od_user{name = <<"u1">>},

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(hs1), HS1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(hs1), HS1)),
    subscriptions_test_utils:update_document(Node, od_handle_service, ?ID(hs1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(hs1), HS1#od_handle_service{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers where user that have access to handle_service is logged-n
% and user is connected with handle_service through group
handle_service_update_through_groups_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    HS1 = #od_handle_service{name = <<"initial">>, groups = #{?ID(g1) => []}},
    U1 = #od_user{name = <<"u1">>},

    G1 = #od_group{
        name = <<"g1">>,
        users = #{?ID(u1) => privileges:group_admin()},
        handle_services = [?ID(hs1)]
    },

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(hs1), HS1),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(hs1), HS1)),
    subscriptions_test_utils:update_document(Node, od_handle_service, ?ID(hs1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(hs1), HS1#od_handle_service{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers that support changed space
share_update_through_support_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), [?ID(sp1)]),
    Sp1 = #od_space{name = <<"whatever">>, providers = #{PID => 0}, shares = [?ID(sh1)]},
    Sh1 = #od_share{name = <<"initial">>, space = ?ID(sp1)},
    subscriptions_test_utils:save(Node, ?ID(sp1), Sp1),
    subscriptions_test_utils:save(Node, ?ID(sh1), Sh1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, []),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(sh1), Sh1)),
    subscriptions_test_utils:update_document(Node, od_share, ?ID(sh1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(sh1), Sh1#od_share{name = <<"updated">>})
    ]),
    ok.

all_data_in_space_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), [?ID(s1)]),
    PID2 = subscriptions_test_utils:create_provider(Config, ?ID(p2), [?ID(s1)]),
    PID3 = subscriptions_test_utils:create_provider(Config, ?ID(p3), [?ID(s1)]),
    User = #od_user{name = <<"user">>},
    Group = #od_group{name = <<"group">>},

    subscriptions_test_utils:save(Node, ?ID(u1), User),
    subscriptions_test_utils:save(Node, ?ID(u2), User),
    subscriptions_test_utils:save(Node, ?ID(g1), Group),
    subscriptions_test_utils:save(Node, ?ID(g2), Group),


    % when
    Space = #od_space{
        name = <<"space">>,
        users = #{?ID(u1) => privileges:space_manager(), ?ID(u2) => []},
        groups = #{?ID(g1) => privileges:space_admin(), ?ID(g2) => []},
        providers = #{PID => 0, PID2 => 100, PID3 => 1000}
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
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),

    % when
    User = #od_user{
        name = <<"user">>,
        groups = [?ID(g1), ?ID(g2)],
        spaces = [?ID(s1), ?ID(s2)],
        default_space = <<"s1">>,
        eff_groups = #{?ID(g1) => [], ?ID(g3) => [], ?ID(g3) => []}
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
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    User = #od_user{name = <<"user">>, groups = [?ID(g1)]},
    subscriptions_test_utils:save(Node, ?ID(u1), User),

    % when
    Group = #od_group{
        name = <<"Group">>,
        users = #{
            ?ID(u1) => privileges:group_admin(),
            ?ID(u2) => privileges:group_user()
        },
        eff_users = #{
            ?ID(u1) => {privileges:group_admin(), []},
            ?ID(u3) => {privileges:group_privileges(), []}
        },
        children = #{
            ?ID(g1) => privileges:group_admin(),
            ?ID(g3) => privileges:group_privileges()
        },
        parents = [?ID(g1), ?ID(g2)],
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
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), Spaces, Urls),

    % when
    Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:update_document(Node, od_provider, PID, #{name => Name}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(PID, #od_provider{spaces = Spaces, urls = Urls, name = Name})
    ]).

updates_for_with_providers_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), [?ID(s1), ?ID(s2)], [<<"p1-url1">>]),
    P2 = #od_provider{spaces = [?ID(s2)], urls = [<<"p2-url1">>, <<"p2-url2">>], name = <<"p2">>},
    P3 = #od_provider{spaces = [?ID(s1)], urls = [<<"p3-url1">>], name = <<"p3">>},


    % when
    Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:save(Node, ?ID(p2), P2),
    subscriptions_test_utils:save(Node, ?ID(p3), P3),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(PID, #od_provider{spaces = [?ID(s1), ?ID(s2)], urls = [<<"p1-url1">>], name = ?ID(p1)}),
        subscriptions_test_utils:public_only_provider_expectation(?ID(p2), P2#od_provider.name, P2#od_provider.urls),
        subscriptions_test_utils:public_only_provider_expectation(?ID(p3), P3#od_provider.name, P3#od_provider.urls)
    ]),
    ok.

only_public_user_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), [?ID(s1)]),
    U1 = #od_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),

    % when
    Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages(Context, [
        subscriptions_test_utils:public_only_user_expectation(?ID(u1), <<"updated">>)
    ], [
        subscriptions_test_utils:expectation(?ID(u1), U1),
        subscriptions_test_utils:expectation(?ID(u1), U1#od_user{name = <<"updated">>})
    ]),
    ok.

user_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), [?ID(s1)]),
    U1 = #od_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(u1), U1)),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(u1), U1#od_user{name = <<"updated">>})
    ]),
    ok.

simple_delete_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), [?ID(s1)]),
    U1 = #od_user{name = <<"u1">>, groups = [?ID(g1)]},
    S1 = #od_space{name = <<"s1">>, providers = #{PID => 0},
        users = #{?ID(u1) => []}},
    G1 = #od_group{name = <<"g1">>, users = #{?ID(u1) => []}},

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(s1), S1),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(u1), U1)),
    subscriptions_test_utils:delete_document(Node, od_user, ?ID(u1)),
    subscriptions_test_utils:delete_document(Node, od_group, ?ID(g1)),
    subscriptions_test_utils:delete_document(Node, od_space, ?ID(s1)),

    % then
    subscriptions_test_utils:verify_messages(Context, [
        [{<<"id">>, ?ID(g1)}, {<<"od_group">>, <<"delete">>}],
        [{<<"id">>, ?ID(u1)}, {<<"od_user">>, <<"delete">>}],
        [{<<"id">>, ?ID(s1)}, {<<"od_space">>, <<"delete">>}]
    ], []),
    ok.

% checks if no update is lost in case of many updates in short time
multiple_updates_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), [?ID(s1)]),
    U1 = #od_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(u1), U1)),

    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated1">>, groups => [<<"gr1">>]}),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated2">>}),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated3">>, groups => [<<"gr2">>], spaces => [<<"sp1">>]}),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated4">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(u1), U1#od_user{name = <<"updated4">>, groups = [<<"gr2">>], spaces = [<<"sp1">>]})
    ]),
    ok.

no_group_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    G1 = #od_group{name = <<"g1">>},
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    subscriptions_test_utils:update_document(Node, od_group, ?ID(g1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_absent(Context, [
        subscriptions_test_utils:expectation(?ID(g1), G1#od_group{name = <<"updated">>})
    ]),
    ok.

group_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    U1 = #od_user{name = <<"u1">>},
    G1 = #od_group{name = <<"g1">>, users = #{?ID(u1) => []}},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(g1), G1)),
    subscriptions_test_utils:update_document(Node, od_group, ?ID(g1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(g1), G1#od_group{name = <<"updated">>})
    ]),
    ok.

ancestor_group_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    U1 = #od_user{name = <<"u1">>,
        eff_groups = #{
            ?ID(g1) => [],
            ?ID(g2) => [],
            ?ID(g3) => []
        }},
    G1 = #od_group{name = <<"g1">>,
        users = #{?ID(u1) => []},
        parents = [?ID(g2)],
        eff_users = #{?ID(u1) => {[], []}}},
    G2 = #od_group{name = <<"g2">>,
        users = #{},
        parents = [?ID(g3)],
        children = #{?ID(g1) => []},
        eff_users = #{?ID(u1) => {[], []}}},
    G3 = #od_group{name = <<"g3">>,
        users = #{},
        children = #{?ID(g2) => []},
        eff_users = #{?ID(u1) => {[], []}}},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),
    subscriptions_test_utils:save(Node, ?ID(g2), G2),
    subscriptions_test_utils:save(Node, ?ID(g3), G3),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(g3), G3)),
    subscriptions_test_utils:update_document(Node, od_group, ?ID(g1), #{name => <<"updated">>}),
    subscriptions_test_utils:update_document(Node, od_group, ?ID(g2), #{name => <<"updated">>}),
    subscriptions_test_utils:update_document(Node, od_group, ?ID(g3), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(g1), G1#od_group{name = <<"updated">>}),
        subscriptions_test_utils:expectation(?ID(g2), G2#od_group{name = <<"updated">>}),
        subscriptions_test_utils:expectation(?ID(g3), G3#od_group{name = <<"updated">>})
    ]),
    ok.

child_group_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),

    U1 = #od_user{name = <<"u1">>,
        eff_groups = #{?ID(g1) => []}},
    U2 = #od_user{name = <<"u2">>,
        eff_groups = #{?ID(g2) => []}},

    G1 = #od_group{name = <<"g1">>,
        users = #{?ID(u1) => []},
        parents = [?ID(g2)],
        eff_children = #{?ID(g2) => {[], []}, ?ID(g1) => {[], []}},
        eff_users = #{?ID(u1) => {[], []}}},
    G2 = #od_group{name = <<"g2">>,
        users = #{?ID(u2) => []},
        children = #{?ID(g1) => []},
        eff_children = #{?ID(g1) => {[], []}},
        eff_users = #{?ID(u1) => {[], []}, ?ID(u2) => {[], []}}},

    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:save(Node, ?ID(u2), U2),
    subscriptions_test_utils:save(Node, ?ID(g2), G2),
    subscriptions_test_utils:save(Node, ?ID(g1), G1),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [?ID(u1)]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(g1), G1)),
    subscriptions_test_utils:update_document(Node, od_group, ?ID(g1), #{name => <<"updated">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(g1), G1#od_group{name = <<"updated">>})
    ]),
    ok.

updates_for_added_user_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), [?ID(s1), ?ID(s2)]),
    U1 = #od_user{name = <<"u1">>, groups = [?ID(g1)],
        eff_groups = #{?ID(g1) => [], ?ID(g2) => []}, spaces = [?ID(s2)]},
    G1 = #od_group{name = <<"g1">>, users = #{?ID(u1) => []},
        spaces = [?ID(s1)], eff_children = #{?ID(g1) => {[], []}, ?ID(g2) => {[], []}},
        parents = [?ID(g2)]},
    G2 = #od_group{name = <<"g1">>, users = #{},
        spaces = [], eff_children = #{?ID(g2) => {[], []}},
        children = #{?ID(g1) => [], ?ID(g3) => []}},
    G3 = #od_group{name = <<"g1">>, users = #{},
        spaces = [], eff_children = #{?ID(g3) => {[], []}, ?ID(g2) => {[], []}},
        parents = [?ID(g2)]},
    S1 = #od_space{name = <<"s1">>, providers = #{PID => 0}, groups = #{?ID(g1) => []}},
    S2 = #od_space{name = <<"s2">>, providers = #{PID => 0}, users = #{?ID(u1) => []}},
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
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    U1 = #od_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    Rev0 = subscriptions_test_utils:get_rev(Node, od_user, ?ID(u1)),

    % when
    Context = subscriptions_test_utils:init_messages(Node, PID, []),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated1">>}),
    Rev1 = subscriptions_test_utils:get_rev(Node, od_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated2">>}),
    Rev2 = subscriptions_test_utils:get_rev(Node, od_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated3">>}),
    Rev3 = subscriptions_test_utils:get_rev(Node, od_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated4">>}),
    Rev4 = subscriptions_test_utils:get_rev(Node, od_user, ?ID(u1)),

    % then
    subscriptions_test_utils:verify_messages_present(Context#subs_ctx{users = [(?ID(u1))]}, [
        subscriptions_test_utils:expectation_with_rev(
            [Rev4, Rev3, Rev2, Rev1, Rev0],
            subscriptions_test_utils:expectation(?ID(u1), U1#od_user{name = <<"updated4">>}))
    ]),
    ok.

updates_have_revisions_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    U1 = #od_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    Rev0 = subscriptions_test_utils:get_rev(Node, od_user, ?ID(u1)),

    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [(?ID(u1))]),
    Context = subscriptions_test_utils:flush_messages(Context1, subscriptions_test_utils:expectation(?ID(u1), U1)),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated1">>}),
    Rev1 = subscriptions_test_utils:get_rev(Node, od_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated2">>}),
    Rev2 = subscriptions_test_utils:get_rev(Node, od_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated3">>}),
    Rev3 = subscriptions_test_utils:get_rev(Node, od_user, ?ID(u1)),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated4">>}),
    Rev4 = subscriptions_test_utils:get_rev(Node, od_user, ?ID(u1)),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation_with_rev(
            [Rev4, Rev3, Rev2, Rev1, Rev0],
            subscriptions_test_utils:expectation(?ID(u1), U1#od_user{name = <<"updated4">>}))
    ]),
    ok.

fetches_changes_older_than_in_cache(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    U1 = #od_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated1">>}),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated2">>}),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated3">>}),

    Context = subscriptions_test_utils:init_messages(Node, PID, [(?ID(u1))]),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated4">>}),

    % when
    _ForgottenContext = subscriptions_test_utils:flush_messages(Context,
        subscriptions_test_utils:expectation(?ID(u1), U1#od_user{name = <<"updated4">>})),

    subscriptions_test_utils:empty_cache(Node),
    subscriptions_test_utils:save(Node, ?ID(u2), #od_user{name = <<"u2">>}),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(u1), U1#od_user{name = <<"updated4">>})
    ]),
    ok.

fetches_changes_older_than_in_cache_without_save(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), []),
    timer:sleep(timer:seconds(5)), % sleep to save provider at disk

    U1 = #od_user{name = <<"u1">>},
    subscriptions_test_utils:save(Node, ?ID(u1), U1),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated1">>}),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated2">>}),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated3">>}),

    Context = subscriptions_test_utils:init_messages(Node, PID, [(?ID(u1))]),
    subscriptions_test_utils:update_document(Node, od_user, ?ID(u1), #{name => <<"updated4">>}),

    % when
    _ForgottenContext = subscriptions_test_utils:flush_messages(Context,
        subscriptions_test_utils:expectation(?ID(u1), U1#od_user{name = <<"updated4">>})),

    subscriptions_test_utils:empty_cache(Node),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
        subscriptions_test_utils:expectation(?ID(u1), U1#od_user{name = <<"updated4">>})
    ]),
    ok.

fetches_changes_from_both_cache_and_db(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), [
        ?ID(s1), ?ID(s2), ?ID(s3), ?ID(s4), ?ID(s5), ?ID(s6), ?ID(s7), ?ID(s8), ?ID(s9)
    ]),
    Space = #od_space{name = <<"initial">>, providers = #{PID => 0}},
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
    PID = subscriptions_test_utils:create_provider(Config, ?ID(p1), [
        ?ID(s1), ?ID(s2), ?ID(s3), ?ID(s4), ?ID(s5), ?ID(s6), ?ID(s7), ?ID(s8), ?ID(s9)
    ]),
    Space = #od_space{name = <<"initial">>, providers = #{PID => 0}},
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
    [{?LOAD_MODULES, [oz_test_utils, subscriptions_test_utils]} | Config].

end_per_suite(_Config) ->
    timer:sleep(342342346),
    ok.

init_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, translator, [passthrough]),
    ok = test_utils:mock_expect(Nodes, translator, calculate_space_aliases,
        fun(_, _) ->
            #{}
        end),
    Config.

end_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, translator),
    subscriptions_test_utils:flush(),
    ok.

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
