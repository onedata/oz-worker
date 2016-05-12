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
    stress_test/1,
    provider_connection_checks_test/1,
    all_data_in_space_update_test/1,
    all_data_in_user_update_test/1,
    all_data_in_group_update_test/1]).

-define(MESSAGES_WAIT_TIMEOUT, timer:seconds(2)).
-define(MESSAGES_RECEIVE_ATTEMPTS, 60).

%% appends function name to id (atom) and yields binary accepted by the db
-define(ID(Id), list_to_binary(
    atom_to_list(Id) ++ " # " ++
        atom_to_list(element(2, element(2, process_info(self(), current_function))))
)).

%% helper record for maintaining subscription progress between message receives
-record(subs_ctx, {
    node :: node(),
    provider :: binary(),
    users :: [binary()],
    resume_at :: subscriptions:seq(),
    missing :: [subscriptions:seq()]
}).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    provider_connection_checks_test,
    multiple_updates_test,
    space_update_through_support_test,
    space_update_through_users_test,
    space_update_through_groups_test,
    all_data_in_space_update_test,
    all_data_in_user_update_test,
    all_data_in_group_update_test,
    user_update_test,
    simple_delete_test,
    group_update_through_users_test,
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
    P1 = create_provider(Node, ?ID(p1), []),

    %% no connection
    ?assertNot(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% local connection
    LocalConn = proc_lib:spawn(Node, StubConnFun),
    call_worker(Node, {add_connection, P1, LocalConn}),
    ?assert(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% broken local connection
    exit(LocalConn, killed),
    ?assertNot(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% other local connection
    NewLocalConn = proc_lib:spawn(Node, StubConnFun),
    call_worker(Node, {add_connection, P1, NewLocalConn}),
    ?assert(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% connections removed
    call_worker(Node, {remove_connection, P1, LocalConn}),
    call_worker(Node, {remove_connection, P1, NewLocalConn}),
    ?assertNot(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% connection at different worker
    RemoteConn = proc_lib:spawn(OtherNode, StubConnFun),
    call_worker(Node, {add_connection, P1, RemoteConn}),
    ?assert(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% addition of broken connection
    NewRemoteConn = proc_lib:spawn(OtherNode, StubConnFun),
    exit(NewRemoteConn, killed),
    call_worker(Node, {add_connection, P1, NewRemoteConn}),
    ?assert(rpc:call(Node, subscriptions, any_connection_active, [P1])),

    %% all connections broken
    exit(RemoteConn, killed),
    ?assertNot(rpc:call(Node, subscriptions, any_connection_active, [P1])),
    ok.

% Checks if update won't appear if provider does not support space
no_space_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), []),
    S1 = #space{name = <<"initial">>, providers_supports = []},
    save(Node, ?ID(s1), S1),

    % when
    Context = init_messages(Node, PID, []),
    update_document(Node, space, ?ID(s1), #{name => <<"updated">>}),

    % then
    verify_messages_absent(Context, [
        expectation(?ID(s1), S1#space{name = <<"initial">>}),
        expectation(?ID(s1), S1#space{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers that support changed space
space_update_through_support_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), [?ID(s1)]),
    S1 = #space{name = <<"initial">>, providers_supports = [{PID, 0}]},
    save(Node, ?ID(s1), S1),

    % when
    Context1 = init_messages(Node, PID, []),
    Context = flush_messages(Context1, expectation(?ID(s1), S1)),
    update_document(Node, space, ?ID(s1), #{name => <<"updated">>}),

    % then
    verify_messages_present(Context, [
        expectation(?ID(s1), S1#space{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers where user that have access to space is logged-in
space_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), []),
    S1 = #space{name = <<"initial">>, users = [{?ID(u1), []}]},
    U1 = #onedata_user{name = <<"u1">>},

    save(Node, ?ID(u1), U1),
    save(Node, ?ID(s1), S1),

    % when
    Context1 = init_messages(Node, PID, [?ID(u1)]),
    Context = flush_messages(Context1, expectation(?ID(s1), S1)),
    update_document(Node, space, ?ID(s1), #{name => <<"updated">>}),

    % then
    verify_messages_present(Context, [
        expectation(?ID(s1), S1#space{name = <<"updated">>})
    ]),
    ok.

% Checks if update is pushed to providers where user that have access to space is logged-n
% and user is connected with space through group
space_update_through_groups_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), []),
    S1 = #space{name = <<"initial">>, groups = [{?ID(g1), []}]},
    U1 = #onedata_user{name = <<"u1">>},

    G1 = #user_group{
        name = <<"g1">>,
        users = [{?ID(u1), privileges:group_admin()}],
        spaces = [?ID(s1)]
    },

    save(Node, ?ID(u1), U1),
    save(Node, ?ID(s1), S1),
    save(Node, ?ID(g1), G1),

    % when
    Context1 = init_messages(Node, PID, [?ID(u1)]),
    Context = flush_messages(Context1, expectation(?ID(s1), S1)),
    update_document(Node, space, ?ID(s1), #{name => <<"updated">>}),

    % then
    verify_messages_present(Context, [
        expectation(?ID(s1), S1#space{name = <<"updated">>})
    ]),
    ok.

all_data_in_space_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), [?ID(s1)]),
    PID2 = create_provider(Node, ?ID(p2), [?ID(s1)]),
    PID3 = create_provider(Node, ?ID(p3), [?ID(s1)]),
    User = #onedata_user{name = <<"user">>},
    Group = #user_group{name = <<"group">>},

    save(Node, ?ID(u1), User),
    save(Node, ?ID(u2), User),
    save(Node, ?ID(g1), Group),
    save(Node, ?ID(g2), Group),


    % when
    Space = #space{
        name = <<"space">>,
        users = [{?ID(u1), privileges:space_manager()}, {?ID(u2), []}],
        groups = [{?ID(g1), privileges:space_admin()}, {?ID(g2), []}],
        providers_supports = [{PID, 0}, {PID2, 100}, {PID3, 1000}]
    },
    Context = init_messages(Node, PID, []),
    save(Node, ?ID(s1), Space),

    % then
    verify_messages_present(Context, [
        expectation(?ID(s1), Space)
    ]),
    ok.

all_data_in_user_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), []),

    % when
    User = #onedata_user{
        name = <<"user">>,
        groups = [?ID(g1), ?ID(g2)],
        spaces = [?ID(s1), ?ID(s2)],
        default_space = <<"s1">>,
        effective_groups = [?ID(g1), ?ID(g3), ?ID(g3)]
    },
    Context = init_messages(Node, PID, [?ID(u1)]),
    save(Node, ?ID(u1), User),

    % then
    verify_messages_present(Context, [
        expectation(?ID(u1), User)
    ]),
    ok.

all_data_in_group_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), []),
    User = #onedata_user{name = <<"user">>, groups = [?ID(g1)]},
    save(Node, ?ID(u1), User),

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
    Context = init_messages(Node, PID, [?ID(u1)]),
    save(Node, ?ID(g1), Group),

    % then
    verify_messages_present(Context, [
        expectation(?ID(g1), Group)
    ]),
    ok.

only_public_user_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), [?ID(s1)]),
    U1 = #onedata_user{name = <<"u1">>},
    save(Node, ?ID(u1), U1),

    % when
    Context = init_messages(Node, PID, []),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated">>}),

    % then
    verify_messages(Context, [
        public_only_user_expectation(?ID(u1), <<"updated">>)
    ], [
        expectation(?ID(u1), U1),
        expectation(?ID(u1), U1#onedata_user{name = <<"updated">>})
    ]),
    ok.

user_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), [?ID(s1)]),
    U1 = #onedata_user{name = <<"u1">>},
    save(Node, ?ID(u1), U1),

    % when
    Context1 = init_messages(Node, PID, [?ID(u1)]),
    Context = flush_messages(Context1, expectation(?ID(u1), U1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated">>}),

    % then
    verify_messages_present(Context, [
        expectation(?ID(u1), U1#onedata_user{name = <<"updated">>})
    ]),
    ok.

simple_delete_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), [?ID(s1)]),
    U1 = #onedata_user{name = <<"u1">>, groups = [?ID(g1)]},
    S1 = #space{name = <<"s1">>, providers_supports = [{PID, 0}],
        users = [{?ID(u1), []}]},
    G1 = #user_group{name = <<"g1">>, users = [{?ID(u1), []}]},

    save(Node, ?ID(u1), U1),
    save(Node, ?ID(s1), S1),
    save(Node, ?ID(g1), G1),

    % when
    Context1 = init_messages(Node, PID, [?ID(u1)]),
    Context = flush_messages(Context1, expectation(?ID(u1), U1)),
    delete_document(Node, onedata_user, ?ID(u1)),
    delete_document(Node, user_group, ?ID(g1)),
    delete_document(Node, space, ?ID(s1)),

    % then
    verify_messages(Context, [
        [{<<"id">>, ?ID(g1)}, {<<"group">>, <<"delete">>}],
        [{<<"id">>, ?ID(u1)}, {<<"user">>, <<"delete">>}],
        [{<<"id">>, ?ID(s1)}, {<<"space">>, <<"delete">>}]
    ], []),
    ok.

% checks if no update is lost in case of many updates in short time
multiple_updates_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), [?ID(s1)]),
    U1 = #onedata_user{name = <<"u1">>},
    save(Node, ?ID(u1), U1),

    % when
    Context1 = init_messages(Node, PID, [?ID(u1)]),
    Context = flush_messages(Context1, expectation(?ID(u1), U1)),

    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>, groups => [<<"gr1">>]}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>, groups => [<<"gr2">>], spaces => [<<"sp1">>]}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),

    % then
    verify_messages_present(Context, [
        expectation(?ID(u1), U1#onedata_user{name = <<"updated4">>, groups = [<<"gr2">>], spaces = [<<"sp1">>]})
    ]),
    ok.

no_group_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), []),
    G1 = #user_group{name = <<"g1">>},
    save(Node, ?ID(g1), G1),

    % when
    Context = init_messages(Node, PID, [?ID(u1)]),
    update_document(Node, user_group, ?ID(g1), #{name => <<"updated">>}),

    % then
    verify_messages_absent(Context, [
        expectation(?ID(g1), G1#user_group{name = <<"updated">>})
    ]),
    ok.

group_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), []),
    U1 = #onedata_user{name = <<"u1">>},
    G1 = #user_group{name = <<"g1">>, users = [{?ID(u1), []}]},
    save(Node, ?ID(u1), U1),
    save(Node, ?ID(g1), G1),

    % when
    Context1 = init_messages(Node, PID, [?ID(u1)]),
    Context = flush_messages(Context1, expectation(?ID(g1), G1)),
    update_document(Node, user_group, ?ID(g1), #{name => <<"updated">>}),

    % then
    verify_messages_present(Context, [
        expectation(?ID(g1), G1#user_group{name = <<"updated">>})
    ]),
    ok.

ancestor_group_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), []),
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
    save(Node, ?ID(u1), U1),
    save(Node, ?ID(g1), G1),
    save(Node, ?ID(g2), G2),
    save(Node, ?ID(g3), G3),

    % when
    Context1 = init_messages(Node, PID, [?ID(u1)]),
    Context = flush_messages(Context1, expectation(?ID(g3), G3)),
    update_document(Node, user_group, ?ID(g1), #{name => <<"updated">>}),
    update_document(Node, user_group, ?ID(g2), #{name => <<"updated">>}),
    update_document(Node, user_group, ?ID(g3), #{name => <<"updated">>}),

    % then
    verify_messages_present(Context, [
        expectation(?ID(g1), G1#user_group{name = <<"updated">>}),
        expectation(?ID(g2), G2#user_group{name = <<"updated">>}),
        expectation(?ID(g3), G3#user_group{name = <<"updated">>})
    ]),
    ok.

updates_for_added_user_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), [?ID(s1), ?ID(s2)]),
    U1 = #onedata_user{name = <<"u1">>, groups = [?ID(g1)], spaces = [?ID(s2)]},
    G1 = #user_group{name = <<"g1">>, users = [{?ID(u1), []}], spaces = [?ID(s1)]},
    S1 = #space{name = <<"s1">>, providers_supports = [{PID, 0}], groups = [{?ID(g1), []}]},
    S2 = #space{name = <<"s2">>, providers_supports = [{PID, 0}], users = [{?ID(u1), []}]},
    save(Node, ?ID(u1), U1),
    save(Node, ?ID(g1), G1),
    save(Node, ?ID(s1), S1),
    save(Node, ?ID(s2), S2),


    Context1 = init_messages(Node, PID, []),
    Context = flush_messages(Context1, expectation(?ID(s2), S2)),

    % when & then
    verify_messages_present(Context#subs_ctx{users = [?ID(u1)]}, [
        expectation(?ID(u1), U1),
        expectation(?ID(g1), G1),
        expectation(?ID(s1), S1),
        expectation(?ID(s2), S2)
    ]),
    ok.

updates_for_added_user_have_revisions_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), []),
    U1 = #onedata_user{name = <<"u1">>},
    save(Node, ?ID(u1), U1),
    Rev0 = get_rev(Node, onedata_user, ?ID(u1)),

    % when
    Context = init_messages(Node, PID, []),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>}),
    Rev1 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    Rev2 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>}),
    Rev3 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),
    Rev4 = get_rev(Node, onedata_user, ?ID(u1)),

    % then
    verify_messages_present(Context#subs_ctx{users = [(?ID(u1))]}, [
        expectation_with_rev(
            [Rev4, Rev3, Rev2, Rev1, Rev0],
            expectation(?ID(u1), U1#onedata_user{name = <<"updated4">>}))
    ]),
    ok.

updates_have_revisions_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), []),
    U1 = #onedata_user{name = <<"u1">>},
    save(Node, ?ID(u1), U1),
    Rev0 = get_rev(Node, onedata_user, ?ID(u1)),

    % when
    Context1 = init_messages(Node, PID, [(?ID(u1))]),
    Context = flush_messages(Context1, expectation(?ID(u1), U1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>}),
    Rev1 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    Rev2 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>}),
    Rev3 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),
    Rev4 = get_rev(Node, onedata_user, ?ID(u1)),

    % then
    verify_messages_present(Context, [
        expectation_with_rev(
            [Rev4, Rev3, Rev2, Rev1, Rev0],
            expectation(?ID(u1), U1#onedata_user{name = <<"updated4">>}))
    ]),
    ok.

fetches_changes_older_than_in_cache(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), []),
    U1 = #onedata_user{name = <<"u1">>},
    save(Node, ?ID(u1), U1),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),

    % when
    Context = init_messages(Node, PID, [(?ID(u1))]),
    _ForgottenContext = flush_messages(Context,
        expectation(?ID(u1), U1#onedata_user{name = <<"updated4">>})),

    empty_cache(Node),

    % then
    verify_messages_present(Context, [
        expectation(?ID(u1), U1#onedata_user{name = <<"updated4">>})
    ]),
    ok.


fetches_changes_from_both_cache_and_db(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), [
        ?ID(s1), ?ID(s2), ?ID(s3), ?ID(s4), ?ID(s5), ?ID(s6), ?ID(s7), ?ID(s8), ?ID(s9)
    ]),
    Space = #space{name = <<"initial">>, providers_supports = [{PID, 0}]},
    save(Node, ?ID(s1), Space),
    save(Node, ?ID(s3), Space),
    save(Node, ?ID(s2), Space),
    save(Node, ?ID(s4), Space),
    save(Node, ?ID(s5), Space),
    save(Node, ?ID(s6), Space),
    save(Node, ?ID(s7), Space),
    save(Node, ?ID(s8), Space),
    save(Node, ?ID(s9), Space),

    % when
    Copy = Context = init_messages(Node, PID, []),
    flush_messages(Context, expectation(?ID(s9), Space)),
    empty_first_half_of_cache(Node),

    % then
    verify_messages_present(Copy, [
        expectation(?ID(s1), Space),
        expectation(?ID(s2), Space),
        expectation(?ID(s3), Space),
        expectation(?ID(s4), Space),
        expectation(?ID(s5), Space),
        expectation(?ID(s6), Space),
        expectation(?ID(s7), Space),
        expectation(?ID(s8), Space),
        expectation(?ID(s9), Space)
    ]),
    ok.

stress_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),

    ProvidersCount = ?config(providers_count, Config),
    DocsCount = ?config(docs_count, Config),

    Results = utils:pmap(fun(ID) ->
        %% given
        PNameList = "provider_" ++ integer_to_list(ID),
        PName = list_to_binary(PNameList),
        SIDs = lists:map(fun(ID1) ->
            list_to_binary("space_" ++ integer_to_list(ID1) ++ "@" ++ PNameList)
        end, lists:seq(1, DocsCount)),

        %% when
        PID = create_provider(Node, PName, SIDs),
        Space = #space{name = <<"name">>, providers_supports = [{PID, 0}]},
        Context = init_messages(Node, PID, []),
        lists:map(fun(SID) ->
            save(Node, SID, Space)
        end, SIDs),

        %% then
        Start = erlang:system_time(milli_seconds),
        verify_messages_present(Context,
            lists:map(fun(SID) -> expectation(SID, Space) end, SIDs)
        ),
        {ok, erlang:system_time(milli_seconds) - Start}
    end, lists:seq(1, ProvidersCount)),

    lists:map(fun(Res) ->
        ?assertMatch({ok, _}, Res)
    end, Results),

    UpdatesMeanTime = lists:sum(lists:map(fun
        ({ok, Time}) -> Time
    end, Results)) / length(Results),

    [
        #parameter{name = updates_await, value = UpdatesMeanTime, unit = "ms",
            description = "Time until every update arrived (providers mean)"}
    ].

fetches_changes_when_cache_has_gaps(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    PID = create_provider(Node, ?ID(p1), [
        ?ID(s1), ?ID(s2), ?ID(s3), ?ID(s4), ?ID(s5), ?ID(s6), ?ID(s7), ?ID(s8), ?ID(s9)
    ]),
    Space = #space{name = <<"initial">>, providers_supports = [{PID, 0}]},
    save(Node, ?ID(s1), Space),
    save(Node, ?ID(s2), Space),
    save(Node, ?ID(s3), Space),
    save(Node, ?ID(s4), Space),
    save(Node, ?ID(s5), Space),
    save(Node, ?ID(s6), Space),
    save(Node, ?ID(s7), Space),
    save(Node, ?ID(s8), Space),
    save(Node, ?ID(s9), Space),

    % when
    Copy = Context = init_messages(Node, PID, []),
    flush_messages(Context, expectation(?ID(s9), Space)),
    empty_odd_seqs_in_cache(Node),

    % then
    verify_messages_present(Copy, [
        expectation(?ID(s1), Space),
        expectation(?ID(s2), Space),
        expectation(?ID(s3), Space),
        expectation(?ID(s4), Space),
        expectation(?ID(s5), Space),
        expectation(?ID(s6), Space),
        expectation(?ID(s7), Space),
        expectation(?ID(s8), Space),
        expectation(?ID(s9), Space)
    ]),
    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).

init_per_testcase(_, _Config) ->
    _Config.

end_per_testcase(_, _Config) ->
    flush(),
    ok.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================

empty_cache(Node) ->
    update_document(Node, subscriptions_state, ?SUBSCRIPTIONS_STATE_KEY, #{
        cache => gb_trees:empty()
    }).

empty_first_half_of_cache(Node) ->
    {ok, #document{value = #subscriptions_state{cache = Cache}}} =
        rpc:call(Node, subscriptions_state, get, [?SUBSCRIPTIONS_STATE_KEY]),

    AsList = gb_trees:to_list(Cache),
    {MedianSeq, _} = lists:nth(length(AsList) div 2, AsList),
    Filtered = lists:filter(fun({Seq, _}) -> Seq > MedianSeq end, AsList),

    UpdatedCache = lists:foldl(fun({Seq, Val}, Acc) ->
        gb_trees:enter(Seq, Val, Acc)
    end, gb_trees:empty(), Filtered),

    update_document(Node, subscriptions_state, ?SUBSCRIPTIONS_STATE_KEY, #{
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

    update_document(Node, subscriptions_state, ?SUBSCRIPTIONS_STATE_KEY, #{
        cache => UpdatedCache
    }).

call_worker(Node, Req) ->
    rpc:call(Node, worker_proxy, call, [?SUBSCRIPTIONS_WORKER_NAME, Req]).

%%%===================================================================
%%% Internal: datastore setup
%%%===================================================================

save(Node, ID, Value) ->
    ?assertMatch({ok, ID}, rpc:call(Node, element(1, Value), save,
        [#document{key = ID, value = Value}])).

update_document(Node, Model, ID, Diff) ->
    ?assertMatch({ok, _}, rpc:call(Node, Model, update, [ID, Diff])).

delete_document(Node, Model, ID) ->
    ?assertMatch(ok, rpc:call(Node, Model, delete, [ID])).

get_rev(Node, Model, ID) ->
    Result = rpc:call(Node, Model, get, [ID]),
    ?assertMatch({ok, _}, Result),
    {ok, #document{rev = Rev}} = Result,
    Rev.

create_provider(Node, Name, Spaces) ->
    {_, CSRFile, _} = generate_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    Params = [Name, [<<"127.0.0.1">>], <<"https://127.0.0.1:443">>, CSR],
    {ok, ID, _} = rpc:call(Node, provider_logic, create, Params),
    {ok, ID} = rpc:call(Node, provider, update, [ID, #{spaces => Spaces}]),
    ID.


generate_cert_files() ->
    {MegaSec, Sec, MiliSec} = erlang:now(),
    Prefix = lists:foldl(fun(Int, Acc) ->
        Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {KeyFile, CSRFile, CertFile}.


%%%===================================================================
%%% Internal: Message expectations
%%%===================================================================

expectation(ID, #space{name = Name, providers_supports = Supports,
    groups = Groups, users = Users}) ->
    space_expectation(ID, Name, Users, Groups, Supports);
expectation(ID, #onedata_user{name = Name, groups = Groups, space_names = SpaceNames,
    default_space = DefaultSpace, effective_groups = EGroups}) ->
    user_expectation(ID, Name, maps:to_list(SpaceNames), Groups, EGroups, case DefaultSpace of
        undefined -> <<"undefined">>;
        _ -> DefaultSpace
    end);
expectation(ID, #user_group{name = Name, users = Users, spaces = Spaces,
    effective_users = EUsers, nested_groups = NGroups, parent_groups = PGroups}) ->
    group_expectation(ID, Name, Users, EUsers, Spaces, NGroups, PGroups).

space_expectation(ID, Name, Users, Groups, Supports) ->
    [{<<"id">>, ID}, {<<"space">>, [
        {<<"id">>, ID},
        {<<"name">>, Name},
        {<<"providers_supports">>, Supports},
        {<<"users">>, privileges_as_binaries(Users)},
        {<<"groups">>, privileges_as_binaries(Groups)}
    ]}].

user_expectation(ID, Name, Spaces, Groups, EGroups, DefaultSpace) ->
    [{<<"id">>, ID}, {<<"user">>, [
        {<<"name">>, Name},
        {<<"space_names">>, Spaces},
        {<<"group_ids">>, Groups},
        {<<"effective_group_ids">>, EGroups},
        {<<"default_space">>, DefaultSpace},
        {<<"public_only">>, false}
    ]}].

public_only_user_expectation(ID, Name) ->
    [{<<"id">>, ID}, {<<"user">>, [
        {<<"name">>, Name},
        {<<"space_ids">>, []},
        {<<"group_ids">>, []},
        {<<"effective_group_ids">>, []},
        {<<"default_space">>, <<"undefined">>},
        {<<"public_only">>, true}
    ]}].

group_expectation(ID, Name, Users, EUsers, Spaces, NGroups, PGroups) ->
    [{<<"id">>, ID}, {<<"group">>, [
        {<<"name">>, Name},
        {<<"spaces">>, Spaces},
        {<<"users">>, privileges_as_binaries(Users)},
        {<<"effective_users">>, privileges_as_binaries(EUsers)},
        {<<"nested_groups">>, privileges_as_binaries(NGroups)},
        {<<"parent_groups">>, PGroups}
    ]}].

privileges_as_binaries(IDsWithPrivileges) ->
    lists:map(fun({ID, Privileges}) ->
        {ID, lists:map(fun(Privilege) ->
            atom_to_binary(Privilege, latin1)
        end, Privileges)}
    end, IDsWithPrivileges).

expectation_with_rev(Revs, Expectation) ->
    [{<<"revs">>, Revs} | Expectation].

%%%===================================================================
%%% Internal: Message presence/absence verification
%%%===================================================================

verify_messages_present(Context, Expected) ->
    verify_messages_present(Context, Expected, ?MESSAGES_RECEIVE_ATTEMPTS).

verify_messages_present(Context, Expected, AttemptsLimit) ->
    verify_messages(Context, AttemptsLimit, Expected, []).

verify_messages_absent(Context, Forbidden) ->
    verify_messages(Context, ?MESSAGES_RECEIVE_ATTEMPTS, [], Forbidden).

init_messages(Node, ProviderID, Users) ->
    call_worker(Node, {add_connection, ProviderID, self()}),

    Start = case rpc:call(Node, changes_cache, newest_seq, []) of
        {ok, Val} -> Val; _ -> 0
    end,


    #subs_ctx{node = Node, provider = ProviderID,
        users = Users, resume_at = Start, missing = []}.

flush_messages(Context, LastExpected) ->
    UpdatedContext = verify_messages(Context#subs_ctx{
        %% if changes were actually send before init, we could miss last expected
        resume_at = Context#subs_ctx.resume_at - 3
    }, [LastExpected], []),
    flush(),
    UpdatedContext.

flush() ->
    receive _ -> flush() after ?MESSAGES_WAIT_TIMEOUT -> ok end.

verify_messages(Context, Expected, Forbidden) ->
    verify_messages(Context, ?MESSAGES_RECEIVE_ATTEMPTS, Expected, Forbidden).

verify_messages(Context, _, [], []) ->
    Context;
verify_messages(Context, 0, Expected, _) ->
    ?assertMatch(Expected, []),
    Context;
verify_messages(Context, Retries, Expected, Forbidden) ->
    #subs_ctx{node = Node, provider = ProviderID,
        users = Users, resume_at = ResumeAt, missing = Missing} = Context,

    call_worker(Node, {update_users, ProviderID, Users}),
    call_worker(Node, {update_missing_seq, ProviderID, ResumeAt, Missing}),
    All = lists:append(get_messages()),

    Seqs = extract_seqs(All),
    NextResumeAt = largest([ResumeAt | Seqs]),
    NewExpectedSeqs = new_expected_seqs(NextResumeAt, ResumeAt),
    NextMissing = (Missing ++ NewExpectedSeqs) -- Seqs,
    NextContext = Context#subs_ctx{
        resume_at = NextResumeAt,
        missing = NextMissing
    },

    ?assertMatch(Forbidden, remove_matched_expectations(Forbidden, All)),
    RemainingExpected = remove_matched_expectations(Expected, All),
    verify_messages(NextContext, Retries - 1, RemainingExpected, Forbidden).

get_messages() ->
    receive {push, Messages} ->
        [json_utils:decode(Messages)]
    after ?MESSAGES_WAIT_TIMEOUT -> [] end.


new_expected_seqs(NextResumeAt, ResumeAt) ->
    case NextResumeAt > (ResumeAt + 1) of
        true -> lists:seq(ResumeAt + 1, NextResumeAt);
        false -> []
    end.

largest(List) ->
    hd(lists:reverse(lists:usort(List))).

extract_seqs(Messages) ->
    lists:map(fun(Message) ->
        proplists:get_value(<<"seq">>, Message, -2)
    end, Messages).

remove_matched_expectations(Expected, Messages) ->
    lists:filter(fun(Exp) ->
        lists:all(fun(Msg) -> length(Exp -- Msg) =/= 0 end, Messages)
    end, Expected).
