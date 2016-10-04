%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This test suite covers group graph related behaviour.
%%% @end
%%%-------------------------------------------------------------------
-module(group_graph_test_SUITE).
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
-export([grand_scenario_test/1, conditional_update_test/1,
    nested_groups_in_dev_setup_test/1, cycles_elimination_test/1,
    user_becoming_groupless_test/1, concurrent_updates_test/1]).

-define(assertUnorderedMatch(Guard, Expr), (fun() ->
    Sorted = lists:sort(Guard),
    ?assertMatch(Sorted, lists:sort(Expr))
end)()).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    concurrent_updates_test,
    user_becoming_groupless_test,
    cycles_elimination_test,
    grand_scenario_test,
    conditional_update_test,
    nested_groups_in_dev_setup_test
]).

nested_groups_in_dev_setup_test(Config) ->
    [Node] = ?config(oz_worker_nodes, Config),
    save(Node, #document{key = <<"user1">>, value = #od_user{groups = []}}),

    ?assertMatch(ok, rpc:call(Node, dev_utils, set_up_test_entities, [[], [
        {<<"group1">>, [{<<"users">>, [<<"user1">>]}, {<<"groups">>, [<<"group2">>]}]},
        {<<"group2">>, [{<<"users">>, [<<"user1">>]}, {<<"groups">>, [<<"group3">>, <<"group4">>]}]},
        {<<"group3">>, [{<<"users">>, [<<"user1">>]}, {<<"groups">>, []}]},
        {<<"group4">>, [{<<"users">>, [<<"user1">>]}, {<<"groups">>, []}]}
    ], []])),

    #document{value = #od_group{children = C1, parents = P1, eff_children = EG1}}
        = get(Node, user_group, <<"group1">>),
    #document{value = #od_group{children = C2, parents = P2, eff_children = EG2}}
        = get(Node, user_group, <<"group2">>),
    #document{value = #od_group{children = C3, parents = P3, eff_children = EG3}}
        = get(Node, user_group, <<"group3">>),
    #document{value = #od_group{children = C4, parents = P4, eff_children = EG4}}
        = get(Node, user_group, <<"group4">>),

    ?assertUnorderedMatch([{<<"group2">>, [group_view_data]}], C1),
    ?assertUnorderedMatch([{<<"group3">>, [group_view_data]}, {<<"group4">>, [group_view_data]}], C2),
    ?assertUnorderedMatch([], C3),
    ?assertUnorderedMatch([], C4),

    ?assertUnorderedMatch([], P1),
    ?assertUnorderedMatch([<<"group1">>], P2),
    ?assertUnorderedMatch([<<"group2">>], P3),
    ?assertUnorderedMatch([<<"group2">>], P4),

    ?assertUnorderedMatch([<<"group1">>], EG1),
    ?assertUnorderedMatch([<<"group1">>, <<"group2">>], EG2),
    ?assertUnorderedMatch([<<"group1">>, <<"group2">>, <<"group3">>], EG3),
    ?assertUnorderedMatch([<<"group1">>, <<"group2">>, <<"group4">>], EG4),
    ok.

concurrent_updates_test(Config) ->
    [Node] = ?config(oz_worker_nodes, Config),

    GIDs = [A, B, C, D, E, F, G, H, I, J, K, L, M, N] = [<<"A">>, <<"B">>, <<"C">>, <<"D">>,
        <<"E">>, <<"F">>, <<"G">>, <<"H">>, <<"I">>, <<"J">>, <<"K">>, <<"L">>, <<"M">>, <<"N">>],
    UIDs = [U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14] = [<<"U1">>, <<"U2">>, <<"U3">>, <<"U4">>,
        <<"U5">>, <<"U6">>, <<"U7">>, <<"U8">>, <<"U9">>, <<"U10">>, <<"U11">>, <<"U12">>, <<"U13">>, <<"U14">>],

    User = privileges:group_user(),
    Manager = privileges:group_manager(),

    save(Node, #document{key = U1, value = #od_user{}}),
    save(Node, #document{key = U2, value = #od_user{}}),
    save(Node, #document{key = U3, value = #od_user{}}),
    save(Node, #document{key = U4, value = #od_user{}}),
    save(Node, #document{key = U5, value = #od_user{}}),
    save(Node, #document{key = U6, value = #od_user{}}),
    save(Node, #document{key = U7, value = #od_user{}}),
    save(Node, #document{key = U8, value = #od_user{}}),
    save(Node, #document{key = U9, value = #od_user{}}),
    save(Node, #document{key = U10, value = #od_user{}}),
    save(Node, #document{key = U11, value = #od_user{}}),
    save(Node, #document{key = U12, value = #od_user{}}),
    save(Node, #document{key = U13, value = #od_user{}}),
    save(Node, #document{key = U14, value = #od_user{}}),

    save(Node, #document{key = A, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = B, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = C, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = D, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = E, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = F, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = G, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = H, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = I, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = J, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = K, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = L, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = M, value = #od_group{parents = [], children = [], users = []}}),
    save(Node, #document{key = N, value = #od_group{parents = [], children = [], users = []}}),

    FirstLinkOps = [
        {user_group, A, #{parent_groups => [], nested_groups => [{B, User}], users => []}},
        {user_group, B, #{parent_groups => [A], nested_groups => [], users => []}},
        {user_group, C, #{parent_groups => [], nested_groups => [{D, User}], users => []}},
        {user_group, D, #{parent_groups => [C], nested_groups => [], users => []}},
        {user_group, E, #{parent_groups => [], nested_groups => [{F, User}], users => []}},
        {user_group, F, #{parent_groups => [E], nested_groups => [], users => []}},
        {user_group, G, #{parent_groups => [], nested_groups => [{H, User}], users => []}},
        {user_group, H, #{parent_groups => [G], nested_groups => [], users => []}},
        {user_group, I, #{parent_groups => [], nested_groups => [{J, User}], users => []}},
        {user_group, J, #{parent_groups => [I], nested_groups => [], users => []}},
        {user_group, K, #{parent_groups => [], nested_groups => [{L, User}], users => []}},
        {user_group, L, #{parent_groups => [K], nested_groups => [], users => []}},
        {user_group, M, #{parent_groups => [], nested_groups => [{N, User}], users => []}},
        {user_group, N, #{parent_groups => [M], nested_groups => [], users => []}}
    ],
    SecondLinkOps = [
        {user_group, A, #{parent_groups => [], nested_groups => [{B, User}], users => []}},
        {user_group, B, #{parent_groups => [A], nested_groups => [{C, User}], users => []}},
        {user_group, C, #{parent_groups => [B], nested_groups => [{D, User}], users => []}},
        {user_group, D, #{parent_groups => [C], nested_groups => [{E, User}], users => []}},
        {user_group, E, #{parent_groups => [D], nested_groups => [{F, User}], users => []}},
        {user_group, F, #{parent_groups => [E], nested_groups => [{G, User}], users => []}},
        {user_group, G, #{parent_groups => [F], nested_groups => [{H, User}], users => []}},
        {user_group, H, #{parent_groups => [G], nested_groups => [{I, User}], users => []}},
        {user_group, I, #{parent_groups => [H], nested_groups => [{J, User}], users => []}},
        {user_group, J, #{parent_groups => [I], nested_groups => [{K, User}], users => []}},
        {user_group, K, #{parent_groups => [J], nested_groups => [{L, User}], users => []}},
        {user_group, L, #{parent_groups => [K], nested_groups => [{M, User}], users => []}},
        {user_group, M, #{parent_groups => [L], nested_groups => [{N, User}], users => []}},
        {user_group, N, #{parent_groups => [M], nested_groups => [], users => []}}
    ],
    FirstBreakOps = [
        {user_group, E, #{parent_groups => [D], nested_groups => [], users => []}},
        {user_group, F, #{parent_groups => [], nested_groups => [{G, User}], users => []}},

        {user_group, K, #{parent_groups => [J], nested_groups => [], users => []}},
        {user_group, L, #{parent_groups => [], nested_groups => [{M, User}], users => []}}
    ],
    SecondBreakOps = [
        {user_group, B, #{parent_groups => [A], nested_groups => [], users => []}},
        {user_group, C, #{parent_groups => [], nested_groups => [{D, User}], users => []}},

        {user_group, G, #{parent_groups => [F], nested_groups => [], users => []}},
        {user_group, H, #{parent_groups => [], nested_groups => [{I, User}], users => []}}
    ],

    UIDsWithManagerPrivileges = lists:map(fun(UID) -> {UID, Manager} end, UIDs),
    UIDsWithViewPrivileges = lists:map(fun(UID) -> {UID, User} end, UIDs),

    JoinUsersOps = lists:foldl(fun(NN, All) ->
        All ++ [
            {user_group, B, #{users => lists:nthtail(NN, UIDsWithManagerPrivileges)}},
            {user_group, E, #{users => lists:nthtail(NN, UIDsWithManagerPrivileges)}},
            {user_group, G, #{users => lists:nthtail(NN, UIDsWithManagerPrivileges)}},
            {user_group, K, #{users => lists:nthtail(NN, UIDsWithManagerPrivileges)}},
            {user_group, N, #{users => lists:nthtail(NN, UIDsWithManagerPrivileges)}}
        ] ++ lists:map(fun(UID) -> {onedata_user, UID, #{groups => [B, E, G, K, N]}} end, lists:nthtail(NN, UIDs))

    end, [], lists:reverse(lists:seq(0, 13))),


    Data = [
        {mangle, FirstLinkOps ++ SecondLinkOps ++ FirstBreakOps ++ SecondBreakOps ++ JoinUsersOps},
        {refresh, lists:seq(1, 700)}
    ],

    Fun = fun
        ({mangle, Docs}) -> lists:foreach(fun({Type, ID, Diff}) ->
            update(Node, Type, ID, Diff),
            case Type of
                user_group -> mark_group_changed(Node, ID);
                onedata_user -> mark_user_changed(Node, ID)
            end
        end, Docs);
        ({refresh, Seq}) -> lists:foreach(fun(_S) -> refresh(Node) end, Seq)
    end,

    %% when
    utils:pforeach(Fun, Data),
    refresh(Node),

    %% then
    GroupsDocs = lists:map(fun(GID) -> get(Node, user_group, GID) end, GIDs),
    UserDocs = lists:map(fun(UID) -> get(Node, onedata_user, UID) end, UIDs),
    EffectiveGroupsOfGroups = lists:map(fun effective_groups/1, GroupsDocs),
    EffectiveGroupsOfUsers = lists:map(fun effective_groups/1, UserDocs),
    EffectiveUsersOfGroups = lists:map(fun effective_users/1, GroupsDocs),

    ?assertMatch([
        [<<"A">>],
        [<<"A">>, <<"B">>],
        [<<"C">>],
        [<<"C">>, <<"D">>],
        [<<"C">>, <<"D">>, <<"E">>],
        [<<"F">>],
        [<<"F">>, <<"G">>],
        [<<"H">>],
        [<<"H">>, <<"I">>],
        [<<"H">>, <<"I">>, <<"J">>],
        [<<"H">>, <<"I">>, <<"J">>, <<"K">>],
        [<<"L">>],
        [<<"L">>, <<"M">>],
        [<<"L">>, <<"M">>, <<"N">>]
    ], EffectiveGroupsOfGroups),

    Zipped = lists:zip([
        UIDsWithViewPrivileges,
        UIDsWithManagerPrivileges,
        UIDsWithViewPrivileges,
        UIDsWithViewPrivileges,
        UIDsWithManagerPrivileges,
        UIDsWithViewPrivileges,
        UIDsWithManagerPrivileges,
        UIDsWithViewPrivileges,
        UIDsWithViewPrivileges,
        UIDsWithViewPrivileges,
        UIDsWithManagerPrivileges,
        UIDsWithViewPrivileges,
        UIDsWithViewPrivileges,
        UIDsWithManagerPrivileges
    ], EffectiveUsersOfGroups),

    lists:foreach(fun({Expected, Actual}) -> ?assertUnorderedMatch(Expected, Actual) end, Zipped),
    lists:foreach(fun(EGoU) -> ?assertUnorderedMatch(GIDs, EGoU) end, EffectiveGroupsOfUsers),
    ok.

conditional_update_test(Config) ->
    [Node] = ?config(oz_worker_nodes, Config),

    G1 = #od_group{
        users = [{<<"U1">>, [group_change_data]}],
        children = [{<<"2">>, [group_change_data]}],
        parents = [],
        eff_users = [{<<"U1">>, [group_change_data]}],
        eff_children = [<<"1">>]},
    G2 = #od_group{
        users = [{<<"U2">>, [group_change_data]}],
        children = [],
        parents = [<<"1">>],
        eff_users = [{<<"U2">>, [group_change_data]}],
        eff_children = [<<"2">>]},

    save(Node, <<"1">>, G1),
    save(Node, <<"2">>, G2),
    mark_group_changed(Node, <<"1">>),
    mark_group_changed(Node, <<"2">>),
    refresh(Node),

    Doc1 = get(Node, user_group, <<"1">>),
    Doc2 = get(Node, user_group, <<"2">>),
    ?assertUnorderedMatch([<<"1">>], effective_groups(Doc1)),
    ?assertUnorderedMatch([<<"1">>, <<"2">>], effective_groups(Doc2)),
    ?assertUnorderedMatch([{<<"U1">>, [group_change_data]}, {<<"U2">>,
        [group_change_data]}], effective_users(Doc1)),
    ?assertUnorderedMatch([{<<"U2">>, [group_change_data]}], effective_users(Doc2)),

    %% when
    test_utils:mock_unload(Node),
    test_utils:mock_new(Node, user_group),
    mark_group_changed(Node, <<"1">>),
    mark_group_changed(Node, <<"2">>),
    refresh(Node),

    %% then
    test_utils:mock_assert_num_calls(Node, user_group, 'after',
        ['_', '_', '_', '_', {ok, '_'}], 0),
    ok.

cycles_elimination_test(Config) ->
    [Node] = ?config(oz_worker_nodes, Config),

    %% given
    Users = [{<<"u">>, []}],
    G1 = #od_group{users = Users, children = [{<<"2">>, [group_view_data]}], parents = []},
    G2 = #od_group{users = Users, children = [{<<"3">>, [group_view_data]}], parents = [<<"1">>]},
    G3 = #od_group{users = Users, children = [], parents = [<<"2">>]},

    save(Node, <<"1">>, G1),
    save(Node, <<"2">>, G2),
    save(Node, <<"3">>, G3),
    mark_group_changed(Node, <<"1">>),
    mark_group_changed(Node, <<"2">>),
    mark_group_changed(Node, <<"3">>),
    refresh(Node),

    %% when
    NewG1 = #od_group{users = Users, children = [{<<"2">>, [group_view_data]}], parents = [<<"3">>]},
    NewG3 = #od_group{users = Users, children = [{<<"1">>, [group_view_data]}], parents = [<<"2">>]},
    save(Node, <<"1">>, NewG1),
    save(Node, <<"3">>, NewG3),
    mark_group_changed(Node, <<"1">>),
    mark_group_changed(Node, <<"3">>),
    refresh(Node),

    %% then
    #document{value = #od_group{parents = P1}} = get(Node, user_group, <<"1">>),
    #document{value = #od_group{parents = P2}} = get(Node, user_group, <<"2">>),
    #document{value = #od_group{parents = P3}} = get(Node, user_group, <<"3">>),

    ?assertEqual(true, P1 =/= [<<"3">>] orelse P2 =/= [<<"1">>] orelse P3 =/= [<<"2">>]),
    ok.

user_becoming_groupless_test(Config) ->
    [Node] = ?config(oz_worker_nodes, Config),
    GID = <<"group@user_becoming_groupless_test">>,
    UID = <<"user@user_becoming_groupless_test">>,

    %% given
    save(Node, #document{key = GID, value = #od_group{users = [{UID, []}]}}),
    save(Node, #document{key = UID, value = #od_user{groups = [GID]}}),
    mark_group_changed(Node, GID),
    mark_user_changed(Node, UID),
    refresh(Node),
    ?assertMatch([GID], effective_groups(get(Node, onedata_user, UID))),

    %% when
    ?assertMatch(ok, rpc:call(Node, od_group, delete, [GID])),
    update(Node, onedata_user, UID, #{groups => []}),
    mark_user_changed(Node, UID),
    refresh(Node),

    %% then
    ?assertMatch([], effective_groups(get(Node, onedata_user, UID))).

grand_scenario_test(Config) ->
    [Node] = ?config(oz_worker_nodes, Config),
    [P1, P2, P3, P4, P5, P6, P7, P8, P9, P10] = [group_change_data,
        group_create_space, group_create_space_token, group_invite_user,
        group_join_space, group_leave_space, group_remove,
        group_remove_user, group_set_privileges, group_view_data],
    {ID1, ID2, ID3, ID4, ID5, ID6, ID7, ID8} = {<<"ID1">>, <<"ID2">>,
        <<"ID3">>, <<"ID4">>, <<"ID5">>, <<"ID6">>, <<"ID7">>, <<"ID8">>},

    %% Part A - single group
    %% given
    U1G1 = {<<"U1">>, [P1, P2]},
    G1 = #od_group{users = [U1G1], children = [], parents = []},
    save(Node, ID1, G1),
    save(Node, #document{key = <<"U1">>, value = #od_user{groups = [ID1]}}),

    %% given
    mark_group_changed(Node, ID1),
    refresh(Node),

    %% then
    Doc1A = get(Node, user_group, ID1),
    ?assertUnorderedMatch([ID1], effective_groups(Doc1A)),
    ?assertUnorderedMatch([U1G1], effective_users(Doc1A)),

    %% Part B - child attached
    %% given
    U1G2 = {<<"U1">>, [P1, P3, P10]},
    U2G2 = {<<"U2">>, [P1, P2]},
    G2 = #od_group{users = [U1G2, U2G2], children = [], parents = [ID1]},
    save(Node, ID2, G2),
    update(Node, user_group, ID1, #{nested_groups => [{ID2, [P2, P3, P4, P6]}]}),
    save(Node, #document{key = <<"U1">>, value = #od_user{groups = [ID1, ID2]}}),
    save(Node, #document{key = <<"U2">>, value = #od_user{groups = [ID1]}}),

    %% when
    mark_group_changed(Node, ID1),
    mark_group_changed(Node, ID2),
    refresh(Node),

    %% then
    Doc1B = get(Node, user_group, ID1),
    Doc2B = get(Node, user_group, ID2),
    ?assertUnorderedMatch([ID1], effective_groups(Doc1B)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]},
        {<<"U2">>, [P2]}], effective_users(Doc1B)),
    ?assertUnorderedMatch([ID1, ID2], effective_groups(Doc2B)),
    ?assertUnorderedMatch([U1G2, U2G2], effective_users(Doc2B)),

    %% Part C - grandchild attached
    %% given
    U2G3 = {<<"U2">>, [P1, P2, P3, P4, P6]},
    U3G3 = {<<"U3">>, [P1, P2, P5, P6]},
    G3 = #od_group{users = [U2G3, U3G3], children = [], parents = [ID2]},
    save(Node, ID3, G3),
    update(Node, user_group, ID2, #{nested_groups => [{ID3, [P3, P4, P5, P6]}]}),
    save(Node, #document{key = <<"U2">>, value = #od_user{groups = [ID1, ID3]}}),
    save(Node, #document{key = <<"U3">>, value = #od_user{groups = [ID3]}}),

    %% when
    mark_group_changed(Node, ID2),
    mark_group_changed(Node, ID3),
    refresh(Node),

    %% then
    Doc1C = get(Node, user_group, ID1),
    Doc2C = get(Node, user_group, ID2),
    Doc3C = get(Node, user_group, ID3),
    ?assertUnorderedMatch([ID1], effective_groups(Doc1C)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P4, P6]},
        {<<"U3">>, [P6]}], effective_users(Doc1C)),
    ?assertUnorderedMatch([ID1, ID2], effective_groups(Doc2C)),
    ?assertUnorderedMatch([U1G2, {<<"U2">>, [P1, P2, P3, P4, P6]},
        {<<"U3">>, [P5, P6]}], effective_users(Doc2C)),
    ?assertUnorderedMatch([ID1, ID2, ID3], effective_groups(Doc3C)),
    ?assertUnorderedMatch([U2G3, U3G3], effective_users(Doc3C)),

    %% Part D - parent attached
    %% given
    U1G4 = {<<"U1">>, [P3]},
    U4G4 = {<<"U4">>, [P4]},
    G4 = #od_group{users = [U1G4, U4G4], children = [
        {ID1, [P1, P2, P9]}], parents = []},
    save(Node, ID4, G4),
    update(Node, user_group, ID1, #{parent_groups => [ID4]}),
    save(Node, #document{key = <<"U4">>, value = #od_user{groups = [ID4]}}),

    %% when
    mark_group_changed(Node, ID1),
    mark_group_changed(Node, ID4),
    refresh(Node),

    %% then
    Doc1D = get(Node, user_group, ID1),
    Doc2D = get(Node, user_group, ID2),
    Doc3D = get(Node, user_group, ID3),
    Doc4D = get(Node, user_group, ID4),
    ?assertUnorderedMatch([ID1, ID4], effective_groups(Doc1D)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P4, P6]},
        {<<"U3">>, [P6]}], effective_users(Doc1D)),
    ?assertUnorderedMatch([ID1, ID2, ID4], effective_groups(Doc2D)),
    ?assertUnorderedMatch([U1G2, {<<"U2">>, [P1, P2, P3, P4, P6]},
        {<<"U3">>, [P5, P6]}], effective_users(Doc2D)),
    ?assertUnorderedMatch([ID1, ID2, ID3, ID4], effective_groups(Doc3D)),
    ?assertUnorderedMatch([U2G3, U3G3], effective_users(Doc3D)),
    ?assertUnorderedMatch([ID4], effective_groups(Doc4D)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2]},
        {<<"U3">>, []}, {<<"U4">>, [P4]}], effective_users(Doc4D)),


    %% Part E - sibling attached
    %% given
    U2G5 = {<<"U2">>, [P3]},
    U4G5 = {<<"U4">>, [P4, P5]},
    G5 = #od_group{users = [U2G5, U4G5], children = [],
        parents = [ID4]},
    save(Node, ID5, G5),
    update(Node, user_group, ID4, #{nested_groups => [
        {ID1, [P1, P2, P9]}, {ID5, [P2, P3, P4, P5, P6]}]}),

    %% when
    mark_group_changed(Node, ID4),
    mark_group_changed(Node, ID5),
    refresh(Node),

    %% then
    Doc1E = get(Node, user_group, ID1),
    Doc2E = get(Node, user_group, ID2),
    Doc3E = get(Node, user_group, ID3),
    Doc4E = get(Node, user_group, ID4),
    Doc5E = get(Node, user_group, ID5),
    ?assertUnorderedMatch([ID1, ID4], effective_groups(Doc1E)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P4, P6]},
        {<<"U3">>, [P6]}], effective_users(Doc1E)),
    ?assertUnorderedMatch([ID1, ID2, ID4], effective_groups(Doc2E)),
    ?assertUnorderedMatch([U1G2, {<<"U2">>, [P1, P2, P3, P4, P6]},
        {<<"U3">>, [P5, P6]}], effective_users(Doc2E)),
    ?assertUnorderedMatch([ID1, ID2, ID3, ID4], effective_groups(Doc3E)),
    ?assertUnorderedMatch([U2G3, U3G3], effective_users(Doc3E)),
    ?assertUnorderedMatch([ID4], effective_groups(Doc4E)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3]},
        {<<"U3">>, []}, {<<"U4">>, [P4, P5]}], effective_users(Doc4E)),
    ?assertUnorderedMatch([ID4, ID5], effective_groups(Doc5E)),
    ?assertUnorderedMatch([U2G5, U4G5], effective_users(Doc5E)),

    %% Part F - sibling-grandchild link
    %% given
    update(Node, user_group, ID5, #{nested_groups => [{ID3, [P1, P5, P6]}]}),
    update(Node, user_group, ID3, #{parent_groups => [ID2, ID5]}),

    %% when
    mark_group_changed(Node, ID3),
    mark_group_changed(Node, ID5),
    refresh(Node),

    %% then
    Doc1F = get(Node, user_group, ID1),
    Doc2F = get(Node, user_group, ID2),
    Doc3F = get(Node, user_group, ID3),
    Doc4F = get(Node, user_group, ID4),
    Doc5F = get(Node, user_group, ID5),
    ?assertUnorderedMatch([ID1, ID4], effective_groups(Doc1F)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P4, P6]},
        {<<"U3">>, [P6]}], effective_users(Doc1F)),
    ?assertUnorderedMatch([ID1, ID2, ID4], effective_groups(Doc2F)),
    ?assertUnorderedMatch([U1G2, {<<"U2">>, [P1, P2, P3, P4, P6]},
        {<<"U3">>, [P5, P6]}], effective_users(Doc2F)),
    ?assertUnorderedMatch([ID1, ID2, ID3, ID4, ID5], effective_groups(Doc3F)),
    ?assertUnorderedMatch([U2G3, U3G3], effective_users(Doc3F)),
    ?assertUnorderedMatch([ID4], effective_groups(Doc4F)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P6]},
        {<<"U3">>, [P5, P6]}, {<<"U4">>, [P4, P5]}], effective_users(Doc4F)),
    ?assertUnorderedMatch([ID4, ID5], effective_groups(Doc5F)),
    ?assertUnorderedMatch([{<<"U2">>, [P1, P3, P6]}, {<<"U3">>, [P1, P5, P6]},
        {<<"U4">>, [P4, P5]}], effective_users(Doc5F)),

    %% Part G - separate component
    %% given
    U1G6 = {<<"U1">>, [P1, P2, P7, P8, P9]},
    U2G7 = {<<"U2">>, [P2, P7, P9]},
    U3G8 = {<<"U3">>, [P3, P8]},
    G6 = #od_group{users = [U1G6], children = [
        {ID7, [P1, P2, P6, P7, P8]}], parents = []},
    G7 = #od_group{users = [U2G7], children = [
        {ID8, [P1, P2, P6, P8, P9]}], parents = [ID6]},
    G8 = #od_group{users = [U3G8], children = [],
        parents = [ID7]},
    save(Node, ID6, G6),
    save(Node, ID7, G7),
    save(Node, ID8, G8),
    save(Node, #document{key = <<"U1">>, value = #od_user{groups = [ID1, ID2, ID6]}}),
    save(Node, #document{key = <<"U2">>, value = #od_user{groups = [ID1, ID7]}}),
    save(Node, #document{key = <<"U3">>, value = #od_user{groups = [ID3, ID8]}}),

    %% given
    mark_group_changed(Node, ID6),
    mark_group_changed(Node, ID7),
    mark_group_changed(Node, ID8),
    refresh(Node),

    %% then - old component (unchanged)
    Doc1G = get(Node, user_group, ID1),
    Doc2G = get(Node, user_group, ID2),
    Doc3G = get(Node, user_group, ID3),
    Doc4G = get(Node, user_group, ID4),
    Doc5G = get(Node, user_group, ID5),
    ?assertUnorderedMatch([ID1, ID4], effective_groups(Doc1G)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P4, P6]},
        {<<"U3">>, [P6]}], effective_users(Doc1G)),
    ?assertUnorderedMatch([ID1, ID2, ID4], effective_groups(Doc2G)),
    ?assertUnorderedMatch([U1G2, {<<"U2">>, [P1, P2, P3, P4, P6]},
        {<<"U3">>, [P5, P6]}], effective_users(Doc2G)),
    ?assertUnorderedMatch([ID1, ID2, ID3, ID4, ID5], effective_groups(Doc3G)),
    ?assertUnorderedMatch([U2G3, U3G3], effective_users(Doc3G)),
    ?assertUnorderedMatch([ID4], effective_groups(Doc4G)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P6]},
        {<<"U3">>, [P5, P6]}, {<<"U4">>, [P4, P5]}], effective_users(Doc4G)),
    ?assertUnorderedMatch([ID4, ID5], effective_groups(Doc5G)),
    ?assertUnorderedMatch([{<<"U2">>, [P1, P3, P6]}, {<<"U3">>, [P1, P5, P6]},
        {<<"U4">>, [P4, P5]}], effective_users(Doc5G)),
    %% then - new component
    Doc6G = get(Node, user_group, ID6),
    Doc7G = get(Node, user_group, ID7),
    Doc8G = get(Node, user_group, ID8),
    ?assertUnorderedMatch([ID6], effective_groups(Doc6G)),
    ?assertUnorderedMatch([ID7, ID6], effective_groups(Doc7G)),
    ?assertUnorderedMatch([ID8, ID7, ID6], effective_groups(Doc8G)),
    ?assertUnorderedMatch([U1G6, {<<"U2">>, [P2, P7]},
        {<<"U3">>, [P8]}], effective_users(Doc6G)),
    ?assertUnorderedMatch([U2G7, {<<"U3">>, [P8]}], effective_users(Doc7G)),
    ?assertUnorderedMatch([U3G8], effective_users(Doc8G)),

    %% Part H - components linked
    %% given
    update(Node, user_group, ID1, #{nested_groups => [
        {ID2, [P2, P3, P4, P6]}, {ID7, [P7, P8, P9]}]}),
    update(Node, user_group, ID7, #{parent_groups => [ID6, ID1]}),

    %% when
    mark_group_changed(Node, ID7),
    mark_group_changed(Node, ID1),
    refresh(Node),

    %% then
    Doc1H = get(Node, user_group, ID1),
    Doc2H = get(Node, user_group, ID2),
    Doc3H = get(Node, user_group, ID3),
    Doc4H = get(Node, user_group, ID4),
    Doc5H = get(Node, user_group, ID5),
    Doc6H = get(Node, user_group, ID6),
    Doc7H = get(Node, user_group, ID7),
    Doc8H = get(Node, user_group, ID8),
    ?assertUnorderedMatch([ID1, ID4], effective_groups(Doc1H)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]},
        {<<"U2">>, [P2, P3, P4, P6, P7, P9]},
        {<<"U3">>, [P6, P8]}], effective_users(Doc1H)),
    ?assertUnorderedMatch([ID1, ID2, ID4], effective_groups(Doc2H)),
    ?assertUnorderedMatch([U1G2, {<<"U2">>, [P1, P2, P3, P4, P6]},
        {<<"U3">>, [P5, P6]}], effective_users(Doc2H)),
    ?assertUnorderedMatch([ID1, ID2, ID3, ID4, ID5], effective_groups(Doc3H)),
    ?assertUnorderedMatch([U2G3, U3G3], effective_users(Doc3H)),
    ?assertUnorderedMatch([ID4], effective_groups(Doc4H)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P6, P9]},
        {<<"U3">>, [P5, P6]}, {<<"U4">>, [P4, P5]}], effective_users(Doc4H)),
    ?assertUnorderedMatch([ID4, ID5], effective_groups(Doc5H)),
    ?assertUnorderedMatch([{<<"U2">>, [P1, P3, P6]}, {<<"U3">>, [P1, P5, P6]},
        {<<"U4">>, [P4, P5]}], effective_users(Doc5H)),
    ?assertUnorderedMatch([ID6], effective_groups(Doc6H)),
    ?assertUnorderedMatch([ID7, ID6, ID1, ID4], effective_groups(Doc7H)),
    ?assertUnorderedMatch([ID8, ID7, ID6, ID1, ID4], effective_groups(Doc8H)),
    ?assertUnorderedMatch([U1G6, {<<"U2">>, [P2, P7]},
        {<<"U3">>, [P8]}], effective_users(Doc6H)),
    ?assertUnorderedMatch([U2G7, {<<"U3">>, [P8]}], effective_users(Doc7H)),
    ?assertUnorderedMatch([U3G8], effective_users(Doc8H)),


    %% Part I - new components as links lost
    %% given
    update(Node, user_group, ID1, #{nested_groups => [{ID2, [P2, P3, P4, P6]}]}),
    update(Node, user_group, ID7, #{parent_groups => [ID6]}),

    %% when
    mark_group_changed(Node, ID7),
    mark_group_changed(Node, ID1),
    refresh(Node),

    %% then
    Doc1I = get(Node, user_group, ID1),
    Doc2I = get(Node, user_group, ID2),
    Doc3I = get(Node, user_group, ID3),
    Doc4I = get(Node, user_group, ID4),
    Doc5I = get(Node, user_group, ID5),
    Doc6I = get(Node, user_group, ID6),
    Doc7I = get(Node, user_group, ID7),
    Doc8I = get(Node, user_group, ID8),
    ?assertUnorderedMatch([ID1, ID4], effective_groups(Doc1I)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P4, P6]},
        {<<"U3">>, [P6]}], effective_users(Doc1I)),
    ?assertUnorderedMatch([ID1, ID2, ID4], effective_groups(Doc2I)),
    ?assertUnorderedMatch([U1G2, {<<"U2">>, [P1, P2, P3, P4, P6]},
        {<<"U3">>, [P5, P6]}], effective_users(Doc2I)),
    ?assertUnorderedMatch([ID1, ID2, ID3, ID4, ID5], effective_groups(Doc3I)),
    ?assertUnorderedMatch([U2G3, U3G3], effective_users(Doc3I)),
    ?assertUnorderedMatch([ID4], effective_groups(Doc4I)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P6]},
        {<<"U3">>, [P5, P6]}, {<<"U4">>, [P4, P5]}], effective_users(Doc4I)),
    ?assertUnorderedMatch([ID4, ID5], effective_groups(Doc5I)),
    ?assertUnorderedMatch([{<<"U2">>, [P1, P3, P6]}, {<<"U3">>, [P1, P5, P6]},
        {<<"U4">>, [P4, P5]}], effective_users(Doc5I)),
    ?assertUnorderedMatch([ID6], effective_groups(Doc6I)),
    ?assertUnorderedMatch([ID7, ID6], effective_groups(Doc7I)),
    ?assertUnorderedMatch([ID8, ID7, ID6], effective_groups(Doc8I)),
    ?assertUnorderedMatch([U1G6, {<<"U2">>, [P2, P7]},
        {<<"U3">>, [P8]}], effective_users(Doc6I)),
    ?assertUnorderedMatch([U2G7, {<<"U3">>, [P8]}], effective_users(Doc7I)),
    ?assertUnorderedMatch([U3G8], effective_users(Doc8I)),

    %% Part J - effective groups in users are coherent
    ?assertUnorderedMatch([ID1, ID2, ID4, ID6],
        effective_groups(get(Node, onedata_user, <<"U1">>))),
    ?assertUnorderedMatch([ID1, ID4, ID6, ID7],
        effective_groups(get(Node, onedata_user, <<"U2">>))),
    ?assertUnorderedMatch([ID1, ID2, ID3, ID4, ID5, ID6, ID7, ID8],
        effective_groups(get(Node, onedata_user, <<"U3">>))),
    ?assertUnorderedMatch([ID4],
        effective_groups(get(Node, onedata_user, <<"U4">>))),
    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).

init_per_testcase(_, _Config) ->
    [Node] = ?config(oz_worker_nodes, _Config),
    test_utils:mock_new(Node, user_group),
    test_utils:mock_expect(Node, user_group, 'after', fun(_, _, _, _, _) ->
        ok
    end),
    ok = rpc:call(Node, application, set_env, [?APP_Name,
        group_graph_refresh_interval, -1]),
    reset_state(Node),
    _Config.

end_per_testcase(_, _Config) ->
    [Node] = ?config(oz_worker_nodes, _Config),
    test_utils:mock_unload(Node),
    ok.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================

get(Node, Model, ID) ->
    Result = rpc:call(Node, Model, get, [ID]),
    ?assertMatch({ok, _}, Result),
    {ok, Doc} = Result,
    Doc.

effective_users(#document{value = #od_group{eff_users = Users}}) ->
    Users.

effective_groups(#document{value = #od_user{eff_groups = Groups}}) ->
    Groups;
effective_groups(#document{value = #od_group{eff_children = Groups}}) ->
    Groups.

update(Node, Type, ID, Diff) ->
    ?assertMatch({ok, ID}, rpc:call(Node, Type, update, [ID, Diff])).

save(Node, #document{key = K, value = V}) -> save(Node, K, V).
save(Node, ID, Value) ->
    ?assertMatch({ok, ID}, rpc:call(Node, element(1, Value), save,
        [#document{key = ID, value = Value}])).

mark_group_changed(Node, ID) ->
    ?assertMatch(ok, rpc:call(Node, group_graph, mark_group_changed, [ID])).

mark_user_changed(Node, ID) ->
    ?assertMatch(ok, rpc:call(Node, group_graph, mark_user_changed, [ID])).

reset_state(Node) ->
    save(Node, #document{key = <<"groups_graph_caches_state">>,
        value = #groups_graph_caches_state{last_rebuild = 0}
    }).

refresh(Node) ->
    ?assertMatch(ok, rpc:call(Node, group_graph, refresh_effective_caches, [])).
