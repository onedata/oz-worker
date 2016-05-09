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
-module(group_graph_test_SUITE).
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
-export([grand_scenario_test/1, conditional_update_test/1]).

-define(assertUnorderedMatch(Guard, Expr), (fun() ->
    Sorted = lists:sort(Guard),
    ?assertMatch(Sorted, lists:sort(Expr))
end)()).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    grand_scenario_test,
    conditional_update_test
]).

conditional_update_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),

    G1 = #user_group{
        users = [{<<"U1">>, [group_change_data]}],
        child_groups = [{<<"2">>, [group_change_data]}],
        parent_groups = [],
        effective_users = [{<<"U1">>, [group_change_data]}],
        effective_groups = [<<"1">>]},
    G2 = #user_group{
        users = [{<<"U2">>, [group_change_data]}],
        child_groups = [],
        parent_groups = [<<"1">>],
        effective_users = [{<<"U2">>, [group_change_data]}],
        effective_groups = [<<"2">>]},

    save(Node, <<"1">>, G1),
    save(Node, <<"2">>, G2),
    set_poi(Node, <<"1">>),
    set_poi(Node, <<"2">>),
    refresh(Node),

    Doc1 = get(Node, user_group, <<"1">>),
    Doc2 = get(Node, user_group, <<"2">>),
    ?assertUnorderedMatch([<<"1">>], effective_groups(Doc1)),
    ?assertUnorderedMatch([<<"1">>, <<"2">>], effective_groups(Doc2)),
    ?assertUnorderedMatch([{<<"U1">>, [group_change_data]}, {<<"U2">>, [group_change_data]}], effective_users(Doc1)),
    ?assertUnorderedMatch([{<<"U2">>, [group_change_data]}], effective_users(Doc2)),

    %% when
    meck:new(user_group, [passthrough]),
    set_poi(Node, <<"1">>),
    set_poi(Node, <<"2">>),
    refresh(Node),

    %% then
    ?assertEqual(0, meck:num_calls(user_group, update, '_')),
    ?assertEqual(0, meck:num_calls(user_group, save, '_')),
    meck:unload(user_group),
    ok.

grand_scenario_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    [P1, P2, P3, P4, P5, P6, P7, P8, P9, P10] = [group_change_data,
        group_create_space, group_create_space_token, group_invite_user,
        group_join_space, group_leave_space, group_remove,
        group_remove_user, group_set_privileges, group_view_data],
    {ID1, ID2, ID3, ID4, ID5, ID6, ID7, ID8} = {<<"ID1">>, <<"ID2">>,
        <<"ID3">>, <<"ID4">>, <<"ID5">>, <<"ID6">>, <<"ID7">>, <<"ID8">>},

    %% Part A - single group
    %% given
    U1G1 = {<<"U1">>, [P1, P2]},
    G1 = #user_group{users = [U1G1],
        child_groups = [], parent_groups = [],
        effective_users = [U1G1],
        effective_groups = [ID1]},
    save(Node, ID1, G1),

    %% given
    set_poi(Node, ID1),
    refresh(Node),

    %% then
    Doc1A = get(Node, user_group, ID1),
    ?assertUnorderedMatch([ID1], effective_groups(Doc1A)),
    ?assertUnorderedMatch([U1G1], effective_users(Doc1A)),

    %% Part B - child attached
    %% given
    U1G2 = {<<"U1">>, [P1, P3, P10]},
    U2G2 = {<<"U2">>, [P1, P2]},
    G2 = #user_group{users = [U1G2, U2G2],
        child_groups = [], parent_groups = [ID1],
        effective_users = [U1G2, U2G2],
        effective_groups = [ID2]},
    save(Node, ID2, G2),
    update(Node, user_group, ID1, #{child_groups => [{ID2, [P2, P3, P4, P6]}]}),

    %% when
    set_poi(Node, ID1),
    set_poi(Node, ID2),
    refresh(Node),

    %% then
    Doc1B = get(Node, user_group, ID1),
    Doc2B = get(Node, user_group, ID2),
    ?assertUnorderedMatch([ID1], effective_groups(Doc1B)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2]}], effective_users(Doc1B)),
    ?assertUnorderedMatch([ID1, ID2], effective_groups(Doc2B)),
    ?assertUnorderedMatch([U1G2, U2G2], effective_users(Doc2B)),

    %% Part C - grandchild attached
    %% given
    U2G3 = {<<"U2">>, [P1, P2, P3, P4, P6]},
    U3G3 = {<<"U3">>, [P1, P2, P5, P6]},
    G3 = #user_group{users = [U2G3, U3G3],
        child_groups = [], parent_groups = [ID2],
        effective_users = [U2G3, U3G3],
        effective_groups = [ID3]},
    save(Node, ID3, G3),
    update(Node, user_group, ID2, #{child_groups => [{ID3, [P3, P4, P5, P6]}]}),

    %% when
    set_poi(Node, ID2),
    set_poi(Node, ID3),
    refresh(Node),

    %% then
    Doc1C = get(Node, user_group, ID1),
    Doc2C = get(Node, user_group, ID2),
    Doc3C = get(Node, user_group, ID3),
    ?assertUnorderedMatch([ID1], effective_groups(Doc1C)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P4, P6]}, {<<"U3">>, [P6]}], effective_users(Doc1C)),
    ?assertUnorderedMatch([ID1, ID2], effective_groups(Doc2C)),
    ?assertUnorderedMatch([U1G2, {<<"U2">>, [P1, P2, P3, P4, P6]}, {<<"U3">>, [P5, P6]}], effective_users(Doc2C)),
    ?assertUnorderedMatch([ID1, ID2, ID3], effective_groups(Doc3C)),
    ?assertUnorderedMatch([U2G3, U3G3], effective_users(Doc3C)),

    %% Part D - parent attached
    %% given
    U1G4 = {<<"U1">>, [P3]},
    U4G4 = {<<"U4">>, [P4]},
    G4 = #user_group{users = [U1G4, U4G4],
        child_groups = [{ID1, [P1, P2, P9]}], parent_groups = [],
        effective_users = [U4G4, U1G4],
        effective_groups = [ID4]},
    save(Node, ID4, G4),
    update(Node, user_group, ID1, #{parent_groups => [ID4]}),

    %% when
    set_poi(Node, ID1),
    set_poi(Node, ID4),
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
    G5 = #user_group{users = [U2G5, U4G5],
        child_groups = [], parent_groups = [ID4],
        effective_users = [U2G5, U4G5],
        effective_groups = [ID5]},
    save(Node, ID5, G5),
    update(Node, user_group, ID4, #{child_groups => [{ID1, [P1, P2, P9]}, {ID5, [P2, P3, P4, P5, P6]}]}),

    %% when
    set_poi(Node, ID4),
    set_poi(Node, ID5),
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
    update(Node, user_group, ID5, #{child_groups => [{ID3, [P1, P5, P6]}]}),
    update(Node, user_group, ID3, #{parent_groups => [ID2, ID5]}),

    %% when
    set_poi(Node, ID3),
    set_poi(Node, ID5),
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
    G6 = #user_group{users = [U1G6],
        child_groups = [{ID7, [P1, P2, P6, P7, P8]}], parent_groups = [],
        effective_users = [U1G6],
        effective_groups = [ID6]},
    G7 = #user_group{users = [U2G7],
        child_groups = [{ID8, [P1, P2, P6, P8, P9]}], parent_groups = [ID6],
        effective_users = [U2G7],
        effective_groups = [ID7]},
    G8 = #user_group{users = [U3G8],
        child_groups = [], parent_groups = [ID7],
        effective_users = [U3G8],
        effective_groups = [ID8]},
    save(Node, ID6, G6),
    save(Node, ID7, G7),
    save(Node, ID8, G8),

    %% given
    set_poi(Node, ID6),
    set_poi(Node, ID7),
    set_poi(Node, ID8),
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
    ?assertUnorderedMatch([U1G6, {<<"U2">>, [P2, P7]}, {<<"U3">>, [P8]}], effective_users(Doc6G)),
    ?assertUnorderedMatch([U2G7, {<<"U3">>, [P8]}], effective_users(Doc7G)),
    ?assertUnorderedMatch([U3G8], effective_users(Doc8G)),

    %% Part H - components linked
    %% given
    update(Node, user_group, ID1, #{child_groups => [{ID2, [P2, P3, P4, P6]}, {ID7, [P7, P8, P9]}]}),
    update(Node, user_group, ID7, #{parent_groups => [ID6, ID1]}),

    %% when
    set_poi(Node, ID7),
    set_poi(Node, ID1),
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
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2, P3, P4, P6, P7, P9]},
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
    ?assertUnorderedMatch([U1G6, {<<"U2">>, [P2, P7]}, {<<"U3">>, [P8]}], effective_users(Doc6H)),
    ?assertUnorderedMatch([U2G7, {<<"U3">>, [P8]}], effective_users(Doc7H)),
    ?assertUnorderedMatch([U3G8], effective_users(Doc8H)),


    %% Part I - new components as links lost
    %% given
    update(Node, user_group, ID1, #{child_groups => [{ID2, [P2, P3, P4, P6]}]}),
    update(Node, user_group, ID7, #{parent_groups => [ID6]}),

    %% when
    set_poi(Node, ID7),
    set_poi(Node, ID1),
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
    ?assertUnorderedMatch([U1G6, {<<"U2">>, [P2, P7]}, {<<"U3">>, [P8]}], effective_users(Doc6I)),
    ?assertUnorderedMatch([U2G7, {<<"U3">>, [P8]}], effective_users(Doc7I)),
    ?assertUnorderedMatch([U3G8], effective_users(Doc8I)),

    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).

init_per_testcase(_, _Config) ->
    [Node | _] = ?config(oz_worker_nodes, _Config),
    ok = rpc:call(Node, application, set_env, [?APP_Name, group_graph_refresh_interval, -1]),
    reset_state(Node),
    _Config.

end_per_testcase(_, _Config) ->
    ok.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================

get(Node, Model, ID) ->
    Result = rpc:call(Node, Model, get, [ID]),
    ?assertMatch({ok, _}, Result),
    {ok, Doc} = Result, Doc.


effective_users(#document{value = #user_group{effective_users = Users}}) ->
    Users.

effective_groups(#document{value = #user_group{effective_groups = Groups}}) ->
    Groups.

update(Node, Type, ID, Diff) ->
    ?assertMatch({ok, ID}, rpc:call(Node, Type, update, [ID, Diff])).

save(Node, #document{key = K, value = V}) -> save(Node, K, V).
save(Node, ID, Value) ->
    ?assertMatch({ok, ID}, rpc:call(Node, element(1, Value), save,
        [#document{key = ID, value = Value}])).

set_poi(Node, ID) ->
    ?assertMatch(ok, rpc:call(Node, group_graph, set_poi, [ID])).

reset_state(Node) ->
    save(Node, #document{key = <<"group_graph_context">>,
        value = #group_graph_context{last_rebuild = 0}
    }).

refresh(Node) ->
    ?assertMatch(ok, rpc:call(Node, group_graph, refresh, [])).
