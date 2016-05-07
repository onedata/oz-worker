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
-export([effective_attrs_maintained_test/1]).

-define(assertUnorderedMatch(Guard, Expr), (fun() ->
    Sorted = lists:sort(Guard),
    ?assertMatch(Sorted, lists:sort(Expr))
end)()).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    effective_attrs_maintained_test
]).


effective_attrs_maintained_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    [P1, P2, P3, P4, P5, P6, P7, P8, P9, P10] = [group_change_data,
        group_create_space, group_create_space_token, group_invite_user,
        group_join_space, group_leave_space, group_remove,
        group_remove_user, group_set_privileges, group_view_data],
    {ID1, ID2, ID3, ID4, ID5, ID6, ID7, ID8} = {<<"ID1">>, <<"ID2">>,
        <<"ID3">>, <<"ID4">>, <<"ID5">>, <<"ID6">>, <<"ID7">>, <<"ID8">>},

    %% separate group
    U1G1 = {<<"U1">>, [P1, P2]},
    G1 = #user_group{users = [U1G1],
        child_groups = [], parent_groups = [],
        effective_users = [U1G1],
        effective_groups = [ID1]},

    save(Node, ID1, G1),
    set_poi(Node, ID1),
    refresh(Node),
    Doc1A = get(Node, user_group, ID1),
    ct:print("~p", [Doc1A]),
    ?assertUnorderedMatch([ID1], effective_groups(Doc1A)),
    ?assertUnorderedMatch([U1G1], effective_users(Doc1A)),

    %% child attached
    U1G2 = {<<"U1">>, [P1, P3, P4]},
    U2G2 = {<<"U2">>, [P1, P2]},
    G2 = #user_group{users = [U1G2, U2G2],
        child_groups = [], parent_groups = [ID1],
        effective_users = [U1G2, U2G2],
        effective_groups = [ID2]},
    save(Node, ID2, G2),
    save(Node, ID1, G1#user_group{child_groups = [{ID2, [P2, P3]}]}),

    set_poi(Node, ID1),
    set_poi(Node, ID2),
    refresh(Node),

    Doc1B = get(Node, user_group, ID1),
    Doc2B = get(Node, user_group, ID2),

    ct:print("~p", [Doc1B]),
    ct:print("~p", [Doc2B]),

    ?assertUnorderedMatch([ID1], effective_groups(Doc1B)),
    ?assertUnorderedMatch([{<<"U1">>, [P1, P2, P3]}, {<<"U2">>, [P2]}], effective_users(Doc1B)),

    ?assertUnorderedMatch([ID1, ID2], effective_groups(Doc2B)),
    ?assertUnorderedMatch([U1G2, U2G2], effective_users(Doc2B)),

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
