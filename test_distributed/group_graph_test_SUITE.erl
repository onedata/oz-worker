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


%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    effective_attrs_maintained_test
]).


effective_attrs_maintained_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    {ID1, ID2, ID3, ID4, ID5, ID6, ID7, ID8} = {<<"ID1">>, <<"ID2">>,
        <<"ID3">>, <<"ID4">>, <<"ID5">>, <<"ID6">>, <<"ID7">>, <<"ID8">>},

    %% separate group
    G1 = #user_group{users = [{<<"U">>, [p1, p2]}],
        child_groups = [], parent_groups = []},

    save(Node, ID1, G1),
    set_poi(Node, ID1),
    refresh(Node),

    Doc1 = get(Node, user_group, ID1),
    ?assertMatch([ID1], effective_groups(Doc1)),
    ?assertMatch([{<<"U">>, [p1, p2]}], effective_users(Doc1)),

    %% child attached
    G2 = #user_group{users = [{<<"U">>, [p1, p3]}, {<<"W">>, [p1, p2]}],
        child_groups = [], parent_groups = [G1]},
    save(Node, ID2, G2),
    save(Node, ID1, G1#user_group{child_groups = [{G2, [p2, p3]}]}),
    set_poi(Node, ID1),
    set_poi(Node, ID2),
    refresh(Node),
    Doc2 = get(Node, user_group, ID1),
    Doc3 = get(Node, user_group, ID2),
    ct:print("~p", [Doc2]),
    ct:print("~p", [Doc3]),

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
