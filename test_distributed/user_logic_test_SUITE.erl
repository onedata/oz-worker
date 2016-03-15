%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains tests for user logic module.
%%% @end
%%%-------------------------------------------------------------------
-module(user_logic_test_SUITE).
-author("Krzysztof Trzepla").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([set_space_name_mapping_test/1, clean_space_name_mapping_test/1]).

all() ->
    ?ALL([
        set_space_name_mapping_test,
        clean_space_name_mapping_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

set_space_name_mapping_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),

    {ok, UserId} = ?assertMatch({ok, _}, oz_test_utils:create_user(Config, #onedata_user{})),

    SpaceId1 = <<"1111111111">>,
    SpaceName1 = <<"space_name">>,
    set_space_name_mapping(Node, UserId, SpaceId1, SpaceName1),
    ?assertEqual({ok, SpaceName1}, get_space_name_mapping(Node, UserId, SpaceId1)),

    SpaceId2 = <<"2222222222">>,
    SpaceName2 = <<"different_space_name">>,
    set_space_name_mapping(Node, UserId, SpaceId2, SpaceName2),
    ?assertEqual({ok, SpaceName2}, get_space_name_mapping(Node, UserId, SpaceId2)),

    SpaceId3 = <<"3333333333">>,
    SpaceName3 = <<"space_name">>,
    set_space_name_mapping(Node, UserId, SpaceId3, SpaceName3),
    ?assertEqual({ok, <<SpaceName3/binary, "#", SpaceId3:6/binary>>},
        get_space_name_mapping(Node, UserId, SpaceId3)),

    SpaceId4 = <<"3333333334">>,
    SpaceName4 = <<"space_name">>,
    set_space_name_mapping(Node, UserId, SpaceId4, SpaceName4),
    ?assertEqual({ok, <<SpaceName4/binary, "#", SpaceId4:7/binary>>},
        get_space_name_mapping(Node, UserId, SpaceId4)).

clean_space_name_mapping_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),

    SpaceName = <<"space_name">>,
    {ok, UserId} = ?assertMatch({ok, _}, oz_test_utils:create_user(Config, #onedata_user{})),
    {ok, GroupId} = ?assertMatch({ok, _}, oz_test_utils:create_group(Config, UserId, <<"group">>)),
    {ok, SpaceId} = ?assertMatch({ok, _}, oz_test_utils:create_space(Config, {group, GroupId}, SpaceName)),
    ?assertEqual(ok, oz_test_utils:join_space(Config, {user, UserId}, SpaceId)),

    ?assertNot(clean_space_name_mapping(Node, UserId, SpaceId)),
    ?assertEqual({ok, SpaceName}, get_space_name_mapping(Node, UserId, SpaceId)),

    ?assert(oz_test_utils:leave_space(Config, {group, GroupId}, SpaceId)),
    ?assertNot(clean_space_name_mapping(Node, UserId, SpaceId)),
    ?assertEqual({ok, SpaceName}, get_space_name_mapping(Node, UserId, SpaceId)),

    ?assert(oz_test_utils:leave_space(Config, {user, UserId}, SpaceId)),
    ?assert(clean_space_name_mapping(Node, UserId, SpaceId)),
    ?assertMatch({ok, #document{value = #onedata_user{space_names = #{}}}},
        rpc:call(Node, onedata_user, get, [UserId])).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json"), [oz_test_utils]),
    timer:sleep(10000), % TODO add nagios to GR and delete sleep
    NewConfig.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================

set_space_name_mapping(Node, UserId, SpaceId, SpaceName) ->
    ?assertEqual(ok, rpc:call(Node, user_logic, set_space_name_mapping,
        [UserId, SpaceId, SpaceName])).

get_space_name_mapping(Node, UserId, SpaceId) ->
    {ok, Data} = ?assertMatch({ok, _},
        rpc:call(Node, space_logic, get_data, [SpaceId, {user, UserId}])),
    Name = ?assertMatch(<<_/binary>>, proplists:get_value(name, Data)),
    {ok, Name}.

clean_space_name_mapping(Node, UserId, SpaceId) ->
    rpc:call(Node, user_logic, clean_space_name_mapping, [UserId, SpaceId]).