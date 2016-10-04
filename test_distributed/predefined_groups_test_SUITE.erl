%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains tests for automatic creation of predefined groups.
%%% @end
%%%-------------------------------------------------------------------
-module(predefined_groups_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([predefined_groups_test/1, global_groups_test/1]).
-export([automatic_space_membership_via_global_group_test/1]).

all() ->
    ?ALL([
        predefined_groups_test,
        global_groups_test,
        automatic_space_membership_via_global_group_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

% This test checks if the procedure creating predefined groups works as
% expected. It is normally called from node_manager_plugin:after_init callback -
% so will already have happened during environment initialization, before
% this test starts. This means that to test it we need to call the procedure
% once again after the environment has started.
predefined_groups_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    % Prepare config entry that will define what groups should be created, each
    % with different OZ API privileges.
    PredefinedGroups = [
        #{
            id => <<"group1">>,
            name => <<"Group 1">>,
            oz_api_privileges => {oz_api_privileges, all_privileges}
        },
        #{
            id => <<"group2">>,
            name => <<"Group 2">>,
            oz_api_privileges => [view_privileges, set_privileges]
        },
        #{
            id => <<"group3">>,
            name => <<"Group 3">>,
            oz_api_privileges => []
        }
    ],
    % Set the corresponding env variable on every node
    test_utils:set_env(Node, oz_worker, predefined_groups, PredefinedGroups),
    % Call the group creation procedure. The function reads from env and
    % creates the predefined groups
    ?assertEqual(ok, rpc:call(Node, group_logic, create_predefined_groups, [])),
    % Now, lets check if the groups are present in the system and have desired
    % privileges.
    CheckGroup = fun(ExpId, ExpName, ExpPrivileges) ->
        GroupResult = rpc:call(Node, od_group, get, [ExpId]),
        % Check if group was found by ID
        ?assertMatch({ok, _}, GroupResult),
        % Check if the name is correct
        {ok, #document{value = #od_group{name = ActualName}}} = GroupResult,
        ?assertEqual(ExpName, ActualName),
        % Check if OZ API privileges are correct
        PrivsResult = rpc:call(Node, oz_api_privileges_logic, get, [
            ExpId, od_group
        ]),
        % Check if privileges were found by group ID
        ?assertMatch({ok, _}, PrivsResult),
        % Check if the privileges are correct
        {ok, ActualPrivileges} = PrivsResult,
        ?assertEqual(ExpPrivileges, ActualPrivileges)
    end,
    AllPrivs = rpc:call(Node, oz_api_privileges, all_privileges, []),
    CheckGroup(<<"group1">>, <<"Group 1">>, AllPrivs),
    CheckGroup(<<"group2">>, <<"Group 2">>, [view_privileges, set_privileges]),
    CheckGroup(<<"group3">>, <<"Group 3">>, []),
    ok.


% This test checks if global groups mechanism works as expected. If enabled, it
% should automatically add every new user to given groups on creation, and
% properly set his privileges according to config.
global_groups_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    % Set the predefined groups env
    PredefinedGroups = [
        #{
            id => <<"admins_group">>,
            name => <<"Admins group">>,
            oz_api_privileges => {oz_api_privileges, all_privileges}
        },
        #{
            id => <<"all_users_group">>,
            name => <<"All users">>,
            oz_api_privileges => []
        },
        #{
            id => <<"access_to_public_data">>,
            name => <<"Access to public data">>,
            oz_api_privileges => []
        }
    ],
    test_utils:set_env(Node, oz_worker, predefined_groups, PredefinedGroups),
    % Make sure predefined groups are created
    ?assertEqual(ok, rpc:call(Node, group_logic, create_predefined_groups, [])),
    % Enable global groups
    test_utils:set_env(Node, oz_worker, enable_global_groups, true),
    % Set automatic global groups
    GlobalGroups = [
        {<<"all_users_group">>, [group_view_data]},
        {<<"access_to_public_data">>, []}
    ],
    test_utils:set_env(Node, oz_worker, global_groups, GlobalGroups),
    % Now, creating a new user should cause him to automatically belong to
    % global groups with specified privileges.
    {ok, UserId} = oz_test_utils:create_user(
        Config, #od_user{name = <<"User with automatic groups">>}
    ),
    {ok, #document{value = #od_user{
        groups = UserGroups
    }}} = oz_test_utils:get_user(Config, UserId),
    ExpectedGroups = [<<"all_users_group">>, <<"access_to_public_data">>],
    % Check if user belongs to correct groups
    ?assertEqual(lists:usort(UserGroups), lists:usort(ExpectedGroups)),
    % Check if privileges are set correctly
    {ok, #document{value = #od_group{
        users = AllUsersGroupPrivs
    }}} = oz_test_utils:get_group(Config, <<"all_users_group">>),
    ?assertEqual(
        proplists:get_value(UserId, AllUsersGroupPrivs), [group_view_data]),
    {ok, #document{value = #od_group{
        users = PublicAccessGroupPrivs
    }}} = oz_test_utils:get_group(Config, <<"access_to_public_data">>),
    ?assertEqual(
        proplists:get_value(UserId, PublicAccessGroupPrivs), []),
    % Make sure that disabling global groups has desired effects
    test_utils:set_env(Node, oz_worker, enable_global_groups, false),
    {ok, UserNoGroupsId} = oz_test_utils:create_user(
        Config, #od_user{name = <<"User with NO automatic groups">>}
    ),
    {ok, #document{value = #od_user{
        groups = ShouldBeEmptyList
    }}} = oz_test_utils:get_user(Config, UserNoGroupsId),
    ?assertEqual(ShouldBeEmptyList, []),
    ok.


% This test checks if it is possible to arrange automatic space membership
% using the global groups mechanism.
automatic_space_membership_via_global_group_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    % Set the predefined groups env
    PredefinedGroups = [
        #{
            id => <<"all_users_group">>,
            name => <<"All users">>,
            oz_api_privileges => []
        }
    ],
    test_utils:set_env(Node, oz_worker, predefined_groups, PredefinedGroups),
    % Make sure predefined groups are created
    ?assertEqual(ok, rpc:call(Node, group_logic, create_predefined_groups, [])),
    % Enable global groups
    test_utils:set_env(Node, oz_worker, enable_global_groups, true),
    % Set automatic global groups
    GlobalGroups = [
        {<<"all_users_group">>, []}
    ],
    test_utils:set_env(Node, oz_worker, global_groups, GlobalGroups),
    % Create a space and add the All Users group to it.
    % First, we need a dummy user for space creation.
    {ok, DummyUser} = oz_test_utils:create_user(
        Config, #od_user{name = <<"Dummy">>}
    ),
    {ok, OpenSpaceId} = oz_test_utils:create_space(
        Config, {user, DummyUser}, <<"OpenSpace">>
    ),
    {ok, OpenSpaceId} = oz_test_utils:add_member_to_space(
        Config, {group, <<"all_users_group">>}, OpenSpaceId
    ),
    % Now, every created user should belong to the All Users group and thus
    % have access to the OpenSpace.
    {ok, UserId} = oz_test_utils:create_user(
        Config, #od_user{name = <<"User with automatic space membership">>}
    ),
    {ok, #document{value = #od_user{
        space_aliases = SpaceNames
    }}} = oz_test_utils:get_user(Config, UserId),
    % To effectively belong to the space, user needs to have a space name
    % mapping and be resolvable as effective member.
    ?assert(maps:is_key(OpenSpaceId, SpaceNames)),
    ?assert(oz_test_utils:space_has_effective_user(
        Config, OpenSpaceId, UserId
    )),
    % Make sure that disabling global groups has desired effects
    test_utils:set_env(Node, oz_worker, enable_global_groups, false),
    {ok, UserIdWithoutAccess} = oz_test_utils:create_user(
        Config, #od_user{name = <<"User with NO membership">>}
    ),
    {ok, #document{value = #od_user{
        space_aliases = ShouldNotContainTheOpenSpace
    }}} = oz_test_utils:get_user(Config, UserIdWithoutAccess),
    ?assert(not maps:is_key(OpenSpaceId, ShouldNotContainTheOpenSpace)),
    ?assert(not oz_test_utils:space_has_effective_user(
        Config, OpenSpaceId, UserIdWithoutAccess
    )),
    % Make sure that removing the first user from global group will cause him
    % to lose access to OpenSpace.
    true = oz_test_utils:group_remove_user(
        Config, <<"all_users_group">>, UserId
    ),
    {ok, #document{value = #od_user{
        space_aliases = ShouldNoLongerContainTheOpenSpace
    }}} = oz_test_utils:get_user(Config, UserId),
    ?assert(not maps:is_key(OpenSpaceId, ShouldNoLongerContainTheOpenSpace)),
    ?assert(not oz_test_utils:space_has_effective_user(
        Config, OpenSpaceId, UserId
    )),
    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(
        Config, ?TEST_FILE(Config, "env_desc.json"), [oz_test_utils]
    ),
    NewConfig.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).