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
-module(logic_modules_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([provider_logic_test/1]).

all() ->
    ?ALL([
        provider_logic_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

provider_logic_test(Config) ->

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
            oz_privileges => {privileges, oz_privileges}
        },
        #{
            id => <<"all_users_group">>,
            name => <<"All users">>,
            oz_privileges => []
        },
        #{
            id => <<"access_to_public_data">>,
            name => <<"Access to public data">>,
            oz_privileges => []
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
            oz_privileges => []
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