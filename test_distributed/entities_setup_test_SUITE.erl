%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for automatic creation of predefined entities and some misc tests
%%% related to Onezone setup in general.
%%% @end
%%%-------------------------------------------------------------------
-module(entities_setup_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
    predefined_groups_test/1,
    global_groups_test/1,
    automatic_space_membership_via_global_group_test/1,
    automatic_first_space_test/1,
    default_onezone_plugins_pass_validation_test/1
]).

all() ->
    ?ALL([
        predefined_groups_test,
        global_groups_test,
        automatic_space_membership_via_global_group_test,
        automatic_first_space_test,
        default_onezone_plugins_pass_validation_test
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
    % Prepare config entry that will define what groups should be created, each
    % with different OZ API privileges.
    PredefinedGroups = [
        #{
            id => <<"group1">>,
            name => <<"Group 1">>,
            oz_privileges => {privileges, oz_privileges}
        },
        #{
            id => <<"group2">>,
            name => <<"Group 2">>,
            oz_privileges => [?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES]
        },
        #{
            id => <<"group3">>,
            name => <<"Group 3">>,
            oz_privileges => []
        }
    ],
    % Set the corresponding env variable on every node
    oz_test_utils:set_env(Config, predefined_groups, PredefinedGroups),
    % Call the group creation procedure. The function reads from env and
    % creates the predefined groups
    ?assertEqual(ok, oz_test_utils:call_oz(
        Config, group_logic, ensure_predefined_groups, []
    )),
    % Now, check if the groups are present in the system and have desired
    % privileges.
    CheckGroup = fun(ExpId, ExpName, ExpPrivileges) ->
        GroupResult = oz_test_utils:call_oz(Config, od_group, get, [ExpId]),
        % Check if group was found by ID
        ?assertMatch({ok, _}, GroupResult),
        % Check if the name is correct
        {ok, #document{value = #od_group{name = ActualName}}} = GroupResult,
        ?assertEqual(ExpName, ActualName),
        % Check if OZ API privileges are correct
        {ok, PrivsResult} = oz_test_utils:group_get_oz_privileges(Config, ExpId),
        % Check if the privileges are correct
        ?assertEqual(lists:sort(ExpPrivileges), lists:sort(PrivsResult)),
        % Check if group is marked as protected
        {ok, #document{value = #od_group{protected = Protected}}} = GroupResult,
        ?assertEqual(true, Protected)
    end,
    CheckGroup(<<"group1">>, <<"Group 1">>, privileges:oz_privileges()),
    CheckGroup(<<"group2">>, <<"Group 2">>, [?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES]),
    CheckGroup(<<"group3">>, <<"Group 3">>, []),

    % If a predefined group already exists, but name or privileges in config changes,
    % it should be updated
    NewPredefinedGroups = [
        #{
            id => <<"group1">>,
            name => <<"New group">>,
            oz_privileges => [?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_SPACES_SET_PRIVILEGES]
        },
        #{
            id => <<"group2">>,
            name => <<"My Unit">>,
            oz_privileges => {privileges, oz_viewer}
        },
        #{
            id => <<"group3">>,
            name => <<"My Organization">>,
            oz_privileges => {privileges, oz_admin}
        }
    ],
    oz_test_utils:set_env(Config, predefined_groups, NewPredefinedGroups),
    ?assertEqual(ok, oz_test_utils:call_oz(
        Config, group_logic, ensure_predefined_groups, []
    )),

    CheckGroup(<<"group1">>, <<"New group">>, [?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_SPACES_SET_PRIVILEGES]),
    CheckGroup(<<"group2">>, <<"My Unit">>, privileges:oz_viewer()),
    CheckGroup(<<"group3">>, <<"My Organization">>, privileges:oz_admin()),

    ok.


% This test checks if global groups mechanism works as expected. If enabled, it
% should automatically add every new user to given groups on creation, and
% properly set his privileges according to config.
global_groups_test(Config) ->
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
    oz_test_utils:set_env(Config, predefined_groups, PredefinedGroups),
    % Make sure predefined groups are created
    ?assertEqual(ok, oz_test_utils:call_oz(
        Config, group_logic, ensure_predefined_groups, []
    )),
    % Enable global groups
    oz_test_utils:set_env(Config, enable_global_groups, true),
    % Set automatic global groups
    GlobalGroups = [
        {<<"all_users_group">>, [?GROUP_VIEW]},
        {<<"access_to_public_data">>, []}
    ],
    oz_test_utils:set_env(Config, global_groups, GlobalGroups),
    % Now, creating a new user should cause him to automatically belong to
    % global groups with specified privileges.
    {ok, UserId} = oz_test_utils:create_user(
        Config, #{<<"fullName">> => <<"User with automatic groups">>}
    ),
    {ok, #od_user{
        groups = UserGroups
    }} = oz_test_utils:get_user(Config, UserId),
    ExpectedGroups = [<<"all_users_group">>, <<"access_to_public_data">>],
    % Check if user belongs to correct groups
    ?assertEqual(lists:usort(UserGroups), lists:usort(ExpectedGroups)),
    % Check if privileges are set correctly
    {ok, #od_group{
        users = AllUsersGroupPrivs
    }} = oz_test_utils:get_group(Config, <<"all_users_group">>),
    ?assertEqual(
        maps:get(UserId, AllUsersGroupPrivs), [?GROUP_VIEW]),
    {ok, #od_group{
        users = PublicAccessGroupPrivs
    }} = oz_test_utils:get_group(Config, <<"access_to_public_data">>),
    ?assertEqual(
        maps:get(UserId, PublicAccessGroupPrivs), []),
    % Make sure that disabling global groups has desired effects
    oz_test_utils:set_env(Config, enable_global_groups, false),
    {ok, UserNoGroupsId} = oz_test_utils:create_user(
        Config, #{<<"fullName">> => <<"User with NO automatic groups">>}
    ),
    {ok, #od_user{
        groups = ShouldBeEmptyList
    }} = oz_test_utils:get_user(Config, UserNoGroupsId),
    ?assertEqual(ShouldBeEmptyList, []),
    ok.


% This test checks if it is possible to arrange automatic space membership
% using the global groups mechanism.
automatic_space_membership_via_global_group_test(Config) ->
    % Set the predefined groups env
    PredefinedGroups = [
        #{
            id => <<"all_users_group">>,
            name => <<"All users">>,
            oz_privileges => []
        }
    ],
    oz_test_utils:set_env(Config, predefined_groups, PredefinedGroups),
    % Make sure predefined groups are created
    ?assertEqual(ok, oz_test_utils:call_oz(
        Config, group_logic, ensure_predefined_groups, []
    )),
    % Enable global groups
    oz_test_utils:set_env(Config, enable_global_groups, true),
    % Set automatic global groups
    GlobalGroups = [
        {<<"all_users_group">>, []}
    ],
    oz_test_utils:set_env(Config, global_groups, GlobalGroups),
    % Create a space and add the All Users group to it.
    % First, we need a dummy user for space creation.
    {ok, DummyUser} = oz_test_utils:create_user(
        Config, #{<<"fullName">> => <<"Dummy">>}
    ),
    {ok, OpenSpaceId} = oz_test_utils:create_space(
        Config, ?USER(DummyUser), <<"OpenSpace">>
    ),
    {ok, <<"all_users_group">>} = oz_test_utils:space_add_group(
        Config, OpenSpaceId, <<"all_users_group">>
    ),
    % Now, every created user should belong to the All Users group and thus
    % have access to the OpenSpace.
    {ok, UserId} = oz_test_utils:create_user(
        Config, #{<<"fullName">> => <<"User with automatic space membership">>}
    ),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, #od_user{
        eff_spaces = EffSpaces
    }} = oz_test_utils:get_user(Config, UserId),
    % To effectively belong to the space, user needs to have a space name
    % mapping and be resolvable as effective member.
    ?assert(maps:is_key(OpenSpaceId, EffSpaces)),
    ?assert(oz_test_utils:space_has_effective_user(
        Config, OpenSpaceId, UserId
    )),
    % Make sure that disabling global groups has desired effects
    oz_test_utils:set_env(Config, enable_global_groups, false),
    {ok, UserIdWithoutAccess} = oz_test_utils:create_user(
        Config, #{<<"fullName">> => <<"User with NO membership">>}
    ),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, #od_user{
        eff_spaces = ShouldNotContainTheOpenSpace
    }} = oz_test_utils:get_user(Config, UserIdWithoutAccess),
    ?assert(not maps:is_key(OpenSpaceId, ShouldNotContainTheOpenSpace)),
    ?assert(not oz_test_utils:space_has_effective_user(
        Config, OpenSpaceId, UserIdWithoutAccess
    )),
    % Make sure that removing the first user from global group will cause him
    % to lose access to OpenSpace.
    ok = oz_test_utils:group_remove_user(
        Config, <<"all_users_group">>, UserId
    ),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, #od_user{
        eff_spaces = ShouldNoLongerContainTheOpenSpace
    }} = oz_test_utils:get_user(Config, UserId),
    ?assert(not maps:is_key(OpenSpaceId, ShouldNoLongerContainTheOpenSpace)),
    ?assert(not oz_test_utils:space_has_effective_user(
        Config, OpenSpaceId, UserId
    )),
    ok.


% This test checks if automatic first space mechanism works as expected.
automatic_first_space_test(Config) ->
    oz_test_utils:set_env(Config, enable_automatic_first_space, false),
    {ok, UserId} = oz_test_utils:create_user(Config),
    ?assertMatch({ok, #od_user{spaces = []}}, oz_test_utils:get_user(Config, UserId)),

    % Enable automatic first space and check again
    oz_test_utils:set_env(Config, enable_automatic_first_space, true),
    {ok, UserId2} = oz_test_utils:create_user(Config),
    {ok, UserRecord2} = oz_test_utils:get_user(Config, UserId2),
    [SpaceId2] = ?assertMatch([_], UserRecord2#od_user.spaces),
    {ok, #od_space{users = Users}} = oz_test_utils:get_space(Config, SpaceId2),
    ?assert(maps:is_key(UserId2, Users)).


default_onezone_plugins_pass_validation_test(Config) ->
    ?assert(oz_test_utils:call_oz(Config, onezone_plugins, init, [])).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

end_per_suite(_) ->
    ok.