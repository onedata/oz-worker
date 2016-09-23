%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains tests for user logic module concerning:
%%%   - personal space name mapping for users
%%%   - logging in via basic auth by interacting with onepanel
%%%   - automatic adding of users to predefined groups based on onepanel role
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
-export([all/0, init_per_suite/1, end_per_suite/1, end_per_testcase/2]).
-export([
    set_space_name_mapping_test/1,
    clean_space_name_mapping_test/1,
    remove_space_test/1,
    basic_auth_login_test/1,
    automatic_group_membership_test/1,
    change_password_test/1
]).

all() ->
    ?ALL([
        set_space_name_mapping_test,
        clean_space_name_mapping_test,
        remove_space_test,
        basic_auth_login_test,
        automatic_group_membership_test,
        change_password_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

set_space_name_mapping_test(Config) ->
    [Node | _] = Nodes = ?config(oz_worker_nodes, Config),

    {ok, UserId} = ?assertMatch(
        {ok, _}, oz_test_utils:create_user(Config, #onedata_user{})
    ),

    SpaceName1 = <<"space_name">>,
    {ok, SpaceId1} = ?assertMatch({ok, _},
        oz_test_utils:create_space(Config, {user, UserId}, SpaceName1)),
    ?assertEqual(SpaceName1, get_space_name_mapping(Node, UserId, SpaceId1)),

    SpaceName2 = <<"different_space_name">>,
    {ok, SpaceId2} = ?assertMatch({ok, _},
        oz_test_utils:create_space(Config, {user, UserId}, SpaceName2)),
    ?assertEqual(SpaceName2, get_space_name_mapping(Node, UserId, SpaceId2)),

    SpaceName3 = <<"space_name">>,
    {ok, SpaceId3} = ?assertMatch({ok, _},
        oz_test_utils:create_space(Config, {user, UserId}, SpaceName3)),
    ?assertEqual(<<SpaceName3/binary, "#", SpaceId3:6/binary>>,
        get_space_name_mapping(Node, UserId, SpaceId3)),

    SpaceName4 = <<"space_name">>,
    SpaceId4 = <<SpaceId3:6/binary, "$random">>,
    space_save_mock(Nodes, SpaceId4),
    {ok, SpaceId4} = ?assertMatch({ok, _},
        oz_test_utils:create_space(Config, {user, UserId}, SpaceName4)),
    ?assertEqual(<<SpaceName4/binary, "#", SpaceId4:7/binary>>,
        get_space_name_mapping(Node, UserId, SpaceId4)),

    SpaceName5 = <<"modified_space_name">>,
    ?assertEqual(ok, oz_test_utils:modify_space(
        Config, SpaceId4, {user, UserId}, SpaceName5)),
    ?assertEqual(SpaceName5, get_space_name_mapping(Node, UserId, SpaceId4)).

clean_space_name_mapping_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),

    SpaceName = <<"space_name">>,
    {ok, UserId} = ?assertMatch(
        {ok, _}, oz_test_utils:create_user(Config, #onedata_user{})
    ),
    {ok, GroupId} = ?assertMatch(
        {ok, _}, oz_test_utils:create_group(Config, UserId, <<"group">>)
    ),
    {ok, SpaceId} = ?assertMatch(
        {ok, _}, oz_test_utils:create_space(Config, {group, GroupId}, SpaceName)
    ),
    ?assertMatch({ok, _},
        oz_test_utils:add_member_to_space(Config, {user, UserId}, SpaceId)),

    ?assertNot(clean_space_name_mapping(Node, UserId, SpaceId)),
    ?assertEqual(SpaceName, get_space_name_mapping(Node, UserId, SpaceId)),

    ?assert(oz_test_utils:leave_space(Config, {group, GroupId}, SpaceId)),
    ?assertNot(clean_space_name_mapping(Node, UserId, SpaceId)),
    ?assertEqual(SpaceName, get_space_name_mapping(Node, UserId, SpaceId)),

    ?assert(oz_test_utils:leave_space(Config, {user, UserId}, SpaceId)),
    ?assert(clean_space_name_mapping(Node, UserId, SpaceId)),
    ?assertMatch({ok, #document{value = #onedata_user{space_names = #{}}}},
        rpc:call(Node, onedata_user, get, [UserId])).

remove_space_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),

    SpaceName = <<"space_name">>,
    {ok, UserId} = ?assertMatch(
        {ok, _}, oz_test_utils:create_user(Config, #onedata_user{})
    ),
    % Every user gets his first space upon creation, account it
    {ok, #document{
        value = #onedata_user{
            spaces = [FirstSpace],
            space_names = SpaceNames
        }}} = oz_test_utils:get_user(Config, UserId),
    FirstSpaceName = maps:get(FirstSpace, SpaceNames),
    {ok, SpaceId} = ?assertMatch(
        {ok, _}, oz_test_utils:create_space(Config, {user, UserId}, SpaceName)
    ),

    ?assertEqual(SpaceName, get_space_name_mapping(Node, UserId, SpaceId)),
    ?assertEqual(FirstSpaceName,
        get_space_name_mapping(Node, UserId, FirstSpace)
    ),
    oz_test_utils:remove_space(Config, SpaceId),
    oz_test_utils:remove_space(Config, FirstSpace),
    ?assertMatch({ok, #document{value = #onedata_user{
        spaces = [], space_names = #{}
    }}}, rpc:call(Node, onedata_user, get, [UserId])).

% Check if basic auth login endpoint works.
basic_auth_login_test(Config) ->
    % To resolve user details, OZ asks onepanel via a REST endpoint. In this
    % test, onepanel is mocked using appmock.
    [Node | _] = ?config(oz_worker_nodes, Config),
    OneZoneIP = test_utils:get_docker_ip(Node),
    % Set the env variable in OZ that points to onepanel URL to appmock's
    % mocked endpoint.
    ok = test_utils:set_env(
        Node, oz_worker, onepanel_rest_url, appmock_mocked_endpoint(Config)
    ),
    % Appmock's app description contains mocked user (user1:password1),
    % now just try to log in into OZ using basic auth endpoint and
    % check if it works correctly.
    BasicAuthEndpoint = str_utils:format_bin(
        "https://~s/do_login", [OneZoneIP]
    ),
    UserPasswordB64 = base64:encode(<<"user1:password1">>),
    BasicAuthHeaders = [
        {<<"authorization">>, <<"Basic ", UserPasswordB64/binary>>}
    ],
    Response = http_client:post(
        BasicAuthEndpoint, BasicAuthHeaders, [], [insecure]
    ),
    ?assertMatch({ok, 200, _, _}, Response),
    {ok, 200, RespHeaders, _} = Response,
    % Make sure response headers contain cookie with session id - which means
    % that the user has logged in.
    Cookie = proplists:get_value(<<"set-cookie">>, RespHeaders, <<"">>),
    ?assertMatch(<<"session_id=", _/binary>>, Cookie),
    % Try some inexistent user credentials if 401 is returned
    WrongUserPasswordB64 = base64:encode(<<"lol:wut">>),
    WrongBasicAuthHeaders = [
        {<<"authorization">>, <<"Basic ", WrongUserPasswordB64/binary>>}
    ],
    ?assertMatch({ok, 401, _, _}, http_client:post(
        BasicAuthEndpoint, WrongBasicAuthHeaders, [], [insecure]
    )),
    ok.

% Users that log in through basic auth should automatically be added to
% groups based on 'onepanel_role_to_group_mapping' env setting.
automatic_group_membership_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    OneZoneIP = test_utils:get_docker_ip(Node),
    % First make sure that groups for tests exist in the system. We can use
    % the predefined groups mechanism here.
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
        }
    ],
    % Set the corresponding env variable on one of the nodes
    ok = test_utils:set_env(
        Node, oz_worker, predefined_groups, PredefinedGroups
    ),
    % Call the group creation procedure
    ok = rpc:call(Node, group_logic, create_predefined_groups, []),
    % Now, prepare config entry for onepanel role to groups mapping. We want
    % everyone with role "user2Role" to belong to both groups 1 and 2.
    RoleToGroupMapping = #{
        <<"user2Role">> => [<<"group1">>, <<"group2">>]
    },
    % Set the corresponding env
    ok = test_utils:set_env(
        Node, oz_worker, onepanel_role_to_group_mapping, RoleToGroupMapping
    ),
    % Set the env variable in OZ that points to onepanel URL to appmock's
    % mocked endpoint.
    ok = test_utils:set_env(
        Node, oz_worker, onepanel_rest_url, appmock_mocked_endpoint(Config)
    ),
    % Try to log in using credentials user2:password2 (user with id user2Id)
    % and see if he was added to both groups.
    BasicAuthEndpoint = str_utils:format_bin(
        "https://~s/do_login", [OneZoneIP]
    ),
    % Appmock's app description contains mocked user (user2:password2)
    UserPasswordB64 = base64:encode(<<"user2:password2">>),
    BasicAuthHeaders = [
        {<<"authorization">>, <<"Basic ", UserPasswordB64/binary>>}
    ],
    ?assertMatch({ok, 200, _, _}, http_client:post(
        BasicAuthEndpoint, BasicAuthHeaders, [], [insecure]
    )),
    % now for the groups check
    {ok, [{groups, GroupIds}]} = ?assertMatch(
        {ok, [{groups, _}]},
        rpc:call(Node, user_logic, get_groups, [<<"user2Id">>])
    ),
    ?assertEqual([<<"group1">>, <<"group2">>], lists:sort(GroupIds)),
    ok.

% This tests checks if password changing procedure correctly follows the
% request to onepanel.
change_password_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    % Set the env variable in OZ that points to onepanel URL to appmock's
    % mocked endpoint.
    ok = test_utils:set_env(
        Node, oz_worker, onepanel_rest_url, appmock_mocked_endpoint(Config)
    ),
    % Appmock's app description contains change password endpoint that
    % accepts (user3:password3) credentials for user with id userId3
    % Try to change password of userId3. First, use wrong password.
    ?assertEqual({error, <<"Invalid password">>}, rpc:call(
        Node, user_logic, change_user_password, [
            <<"user3">>, <<"bad_password">>, <<"new_password">>
        ]
    )),
    % Now with correct credentials
    ?assertEqual(ok, rpc:call(
        Node, user_logic, change_user_password, [
            <<"user3">>, <<"password3">>, <<"new_password">>
        ]
    )),
    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(etls),
    hackney:start(),
    NewConfig = ?TEST_INIT(
        Config, ?TEST_FILE(Config, "env_desc.json"), [oz_test_utils]
    ),
    NewConfig.

end_per_suite(Config) ->
    hackney:stop(),
    application:stop(etls),
    test_node_starter:clean_environment(Config).

end_per_testcase(Config, set_space_name_mapping_test) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_validate_and_unload(Nodes, space);
end_per_testcase(_Config, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

space_save_mock(Nodes, SpaceId) ->
    test_utils:mock_new(Nodes, space),
    test_utils:mock_expect(Nodes, space, save, fun(Doc) ->
        meck:passthrough([Doc#document{key = SpaceId}])
    end).

get_space_name_mapping(Node, UserId, SpaceId) ->
    {ok, Data} = ?assertMatch({ok, _},
        rpc:call(Node, space_logic, get_data, [SpaceId, {user, UserId}])),
    proplists:get_value(name, Data).

clean_space_name_mapping(Node, UserId, SpaceId) ->
    rpc:call(Node, user_logic, clean_space_name_mapping, [UserId, SpaceId]).

appmock_mocked_endpoint(Config) ->
    [AppmockNode] = ?config(appmock_nodes, Config),
    AppmockIP = test_utils:get_docker_ip(AppmockNode),
    str_utils:format("https://~s:9443", [AppmockIP]).