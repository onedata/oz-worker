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
    basic_auth_login_test/1,
    automatic_group_membership_test/1,
    change_password_test/1
]).

all() ->
    ?ALL([
        basic_auth_login_test,
        automatic_group_membership_test,
        change_password_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

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
            oz_privileges => {privileges, oz_privileges}
        },
        #{
            id => <<"group2">>,
            name => <<"Group 2">>,
            oz_privileges => [view_privileges, set_privileges]
        }
    ],
    % Set the corresponding env variable on one of the nodes
    ok = test_utils:set_env(
        Node, oz_worker, predefined_groups, PredefinedGroups
    ),
    % Call the group creation procedure
    ok = rpc:call(Node, n_group_logic, create_predefined_groups, []),
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
    {ok, #od_user{groups = GroupIds}} = oz_test_utils:get_user(Config, <<"user2Id">>),
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
        Node, n_user_logic, change_user_password, [
            <<"user3">>, <<"bad_password">>, <<"new_password">>
        ]
    )),
    % Now with correct credentials
    ?assertEqual(ok, rpc:call(
        Node, n_user_logic, change_user_password, [
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
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(etls).

end_per_testcase(Config, set_space_name_mapping_test) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_validate_and_unload(Nodes, od_space);
end_per_testcase(_Config, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

appmock_mocked_endpoint(Config) ->
    [AppmockNode] = ?config(appmock_nodes, Config),
    AppmockIP = test_utils:get_docker_ip(AppmockNode),
    str_utils:format("https://~s:9443", [AppmockIP]).