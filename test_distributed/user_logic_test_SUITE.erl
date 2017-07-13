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
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    basic_auth_login_test/1,
    automatic_group_membership_test/1,
    change_password_test/1,
    merge_groups_in_linked_accounts_test/1
]).

all() ->
    ?ALL([
        basic_auth_login_test,
        automatic_group_membership_test,
        change_password_test,
        merge_groups_in_linked_accounts_test
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
    BasicAuthHeaders = #{
        <<"authorization">> => <<"Basic ", UserPasswordB64/binary>>
    },
    Response = http_client:post(
        BasicAuthEndpoint, BasicAuthHeaders, [], [insecure]
    ),
    ?assertMatch({ok, 200, _, _}, Response),
    {ok, 200, RespHeaders, _} = Response,
    % Make sure response headers contain cookie with session id - which means
    % that the user has logged in.
    Cookie = maps:get(<<"set-cookie">>, RespHeaders, <<"">>),
    ?assertMatch(<<"session_id=", _/binary>>, Cookie),
    % Try some inexistent user credentials if 401 is returned
    WrongUserPasswordB64 = base64:encode(<<"lol:wut">>),
    WrongBasicAuthHeaders = #{
        <<"authorization">> => <<"Basic ", WrongUserPasswordB64/binary>>
    },
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
            oz_privileges => [?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES]
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
    BasicAuthHeaders = #{
        <<"authorization">> => <<"Basic ", UserPasswordB64/binary>>
    },
    ?assertMatch({ok, 200, _, _}, http_client:post(
        BasicAuthEndpoint, BasicAuthHeaders, [], [insecure]
    )),
    % now for the groups check
    User2Id = oz_test_utils:call_oz(
        Config, user_logic, onepanel_uid_to_system_uid, [<<"user2Id">>]
    ),
    {ok, #od_user{groups = GroupIds}} = oz_test_utils:get_user(Config, User2Id),
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


-define(IDP, some_oidc_provider).
-define(IDP_WITH_SUPERGROUP, some_oidc_provider_with_super_group).
-define(SUPERGROUP, <<"vo:example-vo/tm:admins">>).
-define(SUPERGROUP_NAME, <<"admins">>).
-define(USER_PRIVS, privileges:group_user()).
-define(MANAGER_PRIVS, privileges:group_manager()).
-define(ADMIN_PRIVS, privileges:group_admin()).

merge_groups_in_linked_accounts_test(Config) ->
    % Super groups are mocked in init per testcase
    % Start with linked acc with no groups
    FirstLinkedAcc = #linked_account{provider_id = ?IDP, groups = []},
    {ok, UserId} = oz_test_utils:call_oz(
        Config, user_logic, create_user_by_linked_account, [FirstLinkedAcc]
    ),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ?assert(has_linked_accounts(Config, UserId, [FirstLinkedAcc])),
    ?assertEqual({ok, []}, oz_test_utils:get_user_groups(Config, UserId)),
    ?assertEqual({ok, []}, oz_test_utils:get_user_eff_groups(Config, UserId)),

    % Try linked acc with a group
    SecondLinkedAcc = #linked_account{provider_id = ?IDP, groups = [
        <<"vo:test-vo/user:member">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, SecondLinkedAcc]
    ),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ?assert(has_linked_accounts(Config, UserId, [SecondLinkedAcc])),
    ?assert(has_group(
        Config, UserId,
        <<"vo:test-vo">>, <<"test-vo">>, organization,
        ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:test-vo">>, <<"test-vo">>, organization,
        ?USER_PRIVS, effective
    )),

    % Go back to linked acc with no groups and see if they were removed
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, FirstLinkedAcc]
    ),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ?assertEqual({ok, []}, oz_test_utils:get_user_groups(Config, UserId)),
    ?assertEqual({ok, []}, oz_test_utils:get_user_eff_groups(Config, UserId)),

    % Linked acc with two groups
    ThirdLinkedAcc = #linked_account{provider_id = ?IDP, groups = [
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:some-role/user:admin">>,
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:other-role/user:manager">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, ThirdLinkedAcc]
    ),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ?assert(has_linked_accounts(Config, UserId, [ThirdLinkedAcc])),
    ?assertNot(has_group(
        Config, UserId,
        <<"vo:another-vo">>,
        <<"another-vo">>, organization, ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo">>,
        <<"another-vo">>, organization, ?USER_PRIVS, effective
    )),
    ?assertNot(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit">>,
        <<"some-unit">>, unit, ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit">>,
        <<"some-unit">>, unit, ?USER_PRIVS, effective
    )),
    ?assertNot(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team">>,
        <<"some-team">>, team, ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team">>,
        <<"some-team">>, team, ?USER_PRIVS, effective
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:some-role">>,
        <<"some-role">>, role, ?ADMIN_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:some-role">>,
        <<"some-role">>, role, ?ADMIN_PRIVS, effective
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:other-role">>,
        <<"other-role">>, role, ?MANAGER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:other-role">>,
        <<"other-role">>, role, ?MANAGER_PRIVS, effective
    )),

    % Linked acc the same as before but with one group removed
    FourthLinkedAcc = #linked_account{provider_id = ?IDP, groups = [
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:some-role/user:admin">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, FourthLinkedAcc]
    ),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ?assert(has_linked_accounts(Config, UserId, [FourthLinkedAcc])),
    ?assertNot(has_group(
        Config, UserId,
        <<"vo:another-vo">>,
        <<"another-vo">>, organization, ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo">>,
        <<"another-vo">>, organization, ?USER_PRIVS, effective
    )),
    ?assertNot(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit">>,
        <<"some-unit">>, unit, ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit">>,
        <<"some-unit">>, unit, ?USER_PRIVS, effective
    )),
    ?assertNot(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team">>,
        <<"some-team">>, team, ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team">>,
        <<"some-team">>, team, ?USER_PRIVS, effective
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:some-role">>,
        <<"some-role">>, role, ?ADMIN_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:some-role">>,
        <<"some-role">>, role, ?ADMIN_PRIVS, effective
    )),
    ?assertNot(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:other-role">>,
        <<"other-role">>, role, ?MANAGER_PRIVS, direct
    )),
    ?assertNot(has_group(
        Config, UserId,
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:other-role">>,
        <<"other-role">>, role, ?MANAGER_PRIVS, effective
    )),

    % Linked acc from other provider
    FifthLinkedAcc = #linked_account{provider_id = ?IDP_WITH_SUPERGROUP, groups = [
        <<"vo:example-vo/tm:new-team/user:manager">>,
        <<(?SUPERGROUP)/binary, "/user:member">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, FifthLinkedAcc]
    ),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ?assert(has_linked_accounts(Config, UserId, [FourthLinkedAcc, FifthLinkedAcc])),
    % Because the user belongs to the super group, he should have admin
    % rights (effective) in all the groups, beside the admin group (his privs
    % there are decided based on membership spec in admin group).
    ?assertNot(has_group(
        Config, UserId,
        <<"vo:example-vo">>,
        <<"example-vo">>, organization, ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:example-vo">>,
        <<"example-vo">>, organization, ?ADMIN_PRIVS, effective
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:example-vo/tm:new-team">>,
        <<"new-team">>, team, ?MANAGER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:example-vo/tm:new-team">>,
        <<"new-team">>, team, ?ADMIN_PRIVS, effective
    )),
    ?assert(has_group(
        Config, UserId,
        ?SUPERGROUP,
        ?SUPERGROUP_NAME, team, ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        ?SUPERGROUP,
        ?SUPERGROUP_NAME, team, ?USER_PRIVS, effective
    )),

    % Linked acc the same as above but without the super group
    SixthLinkedAcc = #linked_account{provider_id = ?IDP_WITH_SUPERGROUP, groups = [
        <<"vo:example-vo/tm:new-team/user:manager">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, SixthLinkedAcc]
    ),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ?assert(has_linked_accounts(Config, UserId, [FourthLinkedAcc, SixthLinkedAcc])),
    % Now, the user should lose admin privs to all groups
    ?assertNot(has_group(
        Config, UserId,
        <<"vo:example-vo">>,
        <<"example-vo">>, organization, ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:example-vo">>,
        <<"example-vo">>, organization, ?USER_PRIVS, effective
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:example-vo/tm:new-team">>,
        <<"new-team">>, team, ?MANAGER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:example-vo/tm:new-team">>,
        <<"new-team">>, team, ?MANAGER_PRIVS, effective
    )),
    ?assertNot(has_group(
        Config, UserId,
        ?SUPERGROUP,
        ?SUPERGROUP_NAME, team, ?USER_PRIVS, direct
    )),
    ?assertNot(has_group(
        Config, UserId,
        ?SUPERGROUP,
        ?SUPERGROUP_NAME, team, ?USER_PRIVS, effective
    )),

    % Linked acc the same as above but user's privileges in the group
    % are downgraded to member.
    SeventhLinkedAcc = #linked_account{provider_id = ?IDP_WITH_SUPERGROUP, groups = [
        <<"vo:example-vo/tm:new-team/user:member">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, SeventhLinkedAcc]
    ),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ?assert(has_linked_accounts(Config, UserId, [FourthLinkedAcc, SeventhLinkedAcc])),
    % Now, the user should lose admin privs to all groups
    ?assertNot(has_group(
        Config, UserId,
        <<"vo:example-vo">>,
        <<"example-vo">>, organization, ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:example-vo">>,
        <<"example-vo">>, organization, ?USER_PRIVS, effective
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:example-vo/tm:new-team">>,
        <<"new-team">>, team, ?USER_PRIVS, direct
    )),
    ?assert(has_group(
        Config, UserId,
        <<"vo:example-vo/tm:new-team">>,
        <<"new-team">>, team, ?USER_PRIVS, effective
    )),
    ?assertNot(has_group(
        Config, UserId,
        ?SUPERGROUP,
        ?SUPERGROUP_NAME, team, ?USER_PRIVS, direct
    )),
    ?assertNot(has_group(
        Config, UserId,
        ?SUPERGROUP,
        ?SUPERGROUP_NAME, team, ?USER_PRIVS, effective
    )),
    ok.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(merge_groups_in_linked_accounts_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, auth_utils, [passthrough]),
    ok = test_utils:mock_expect(Nodes, auth_utils, get_super_group,
        fun(ProviderId) ->
            case ProviderId of
                ?IDP -> undefined;
                ?IDP_WITH_SUPERGROUP -> ?SUPERGROUP
            end
        end),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(merge_groups_in_linked_accounts_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    % Used in merge_groups_in_linked_accounts_test
    test_utils:mock_unload(Nodes, auth_utils);
end_per_testcase(_, _) ->
    ok.

end_per_suite(Config) ->
    hackney:stop(),
    ssl:stop().


%%%===================================================================
%%% Internal functions
%%%===================================================================

appmock_mocked_endpoint(Config) ->
    [AppmockNode] = ?config(appmock_nodes, Config),
    AppmockIP = test_utils:get_docker_ip(AppmockNode),
    str_utils:format("https://~s:9443", [AppmockIP]).


%% DirectOrEff :: direct | effective
has_group(Config, UserId, GroupSpec, Name, Type, Privileges, DirectOrEff) ->
    try
        GroupId = oz_test_utils:call_oz(
            Config, idp_group_mapping, group_spec_to_db_id, [GroupSpec]
        ),
        {ok, UserGroups} = case DirectOrEff of
            direct ->
                oz_test_utils:get_user_groups(Config, UserId);
            effective ->
                oz_test_utils:get_user_eff_groups(Config, UserId)
        end,
        BelongsToGroup = lists:member(GroupId, UserGroups),
        {ok, #od_group{
            name = GroupName, type = GroupType
        }} = oz_test_utils:get_group(Config, GroupId),
        NameAndTypeMatch = GroupName =:= Name andalso GroupType =:= Type,
        case BelongsToGroup andalso NameAndTypeMatch of
            false ->
                false;
            true ->
                {ok, UserPrivileges} = case DirectOrEff of
                    direct ->
                        oz_test_utils:get_group_user_privileges(
                            Config, GroupId, UserId
                        );
                    effective ->
                        oz_test_utils:get_group_eff_user_privileges(
                            Config, GroupId, UserId
                        )
                end,
                UserPrivileges =:= Privileges
        end
    catch _:_ ->
        false
    end.


has_linked_accounts(Config, UserId, LinkedAccounts) ->
    {ok, #od_user{
        linked_accounts = ActualLinkedAccounts
    }} = oz_test_utils:get_user(Config, UserId),
    lists:sort(LinkedAccounts) =:= lists:sort(ActualLinkedAccounts).