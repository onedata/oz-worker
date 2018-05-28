%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
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
-include("datastore/oz_datastore_models.hrl").
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
    basic_auth_cache_test/1,
    basic_auth_login_test/1,
    automatic_group_membership_test/1,
    change_password_test/1,
    merge_groups_in_linked_accounts_test/1
]).

all() ->
    ?ALL([
        basic_auth_cache_test,
        basic_auth_login_test,
        automatic_group_membership_test,
        change_password_test,
        merge_groups_in_linked_accounts_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

% Check if basic auth login endpoint works.
basic_auth_cache_test(Config) ->
    Login = <<"user1">>,
    Password = <<"password1">>,
    IncorrectPassword = <<"password2">>,

    lists:foreach(
        % As basic auth cache is node specific,
        % it should be tested on each node individually
        fun(Node) ->
            Nodes = [Node],
            TempConf = lists:map(
                fun
                    ({oz_worker_nodes, _}) -> {oz_worker_nodes, Nodes};
                    (Val) -> Val
                end, Config
            ),

            % Make sure there is no cached record for given user
            ?assertMatch({error, not_found}, oz_test_utils:call_oz(
                TempConf, basic_auth_cache, get, [Login, Password])
            ),

            % After calling authenticating function, fetched resource should be cached
            ?assertMatch({ok, _}, oz_test_utils:call_oz(
                TempConf, user_logic, authenticate_by_basic_credentials, [Login, Password]
            )),
            ?assertMatch({ok, _}, oz_test_utils:call_oz(
                TempConf, basic_auth_cache, get, [Login, Password])
            ),

            % It should be still valid after only 2 seconds and should expire after 5 seconds overall
            timer:sleep(timer:seconds(2)),
            ?assertMatch({ok, _}, oz_test_utils:call_oz(
                TempConf, basic_auth_cache, get, [Login, Password])
            ),
            timer:sleep(timer:seconds(3)),
            ?assertMatch({error, not_found}, oz_test_utils:call_oz(
                TempConf, basic_auth_cache, get, [Login, Password])
            ),

            % Authenticating with incorrect credentials should fail with nothing cached
            ?assertMatch({error, _}, oz_test_utils:call_oz(
                TempConf, user_logic, authenticate_by_basic_credentials, [Login, IncorrectPassword]
            )),
            ?assertMatch({error, not_found}, oz_test_utils:call_oz(
                TempConf, basic_auth_cache, get, [Login, Password])
            )

        end, ?config(oz_worker_nodes, Config)
    ),
    ok.


% Check if basic auth login endpoint works.
basic_auth_login_test(Config) ->
    % To resolve user details, OZ asks onepanel via a REST endpoint. In this
    % test, onepanel is simulated by mocking http_client.
    Nodes = ?config(oz_worker_nodes, Config),
    {ok, Domain} = test_utils:get_env(hd(Nodes), ?APP_NAME, http_domain),
    % now just try to log in into OZ using basic auth endpoint and
    % check if it works correctly.
    BasicAuthEndpoint = str_utils:format_bin(
        "https://~s/do_login", [Domain]
    ),
    UserPasswordB64 = base64:encode(<<"user1:password1">>),
    BasicAuthHeaders = #{
        <<"authorization">> => <<"Basic ", UserPasswordB64/binary>>
    },
    Opts = [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}],
    Response = http_client:post(
        BasicAuthEndpoint, BasicAuthHeaders, [], Opts
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
        BasicAuthEndpoint, WrongBasicAuthHeaders, [], Opts
    )),
    ok.

% Users that log in through basic auth should automatically be added to
% groups based on 'onepanel_role_to_group_mapping' env setting.
automatic_group_membership_test(Config) ->
    Nodes = [Node | _] = ?config(oz_worker_nodes, Config),
    {ok, Domain} = test_utils:get_env(Node, ?APP_NAME, http_domain),
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
    set_env_on_nodes(Nodes, ?APP_NAME, predefined_groups, PredefinedGroups),
    % Call the group creation procedure
    ok = rpc:call(Node, group_logic, create_predefined_groups, []),
    % Now, prepare config entry for onepanel role to groups mapping. We want
    % everyone with role "user2Role" to belong to both groups 1 and 2.
    RoleToGroupMapping = #{
        <<"user2Role">> => [<<"group1">>, <<"group2">>]
    },
    set_env_on_nodes(Nodes, ?APP_NAME, onepanel_role_to_group_mapping, RoleToGroupMapping),
    % Try to log in using credentials user2:password2 (user with id user2Id)
    % and see if he was added to both groups.
    BasicAuthEndpoint = str_utils:format_bin(
        "https://~s/do_login", [Domain]
    ),
    % Appmock's app description contains mocked user (user2:password2)
    UserPasswordB64 = base64:encode(<<"user2:password2">>),
    BasicAuthHeaders = #{
        <<"authorization">> => <<"Basic ", UserPasswordB64/binary>>
    },
    Opts = [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}],
    ?assertMatch({ok, 200, _, _}, http_client:post(
        BasicAuthEndpoint, BasicAuthHeaders, [], Opts
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
    FirstLinkedAcc = #linked_account{idp = ?IDP, groups = []},
    {ok, #document{key = UserId}} = oz_test_utils:call_oz(
        Config, user_logic, create_user_by_linked_account, [FirstLinkedAcc]
    ),
    ?assert(has_linked_accounts(Config, UserId, [FirstLinkedAcc])),
    ?assertEqual({ok, []}, oz_test_utils:user_get_groups(Config, UserId)),
    ?assertEqual({ok, []}, oz_test_utils:user_get_eff_groups(Config, UserId)),

    % Try linked acc with a group
    SecondLinkedAcc = #linked_account{idp = ?IDP, groups = [
        <<"vo:test-vo/user:member">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, SecondLinkedAcc]
    ),
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
    ?assertEqual({ok, []}, oz_test_utils:user_get_groups(Config, UserId)),
    ?assertEqual({ok, []}, oz_test_utils:user_get_eff_groups(Config, UserId)),

    % Linked acc with two groups
    ThirdLinkedAcc = #linked_account{idp = ?IDP, groups = [
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:some-role/user:admin">>,
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:other-role/user:manager">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, ThirdLinkedAcc]
    ),
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
    FourthLinkedAcc = #linked_account{idp = ?IDP, groups = [
        <<"vo:another-vo/ut:some-unit/tm:some-team/rl:some-role/user:admin">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, FourthLinkedAcc]
    ),
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
    FifthLinkedAcc = #linked_account{idp = ?IDP_WITH_SUPERGROUP, groups = [
        <<"vo:example-vo/tm:new-team/user:manager">>,
        <<(?SUPERGROUP)/binary, "/user:member">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, FifthLinkedAcc]
    ),
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
    SixthLinkedAcc = #linked_account{idp = ?IDP_WITH_SUPERGROUP, groups = [
        <<"vo:example-vo/tm:new-team/user:manager">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, SixthLinkedAcc]
    ),
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
    SeventhLinkedAcc = #linked_account{idp = ?IDP_WITH_SUPERGROUP, groups = [
        <<"vo:example-vo/tm:new-team/user:member">>
    ]},
    oz_test_utils:call_oz(
        Config, user_logic, merge_linked_account, [UserId, SeventhLinkedAcc]
    ),
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
    Posthook = fun(NewConfig) ->
        Nodes = ?config(oz_worker_nodes, NewConfig),
        set_env_on_nodes(Nodes, ctool, force_insecure_connections, true),
        % Sleep a while before mocking http_client (which is done in
        % init_per_testcase) - otherwise meck's reloading and purging the module
        % can cause the oz-worker application to crash.
        timer:sleep(5000),
        NewConfig
    end,
    [{env_up_posthook, Posthook}, {?LOAD_MODULES, [oz_test_utils]} | Config].

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
init_per_testcase(Case, Config) when 
Case =:= basic_auth_cache_test;
Case =:= basic_auth_login_test;
Case =:= automatic_group_membership_test ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, http_client, [passthrough]),
    ok = mock_onepanel_rest_get(Nodes),
    Config;
init_per_testcase(change_password_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, http_client, [passthrough]),
    ok = mock_onepanel_rest_get(Nodes),
    ok = mock_onepanel_rest_patch(Nodes),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(merge_groups_in_linked_accounts_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    % Used in merge_groups_in_linked_accounts_test
    test_utils:mock_unload(Nodes, auth_utils);
end_per_testcase(Case, Config) when
Case =:= basic_auth_cache_test;
Case =:= basic_auth_login_test;
Case =:= change_password_test;
Case =:= automatic_group_membership_test ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, http_client);
end_per_testcase(_, _) ->
    ok.

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


%%%===================================================================
%%% Internal functions
%%%===================================================================

mock_onepanel_rest_get(Nodes) ->
    ok = test_utils:mock_expect(Nodes, http_client, get,
        fun(Url, Headers, Body, Options) ->
            case binary:match(Url, <<"9443/api/v3/onepanel/users">>) of
                nomatch -> meck:passthrough([Url, Headers, Body, Options]);
                _ ->
                    <<"Basic ", UserAndPassword/binary>> = 
                        maps:get(<<"Authorization">>, Headers),
                    [User, Passwd] =
                        binary:split(base64:decode(UserAndPassword), <<":">>),
                    case {User, Passwd} of
                        {<<"user1">>, <<"password1">>} ->
                            ResponseBody = json_utils:encode(#{
                                <<"userId">> => <<"user1Id">>,
                                <<"userRole">> => <<"user1Role">>
                            }),
                            {ok, 200, #{}, ResponseBody};
                        {<<"user2">>, <<"password2">>} ->
                            ResponseBody = json_utils:encode(#{
                                <<"userId">> => <<"user2Id">>,
                                <<"userRole">> => <<"user2Role">>
                            }),
                            {ok, 200, #{}, ResponseBody};
                        _ -> {ok, 401, #{}, <<"">>}
                    end
            end
        end),
    ok.

mock_onepanel_rest_patch(Nodes) ->
    ok = test_utils:mock_expect(Nodes, http_client, patch,
        fun(Url, Headers, Body, Options) ->
            case binary:match(Url, <<"9443/api/v3/onepanel/users">>) of
                nomatch -> meck:passthrough([Url, Headers, Body, Options]);
                _ ->
                    <<"Basic ", UserAndPassword/binary>> = 
                        maps:get(<<"Authorization">>, Headers),
                    [User, Passwd] =
                        binary:split(base64:decode(UserAndPassword), <<":">>),
                    case {User, Passwd} of
                        {<<"user3">>, <<"password3">>} ->
                            BodyMap = json_utils:decode(Body),
                            OldPassword = maps:get(<<"currentPassword">>, 
                                                   BodyMap, undefined),
                            NewPassword = maps:get(<<"newPassword">>, 
                                                   BodyMap, undefined),
                            case {OldPassword, NewPassword} of
                                {undefined, _} ->
                                    {ok, 400, #{}, <<"">>};
                                {_, undefined} ->
                                    {ok, 400, #{}, <<"">>};
                                _ ->
                                    {ok, 204, #{}, <<"">>}
                            end;
                        _ -> {ok, 401, #{}, <<"">>}
                    end
            end
        end),
    ok.


%% DirectOrEff :: direct | effective
has_group(Config, UserId, GroupSpec, Name, Type, Privileges, DirectOrEff) ->
    try
        GroupId = oz_test_utils:call_oz(
            Config, idp_group_mapping, group_spec_to_db_id, [GroupSpec]
        ),
        {ok, UserGroups} = case DirectOrEff of
            direct ->
                oz_test_utils:user_get_groups(Config, UserId);
            effective ->
                oz_test_utils:user_get_eff_groups(Config, UserId)
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
                        oz_test_utils:group_get_user_privileges(
                            Config, GroupId, UserId
                        );
                    effective ->
                        oz_test_utils:group_get_eff_user_privileges(
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


set_env_on_nodes(Nodes, AppName, Key, Value) ->
    lists:foreach(fun(Node) ->
        ok = test_utils:set_env(Node, AppName, Key, Value)
    end, Nodes).
