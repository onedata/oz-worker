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
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("auth/entitlement_mapping.hrl").
-include("http/gui_paths.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("gui/include/gui_session.hrl").

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
    coalesce_entitlements_test/1
]).

all() ->
    ?ALL([
        basic_auth_cache_test,
        basic_auth_login_test,
        automatic_group_membership_test,
        change_password_test,
        coalesce_entitlements_test
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
            oz_test_utils:toggle_basic_auth(Config, true),
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
            ),

            % When basic auth is disabled in auth.config, proper error should be returned
            oz_test_utils:toggle_basic_auth(Config, false),
            ?assertMatch({error, onepanel_auth_disabled}, oz_test_utils:call_oz(
                TempConf, user_logic, authenticate_by_basic_credentials, [Login, Password]
            ))
        end, ?config(oz_worker_nodes, Config)
    ),
    ok.


% Check if basic auth login endpoint works.
basic_auth_login_test(Config) ->
    % To resolve user details, OZ asks onepanel via a REST endpoint. In this
    % test, onepanel is simulated by mocking http_client.
    % now just try to log in into OZ using basic auth endpoint and
    % check if it works correctly.
    BasicAuthEndpoint = str_utils:format_bin(
        "https://~s~s", [oz_test_utils:oz_domain(Config), ?LOGIN_PATH]
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
    CookieKey = ?SESSION_COOKIE_KEY,
    CookieLen = byte_size(?SESSION_COOKIE_KEY),
    ?assertMatch(<<CookieKey:CookieLen/binary, "=", _/binary>>, Cookie),
    % Try some inexistent user credentials if 401 is returned
    WrongUserPasswordB64 = base64:encode(<<"lol:wut">>),
    WrongBasicAuthHeaders = #{
        <<"authorization">> => <<"Basic ", WrongUserPasswordB64/binary>>
    },
    ?assertMatch({ok, 401, _, _}, http_client:post(
        BasicAuthEndpoint, WrongBasicAuthHeaders, [], Opts
    )),

    % Basic auth login should not work if basic auth is disabled in auth.config
    oz_test_utils:toggle_basic_auth(Config, false),
    ?assertMatch({ok, 400, _, _}, http_client:post(
        BasicAuthEndpoint, BasicAuthHeaders, [], Opts
    )),
    ?assertMatch({ok, 400, _, _}, http_client:post(
        BasicAuthEndpoint, WrongBasicAuthHeaders, [], Opts
    )),

    ok.

% Users that log in through basic auth should automatically be added to
% groups based on 'onepanel_role_to_group_mapping' env setting.
automatic_group_membership_test(Config) ->
    Nodes = [Node | _] = ?config(oz_worker_nodes, Config),
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
    test_utils:set_env(Nodes, ?APP_NAME, predefined_groups, PredefinedGroups),
    % Call the group creation procedure
    ok = rpc:call(Node, group_logic, create_predefined_groups, []),
    % Now, prepare config entry for onepanel role to groups mapping. We want
    % everyone with role "user2Role" to belong to both groups 1 and 2.
    RoleToGroupMapping = #{
        <<"user2Role">> => [<<"group1">>, <<"group2">>]
    },
    test_utils:set_env(Nodes, ?APP_NAME, onepanel_role_to_group_mapping, RoleToGroupMapping),
    % Try to log in using credentials user2:password2 (user with id user2Id)
    % and see if he was added to both groups.
    BasicAuthEndpoint = str_utils:format_bin(
        "https://~s~s", [oz_test_utils:oz_domain(Config), ?LOGIN_PATH]
    ),
    % See mock_onepanel_rest_get/1.
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
    % See mock_onepanel_rest_patch/1.
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

    oz_test_utils:toggle_basic_auth(Config, false),
    % Password changing should not work if basic auth is disabled in auth.config
    ?assertEqual({error, onepanel_auth_disabled}, rpc:call(
        Node, user_logic, change_user_password, [
            <<"user3">>, <<"bad_password">>, <<"new_password">>
        ]
    )),

    % Password changing should not work if basic auth is disabled in auth.config
    ?assertEqual({error, onepanel_auth_disabled}, rpc:call(
        Node, user_logic, change_user_password, [
            <<"user3">>, <<"password3">>, <<"new_password">>
        ]
    )),
    ok.


% Below macros use the variables Config and UserId, which appear in merge_groups_in_linked_accounts_test/1
-define(RETRIES, 600).
-define(INTERVAL, 100). % 600 Attempts every 100 ms - 1 minute

-define(assertGroupExists(__Flag, __IdP, __RawEntitlement),
    ?assertEqual(
        __Flag,
        check_group_exists(Config, __IdP, __RawEntitlement),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertHasGroup(__Flag, __IdP, __RawEntitlement, __RelationType),
    ?assertEqual(
        __Flag,
        check_has_group(Config, UserId, __IdP, __RawEntitlement, __RelationType),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertGroupsCount(__Direct, __Effective),
    ?assertEqual(
        true,
        check_groups_count(Config, UserId, __Direct, __Effective),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertGroupStructure(__IdP, __ParentRawEntitlement, __ChildRawEntitlement, __RelationType),
    ?assertEqual(
        true,
        check_group_structure(Config, __IdP, __ParentRawEntitlement, __ChildRawEntitlement, __RelationType),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertHasLinkedAccount(__LinkedAcc),
    ?assert(check_has_linked_account(Config, UserId, __LinkedAcc))
).

-define(assertLinkedAccountsCount(__Number),
    ?assertEqual(
        __Number,
        length(get_linked_accounts(Config, UserId)),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(DUMMY_IDP, dummyIdP).
-define(OTHER_IDP, anotherIdP).
-define(THIRD_IDP, thirdIdP).

coalesce_entitlements_test(Config) ->
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, false, undefined, undefined, undefined),
    overwrite_entitlement_mapping(Config, ?OTHER_IDP, false, undefined, undefined, undefined),
    overwrite_entitlement_mapping(Config, ?THIRD_IDP, false, undefined, undefined, undefined),

    % Helpers for shorter code
    LinkedAcc = fun(IdP, Entitlements) ->
        #linked_account{
            idp = IdP,
            subject_id = <<(atom_to_binary(IdP, utf8))/binary, "-subjectId">>,
            entitlements = Entitlements
        }
    end,

    FirstLinkedAcc = LinkedAcc(?DUMMY_IDP, []),
    {ok, #document{key = UserId}} = oz_test_utils:call_oz(
        Config, user_logic, create_user_by_linked_account, [FirstLinkedAcc]
    ),
    ?assertHasLinkedAccount(FirstLinkedAcc),
    ?assertGroupsCount(0, 0),
    ?assertLinkedAccountsCount(1),

    MergeAcc = fun(IdP, Entitlements) ->
        oz_test_utils:call_oz(Config, user_logic, merge_linked_account, [
            UserId, LinkedAcc(IdP, Entitlements)
        ]),
        % Make sure
        ?assertHasLinkedAccount(LinkedAcc(IdP, Entitlements))
    end,

    % Entitlement mapping is disabled for now
    MergeAcc(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    ?assertGroupsCount(0, 0),
    ?assertLinkedAccountsCount(1),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"thirdGroup">>),

    % Enable entitlement mapping that uses a flat_entitlement_parser
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, true, flat_entitlement_parser, undefined, undefined),
    MergeAcc(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    ?assertGroupsCount(2, 2),
    ?assertLinkedAccountsCount(1),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"thirdGroup">>),

    % Simulate a situation when a new entitlement appears
    MergeAcc(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>, <<"thirdGroup">>]),
    ?assertGroupsCount(3, 3),
    ?assertLinkedAccountsCount(1),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"thirdGroup">>),

    % Simulate a situation when two user entitlements are withdrawn
    MergeAcc(?DUMMY_IDP, [<<"anotherGroup">>]),
    ?assertGroupsCount(1, 1),
    ?assertLinkedAccountsCount(1),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    % The groups should not be removed
    ?assertGroupExists(true, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"thirdGroup">>),

    % Link a new user account in IdP that has currently disabled entitlement mapping
    MergeAcc(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertGroupsCount(1, 1),
    ?assertLinkedAccountsCount(2),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertHasGroup(false, ?OTHER_IDP, <<"users/admins">>, direct),
    ?assertHasGroup(false, ?OTHER_IDP, <<"users/developers">>, direct),
    ?assertHasGroup(false, ?OTHER_IDP, <<"users/technicians">>, direct),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"users/admins">>),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"users/developers">>),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"users/technicians">>),

    % Turn on entitlement mapping in ?ANOTHER_IDP, with "users/admins" as admin Group
    overwrite_entitlement_mapping(Config, ?OTHER_IDP, true, nested_entitlement_parser, undefined, "users/admins"),
    MergeAcc(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertGroupsCount(4, 5),
    ?assertLinkedAccountsCount(2),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/admins">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/developers">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/technicians">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users">>, effective),
    ?assertGroupExists(true, ?OTHER_IDP, <<"users/admins">>),
    ?assertGroupExists(true, ?OTHER_IDP, <<"users/developers">>),
    ?assertGroupExists(true, ?OTHER_IDP, <<"users/technicians">>),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/admins">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/developers">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/technicians">>, direct),
    % The admin group should belong to all groups
    ?assertGroupStructure(?OTHER_IDP, <<"users/developers">>, <<"users/admins">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users/technicians">>, <<"users/admins">>, direct),

    % Turn off entitlement mapping for the DUMMY_IDP, which should remove related entitlements
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, false, undefined, undefined, undefined),
    MergeAcc(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>, <<"thirdGroup">>]),
    ?assertGroupsCount(3, 4),
    ?assertLinkedAccountsCount(2),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/admins">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/developers">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/technicians">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users">>, effective),

    % Turn on entitlement mapping for the DUMMY_IDP again
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, true, flat_entitlement_parser, undefined, undefined),
    % Turn on entitlement mapping for the ?THIRD_IDP, including a VO and admin group
    overwrite_entitlement_mapping(Config, ?THIRD_IDP, true, nested_entitlement_parser, "Third-VO", "staff/admins/privileged"),
    % Single login from ?THIRD_IDP should also trigger adding the entitlements from ?DUMMY_IDP
    MergeAcc(?THIRD_IDP, [<<"staff/vm-operators">>, <<"task4.1">>, <<"testGroup">>, <<"staff/admins/privileged">>]),
    ?assertGroupsCount(10, 14),
    ?assertLinkedAccountsCount(3),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/admins">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/developers">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/technicians">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users">>, effective),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff/vm-operators">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"task4.1">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"testGroup">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff/admins/privileged">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"Third-VO">>, effective),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff">>, effective),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff/admins">>, effective),
    % All groups should belong to their parents and the top parent to the VO group
    ?assertGroupStructure(?THIRD_IDP, <<"staff">>, <<"staff/vm-operators">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"staff">>, <<"staff/admins">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"staff/admins">>, <<"staff/admins/privileged">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"staff">>, <<"staff/admins/privileged">>, effective),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff/vm-operators">>, effective),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff/admins">>, effective),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff/admins/privileged">>, effective),
    % The admin group should belong to all groups
    ?assertGroupStructure(?THIRD_IDP, <<"staff">>, <<"staff/admins/privileged">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"staff/admins">>, <<"staff/admins/privileged">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"task4.1">>, <<"staff/admins/privileged">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"testGroup">>, <<"staff/admins/privileged">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff/admins/privileged">>, direct),

    % Turn off entitlement mapping for all IdPs
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, false, undefined, undefined, undefined),
    overwrite_entitlement_mapping(Config, ?OTHER_IDP, false, undefined, undefined, undefined),
    overwrite_entitlement_mapping(Config, ?THIRD_IDP, false, undefined, undefined, undefined),
    % Merge one of the accounts, which should trigger removal of all entitlements
    MergeAcc(?THIRD_IDP, [<<"staff/vm-operators">>, <<"task4.1">>, <<"testGroup">>, <<"staff/admins/privileged">>]),
    ?assertGroupsCount(0, 0),
    ?assertLinkedAccountsCount(3),

    % Turn the mapping on again
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, true, flat_entitlement_parser, undefined, undefined),
    overwrite_entitlement_mapping(Config, ?OTHER_IDP, true, nested_entitlement_parser, undefined, "users/admins"),
    overwrite_entitlement_mapping(Config, ?THIRD_IDP, true, nested_entitlement_parser, "Third-VO", "staff/admins/privileged"),
    % Merge one of the accounts, which should add the entitlements again
    MergeAcc(?THIRD_IDP, [<<"staff/vm-operators">>, <<"task4.1">>, <<"testGroup">>, <<"staff/admins/privileged">>]),
    ?assertGroupsCount(10, 14),
    ?assertLinkedAccountsCount(3),

    % Check if all groups are protected
    lists:foreach(fun({IdP, Entitlement}) ->
        ?assert(is_protected(Config, IdP, Entitlement))
    end, [
        {?DUMMY_IDP, <<"group/subgroup">>},
        {?DUMMY_IDP, <<"anotherGroup">>},
        {?DUMMY_IDP, <<"thirdGroup">>},

        {?OTHER_IDP, <<"users/admins">>},
        {?OTHER_IDP, <<"users/developers">>},
        {?OTHER_IDP, <<"users/technicians">>},
        {?OTHER_IDP, <<"users">>},

        {?THIRD_IDP, <<"staff/vm-operators">>},
        {?THIRD_IDP, <<"task4.1">>},
        {?THIRD_IDP, <<"testGroup">>},
        {?THIRD_IDP, <<"staff/admins/privileged">>},
        {?THIRD_IDP, <<"staff">>},
        {?THIRD_IDP, <<"staff/admins">>}
    ]).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) ->
        Nodes = ?config(oz_worker_nodes, NewConfig),
        test_utils:set_env(Nodes, ctool, force_insecure_connections, true),
        % Sleep a while before mocking http_client (which is done in
        % init_per_testcase) - otherwise meck's reloading and purging the module
        % can cause the oz-worker application to crash.
        timer:sleep(5000),
        NewConfig
    end,
    [{env_up_posthook, Posthook}, {?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(Case, Config) when
    Case =:= basic_auth_cache_test;
    Case =:= basic_auth_login_test;
    Case =:= automatic_group_membership_test ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, http_client, [passthrough]),
    ok = mock_onepanel_rest_get(Nodes),
    init_per_testcase(default, Config);
init_per_testcase(change_password_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, http_client, [passthrough]),
    ok = mock_onepanel_rest_get(Nodes),
    ok = mock_onepanel_rest_patch(Nodes),
    init_per_testcase(default, Config);
init_per_testcase(_, Config) ->
    oz_test_utils:toggle_basic_auth(Config, true),
    Config.

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
                        maps:get(<<"authorization">>, Headers),
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
                        maps:get(<<"authorization">>, Headers),
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


check_group_exists(Config, IdP, RawEntitlement) ->
    try
        IdPEntitlement = expected_parsing_result(Config, IdP, RawEntitlement),
        GroupId = entitlement_mapping:gen_group_id(IdPEntitlement),
        oz_test_utils:call_oz(Config, group_logic, exists, [GroupId])
    catch _:_ ->
        false
    end.


%% RelationType :: direct | effective
check_has_group(Config, UserId, IdP, RawEntitlement, RelationType) ->
    try
        IdPEntitlement = #idp_entitlement{
            idp = IdP,
            path = Path,
            privileges = Privileges
        } = expected_parsing_result(Config, IdP, RawEntitlement),
        #idp_group{name = Name, type = Type} = lists:last(Path),
        NormalizedName = entity_logic:normalize_name(Name),
        GroupId = entitlement_mapping:gen_group_id(IdPEntitlement),

        UserGroups = get_groups(Config, UserId, RelationType),
        BelongsToGroup = lists:member(GroupId, UserGroups),
        {ok, #od_group{
            name = GroupName, type = GroupType
        }} = oz_test_utils:get_group(Config, GroupId),
        NameAndTypeMatch = GroupName =:= NormalizedName andalso GroupType =:= Type,
        case BelongsToGroup andalso NameAndTypeMatch of
            false ->
                false;
            true ->
                case RelationType of
                    direct ->
                        {ok, UserPrivileges} = oz_test_utils:call_oz(
                            Config, group_logic, get_user_privileges, [?ROOT, GroupId, UserId]
                        ),
                        UserPrivileges =:= entitlement_mapping:map_privileges(Privileges);
                    effective ->
                        % Do not check effective privileges as they are hard
                        % to predict - might be inherited via different
                        % membership paths than the one currently examined
                        true
                end
        end
    catch _:_ ->
        false
    end.


check_groups_count(Config, UserId, ExpDirectCount, ExpEffectiveCount) ->
    DirectCount = length(get_groups(Config, UserId, direct)),
    EffectiveCount = length(get_groups(Config, UserId, effective)),
    EntitlementsCount = length(get_entitlements(Config, UserId)),
    DirectCount =:= ExpDirectCount andalso
        EntitlementsCount =:= ExpDirectCount andalso
        EffectiveCount =:= ExpEffectiveCount.


check_group_structure(Config, IdP, ParentRawEntitlement, ChildRawEntitlement, RelationType) ->
    try
        ParentEntitlement = #idp_entitlement{
            idp = IdP,
            path = ParentPath
        } = expected_parsing_result(Config, IdP, ParentRawEntitlement),
        ChildEntitlement = #idp_entitlement{
            idp = IdP,
            path = ChildPath
        } = expected_parsing_result(Config, IdP, ChildRawEntitlement),
        #idp_group{name = ParentName, type = ParentType} = lists:last(ParentPath),
        #idp_group{name = ChildName, type = ChildType} = lists:last(ChildPath),
        NormalizedParentName = entity_logic:normalize_name(ParentName),
        NormalizedChildName = entity_logic:normalize_name(ChildName),
        % If the child group is the adminGroup, it should have admin privileges in parents
        ParentGroupId = entitlement_mapping:gen_group_id(ParentEntitlement),
        ChildGroupId = entitlement_mapping:gen_group_id(ChildEntitlement),
        % Check if names and types of groups are as expected
        {ok, #od_group{
            name = NormalizedParentName, type = ParentType
        }} = oz_test_utils:get_group(Config, ParentGroupId),
        {ok, #od_group{
            name = NormalizedChildName, type = ChildType
        }} = oz_test_utils:get_group(Config, ChildGroupId),
        case RelationType of
            effective ->
                % Do not check effective privileges as they are hard
                % to predict - might be inherited via different
                % membership paths than the one currently examined
                {ok, _} = oz_test_utils:call_oz(
                    Config, group_logic, get_eff_child, [?ROOT, ParentGroupId, ChildGroupId]
                ),
                true;
            direct ->
                {ok, ActualChildPrivs} = oz_test_utils:call_oz(
                    Config, group_logic, get_child_privileges, [?ROOT, ParentGroupId, ChildGroupId]
                ),
                ExpChildPrivs = (lists:last(ChildPath))#idp_group.privileges,
                ActualChildPrivs =:= entitlement_mapping:map_privileges(ExpChildPrivs)
        end
    catch _:_ ->
        false
    end.


is_protected(Config, IdP, RawEntitlement) ->
    IdPEntitlement = expected_parsing_result(Config, IdP, RawEntitlement),
    GroupId = entitlement_mapping:gen_group_id(IdPEntitlement),
    case oz_test_utils:get_group(Config, GroupId) of
        {ok, #od_group{protected = true}} -> true;
        _ -> false
    end.


expected_parsing_result(Config, IdP, RawEntitlement) ->
    Parser = get_parser(Config, IdP),
    Parser =:= undefined andalso throw(undefined_parser),
    Entitlement = #idp_entitlement{
        idp = IdP,
        path = ParentPath
    } = Parser:parse(IdP, RawEntitlement, parser_config(Parser)),
    % Include the VO group if it was specified and is not the same as the entitlement
    % (in such case just update its type to organization).
    EntitlementWithVoGroup = case get_vo_group(Config, IdP) of
        undefined ->
            Entitlement;
        RawEntitlement ->
            % The RawEntitlement is the Vo Group
            Entitlement#idp_entitlement{
                path = [#idp_group{type = organization, name = RawEntitlement}]
            };
        VoName ->
            % Prepend the Vo to the group path
            Entitlement#idp_entitlement{
                path = [#idp_group{type = organization, name = VoName} | ParentPath]
            }
    end,
    % Include the admin group privs if the admin group was specified and is the same
    % as the entitlement
    case get_admin_group(Config, IdP) of
        RawEntitlement ->
            [Last | T] = lists:reverse(EntitlementWithVoGroup#idp_entitlement.path),
            OverridenPrivs = Last#idp_group{privileges = admin},
            EntitlementWithVoGroup#idp_entitlement{path = lists:reverse([OverridenPrivs | T])};
        _ ->
            EntitlementWithVoGroup
    end.


get_groups(Config, UserId, RelationType) ->
    {ok, UserGroups} = case RelationType of
        direct -> oz_test_utils:user_get_groups(Config, UserId);
        effective -> oz_test_utils:user_get_eff_groups(Config, UserId)
    end,
    UserGroups.


get_entitlements(Config, UserId) ->
    {ok, #od_user{entitlements = Entitlements}} = oz_test_utils:get_user(Config, UserId),
    Entitlements.


check_has_linked_account(Config, UserId, LinkedAccount) ->
    lists:member(LinkedAccount, get_linked_accounts(Config, UserId)).


get_linked_accounts(Config, UserId) ->
    {ok, #od_user{
        linked_accounts = LinkedAccounts
    }} = oz_test_utils:get_user(Config, UserId),
    LinkedAccounts.


overwrite_entitlement_mapping(Config, IdP, Enabled, Parser, VoGroupName, AdminGroup) ->
    OldAuthConfig = oz_test_utils:read_auth_config(Config),
    SupportedIdPs = maps:get(supportedIdps, OldAuthConfig, []),

    IdPConfig = {IdP, #{
        protocol => openid,
        protocolConfig => #{
            plugin => default_oidc_plugin,
            entitlementMapping => #{
                enabled => Enabled,
                voGroupName => VoGroupName,
                adminGroup => AdminGroup,
                parser => Parser,
                parserConfig => parser_config(Parser)
            }
        }
    }},

    oz_test_utils:overwrite_auth_config(Config, #{
        openidConfig => #{
            enabled => true
        },
        supportedIdps => lists:keystore(IdP, 1, SupportedIdPs, IdPConfig)
    }).


get_admin_group(Config, IdP) ->
    AdminGroup = oz_test_utils:call_oz(Config, auth_config, get_entitlement_mapping_config, [
        IdP, [adminGroup], {default, undefined}
    ]),
    case AdminGroup of
        undefined -> undefined;
        Str -> list_to_binary(Str)
    end.


get_vo_group(Config, IdP) ->
    VoGroup = oz_test_utils:call_oz(Config, auth_config, get_entitlement_mapping_config, [
        IdP, [voGroupName], {default, undefined}
    ]),
    case VoGroup of
        undefined -> undefined;
        Str -> list_to_binary(Str)
    end.


get_parser(Config, IdP) ->
    oz_test_utils:call_oz(Config, auth_config, get_entitlement_mapping_config, [
        IdP, [parser], {default, undefined}
    ]).


parser_config(undefined) -> #{
};
parser_config(flat_entitlement_parser) -> #{
    groupType => team,
    groupPrivilegesInVo => member,
    userPrivileges => member
};
parser_config(nested_entitlement_parser) -> #{
    splitWith => "/",
    topGroupType => unit,
    topGroupPrivilegesInVo => member,
    subGroupsType => team,
    subGroupsPrivilegesInParent => member,
    userPrivileges => member
}.
