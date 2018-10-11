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
        "https://~s/do_login", [Domain]
    ),
    % See get/1.
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

-define(assertGroupExists(__Flag, __IdP, __Parser, __RawEntitlement),
    ?assertEqual(
        __Flag,
        group_exists(Config, __IdP, __Parser, __RawEntitlement),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertHasGroup(__Flag, __IdP, __Parser, __RawEntitlement, __RelationType),
    ?assertEqual(
        __Flag,
        has_group(Config, UserId, __IdP, __Parser, __RawEntitlement, __RelationType),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertGroupsCount(__Direct, __Effective),
    ?assertEqual(
        true,
        has_groups_count(Config, UserId, __Direct, __Effective),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertGroupStructure(__IdP, __Parser, __ParentRawEntitlement, __ChildRawEntitlement),
    ?assertEqual(
        true,
        check_group_structure(Config, __IdP, __Parser, __ParentRawEntitlement, __ChildRawEntitlement),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertHasLinkedAccount(__LinkedAcc),
    ?assert(has_linked_account(Config, UserId, __LinkedAcc))
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
-define(ANOTHER_IDP, anotherIdP).
-define(THIRD_IDP, thirdIdP).

merge_groups_in_linked_accounts_test(Config) ->
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, false, undefined, undefined, undefined),
    overwrite_entitlement_mapping(Config, ?ANOTHER_IDP, false, undefined, undefined, undefined),
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
    ?assertGroupExists(false, ?DUMMY_IDP, flat_entitlement_parser, <<"group/subgroup">>),
    ?assertGroupExists(false, ?DUMMY_IDP, flat_entitlement_parser, <<"anotherGroup">>),
    ?assertGroupExists(false, ?DUMMY_IDP, flat_entitlement_parser, <<"thirdGroup">>),

    % Enable entitlement mapping that uses a flat_entitlement_parser
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, true, flat_entitlement_parser, undefined, undefined),
    MergeAcc(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    ?assertGroupsCount(2, 2),
    ?assertLinkedAccountsCount(1),
    ?assertHasGroup(true, ?DUMMY_IDP, flat_entitlement_parser, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, flat_entitlement_parser, <<"anotherGroup">>, direct),
    ?assertGroupExists(true, ?DUMMY_IDP, flat_entitlement_parser, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, flat_entitlement_parser, <<"anotherGroup">>),
    ?assertGroupExists(false, ?DUMMY_IDP, flat_entitlement_parser, <<"thirdGroup">>),

    % Simulate a situation when a new entitlement appears
    MergeAcc(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>, <<"thirdGroup">>]),
    ?assertGroupsCount(3, 3),
    ?assertLinkedAccountsCount(1),
    ?assertHasGroup(true, ?DUMMY_IDP, flat_entitlement_parser, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, flat_entitlement_parser, <<"anotherGroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, flat_entitlement_parser, <<"thirdGroup">>, direct),
    ?assertGroupExists(true, ?DUMMY_IDP, flat_entitlement_parser, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, flat_entitlement_parser, <<"anotherGroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, flat_entitlement_parser, <<"thirdGroup">>),

    % Simulate a situation when two user entitlements are withdrawn
    MergeAcc(?DUMMY_IDP, [<<"anotherGroup">>]),
    ?assertGroupsCount(1, 1),
    ?assertLinkedAccountsCount(1),
    ?assertHasGroup(false, ?DUMMY_IDP, flat_entitlement_parser, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, flat_entitlement_parser, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, flat_entitlement_parser, <<"thirdGroup">>, direct),
    % The groups should not be removed
    ?assertGroupExists(true, ?DUMMY_IDP, flat_entitlement_parser, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, flat_entitlement_parser, <<"anotherGroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, flat_entitlement_parser, <<"thirdGroup">>),

    % Link a new user account in IdP that has currently disabled entitlement mapping
    MergeAcc(?ANOTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertGroupsCount(1, 1),
    ?assertLinkedAccountsCount(2),
    ?assertHasGroup(false, ?DUMMY_IDP, flat_entitlement_parser, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, flat_entitlement_parser, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, flat_entitlement_parser, <<"thirdGroup">>, direct),
    ?assertHasGroup(false, ?ANOTHER_IDP, nested_entitlement_parser, <<"users/admins">>, direct),
    ?assertHasGroup(false, ?ANOTHER_IDP, nested_entitlement_parser, <<"users/developers">>, direct),
    ?assertHasGroup(false, ?ANOTHER_IDP, nested_entitlement_parser, <<"users/technicians">>, direct),
    ?assertGroupExists(false, ?DUMMY_IDP, nested_entitlement_parser, <<"users/admins">>),
    ?assertGroupExists(false, ?DUMMY_IDP, nested_entitlement_parser, <<"users/developers">>),
    ?assertGroupExists(false, ?DUMMY_IDP, nested_entitlement_parser, <<"users/technicians">>),

    % Turn on entitlement mapping in ?ANOTHER_IDP, with "users/admins" as admin Group
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, true, nested_entitlement_parser, undefined, "users/admins"),
    MergeAcc(?ANOTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertGroupsCount(4, 5),
    ?assertLinkedAccountsCount(2),
    ?assertHasGroup(false, ?DUMMY_IDP, flat_entitlement_parser, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, flat_entitlement_parser, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, flat_entitlement_parser, <<"thirdGroup">>, direct),
    ?assertHasGroup(true, ?ANOTHER_IDP, nested_entitlement_parser, <<"users/admins">>, direct),
    ?assertHasGroup(true, ?ANOTHER_IDP, nested_entitlement_parser, <<"users/developers">>, direct),
    ?assertHasGroup(true, ?ANOTHER_IDP, nested_entitlement_parser, <<"users/technicians">>, direct),
    ?assertHasGroup(true, ?ANOTHER_IDP, nested_entitlement_parser, <<"users">>, effective),
    ?assertGroupExists(true, ?DUMMY_IDP, nested_entitlement_parser, <<"users/admins">>),
    ?assertGroupExists(true, ?DUMMY_IDP, nested_entitlement_parser, <<"users/developers">>),
    ?assertGroupExists(true, ?DUMMY_IDP, nested_entitlement_parser, <<"users/technicians">>),
    ?assertGroupStructure(?ANOTHER_IDP, nested_entitlement_parser, <<"users">>, <<"users/admins">>),
    ?assertGroupStructure(?ANOTHER_IDP, nested_entitlement_parser, <<"users">>, <<"users/developers">>),
    ?assertGroupStructure(?ANOTHER_IDP, nested_entitlement_parser, <<"users">>, <<"users/technicians">>),
    % The admin group should belong to all groups
    ?assertGroupStructure(?ANOTHER_IDP, nested_entitlement_parser, <<"users/developers">>, <<"users/admins">>),
    ?assertGroupStructure(?ANOTHER_IDP, nested_entitlement_parser, <<"users/technicians">>, <<"users/admins">>),

    % Turn off entitlement mapping on the DUMMY_IDP, which should remove related entitlements
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, false, undefined, undefined, undefined),
    MergeAcc(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>, <<"thirdGroup">>]),
    ?assertGroupsCount(3, 4),
    ?assertLinkedAccountsCount(2),
    ?assertHasGroup(false, ?DUMMY_IDP, flat_entitlement_parser, <<"group/subgroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, flat_entitlement_parser, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, flat_entitlement_parser, <<"thirdGroup">>, direct),
    ?assertHasGroup(true, ?ANOTHER_IDP, nested_entitlement_parser, <<"users/admins">>, direct),
    ?assertHasGroup(true, ?ANOTHER_IDP, nested_entitlement_parser, <<"users/developers">>, direct),
    ?assertHasGroup(true, ?ANOTHER_IDP, nested_entitlement_parser, <<"users/technicians">>, direct),
    ?assertHasGroup(true, ?ANOTHER_IDP, nested_entitlement_parser, <<"users">>, effective),


    % @fixme check if all groups are protected

    ok.

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

init_per_testcase(merge_groups_in_linked_accounts_test, Config) ->
    init_per_testcase(default, Config);
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
init_per_testcase(default, Config) ->
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


group_exists(Config, IdP, Parser, RawEntitlement) ->
    IdPEntitlement = Parser:parse(IdP, RawEntitlement, parser_config(Parser)),
    GroupId = entitlement_mapping:gen_group_id(IdPEntitlement),
    oz_test_utils:call_oz(Config, group_logic, exists, [GroupId]).


%% RelationType :: direct | effective
has_group(Config, UserId, IdP, Parser, RawEntitlement, RelationType) ->
    try
        IdPEntitlement = #idp_entitlement{
            idp = IdP,
            path = Path,
            privileges = Privileges
        } = Parser:parse(IdP, RawEntitlement, parser_config(Parser)),
        #idp_group{name = Name, type = Type} = lists:last(Path),
        NormalizedName = entity_logic:normalize_name(Name),
        GroupId = entitlement_mapping:gen_group_id(IdPEntitlement),
        ct:print("IdPEntitlement: ~p", [IdPEntitlement]),
        ct:print("lists:last(Path): ~p", [lists:last(Path)]),
        ct:print("GroupId: ~p", [GroupId]),

        UserGroups = get_groups(Config, UserId, RelationType),
        ct:print("UserGroups: ~p", [UserGroups]),
        BelongsToGroup = lists:member(GroupId, UserGroups),
        {ok, #od_group{
            name = GroupName, type = GroupType
        }} = oz_test_utils:get_group(Config, GroupId),
        NameAndTypeMatch = GroupName =:= NormalizedName andalso GroupType =:= Type,
        ct:print("GroupName: ~p", [GroupName]),
        ct:print("Name: ~p", [Name]),
        ct:print("GroupType: ~p", [GroupType]),
        ct:print("Type: ~p", [Type]),
        ct:print("NameAndTypeMatch: ~p", [NameAndTypeMatch]),
        case BelongsToGroup andalso NameAndTypeMatch of
            false ->
                false;
            true ->
                {ok, UserPrivileges} = case RelationType of
                    direct ->
                        oz_test_utils:call_oz(Config, group_logic, get_user_privileges, [
                            ?ROOT, GroupId, UserId
                        ]);
                    effective ->
                        oz_test_utils:call_oz(Config, group_logic, get_eff_user_privileges, [
                            ?ROOT, GroupId, UserId
                        ])
                end,
                UserPrivileges =:= entitlement_mapping:map_privileges(Privileges)
        end
    catch T:M ->
        ct:print("ERROR: ~p", [{T, M, erlang:get_stacktrace()}]),
        false
    end.


has_groups_count(Config, UserId, ExpDirectCount, ExpEffectiveCount) ->
    DirectCount = length(get_groups(Config, UserId, direct)),
    EffectiveCount = length(get_groups(Config, UserId, effective)),
    EntitlementsCount = length(get_entitlements(Config, UserId)),
    DirectCount =:= ExpDirectCount andalso
        EntitlementsCount =:= ExpDirectCount andalso
        EffectiveCount =:= ExpEffectiveCount.


check_group_structure(Config, IdP, Parser, ParentRawEntitlement, ChildRawEntitlement) ->
    try
        ParentEntitlement = #idp_entitlement{
            idp = IdP,
            path = ParentPath
        } = Parser:parse(IdP, ParentRawEntitlement, parser_config(Parser)),
        ChildEntitlement = #idp_entitlement{
            idp = IdP,
            path = ChildPath
        } = Parser:parse(IdP, ChildRawEntitlement, parser_config(Parser)),
        #idp_group{name = ParentName, type = ParentType} = lists:last(ParentPath),
        #idp_group{name = ChildName, type = ChildType} = lists:last(ChildPath),
        NormalizedParentName = entity_logic:normalize_name(ParentName),
        NormalizedChildName = entity_logic:normalize_name(ChildName),
        % If the child group is the adminGroup, it should have admin privileges in parents
        ChildPrivs = case get_admin_group(Config, IdP) of
            ChildRawEntitlement -> admin;
            _ -> (lists:last(ChildPath))#idp_group.privileges
        end,
        ParentGroupId = entitlement_mapping:gen_group_id(ParentEntitlement),
        ChildGroupId = entitlement_mapping:gen_group_id(ChildEntitlement),
        % Check if names and types of groups are as expected
        {ok, #od_group{
            name = NormalizedParentName, type = ParentType
        }} = oz_test_utils:get_group(Config, ParentGroupId),
        {ok, #od_group{
            name = NormalizedChildName, type = ChildType
        }} = oz_test_utils:get_group(Config, ChildGroupId),
        {ok, ActualChildPrivs} = oz_test_utils:call_oz(
            Config, group_logic, get_child_privileges, [?ROOT, ParentGroupId, ChildGroupId]
        ),
        ActualChildPrivs =:= entitlement_mapping:map_privileges(ChildPrivs)
    catch _:_ ->
        false
    end.



get_groups(Config, UserId, RelationType) ->
    {ok, UserGroups} = case RelationType of
        direct ->
            oz_test_utils:user_get_groups(Config, UserId);
        effective ->
            oz_test_utils:user_get_eff_groups(Config, UserId)
    end,
    UserGroups.


get_entitlements(Config, UserId) ->
    {ok, #od_user{entitlements = Entitlements}} = oz_test_utils:get_user(Config, UserId),
    Entitlements.


has_linked_account(Config, UserId, LinkedAccount) ->
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
    oz_test_utils:call_oz(Config, auth_config, get_entitlement_mapping_config, [
        IdP, [adminGroup], {default, undefined}
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
