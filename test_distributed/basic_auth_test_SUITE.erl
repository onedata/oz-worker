%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018-2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests for features related to basic auth and
%%% password management.
%%% @end
%%%-------------------------------------------------------------------
-module(basic_auth_test_SUITE).
-author("Lukasz Opiola").

-include("http/gui_paths.hrl").
-include("api_test_utils.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    basic_auth_authenticate_test/1,
    basic_auth_endpoint_test/1,
    change_password_test/1,
    set_password_test/1,
    migrate_onepanel_user_to_onezone/1
]).

all() ->
    ?ALL([
        basic_auth_authenticate_test,
        basic_auth_endpoint_test,
        change_password_test,
        set_password_test,
        migrate_onepanel_user_to_onezone
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================

basic_auth_authenticate_test(Config) ->
    Username1 = <<"user1">>,
    Pass1 = <<"password1">>,
    {ok, U1} = oz_test_utils:create_user(Config, #{<<"username">> => Username1, <<"password">> => Pass1}),

    Username2 = <<"user2">>,
    Pass2 = <<"password2">>,
    {ok, U2} = oz_test_utils:create_user(Config, #{<<"username">> => Username2, <<"password">> => Pass2}),

    Authenticate = fun(Username, Password) ->
        oz_test_utils:call_oz(Config, basic_auth, check_basic_auth, [Username, Password])
    end,

    ?assertMatch({true, ?USER(U1)}, Authenticate(Username1, Pass1)),
    ?assertMatch({true, ?USER(U2)}, Authenticate(Username2, Pass2)),

    ?assertMatch(?ERROR_BAD_BASIC_CREDENTIALS, Authenticate(Username1, Pass2)),
    ?assertMatch(?ERROR_BAD_BASIC_CREDENTIALS, Authenticate(Username2, Pass1)),
    ?assertMatch(?ERROR_BAD_BASIC_CREDENTIALS, Authenticate(<<"foo">>, <<"bar">>)),

    oz_test_utils:call_oz(Config, user_logic, toggle_basic_auth, [?ROOT, U1, false]),
    oz_test_utils:call_oz(Config, user_logic, toggle_basic_auth, [?ROOT, U2, false]),
    ?assertMatch(?ERROR_BASIC_AUTH_DISABLED, Authenticate(Username1, Pass1)),
    ?assertMatch(?ERROR_BASIC_AUTH_DISABLED, Authenticate(Username2, Pass2)),

    oz_test_utils:call_oz(Config, user_logic, toggle_basic_auth, [?ROOT, U2, true]),
    ?assertMatch(?ERROR_BASIC_AUTH_DISABLED, Authenticate(Username1, Pass1)),
    ?assertMatch({true, ?USER(U2)}, Authenticate(Username2, Pass2)),

    oz_test_utils:toggle_basic_auth(Config, false),
    ?assertMatch(?ERROR_BASIC_AUTH_NOT_SUPPORTED, Authenticate(Username1, Pass1)),
    ?assertMatch(?ERROR_BASIC_AUTH_NOT_SUPPORTED, Authenticate(Username2, Pass2)),
    ok.


basic_auth_endpoint_test(Config) ->
    Username = <<"user1">>,
    Pass = <<"password1">>,
    {ok, User} = oz_test_utils:create_user(Config, #{<<"username">> => Username, <<"password">> => Pass}),

    Endpoint = oz_test_utils:oz_url(Config, <<?LOGIN_PATH>>),
    UserPasswordB64 = base64:encode(<<Username/binary, ":", Pass/binary>>),
    BasicAuthHeaders = #{
        ?HDR_AUTHORIZATION => <<"Basic ", UserPasswordB64/binary>>
    },
    Opts = [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}],
    Response = http_client:post(Endpoint, BasicAuthHeaders, [], Opts),
    ?assertMatch({ok, 200, _, _}, Response),
    {ok, 200, RespHeaders, _} = Response,
    % Make sure response headers contain cookie with session id - which means
    % that the user has logged in.
    Cookie = maps:get(<<"set-cookie">>, RespHeaders, <<"">>),
    % Cookie: SID=99e4557|4bec19d7; Version=1; Expires=Mon, 13-May-2019 07:46:05 GMT; ...
    CookieKey = ?SESSION_COOKIE_KEY,
    CookieLen = byte_size(?SESSION_COOKIE_KEY),
    ?assertMatch(<<CookieKey:CookieLen/binary, "=", _/binary>>, Cookie),
    [CookieKeyAndValue | _] = binary:split(Cookie, <<";">>),
    % CookieKeyAndValue: SID=99e4557|4bec19d7
    <<CookieKey:CookieLen/binary, "=", CookieValue/binary>> = CookieKeyAndValue,
    % CookieValue: 99e4557|4bec19d7
    SessionId = gui_session:get_session_id(CookieValue),
    ?assertMatch({ok, User}, oz_test_utils:call_oz(Config, session, get_user_id, [SessionId])),

    % If basic auth is disabled for given user, he should not be able to authenticate
    oz_test_utils:call_oz(Config, user_logic, toggle_basic_auth, [?ROOT, User, false]),
    ?assertMatch({ok, 401, _, _}, http_client:post(Endpoint, BasicAuthHeaders, [], Opts)),

    % Try some inexistent user credentials
    WrongUserPasswordB64 = base64:encode(<<"lol:wut">>),
    WrongBasicAuthHeaders = #{
        ?HDR_AUTHORIZATION => <<"Basic ", WrongUserPasswordB64/binary>>
    },
    ?assertMatch({ok, 401, _, _}, http_client:post(Endpoint, WrongBasicAuthHeaders, [], Opts)),

    % Basic auth login should not work if basic auth is disabled in auth.config
    oz_test_utils:toggle_basic_auth(Config, false),
    ?assertMatch({ok, 400, _, _}, http_client:post(Endpoint, BasicAuthHeaders, [], Opts)),
    ?assertMatch({ok, 400, _, _}, http_client:post(Endpoint, WrongBasicAuthHeaders, [], Opts)),

    ok.


change_password_test(Config) ->
    Username1 = <<"user1">>,
    OldPass1 = <<"password1">>,
    NewPass1 = <<"newPass1">>,
    {ok, U1} = oz_test_utils:create_user(Config, #{<<"username">> => Username1, <<"password">> => OldPass1}),

    Authenticate = fun(Username, Password) ->
        oz_test_utils:call_oz(Config, basic_auth, check_basic_auth, [Username, Password])
    end,
    ChangePassword = fun(User, OldPassword, NewPassword) ->
        oz_test_utils:call_oz(Config, user_logic, change_password, [?USER(User), User, OldPassword, NewPassword])
    end,

    ?assertMatch({true, ?USER(U1)}, Authenticate(Username1, OldPass1)),
    ?assertMatch(?ERROR_BAD_BASIC_CREDENTIALS, Authenticate(Username1, NewPass1)),

    ?assertMatch(?ERROR_BAD_BASIC_CREDENTIALS, ChangePassword(U1, <<"asdfsdaf">>, NewPass1)),
    ?assertMatch(?ERROR_BAD_VALUE_PASSWORD, ChangePassword(U1, OldPass1, <<"1">>)),

    ?assertMatch(ok, ChangePassword(U1, OldPass1, NewPass1)),
    ?assertMatch(?ERROR_BAD_BASIC_CREDENTIALS, Authenticate(Username1, OldPass1)),
    ?assertMatch({true, ?USER(U1)}, Authenticate(Username1, NewPass1)),

    % Create second user without basic auth enabled
    Username2 = <<"user2">>,
    FirstPass2 = <<"password2">>,
    SecondPass2 = <<"newPass2">>,
    {ok, U2} = oz_test_utils:create_user(Config, #{<<"username">> => Username2}),

    ?assertMatch(?ERROR_BASIC_AUTH_DISABLED, ChangePassword(U2, undefined, FirstPass2)),
    oz_test_utils:call_oz(Config, user_logic, toggle_basic_auth, [?ROOT, U2, true]),
    ?assertMatch(?ERROR_BAD_BASIC_CREDENTIALS, ChangePassword(U2, <<"123">>, FirstPass2)),
    ?assertMatch(ok, ChangePassword(U2, undefined, FirstPass2)),

    ?assertMatch({true, ?USER(U2)}, Authenticate(Username2, FirstPass2)),
    ?assertMatch(?ERROR_BAD_BASIC_CREDENTIALS, Authenticate(Username2, SecondPass2)),

    ?assertMatch(ok, ChangePassword(U2, FirstPass2, SecondPass2)),
    ?assertMatch(?ERROR_BAD_BASIC_CREDENTIALS, Authenticate(Username2, FirstPass2)),
    ?assertMatch({true, ?USER(U2)}, Authenticate(Username2, SecondPass2)),

    ok.


set_password_test(Config) ->
    Username1 = <<"user1">>,
    NewPass1 = <<"newPass1">>,
    {ok, U1} = oz_test_utils:create_user(Config, #{<<"username">> => Username1}),

    Authenticate = fun(Username, Password) ->
        oz_test_utils:call_oz(Config, basic_auth, check_basic_auth, [Username, Password])
    end,
    SetPassword = fun(User, NewPassword) ->
        oz_test_utils:call_oz(Config, user_logic, set_password, [?ROOT, User, NewPassword])
    end,

    ?assertMatch(?ERROR_BASIC_AUTH_DISABLED, Authenticate(Username1, NewPass1)),
    oz_test_utils:call_oz(Config, user_logic, toggle_basic_auth, [?ROOT, U1, true]),
    % If no password was set, ERROR_BASIC_AUTH_DISABLED is expected
    ?assertMatch(?ERROR_BASIC_AUTH_DISABLED, Authenticate(Username1, NewPass1)),

    ?assertMatch(?ERROR_BAD_VALUE_PASSWORD, SetPassword(U1, <<"1">>)),
    ?assertMatch(ok, SetPassword(U1, NewPass1)),
    ?assertMatch({true, ?USER(U1)}, Authenticate(Username1, NewPass1)),
    ?assertMatch(?ERROR_BAD_BASIC_CREDENTIALS, Authenticate(Username1, <<"bad-pass">>)),
    ok.


migrate_onepanel_user_to_onezone(Config) ->
    Roles = [regular, admin],

    lists:foreach(fun(Role) ->
        OnepanelUserId = str_utils:rand_hex(8),
        OnepanelUsername = str_utils:format_bin("onepanel-~s", [Role]),
        Password = str_utils:format_bin("password-~s", [Role]),
        PasswordHash = onedata_passwords:create_hash(Password),
        GroupMapping = oz_test_utils:get_env(Config, onepanel_role_to_group_mapping),
        ExpectedGroups = maps:get(atom_to_binary(Role, utf8), GroupMapping, []),

        oz_test_utils:call_oz(Config, basic_auth, migrate_onepanel_user_to_onezone, [
            OnepanelUserId, OnepanelUsername, PasswordHash, Role
        ]),

        ExpUserId = basic_auth:onepanel_uid_to_system_uid(OnepanelUserId),
        ?assertMatch({true, ?USER(ExpUserId)}, oz_test_utils:call_oz(Config, basic_auth, check_basic_auth, [
            OnepanelUsername, Password
        ])),

        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

        lists:foreach(fun(Group) ->
            ?assert(oz_test_utils:call_oz(Config, group_logic, has_eff_user, [Group, ExpUserId]))
        end, ExpectedGroups),

        IsInCluster = oz_test_utils:call_oz(Config, cluster_logic, has_eff_user, [
            ?ONEZONE_CLUSTER_ID, ExpUserId
        ]),

        % Only users with admin role should be added to Onezone cluster
        case Role of
            regular -> ?assertNot(IsInCluster);
            admin -> ?assert(IsInCluster)
        end
    end, Roles),

    % Migration should make sure username is not occupied, or change it for
    % the conflicting user (it is more important to migrate onepanel username
    % than retain an alias, as the username is used for signing in).
    oz_test_utils:delete_all_entities(Config),
    {ok, ConflictingUser} = oz_test_utils:create_user(Config, #{<<"username">> => <<"admin">>}),

    {ok, AdminUserId} = oz_test_utils:call_oz(Config, basic_auth, migrate_onepanel_user_to_onezone, [
        datastore_utils:gen_key(), <<"admin">>, onedata_passwords:create_hash(<<"1234">>), admin
    ]),

    {ok, AllUsers} = oz_test_utils:list_users(Config),
    ?assertEqual(lists:sort([ConflictingUser, AdminUserId]), lists:sort(AllUsers)),

    ?assertMatch(
        {ok, #document{key = AdminUserId, value = #od_user{username = <<"admin">>}}},
        oz_test_utils:call_oz(Config, od_user, get_by_username, [<<"admin">>])
    ),

    ?assertMatch(
        {ok, #document{key = ConflictingUser, value = #od_user{username = undefined}}},
        oz_test_utils:call_oz(Config, od_user, get, [ConflictingUser])
    ),

    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(_, Config) ->
    oz_test_utils:toggle_basic_auth(Config, true),
    Config.

end_per_testcase(_, Config) ->
    oz_test_utils:delete_all_entities(Config),
    ok.

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().
