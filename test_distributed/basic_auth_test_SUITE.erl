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
    basic_auth_authenticate/1,
    user_access_block/1,
    change_password/1,
    set_password/1,
    migrate_onepanel_user_to_onezone/1
]).

all() ->
    ?ALL([
        basic_auth_authenticate,
        user_access_block,
        change_password,
        set_password,
        migrate_onepanel_user_to_onezone
    ]).

-define(assertUnauthorized(AuthError, Term), ?assertEqual(?ERROR_UNAUTHORIZED(AuthError), Term)).

%%%===================================================================
%%% Test functions
%%%===================================================================

basic_auth_authenticate(Config) ->
    Username1 = <<"user1">>,
    Pass1 = <<"password1">>,
    {ok, U1} = oz_test_utils:create_user(Config, #{<<"username">> => Username1, <<"password">> => Pass1}),

    Username2 = <<"user2">>,
    Pass2 = <<"password2">>,
    {ok, U2} = oz_test_utils:create_user(Config, #{<<"username">> => Username2, <<"password">> => Pass2}),

    ?assertMatch({true, ?USER(U1)}, authenticate(Config, Username1, Pass1)),
    ?assertMatch({true, ?USER(U2)}, authenticate(Config, Username2, Pass2)),
    ?assertEqual({ok, U1}, authenticate_by_http_endpoint(Config, Username1, Pass1)),
    ?assertEqual({ok, U2}, authenticate_by_http_endpoint(Config, Username2, Pass2)),

    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, authenticate(Config, Username1, Pass2)),
    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, authenticate(Config, Username2, Pass1)),
    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, authenticate_by_http_endpoint(Config, Username1, Pass2)),
    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, authenticate_by_http_endpoint(Config, Username2, Pass1)),

    % Try some inexistent user credentials
    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, authenticate(Config, <<"foo">>, <<"bar">>)),
    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, authenticate_by_http_endpoint(Config, <<"foo">>, <<"bar">>)),

    % If basic auth is disabled for given user, he should not be able to authenticate
    toggle_basic_auth(Config, U1, false),
    toggle_basic_auth(Config, U2, false),
    ?assertUnauthorized(?ERROR_BASIC_AUTH_DISABLED, authenticate(Config, Username1, Pass1)),
    ?assertUnauthorized(?ERROR_BASIC_AUTH_DISABLED, authenticate(Config, Username2, Pass2)),
    ?assertUnauthorized(?ERROR_BASIC_AUTH_DISABLED, authenticate_by_http_endpoint(Config, Username1, Pass1)),
    ?assertUnauthorized(?ERROR_BASIC_AUTH_DISABLED, authenticate_by_http_endpoint(Config, Username2, Pass2)),

    toggle_basic_auth(Config, U2, true),
    ?assertUnauthorized(?ERROR_BASIC_AUTH_DISABLED, authenticate(Config, Username1, Pass1)),
    ?assertUnauthorized(?ERROR_BASIC_AUTH_DISABLED, authenticate_by_http_endpoint(Config, Username1, Pass1)),
    ?assertMatch({true, ?USER(U2)}, authenticate(Config, Username2, Pass2)),
    ?assertEqual({ok, U2}, authenticate_by_http_endpoint(Config, Username2, Pass2)),

    oz_test_utils:toggle_basic_auth(Config, false),
    ?assertUnauthorized(?ERROR_BASIC_AUTH_NOT_SUPPORTED, authenticate(Config, Username1, Pass1)),
    ?assertUnauthorized(?ERROR_BASIC_AUTH_NOT_SUPPORTED, authenticate(Config, Username2, Pass2)),
    ?assertUnauthorized(?ERROR_BASIC_AUTH_NOT_SUPPORTED, authenticate_by_http_endpoint(Config, Username1, Pass1)),
    ?assertUnauthorized(?ERROR_BASIC_AUTH_NOT_SUPPORTED, authenticate_by_http_endpoint(Config, Username2, Pass2)).


user_access_block(Config) ->
    Username = <<"user7">>,
    Pass = <<"password7">>,
    {ok, User} = oz_test_utils:create_user(Config, #{<<"username">> => Username, <<"password">> => Pass}),

    ?assertMatch({true, ?USER(User)}, authenticate(Config, Username, Pass)),
    ?assertEqual({ok, User}, authenticate_by_http_endpoint(Config, Username, Pass)),

    oz_test_utils:toggle_user_access_block(Config, User, true),
    ?assertUnauthorized(?ERROR_USER_BLOCKED, authenticate(Config, Username, Pass)),
    ?assertUnauthorized(?ERROR_USER_BLOCKED, authenticate_by_http_endpoint(Config, Username, Pass)),

    oz_test_utils:toggle_user_access_block(Config, User, false),
    ?assertMatch({true, ?USER(User)}, authenticate(Config, Username, Pass)),
    ?assertEqual({ok, User}, authenticate_by_http_endpoint(Config, Username, Pass)).


change_password(Config) ->
    Username1 = <<"user1">>,
    OldPass1 = <<"password1">>,
    NewPass1 = <<"newPass1">>,
    {ok, U1} = oz_test_utils:create_user(Config, #{<<"username">> => Username1, <<"password">> => OldPass1}),

    ?assertMatch({true, ?USER(U1)}, authenticate(Config, Username1, OldPass1)),
    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, authenticate(Config, Username1, NewPass1)),

    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, change_password(Config, U1, <<"asdfsdaf">>, NewPass1)),
    ?assertMatch(?ERROR_BAD_VALUE_PASSWORD, change_password(Config, U1, OldPass1, <<"1">>)),

    ?assertMatch(ok, change_password(Config, U1, OldPass1, NewPass1)),
    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, authenticate(Config, Username1, OldPass1)),
    ?assertMatch({true, ?USER(U1)}, authenticate(Config, Username1, NewPass1)),

    % Create second user without basic auth enabled
    Username2 = <<"user2">>,
    FirstPass2 = <<"password2">>,
    SecondPass2 = <<"newPass2">>,
    {ok, U2} = oz_test_utils:create_user(Config, #{<<"username">> => Username2}),

    ?assertUnauthorized(?ERROR_BASIC_AUTH_DISABLED, change_password(Config, U2, undefined, FirstPass2)),
    toggle_basic_auth(Config, U2, true),
    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, change_password(Config, U2, <<"123">>, FirstPass2)),
    ?assertMatch(ok, change_password(Config, U2, undefined, FirstPass2)),

    ?assertMatch({true, ?USER(U2)}, authenticate(Config, Username2, FirstPass2)),
    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, authenticate(Config, Username2, SecondPass2)),

    ?assertMatch(ok, change_password(Config, U2, FirstPass2, SecondPass2)),
    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, authenticate(Config, Username2, FirstPass2)),
    ?assertMatch({true, ?USER(U2)}, authenticate(Config, Username2, SecondPass2)),

    ok.


set_password(Config) ->
    Username1 = <<"user1">>,
    NewPass1 = <<"newPass1">>,
    {ok, U1} = oz_test_utils:create_user(Config, #{<<"username">> => Username1}),

    ?assertUnauthorized(?ERROR_BASIC_AUTH_DISABLED, authenticate(Config, Username1, NewPass1)),
    toggle_basic_auth(Config, U1, true),
    % If no password was set, ERROR_BASIC_AUTH_DISABLED is expected
    ?assertUnauthorized(?ERROR_BASIC_AUTH_DISABLED, authenticate(Config, Username1, NewPass1)),

    ?assertMatch(?ERROR_BAD_VALUE_PASSWORD, set_password(Config, U1, <<"1">>)),
    ?assertMatch(ok, set_password(Config, U1, NewPass1)),
    ?assertMatch({true, ?USER(U1)}, authenticate(Config, Username1, NewPass1)),
    ?assertUnauthorized(?ERROR_BAD_BASIC_CREDENTIALS, authenticate(Config, Username1, <<"bad-pass">>)),
    ok.


migrate_onepanel_user_to_onezone(Config) ->
    Roles = [regular, admin],

    lists:foreach(fun(Role) ->
        OnepanelUserId = str_utils:rand_hex(16),
        OnepanelUsername = str_utils:format_bin("onepanel-~ts", [Role]),
        Password = str_utils:format_bin("password-~ts", [Role]),
        PasswordHash = onedata_passwords:create_hash(Password),
        GroupMapping = oz_test_utils:get_env(Config, onepanel_role_to_group_mapping),
        ExpectedGroups = maps:get(atom_to_binary(Role, utf8), GroupMapping, []),

        oz_test_utils:call_oz(Config, basic_auth, migrate_onepanel_user_to_onezone, [
            OnepanelUserId, OnepanelUsername, PasswordHash, Role
        ]),

        ExpUserId = oz_test_utils:call_oz(Config, basic_auth, onepanel_uid_to_system_uid, [OnepanelUserId]),
        ?assertMatch({true, ?USER(ExpUserId)}, oz_test_utils:call_oz(Config, basic_auth, authenticate, [
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
        str_utils:rand_hex(16), <<"admin">>, onedata_passwords:create_hash(<<"1234">>), admin
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
    application:ensure_all_started(hackney),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(_, Config) ->
    oz_test_utils:toggle_basic_auth(Config, true),
    Config.

end_per_testcase(_, Config) ->
    oz_test_utils:delete_all_entities(Config),
    ok.

end_per_suite(_Config) ->
    application:stop(hackney),
    ssl:stop().

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
authenticate(Config, Username, Password) ->
    oz_test_utils:call_oz(Config, basic_auth, authenticate, [Username, Password]).


%% @private
set_password(Config, User, NewPassword) ->
    oz_test_utils:call_oz(Config, user_logic, set_password, [?ROOT, User, NewPassword]).


%% @private
change_password(Config, User, OldPassword, NewPassword) ->
    oz_test_utils:call_oz(Config, user_logic, change_password, [?USER(User), User, OldPassword, NewPassword]).


%% @private
toggle_basic_auth(Config, User, Enabled) ->
    oz_test_utils:call_oz(Config, user_logic, toggle_basic_auth, [?ROOT, User, Enabled]).


%% @private
authenticate_by_http_endpoint(Config, Username, Password) ->
    Endpoint = oz_test_utils:oz_url(Config, <<?LOGIN_PATH>>),
    UserPasswordB64 = base64:encode(<<Username/binary, ":", Password/binary>>),
    BasicAuthHeaders = #{?HDR_AUTHORIZATION => <<"Basic ", UserPasswordB64/binary>>},
    Opts = [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}],
    case http_client:post(Endpoint, BasicAuthHeaders, [], Opts) of
        {ok, 200, #{?HDR_SET_COOKIE := SetCookieHeader}, _} ->
            % make sure response headers contain cookie with session id - which means that the user has logged in
            SessionCookie = oz_test_utils:parse_resp_session_cookie(SetCookieHeader),
            {ok, SessionId} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
                Config, gui_session, peek_session_id, [SessionCookie]
            )),
            oz_test_utils:call_oz(Config, session, get_user_id, [SessionId]);
        {ok, _, _, RespBody} ->
            errors:from_json(maps:get(<<"error">>, json_utils:decode(RespBody)))
    end.