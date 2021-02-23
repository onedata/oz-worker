%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This test SUITE checks various way of authorizing a REST request.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_auth_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("auth/auth_errors.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-include("api_test_utils.hrl").


-define(CORRECT_USERNAME, <<"user-username">>).
-define(CORRECT_PASSWORD, <<"user-passwd">>).
-define(BAD_USERNAME, <<"bad-username">>).
-define(BAD_PASSWORD, <<"bad-passwd">>).


%% API
-export([all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    access_token_test/1,
    basic_auth_test/1,
    external_access_token_test/1,
    gui_token_test/1
]).

all() ->
    ?ALL([
        access_token_test,
        basic_auth_test,
        external_access_token_test,
        gui_token_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

% Check access token based authorization
access_token_test(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #{<<"fullName">> => <<"U1">>}),

    {ok, Token} = oz_test_utils:create_client_token(
        Config, UserId
    ),

    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => Token},
        {ok, #{<<"fullName">> => <<"U1">>}}
    )),

    ?assert(check_rest_call(Config,
        #{?HDR_AUTHORIZATION => <<"Bearer ", Token/binary>>},
        {ok, #{<<"fullName">> => <<"U1">>}}
    )),

    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => <<"bad-token">>},
        ?ERROR_BAD_TOKEN
    )),

    % check if user blocking works as expected
    oz_test_utils:toggle_user_access_block(Config, UserId, true),
    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => Token},
        ?ERROR_USER_BLOCKED
    )),

    % after unblocking, authentication should work again
    oz_test_utils:toggle_user_access_block(Config, UserId, false),
    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => Token},
        {ok, #{<<"fullName">> => <<"U1">>}}
    )).


basic_auth_test(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #{
        <<"username">> => ?CORRECT_USERNAME,
        <<"password">> => ?CORRECT_PASSWORD
    }),

    oz_test_utils:toggle_basic_auth(Config, true),

    ?assert(check_rest_call(Config,
        basic_auth_header(?CORRECT_USERNAME, ?CORRECT_PASSWORD),
        {ok, #{<<"userId">> => UserId, <<"username">> => ?CORRECT_USERNAME}}
    )),

    ?assert(check_rest_call(Config,
        basic_auth_header(?BAD_USERNAME, ?BAD_PASSWORD),
        ?ERROR_BAD_BASIC_CREDENTIALS
    )),

    % check if user blocking works as expected
    oz_test_utils:toggle_user_access_block(Config, UserId, true),
    ?assert(check_rest_call(Config,
        basic_auth_header(?CORRECT_USERNAME, ?CORRECT_PASSWORD),
        ?ERROR_USER_BLOCKED
    )),

    % after unblocking, authentication should work again
    oz_test_utils:toggle_user_access_block(Config, UserId, false),
    ?assert(check_rest_call(Config,
        basic_auth_header(?CORRECT_USERNAME, ?CORRECT_PASSWORD),
        {ok, #{<<"userId">> => UserId, <<"username">> => ?CORRECT_USERNAME}}
    )),

    % REST authorization by credentials should not work if basic auth is disabled in auth.config
    oz_test_utils:toggle_basic_auth(Config, false),
    ?assert(check_rest_call(Config,
        basic_auth_header(?CORRECT_USERNAME, ?CORRECT_PASSWORD),
        ?ERROR_BASIC_AUTH_NOT_SUPPORTED
    )),
    ?assert(check_rest_call(Config,
        basic_auth_header(?BAD_USERNAME, ?BAD_PASSWORD),
        ?ERROR_BASIC_AUTH_NOT_SUPPORTED
    )).


external_access_token_test(Config) ->
    % auth.config and authorization procedure are mocked in init_per_testcase
    DummyIdP = ?config(dummyIdP, Config),
    AnotherIdP = ?config(anotherIdP, Config),
    DisabledIdP = ?config(disabledIdP, Config),
    UserSubjectIdFun = ?config(user_subject_id_fun, Config),
    UserFullNameFun = ?config(user_full_name_fun, Config),
    CorrectAccessTokenFun = ?config(correct_access_token_fun, Config),
    PrefixFun = ?config(prefix_fun, Config),

    UserIdFun = fun(IdP) ->
        oz_test_utils:call_oz(Config, linked_accounts, gen_user_id, [IdP, UserSubjectIdFun(IdP)])
    end,

    XAuthTokenFun = fun(IdP) ->
        <<(PrefixFun(IdP))/binary, (CorrectAccessTokenFun(IdP))/binary>>
    end,

    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => XAuthTokenFun(DummyIdP)},
        {ok, #{<<"userId">> => UserIdFun(DummyIdP), <<"fullName">> => UserFullNameFun(DummyIdP)}}
    )),

    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => XAuthTokenFun(AnotherIdP)},
        {ok, #{<<"userId">> => UserIdFun(AnotherIdP), <<"fullName">> => UserFullNameFun(AnotherIdP)}}
    )),

    % check if user blocking works as expected
    oz_test_utils:toggle_user_access_block(Config, UserIdFun(DummyIdP), true),
    oz_test_utils:toggle_user_access_block(Config, UserIdFun(AnotherIdP), true),
    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => XAuthTokenFun(DummyIdP)},
        ?ERROR_USER_BLOCKED
    )),
    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => XAuthTokenFun(AnotherIdP)},
        ?ERROR_USER_BLOCKED
    )),

    % after unblocking, authentication should work again
    oz_test_utils:toggle_user_access_block(Config, UserIdFun(DummyIdP), false),
    oz_test_utils:toggle_user_access_block(Config, UserIdFun(AnotherIdP), false),
    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => XAuthTokenFun(DummyIdP)},
        {ok, #{<<"userId">> => UserIdFun(DummyIdP), <<"fullName">> => UserFullNameFun(DummyIdP)}}
    )),
    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => XAuthTokenFun(AnotherIdP)},
        {ok, #{<<"userId">> => UserIdFun(AnotherIdP), <<"fullName">> => UserFullNameFun(AnotherIdP)}}
    )),

    % DisabledIdP has disabled authority delegation
    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => XAuthTokenFun(DisabledIdP)},
        ?ERROR_BAD_TOKEN
    )),

    % prefix from DummyIdP, access token from AnotherIdP
    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => <<(PrefixFun(DummyIdP))/binary, (CorrectAccessTokenFun(AnotherIdP))/binary>>},
        ?ERROR_BAD_IDP_ACCESS_TOKEN(DummyIdP)
    )),

    ?assert(check_rest_call(Config,
        #{?HDR_X_AUTH_TOKEN => <<"completely-bad-token">>},
        ?ERROR_BAD_TOKEN
    )).


gui_token_test(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #{<<"fullName">> => <<"U1">>}),
    {ok, {Provider1, _}} = oz_test_utils:create_provider(
        Config, UserId, ?UNIQUE_STRING
    ),
    {ok, {Provider2, _}} = oz_test_utils:create_provider(
        Config, UserId, ?UNIQUE_STRING
    ),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space_by_provider(Config, Provider1, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    {ok, {SessionId, Cookie}} = oz_test_utils:log_in(Config, UserId),

    {ok, {GuiToken1, Ttl}} = oz_test_utils:call_oz(Config, token_logic, create_access_token_for_gui, [
        ?USER(UserId), UserId, SessionId, ?SERVICE(?OP_WORKER, Provider1)
    ]),
    {ok, SerializedGuiToken1} = tokens:serialize(GuiToken1),

    {ok, {GuiToken2, Ttl}} = oz_test_utils:call_oz(Config, token_logic, create_access_token_for_gui, [
        ?USER(UserId), UserId, SessionId, ?SERVICE(?OP_PANEL, Provider1)
    ]),
    {ok, SerializedGuiToken2} = tokens:serialize(GuiToken2),

    {ok, Provider1IdentityToken} = oz_test_utils:call_oz(Config, token_logic, create_provider_named_token, [
        ?PROVIDER(Provider1), Provider1, #{<<"name">> => datastore_key:new(), <<"type">> => ?IDENTITY_TOKEN}
    ]),
    {ok, Provider2IdentityToken} = oz_test_utils:call_oz(Config, token_logic, create_provider_named_token, [
        ?PROVIDER(Provider2), Provider2, #{<<"name">> => datastore_key:new(), <<"type">> => ?IDENTITY_TOKEN}
    ]),
    Provider1ServiceToken = tokens:add_oneprovider_service_indication(?OP_WORKER, ozt_tokens:ensure_serialized(Provider1IdentityToken)),
    Provider2ServiceToken = tokens:add_oneprovider_service_indication(?OP_PANEL, ozt_tokens:ensure_serialized(Provider2IdentityToken)),

    ?assert(check_rest_call(Config,
        #{
            ?HDR_X_AUTH_TOKEN => SerializedGuiToken1,
            ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>,
            ?HDR_X_ONEDATA_SERVICE_TOKEN => Provider1ServiceToken
        },
        {ok, #{<<"fullName">> => <<"U1">>}}
    )),

    % no session cookie
    ?assert(check_rest_call(Config,
        #{
            ?HDR_X_AUTH_TOKEN => SerializedGuiToken1,
            ?HDR_X_ONEDATA_SERVICE_TOKEN => Provider1ServiceToken
        },
        ?ERROR_TOKEN_SESSION_INVALID
    )),

    % invalid service
    ?assert(check_rest_call(Config,
        #{
            ?HDR_AUTHORIZATION => <<"Bearer ", SerializedGuiToken2/binary>>,
            ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>,
            ?HDR_X_ONEDATA_SERVICE_TOKEN => Provider2ServiceToken
        },
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_service{whitelist = [?SERVICE(?OP_PANEL, Provider1)]})
    )),

    % bad service token
    ?assert(check_rest_call(Config,
        #{
            ?HDR_AUTHORIZATION => <<"Bearer ", SerializedGuiToken2/binary>>,
            ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>,
            ?HDR_X_ONEDATA_SERVICE_TOKEN => SerializedGuiToken1
        },
        ?ERROR_BAD_SERVICE_TOKEN(?ERROR_NOT_AN_IDENTITY_TOKEN(?ACCESS_TOKEN(SessionId)))
    )),

    % missing service token
    ?assert(check_rest_call(Config,
        #{
            ?HDR_X_AUTH_TOKEN => SerializedGuiToken1,
            ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>
        },
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_service{whitelist = [?SERVICE(?OP_WORKER, Provider1)]})
    )),

    % check if user blocking works as expected
    oz_test_utils:toggle_user_access_block(Config, UserId, true),
    ?assert(check_rest_call(Config,
        #{
            ?HDR_X_AUTH_TOKEN => SerializedGuiToken1,
            ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>,
            ?HDR_X_ONEDATA_SERVICE_TOKEN => Provider1ServiceToken
        },
        ?ERROR_USER_BLOCKED
    )),

    % blocking the user causes the session to be deleted - despite unblocking,
    % the token should no longer work
    oz_test_utils:toggle_user_access_block(Config, UserId, false),
    ?assert(check_rest_call(Config,
        #{
            ?HDR_X_AUTH_TOKEN => SerializedGuiToken1,
            ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>,
            ?HDR_X_ONEDATA_SERVICE_TOKEN => Provider1ServiceToken
        },
        ?ERROR_TOKEN_SESSION_INVALID
    )),

    % after unblocking and with a new session and token, authentication should work again
    {ok, {NewSessionId, NewCookie}} = oz_test_utils:log_in(Config, UserId),
    {ok, {GuiToken3, Ttl}} = oz_test_utils:call_oz(Config, token_logic, create_access_token_for_gui, [
        ?USER(UserId), UserId, NewSessionId, ?SERVICE(?OP_WORKER, Provider1)
    ]),
    {ok, SerializedGuiToken3} = tokens:serialize(GuiToken3),
    ?assert(check_rest_call(Config,
        #{
            ?HDR_X_AUTH_TOKEN => SerializedGuiToken3,
            ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", NewCookie/binary>>,
            ?HDR_X_ONEDATA_SERVICE_TOKEN => Provider1ServiceToken
        },
        {ok, #{<<"fullName">> => <<"U1">>}}
    )),

    % non-matching session cookie
    ?assert(check_rest_call(Config,
        #{
            ?HDR_X_AUTH_TOKEN => SerializedGuiToken3,
            ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>,  % old cookie
            ?HDR_X_ONEDATA_SERVICE_TOKEN => Provider1ServiceToken
        },
        ?ERROR_TOKEN_SESSION_INVALID
    )),

    % expired token
    oz_test_utils:simulate_seconds_passing(Ttl + 1),
    ?assert(check_rest_call(Config,
        #{
            ?HDR_X_AUTH_TOKEN => SerializedGuiToken3,
            ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>,
            ?HDR_X_ONEDATA_SERVICE_TOKEN => Provider1ServiceToken
        },
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = oz_test_utils:timestamp_seconds(Config) - 1})
    )).

%%%===================================================================
%%% Helper functions
%%%===================================================================

basic_auth_header(Username, Password) ->
    UserPasswdB64 = base64:encode(<<Username/binary, ":", Password/binary>>),
    #{?HDR_AUTHORIZATION => <<"Basic ", UserPasswdB64/binary>>}.


check_rest_call(Config, Headers, ExpResult) ->
    Expect = case ExpResult of
        {ok, BodyContains} ->
            #{
                code => 200,
                body => {contains, BodyContains}
            };
        {error, _} = Error ->
            % during encoding to json, atoms are converted to strings and the
            % payload can slightly change - do the same to match the REST responses
            ExpErrorJson = json_utils:decode(json_utils:encode(errors:to_json(?ERROR_UNAUTHORIZED(Error)))),
            #{
                code => 401,
                body => #{<<"error">> => ExpErrorJson}
            }
    end,
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => Headers
        },
        expect => Expect
    }).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


init_per_testcase(external_access_token_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),

    DummyIdP = dummyIdP,
    AnotherIdP = anotherIdP,
    DisabledIdP = disabledIdP,
    CorrectAccessTokenFun = fun
        (dummyIdP) -> <<"correct-access-token-dummyIdP">>;
        (anotherIdP) -> <<"correct-access-token-anotherIdP">>;
        (disabledIdP) -> <<"correct-access-token-disabledIdP">>
    end,
    PrefixFun = fun
        (dummyIdP) -> <<"dummyIdP/">>;
        (anotherIdP) -> <<"anotherIdP#">>;
        (disabledIdP) -> <<"disabledIdP:">>
    end,
    UserSubjectIdFun = fun
        (dummyIdP) -> <<"user1subjectId">>;
        (anotherIdP) -> <<"user2subjectId">>;
        (disabledIdP) -> <<"user3subjectId">>
    end,
    UserFullNameFun = fun
        (dummyIdP) -> <<"User1">>;
        (anotherIdP) -> <<"User2">>;
        (disabledIdP) -> <<"User3">>
    end,

    oz_test_utils:overwrite_auth_config(Config, #{
        openidConfig => #{
            enabled => true
        },
        supportedIdps => [
            {DummyIdP, #{
                protocol => openid,
                protocolConfig => #{
                    plugin => default_oidc_plugin,
                    authorityDelegation => #{
                        enabled => true,
                        tokenPrefix => binary_to_list(PrefixFun(DummyIdP))
                    },
                    attributeMapping => #{
                        subjectId => {required, "id"},
                        fullName => {required, "name"}
                    }
                }
            }},
            {AnotherIdP, #{
                protocol => openid,
                protocolConfig => #{
                    plugin => default_oidc_plugin,
                    authorityDelegation => #{
                        enabled => true,
                        tokenPrefix => binary_to_list(PrefixFun(AnotherIdP))
                    },
                    attributeMapping => #{
                        subjectId => {required, "id"},
                        fullName => {required, "name"}
                    }
                }
            }},
            {DisabledIdP, #{
                protocol => openid,
                protocolConfig => #{
                    plugin => default_oidc_plugin,
                    authorityDelegation => #{
                        enabled => false,
                        tokenPrefix => binary_to_list(PrefixFun(DisabledIdP))
                    },
                    attributeMapping => #{
                        subjectId => {required, "id"},
                        fullName => {required, "name"}
                    }
                }
            }}
        ]
    }),

    ok = test_utils:mock_new(Nodes, default_oidc_plugin, [passthrough]),
    ok = test_utils:mock_expect(Nodes, default_oidc_plugin, get_user_info, fun(IdP, AccessToken) ->
        case AccessToken =:= CorrectAccessTokenFun(IdP) of
            true ->
                {ok, #{<<"id">> => UserSubjectIdFun(IdP), <<"name">> => UserFullNameFun(IdP)}};
            false ->
                throw(?ERROR_BAD_IDP_RESPONSE(IdP, 401, #{}, <<>>))

        end
    end),

    [
        {dummyIdP, DummyIdP},
        {anotherIdP, AnotherIdP},
        {disabledIdP, DisabledIdP},
        {user_subject_id_fun, UserSubjectIdFun},
        {user_full_name_fun, UserFullNameFun},
        {correct_access_token_fun, CorrectAccessTokenFun},
        {prefix_fun, PrefixFun} |
        Config
    ];

init_per_testcase(gui_token_test, Config) ->
    oz_test_utils:freeze_time(Config),
    Config;

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(external_access_token_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, default_oidc_plugin);

end_per_testcase(gui_token_test, Config) ->
    oz_test_utils:unfreeze_time(Config);

end_per_testcase(_, _) ->
    ok.
