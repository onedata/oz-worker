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
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").


-define(CORRECT_LOGIN, <<"user-login">>).
-define(CORRECT_PASSWORD, <<"user-passwd">>).
-define(BAD_LOGIN, <<"bad-login">>).
-define(BAD_PASSWORD, <<"bad-passwd">>).


%% API
-export([all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    macaroon_test/1,
    basic_auth_test/1,
    external_access_token_test/1
]).

all() ->
    ?ALL([
        macaroon_test,
        basic_auth_test,
        external_access_token_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

% Check macaroon based authorization
macaroon_test(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #od_user{name = <<"U1">>}),

    {ok, Macaroon} = oz_test_utils:create_client_token(
        Config, UserId
    ),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"Macaroon">> => Macaroon}
        },
        expect => #{
            code => 200,
            body => {contains, #{<<"name">> => <<"U1">>}}
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"X-Auth-Token">> => Macaroon}
        },
        expect => #{
            code => 200,
            body => {contains, #{<<"name">> => <<"U1">>}}
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"macaroon">> => <<"bad-macaroon">>}
        },
        expect => #{
            code => 401
        }
    })),

    ok.


basic_auth_test(Config) ->
    % Basic auth procedure is mocked in init_per_testcase,
    % and user is initialized there
    UserId = ?config(user_id, Config),
    oz_test_utils:toggle_basic_auth(Config, true),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => basic_auth_header(?CORRECT_LOGIN, ?CORRECT_PASSWORD)
        },
        expect => #{
            code => 200,
            body => {contains, #{
                <<"userId">> => UserId, <<"name">> => ?CORRECT_LOGIN
            }}
        }
    })),
    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => basic_auth_header(?BAD_LOGIN, ?BAD_PASSWORD)
        },
        expect => #{
            code => 401
        }
    })),

    % REST authorization by credentials should not work if basic auth is disabled in auth.config
    oz_test_utils:toggle_basic_auth(Config, false),
    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => basic_auth_header(?CORRECT_LOGIN, ?CORRECT_PASSWORD)
        },
        expect => #{
            code => 400
        }
    })),
    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => basic_auth_header(?BAD_LOGIN, ?BAD_PASSWORD)
        },
        expect => #{
            code => 400
        }
    })),

    ok.


external_access_token_test(Config) ->
    % auth.config and authorization procedure are mocked in init_per_testcase
    DummyIdP = ?config(dummyIdP, Config),
    AnotherIdP = ?config(anotherIdP, Config),
    DisabledIdP = ?config(disabledIdP, Config),
    UserSubjectIdFun = ?config(user_subject_id_fun, Config),
    UserNameFun = ?config(user_name_fun, Config),
    CorrectAccessTokenFun = ?config(correct_access_token_fun, Config),
    PrefixFun = ?config(prefix_fun, Config),

    UserIdFun = fun(IdP) ->
        user_logic:idp_uid_to_system_uid(IdP, UserSubjectIdFun(IdP))
    end,

    XAuthTokenFun = fun(IdP) ->
        <<(PrefixFun(IdP))/binary, (CorrectAccessTokenFun(IdP))/binary>>
    end,

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"X-Auth-Token">> => XAuthTokenFun(DummyIdP)}
        },
        expect => #{
            code => 200,
            body => {contains, #{
                <<"userId">> => UserIdFun(DummyIdP),
                <<"name">> => UserNameFun(DummyIdP)
            }}
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"X-Auth-Token">> => XAuthTokenFun(AnotherIdP)}
        },
        expect => #{
            code => 200,
            body => {contains, #{
                <<"userId">> => UserIdFun(AnotherIdP),
                <<"name">> => UserNameFun(AnotherIdP)
            }}
        }
    })),

    % DisabledIdP has disabled authority delegation
    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"X-Auth-Token">> => XAuthTokenFun(DisabledIdP)}
        },
        expect => #{
            code => 401
        }
    })),

    % prefix from DummyIdP, access token from AnotherIdP
    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"X-Auth-Token">> => <<
                (PrefixFun(DummyIdP))/binary, (CorrectAccessTokenFun(AnotherIdP))/binary
            >>}
        },
        expect => #{
            code => 401
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"X-Auth-Token">> => <<"completely-bad-token">>}
        },
        expect => #{
            code => 401
        }
    })),

    ok.


basic_auth_header(Login, Password) ->
    UserAndPassword = base64:encode(<<Login/binary, ":", Password/binary>>),
    #{<<"Authorization">> => <<"Basic ", UserAndPassword/binary>>}.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) ->
        % Sleep a while before mocking http_client (which is done in
        % init_per_testcase) - otherwise meck's reloading and purging the module
        % can cause the oz-worker application to crash.
        timer:sleep(5000),
        NewConfig
    end,
    [{env_up_posthook, Posthook}, {?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


init_per_testcase(basic_auth_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    OnepanelUserId = <<"user1Id">>,
    UserId = user_logic:onepanel_uid_to_system_uid(OnepanelUserId),

    ok = test_utils:mock_new(Nodes, http_client, [passthrough]),
    test_utils:mock_expect(Nodes, http_client, get, fun(Url, Headers, Body, Options) ->
        case binary:match(Url, <<"9443/api/v3/onepanel/users">>) of
            nomatch ->
                meck:passthrough([Url, Headers, Body, Options]);
            _ ->
                <<"Basic ", UserAndPassword/binary>> =
                    maps:get(<<"Authorization">>, Headers),
                [User, Passwd] =
                    binary:split(base64:decode(UserAndPassword), <<":">>),
                case {User, Passwd} of
                    {?CORRECT_LOGIN, ?CORRECT_PASSWORD} ->
                        ResponseBody = json_utils:encode(#{
                            <<"userId">> => OnepanelUserId,
                            <<"userRole">> => <<"user1Role">>
                        }),
                        {ok, 200, #{}, ResponseBody};
                    _ ->
                        {ok, 401, #{}, <<"">>}
                end
        end
    end),
    [{user_id, UserId} | Config];

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
    UserNameFun = fun
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
                        name => {required, "name"}
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
                        name => {required, "name"}
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
                        name => {required, "name"}
                    }
                }
            }}
        ]
    }),

    ok = test_utils:mock_new(Nodes, default_oidc_plugin, [passthrough]),
    ok = test_utils:mock_expect(Nodes, default_oidc_plugin, get_user_info, fun(IdP, AccessToken) ->
        case AccessToken =:= CorrectAccessTokenFun(IdP) of
            true ->
                {ok, #{<<"id">> => UserSubjectIdFun(IdP), <<"name">> => UserNameFun(IdP)}};
            false ->
                throw(?ERROR_BAD_IDP_RESPONSE(IdP, 401, #{}, <<>>))

        end
    end),

    [
        {dummyIdP, DummyIdP},
        {anotherIdP, AnotherIdP},
        {disabledIdP, DisabledIdP},
        {user_subject_id_fun, UserSubjectIdFun},
        {user_name_fun, UserNameFun},
        {correct_access_token_fun, CorrectAccessTokenFun},
        {prefix_fun, PrefixFun} |
        Config
    ];

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(basic_auth_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, http_client);

end_per_testcase(external_access_token_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, default_oidc_plugin);

end_per_testcase(_, _) ->
    ok.
