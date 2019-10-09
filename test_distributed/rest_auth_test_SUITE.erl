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

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{?HDR_X_AUTH_TOKEN => Token}
        },
        expect => #{
            code => 200,
            body => {contains, #{<<"fullName">> => <<"U1">>}}
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{?HDR_X_AUTH_TOKEN => Token}
        },
        expect => #{
            code => 200,
            body => {contains, #{<<"fullName">> => <<"U1">>}}
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{?HDR_X_AUTH_TOKEN => <<"bad-token">>}
        },
        expect => #{
            code => 401
        }
    })),

    ok.


basic_auth_test(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #{
        <<"username">> => ?CORRECT_USERNAME,
        <<"password">> => ?CORRECT_PASSWORD
    }),

    oz_test_utils:toggle_basic_auth(Config, true),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => basic_auth_header(?CORRECT_USERNAME, ?CORRECT_PASSWORD)
        },
        expect => #{
            code => 200,
            body => {contains, #{
                <<"userId">> => UserId, <<"username">> => ?CORRECT_USERNAME
            }}
        }
    })),
    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => basic_auth_header(?BAD_USERNAME, ?BAD_PASSWORD)
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
            headers => basic_auth_header(?CORRECT_USERNAME, ?CORRECT_PASSWORD)
        },
        expect => #{
            code => 401
        }
    })),
    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => basic_auth_header(?BAD_USERNAME, ?BAD_PASSWORD)
        },
        expect => #{
            code => 401
        }
    })),

    ok.


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
        linked_accounts:gen_user_id(IdP, UserSubjectIdFun(IdP))
    end,

    XAuthTokenFun = fun(IdP) ->
        <<(PrefixFun(IdP))/binary, (CorrectAccessTokenFun(IdP))/binary>>
    end,

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{?HDR_X_AUTH_TOKEN => XAuthTokenFun(DummyIdP)}
        },
        expect => #{
            code => 200,
            body => {contains, #{
                <<"userId">> => UserIdFun(DummyIdP),
                <<"fullName">> => UserFullNameFun(DummyIdP)
            }}
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{?HDR_X_AUTH_TOKEN => XAuthTokenFun(AnotherIdP)}
        },
        expect => #{
            code => 200,
            body => {contains, #{
                <<"userId">> => UserIdFun(AnotherIdP),
                <<"fullName">> => UserFullNameFun(AnotherIdP)
            }}
        }
    })),

    % DisabledIdP has disabled authority delegation
    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{?HDR_X_AUTH_TOKEN => XAuthTokenFun(DisabledIdP)}
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
            headers => #{?HDR_X_AUTH_TOKEN => <<
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
            headers => #{?HDR_X_AUTH_TOKEN => <<"completely-bad-token">>}
        },
        expect => #{
            code => 401
        }
    })),

    ok.


gui_token_test(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #{<<"fullName">> => <<"U1">>}),
    {ok, {Provider1, Provider1Token}} = oz_test_utils:create_provider(
        Config, UserId, ?UNIQUE_STRING
    ),
    {ok, {_Provider2, Provider2Token}} = oz_test_utils:create_provider(
        Config, UserId, ?UNIQUE_STRING
    ),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space(Config, Provider1, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    {ok, {SessionId, _Cookie}} = oz_test_utils:log_in(Config, UserId),

    {ok, {GuiToken1, Ttl}} = oz_test_utils:call_oz(Config, token_logic, create_gui_access_token, [
        ?USER(UserId), UserId, SessionId, ?AUD(?OP_WORKER, Provider1)
    ]),
    {ok, SerializedGuiToken1} = tokens:serialize(GuiToken1),

    {ok, {GuiToken2, Ttl}} = oz_test_utils:call_oz(Config, token_logic, create_gui_access_token, [
        ?USER(UserId), UserId, SessionId, ?AUD(?OP_PANEL, Provider1)
    ]),
    {ok, SerializedGuiToken2} = tokens:serialize(GuiToken2),

    Provider1AudToken = tokens:build_service_access_token(?OP_WORKER, Provider1Token),
    Provider2AudToken = tokens:build_service_access_token(?OP_PANEL, Provider2Token),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{
                ?HDR_X_AUTH_TOKEN => SerializedGuiToken1,
                ?HDR_X_ONEDATA_AUDIENCE_TOKEN => Provider1AudToken
            }
        },
        expect => #{
            code => 200, % correct audience
            body => {contains, #{<<"fullName">> => <<"U1">>}}
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{
                ?HDR_AUTHORIZATION => <<"Bearer ", SerializedGuiToken2/binary>>,
                ?HDR_X_ONEDATA_AUDIENCE_TOKEN => Provider2AudToken
            }
        },
        expect => #{
            code => 401 % invalid audience
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{
                ?HDR_X_AUTH_TOKEN => SerializedGuiToken1
            }
        },
        expect => #{
            code => 401 % missing audience token
        }
    })),

    oz_test_utils:simulate_time_passing(Config, Ttl + 1),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{
                ?HDR_X_AUTH_TOKEN => SerializedGuiToken1,
                ?HDR_X_ONEDATA_AUDIENCE_TOKEN => Provider1AudToken
            }
        },
        expect => #{
            code => 401 % expired token
        }
    })),
    ok.

%%%===================================================================
%%% Helper functions
%%%===================================================================

basic_auth_header(Username, Password) ->
    UserPasswdB64 = base64:encode(<<Username/binary, ":", Password/binary>>),
    #{?HDR_AUTHORIZATION => <<"Basic ", UserPasswdB64/binary>>}.

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
    oz_test_utils:mock_time(Config),
    Config;

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(external_access_token_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, default_oidc_plugin);

end_per_testcase(gui_token_test, Config) ->
    oz_test_utils:unmock_time(Config);

end_per_testcase(_, _) ->
    ok.
