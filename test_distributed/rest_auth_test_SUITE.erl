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
    UserName = ?config(user_name, Config),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => basic_auth_header(?CORRECT_LOGIN, ?CORRECT_PASSWORD)
        },
        expect => #{
            code => 200,
            body => {contains, #{
                <<"userId">> => UserId, <<"name">> => UserName
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

    ok.


external_access_token_test(Config) ->
    % OAuth config and authorization procedure are mocked in init_per_testcase,
    % and user is initialized there
    UserId = ?config(user_id, Config),
    UserName = ?config(user_name, Config),
    IndigoToken = ?config(indigo_token, Config),
    GoogleToken = ?config(google_token, Config),
    BadGoogleToken = ?config(bad_google_token, Config),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"X-Auth-Token">> => IndigoToken}
        },
        expect => #{
            code => 200,
            body => {contains, #{
                <<"userId">> => UserId, <<"name">> => UserName
            }}
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"X-Auth-Token">> => GoogleToken}
        },
        expect => #{
            code => 200,
            body => {contains, #{
                <<"userId">> => UserId, <<"name">> => UserName
            }}
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"X-Auth-Token">> => BadGoogleToken}
        },
        expect => #{
            code => 401
        }
    })),

    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"X-Auth-Token">> => <<"bad-token">>}
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
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


init_per_testcase(basic_auth_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    UserName = <<"U1">>,
    {ok, UserId} = oz_test_utils:create_user(Config, #od_user{name = UserName}),
    {ok, UserDoc} = oz_test_utils:call_oz(Config, od_user, get, [UserId]),
    ok = test_utils:mock_new(Nodes, user_logic, [passthrough]),
    ok = test_utils:mock_expect(Nodes, user_logic, authenticate_by_basic_credentials,
        fun
            (?CORRECT_LOGIN, ?CORRECT_PASSWORD) ->
                {ok, UserDoc};
            (?BAD_LOGIN, ?BAD_PASSWORD) ->
                {error, <<"Invalid login or password">>};
            (_, _) ->
                meck:passthrough()
        end),
    [{user_id, UserId}, {user_name, UserName} | Config];

init_per_testcase(external_access_token_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    UserName = <<"U1">>,
    {ok, UserId} = oz_test_utils:create_user(Config, #od_user{name = UserName}),
    GoogleProvId = google,
    IndigoProvId = indigo,
    GooglePrefix = <<"google:">>,
    IndigoPrefix = <<"indigo-">>,
    GoogleToken = <<"acess-token-google-dummy">>,
    IndigoToken = <<"acess-token-indigo-dummy">>,

    ok = test_utils:mock_new(Nodes, auth_config, [passthrough]),
    ok = test_utils:mock_expect(Nodes, auth_config, get_providers_with_auth_delegation,
        fun() ->
            [{IndigoProvId, IndigoPrefix}, {GoogleProvId, GooglePrefix}]
        end),

    ok = test_utils:mock_new(Nodes, auth_utils, [passthrough]),
    ok = test_utils:mock_expect(Nodes, auth_utils, acquire_user_by_external_access_token,
        fun(ProviderId, AccessToken) ->
            case {ProviderId, AccessToken} of
                {GoogleProvId, GoogleToken} ->
                    {ok, #document{key = UserId, value = #od_user{}}};
                {IndigoProvId, IndigoToken} ->
                    {ok, #document{key = UserId, value = #od_user{}}};
                {_, _} ->
                    {error, bad_access_token}
            end
        end),

    [
        {user_id, UserId}, {user_name, UserName},
        {google_token, <<GooglePrefix/binary, GoogleToken/binary>>},
        {bad_google_token, <<GooglePrefix/binary, "bad-token">>},
        {indigo_token, <<IndigoPrefix/binary, IndigoToken/binary>>} |
        Config
    ];

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(basic_auth_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, user_logic);

end_per_testcase(external_access_token_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, auth_config),
    test_utils:mock_unload(Nodes, auth_utils);

end_per_testcase(_, _) ->
    ok.
