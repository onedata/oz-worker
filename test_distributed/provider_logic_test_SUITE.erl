%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains tests for automatic creation of predefined groups.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_logic_test_SUITE).
-author("Lukasz Opiola").

-include("entity_logic_errors.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-define(CREATE_PROVIDER_DATA, begin
    {_, __CSRFile, _} = generate_cert_files(),
    {ok, __CSR} = file:read_file(__CSRFile),
    #{
        <<"name">> => <<"ProvName">>,
        <<"urls">> => [<<"127.0.0.1">>],
        <<"redirectionPoint">> => <<"https://127.0.0.1">>,
        <<"csr">> => __CSR,
        <<"latitude">> => 50.0,
        <<"longitude">> => -24.8
    }
end).

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    create_test/1,
    support_space_test/1,
    get_test/1,
    get_spaces_test/1,
    get_eff_users_test/1,
    get_eff_groups_test/1
]).

all() ->
    ?ALL([
        create_test,
        support_space_test,
        get_test,
        get_spaces_test,
        get_eff_users_test,
        get_eff_groups_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA]
    )),
    ?assertMatch({error, ?EL_BAD_DATA(<<"csr">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA#{
            <<"csr">> => <<"wrong-csr">>
        }]
    )),
    ?assertMatch({error, ?EL_BAD_DATA(<<"latitude">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA#{
            <<"latitude">> => -91
        }]
    )),
    ?assertMatch({error, ?EL_BAD_DATA(<<"latitude">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA#{
            <<"latitude">> => 91
        }]
    )),
    ?assertMatch({error, ?EL_BAD_DATA(<<"longitude">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA#{
            <<"longitude">> => -181
        }]
    )),
    ?assertMatch({error, ?EL_BAD_DATA(<<"longitude">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA#{
            <<"longitude">> => 181
        }]
    )).


support_space_test(Config) ->
    MinimumSupportSize = min_support_size(Config),
    {ok, U1} = oz_test_utils:call_oz(
        Config, n_user_logic, create, [#od_user{}]
    ),
    {ok, S1} = oz_test_utils:call_oz(
        Config, n_space_logic, create, [?USER(U1), #{
            <<"name">> => <<"Space1">>
        }]
    ),
    {ok, {P1, _}} = oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA]
    ),
    % Try bad token first
    ct:print("a ~p", [P1]),
    ct:print("b ~p", [?PROVIDER(P1)]),
    ?assertMatch({error, ?EL_BAD_TOKEN(<<"token">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => <<"bad-token">>, <<"size">> => MinimumSupportSize
        }]
    )),
    % Bad token type
    {ok, BadMacaroon} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_user_token, [?USER(U1), S1]
    ),
    ?assertMatch({error, ?EL_BAD_TOKEN_TYPE(<<"token">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => BadMacaroon, <<"size">> => MinimumSupportSize
        }]
    )),
    {ok, Macaroon} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?USER(U1), S1]
    ),
    % Bad support size
    {error, ?EL_BAD_DATA(<<"size">>)} = oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => Macaroon, <<"size">> => 100
        }]
    ),
    % Correct request
    ?assertMatch(ok, oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => Macaroon, <<"size">> => MinimumSupportSize
        }]
    )),
    % Try to support by already supporting provider
    {ok, Macaroon2} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?USER(U1), S1]
    ),
    ?assertMatch({error, ?EL_RELATION_EXISTS}, oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => Macaroon2, <<"size">> => MinimumSupportSize
        }]
    )),
    % Create another space and support, this time by serialized token
    {ok, S2} = oz_test_utils:call_oz(
        Config, n_space_logic, create, [?USER(U1), #{
            <<"name">> => <<"Space2">>
        }]
    ),
    {ok, Macaroon3} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?USER(U1), S2]
    ),
    Token = token_logic:serialize(Macaroon3),
    ?assertMatch(ok, oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => Token, <<"size">> => MinimumSupportSize
        }]
    )).


get_test(Config) ->
    Data = ?CREATE_PROVIDER_DATA,
    {ok, {P1, _}} = oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, Data]
    ),
    ExpectedProvider = #od_provider{
        name = maps:get(<<"name">>, Data),
        urls = maps:get(<<"urls">>, Data),
        redirection_point = maps:get(<<"redirectionPoint">>, Data),
        latitude = maps:get(<<"latitude">>, Data),
        longitude = maps:get(<<"longitude">>, Data),
        spaces = [],
        eff_users = #{},
        eff_groups = #{}
    },
    ?assertMatch({ok, ExpectedProvider}, oz_test_utils:call_oz(
        Config, n_provider_logic, get, [?PROVIDER(P1), P1]
    )),
    {ok, {P2, _}} = oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA]
    ),
    % Provider 2 should be able to get data about provider 1
    ?assertMatch({ok, ExpectedProvider}, oz_test_utils:call_oz(
        Config, n_provider_logic, get, [?PROVIDER(P2), P1]
    )),
    % But anyone should not
    ?assertMatch({ok, ExpectedProvider}, oz_test_utils:call_oz(
        Config, n_provider_logic, get, [?NOBODY, P1]
    )).


get_spaces_test(Config) ->
    {ok, {P1, _}} = oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA]
    ),
    ?assertMatch({ok, []}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_spaces, [?PROVIDER(P1), P1]
    )),
    % Create some spaces
    {S1, S2, S3} = create_and_support_3_spaces(Config, P1),
    ExpectedSpaces = lists:sort([S1, S2, S3]),
    % Get spaces
    {ok, Spaces} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_spaces, [?PROVIDER(P1), P1]
    )),
    ?assertEqual(ExpectedSpaces, lists:sort(Spaces)),
    % Get each space
    ?assertMatch({ok, #od_space{name = <<"s1">>}}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_space, [?PROVIDER(P1), P1, S1]
    )),
    ?assertMatch({ok, #od_space{name = <<"s2">>}}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_space, [?PROVIDER(P1), P1, S2]
    )),
    ?assertMatch({ok, #od_space{name = <<"s3">>}}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_space, [?PROVIDER(P1), P1, S3]
    )),

    ok.


get_eff_users_test(Config) ->
    ok.


get_eff_groups_test(Config) ->
    ok.



%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(
        Config, ?TEST_FILE(Config, "env_desc.json"), [oz_test_utils]
    ),
    NewConfig.


end_per_suite(Config) ->
    ok.
%%    test_node_starter:clean_environment(Config).


create_and_support_3_spaces(Config, ProvId) ->
    MinimumSupportSize = min_support_size(Config),
    {ok, S1} = oz_test_utils:call_oz(
        Config, n_space_logic, create, [?ROOT, #{<<"name">> => <<"s1">>}]
    ),
    {ok, S2} = oz_test_utils:call_oz(
        Config, n_space_logic, create, [?ROOT, #{<<"name">> => <<"s2">>}]
    ),
    {ok, S3} = oz_test_utils:call_oz(
        Config, n_space_logic, create, [?ROOT, #{<<"name">> => <<"s3">>}]
    ),
    % Support them by the provider
    {ok, Macaroon1} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?ROOT, S1]
    ),
    {ok, Macaroon2} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?ROOT, S2]
    ),
    {ok, Macaroon3} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?ROOT, S3]
    ),
    ok = oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(ProvId), ProvId, #{
            <<"token">> => Macaroon1, <<"size">> => MinimumSupportSize
        }]
    ),
    ok = oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(ProvId), ProvId, #{
            <<"token">> => Macaroon2, <<"size">> => MinimumSupportSize
        }]
    ),
    ok = oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(ProvId), ProvId, #{
            <<"token">> => Macaroon3, <<"size">> => MinimumSupportSize
        }]
    ),
    {S1, S2, S3}.


min_support_size(Config) ->
    {ok, MinimumSupportSize} = oz_test_utils:call_oz(
        Config, application, get_env, [oz_worker, minimum_space_support_size]
    ),
    MinimumSupportSize.


generate_cert_files() ->
    Prefix = "provider" ++ integer_to_list(erlang:system_time(micro_seconds)),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {KeyFile, CSRFile, CertFile}.