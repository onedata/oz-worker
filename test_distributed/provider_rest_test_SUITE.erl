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
-module(provider_rest_test_SUITE).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include("entity_logic_errors.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-record(rest_test_spec, {
    method = get :: get | patch | post | put | delete,
    path = <<"/">> :: binary (),
    not_found_path = undefined :: binary () | undefined,
    client = #{
        correct => [],
        unauthorized => [],
        forbidden => []
    },
    data = #{
        required => [binary],
        optional => [binary],
        at_least_one => [binary],
        correct_values => #{binary => binary},
        bad_values => #{binary => binary} % tu moze byc tupla z rodzajem bledu
    },
    correct_result = #{
        code => integer,
        headers => [],
        body => <<"">>
    }
}).


%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    create_test/1,
    list_test/1,
    get_test/1,
    get_users_test/1
%%    get_groups_test/1,
%%    get_spaces_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        get_users_test
%%        get_groups_test,
%%        get_spaces_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    RestPrefix = rest_test_utils:get_rest_api_prefix(Config),
    {_, CSRFile, _} = oz_test_utils:generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    RestTestSpec = #rest_test_spec{
        method = create,
        path = <<"/providers">>,
        client = #{correct => nobody},
        data = #{
            required => [<<"name">>, <<"urls">>, <<"redirectionPoint">>, <<"csr">>],
            optional => [<<"longitude">>, <<"longitude">>],
            correct_values => #{
                <<"name">> => <<"ProvName">>,
                <<"urls">> => [<<"127.0.0.1">>],
                <<"redirectionPoint">> => <<"https://127.0.0.1">>,
                <<"csr">> => CSR,
                <<"latitude">> => 50.0,
                <<"longitude">> => -24.8
            },
            bad_values => #{
                <<"name">> => #{
                    empty => <<"">>,
                    bad => 1234
                },
                <<"urls">> => #{
                    empty => [],
                    bad => <<"127.0.0.1">>
                },
                <<"redirectionPoint">> => #{
                    empty => <<"">>,
                    bad => 1234
                },
                <<"csr">> => #{
                    empty => <<"">>,
                    bad => 1234
                },
                <<"latitude">> => #{
                    bad => [-1500, -90.1, 90.1, 1500]
                },
                <<"longitude">> => #{
                    bad => [-1500, -180.1, 180.1, 1500]
                }
            }
        },
        correct_result = #{
            code = 201,
            headers = #{
                <<"location">> => {match, <<RestPrefix/binary, "/provider/.*">>}
            }
        }
    },
    ?assert(rest_test_utils:test_endpoint(RestTestSpec)).


list_test(Config) ->
    % Register some providers
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(Config, <<"P1">>),
    {ok, {P2, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P2">>),
    {ok, {P3, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P3">>),
    {ok, {P4, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P4">>),
    {ok, {P5, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P5">>),
    % Create two users, grant one of them the privilege to list providers.
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, U1, grant, [list_providers]),

    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    RestTestSpec = #rest_test_spec{
        method = get,
        path = <<"/providers">>,
        client = #{
            correct => [{user, U1}],
            unauthorized => [nobody],
            forbidden => [{user, U2}, {provider, KeyFile, CertFile}]
        },
        correct_result = #{
            code = 200,
            body = #{
                <<"providers">> => [P1, P2, P3, P4, P5]
            }
        }
    },
    ?assert(rest_test_utils:test_endpoint(RestTestSpec)).


get_test(Config) ->
    % Create a provider manually
    {KeyFile1, CSRFile1, CertFile1} = oz_test_utils:generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile1),
    {ok, {P1, Certificate}} = oz_test_utils:create_provider(Config, #{
        <<"name">> => <<"Provider 1">>,
        <<"urls">> => [<<"172.16.0.10">>, <<"172.16.0.11">>],
        <<"redirectionPoint">> => <<"https://hostname.com">>,
        <<"csr">> => CSR,
        <<"latitude">> => 14.78,
        <<"longitude">> => -106.12
    }),
    ok = file:write_file(CertFile1, Certificate),
    % Create a second provider
    {ok, {_P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P2">>
    ),
    % Create two users, grant one of them the privilege to list providers.
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, U1, grant, [list_providers]),

    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    RestTestSpec = #rest_test_spec{
        method = get,
        path = [<<"/providers/">>, P1],
        not_found_path = [<<"/providers/">>, <<"wrong-provider-id">>],
        client = #{
            correct => [
                {user, U1},
                {provider, KeyFile1, CertFile1},
                {provider, KeyFile2, CertFile2}
            ],
            unauthorized => [nobody],
            forbidden => [{user, U2}]
        },
        correct_result = #{
            code = 200,
            body = #{
                <<"providerId">> => P1,
                <<"name">> => <<"Provider 1">>,
                <<"urls">> => [<<"172.16.0.10">>, <<"172.16.0.11">>],
                <<"redirectionPoint">> => <<"https://hostname.com">>,
                <<"latitude">> => 14.78,
                <<"longitude">> => -106.12
            }
        }
    },
    ?assert(rest_test_utils:test_endpoint(RestTestSpec)),
    % Provider should be also able to retrieve this info using another path,
    % without id (id is deduced from authorization)
    RestTestSpec2 = #rest_test_spec{
        method = get,
        path = <<"/provider">>,
        client = #{
            correct => [{provider, KeyFile1, CertFile1}],
            unauthorized => [nobody]
        },
        correct_result = #{
            code = 200,
            body = #{
                <<"providerId">> => P1,
                <<"name">> => <<"Provider 1">>,
                <<"urls">> => [<<"172.16.0.10">>, <<"172.16.0.11">>],
                <<"redirectionPoint">> => <<"https://hostname.com">>,
                <<"latitude">> => 14.78,
                <<"longitude">> => -106.12
            }
        }
    },
    ?assert(rest_test_utils:test_endpoint(RestTestSpec2)).


get_users_test(Config) ->
    SupportSize = min_support_size(Config) + 1,
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(Config, <<"P1">>),
    % Create some spaces - groups - users structure
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U4} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), <<"S1">>),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U2), <<"S2">>),
    {ok, S3} = oz_test_utils:create_space(Config, ?USER(U3), <<"S3">>),
    {ok, S4} = oz_test_utils:create_space(Config, ?USER(U4), <<"S4">>),
    {ok, U5} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U6} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U5), <<"G1">>),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U6), <<"G2">>),
    {ok, S1} = oz_test_utils:add_group_to_space(Config, ?ROOT, S1, G1),
    {ok, S2} = oz_test_utils:add_group_to_space(Config, ?ROOT, S2, G2),
    {ok, U7} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U8} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U9} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U10} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:add_group_to_space(Config, ?ROOT, G1, U7),
    {ok, G1} = oz_test_utils:add_group_to_space(Config, ?ROOT, G1, U8),
    {ok, G2} = oz_test_utils:add_user_to_group(Config, ?ROOT, G2, U9),
    {ok, G2} = oz_test_utils:add_user_to_group(Config, ?ROOT, G2, U10),
    % Support the spaces by provider
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    {ok, Token1} = oz_test_utils:space_invite_provider_token(Config, ?USER(U1), S1),
    {ok, Token2} = oz_test_utils:space_invite_provider_token(Config, ?USER(U1), S2),
    {ok, Token3} = oz_test_utils:space_invite_provider_token(Config, ?USER(U1), S3),
    {ok, Token4} = oz_test_utils:space_invite_provider_token(Config, ?USER(U1), S4),
    {ok, P1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), P1, Token1, SupportSize),
    {ok, P2} = oz_test_utils:support_space(Config, ?PROVIDER(P2), P2, Token2, SupportSize),
    {ok, P3} = oz_test_utils:support_space(Config, ?PROVIDER(P3), P3, Token3, SupportSize),
    {ok, P4} = oz_test_utils:support_space(Config, ?PROVIDER(P4), P4, Token4, SupportSize),

    ok = oz_test_utils:set_user_oz_privileges(Config, U1, grant, [list_users_of_provider]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    RestTestSpec = #rest_test_spec{
        method = get,
        path = [<<"/providers/">>, P1, <<"/users">>],
        client = #{
            correct => [
                {user, U1}
            ],
            unauthorized => [nobody],
            forbidden => [{user, U2}, {provider, KeyFile, CertFile}]
        },
        correct_result = #{
            code = 200,
            body = #{
                <<"users">> => [U1, U2, U3, U4, U5, U6, U7, U8, U9, U10]
            }
        }
    },
    ?assert(rest_test_utils:test_endpoint(RestTestSpec)).


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


ensure_eff_graph_up_to_date(Config) ->
    oz_test_utils:call_oz(Config, entity_graph, ensure_up_to_date, []).


min_support_size(Config) ->
    {ok, MinimumSupportSize} = oz_test_utils:call_oz(
        Config, application, get_env, [oz_worker, minimum_space_support_size]
    ),
    MinimumSupportSize.
