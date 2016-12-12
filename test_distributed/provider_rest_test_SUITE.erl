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
    create_test/1
]).

all() ->
    ?ALL([
        create_test
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
    ?assert(rest_test_utils:check_endpoint(RestTestSpec)),
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


ensure_eff_graph_up_to_date(Config) ->
    oz_test_utils:call_oz(Config, entity_graph, ensure_up_to_date, []).


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