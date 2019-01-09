%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains test concerning the public configuration
%%% endpoint.
%%% @end
%%%-------------------------------------------------------------------
-module(zone_api_test_SUITE).
-author("Wojciech Geisler").

-include("rest.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-include("api_test_utils.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    get_configuration_test/1
]).

all() ->
    ?ALL([
        get_configuration_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

get_configuration_test(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),

    SubdomainDelegation = false,
    {ok, OZDomainString} = oz_test_utils:get_oz_domain(Config),
    {ok, OZNameString} = oz_test_utils:get_env(Config, oz_worker, oz_name),
    OZVersion = oz_test_utils:call_oz(Config, oz_worker, get_version, []),
    {ok, OZBuildString} = oz_test_utils:get_env(Config, oz_worker, build_version),
    {ok, OZCompatibleOneproviders} = oz_test_utils:get_env(Config, oz_worker,
        compatible_op_versions),

    {_, []} = rpc:multicall(Nodes, application, set_env, [
        ?APP_NAME, subdomain_delegation_enabled, SubdomainDelegation
    ]),

    Expected = #{
        <<"name">> => str_utils:to_binary(OZNameString),
        <<"domain">> => str_utils:to_binary(OZDomainString),
        <<"version">> => str_utils:to_binary(OZVersion),
        <<"build">> => str_utils:to_binary(OZBuildString),
        <<"compatibleOneproviderVersions">> =>
        [str_utils:to_binary(V) || V <- OZCompatibleOneproviders],
        <<"subdomainDelegationEnabled">> => SubdomainDelegation
    },

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/configuration">>,
            expected_code = ?HTTP_200_OK,
            expected_body = Expected
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


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


init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_, _Config) ->
    ok.