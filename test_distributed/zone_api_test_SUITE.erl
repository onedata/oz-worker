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
    get_configuration_test/1,
    get_old_configuration_endpoint_test/1
]).

all() ->
    ?ALL([
        get_configuration_test,
        get_old_configuration_endpoint_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

get_configuration_test(Config) ->

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/configuration">>,
            expected_code = ?HTTP_200_OK,
            expected_body = expected_configuration(Config)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


%% Legacy endpoint should be available on path without the API prefix
get_old_configuration_endpoint_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    {ok, Domain} = test_utils:get_env(Node, ?APP_NAME, http_domain),

    RestSpec = #{
        request => #{
            url => str_utils:format_bin("https://~s", [Domain]),
            path => <<"/configuration">>
        },
        expect => #{
            code => 200,
            body => expected_configuration(Config)
        }
    },

    ?assert(rest_test_utils:check_rest_call(Config, RestSpec)).


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


%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
expected_configuration(Config) ->
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

    oz_test_utils:overwrite_auth_config(Config, #{
        openidConfig => #{
            enabled => true
        },
        supportedIdps => [
            {idp1, #{
                protocol => openid,
                protocolConfig => #{
                    plugin => default_oidc_plugin,
                    offlineAccess => false
                }
            }},
            {idp2, #{
                protocol => openid,
                protocolConfig => #{
                    plugin => default_oidc_plugin,
                    offlineAccess => true
                }
            }}
        ]
    }),
    SupportedIdPs = [
        #{<<"id">> =>  <<"idp1">>, <<"offlineAccess">> =>  false},
        #{<<"id">> =>  <<"idp2">>, <<"offlineAccess">> =>  true}
    ],

    #{
        <<"name">> => str_utils:to_binary(OZNameString),
        <<"domain">> => str_utils:to_binary(OZDomainString),
        <<"version">> => str_utils:to_binary(OZVersion),
        <<"build">> => str_utils:to_binary(OZBuildString),
        <<"compatibleOneproviderVersions">> =>
        [str_utils:to_binary(V) || V <- OZCompatibleOneproviders],
        <<"subdomainDelegationSupported">> => SubdomainDelegation,
        <<"supportedIdPs">> => SupportedIdPs
    }.