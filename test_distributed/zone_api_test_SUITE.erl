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

-include("datastore/oz_datastore_models.hrl").
-include("http/rest.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/onedata.hrl").
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
    get_old_configuration_endpoint_test/1,
    list_privileges_test/1,
    default_gui_messages_are_empty_or_migrated/1,
    unknown_gui_messages_are_not_found/1
]).

all() ->
    ?ALL([
        get_configuration_test,
        get_old_configuration_endpoint_test,
        list_privileges_test,
        default_gui_messages_are_empty_or_migrated,
        unknown_gui_messages_are_not_found
    ]).

-define(SIGNIN_NOTIFICATION, "custom login notification").

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
    Domain = oz_test_utils:get_env(Config, http_domain),

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


list_privileges_test(Config) ->
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/privileges">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"viewer">> => [atom_to_binary(P, utf8) || P <- privileges:oz_viewer()],
                <<"admin">> => [atom_to_binary(P, utf8) || P <- privileges:oz_admin()]
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


default_gui_messages_are_empty_or_migrated(Config) ->
    oz_test_utils:set_env(Config, login_notification, ?SIGNIN_NOTIFICATION),
    lists:foreach(fun(MessageId) ->
        {ok, #document{value = Record}} = ?assertMatch({ok, #document{}},
            oz_test_utils:call_oz(Config, gui_message, get, [MessageId])),
        ?assertEqual(#gui_message{enabled = true, body = <<>>}, Record)
    end, gui_message_ids() -- [<<"signin_notification">>]),

    {ok, #document{value = Record}} = ?assertMatch({ok, #document{}},
        oz_test_utils:call_oz(Config, gui_message, get, [<<"signin_notification">>])),
    ?assertEqual(#gui_message{enabled = true, body = <<?SIGNIN_NOTIFICATION>>}, Record).


unknown_gui_messages_are_not_found(Config) ->
    ?assertEqual({error, not_found},
        oz_test_utils:call_oz(Config, gui_message, get, [<<"unknownId">>])).


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

    SubdomainDelegationSupported = false,
    OZDomain = oz_test_utils:oz_domain(Config),
    OZName = case oz_test_utils:get_env(Config, oz_name) of
        undefined -> null;
        OZNameString -> list_to_binary(OZNameString)
    end,

    OZVersion = oz_test_utils:call_oz(Config, oz_worker, get_release_version, []),

    OZBuild = case oz_test_utils:get_env(Config, build_version) of
        "" -> <<"unknown">>;
        Build -> list_to_binary(Build)
    end,

    {_, []} = rpc:multicall(Nodes, application, set_env, [
        ?APP_NAME, subdomain_delegation_supported, SubdomainDelegationSupported
    ]),

    MockedSupportedIdPs = [
        #{<<"id">> => <<"idp1">>, <<"offlineAccess">> => false},
        #{<<"id">> => <<"idp2">>, <<"offlineAccess">> => true}
    ],
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

    MockedCompatibleVersions = [<<"18.02.0-rc13">>, <<"18.02.1">>, <<"18.02.2">>],
    oz_test_utils:overwrite_compatibility_registry(Config, #{
        <<"revision">> => 1,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                OZVersion => MockedCompatibleVersions
            }
        }
    }),

    #{
        <<"name">> => OZName,
        <<"domain">> => OZDomain,
        <<"version">> => OZVersion,
        <<"build">> => OZBuild,
        <<"compatibleOneproviderVersions">> => MockedCompatibleVersions,
        <<"subdomainDelegationSupported">> => SubdomainDelegationSupported,
        <<"supportedIdPs">> => MockedSupportedIdPs
    }.


-spec gui_message_ids() -> [binary()].
gui_message_ids() -> [
    <<"cookie_consent_notification">>,
    <<"privacy_policy">>,
    <<"signin_notification">>
].