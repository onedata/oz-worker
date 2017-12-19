%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning provider API (REST + logic).
%%% @end
%%%-------------------------------------------------------------------
-module(provider_api_test_SUITE).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("cluster_worker/include/api_errors.hrl").

-include("api_test_utils.hrl").

-define(MAP_GROUP_TEST_AUTH, test_auth).
-define(MAP_GROUP_TEST_AUTH_BIN, atom_to_binary(?MAP_GROUP_TEST_AUTH, latin1)).
-define(MAP_GROUP_TEST_AUTH_MODULE, test_auth_module).
-define(MAPPED_MEMBERSHIP_SPEC, <<"mapped_group1/user:member">>).
-define(MAPPED_GROUP_SPEC, <<"mapped_group1">>).


%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    create_test/1,
    get_test/1,
    get_self_test/1,
    list_test/1,
    update_test/1,
    delete_test/1,
    delete_self_test/1,

    list_eff_users_test/1,
    get_eff_user_test/1,
    list_eff_groups_test/1,
    get_eff_group_test/1,

    list_spaces_test/1,
    list_self_spaces_test/1,
    get_space_test/1,
    get_self_space_test/1,
    support_space_test/1,
    update_support_size_test/1,
    revoke_support_test/1,

    check_my_ports_test/1,
    check_my_ip_test/1,
    map_group_test/1,
    update_subdomain_test/1,
    update_domain_test/1,
    get_domain_config_test/1,
    get_current_time_test/1
]).

all() ->
    ?ALL([
        create_test,
        get_test,
        get_self_test,
        list_test,
        update_test,
        delete_test,
        delete_self_test,

        list_eff_users_test,
        get_eff_user_test,
        list_eff_groups_test,
        get_eff_group_test,

        list_spaces_test,
        list_self_spaces_test,
        get_space_test,
        get_self_space_test,
        support_space_test,
        update_support_size_test,
        revoke_support_test,

        check_my_ports_test,
        check_my_ip_test,
        map_group_test,
        update_subdomain_test,
        update_domain_test,
        get_domain_config_test,
        get_current_time_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

% TODO VFS-2918 (all tests) <<"clientName">> -> <<"name">>

create_test(Config) ->
    {_, CSRFile, _} = oz_test_utils:generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    ExpName = ?PROVIDER_NAME1,

    {ok, OZDomainString} = oz_test_utils:get_oz_domain(Config),
    OZDomain = list_to_binary(OZDomainString),

    VerifyFun = fun(ProviderId, Certificate, Data) ->
        ExpLatitude = maps:get(<<"latitude">>, Data, 0.0),
        ExpLongitude = maps:get(<<"longitude">>, Data, 0.0),
        ExpSubdomainDelegation = maps:get(<<"subdomainDelegation">>, Data),
        {ExpSubdomain, ExpDomain} = case ExpSubdomainDelegation of
            true ->
                Subdomain = maps:get(<<"subdomain">>, Data),
                {Subdomain, <<Subdomain/binary, ".", OZDomain/binary>>};
            false ->
                {undefined, maps:get(<<"domain">>, Data)}
        end,

        true = is_binary(Certificate),
        {ok, Provider} = oz_test_utils:get_provider(Config, ProviderId),
        ?assertEqual(ExpName, Provider#od_provider.name),
        ?assertEqual(ExpDomain, Provider#od_provider.domain),
        ?assertEqual(ExpSubdomainDelegation, Provider#od_provider.subdomain_delegation),
        ?assertEqual(ExpSubdomain, Provider#od_provider.subdomain),
        ?assertEqual(ExpLatitude, Provider#od_provider.latitude),
        ?assertEqual(ExpLongitude, Provider#od_provider.longitude),

        % check also provider_logic:get_url function
        ExpProviderURL = str_utils:format_bin("https://~s", [ExpDomain]),
        {ok, ProviderURL} = oz_test_utils:call_oz(
            Config, provider_logic, get_url, [ProviderId]
        ),
        ?assertEqual(ProviderURL, ExpProviderURL),

        % delete provider to avoid "subdomain occupied" errors
        oz_test_utils:delete_provider(Config, ProviderId),
        true
    end,

    %% Create provider with subdomain delegation turned off
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider">>,
            expected_code = ?HTTP_200_OK,
            expected_body = ?OK_ENV(fun(_Env, Data) -> fun(Value) ->
                    Certificate = maps:get(<<"certificate">>, Value),
                    ProviderId = maps:get(<<"providerId">>, Value),
                    VerifyFun(ProviderId, Certificate, Data)
                end
            end)
        },
        logic_spec = #logic_spec{
            operation = create,
            module = provider_logic,
            function = create,
            args = [client, data],
            expected_result = ?OK_ENV(fun(_Env, Data) ->
                ?OK_TERM(fun({ProviderId, Certificate}) ->
                    VerifyFun(ProviderId, Certificate, Data)
                end)
            end)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [
                <<"clientName">>, <<"domain">>, <<"subdomainDelegation">>, <<"csr">>
            ],
            optional = [<<"latitude">>, <<"longitude">>],
            correct_values = #{
                <<"clientName">> => [ExpName],
                <<"domain">> => [<<"multilevel.provider-domain.org">>],
                <<"subdomainDelegation">> => [false],
                <<"csr">> => [CSR],
                <<"latitude">> => [rand:uniform() * 90],
                <<"longitude">> => [rand:uniform() * 180]
            },
            bad_values = [
                {<<"clientName">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"clientName">>)},
                {<<"clientName">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"clientName">>)},
                {<<"domain">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"domain">>)},
                {<<"domain">>, <<"https://domain.com">>, ?ERROR_BAD_VALUE_DOMAIN(<<"domain">>)},
                {<<"domain">>, <<"domain.com:443">>, ?ERROR_BAD_VALUE_DOMAIN(<<"domain">>)},
                {<<"domain">>, <<".leadingdot">>, ?ERROR_BAD_VALUE_DOMAIN(<<"domain">>)},
                {<<"domain">>, <<"trailing-.hyphen">>, ?ERROR_BAD_VALUE_DOMAIN(<<"domain">>)},
                {<<"subdomainDelegation">>, <<"binary">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
                {<<"subdomainDelegation">>, bad_bool, ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
                {<<"csr">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"csr">>)},
                {<<"csr">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"csr">>)},
                {<<"csr">>, <<"wrong-csr">>, ?ERROR_BAD_DATA(<<"csr">>)},
                {<<"latitude">>, <<"ASDASD">>, ?ERROR_BAD_VALUE_FLOAT(<<"latitude">>)},
                {<<"latitude">>, -1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"latitude">>, -90.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"latitude">>, 90.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"latitude">>, 1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"longitude">>, <<"ASDASD">>, ?ERROR_BAD_VALUE_FLOAT(<<"longitude">>)},
                {<<"longitude">>, -1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)},
                {<<"longitude">>, -180.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)},
                {<<"longitude">>, 180.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)},
                {<<"longitude">>, 1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    %% Create provider with subdomain delegation turned on
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        data_spec = #data_spec{
            required = [
                <<"clientName">>, <<"subdomain">>, <<"ipList">>,
                <<"subdomainDelegation">>, <<"csr">>
            ],
            optional = [<<"latitude">>, <<"longitude">>],
            correct_values = #{
                <<"clientName">> => [ExpName],
                <<"subdomainDelegation">> => [true],
                <<"subdomain">> => [<<"prov-sub">>],
                <<"ipList">> => [[<<"2.4.6.8">>, <<"255.253.251.2">>]],
                <<"csr">> => [CSR],
                <<"latitude">> => [rand:uniform() * 90],
                <<"longitude">> => [rand:uniform() * 180]
            },
            bad_values = [
                {<<"subdomain">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"subdomain">>)},
                {<<"subdomain">>, <<"port:443">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<"Uppercase">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<"under_score">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<"-leadinghyphen">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<"trailingdot.">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<"multi.part">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<"https://protocol">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"ipList">>, [atom], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, atom, ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, [<<"256.256.256.256">>], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_test(Config) ->
    ExpName = ?PROVIDER_NAME1,
    ExpDomain = ?DOMAIN,
    ExpLatitude = rand:uniform() * 90,
    ExpLongitude = rand:uniform() * 180,
    ProviderDetails = #{
        <<"name">> => ExpName,
        <<"domain">> => ExpDomain,
        <<"latitude">> => ExpLatitude,
        <<"longitude">> => ExpLongitude
    },
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, ProviderDetails#{<<"subdomainDelegation">> => false}
    ),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    SupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, S1} = oz_test_utils:support_space(Config, P1, S1, SupportSize),

    ExpDetails = ProviderDetails#{
        <<"clientName">> => ?PROVIDER_NAME1,
        <<"online">> => true
    },

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, Admin},
                {user, NonAdmin},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = get,
            args = [client, P1],
            expected_result = ?OK_TERM(
                fun(#od_provider{
                    name = Name, subdomain_delegation = false,
                    domain = Domain, subdomain = undefined,
                    latitude = Latitude, longitude = Longitude,
                    spaces = Spaces, eff_users = EffUsers, eff_groups = #{}
                }) ->
                    ?assertEqual(Name, ExpName),
                    ?assertEqual(Domain, ExpDomain),
                    ?assertEqual(Latitude, ExpLatitude),
                    ?assertEqual(Longitude, ExpLongitude),
                    ?assertEqual(Spaces, #{S1 => SupportSize}),
                    ?assertEqual(EffUsers, #{U1 => [{od_space, S1}]})
                end)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_provider, id = P1, aspect = instance},
            expected_result = ?OK_MAP(#{
                <<"online">> => true,
                <<"name">> => ExpName, <<"domain">> => ExpDomain,
                <<"effectiveGroups">> => [], <<"effectiveUsers">> => [U1],
                <<"latitude">> => ExpLatitude, <<"longitude">> => ExpLongitude,
                <<"spaces">> => #{S1 => SupportSize},
                <<"subdomain">> => <<"undefined">>,
                <<"subdomainDelegation">> => false,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(P1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check protected data
    GetProtectedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, Admin},
                {provider, P2, KeyFile2, CertFile2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpDetails#{<<"providerId">> => P1}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = get_protected_data,
            args = [client, P1],
            expected_result = ?OK_MAP(ExpDetails)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_provider, id = P1,
                aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP(ExpDetails#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(P1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)).


get_self_test(Config) ->
    ProviderDetails = ?PROVIDER_DETAILS(?PROVIDER_NAME1),
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, ProviderDetails#{<<"subdomainDelegation">> => false}
    ),

    ExpDetails = ProviderDetails#{
        <<"clientName">> => ?PROVIDER_NAME1,
        <<"online">> => false
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, KeyFile1, CertFile1}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/provider">>,
            expected_code = ?HTTP_200_OK,
            expected_body = ExpDetails#{<<"providerId">> => P1 }
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_provider, id = ?SELF,
                aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP(ExpDetails#{
                <<"online">> => true,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(P1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_test(Config) ->
    % Make sure that providers created in other tests are deleted.
    ok = oz_test_utils:delete_all_entities(Config),

    % Register some providers
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, {P2, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P2">>),
    {ok, {P3, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P3">>),
    {ok, {P4, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P4">>),
    {ok, {P5, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P5">>),
    ExpProviders = [P1, P2, P3, P4, P5],

    % Create two users, grant one of them the privilege to list providers.
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST
    ]),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P1, KeyFile, CertFile}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/providers">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"providers">> => ExpProviders}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpProviders)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:exist function
    lists:foreach(
        fun(ProviderId) ->
            ?assert(oz_test_utils:call_oz(
                Config, provider_logic, exists, [ProviderId])
            )
        end, ExpProviders
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, provider_logic, exists, [<<"asdiucyaie827346w">>])
    ).


update_test(Config) ->
    Name = ?PROVIDER_NAME1,
    Latitude = rand:uniform() * 90,
    Longitude = rand:uniform() * 180,
    ProviderDetails = #{
        <<"name">> => Name, <<"domain">> => ?DOMAIN,
        <<"subdomainDelegation">> => false,
        <<"latitude">> => Latitude, <<"longitude">> => Longitude
    },

    EnvSetUpFun = fun() ->
        {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
            Config, ProviderDetails
        ),
        #{providerId => P1, providerClient => {provider, P1, KeyFile1, CertFile1}}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{providerId := ProvId} = _Env, Data) ->
        {ok, Provider} = oz_test_utils:get_provider(Config, ProvId),
        {ExpName, ExpLatitude, ExpLongitude} = case ShouldSucceed of
            false ->
                {Name, Latitude, Longitude};
            true ->
                {
                    maps:get(<<"clientName">>, Data, Name),
                    maps:get(<<"latitude">>, Data, Latitude),
                    maps:get(<<"longitude">>, Data, Longitude)
                }
        end,
        ?assertEqual(ExpName, Provider#od_provider.name),
        ?assertEqual(ExpLatitude, Provider#od_provider.latitude),
        ?assertEqual(ExpLongitude, Provider#od_provider.longitude)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                providerClient
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = <<"/provider">>,
            expected_code = ?HTTP_204_NO_CONTENT
        },
        data_spec = DataSpec = #data_spec{
            at_least_one = [<<"clientName">>, <<"latitude">>, <<"longitude">>],
            correct_values = #{
                <<"clientName">> => [?PROVIDER_NAME2],
                <<"latitude">> => [rand:uniform() * 90],
                <<"longitude">> => [rand:uniform() * 180]
            },
            bad_values = [
                {<<"clientName">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"clientName">>)},
                {<<"clientName">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"clientName">>)},
                {<<"latitude">>, <<"ASDASD">>, ?ERROR_BAD_VALUE_FLOAT(<<"latitude">>)},
                {<<"latitude">>, -1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"latitude">>, -90.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"latitude">>, 90.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"latitude">>, 1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"longitude">>, <<"ASDASD">>, ?ERROR_BAD_VALUE_FLOAT(<<"longitude">>)},
                {<<"longitude">>, -1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)},
                {<<"longitude">>, -180.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)},
                {<<"longitude">>, 180.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)},
                {<<"longitude">>, 1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check that clients can't make request on behalf of other client
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST
    ]),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),
    ApiTestSpec2 = #api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, Admin},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = update,
            args = [client, providerId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_provider, id = providerId, aspect = instance},
            expected_result = ?OK
        },
        data_spec = DataSpec
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_DELETE
    ]),

    EnvSetUpFun = fun() ->
        {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
            Config, ?PROVIDER_NAME1
        ),
        #{providerId => P1, providerClient => {provider, P1, KeyFile1, CertFile1}}
    end,
    DeleteEntityFun = fun(#{providerId := ProviderId} = _Env) ->
        oz_test_utils:delete_provider(Config, ProviderId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{providerId := ProviderId} = _Env, _) ->
        {ok, Providers} = oz_test_utils:list_providers(Config),
        ?assertEqual(lists:member(ProviderId, Providers), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/providers/">>, providerId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = delete,
            args = [client, providerId],
            expected_result = ?OK
        }
        % TODO VFS-3902
%%        gs_spec = #gs_spec{
%%            operation = delete,
%%            gri = #gri{type = od_provider, id = providerId, aspect = instance},
%%            expected_result = ?OK
%%        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )),

    % Also check that provider can delete himself
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{correct = [providerClient]}
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_self_test(Config) ->
    EnvSetUpFun = fun() ->
        {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
            Config, ?PROVIDER_NAME1
        ),
        #{providerId => P1, providerClient => {provider, P1, KeyFile1, CertFile1}}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{providerId := ProviderId} = _Env, _) ->
        {ok, Providers} = oz_test_utils:list_providers(Config),
        ?assertEqual(lists:member(ProviderId, Providers), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [providerClient]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = <<"/provider">>,
            expected_code = ?HTTP_202_ACCEPTED
        }
        % TODO VFS-3902
%%        gs_spec = #gs_spec{
%%            operation = delete,
%%            gri = #gri{type = od_provider, id = ?SELF, aspect = instance},
%%            expected_result = ?OK
%%        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


list_eff_users_test(Config) ->
    {
        {P1, KeyFile1, CertFile1}, _S1, _Groups, Users, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_provider_eff_users_env(Config),

    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST_USERS
    ]),

    [{U3, _}, {U4, _}, {U5, _}, {U6, _}] = Users,
    ExpUsers = [U1, U2, U3, U4, U5, U6],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, U2},
                {user, NonAdmin},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/effective_users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = get_eff_users,
            args = [client, P1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:has_eff_user function
    lists:foreach(
        fun(UserId) ->
            ?assert(oz_test_utils:call_oz(
                Config, provider_logic, has_eff_user, [P1, UserId])
            )
        end, ExpUsers
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, provider_logic, has_eff_user, [P1, <<"asdiucyaie827346w">>])
    ).


get_eff_user_test(Config) ->
    {
        {P1, KeyFile1, CertFile1}, S1, _Groups, EffUsers, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_provider_eff_users_env(Config),

    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),
    {ok, S1} = oz_test_utils:support_space(
        Config, P2, S1, oz_test_utils:minimum_support_size(Config)
    ),

    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST_USERS
    ]),

    lists:foreach(
        fun({UserId, UserDetails}) ->
            ExpDetails = UserDetails#{
                <<"emailList">> => [],
                <<"linkedAccounts">> => []
            },
            ExpDetailsWithoutAlias = maps:remove(<<"alias">>, ExpDetails),

            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, Admin},
                        {provider, P1, KeyFile1, CertFile1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, U1},
                        {user, U2},
                        {user, NonAdmin},
                        {provider, P2, KeyFile2, CertFile2}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/providers/">>, P1, <<"/effective_users/">>, UserId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = ExpDetails#{<<"userId">> => UserId}
                },
                logic_spec = #logic_spec{
                    module = provider_logic,
                    function = get_eff_user,
                    args = [client, P1, UserId],
                    expected_result = ?OK_MAP(ExpDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_user, id = UserId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_PROVIDER(P1),
                    expected_result = ?OK_MAP(ExpDetailsWithoutAlias#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Id, UserId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffUsers
    ).


list_eff_groups_test(Config) ->
    {
        {P1, KeyFile1, CertFile1}, _S1, Groups, _Users, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_provider_eff_users_env(Config),

    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST_GROUPS
    ]),

    [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}, {G6, _}] = Groups,
    ExpGroups = [G1, G2, G3, G4, G5, G6],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, U2},
                {user, NonAdmin},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/effective_groups">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = get_eff_groups,
            args = [client, P1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:has_eff_group function
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:call_oz(
                Config, provider_logic, has_eff_group, [P1, GroupId])
            )
        end, ExpGroups
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, provider_logic, has_eff_group, [P1, <<"asdiucyaie827346w">>])
    ).


get_eff_group_test(Config) ->
    {
        {P1, KeyFile1, CertFile1}, S1, EffGroups, _Users, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_provider_eff_users_env(Config),

    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),
    {ok, S1} = oz_test_utils:support_space(
        Config, P2, S1, oz_test_utils:minimum_support_size(Config)
    ),

    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST_GROUPS
    ]),

    lists:foreach(
        fun({GroupId, GroupDetails}) ->
            GroupDetailsBinary = GroupDetails#{
                <<"type">> => atom_to_binary(
                    maps:get(<<"type">>, GroupDetails), utf8
                )
            },

            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, Admin},
                        {provider, P1, KeyFile1, CertFile1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, U1},
                        {user, U2},
                        {user, NonAdmin},
                        {provider, P2, KeyFile2, CertFile2}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/providers/">>, P1, <<"/effective_groups/">>, GroupId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = GroupDetailsBinary#{
                        <<"groupId">> => GroupId
                    }
                },
                logic_spec = #logic_spec{
                    module = provider_logic,
                    function = get_eff_group,
                    args = [client, P1, GroupId],
                    expected_result = ?OK_MAP(GroupDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_group, id = GroupId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_PROVIDER(P1),
                    expected_result = ?OK_MAP(GroupDetailsBinary#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Id, GroupId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffGroups
    ).


list_spaces_test(Config) ->
    {
        {P1, KeyFile1, CertFile1}, {P2, KeyFile2, CertFile2},
        [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}]
    } = create_2_providers_and_5_supported_spaces(Config),

    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST_SPACES
    ]),

    ExpSpaces = [S1, S2, S3, S4, S5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/spaces">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = get_spaces,
            args = [client, P1],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_self_spaces_test(Config) ->
    {
        {P1, KeyFile1, CertFile1}, _,
        [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}]
    } = create_2_providers_and_5_supported_spaces(Config),

    ExpSpaces = [S1, S2, S3, S4, S5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, KeyFile1, CertFile1}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/provider/spaces">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_space_test(Config) ->
    {
        {P1, KeyFile1, CertFile1}, {P2, KeyFile2, CertFile2}, Spaces
    } = create_2_providers_and_5_supported_spaces(Config),

    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST_SPACES
    ]),

    lists:foreach(
        fun({SpaceId, SpaceDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, Admin},
                        {provider, P1, KeyFile1, CertFile1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin},
                        {provider, P2, KeyFile2, CertFile2}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/providers/">>, P1, <<"/spaces/">>, SpaceId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = SpaceDetails#{<<"spaceId">> => SpaceId}
                },
                logic_spec = #logic_spec{
                    operation = get,
                    module = provider_logic,
                    function = get_space,
                    args = [client, P1, SpaceId],
                    expected_result = ?OK_MAP(SpaceDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_space, id = SpaceId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_PROVIDER(P1),
                    expected_result = ?OK_MAP(SpaceDetails#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Id, SpaceId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, Spaces
    ).


get_self_space_test(Config) ->
    {
        {P1, KeyFile1, CertFile1}, _, Spaces
    } = create_2_providers_and_5_supported_spaces(Config),

    lists:foreach(
        fun({SpaceId, SpaceDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [{provider, P1, KeyFile1, CertFile1}]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/provider/spaces/">>, SpaceId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = SpaceDetails#{<<"spaceId">> => SpaceId}
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, Spaces
    ).


support_space_test(Config) ->
    MinSupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME1
    ),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, BadMacaroon1} = oz_test_utils:space_invite_user_token(Config, ?USER(U1), S1),
    {ok, BadTokenType1} = token_logic:serialize(BadMacaroon1),

    % Reused in all specs
    BadValues = [
        {<<"token">>, <<"bad-token">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
        {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
        {<<"token">>, BadTokenType1, ?ERROR_BAD_VALUE_BAD_TOKEN_TYPE(<<"token">>)},
        {<<"size">>, <<"binary">>, ?ERROR_BAD_VALUE_INTEGER(<<"size">>)},
        {<<"size">>, 0, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)},
        {<<"size">>, -1000, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)},
        {<<"size">>, MinSupportSize - 1, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)}
    ],

    VerifyFun = fun(SpaceId) ->
        % Should return space id of the newly supported space
        {ok, #od_space{
            providers = Providers
        }} = oz_test_utils:get_space(Config, SpaceId),

        % Test also provider_logic:supports_space fun
        DoesP1Supports = oz_test_utils:call_oz(
            Config, provider_logic, supports_space, [P1, SpaceId]
        ) andalso maps:is_key(P1, Providers),
        DoesP2Supports = oz_test_utils:call_oz(
            Config, provider_logic, supports_space, [P2, SpaceId]
        ) andalso maps:is_key(P2, Providers),

        DoesP1Supports orelse DoesP2Supports
    end,

    % Check only REST first
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            % Both providers are correct clients, as this is an endpoint
            % dedicated for provider that presents a cert
            correct = [
                root,
                {provider, P1, KeyFile1, CertFile1},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider/spaces/support">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                <<"/provider/spaces/", SpaceId/binary>> = Location,
                VerifyFun(SpaceId)
            end
        },
        data_spec = #data_spec{
            required = [<<"token">>, <<"size">>],
            correct_values = #{
                <<"token">> => [fun() ->
                    % Create a new space and token for every test case
                    % (this value is reused multiple times as many cases of
                    % the api test must be checked).
                    {ok, Space} = oz_test_utils:create_space(
                        Config, ?USER(U1), ?SPACE_NAME2
                    ),
                    {ok, Macaroon} = oz_test_utils:space_invite_provider_token(
                        Config, ?USER(U1), Space
                    ),
                    {ok, TokenBin} = token_logic:serialize(Macaroon),
                    TokenBin
                end],
                <<"size">> => [MinSupportSize]
            },
            bad_values = BadValues
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check logic endpoints - here we have to specify provider Id, so
    % different clients will be authorized.
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            % Only provider 1 is authorized to perform support operation on
            % behalf of provider 1.
            correct = [
                root,
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = provider_logic,
            function = support_space,
            args = [client, P1, data],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % provider_logic should also allow using non-serialized macaroons, check it
    {ok, BadMacaroon3} = oz_test_utils:space_invite_user_token(
        Config, ?USER(U1), S1
    ),
    ApiTestSpec3 = ApiTestSpec2#api_test_spec{
        % client_spec and logic_spec are inherited from ApiTestSpec
        data_spec = #data_spec{
            required = [<<"token">>, <<"size">>],
            correct_values = #{
                <<"token">> => [fun() ->
                    % Create a new space and token for every test case
                    % (this value is reused multiple times as many cases of
                    % the api test must be checked).
                    {ok, Space} = oz_test_utils:create_space(
                        Config, ?USER(U1), <<"space">>
                    ),
                    {ok, Macaroon} = oz_test_utils:space_invite_provider_token(
                        Config, ?USER(U1), Space
                    ),
                    Macaroon
                end],
                <<"size">> => [MinSupportSize]
            },
            bad_values = BadValues ++ [
                {<<"token">>, BadMacaroon3, ?ERROR_BAD_VALUE_BAD_TOKEN_TYPE(<<"token">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)).


update_support_size_test(Config) ->
    MinSupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME1
    ),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
        {ok, S1} = oz_test_utils:support_space(Config, P1, S1, MinSupportSize),
        {ok, S1} = oz_test_utils:support_space(Config, P2, S1, MinSupportSize),
        #{spaceId => S1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, Data) ->
        {ok, #od_space{
            providers = #{P1 := SupportSize}
        }} = oz_test_utils:get_space(Config, SpaceId),

        ExpSupportSize = case ShouldSucceed of
            true -> maps:get(<<"size">>, Data);
            false -> MinSupportSize
        end,
        ?assertEqual(SupportSize, ExpSupportSize)
    end,

    % Check only REST first
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, KeyFile1, CertFile1}]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/provider/spaces/">>, spaceId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        data_spec = #data_spec{
            required = [<<"size">>],
            correct_values = #{
                <<"size">> => [MinSupportSize]
            },
            bad_values = [
                {<<"size">>, <<"binary">>, ?ERROR_BAD_VALUE_INTEGER(<<"size">>)},
                {<<"size">>, 0, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)},
                {<<"size">>, -1000, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)},
                {<<"size">>, MinSupportSize - 1, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check logic endpoints - here we have to specify provider Id, so
    % different clients will be authorized.
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = provider_logic,
            function = update_support_size,
            args = [client, P1, spaceId, data],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


revoke_support_test(Config) ->
    MinSupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME1
    ),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
        {ok, S1} = oz_test_utils:support_space(Config, P1, S1, MinSupportSize),
        {ok, S1} = oz_test_utils:support_space(Config, P2, S1, MinSupportSize),
        #{spaceId => S1}
    end,
    DeleteEntityFun = fun(#{spaceId := SpaceId} = _Env) ->
        oz_test_utils:unsupport_space(Config, P1, SpaceId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _Data) ->
        {ok, #od_space{
            providers = Providers
        }} = oz_test_utils:get_space(Config, SpaceId),
        ?assertEqual(not ShouldSucceed, maps:is_key(P1, Providers))
    end,

    % Check only REST first, and only one correct client at once as a space
    % can only be unsupported once.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            % This is an endpoint dedicated for the provider that presents
            % its certs, no need to check other providers
            correct = [{provider, P1, KeyFile1, CertFile1}]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/provider/spaces/">>, spaceId],
            % TODO VFS-2918
%%            expected_code = ?HTTP_204_NO_CONTENT
            expected_code = ?HTTP_202_ACCEPTED
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )),

    % Check logic endpoint (here provider id must be provided)
    % Check all correct clients one by one
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = revoke_support,
            args = [client, P1, spaceId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


check_my_ports_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME1
    ),
    CorrectData = #{
        <<"undefined">> => <<"http://url1.com">>,
        <<"service1">> => <<"http://service1.com">>,
        <<"service2">> => <<"http://service2.com">>,
        <<"service3">> => <<"http://service3.com">>
    },
    RequiredKeys = maps:keys(CorrectData),
    ExpectedBody = #{
        <<"http://url1.com">> => ok,
        <<"http://service1.com">> => ok,
        <<"http://service2.com">> => error,
        <<"http://service3.com">> => error
    },

    % http_client is mocked in init_per_testcase to return proper values.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody, {provider, P1, KeyFile1, CertFile1}]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider/test/check_my_ports">>,
            expected_code = ?HTTP_200_OK,
            % Convert atoms to binaries in expected body for REST
            expected_body = maps:map(fun(_, ValAtom) ->
                atom_to_binary(ValAtom, utf8)
            end, ExpectedBody)
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = check_my_ports,
            args = [client, data],
            expected_result = ?OK_TERM(fun(Result) ->
                Result =:= ExpectedBody
            end)
        },
        % TODO gs
        data_spec = #data_spec{
            required = RequiredKeys,
            correct_values = maps:map(
                fun(_, Val) ->
                    [Val]
                end, CorrectData)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


check_my_ip_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME1
    ),
    ClientIP = list_to_binary(os:cmd("hostname -i") -- "\n"),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {provider, P1, KeyFile1, CertFile1}
            ]
        },
        % This endpoint makes sense only via REST
        rest_spec = #rest_spec{
            method = get,
            path = <<"/provider/test/check_my_ip">>,
            expected_code = ?HTTP_200_OK,
            expected_body = json_utils:encode_map(ClientIP)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


map_group_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME1
    ),
    MappedGroupHash = idp_group_mapping:group_spec_to_db_id(?MAPPED_GROUP_SPEC),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, P1, KeyFile1, CertFile1}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider/test/map_idp_group">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groupId">> => MappedGroupHash}
        },
        data_spec = #data_spec{
            required = [<<"idp">>, <<"groupId">>],
            correct_values = #{
                <<"idp">> => [?MAP_GROUP_TEST_AUTH_BIN],
                <<"groupId">> => [<<"group1">>]
            },
            bad_values = [
                {<<"idp">>, bad_bool, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"idp">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_subdomain_test(Config) ->
    {ok, OZDomainString} = oz_test_utils:get_oz_domain(Config),
    OZDomain = list_to_binary(OZDomainString),
    Subdomain = <<"proper-subdomain">>,
    IPs = [<<"1.2.3.4">>, <<"5.6.7.8">>],

    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),

    EnvSetUpFun = fun() ->
        {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
            Config, ?PROVIDER_NAME1
        ),
        #{providerId => P1, providerClient => {provider, P1, KeyFile1, CertFile1}}
    end,
    EnvTearDownFun = fun(#{providerId := ProviderId} = _Env) ->
        % delete provider to avoid "subdomain occupied" errors
        oz_test_utils:delete_provider(Config, ProviderId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{providerId := ProviderId} = _Env, _) ->
        {ExpSubdomain, ExpDomain} = case ShouldSucceed of
            false -> {undefined, <<"127.0.0.1">>}; % as set in create_provider_and_certs
            true -> {Subdomain, <<Subdomain/binary, ".", OZDomain/binary>>}
        end,
        {ok, Provider} = oz_test_utils:get_provider(Config, ProviderId),
        ?assertEqual(ExpDomain, Provider#od_provider.domain),
        ?assertEqual(ExpSubdomain, Provider#od_provider.subdomain),
        ?assertEqual(ShouldSucceed, Provider#od_provider.subdomain_delegation),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                providerClient
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = update_domain_config,
            args = [client, providerId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{
                type = od_provider, id = providerId, aspect = domain_config
            },
            expected_result = ?OK
        },
        data_spec = DataSpec = #data_spec{
            required = [
                <<"subdomainDelegation">>, <<"subdomain">>, <<"ipList">>
            ],
            correct_values = #{
                <<"subdomainDelegation">> => [true],
                <<"subdomain">> => [Subdomain],
                <<"ipList">> => [IPs]
            },
            bad_values = [
                {<<"subdomainDelegation">>, bad_bool, ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
                {<<"subdomain">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"subdomain">>)},
                {<<"subdomain">>, <<"port:443">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<"Uppercase">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<"under_score">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<".leadingdot">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<"trailingdot.">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<"multi.part">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"subdomain">>, <<"https://protocol">>, ?ERROR_BAD_VALUE_SUBDOMAIN},
                {<<"ipList">>, [atom], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, atom, ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, [<<"256.256.256.256">>], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, VerifyEndFun
    )),

    % Logic can also accept IPs as tuples,
    % check if they are also properly validated.
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        gs_spec = undefined,
        data_spec = DataSpec#data_spec{
            bad_values = [
                {<<"ipList">>, [{256,256,256,256}], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, [{-1,-1,-1,-1}], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, [{1,1,1,1,1}], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, EnvTearDownFun, VerifyEndFun
    )).


update_domain_test(Config) ->
    Subdomain = <<"prov-sub">>,
    ProviderDetails = #{
        <<"name">> => ?PROVIDER_NAME1,
        <<"subdomain">> => Subdomain,
        <<"ipList">> => [<<"2.4.6.8">>, <<"255.253.251.2">>],
        <<"subdomainDelegation">> => true
    },
    {ok, OZDomainString} = oz_test_utils:get_oz_domain(Config),
    OZDomain = list_to_binary(OZDomainString),

    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),

    EnvSetUpFun = fun() ->
        {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
            Config, ProviderDetails
        ),
        #{providerId => P1, providerClient => {provider, P1, KeyFile1, CertFile1}}
    end,
    EnvTearDownFun = fun(#{providerId := ProviderId} = _Env) ->
        % delete provider to avoid "subdomain occupied" errors
        oz_test_utils:delete_provider(Config, ProviderId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{providerId := ProviderId} = _Env, Data) ->
        {ExpSubdomain, ExpDomain} = case ShouldSucceed of
            true -> {undefined, maps:get(<<"domain">>, Data)};
            false -> {Subdomain, <<Subdomain/binary, ".", OZDomain/binary>>}
        end,
        {ok, Provider} = oz_test_utils:get_provider(Config, ProviderId),
        ?assertEqual(ExpDomain, Provider#od_provider.domain),
        ?assertEqual(ExpSubdomain, Provider#od_provider.subdomain),
        ?assertEqual(not ShouldSucceed, Provider#od_provider.subdomain_delegation),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                providerClient
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = update_domain_config,
            args = [client, providerId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{
                type = od_provider, id = providerId, aspect = domain_config
            },
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [<<"subdomainDelegation">>, <<"domain">>],
            correct_values = #{
                <<"subdomainDelegation">> => [false],
                <<"domain">> => [<<"changed.pl">>, <<"172.17.0.5">>]
            },
            bad_values = [
                {<<"subdomainDelegation">>, bad_bool,
                    ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
                {<<"domain">>, <<"https://hasprotocol">>,
                    ?ERROR_BAD_VALUE_DOMAIN(<<"domain">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, VerifyEndFun
    )).


get_domain_config_test(Config) ->
    % Test without delegated subdomain

    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME1
    ),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),

    % Create two users, grant one of them the privilege to list providers.
    % They still shouldn't be able to read domain aspect
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST
    ]),

    ExpDomain = <<"127.0.0.1">>, % as set in create_provider_and_certs
    ExpBody = #{
        <<"subdomainDelegation">> => false,
        <<"domain">> => ExpDomain
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, Admin},
                {user, NonAdmin},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        logic_spec = LogicSpec = #logic_spec{
            module = provider_logic,
            function = get_domain_config,
            args = [client, P1],
            expected_result = ?OK_MAP(ExpBody)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{type = od_provider, id = P1, aspect = domain_config},
            expected_result = ?OK_MAP(ExpBody#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, P1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % test enabled subdomain delegation
    ExpSubdomain = <<"subdomain">>,
    ExpIPs = [{5,8,2,4}, {10,12,255,255}],
    ExpIPsBin = [<<"5.8.2.4">>, <<"10.12.255.255">>],
    {ok, OZDomain} = oz_test_utils:get_oz_domain(Config),
    ExpDomain2 = <<ExpSubdomain/binary, ".", (list_to_binary(OZDomain))/binary>>,

    oz_test_utils:enable_subdomain_delegation(Config, P1, ExpSubdomain, ExpIPs),

    ExpBody2 = #{
        <<"subdomainDelegation">> => true,
        <<"domain">> => ExpDomain2,
        <<"ipList">> => ExpIPs,
        <<"subdomain">> => ExpSubdomain
    },
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        logic_spec = LogicSpec#logic_spec{expected_result = ?OK_MAP(ExpBody2)},
        gs_spec = GsSpec#gs_spec{
            expected_result = ?OK_MAP(ExpBody2#{
                <<"ipList">> => ExpIPsBin,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, P1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_current_time_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME1
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {provider, P1, KeyFile1, CertFile1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/provider/test/get_current_time">>,
            expected_code = ?HTTP_200_OK,
            expected_body = fun(Value) ->
                maps:get(<<"timeMillis">>, Value) > 0
            end
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = get_current_time,
            args = [client],
            expected_result = ?OK_TERM(fun(Result) ->
                Result > 0
            end)
        }
        % TODO gs
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


init_per_testcase(check_my_ports_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, http_client, [passthrough]),
    ok = test_utils:mock_expect(Nodes, http_client, get,
        fun(Url, Headers, Body, Options) ->
            case Url of
                <<"http://url1.com">> ->
                    {ok, 200, [], <<"whatever body">>};
                <<"http://service1.com">> ->
                    {ok, 200, [], <<"service1">>};
                <<"http://service2.com">> ->
                    {ok, 200, [], <<"body not matching service name">>};
                <<"http://service3.com">> ->
                    {ok, 304, [], <<"">>};
                _ ->
                    meck:passthrough([Url, Headers, Body, Options])
            end
        end),
    init_per_testcase(default, Config);
init_per_testcase(map_group_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, auth_config),
    ok = test_utils:mock_expect(
        Nodes, auth_config, has_group_mapping_enabled, fun(_) -> true end
    ),
    ok = test_utils:mock_expect(Nodes, auth_config, get_auth_providers,
        fun() -> [?MAP_GROUP_TEST_AUTH | meck:passthrough([])] end
    ),
    ok = test_utils:mock_expect(Nodes, auth_config, get_provider_module,
        fun(_) -> ?MAP_GROUP_TEST_AUTH_MODULE end
    ),
    ok = test_utils:mock_new(
        Nodes, ?MAP_GROUP_TEST_AUTH_MODULE, [passthrough, non_strict]
    ),
    ok = test_utils:mock_expect(Nodes, ?MAP_GROUP_TEST_AUTH_MODULE,
        normalized_membership_spec, fun(_) -> ?MAPPED_MEMBERSHIP_SPEC end
    ),
    init_per_testcase(default, Config);
init_per_testcase(_, Config) ->
    Config.


end_per_testcase(check_my_ports_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, http_client),
    end_per_testcase(default, Config);
end_per_testcase(map_group_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, auth_config),
    test_utils:mock_unload(Nodes, ?MAP_GROUP_TEST_AUTH_MODULE),
    end_per_testcase(default, Config);
end_per_testcase(_, _Config) ->
    ok.


%%%===================================================================
%%% Helper functions
%%%===================================================================


create_2_providers_and_5_supported_spaces(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME1
    ),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME2
    ),
    SupportSize = oz_test_utils:minimum_support_size(Config),

    Spaces = lists:map(
        fun(_) ->
            Name = ?UNIQUE_STRING,
            {ok, SpaceId} = oz_test_utils:create_space(Config, ?ROOT, Name),
            {ok, SpaceId} = oz_test_utils:support_space(
                Config, P1, SpaceId, SupportSize
            ),
            {ok, SpaceId} = oz_test_utils:support_space(
                Config, P2, SpaceId, SupportSize
            ),
            SpaceDetails = #{
                <<"name">> => Name,
                <<"providersSupports">> => #{P1 => SupportSize, P2 => SupportSize}
            },
            {SpaceId, SpaceDetails}
        end, lists:seq(1, 5)
    ),

    {{P1, KeyFile1, CertFile1}, {P2, KeyFile2, CertFile2}, Spaces}.
