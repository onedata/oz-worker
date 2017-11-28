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


%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    create_test/1,
    create_with_subdomain_test/1,
    get_test/1,
    list_test/1,
    update_test/1,
    delete_test/1,
    get_eff_users_test/1,
    get_eff_groups_test/1,
    get_spaces_test/1,
    support_space_test/1,
    update_support_size_test/1,
    revoke_support_test/1,
    check_my_ports_test/1,
    check_my_ip_test/1,
    update_subdomaindomain_test/1,
    update_domain_test/1,
    update_domain_to_ip_address_test/1,
    get_domain_config_test/1
]).

all() ->
    ?ALL([
        create_test,
        create_with_subdomain_test,
        get_test,
        list_test,
        support_space_test,
        get_eff_users_test,
        get_eff_groups_test,
        get_spaces_test,
        update_test,
        delete_test,
        update_support_size_test,
        revoke_support_test,
        check_my_ports_test,
        check_my_ip_test,
        update_domain_test,
        update_subdomaindomain_test,
        update_domain_to_ip_address_test,
        get_domain_config_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

% TODO VFS-2918 (all tests) <<"clientName">> -> <<"name">>

create_test(Config) ->
    {_, CSRFile, _} = oz_test_utils:generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    ExpName = <<"ProvName">>,
    ExpDomain = <<"multilevel.provider-domain.org">>,
    ExpLatitude = 50.0,
    ExpLongitude = -24.8,
    ExpSubdomainDelegation = false,
    ExpSubdomain = undefined,

    VerifyFun = fun(ProviderId, Certificate) ->
        true = is_binary(Certificate),
        {ok, Provider} = oz_test_utils:get_provider(Config, ProviderId),
        ?assertEqual(ExpName, Provider#od_provider.name),
        ?assertEqual(ExpDomain, Provider#od_provider.domain),
        ?assertEqual(ExpSubdomainDelegation, Provider#od_provider.subdomain_delegation),
        ?assertEqual(ExpSubdomain, Provider#od_provider.subdomain),
        % Latitude and longitude are optional, so default value might
        % be set.
        ?assert(Provider#od_provider.latitude =:= ExpLatitude orelse
            Provider#od_provider.latitude =:= 0.0),
        ?assert(Provider#od_provider.longitude =:= ExpLongitude orelse
            Provider#od_provider.longitude =:= 0.0),
        true
    end,
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider">>,
            expected_code = ?HTTP_200_OK,
            expected_body = fun(Value) ->
                Certificate = maps:get(<<"certificate">>, Value),
                ProviderId = maps:get(<<"providerId">>, Value),
                VerifyFun(ProviderId, Certificate)
            end
        },
        logic_spec = #logic_spec{
            operation = create,
            module = provider_logic,
            function = create,
            args = [client, data],
            expected_result = ?OK_TERM(fun({ProviderId, Certificate}) ->
                VerifyFun(ProviderId, Certificate)
            end)
        },
        data_spec = #data_spec{
            required = [<<"clientName">>, <<"domain">>, <<"subdomainDelegation">>, <<"csr">>],
            optional = [<<"latitude">>, <<"longitude">>],
            correct_values = #{
                <<"clientName">> => [ExpName],
                <<"domain">> => [ExpDomain],
                <<"subdomainDelegation">> => [ExpSubdomainDelegation],
                <<"csr">> => [CSR],
                <<"latitude">> => [ExpLatitude],
                <<"longitude">> => [ExpLongitude]
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).

%% Create provider with subdomain delegation turned on
create_with_subdomain_test(Config) ->
    {_, CSRFile, _} = oz_test_utils:generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    ExpName = <<"ProvName">>,
    ExpLatitude = 50.0,
    ExpLongitude = -24.8,
    ExpSubdomainDelegation = true,
    ExpSubdomain = <<"prov-sub">>,
    ExpIPs = [<<"2.4.6.8">>, <<"255.253.251.2">>],
    {ok, OZDomainString} = oz_test_utils:call_oz(Config,
        application, get_env, [oz_worker, http_domain]),
    OZDomain = list_to_binary(OZDomainString),
    ExpDomain = <<ExpSubdomain/binary, ".", OZDomain/binary>>,

    VerifyFun = fun(ProviderId, Certificate) ->
        true = is_binary(Certificate),
        {ok, Provider} = oz_test_utils:get_provider(Config, ProviderId),
        ?assertEqual(ExpName, Provider#od_provider.name),
        ?assertEqual(ExpDomain, Provider#od_provider.domain),
        ?assertEqual(ExpSubdomainDelegation, Provider#od_provider.subdomain_delegation),
        ?assertEqual(ExpSubdomain, Provider#od_provider.subdomain),

        % delete provider to avoid "subdomain occupied" errors
        oz_test_utils:delete_provider(Config, ProviderId),
        true
    end,
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider">>,
            expected_code = ?HTTP_200_OK,
            expected_body = fun(Value) ->
                Certificate = maps:get(<<"certificate">>, Value),
                ProviderId = maps:get(<<"providerId">>, Value),
                VerifyFun(ProviderId, Certificate)
            end},
        logic_spec = #logic_spec{
            operation = create,
            module = provider_logic,
            function = create,
            args = [client, data],
            expected_result = ?OK_TERM(fun({ProviderId, Certificate}) ->
                VerifyFun(ProviderId, Certificate)
            end)
        },
        data_spec = #data_spec{
            required = [<<"clientName">>, <<"subdomain">>, <<"ipList">>,
                        <<"subdomainDelegation">>, <<"csr">>],
            optional = [<<"latitude">>, <<"longitude">>],
            correct_values = #{
                <<"clientName">> => [ExpName],
                <<"subdomainDelegation">> => [ExpSubdomainDelegation],
                <<"subdomain">> => [ExpSubdomain],
                <<"ipList">> => [ExpIPs],
                <<"csr">> => [CSR],
                <<"latitude">> => [ExpLatitude],
                <<"longitude">> => [ExpLongitude]
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
                {<<"subdomainDelegation">>, <<"binary">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
                {<<"subdomainDelegation">>, bad_bool, ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
                {<<"ipList">>, [atom], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, atom, ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, [<<"256.256.256.256">>], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_test(Config) ->
    % Create a provider manually
    {KeyFile1, CSRFile1, CertFile1} = oz_test_utils:generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile1),
    ExpName = <<"Provider 1">>,
    ExpDomain = <<"domain.com">>,
    ExpLatitude = 14.78,
    ExpLongitude = -106.12,
    {ok, {P1, Certificate}} = oz_test_utils:create_provider(Config, #{
        <<"clientName">> => ExpName,
        <<"domain">> => ExpDomain,
        <<"subdomainDelegation">> => false,
        <<"csr">> => CSR,
        <<"latitude">> => ExpLatitude,
        <<"longitude">> => ExpLongitude
    }),
    ok = file:write_file(CertFile1, Certificate),
    % Create a second provider
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P2">>
    ),
    % Create two users, grant one of them the privilege to list providers.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST
    ]),


    ExpBody = #{
        <<"name">> => ExpName,
        <<"clientName">> => ExpName,
        <<"domain">> => ExpDomain,
        <<"latitude">> => ExpLatitude,
        <<"longitude">> => ExpLongitude
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {provider, P1, KeyFile1, CertFile1},
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
            expected_body = ExpBody#{<<"providerId">> => P1}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = get_protected_data,
            args = [client, P1],
            expected_result = ?OK_MAP(ExpBody)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Provider 2 was not able to get data of provider 1, but if they support a
    % common space, it should gain access.
    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, <<"space">>),
    {ok, Macaroon1} = oz_test_utils:space_invite_provider_token(Config, ?ROOT, S1),
    {ok, Macaroon2} = oz_test_utils:space_invite_provider_token(Config, ?ROOT, S1),
    {ok, S1} = oz_test_utils:support_space(
        Config, ?PROVIDER(P1), P1, Macaroon1,
        oz_test_utils:minimum_support_size(Config)
    ),
    {ok, S1} = oz_test_utils:support_space(
        Config, ?PROVIDER(P2), P2, Macaroon2,
        oz_test_utils:minimum_support_size(Config)
    ),
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, P1, KeyFile1, CertFile1},
                {provider, P2, KeyFile2, CertFile2}
            ],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/providers/">>, P1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpBody#{<<"providerId">> => P1}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),


    % Provider should be also able to retrieve this info using another path,
    % without id (id is deduced from authorization)
    ApiTestSpec3 = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, KeyFile1, CertFile1}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/provider">>,
            expected_code = ?HTTP_200_OK,
            expected_body = ExpBody#{<<"providerId">> => P1}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)).


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
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST
    ]),


    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}, {provider, P1, KeyFile, CertFile}]
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
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    ExpName = <<"New Prov name">>,
    ExpDomain = <<"127.0.0.1">>, % not changed - it is done in the domain_config aspect
    ExpLatitude = -12.87645,
    ExpLongitude = -4.44,
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {provider, P1, KeyFile1, CertFile1}]
            % No need to check other clients - this endpoint is dedicated to the
            % provider that presents it certs.
            % root client is only checked in logic tests
        },
        rest_spec = #rest_spec{
            method = patch,
            path = <<"/provider">>,
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            operation = update,
            module = provider_logic,
            function = update,
            args = [client, P1, data],
            expected_result = ?OK
        },
        data_spec = #data_spec{
            at_least_one = [
                <<"clientName">>, <<"latitude">>, <<"longitude">>
            ],
            correct_values = #{
                <<"clientName">> => [ExpName],
                <<"latitude">> => [ExpLatitude],
                <<"longitude">> => [ExpLongitude]
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    {ok, Provider} = oz_test_utils:get_provider(Config, P1),
    ?assertEqual(ExpName, Provider#od_provider.name),
    ?assertEqual(ExpDomain, Provider#od_provider.domain),
    ?assertEqual(ExpLatitude, Provider#od_provider.latitude),
    ?assertEqual(ExpLongitude, Provider#od_provider.longitude).


delete_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    % Check REST endpoint (logic endpoint will be checked in later tests)
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, KeyFile1, CertFile1}]
            % No need to check other clients - this endpoint is dedicated to the
            % provider that presents it certs.
            % root client is only checked in logic tests
        },
        rest_spec = #rest_spec{
            method = delete,
            path = <<"/provider">>,
            % TODO VFS-2918
%%            expected_code = ?HTTP_204_NO_CONTENT
            expected_code = ?HTTP_202_ACCEPTED
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % There is also an admin endpoint for deleting providers, same provider
    % should also be able to use it, but other providers should not.
    % Correct clients must be checked one by one, as an entity cannot be
    % deleted multiple times.
    % Create two users, grant one of them the privilege to remove providers.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_DELETE
    ]),

    % Create one provider per every correct client to check all of them (REST)
    {ok, {P3, _KeyFile3, _CertFile3}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P3">>
    ),
    {ok, {P4, KeyFile4, CertFile4}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P4">>
    ),
    RestProvidersAndClients = [
        {P3, {user, Admin}},
        {P4, {provider, P4, KeyFile4, CertFile4}}
    ],
    lists:foreach(
        fun({Provider, CorrectClient}) ->
            ApiTestSpec2 = #api_test_spec{
                client_spec = #client_spec{
                    correct = [CorrectClient]
                },
                rest_spec = #rest_spec{
                    method = delete,
                    path = [<<"/providers/">>, Provider],
                    % TODO VFS-2918
%%                    expected_code = ?HTTP_204_NO_CONTENT
                    expected_code = ?HTTP_202_ACCEPTED
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))
        end, RestProvidersAndClients),

    % Create one provider per every correct client to check all of them (logic)
    {ok, {P5, _KeyFile5, _CertFile5}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P5">>
    ),
    {ok, {P6, _KeyFile6, _CertFile6}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P6">>
    ),
    {ok, {P7, KeyFile7, CertFile7}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P7">>
    ),
    LogicProvidersAndClients = [
        {P5, root},
        {P6, {user, Admin}},
        {P7, {provider, P7, KeyFile7, CertFile7}}
    ],
    lists:foreach(
        fun({Provider, CorrectClient}) ->
            ApiTestSpec2 = #api_test_spec{
                client_spec = #client_spec{
                    correct = [CorrectClient]
                },
                logic_spec = #logic_spec{
                    operation = delete,
                    module = provider_logic,
                    function = delete,
                    args = [client, Provider],
                    expected_result = ?OK
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))
        end, LogicProvidersAndClients),

    % Make sure that unauthorized and forbidden clients cannot delete providers
    {ok, {P8, KeyFile8, CertFile8}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P5">>
    ),
    {ok, {P9, KeyFile9, CertFile9}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P9">>
    ),
    ApiTestSpec3 = #api_test_spec{
        client_spec = #client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P9, KeyFile9, CertFile9}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/providers/">>, P8]
        },
        logic_spec = #logic_spec{
            operation = delete,
            module = provider_logic,
            function = delete,
            args = [client, P8]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)),

    % Make sure that provider cannot be deleted twice (P1 is already deleted)
    ApiTestSpec4 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {user, Admin},
                {user, NonAdmin},
                {provider, P8, KeyFile8, CertFile8}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/providers/">>, P1],
            expected_code = ?HTTP_404_NOT_FOUND
        },
        logic_spec = #logic_spec{
            operation = delete,
            module = provider_logic,
            function = delete,
            args = [client, P1],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec4)).



get_eff_users_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    % Create two users, grant one of them the privilege to list users.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST_USERS
    ]),

    ListApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [{provider, P2, KeyFile2, CertFile2}, {user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/effective_users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => []}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = get_eff_users,
            args = [client, P1],
            expected_result = ?OK_LIST([])
        }
    },
    GetApiTestSpecGenerator = fun(ExcludeOrInclude, UserId, UserName) ->
        ClientSpec = case ExcludeOrInclude of
            include ->
                #client_spec{
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
                };
            exclude ->
                % All clients should receive 404 when asking for non-existent
                % resource.
                #client_spec{
                    correct = [
                        root,
                        {user, Admin},
                        {user, NonAdmin},
                        nobody,
                        {provider, P1, KeyFile1, CertFile1},
                        {provider, P2, KeyFile2, CertFile2}
                    ]
                }
        end,
        ExpBodyContains = #{
            <<"name">> => UserName
        },
        {ExpectedRestCode, ExpectedRestBody} = case ExcludeOrInclude of
            include ->
                {?HTTP_200_OK, {contains, ExpBodyContains#{<<"userId">> => UserId}}};
            exclude ->
                {?HTTP_404_NOT_FOUND, undefined}
        end,
        ExpectedLogicResult = case ExcludeOrInclude of
            include ->
                ?OK_MAP_CONTAINS(ExpBodyContains);
            exclude ->
                ?ERROR_REASON(?ERROR_NOT_FOUND)
        end,
        #api_test_spec{
            client_spec = ClientSpec,
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/providers/">>, P1, <<"/effective_users/">>, UserId],
                expected_code = ExpectedRestCode,
                expected_body = ExpectedRestBody
            },
            logic_spec = #logic_spec{
                operation = get,
                module = provider_logic,
                function = get_eff_user,
                args = [client, P1, UserId],
                expected_result = ExpectedLogicResult
            }
        }
    end,
    CreateRelationFuns = [
        fun() ->
            [{S1, _}, {S2, _}, {S3, _}] =
                oz_test_utils:create_and_support_3_spaces(Config, P1),
            % Create two users for every space
            {include, lists:foldl(
                fun({Counter, Space}, AccMap) ->
                    UserName = str_utils:format_bin("u~B", [Counter]),
                    {ok, User} = oz_test_utils:create_user(
                        Config, #od_user{name = UserName}
                    ),
                    {ok, User} = oz_test_utils:space_add_user(Config, Space, User),
                    AccMap#{User => UserName}
                end, #{}, lists:zip(lists:seq(1, 6), [S1, S2, S3, S1, S2, S3])
            )}
        end,
        fun() ->
            [{S4, _}, {S5, _}, {S6, _}] =
                oz_test_utils:create_and_support_3_spaces(Config, P1),
            % Create two groups for every space and three users for every group
            {include, lists:foldl(
                fun({Counter, Space}, AccMap) ->
                    UserName1 = str_utils:format_bin("u1@g~B", [Counter]),
                    {ok, User1} = oz_test_utils:create_user(
                        Config, #od_user{name = UserName1}
                    ),
                    UserName2 = str_utils:format_bin("u2@g~B", [Counter]),
                    {ok, User2} = oz_test_utils:create_user(
                        Config, #od_user{name = UserName2}
                    ),
                    GroupName = str_utils:format_bin("g~B", [Counter]),
                    {ok, Group} = oz_test_utils:create_group(
                        Config, ?USER(User1), GroupName
                    ),
                    {ok, User2} = oz_test_utils:group_add_user(Config, Group, User2),
                    {ok, Group} = oz_test_utils:space_add_group(Config, Space, Group),
                    AccMap#{User1 => UserName1, User2 => UserName2}
                end, #{}, lists:zip(lists:seq(1, 6), [S4, S5, S6, S4, S5, S6])
            )}
        end,
        fun() ->
            % User via nested groups
            {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, <<"space">>),
            {ok, Macaroon1} = oz_test_utils:space_invite_provider_token(Config, ?ROOT, S1),
            {ok, S1} = oz_test_utils:support_space(
                Config, ?PROVIDER(P1), P1, Macaroon1,
                oz_test_utils:minimum_support_size(Config)
            ),
            {ok, TopGroup} = oz_test_utils:create_group(Config, ?ROOT, <<"gr">>),
            {ok, TopGroup} = oz_test_utils:space_add_group(Config, S1, TopGroup),
            {ok, BottomGroup} = oz_test_utils:create_group(Config, ?ROOT, <<"gr">>),
            {ok, BottomGroup} = oz_test_utils:group_add_group(
                Config, TopGroup, BottomGroup
            ),
            UserName = <<"un">>,
            {ok, User} = oz_test_utils:create_user(
                Config, #od_user{name = UserName}
            ),
            {ok, User} = oz_test_utils:group_add_user(Config, BottomGroup, User),
            {include, #{User => UserName}}
        end,
        fun() ->
            % Make sure that other users cannot be reached this way
            {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
            {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
            {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
            {exclude, #{U1 => [], U2 => [], U3 => []}}
        end

    ],
    ?assert(api_test_scenarios:run_scenario(get_relations, [
        Config, ListApiTestSpec, GetApiTestSpecGenerator, CreateRelationFuns
    ])).


get_eff_groups_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    % Create two users, grant one of them the privilege to list groups.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST_GROUPS
    ]),

    ListApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [{provider, P2, KeyFile2, CertFile2}, {user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/effective_groups">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => []}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = get_eff_groups,
            args = [client, P1],
            expected_result = ?OK_LIST([])
        }
    },
    GetApiTestSpecGenerator = fun(ExcludeOrInclude, GroupId, GroupName) ->
        ClientSpec = case ExcludeOrInclude of
            include ->
                #client_spec{
                    correct = [
                        root,
                        {user, Admin},
                        {provider, P1, KeyFile1, CertFile1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {provider, P2, KeyFile2, CertFile2},
                        {user, NonAdmin}
                    ]
                };
            exclude ->
                % All clients should receive 404 when asking for non-existent
                % resource.
                #client_spec{
                    correct = [
                        root,
                        {user, Admin},
                        {user, NonAdmin},
                        nobody,
                        {provider, P1, KeyFile1, CertFile1},
                        {provider, P2, KeyFile2, CertFile2}
                    ]
                }
        end,
        ExpBodyContains = #{
            <<"name">> => GroupName
        },
        {ExpectedRestCode, ExpectedRestBody} = case ExcludeOrInclude of
            include ->
                {?HTTP_200_OK, {contains, ExpBodyContains#{<<"groupId">> => GroupId}}};
            exclude ->
                {?HTTP_404_NOT_FOUND, undefined}
        end,
        ExpectedLogicResult = case ExcludeOrInclude of
            include ->
                ?OK_MAP_CONTAINS(ExpBodyContains);
            exclude ->
                ?ERROR_REASON(?ERROR_NOT_FOUND)
        end,
        #api_test_spec{
            client_spec = ClientSpec,
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/providers/">>, P1, <<"/effective_groups/">>, GroupId],
                expected_code = ExpectedRestCode,
                expected_body = ExpectedRestBody
            },
            logic_spec = #logic_spec{
                operation = get,
                module = provider_logic,
                function = get_eff_group,
                args = [client, P1, GroupId],
                expected_result = ExpectedLogicResult
            }
        }
    end,
    CreateRelationFuns = [
        fun() ->
            [{S1, _}, {S2, _}, {S3, _}] =
                oz_test_utils:create_and_support_3_spaces(Config, P1),
            [{S4, _}, {S5, _}, {S6, _}] =
                oz_test_utils:create_and_support_3_spaces(Config, P1),
            % Create two groups for every space
            {include, lists:foldl(
                fun({Counter, Space}, AccMap) ->
                    GroupName = str_utils:format_bin("g~B", [Counter]),
                    {ok, Group} = oz_test_utils:create_group(
                        Config, ?ROOT, GroupName
                    ),
                    {ok, Group} = oz_test_utils:space_add_group(Config, Space, Group),
                    AccMap#{Group => GroupName}
                end, #{}, lists:zip(lists:seq(1, 6), [S1, S2, S3, S4, S5, S6])
            )}
        end,
        fun() ->
            % Nested groups
            {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, <<"space">>),
            {ok, Macaroon1} = oz_test_utils:space_invite_provider_token(Config, ?ROOT, S1),
            {ok, S1} = oz_test_utils:support_space(
                Config, ?PROVIDER(P1), P1, Macaroon1,
                oz_test_utils:minimum_support_size(Config)
            ),
            TopGroupName = <<"tgn">>,
            {ok, TopGroup} = oz_test_utils:create_group(
                Config, ?ROOT, TopGroupName
            ),
            {ok, TopGroup} = oz_test_utils:space_add_group(Config, S1, TopGroup),
            BottomGroupName = <<"bgn">>,
            {ok, BottomGroup} = oz_test_utils:create_group(
                Config, ?ROOT, BottomGroupName
            ),
            {ok, BottomGroup} = oz_test_utils:group_add_group(
                Config, TopGroup, BottomGroup
            ),
            {include, #{TopGroup => TopGroupName, BottomGroup => BottomGroupName}}
        end,
        fun() ->
            % Make sure that other users cannot be reached this way
            {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, <<"other">>),
            {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, <<"other">>),
            {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, <<"other">>),
            {exclude, #{G1 => [], G2 => [], G3 => []}}
        end

    ],
    ?assert(api_test_scenarios:run_scenario(get_relations, [
        Config, ListApiTestSpec, GetApiTestSpecGenerator, CreateRelationFuns
    ])).


get_spaces_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    % Create two users, grant one of them the privilege to list spaces.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_PROVIDERS_LIST_SPACES
    ]),

    ListApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}, {provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/spaces">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => []}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = get_spaces,
            args = [client, P1],
            expected_result = ?OK_LIST([])
        }
    },
    GetApiTestSpecGenerator = fun(ExcludeOrInclude, SpaceId, SpaceName) ->
        ClientSpec = case ExcludeOrInclude of
            include ->
                #client_spec{
                    correct = [
                        root,
                        {user, Admin},
                        {provider, P1, KeyFile1, CertFile1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [{user, NonAdmin}]
                };
            exclude ->
                % All clients should receive 404 when asking for non-existent
                % resource.
                #client_spec{
                    correct = [
                        root,
                        {user, Admin},
                        {provider, P1, KeyFile1, CertFile1},
                        {user, NonAdmin}
                    ]
                }
        end,
        ExpBodyContains = #{
            <<"name">> => SpaceName
        },
        {ExpectedRestCode, ExpectedRestBody} = case ExcludeOrInclude of
            include ->
                {?HTTP_200_OK, {contains, ExpBodyContains#{<<"spaceId">> => SpaceId}}};
            exclude ->
                {?HTTP_404_NOT_FOUND, undefined}
        end,
        ExpectedLogicResult = case ExcludeOrInclude of
            include ->
                ?OK_MAP_CONTAINS(ExpBodyContains);
            exclude ->
                ?ERROR_REASON(?ERROR_NOT_FOUND)
        end,
        #api_test_spec{
            client_spec = ClientSpec,
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/providers/">>, P1, <<"/spaces/">>, SpaceId],
                expected_code = ExpectedRestCode,
                expected_body = ExpectedRestBody
            },
            logic_spec = #logic_spec{
                operation = get,
                module = provider_logic,
                function = get_space,
                args = [client, P1, SpaceId],
                expected_result = ExpectedLogicResult
            }
        }
    end,
    CreateRelationFuns = [
        fun() ->
            ExpSpaces = oz_test_utils:create_and_support_3_spaces(Config, P1),
            {include, maps:from_list(ExpSpaces)}
        end,
        fun() ->
            % Make sure that other users cannot be reached this way
            {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, <<"other">>),
            {ok, S2} = oz_test_utils:create_space(Config, ?ROOT, <<"other">>),
            {ok, S3} = oz_test_utils:create_space(Config, ?ROOT, <<"other">>),
            {exclude, #{S1 => [], S2 => [], S3 => []}}
        end

    ],
    ?assert(api_test_scenarios:run_scenario(get_relations, [
        Config, ListApiTestSpec, GetApiTestSpecGenerator, CreateRelationFuns
    ])),

    % Check endpoint dedicated for providers
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P2">>
    ),
    ListApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P2, KeyFile2, CertFile2}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/provider/spaces">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => []}
        }
    },
    GetApiTestSpecGenerator2 = fun(ExcludeOrInclude, SpaceId, SpaceName) ->
        ExpBodyContains = #{
            <<"name">> => SpaceName
        },
        {ExpectedRestCode, ExpectedRestBody} = case ExcludeOrInclude of
            include ->
                {?HTTP_200_OK, {contains, ExpBodyContains#{<<"spaceId">> => SpaceId}}};
            exclude ->
                {?HTTP_404_NOT_FOUND, undefined}
        end,
        #api_test_spec{
            client_spec = #client_spec{
                correct = [{provider, P2, KeyFile2, CertFile2}]
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/provider/spaces/">>, SpaceId],
                expected_code = ExpectedRestCode,
                expected_body = ExpectedRestBody
            }
        }
    end,
    CreateRelationFuns2 = [
        fun() ->
            ExpSpaces = oz_test_utils:create_and_support_3_spaces(Config, P2),
            {include, maps:from_list(ExpSpaces)}
        end,
        fun() ->
            % Make sure that other users cannot be reached this way
            {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, <<"other">>),
            {ok, S2} = oz_test_utils:create_space(Config, ?ROOT, <<"other">>),
            {ok, S3} = oz_test_utils:create_space(Config, ?ROOT, <<"other">>),
            {exclude, #{S1 => [], S2 => [], S3 => []}}
        end

    ],
    ?assert(api_test_scenarios:run_scenario(get_relations, [
        Config, ListApiTestSpec2, GetApiTestSpecGenerator2, CreateRelationFuns2
    ])).


support_space_test(Config) ->
    % TODO VFS-2918
%%    RestPrefix = rest_test_utils:get_rest_api_prefix(Config),
    MinSupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P2">>
    ),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), <<"S1">>),
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
        maps:is_key(P1, Providers) orelse maps:is_key(P2, Providers)
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
            expected_headers = fun(Headers) ->
                % TODO VFS-2918
%%                PrefLen = size(RestPrefix),
%%                <<RestPrefix:PrefLen/binary, "/provider/spaces/", SpaceId/binary>> =
%%                    maps:get(<<"location">>, Headers),
%%                VerifyFun(SpaceId)
                <<"/provider/spaces/", SpaceId/binary>> = maps:get(<<"location">>, Headers),
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
                        Config, ?USER(U1), <<"space">>
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
            correct = [root, {provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody],
            forbidden = [{user, U1}, {provider, P2, KeyFile2, CertFile2}]
        },
        logic_spec = #logic_spec{
            operation = create,
            module = provider_logic,
            function = support_space,
            args = [client, P1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        rest_spec = undefined
        % data_spec is inherited from ApiTestSpec
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
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(Config, <<"P1">>),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(Config, <<"P2">>),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), <<"space">>),
    {ok, Macaroon} = oz_test_utils:space_invite_provider_token(
        Config, ?USER(U1), S1
    ),
    {ok, S1} = oz_test_utils:support_space(
        Config, ?PROVIDER(P1), P1, Macaroon, MinSupportSize
    ),

    % Reused in all specs
    BadValues = [
        {<<"size">>, <<"binary">>, ?ERROR_BAD_VALUE_INTEGER(<<"size">>)},
        {<<"size">>, 0, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)},
        {<<"size">>, -1000, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)},
        {<<"size">>, MinSupportSize - 1, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)}
    ],

    % Check only REST first
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            % This is an endpoint dedicated for the provider that presents
            % its certs, no need to check other providers
            correct = [root, {provider, P1, KeyFile1, CertFile1}]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/provider/spaces/">>, S1],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        data_spec = #data_spec{
            required = [<<"size">>],
            correct_values = #{
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
            % Only provider 1 is authorized to perform change support size
            % operation on behalf of provider 1.
            correct = [root, {provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody],
            forbidden = [{user, U1}, {provider, P2, KeyFile2, CertFile2}]
        },
        logic_spec = #logic_spec{
            operation = update,
            module = provider_logic,
            function = update_support_size,
            args = [client, P1, S1, data],
            expected_result = ?OK
        },
        rest_spec = undefined
        % data_spec is inherited from ApiTestSpec
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


revoke_support_test(Config) ->
    MinSupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P2">>
    ),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), <<"space">>),
    {ok, Macaroon} = oz_test_utils:space_invite_provider_token(
        Config, ?USER(U1), S1
    ),
    {ok, S1} = oz_test_utils:support_space(
        Config, ?PROVIDER(P1), P1, Macaroon, MinSupportSize
    ),

    % Check only REST first, and only one correct client at once as a space
    % can only be unsupported once.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            % This is an endpoint dedicated for the provider that presents
            % its certs, no need to check other providers
            correct = [{provider, P1, KeyFile1, CertFile1}]
        },
        rest_spec = RestSpec = #rest_spec{
            method = delete,
            path = [<<"/provider/spaces/">>, S1],
            % TODO VFS-2918
%%            expected_code = ?HTTP_204_NO_CONTENT
            expected_code = ?HTTP_202_ACCEPTED
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Second attempt to unsupport the same space should return 404
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            expected_code = ?HTTP_404_NOT_FOUND
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % Check logic endpoint (here provider id must be provided)
    % Check all correct clients one by one
    CorrectClients = [root, {provider, P1, KeyFile1, CertFile1}],
    lists:foreach(
        fun(CorrectClient) ->
            % Support the space again
            {ok, Macaroon2} = oz_test_utils:space_invite_provider_token(
                Config, ?USER(U1), S1
            ),
            {ok, S1} = oz_test_utils:support_space(
                Config, ?PROVIDER(P1), P1, Macaroon2, MinSupportSize
            ),
            % First check clients that should fail
            ApiTestSpec3 = ApiTestSpec#api_test_spec{
                client_spec = #client_spec{
                    unauthorized = [nobody],
                    forbidden = [
                        {user, U1},
                        {provider, P2, KeyFile2, CertFile2}
                    ]
                },
                rest_spec = undefined,
                logic_spec = #logic_spec{
                    operation = delete,
                    module = provider_logic,
                    function = revoke_support,
                    args = [client, P1, S1],
                    expected_result = ?OK
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)),
            % Now clients that should succeed
            ApiTestSpec4 = ApiTestSpec3#api_test_spec{
                client_spec = #client_spec{
                    correct = [CorrectClient]
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec4))
        end, CorrectClients),

    % Check if unsupporting the space again via logic returns not found error.
    ApiTestSpec5 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                nobody,
                root,
                {user, U1},
                {provider, P1, KeyFile1, CertFile1},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            operation = delete,
            module = provider_logic,
            function = revoke_support,
            args = [client, P1, S1],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec5)).


check_my_ports_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
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
            operation = create,
            module = provider_logic,
            function = check_my_ports,
            args = [client, data],
            expected_result = ?OK_TERM(fun(Result) ->
                Result =:= ExpectedBody
            end)
        },
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
        Config, <<"P1">>
    ),
    ClientIP = list_to_binary(os:cmd("hostname -i") -- "\n"),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody, {provider, P1, KeyFile1, CertFile1}]
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


update_subdomaindomain_test(Config) ->
    ExpSubdomain = <<"proper-subdomain">>,
    ExpIPs = [{1,2,3,4}, <<"5.6.7.8">>],

    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    % Test enabling subdomain delegation
    Delegated_ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            operation = update,
            module = provider_logic,
            function = update_domain_config,
            args = [client, P1, data],
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [
                <<"subdomainDelegation">>, <<"subdomain">>, <<"ipList">>
            ],
            correct_values = #{
                <<"subdomainDelegation">> => [true],
                <<"subdomain">> => [ExpSubdomain],
                <<"ipList">> => [ExpIPs]
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
                {<<"ipList">>, [{256,256,256,256}], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, [{-1,-1,-1,-1}], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, [{1,1,1,1,1}], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, [<<"256.256.256.256">>], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, Delegated_ApiTestSpec)),

    % Test disabling subdomain delegation
    ExpDomain = <<"provider.domain.org">>,
    Undelegated_ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            operation = update,
            module = provider_logic,
            function = update_domain_config,
            args = [client, P1, data],
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [
                <<"subdomainDelegation">>, <<"domain">>
            ],
            correct_values = #{
                <<"subdomainDelegation">> => [false],
                <<"domain">> => [ExpDomain]
            },
            bad_values = [
                {<<"subdomainDelegation">>, bad_bool, ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
                {<<"domain">>, <<"https://hasprotocol">>, ?ERROR_BAD_VALUE_DOMAIN(<<"domain">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, Undelegated_ApiTestSpec)),
    {ok, Provider} = oz_test_utils:get_provider(Config, P1),
    ?assertEqual(ExpDomain, Provider#od_provider.domain),
    ?assertEqual(undefined, Provider#od_provider.subdomain),
    ?assertEqual(false, Provider#od_provider.subdomain_delegation),
    ok.


update_domain_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),

    NewDomain = <<"changed.pl">>,
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            operation = update,
            module = provider_logic,
            function = update_domain_config,
            args = [client, P1, data],
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [
                <<"subdomainDelegation">>, <<"domain">>
            ],
            correct_values = #{
                <<"subdomainDelegation">> => [false],
                <<"domain">> => [NewDomain]
            },
            bad_values = [
                {<<"subdomainDelegation">>, bad_bool, ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
                {<<"domain">>, <<"https://hasprotocol">>, ?ERROR_BAD_VALUE_DOMAIN(<<"domain">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    {ok, Provider} = oz_test_utils:get_provider(Config, P1),
    ?assertEqual(NewDomain, Provider#od_provider.domain),
    ?assertEqual(undefined, Provider#od_provider.subdomain),
    ?assertEqual(false, Provider#od_provider.subdomain_delegation),
    ok.


% Ensure provider domain can be set to an IP address
update_domain_to_ip_address_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),

    NewDomain = <<"172.17.0.5">>,
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            operation = update,
            module = provider_logic,
            function = update_domain_config,
            args = [client, P1, data],
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [
                <<"subdomainDelegation">>, <<"domain">>
            ],
            correct_values = #{
                <<"subdomainDelegation">> => [false],
                <<"domain">> => [NewDomain]
            },
            bad_values = []
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    {ok, Provider} = oz_test_utils:get_provider(Config, P1),
    ?assertEqual(NewDomain, Provider#od_provider.domain),
    ?assertEqual(undefined, Provider#od_provider.subdomain),
    ?assertEqual(false, Provider#od_provider.subdomain_delegation),
    ok.


%% Test getting aspect "domain_config"
get_domain_config_test(Config) ->

    % Test without delegated subdomain

    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
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
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = get_domain_config,
            args = [client, P1],
            expected_result = ?OK_MAP(ExpBody)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % test enabled subdomain delegation
    ExpSubdomain = <<"subdomain">>,
    ExpIPs = [{5,8,2,4}, {10,12,255,255}],
    {ok, OZDomain} = oz_test_utils:get_domain(Config),
    ExpDomain2 = <<ExpSubdomain/binary, ".", (list_to_binary(OZDomain))/binary>>,

    oz_test_utils:enable_subdomain_delegation(Config, P1, ExpSubdomain, ExpIPs),

    ExpBody2 = #{
        <<"subdomainDelegation">> => true,
        <<"domain">> => ExpDomain2,
        <<"ipList">> => ExpIPs,
        <<"subdomain">> => ExpSubdomain
    },
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, Admin},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = get_domain_config,
            args = [client, P1],
            expected_result = ?OK_MAP(ExpBody2)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


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
    Config;
init_per_testcase(_, Config) ->
    Config.


end_per_testcase(check_my_ports_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, http_client),
    ok;
end_per_testcase(_, _Config) ->
    ok.


%%%===================================================================
%%% Helper functions
%%%===================================================================

