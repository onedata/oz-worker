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

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("auth/entitlement_mapping.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/global_definitions.hrl").

-include("api_test_utils.hrl").

-define(DUMMY_IDP, dummyIdP).


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
    get_eff_user_membership_intermediaries/1,
    list_eff_groups_test/1,
    get_eff_group_test/1,
    get_eff_group_membership_intermediaries/1,
    list_eff_harvesters_test/1,

    list_eff_spaces_test/1,
    list_self_spaces_test/1,
    get_eff_space_test/1,
    get_own_space_test/1,
    legacy_support_space_test/1,
    legacy_update_support_size_test/1,
    legacy_revoke_support_test/1,

    check_my_ports_test/1,
    check_my_ip_test/1,
    map_group_test/1,
    update_subdomain_test/1,
    update_domain_test/1,
    update_domain_is_idempotent_test/1,
    get_domain_config_test/1,
    get_own_domain_config_test/1,
    get_current_time_test/1,
    verify_provider_identity_test/1
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
        get_eff_user_membership_intermediaries,
        list_eff_groups_test,
        get_eff_group_test,
        get_eff_group_membership_intermediaries,
        list_eff_harvesters_test,

        list_eff_spaces_test,
        list_self_spaces_test,
        get_eff_space_test,
        get_own_space_test,
        legacy_support_space_test,
        legacy_supdate_support_size_test,
        legacy_revoke_support_test,

        check_my_ports_test,
        check_my_ip_test,
        map_group_test,
        update_subdomain_test,
        update_domain_test,
        update_domain_is_idempotent_test,
        get_domain_config_test,
        get_own_domain_config_test,
        get_current_time_test,
        verify_provider_identity_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    ExpName = ?CORRECT_NAME,
    {ok, CreatorUserId} = oz_test_utils:create_user(Config),
    % Create invalid tokens to verify error codes
    {ok, ClientToken} = oz_test_utils:create_client_token(Config, CreatorUserId),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(CreatorUserId), ?UNIQUE_STRING),
    {ok, SpaceInviteToken} = oz_test_utils:space_invite_user_token(Config, ?USER(CreatorUserId), Space),
    {ok, SpaceInviteTokenSerialized} = tokens:serialize(SpaceInviteToken),

    OZDomain = oz_test_utils:oz_domain(Config),

    VerifyFun = fun(ProviderId, ProviderToken, Data) ->
        ExpSubdomainDelegation = maps:get(<<"subdomainDelegation">>, Data),
        ExpAdminEmail = maps:get(<<"adminEmail">>, Data),
        ExpLatitude = maps:get(<<"latitude">>, Data, 0.0),
        ExpLongitude = maps:get(<<"longitude">>, Data, 0.0),
        {ExpSubdomain, ExpDomain} = case ExpSubdomainDelegation of
            true ->
                Subdomain = maps:get(<<"subdomain">>, Data),
                {Subdomain, <<Subdomain/binary, ".", OZDomain/binary>>};
            false ->
                {undefined, maps:get(<<"domain">>, Data)}
        end,
        ExpClusterId = ProviderId,

        % Logic returns the token in deserialized format, and REST in serialized
        SerializedToken = case is_binary(ProviderToken) of
            true ->
                ProviderToken;
            false ->
                {ok, Serialized} = tokens:serialize(ProviderToken),
                Serialized
        end,
        ?assertEqual(ok, oz_test_utils:call_oz(Config, provider_logic, verify_provider_identity, [
            ?ROOT, ProviderId, SerializedToken
        ])),
        {ok, Provider} = oz_test_utils:get_provider(Config, ProviderId),
        ?assertEqual(ExpName, Provider#od_provider.name),
        ?assertEqual(ExpDomain, Provider#od_provider.domain),
        ?assertEqual(ExpSubdomainDelegation, Provider#od_provider.subdomain_delegation),
        ?assertEqual(ExpSubdomain, Provider#od_provider.subdomain),
        ?assertEqual(ExpAdminEmail, Provider#od_provider.admin_email),
        ?assertEqual(ExpClusterId, ProviderId),
        ?assertEqual(ExpLatitude, Provider#od_provider.latitude),
        ?assertEqual(ExpLongitude, Provider#od_provider.longitude),

        {ok, Cluster} = oz_test_utils:get_cluster(Config, ExpClusterId),
        ?assertEqual(ProviderId, ExpClusterId),
        ?assertEqual(?ONEPROVIDER, Cluster#od_cluster.type),

        % check also provider_logic:get_url function
        ExpProviderURL = str_utils:format_bin("https://~s", [ExpDomain]),
        {ok, ProviderURL} = oz_test_utils:call_oz(
            Config, provider_logic, get_url, [ProviderId]
        ),
        ?assertEqual(ProviderURL, ExpProviderURL),

        {ok, UserClusters} = oz_test_utils:user_get_clusters(Config, CreatorUserId),
        ?assert(lists:member(ExpClusterId, UserClusters)),

        % delete provider to avoid "subdomain occupied" errors
        oz_test_utils:delete_provider(Config, ProviderId),
        true
    end,

    %% Create provider with subdomain delegation turned off
    oz_test_utils:set_env(Config, subdomain_delegation_supported, false),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/providers">>,
            expected_code = ?HTTP_200_OK,
            expected_body = ?OK_ENV(fun(_Env, Data) -> fun(Value) ->
                ProviderToken = maps:get(<<"providerRootToken">>, Value),
                %% @TODO VFS-5554 Deprecated, for backward compatibility
                ProviderToken = maps:get(<<"macaroon">>, Value),
                ProviderId = maps:get(<<"providerId">>, Value),
                VerifyFun(ProviderId, ProviderToken, Data)
            end
            end)
        },
        logic_spec = #logic_spec{
            operation = create,
            module = provider_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(_Env, Data) ->
                ?OK_TERM(fun({ProviderId, ProviderToken}) ->
                    VerifyFun(ProviderId, ProviderToken, Data)
                end)
            end)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [
                <<"token">>, <<"name">>, <<"adminEmail">>, <<"domain">>, <<"subdomainDelegation">>
            ],
            optional = [<<"latitude">>, <<"longitude">>],
            correct_values = #{
                <<"token">> => [fun() ->
                    {ok, RegistrationToken} = oz_test_utils:create_provider_registration_token(
                        Config, ?USER(CreatorUserId), CreatorUserId
                    ),
                    {ok, Serialized} = tokens:serialize(RegistrationToken),
                    Serialized
                end],
                <<"name">> => [ExpName],
                <<"domain">> => [<<"multilevel.provider-domain.org">>],
                <<"subdomainDelegation">> => [false],
                <<"adminEmail">> => [?ADMIN_EMAIL],
                <<"latitude">> => [rand:uniform() * 90],
                <<"longitude">> => [rand:uniform() * 180]
            },
            bad_values = [
                {<<"domain">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"domain">>)},
                {<<"domain">>, <<"https://domain.com">>, ?ERROR_BAD_VALUE_DOMAIN},
                {<<"domain">>, <<"domain.com:443">>, ?ERROR_BAD_VALUE_DOMAIN},
                {<<"domain">>, <<".leadingdot">>, ?ERROR_BAD_VALUE_DOMAIN},
                {<<"domain">>, <<"trailing-.hyphen">>, ?ERROR_BAD_VALUE_DOMAIN},
                {<<"subdomainDelegation">>, true, ?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED},
                {<<"subdomainDelegation">>, <<"binary">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
                {<<"subdomainDelegation">>, bad_bool, ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
                {<<"adminEmail">>, <<"adminwithoutdomain">>, ?ERROR_BAD_VALUE_EMAIL},
                {<<"latitude">>, <<"ASDASD">>, ?ERROR_BAD_VALUE_FLOAT(<<"latitude">>)},
                {<<"latitude">>, -1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"latitude">>, -90.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"latitude">>, 90.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"latitude">>, 1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90)},
                {<<"longitude">>, <<"ASDASD">>, ?ERROR_BAD_VALUE_FLOAT(<<"longitude">>)},
                {<<"longitude">>, -1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)},
                {<<"longitude">>, -180.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)},
                {<<"longitude">>, 180.1, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)},
                {<<"longitude">>, 1500, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180)},
                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"token">>)},
                {<<"token">>, <<"zxvcsadfgasdfasdf">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, ClientToken, ?ERROR_BAD_VALUE_TOKEN(
                    <<"token">>, ?ERROR_NOT_AN_INVITE_TOKEN(?REGISTER_ONEPROVIDER, ?ACCESS_TOKEN))},
                {<<"token">>, SpaceInviteTokenSerialized, ?ERROR_BAD_VALUE_TOKEN(
                    <<"token">>, ?ERROR_NOT_AN_INVITE_TOKEN(?REGISTER_ONEPROVIDER, ?INVITE_TOKEN(?USER_JOIN_SPACE, Space)))}
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    %% Create provider with subdomain delegation turned on
    oz_test_utils:set_env(Config, subdomain_delegation_supported, true),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        data_spec = #data_spec{
            required = [
                <<"token">>, <<"name">>, <<"subdomain">>, <<"ipList">>,
                <<"adminEmail">>, <<"subdomainDelegation">>
            ],
            optional = [<<"latitude">>, <<"longitude">>],
            correct_values = #{
                <<"token">> => [fun() ->
                    {ok, RegistrationToken} = oz_test_utils:create_provider_registration_token(
                        Config, ?USER(CreatorUserId), CreatorUserId
                    ),
                    {ok, Serialized} = tokens:serialize(RegistrationToken),
                    Serialized
                end],
                <<"name">> => [ExpName],
                <<"subdomainDelegation">> => [true],
                <<"subdomain">> => [<<"prov-sub">>],
                <<"ipList">> => [[<<"2.4.6.8">>, <<"255.253.251.2">>]],
                <<"adminEmail">> => [?ADMIN_EMAIL],
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
    ExpDomain = ?UNIQUE_DOMAIN,
    ExpAdminEmail = ?ADMIN_EMAIL,
    ExpLatitude = rand:uniform() * 90,
    ExpLongitude = rand:uniform() * 180,
    PrivateProviderDetails = #{
        <<"name">> => ExpName,
        <<"adminEmail">> => ExpAdminEmail,
        <<"domain">> => ExpDomain,
        <<"latitude">> => ExpLatitude,
        <<"longitude">> => ExpLongitude
    },
    {ok, P1Creator} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, P1Creator, PrivateProviderDetails#{<<"subdomainDelegation">> => false}
    ),
    {ok, P2Creator} = oz_test_utils:create_user(Config),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, P2Creator, ?PROVIDER_NAME2
    ),

    Cluster1MemberNoViewPrivs = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_VIEW]),

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),

    SupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, S1, SupportSize),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, P1Creator},
                {provider, P1, P1Token},
                {admin, [?OZ_PROVIDERS_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, Cluster1MemberNoViewPrivs},
                {user, P2Creator},
                {user, NonAdmin},
                {provider, P2, P2Token}
            ]
        },
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = get,
            args = [auth, P1],
            expected_result = ?OK_TERM(
                fun(#od_provider{
                    name = Name, subdomain_delegation = false,
                    domain = Domain, subdomain = undefined,
                    admin_email = AdminEmail,
                    latitude = Latitude, longitude = Longitude,
                    storages = StorageIds,
                    eff_spaces = EffSpaces, eff_users = EffUsers, eff_groups = #{}
                }) ->
                    ?assertEqual(ExpName, Name),
                    ?assertEqual(ExpDomain, Domain),
                    ?assertEqual(ExpAdminEmail, AdminEmail),
                    ?assertEqual(ExpLatitude, Latitude),
                    ?assertEqual(ExpLongitude, Longitude),
                    ?assertEqual(StorageIds, [St1]),
                    ?assertEqual(#{S1 => {SupportSize, [{od_storage, St1}]}}, EffSpaces),
                    ?assertEqual(#{U1 => [{od_storage, St1}]}, EffUsers)
                end)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_provider, id = P1, aspect = instance},
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ExpName, <<"domain">> => ExpDomain,
                <<"effectiveGroups">> => [], <<"effectiveUsers">> => [U1],
                <<"latitude">> => ExpLatitude, <<"longitude">> => ExpLongitude,
                <<"spaces">> => #{S1 => SupportSize},
                <<"storages">> => [St1],
                <<"subdomain">> => <<"undefined">>,
                <<"subdomainDelegation">> => false,
                <<"adminEmail">> => ExpAdminEmail,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(P1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    ExpProtectedDetails = maps:remove(<<"adminEmail">>, PrivateProviderDetails#{
        <<"online">> => true
    }),

    % Get and check protected data
    GetProtectedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, P1Creator},
                {user, Cluster1MemberNoViewPrivs},
                {admin, [?OZ_PROVIDERS_VIEW]},
                {provider, P2, P2Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, P2Creator},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpProtectedDetails#{<<"providerId">> => P1}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = provider_logic,
            function = get_protected_data,
            args = [auth, P1],
            expected_result = ?OK_MAP_CONTAINS(ExpProtectedDetails)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_provider, id = P1,
                aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP_CONTAINS(ExpProtectedDetails#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(P1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)).


get_self_test(Config) ->
    ProviderDetails = ?PROVIDER_DETAILS(?PROVIDER_NAME1),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, ProviderDetails#{<<"subdomainDelegation">> => false}
    ),

    ExpDetails = maps:remove(<<"adminEmail">>, ProviderDetails#{
        <<"name">> => ?PROVIDER_NAME1,
        <<"online">> => false
    }),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, P1Token}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/provider">>,
            expected_code = ?HTTP_200_OK,
            expected_body = ExpDetails#{<<"providerId">> => P1}
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_provider, id = ?SELF,
                aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP_CONTAINS(ExpDetails#{
                <<"online">> => true,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
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
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, <<"PROV1">>
    ),
    {ok, {P2, _}} = oz_test_utils:create_provider(Config, <<"PROV2">>),
    {ok, {P3, _}} = oz_test_utils:create_provider(Config, <<"PROV3">>),
    {ok, {P4, _}} = oz_test_utils:create_provider(Config, <<"PROV4">>),
    {ok, {P5, _}} = oz_test_utils:create_provider(Config, <<"PROV5">>),
    ExpProviders = [P1, P2, P3, P4, P5],

    % Create two users, grant one of them the privilege to list providers.
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_PROVIDERS_LIST]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P1, P1Token}
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
            args = [auth],
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
    AdminEmail = <<"email@domain.com">>,
    ProviderDetails = #{
        <<"name">> => Name, <<"adminEmail">> => AdminEmail,
        <<"subdomainDelegation">> => false,
        <<"latitude">> => Latitude, <<"longitude">> => Longitude
    },

    EnvSetUpFun = fun() ->
        {ok, Cluster1Member} = oz_test_utils:create_user(Config),
        {ok, {P1, P1Token}} = oz_test_utils:create_provider(
            Config, Cluster1Member, ProviderDetails#{<<"domain">> => ?UNIQUE_DOMAIN}
        ),
        Cluster1MemberNoUpdatePrivs = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_UPDATE]),
        #{
            providerId => P1, providerClient => {provider, P1, P1Token},
            clusterMember => {user, Cluster1Member}, clusterMemberNoDeletePrivs => {user, Cluster1MemberNoUpdatePrivs}
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{providerId := ProvId} = _Env, Data) ->
        {ok, Provider} = oz_test_utils:get_provider(Config, ProvId),
        {ExpName, ExpAdminEmail, ExpLatitude, ExpLongitude} = case ShouldSucceed of
            false ->
                {Name, AdminEmail, Latitude, Longitude};
            true ->
                {
                    maps:get(<<"name">>, Data, Name),
                    maps:get(<<"adminEmail">>, Data, AdminEmail),
                    maps:get(<<"latitude">>, Data, Latitude),
                    maps:get(<<"longitude">>, Data, Longitude)
                }
        end,
        ?assertEqual(ExpName, Provider#od_provider.name),
        ?assertEqual(ExpAdminEmail, Provider#od_provider.admin_email),
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
            at_least_one = [<<"name">>, <<"adminEmail">>, <<"latitude">>, <<"longitude">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"adminEmail">> => [<<"new+email@gmail.com">>],
                <<"latitude">> => [rand:uniform() * 90],
                <<"longitude">> => [rand:uniform() * 180]
            },
            bad_values = [
                {<<"adminEmail">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"adminEmail">>)},
                {<<"adminEmail">>, <<"nodomain">>, ?ERROR_BAD_VALUE_EMAIL},
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
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check that clients can't make request on behalf of other client
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [
        ?OZ_PROVIDERS_LIST
    ], []),

    {ok, Cluster2Member} = oz_test_utils:create_user(Config),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, Cluster2Member, ?PROVIDER_NAME2
    ),
    Cluster2MemberNoUpdatePrivs = new_cluster_member_with_privs(Config, P2, [], [?CLUSTER_UPDATE]),

    ApiTestSpec2 = #api_test_spec{
        client_spec = ClientSpec#client_spec{
            correct = [
                clusterMember,
                {admin, [?OZ_PROVIDERS_UPDATE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                clusterMemberNoDeletePrivs,
                {user, U1},
                {user, Cluster2MemberNoUpdatePrivs},
                {provider, P2, P2Token}
            ]
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = update,
            args = [auth, providerId, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_provider, id = providerId, aspect = instance},
            expected_result = ?OK_RES
        },
        data_spec = DataSpec
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, Cluster1Member} = oz_test_utils:create_user(Config),
        {ok, {P1, P1Token}} = oz_test_utils:create_provider(
            Config, Cluster1Member, ?PROVIDER_NAME1
        ),
        Cluster1MemberNoDeletePrivs = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_DELETE]),
        #{
            providerId => P1, providerClient => {provider, P1, P1Token},
            clusterId => P1,
            clusterMember => {user, Cluster1Member}, clusterMemberNoDeletePrivs => {user, Cluster1MemberNoDeletePrivs}
        }
    end,
    DeleteEntityFun = fun(#{providerId := ProviderId} = _Env) ->
        oz_test_utils:delete_provider(Config, ProviderId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{providerId := ProviderId, clusterId := ClusterId} = _Env, _) ->
        {ok, Providers} = oz_test_utils:list_providers(Config),
        {ok, Clusters} = oz_test_utils:list_clusters(Config),
        ?assertEqual(not ShouldSucceed, lists:member(ProviderId, Providers)),
        ?assertEqual(not ShouldSucceed, lists:member(ClusterId, Clusters))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                clusterMember,
                root,
                {admin, [?OZ_PROVIDERS_DELETE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                clusterMemberNoDeletePrivs,
                {user, NonAdmin},
                {provider, P2, P2Token}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/providers/">>, providerId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = delete,
            args = [auth, providerId],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_provider, id = providerId, aspect = instance},
            expected_result = ?OK_RES
        }
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
        {ok, {P1, P1Token}} = oz_test_utils:create_provider(
            Config, ?PROVIDER_NAME1
        ),
        #{providerId => P1, providerClient => {provider, P1, P1Token}}
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
            expected_code = ?HTTP_204_NO_CONTENT
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_provider, id = ?SELF, aspect = instance},
            expected_result = ?OK_RES
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


list_eff_users_test(Config) ->
    {
        {P1, P1Token}, _S1, _Groups, Users, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_provider_eff_users_env(Config),

    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),

    Cluster1Member = new_cluster_member_with_privs(Config, P1, [?CLUSTER_VIEW], []),
    Cluster1MemberNoViewPrivs = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_VIEW]),

    [{U3, _}, {U4, _}, {U5, _}, {U6, _}] = Users,
    ExpUsers = [U1, U2, U3, U4, U5, U6],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Cluster1Member},
                {admin, [?OZ_PROVIDERS_LIST_RELATIONSHIPS]},
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, Cluster1MemberNoViewPrivs},
                {user, U1},
                {user, U2},
                {user, NonAdmin},
                {provider, P2, P2Token}
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
            args = [auth, P1],
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
        {P1, P1Token}, S1, _Groups, EffUsers, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_provider_eff_users_env(Config),

    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    Cluster1Member = new_cluster_member_with_privs(Config, P1, [?CLUSTER_VIEW], []),
    Cluster1MemberNoViewPrivs = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_VIEW]),

    {ok, S1} = oz_test_utils:support_space_by_provider(Config, P2, S1),

    lists:foreach(
        fun({UserId, UserDetails}) ->
            ExpDetails = UserDetails#{
                <<"emails">> => [],
                <<"linkedAccounts">> => []
            },

            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, Cluster1Member},
                        {admin, [?OZ_USERS_VIEW]},
                        {provider, P1, P1Token}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, Cluster1MemberNoViewPrivs},
                        {user, U1},
                        {user, U2},
                        {user, NonAdmin},
                        {provider, P2, P2Token}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/providers/">>, P1, <<"/effective_users/">>, UserId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = ExpDetails#{
                        <<"userId">> => UserId,
                        <<"basicAuthEnabled">> => false,

                        % TODO VFS-4506 deprecated, included for backward compatibility
                        <<"name">> => maps:get(<<"fullName">>, UserDetails),
                        <<"login">> => maps:get(<<"username">>, UserDetails),
                        <<"alias">> => maps:get(<<"username">>, UserDetails),
                        <<"emailList">> => maps:get(<<"emails">>, ExpDetails)
                    }
                },
                logic_spec = #logic_spec{
                    module = provider_logic,
                    function = get_eff_user,
                    args = [auth, P1, UserId],
                    expected_result = ?OK_MAP_CONTAINS(ExpDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_user, id = UserId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_PROVIDER(P1),
                    expected_result = ?OK_MAP_CONTAINS(ExpDetails#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = gri:deserialize(EncodedGri),
                            ?assertEqual(Id, UserId)
                        end,

                        % TODO VFS-4506 deprecated, included for backward compatibility
                        <<"name">> => maps:get(<<"fullName">>, UserDetails),
                        <<"login">> => maps:get(<<"username">>, UserDetails),
                        <<"alias">> => maps:get(<<"username">>, UserDetails),
                        <<"emailList">> => maps:get(<<"emails">>, ExpDetails)
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffUsers
    ).


get_eff_user_membership_intermediaries(Config) ->
    %% Create environment with the following relations:
    %%
    %%          Provider1    Provider2    Provider3
    %%              |            |            |
    %%           Storage1     Storage2   Storage3
    %%            |     \    /       \    /
    %%       Space1      Space2     Space3
    %%      /  |  \     /   |  \     /
    %%  User2  |   \   /    |   \   /
    %%          \   Group2  |   Group3
    %%           \   /      |   /  |
    %%            \ /       |  /   |
    %%            Group1----|-'    |
    %%                \     |     /
    %%                 \    |    /
    %%                  \   |   /
    %%                    User1
    %%      <<user>>
    %%      NonAdmin

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),

    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S3} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),

    {ok, Cluster1Member} = oz_test_utils:create_user(Config),
    {ok, Cluster2Member} = oz_test_utils:create_user(Config),
    {ok, Cluster3Member} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Auth}} = oz_test_utils:create_provider(Config, Cluster1Member, ?PROVIDER_NAME1),
    {ok, {P2, P2Auth}} = oz_test_utils:create_provider(Config, Cluster2Member, ?PROVIDER_NAME1),
    {ok, {P3, P3Auth}} = oz_test_utils:create_provider(Config, Cluster3Member, ?PROVIDER_NAME1),
    Cluster1MemberNoViewPrivs = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_VIEW]),
    Cluster2MemberNoViewPrivs = new_cluster_member_with_privs(Config, P2, [], [?CLUSTER_VIEW]),
    Cluster3MemberNoViewPrivs = new_cluster_member_with_privs(Config, P3, [], [?CLUSTER_VIEW]),

    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, St2} = oz_test_utils:create_storage(Config, ?PROVIDER(P2), ?STORAGE_NAME1),
    {ok, St3} = oz_test_utils:create_storage(Config, ?PROVIDER(P3), ?STORAGE_NAME1),

    oz_test_utils:space_add_user(Config, S1, U2),

    oz_test_utils:group_add_group(Config, G2, G1),
    oz_test_utils:group_add_group(Config, G3, G1),

    oz_test_utils:space_add_group(Config, S1, G1),
    oz_test_utils:space_add_group(Config, S1, G2),
    oz_test_utils:space_add_group(Config, S2, G2),
    oz_test_utils:space_add_group(Config, S2, G3),
    oz_test_utils:space_add_group(Config, S3, G3),

    SupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, S1, SupportSize),
    {ok, S2} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, S2, SupportSize),
    {ok, S2} = oz_test_utils:support_space(Config, ?PROVIDER(P2), St2, S2, SupportSize),
    {ok, S3} = oz_test_utils:support_space(Config, ?PROVIDER(P2), St2, S3, SupportSize),
    {ok, S3} = oz_test_utils:support_space(Config, ?PROVIDER(P3), St3, S3, SupportSize),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % {ProviderId, SubjectUser, CorrectClients, ExpIntermediariesRaw}
    ExpectedMembershipIntermediaries = [
        {P1, U1, [{provider, P1, P1Auth}, {user, Cluster1Member}, {user, U1}], ordsets:from_list([
            {od_storage, St1}
        ])},
        {P1, U2, [{provider, P1, P1Auth}, {user, Cluster1Member}, {user, U2}], ordsets:from_list([
            {od_storage, St1}
        ])},

        {P2, U1, [{provider, P2, P2Auth}, {user, Cluster2Member}, {user, U1}], ordsets:from_list([
            {od_storage, St2}
        ])},

        {P3, U1, [{provider, P3, P3Auth}, {user, Cluster3Member}, {user, U1}], ordsets:from_list([
            {od_storage, St3}
        ])}
    ],

    lists:foreach(fun({ProviderId, SubjectUser, CorrectClients, ExpIntermediariesRaw}) ->
        ExpIntermediaries = lists:map(fun({Type, Id}) ->
            #{<<"type">> => gri:serialize_type(Type, regular), <<"id">> => Id}
        end, ExpIntermediariesRaw),
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_PROVIDERS_VIEW]}
                ] ++ CorrectClients,
                unauthorized = [nobody],
                forbidden = [
                    {user, NonAdmin}, {user, U1}, {user, U2},
                    {provider, P1, P1Auth}, {provider, P2, P2Auth}, {provider, P3, P3Auth},
                    {user, Cluster1Member}, {user, Cluster2Member}, {user, Cluster3Member},
                    {user, Cluster1MemberNoViewPrivs}, {user, Cluster2MemberNoViewPrivs}, {user, Cluster3MemberNoViewPrivs}
                ] -- CorrectClients
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/providers/">>, ProviderId, <<"/effective_users/">>, SubjectUser, <<"/membership">>],
                expected_code = ?HTTP_200_OK,
                expected_body = #{<<"intermediaries">> => ExpIntermediaries}
            },
            logic_spec = #logic_spec{
                module = provider_logic,
                function = get_eff_user_membership_intermediaries,
                args = [auth, ProviderId, SubjectUser],
                expected_result = ?OK_LIST(ExpIntermediariesRaw)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, ExpectedMembershipIntermediaries).


list_eff_groups_test(Config) ->
    {
        {P1, P1Token}, _S1, Groups, _Users, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_provider_eff_users_env(Config),

    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    Cluster1Member = new_cluster_member_with_privs(Config, P1, [?CLUSTER_VIEW], []),
    Cluster1MemberNoViewPrivs = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_VIEW]),

    [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}, {G6, _}] = Groups,
    ExpGroups = [G1, G2, G3, G4, G5, G6],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Cluster1Member},
                {admin, [?OZ_PROVIDERS_LIST_RELATIONSHIPS]},
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, Cluster1MemberNoViewPrivs},
                {user, U1},
                {user, U2},
                {user, NonAdmin},
                {provider, P2, P2Token}
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
            args = [auth, P1],
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
        {P1, P1Token}, S1, EffGroups, _Users, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_provider_eff_users_env(Config),

    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    Cluster1Member = new_cluster_member_with_privs(Config, P1, [?CLUSTER_VIEW], []),
    Cluster1MemberNoViewPrivs = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_VIEW]),

    {ok, S1} = oz_test_utils:support_space_by_provider(Config, P2, S1),

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
                        {user, Cluster1Member},
                        {admin, [?OZ_GROUPS_VIEW]},
                        {provider, P1, P1Token}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, Cluster1MemberNoViewPrivs},
                        {user, U1},
                        {user, U2},
                        {user, NonAdmin},
                        {provider, P2, P2Token}
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
                    args = [auth, P1, GroupId],
                    expected_result = ?OK_MAP_CONTAINS(GroupDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_group, id = GroupId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_PROVIDER(P1),
                    expected_result = ?OK_MAP_CONTAINS(GroupDetailsBinary#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = gri:deserialize(EncodedGri),
                            ?assertEqual(Id, GroupId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffGroups
    ).


get_eff_group_membership_intermediaries(Config) ->
    %% Create environment with the following relations:
    %%
    %%          Provider1    Provider2    Provider3
    %%              |            |            |
    %%           Storage1    Storage2     Storage3
    %%            |     \    /       \    /     \
    %%       Space1      Space2     Space3     Space4
    %%      /  |  \     /   |  \     /            \
    %%  User2  |   \   /    |   \   /            Group4
    %%          \   Group2  |   Group3              \
    %%           \   /      |   /  |               User2
    %%            \ /       |  /   |        (the same as in Space1)
    %%            Group1----|-'    |
    %%                \     |     /
    %%                 \    |    /
    %%                  \   |   /
    %%                  UserGroup
    %%                      |
    %%                    User1
    %%
    %%      <<user>>
    %%      NonAdmin

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, UserGroup} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G4} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, S3} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, S4} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),

    {ok, Cluster1Member} = oz_test_utils:create_user(Config),
    {ok, Cluster2Member} = oz_test_utils:create_user(Config),
    {ok, Cluster3Member} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Auth}} = oz_test_utils:create_provider(Config, Cluster1Member, ?PROVIDER_NAME1),
    {ok, {P2, P2Auth}} = oz_test_utils:create_provider(Config, Cluster2Member, ?PROVIDER_NAME1),
    {ok, {P3, P3Auth}} = oz_test_utils:create_provider(Config, Cluster3Member, ?PROVIDER_NAME1),
    Cluster1MemberNoViewPrivs = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_VIEW]),
    Cluster2MemberNoViewPrivs = new_cluster_member_with_privs(Config, P2, [], [?CLUSTER_VIEW]),
    Cluster3MemberNoViewPrivs = new_cluster_member_with_privs(Config, P3, [], [?CLUSTER_VIEW]),

    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, St2} = oz_test_utils:create_storage(Config, ?PROVIDER(P2), ?STORAGE_NAME1),
    {ok, St3} = oz_test_utils:create_storage(Config, ?PROVIDER(P3), ?STORAGE_NAME1),

    oz_test_utils:group_add_user(Config, G4, U2),
    oz_test_utils:space_add_user(Config, S1, U2),

    oz_test_utils:group_add_group(Config, G1, UserGroup),
    oz_test_utils:group_add_group(Config, G2, G1),
    oz_test_utils:group_add_group(Config, G3, G1),
    oz_test_utils:group_add_group(Config, G3, UserGroup),

    oz_test_utils:space_add_group(Config, S1, G1),
    oz_test_utils:space_add_group(Config, S1, G2),
    oz_test_utils:space_add_group(Config, S2, UserGroup),
    oz_test_utils:space_add_group(Config, S2, G2),
    oz_test_utils:space_add_group(Config, S2, G3),
    oz_test_utils:space_add_group(Config, S3, G3),
    oz_test_utils:space_add_group(Config, S4, G4),

    SupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, S1, SupportSize),
    {ok, S2} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, S2, SupportSize),
    {ok, S2} = oz_test_utils:support_space(Config, ?PROVIDER(P2), St2, S2, SupportSize),
    {ok, S3} = oz_test_utils:support_space(Config, ?PROVIDER(P2), St2, S3, SupportSize),
    {ok, S3} = oz_test_utils:support_space(Config, ?PROVIDER(P3), St3, S3, SupportSize),
    {ok, S4} = oz_test_utils:support_space(Config, ?PROVIDER(P3), St3, S4, SupportSize),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ExpectedMembershipIntermediaries = [
        {P1, UserGroup, [{provider, P1, P1Auth}, {user, Cluster1Member}, {user, U1}], ordsets:from_list([
            {od_storage, St1}
        ])},
        {P1, G1, [{provider, P1, P1Auth}, {user, Cluster1Member}, {user, U1}], ordsets:from_list([
            {od_storage, St1}
        ])},
        {P1, G2, [{provider, P1, P1Auth}, {user, Cluster1Member}, {user, U1}], ordsets:from_list([
            {od_storage, St1}
        ])},
        {P1, G3, [{provider, P1, P1Auth}, {user, Cluster1Member}, {user, U1}], ordsets:from_list([
            {od_storage, St1}
        ])},

        {P2, UserGroup, [{provider, P2, P2Auth}, {user, Cluster2Member}, {user, U1}], ordsets:from_list([
            {od_storage, St2}
        ])},
        {P2, G1, [{provider, P2, P2Auth}, {user, Cluster2Member}, {user, U1}], ordsets:from_list([
            {od_storage, St2}
        ])},
        {P2, G2, [{provider, P2, P2Auth}, {user, Cluster2Member}, {user, U1}], ordsets:from_list([
            {od_storage, St2}
        ])},
        {P2, G3, [{provider, P2, P2Auth}, {user, Cluster2Member}, {user, U1}], ordsets:from_list([
            {od_storage, St2}
        ])},

        {P3, UserGroup, [{provider, P3, P3Auth}, {user, Cluster3Member}, {user, U1}], ordsets:from_list([
            {od_storage, St3}
        ])},
        {P3, G3, [{provider, P3, P3Auth}, {user, Cluster3Member}, {user, U1}], ordsets:from_list([
            {od_storage, St3}
        ])},
        {P3, G4, [{provider, P3, P3Auth}, {user, Cluster3Member}, {user, U2}], ordsets:from_list([
            {od_storage, St3}
        ])}
    ],

    lists:foreach(fun({ProviderId, SubjectGroup, CorrectClients, ExpIntermediariesRaw}) ->
        ExpIntermediaries = lists:map(fun({Type, Id}) ->
            #{<<"type">> => gri:serialize_type(Type, regular), <<"id">> => Id}
        end, ExpIntermediariesRaw),
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_PROVIDERS_VIEW]}
                ] ++ CorrectClients,
                unauthorized = [nobody],
                forbidden = [
                    {user, NonAdmin}, {user, U1}, {user, U2},
                    {provider, P1, P1Auth}, {provider, P2, P2Auth}, {provider, P3, P3Auth},
                    {user, Cluster1Member}, {user, Cluster2Member}, {user, Cluster3Member},
                    {user, Cluster1MemberNoViewPrivs}, {user, Cluster2MemberNoViewPrivs}, {user, Cluster3MemberNoViewPrivs}
                ] -- CorrectClients
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/providers/">>, ProviderId, <<"/effective_groups/">>, SubjectGroup, <<"/membership">>],
                expected_code = ?HTTP_200_OK,
                expected_body = #{<<"intermediaries">> => ExpIntermediaries}
            },
            logic_spec = #logic_spec{
                module = provider_logic,
                function = get_eff_group_membership_intermediaries,
                args = [auth, ProviderId, SubjectGroup],
                expected_result = ?OK_LIST(ExpIntermediariesRaw)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, ExpectedMembershipIntermediaries).


list_eff_harvesters_test(Config) ->
    %% Create environment with the following relations:
    %%
    %%             Provider1   
    %%              /      \  
    %%           Space1   Space2      
    %%           /   \    /    \      
    %% Harvester1     \  /      Harvester3         
    %%     |       Harvester2       |
    %%   User1         |          User1
    %%               User1   
    %%      <<user>>
    %%      NonAdmin

    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME1
    ),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),

    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),

    oz_test_utils:support_space_by_provider(Config, P1, S1),
    oz_test_utils:support_space_by_provider(Config, P1, S2),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    {ok, H2} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    {ok, H3} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),

    oz_test_utils:harvester_add_space(Config, H1, S1),
    oz_test_utils:harvester_add_space(Config, H2, S1),
    oz_test_utils:harvester_add_space(Config, H2, S2),
    oz_test_utils:harvester_add_space(Config, H3, S2),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ExpHarvesters = [H1, H2, H3],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_PROVIDERS_LIST_RELATIONSHIPS]},
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1},
                {provider, P2, P2Token}
            ]
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = get_eff_harvesters,
            args = [auth, P1],
            expected_result = ?OK_LIST(ExpHarvesters)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also provider_logic:has_eff_harvester function
    lists:foreach(
        fun(HarvesterId) ->
            ?assert(oz_test_utils:call_oz(
                Config, provider_logic, has_eff_harvester, [P1, HarvesterId])
            )
        end, ExpHarvesters
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, provider_logic, has_eff_harvester, [P1, <<"asdiucyaie827346w">>])
    ).


list_eff_spaces_test(Config) ->
    {
        {P1, P1Token}, {P2, P2Token},
        [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}], _Storages
    } = create_2_providers_and_5_supported_spaces(Config),
    Cluster1Member = new_cluster_member_with_privs(Config, P1, [?CLUSTER_VIEW], []),
    Cluster1MemberNoViewPrivs = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_VIEW]),

    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ExpSpaces = [S1, S2, S3, S4, S5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Cluster1Member},
                {admin, [?OZ_PROVIDERS_LIST_RELATIONSHIPS]},
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, Cluster1MemberNoViewPrivs},
                {user, NonAdmin},
                {provider, P2, P2Token}
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
            function = get_eff_spaces,
            args = [auth, P1],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_self_spaces_test(Config) ->
    {
        {P1, P1Token}, _,
        [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}], _Storages
    } = create_2_providers_and_5_supported_spaces(Config),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ExpSpaces = [S1, S2, S3, S4, S5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, P1Token}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/provider/spaces">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_eff_space_test(Config) ->
    {
        {P1, P1Token}, {P2, P2Token}, Spaces, _Storages
    } = create_2_providers_and_5_supported_spaces(Config),
    Cluster1Member = new_cluster_member_with_privs(Config, P1, [?CLUSTER_VIEW], []),
    Cluster1MemberNoViewPrivs = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_VIEW]),

    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    lists:foreach(
        fun({SpaceId, SpaceDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, Cluster1Member},
                        {admin, [?OZ_SPACES_VIEW]},
                        {provider, P1, P1Token}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, Cluster1MemberNoViewPrivs},
                        {user, NonAdmin},
                        {provider, P2, P2Token}
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
                    function = get_eff_space,
                    args = [auth, P1, SpaceId],
                    expected_result = ?OK_MAP_CONTAINS(SpaceDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_space, id = SpaceId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_PROVIDER(P1),
                    expected_result = ?OK_MAP_CONTAINS(SpaceDetails#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = gri:deserialize(EncodedGri),
                            ?assertEqual(Id, SpaceId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, Spaces
    ).


get_own_space_test(Config) ->
    {
        {P1, P1Token}, _, Spaces, _Storages
    } = create_2_providers_and_5_supported_spaces(Config),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    lists:foreach(
        fun({SpaceId, SpaceDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [{provider, P1, P1Token}]
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


legacy_support_space_test(Config) ->
    MinSupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, Cluster1Member} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, Cluster1Member, ?PROVIDER_NAME1
    ),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, BadInviteToken} = oz_test_utils:space_invite_user_token(Config, ?USER(U1), S1),
    {ok, BadInviteTokenSerialized} = tokens:serialize(BadInviteToken),

    % Reused in all specs
    BadValues = [
        {<<"token">>, <<"bad-token">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
        {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
        {<<"token">>, BadInviteTokenSerialized, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_NOT_AN_INVITE_TOKEN(?SPACE_SUPPORT_TOKEN))},
        {<<"size">>, <<"binary">>, ?ERROR_BAD_VALUE_INTEGER(<<"size">>)},
        {<<"size">>, 0, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)},
        {<<"size">>, -1000, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)},
        {<<"size">>, MinSupportSize - 1, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)}
    ],

    VerifyFun = fun(SpaceId) ->
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        % Should return space id of the newly supported space
        {ok, #od_space{
            eff_providers = Providers
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

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            % Only provider 1 is authorized to perform support operation on
            % behalf of provider 1.
            correct = [
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {provider, P2, P2Token}
            ]
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = support_space,
            args = [auth, P1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_provider, id = P1, aspect = support},
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = SpaceId} = gri:deserialize(EncodedGri),
                    VerifyFun(SpaceId)
                end
            })
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
                    {ok, SpaceSupportToken} = oz_test_utils:create_space_support_token(
                        Config, ?USER(U1), Space
                    ),
                    element(2, {ok, _} = tokens:serialize(SpaceSupportToken))
                end],
                <<"size">> => [MinSupportSize]
            },
            bad_values = BadValues
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % provider_logic should also allow using non-serialized tokens, check it
    {ok, BadToken3} = oz_test_utils:space_invite_user_token(
        Config, ?USER(U1), S1
    ),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        % client_spec and logic_spec are inherited from ApiTestSpec
        gs_spec = undefined,
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
                    {ok, SpaceSupportToken} = oz_test_utils:create_space_support_token(
                        Config, ?USER(U1), Space
                    ),
                    SpaceSupportToken
                end],
                <<"size">> => [MinSupportSize]
            },
            bad_values = BadValues ++ [
                {<<"token">>, BadToken3, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_NOT_AN_INVITE_TOKEN(?SPACE_SUPPORT_TOKEN))}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


legacy_update_support_size_test(Config) ->
    MinSupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, Cluster1Member} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, Cluster1Member, ?PROVIDER_NAME1
    ),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    {ok, U1} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
        {ok, S1} = oz_test_utils:support_space_by_legacy_storage(Config, P1, S1),
        {ok, S1} = oz_test_utils:support_space_by_legacy_storage(Config, P2, S1),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        #{spaceId => S1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, Data) ->
        {ok, #od_space{
            eff_providers = #{P1 := {SupportSize, _}}
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
            correct = [{provider, P1, P1Token}]
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
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {provider, P2, P2Token}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = provider_logic,
            function = update_support_size,
            args = [auth, P1, spaceId, data],
            expected_result = ?OK_RES
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


legacy_revoke_support_test(Config) ->
    {ok, Cluster1Member} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, Cluster1Member, ?PROVIDER_NAME1
    ),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    {ok, U1} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
        {ok, S1} = oz_test_utils:support_space_by_legacy_storage(Config, P1, S1),
        {ok, S1} = oz_test_utils:support_space_by_legacy_storage(Config, P2, S1),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        #{spaceId => S1}
    end,
    DeleteEntityFun = fun(#{spaceId := SpaceId} = _Env) ->
        oz_test_utils:unsupport_space(Config, P1, SpaceId),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _Data) ->
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        {ok, #od_space{
            eff_providers = Providers
        }} = oz_test_utils:get_space(Config, SpaceId),
        ?assertEqual(not ShouldSucceed, maps:is_key(P1, Providers))
    end,

    % Check only REST first, and only one correct client at once as a space
    % can only be unsupported once.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            % This is an endpoint dedicated for the provider that presents
            % its authorization, no need to check other providers
            correct = [{provider, P1, P1Token}]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/provider/spaces/">>, spaceId],
            expected_code = ?HTTP_204_NO_CONTENT
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
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {provider, P2, P2Token}
            ]
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = revoke_support,
            args = [auth, P1, spaceId],
            expected_result = ?OK_RES
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


check_my_ports_test(Config) ->
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
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
            correct = [root, nobody, {provider, P1, P1Token}]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider/public/check_my_ports">>,
            expected_code = ?HTTP_200_OK,
            % Convert atoms to binaries in expected body for REST
            expected_body = maps:map(fun(_, ValAtom) ->
                atom_to_binary(ValAtom, utf8)
            end, ExpectedBody)
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = check_my_ports,
            args = [auth, data],
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
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME1
    ),
    ClientIP = list_to_binary(os:cmd("hostname -i") -- "\n"),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {provider, P1, P1Token}
            ]
        },
        % This endpoint makes sense only via REST
        rest_spec = #rest_spec{
            method = get,
            path = <<"/provider/public/check_my_ip">>,
            expected_code = ?HTTP_200_OK,
            expected_body = json_utils:encode(ClientIP)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


map_group_test(Config) ->
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME1
    ),

    oz_test_utils:overwrite_auth_config(Config, #{
        openidConfig => #{
            enabled => true
        },
        supportedIdps => [
            {?DUMMY_IDP, #{
                protocol => openid,
                protocolConfig => #{
                    plugin => default_oidc_plugin,
                    entitlementMapping => #{
                        enabled => true,
                        parser => nested_entitlement_parser,
                        parserConfig => #{
                            splitWith => "/",
                            topGroupType => unit,
                            topGroupPrivilegesInVo => member,
                            subGroupsType => team,
                            subGroupsPrivilegesInParent => member,
                            userPrivileges => member
                        }
                    }
                }
            }}
        ]
    }),

    RawEntitlement = <<"my-unit/my-team/my-subteam">>,
    ExpIdPEntitlement = #idp_entitlement{
        idp = ?DUMMY_IDP,
        privileges = member,
        path = [
            #idp_group{type = unit, name = <<"my-unit">>, privileges = member},
            #idp_group{type = team, name = <<"my-team">>, privileges = member},
            #idp_group{type = team, name = <<"my-subteam">>, privileges = member}
        ]
    },
    ExpGroupId = entitlement_mapping:gen_group_id(ExpIdPEntitlement),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, P1, P1Token}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider/public/map_idp_group">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groupId">> => ExpGroupId}
        },
        data_spec = #data_spec{
            required = [<<"idp">>, <<"groupId">>],
            correct_values = #{
                <<"idp">> => [atom_to_binary(?DUMMY_IDP, utf8)],
                <<"groupId">> => [RawEntitlement]
            },
            bad_values = [
                {<<"idp">>, bad_idp_name, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"idp">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_subdomain_test(Config) ->
    OZDomain = oz_test_utils:oz_domain(Config),
    Subdomain = <<"proper-subdomain">>,
    IPs = [<<"1.2.3.4">>, <<"5.6.7.8">>],

    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),

    Domain = ?UNIQUE_DOMAIN,

    EnvSetUpFun = fun() ->
        {ok, Cluster1Member} = oz_test_utils:create_user(Config),
        {ok, {P1, P1Token}} = oz_test_utils:create_provider(
            Config, Cluster1Member, ?PROVIDER_DETAILS(?PROVIDER_NAME1, Domain)#{<<"subdomainDelegation">> => false}
        ),
        Cluster1MemberNoUpdatePriv = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_UPDATE]),
        #{
            providerId => P1, providerClient => {provider, P1, P1Token},
            clusterMember => {user, Cluster1Member}, clusterMemberNoUpdatePriv => {user, Cluster1MemberNoUpdatePriv}
        }
    end,
    EnvTearDownFun = fun(#{providerId := ProviderId} = _Env) ->
        % delete provider to avoid "subdomain occupied" errors
        oz_test_utils:delete_provider(Config, ProviderId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{providerId := ProviderId} = _Env, _) ->
        {ExpSubdomain, ExpDomain} = case ShouldSucceed of
            false -> {undefined, Domain};
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
                clusterMember,
                providerClient
            ],
            unauthorized = [nobody],
            forbidden = [
                clusterMemberNoUpdatePriv,
                {user, NonAdmin},
                {provider, P2, P2Token}
            ]
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = update_domain_config,
            args = [auth, providerId, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{
                type = od_provider, id = providerId, aspect = domain_config
            },
            expected_result = ?OK_RES
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
                {<<"ipList">>, [{256, 256, 256, 256}], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, [{-1, -1, -1, -1}], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)},
                {<<"ipList">>, [{1, 1, 1, 1, 1}], ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ipList">>)}
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
        <<"adminEmail">> => <<"admin@onezone.example.com">>,
        <<"subdomain">> => Subdomain,
        <<"ipList">> => [<<"2.4.6.8">>, <<"255.253.251.2">>],
        <<"subdomainDelegation">> => true
    },
    OZDomain = oz_test_utils:oz_domain(Config),

    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),

    EnvSetUpFun = fun() ->
        {ok, Cluster1Member} = oz_test_utils:create_user(Config),
        {ok, {P1, P1Token}} = oz_test_utils:create_provider(
            Config, Cluster1Member, ProviderDetails
        ),
        Cluster1MemberNoUpdatePriv = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_UPDATE]),

        % Disable subdomain delegation in onezone to test
        % ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED
        oz_test_utils:set_env(Config, subdomain_delegation_supported, false),

        #{
            providerId => P1, providerClient => {provider, P1, P1Token},
            clusterMember => {user, Cluster1Member}, clusterMemberNoUpdatePriv => {user, Cluster1MemberNoUpdatePriv}
        }
    end,
    EnvTearDownFun = fun(#{providerId := ProviderId} = _Env) ->
        oz_test_utils:set_env(Config, subdomain_delegation_supported, true),
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
                clusterMember,
                providerClient
            ],
            unauthorized = [nobody],
            forbidden = [
                clusterMemberNoUpdatePriv,
                {provider, P2, P2Token}
            ]
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = update_domain_config,
            args = [auth, providerId, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{
                type = od_provider, id = providerId, aspect = domain_config
            },
            expected_result = ?OK_RES
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
                {<<"subdomainDelegation">>, true,
                    ?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED},
                {<<"domain">>, <<"https://hasprotocol">>, ?ERROR_BAD_VALUE_DOMAIN}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, VerifyEndFun
    )).


%%--------------------------------------------------------------------
%% @doc
%% Checks that the same data can be used for update twice without error
%% (issue VFS-4615)
%% @end
%%--------------------------------------------------------------------
update_domain_is_idempotent_test(Config) ->
    Domain = ?UNIQUE_DOMAIN,
    NewDomain = <<"newdomain.com">>,
    ProviderDetails = #{
        <<"name">> => ?PROVIDER_NAME1,
        <<"adminEmail">> => <<"admin@onezone.example.com">>,
        <<"domain">> => Domain,
        <<"subdomainDelegation">> => false
    },
    {ok, Cluster1Member} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, Cluster1Member, ProviderDetails
    ),

    EnvSetUpFun = fun() ->
        #{providerId => P1, providerClient => {provider, P1, P1Token}}
    end,
    VerifyEndFun = fun(true = _ShouldSucceed, #{providerId := ProviderId} = _Env, _Data) ->
        {ok, Provider} = oz_test_utils:get_provider(Config, ProviderId),
        ?assertEqual(NewDomain, Provider#od_provider.domain),
        ?assertEqual(undefined, Provider#od_provider.subdomain),
        ?assertEqual(false, Provider#od_provider.subdomain_delegation),
        true
    end,
    EnvTearDownFun = fun(_Env) -> ok end,
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Cluster1Member},
                providerClient
            ],
            unauthorized = [],
            forbidden = []
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = update_domain_config,
            args = [auth, providerId, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{
                type = od_provider, id = providerId, aspect = domain_config
            },
            expected_result = ?OK_RES
        },
        data_spec = #data_spec{
            required = [<<"subdomainDelegation">>, <<"domain">>],
            correct_values = #{
                <<"subdomainDelegation">> => [false],
                % provided twice to ensure setting same domain twice works (issue VFS-4615)
                <<"domain">> => [NewDomain, NewDomain]
            },
            bad_values = []
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, VerifyEndFun
    )).


get_domain_config_test(Config) ->
    % Test without delegated subdomain

    ExpDomain = ?UNIQUE_DOMAIN,

    {ok, Cluster1Member} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, Cluster1Member, ?PROVIDER_DETAILS(?PROVIDER_NAME1, ExpDomain)#{<<"subdomainDelegation">> => false}
    ),
    Cluster1MemberNoViewPriv = new_cluster_member_with_privs(Config, P1, [], [?CLUSTER_VIEW]),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_DETAILS(?PROVIDER_NAME2, ?UNIQUE_DOMAIN)#{<<"subdomainDelegation">> => false}
    ),

    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ExpBody = #{
        <<"subdomainDelegation">> => false,
        <<"domain">> => ExpDomain,
        <<"ipList">> => [],
        <<"subdomain">> => null
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Cluster1Member},
                {provider, P1, P1Token},
                {admin, [?OZ_PROVIDERS_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, Cluster1MemberNoViewPriv},
                {provider, P2, P2Token}
            ]
        },
        logic_spec = LogicSpec = #logic_spec{
            module = provider_logic,
            function = get_domain_config,
            args = [auth, P1],
            expected_result = ?OK_MAP_CONTAINS(ExpBody)
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/domain_config">>],
            expected_code = ?HTTP_200_OK,
            expected_body = {contains, ExpBody}
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{type = od_provider, id = P1, aspect = domain_config},
            expected_result = ?OK_MAP_CONTAINS(ExpBody#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(Id, P1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % test enabled subdomain delegation
    ExpSubdomain = <<"subdomain">>,
    ExpIPs = [{5, 8, 2, 4}, {10, 12, 255, 255}],
    ExpIPsBin = [<<"5.8.2.4">>, <<"10.12.255.255">>],
    OZDomain = oz_test_utils:oz_domain(Config),
    ExpDomain2 = <<ExpSubdomain/binary, ".", OZDomain/binary>>,

    oz_test_utils:enable_subdomain_delegation(Config, P1, ExpSubdomain, ExpIPs),

    ExpBody2 = #{
        <<"subdomainDelegation">> => true,
        <<"domain">> => ExpDomain2,
        <<"ipList">> => ExpIPs,
        <<"subdomain">> => ExpSubdomain
    },
    ExpBody2Bin = ExpBody2#{<<"ipList">> => ExpIPsBin},
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        logic_spec = LogicSpec#logic_spec{expected_result = ?OK_MAP(ExpBody2)},
        rest_spec = RestSpec#rest_spec{expected_body = {contains, ExpBody2Bin}},
        gs_spec = GsSpec#gs_spec{
            expected_result = ?OK_MAP_CONTAINS(ExpBody2Bin#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(Id, P1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_own_domain_config_test(Config) ->
    ExpDomain = ?UNIQUE_DOMAIN,
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_DETAILS(?PROVIDER_NAME1, ExpDomain)#{<<"subdomainDelegation">> => false}
    ),
    ExpBody = #{
        <<"subdomainDelegation">> => false,
        <<"domain">> => ExpDomain,
        <<"ipList">> => [],
        <<"subdomain">> => null
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, P1, P1Token}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/provider/domain_config">>],
            expected_code = ?HTTP_200_OK,
            expected_body = {contains, ExpBody}
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_provider, id = ?SELF, aspect = domain_config},
            expected_result = ?OK_MAP_CONTAINS(ExpBody#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(Id, P1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_current_time_test(Config) ->
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME1
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {provider, P1, P1Token}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/provider/public/get_current_time">>,
            expected_code = ?HTTP_200_OK,
            expected_body = fun(Value) ->
                maps:get(<<"timeMillis">>, Value) > 0
            end
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = get_current_time,
            args = [auth],
            expected_result = ?OK_TERM(fun(Result) ->
                Result > 0
            end)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


verify_provider_identity_test(Config) ->
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME1
    ),
    {ok, {P2, _}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),

    {ok, DeserializedToken} = tokens:deserialize(P1Token),
    Timestamp = oz_test_utils:call_oz(
        Config, time_utils, cluster_time_seconds, []
    ),
    TokenNoAuth = tokens:confine(
        DeserializedToken, #cv_authorization_none{}
    ),
    TokenNotExpired = tokens:confine(
        TokenNoAuth, #cv_time{valid_until = Timestamp + 100}
    ),
    TokenExpired = tokens:confine(
        TokenNoAuth, #cv_time{valid_until = Timestamp - 200}
    ),
    {ok, TokenNoAuthBin} = tokens:serialize(TokenNoAuth),
    {ok, TokenNotExpiredBin} = tokens:serialize(TokenNotExpired),
    {ok, TokenExpiredBin} = tokens:serialize(TokenExpired),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {provider, P1, P1Token}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider/public/verify_provider_identity">>,
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = provider_logic,
            function = verify_provider_identity,
            args = [auth, data],
            expected_result = ?OK_RES
        },
        % TODO gs
        data_spec = #data_spec{
            required = [
                <<"providerId">>, <<"token">>
            ],
            correct_values = #{
                <<"providerId">> => [P1],
                <<"token">> => [TokenNoAuthBin, TokenNotExpiredBin]
            },
            bad_values = [
                {<<"providerId">>, <<"">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"providerId">>)},
                {<<"providerId">>, 1234, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"providerId">>)},
                {<<"providerId">>, <<"sdfagh2345qwefg">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"providerId">>)},
                {<<"providerId">>, P2, ?ERROR_TOKEN_INVALID},

                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"token">>)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, TokenExpiredBin, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
                    #cv_time{valid_until = Timestamp - 200}
                )}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Make sure "macaroon" field is accepted too (for backward compatibility)
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        data_spec = #data_spec{
            required = [
                <<"providerId">>, <<"macaroon">>
            ],
            correct_values = #{
                <<"providerId">> => [P1],
                <<"macaroon">> => [TokenNoAuthBin, TokenNotExpiredBin]
            },
            bad_values = [
                {<<"providerId">>, <<"">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"providerId">>)},
                {<<"providerId">>, 1234, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"providerId">>)},
                {<<"providerId">>, <<"sdfagh2345qwefg">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"providerId">>)},
                {<<"providerId">>, P2, ?ERROR_TOKEN_INVALID},

                {<<"macaroon">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"macaroon">>)},
                {<<"macaroon">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"macaroon">>, ?ERROR_BAD_TOKEN)},
                {<<"macaroon">>, TokenExpiredBin, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
                    #cv_time{valid_until = Timestamp - 200}
                )}
            ]
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
    init_per_testcase(default, Config);
init_per_testcase(list_eff_harvesters_test, Config) ->
    oz_test_utils:mock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN),
    init_per_testcase(default, Config);
init_per_testcase(_, Config) ->
    Config.


end_per_testcase(check_my_ports_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, http_client),
    end_per_testcase(default, Config);
end_per_testcase(list_eff_harvesters_test, Config) ->
    oz_test_utils:unmock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN),
    end_per_testcase(default, Config);
end_per_testcase(_, Config) ->
    oz_test_utils:set_env(Config, require_token_for_provider_registration, false),
    oz_test_utils:set_env(Config, subdomain_delegation_supported, true),
    ok.


%%%===================================================================
%%% Helper functions
%%%===================================================================


create_2_providers_and_5_supported_spaces(Config) ->
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME2),
    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, St2} = oz_test_utils:create_storage(Config, ?PROVIDER(P2), ?STORAGE_NAME1),
    SupportSize = oz_test_utils:minimum_support_size(Config),

    Spaces = lists:map(
        fun(_) ->
            Name = ?UNIQUE_STRING,
            {ok, SpaceId} = oz_test_utils:create_space(Config, ?ROOT, Name),
            {ok, SpaceId} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, SpaceId, SupportSize),
            {ok, SpaceId} = oz_test_utils:support_space(Config, ?PROVIDER(P2), St2, SpaceId, SupportSize),
            SpaceDetails = #{
                <<"name">> => Name,
                <<"providers">> => #{P1 => SupportSize, P2 => SupportSize}
            },
            {SpaceId, SpaceDetails}
        end, lists:seq(1, 5)
    ),

    {{P1, P1Token}, {P2, P2Token}, Spaces, [St1, St2]}.


new_cluster_member_with_privs(Config, ProviderId, ToGrant, ToRevoke) ->
    ClusterId = ProviderId,
    {ok, NewMember} = oz_test_utils:create_user(Config),
    oz_test_utils:cluster_add_user(Config, ClusterId, NewMember),
    oz_test_utils:cluster_set_user_privileges(
        Config, ClusterId, NewMember, ToGrant, ToRevoke
    ),
    NewMember.
