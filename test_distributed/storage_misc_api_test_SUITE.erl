%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning storage basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(storage_misc_api_test_SUITE).
-author("Michal Stanisz").

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    create_test/1,
    get_test/1,
    update_test/1,
    delete_test/1,

    support_space_test/1,
    revoke_support_test/1,
    update_support_size_test/1,

    list_spaces_test/1,

    upgrade_legacy_support_test/1,

    support_with_imported_storage_test/1,
    modify_imported_storage_test/1
]).

all() ->
    ?ALL([
        create_test,
        get_test,
        update_test,
        delete_test,

        support_space_test,
        revoke_support_test,
        update_support_size_test,

        list_spaces_test,

        upgrade_legacy_support_test,

        support_with_imported_storage_test,
        modify_imported_storage_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


create_test(Config) ->
    % test cases are divided to ensure value of readonly parameter
    create_test_base(Config, false),
    create_test_base(Config, true).


create_test_base(Config, ReadonlyValue) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),

    VerifyFun = fun(StorageId, ExpectedQosParams, ExpectedImported, ExpectedReadonly) ->
        {ok, Storage} = oz_test_utils:get_storage(Config, StorageId),
        ?assertEqual(?CORRECT_NAME, Storage#od_storage.name),
        ?assertEqual(P1, Storage#od_storage.provider),
        ?assertEqual(ExpectedQosParams, Storage#od_storage.qos_parameters),
        ?assertEqual(ExpectedImported, Storage#od_storage.imported),
        ?assertEqual(ExpectedReadonly, Storage#od_storage.readonly),
        true
    end,
    ExpectedQosParams = fun(StorageId, QosParams) ->
        QosParams#{
            <<"storageId">> => StorageId,
            <<"providerId">> => P1
        }
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1}
            ]
        },
        logic_spec = #logic_spec{
            module = storage_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(_, DataSet) ->
                QosParams =
                    case maps:get(<<"qosParameters">>, DataSet, undefined) of
                        undefined -> maps:get(<<"qos_parameters">>, DataSet, #{});
                        Parameters -> Parameters
                    end,
                ExpectedImported = maps:get(<<"imported">>, DataSet, unknown),
                ExpectedReadonly = maps:get(<<"readonly">>, DataSet, false),
                case ExpectedReadonly andalso ExpectedImported =:= false of
                    true ->
                        ?ERROR_REASON(?ERROR_REQUIRES_IMPORTED_STORAGE(<<"'newly created storage'">>));
                    false ->
                        ?OK_TERM(fun(StorageId) ->
                            VerifyFun(StorageId, ExpectedQosParams(StorageId, QosParams), ExpectedImported, ExpectedReadonly)
                        end)
                end
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_storage, aspect = instance},
            expected_result = ?OK_ENV(fun(_, DataSet) ->
                QosParams =
                    case maps:get(<<"qosParameters">>, DataSet, undefined) of
                        undefined -> maps:get(<<"qos_parameters">>, DataSet, #{});
                        Parameters -> Parameters
                    end,
                ExpectedImported = maps:get(<<"imported">>, DataSet, unknown),
                ExpectedReadonly = maps:get(<<"readonly">>, DataSet, false),
                case ExpectedReadonly andalso ExpectedImported =:= false of
                    true ->
                        ?ERROR_REASON(?ERROR_REQUIRES_IMPORTED_STORAGE(<<"'newly created storage'">>));
                    false ->
                        ?OK_MAP_CONTAINS(#{
                            <<"provider">> => P1,
                            <<"gri">> => fun(EncodedGri) ->
                                #gri{id = StorageId} = gri:deserialize(EncodedGri),
                                VerifyFun(StorageId, ExpectedQosParams(StorageId, QosParams), ExpectedImported, ExpectedReadonly)
                            end})
                end
            end)
        },
        data_spec = #data_spec{
            required = [<<"name">>, <<"readonly">>],
            optional = [<<"qos_parameters">>, <<"qosParameters">>, <<"imported">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"qos_parameters">> => [#{<<"key">> => <<"value">>}, #{<<"key">> => 1}, #{<<"key">> => 123.4}],
                <<"qosParameters">> => [#{<<"key">> => <<"value">>}, #{<<"key">> => 1}, #{<<"key">> => 123.4}],
                <<"imported">> => [true, false],
                <<"readonly">> => [ReadonlyValue]
            },
            bad_values = [
                %% @TODO VFS-5856 <<"qos_parameters">> deprecated, included for backward compatibility 
                {<<"qos_parameters">>, <<"binary">>, ?ERROR_BAD_VALUE_JSON(<<"qos_parameters">>)},
                {<<"qos_parameters">>, #{<<"nested">> => #{<<"key">> => <<"value">>}}, ?ERROR_BAD_VALUE_QOS_PARAMETERS},
                {<<"qosParameters">>, <<"binary">>, ?ERROR_BAD_VALUE_JSON(<<"qosParameters">>)},
                {<<"qosParameters">>, #{<<"nested">> => #{<<"key">> => <<"value">>}}, ?ERROR_BAD_VALUE_QOS_PARAMETERS},
                {<<"qosParameters">>, #{<<"providerId">> => <<"not_correct_provider_id">>},
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"qosParameters.providerId">>, [P1])},
                {<<"imported">>, <<"binary">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"imported">>)},
                {<<"readonly">>, <<"binary">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"readonly">>)}
            ] ++ ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P3, P3Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),

    {ok, S} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    SupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, S} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, S, SupportSize),
    {ok, S} = oz_test_utils:support_space_by_provider(Config, P2, S),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ExpectedQosParameters = #{
        <<"key">> => <<"value">>,
        <<"storageId">> => St1,
        <<"providerId">> => P1
    },
    ExpectedImported = false,
    ExpectedReadonly = false,
    oz_test_utils:update_storage(Config, St1, #{<<"qosParameters">> => ExpectedQosParameters}),

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1},
                {provider, P2, P2Token},
                {provider, P3, P3Token}
            ]
        },
        logic_spec = #logic_spec{
            module = storage_logic,
            function = get,
            args = [auth, St1],
            expected_result = ?OK_TERM(
                fun(#od_storage{
                    qos_parameters = QosParameters,
                    imported = Imported,
                    readonly = Readonly,
                    provider = Provider, spaces = Spaces,
                    eff_users = EffUsers, eff_groups = #{},
                    eff_harvesters = #{},
                    eff_providers = EffProviders,
                    eff_spaces = EffSpaces,
                    top_down_dirty = false, bottom_up_dirty = false
                }) ->
                    ?assertEqual(QosParameters, ExpectedQosParameters),
                    ?assertEqual(Imported, ExpectedImported),
                    ?assertEqual(Readonly, ExpectedReadonly),
                    ?assertEqual(Spaces, #{
                        S => SupportSize
                    }),
                    ?assertEqual(EffUsers, #{
                        U1 => [{od_space, S}]
                    }),
                    ?assertEqual(Provider, P1),
                    ?assertEqual(EffProviders, #{P1 => [{od_storage, <<"self">>}]}),
                    ?assertEqual(EffSpaces, #{S => {SupportSize, [{od_storage, <<"self">>}]}})
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_storage, id = St1, aspect = instance},
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?STORAGE_NAME1,
                <<"provider">> => P1,
                <<"qosParameters">> => ExpectedQosParameters,
                <<"qos_parameters">> => ExpectedQosParameters,
                <<"imported">> => ExpectedImported,
                <<"readonly">> => ExpectedReadonly,
                <<"spaces">> => [S],
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(St1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check shared data
    GetSharedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1, P1Token},
                {provider, P2, P2Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P3, P3Token}
            ]
        },
        logic_spec = #logic_spec{
            module = storage_logic,
            function = get_shared_data,
            args = [auth, St1, S],
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"provider">> => P1,
                <<"name">> => ?STORAGE_NAME1,
                <<"qosParameters">> => ExpectedQosParameters
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_storage, id = St1, aspect = instance, scope = shared},
            auth_hint = ?THROUGH_SPACE(S),
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"provider">> => P1,
                <<"name">> => ?STORAGE_NAME1,
                <<"qosParameters">> => ExpectedQosParameters,
                <<"qos_parameters">> => ExpectedQosParameters,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(St1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetSharedDataApiTestSpec)).


update_test(Config) ->
    % test cases are divided to ensure value of readonly parameter
    update_test(Config, false),
    update_test(Config, true).

update_test(Config, ReadonlyValue) ->
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?CORRECT_NAME),
        #{storageId => S1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{storageId := StorageId} = _Env, Data) ->
        {ok, #od_storage{provider = ProviderId} = Storage} = oz_test_utils:get_storage(Config, StorageId),
        case ShouldSucceed of
            false -> #{};
            true ->
                QosParams =
                    case maps:get(<<"qosParameters">>, Data, undefined) of
                        undefined -> maps:get(<<"qos_parameters">>, Data, #{});
                        Parameters -> Parameters
                    end,
                ExpectedQosParams = QosParams#{
                    <<"storageId">> => StorageId,
                    <<"providerId">> => ProviderId
                },
                ExistingQosParams = Storage#od_storage.qos_parameters,
                ExpImportedStorage = maps:get(<<"imported">>, Data, false),
                ExpReadonly = maps:get(<<"readonly">>, Data, false),
                case ExpReadonly andalso ExpImportedStorage =:= false of
                    true ->
                        #{};
                    false ->
                        ?assertEqual(ExpImportedStorage, Storage#od_storage.imported),
                        ?assertEqual(ExpReadonly, Storage#od_storage.readonly),
                        ?assertMatch(ExpectedQosParams, ExistingQosParams)
                end
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P2, P2Token}
            ]
        },
        logic_spec = #logic_spec{
            module = storage_logic,
            function = update,
            args = [auth, storageId, data],
            expected_result = ?OK_ENV(fun(Env, DataSet) ->
                ExpectedImported = maps:get(<<"imported">>, DataSet, unknown),
                ExpectedReadonly = maps:get(<<"readonly">>, DataSet, false),
                case ExpectedReadonly andalso ExpectedImported =:= false of
                    true ->
                        StorageId = maps:get(storageId, Env),
                        ?ERROR_REASON(?ERROR_REQUIRES_IMPORTED_STORAGE(StorageId));
                    false ->
                        ?OK
                end
            end)
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_storage, id = storageId, aspect = instance},
            expected_result = ?OK_ENV(fun(Env, DataSet) ->
                ExpectedImported = maps:get(<<"imported">>, DataSet, unknown),
                ExpectedReadonly = maps:get(<<"readonly">>, DataSet, false),
                case ExpectedReadonly andalso ExpectedImported =:= false of
                    true ->
                        StorageId = maps:get(storageId, Env),
                        ?ERROR_REASON(?ERROR_REQUIRES_IMPORTED_STORAGE(StorageId));
                    false ->
                        ?OK
                end
            end)
        },
        data_spec = #data_spec{
            required = [<<"imported">>, <<"readonly">>],
            at_least_one = [<<"qos_parameters">>, <<"qosParameters">>],
            correct_values = #{
                <<"qos_parameters">> => [#{<<"key">> => <<"value">>}, #{<<"key">> => 1}, #{<<"key">> => 123.4}],
                <<"qosParameters">> => [#{<<"key">> => <<"value">>}, #{<<"key">> => 1}, #{<<"key">> => 123.4}],
                <<"imported">> => [true, false],
                <<"readonly">> => [ReadonlyValue]
            },
            bad_values = [
                %% @TODO VFS-5856 <<"qos_parameters">> deprecated, included for backward compatibility 
                {<<"qos_parameters">>, <<"binary">>, ?ERROR_BAD_VALUE_JSON(<<"qos_parameters">>)},
                {<<"qos_parameters">>, #{<<"nested">> => #{<<"key">> => <<"value">>}}, ?ERROR_BAD_VALUE_QOS_PARAMETERS},
                {<<"qosParameters">>, <<"binary">>, ?ERROR_BAD_VALUE_JSON(<<"qosParameters">>)},
                {<<"qosParameters">>, #{<<"nested">> => #{<<"key">> => <<"value">>}}, ?ERROR_BAD_VALUE_QOS_PARAMETERS},
                {<<"qosParameters">>, #{<<"providerId">> => <<"not_correct_provider_id">>},
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"qosParameters.providerId">>, [P1])},
                {<<"qosParameters">>, #{<<"storageId">> => <<"not_correct_storage_id">>},
                    fun(#{storageId := StorageId}) ->
                        ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"qosParameters.storageId">>, [StorageId])
                    end},
                {<<"imported">>, <<"binary">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"imported">>)},
                {<<"readonly">>, <<"binary">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"readonly">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
        #{storageId => St1}
    end,
    DeleteEntityFun = fun(#{storageId := StorageId} = _Env) ->
        oz_test_utils:delete_storage(Config, StorageId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{storageId := StorageId} = _Env, _) ->
        ?assertEqual(not ShouldSucceed, oz_test_utils:call_oz(Config, storage_logic, exists, [StorageId]))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, P2, P2Token},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = storage_logic,
            function = delete,
            args = [auth, storageId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_storage, id = storageId, aspect = instance},
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


support_space_test(Config) ->
    MinSupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, {SupportingProviderId, _}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
    {ok, {OtherProviderId, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME2),
    {ok, SupportingStorageId} = oz_test_utils:create_storage(Config, ?PROVIDER(SupportingProviderId), ?STORAGE_NAME1),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, BadInviteToken} = oz_test_utils:space_invite_user_token(Config, ?USER(U1), S1),
    {ok, BadInviteTokenSerialized} = tokens:serialize(BadInviteToken),

    IllegalSupportParamsJson = jsonable_record:to_json(#support_parameters{
        accounting_enabled = true,
        dir_stats_service_enabled = false
    }),

    ozt_providers:simulate_version(SupportingProviderId, ?LINE_21_02),

    % Reused in all specs
    BadValues = [
        {<<"token">>, <<"bad-token">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
        {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
        {<<"token">>, BadInviteTokenSerialized, ?ERROR_BAD_VALUE_TOKEN(<<"token">>,
            ?ERROR_NOT_AN_INVITE_TOKEN(?SUPPORT_SPACE, ?INVITE_TOKEN(?USER_JOIN_SPACE, S1)))},
        {<<"size">>, <<"binary">>, ?ERROR_BAD_VALUE_INTEGER(<<"size">>)},
        {<<"size">>, 0, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)},
        {<<"size">>, -1000, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)},
        {<<"size">>, MinSupportSize - 1, ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, MinSupportSize)},
        {<<"spaceSupportParameters">>, 23452, ?ERROR_BAD_DATA(<<"spaceSupportParameters">>)},
        {<<"spaceSupportParameters">>, <<"bad-data">>, ?ERROR_BAD_DATA(<<"spaceSupportParameters">>)},
        {<<"spaceSupportParameters">>, IllegalSupportParamsJson, ?ERROR_BAD_DATA(
            <<"dirStatsServiceEnabled">>, <<"Dir stats service must be enabled if accounting is enabled">>
        )}
    ],

    VerifyFun = fun(SpaceId, Data) ->
        % Should return space id of the newly supported space
        {ok, #od_space{
            storages = Storages,
            support_parameters_registry = SupportParametersRegistry
        }} = oz_test_utils:get_space(Config, SpaceId),

        ?assertEqual(#support_parameters_registry{
            registry = #{
                SupportingProviderId => ozt_spaces:expected_tweaked_support_parameters(#support_parameters{
                    accounting_enabled = kv_utils:get([<<"spaceSupportParameters">>, <<"accountingEnabled">>], Data, false),
                    dir_stats_service_enabled = kv_utils:get([<<"spaceSupportParameters">>, <<"dirStatsServiceEnabled">>], Data, false),
                    dir_stats_service_status = disabled
                })
            }
        }, SupportParametersRegistry),

        % Test also storage_logic:supports_space fun
        oz_test_utils:call_oz(
            Config, storage_logic, supports_space, [SupportingStorageId, SpaceId]
        ) andalso maps:is_key(SupportingStorageId, Storages)

    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            % Only the provider is authorized to perform support operation on behalf of itself.
            correct = [
                {provider, SupportingProviderId}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {provider, OtherProviderId}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = storage_logic,
            function = support_space,
            args = [auth, SupportingStorageId, data],
            expected_result = ?OK_ENV(fun(_, Data) ->
                ?OK_TERM(fun(SpaceId) -> VerifyFun(SpaceId, Data) end)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_storage, id = SupportingStorageId, aspect = support},
            expected_result = ?OK_ENV(fun(_, Data) ->
                ?OK_MAP_CONTAINS(#{
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = SpaceId} = gri:deserialize(EncodedGri),
                        VerifyFun(SpaceId, Data)
                    end
                })
            end)
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
                    {ok, SpInvProvToken} = oz_test_utils:create_space_support_token(
                        Config, ?USER(U1), Space
                    ),
                    element(2, {ok, _} = tokens:serialize(SpInvProvToken))
                end],
                <<"size">> => [MinSupportSize]
            },
            bad_values = BadValues
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % storage_logic should also allow using non-serialized tokens, check it
    {ok, BadToken3} = oz_test_utils:space_invite_user_token(
        Config, ?USER(U1), S1
    ),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        % client_spec and logic_spec are inherited from ApiTestSpec
        gs_spec = undefined,
        data_spec = #data_spec{
            required = [<<"token">>, <<"size">>],
            optional = [<<"spaceSupportParameters">>],
            correct_values = #{
                <<"token">> => [fun() ->
                    % Create a new space and token for every test case
                    % (this value is reused multiple times as many cases of
                    % the api test must be checked).
                    {ok, Space} = oz_test_utils:create_space(
                        Config, ?USER(U1), <<"space">>
                    ),
                    {ok, SpInvProvToken} = oz_test_utils:create_space_support_token(
                        Config, ?USER(U1), Space
                    ),
                    SpInvProvToken
                end],
                <<"size">> => [MinSupportSize],
                <<"spaceSupportParameters">> => [fun() ->
                    RandAccountingEnabled = ?RAND_BOOL(),
                    #{
                        <<"accountingEnabled">> => RandAccountingEnabled,
                        <<"dirStatsServiceEnabled">> => RandAccountingEnabled orelse ?RAND_BOOL()
                    }
                end]
            },
            bad_values = BadValues ++ [
                {<<"token">>, BadToken3, ?ERROR_BAD_VALUE_TOKEN(<<"token">>,
                    ?ERROR_NOT_AN_INVITE_TOKEN(?SUPPORT_SPACE, ?INVITE_TOKEN(?USER_JOIN_SPACE, S1)))}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


update_support_size_test(Config) ->
    MinSupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, Cluster1Member} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, Cluster1Member, ?PROVIDER_NAME1
    ),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, St2} = oz_test_utils:create_storage(Config, ?PROVIDER(P2), ?STORAGE_NAME1),
    {ok, U1} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
        {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, S1),
        {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P2), St2, S1),

        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        #{spaceId => S1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, Data) ->
        {ok, #od_space{
            storages = #{St1 := SupportSize}
        }} = oz_test_utils:get_space(Config, SpaceId),

        ExpSupportSize = case ShouldSucceed of
            true -> maps:get(<<"size">>, Data);
            false -> MinSupportSize
        end,
        ?assertEqual(SupportSize, ExpSupportSize)
    end,

    ApiTestSpec = #api_test_spec{
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
            module = storage_logic,
            function = update_support_size,
            args = [auth, St1, spaceId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_storage, id = St1, aspect = {space, spaceId}},
            expected_result = ?OK
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
    )).


revoke_support_test(Config) ->
    {ok, Cluster1Member} = oz_test_utils:create_user(Config),
    {ok, {SupportingProviderId, _}} = oz_test_utils:create_provider(
        Config, Cluster1Member, ?PROVIDER_NAME1
    ),
    {ok, {OtherProviderId, _}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    {ok, SupportingStorageId} = oz_test_utils:create_storage(Config, ?PROVIDER(SupportingProviderId), ?STORAGE_NAME1),
    {ok, OtherStorageId} = oz_test_utils:create_storage(Config, ?PROVIDER(OtherProviderId), ?STORAGE_NAME1),
    {ok, U1} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
        {ok, SpaceId} = oz_test_utils:support_space(Config, ?PROVIDER(SupportingProviderId), SupportingStorageId, SpaceId),
        {ok, SpaceId} = oz_test_utils:support_space(Config, ?PROVIDER(OtherProviderId), OtherStorageId, SpaceId),

        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        #{spaceId => SpaceId}
    end,
    DeleteEntityFun = fun(#{spaceId := SpaceId} = _Env) ->
        oz_test_utils:unsupport_space(Config, SupportingStorageId, SpaceId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _Data) ->
        {ok, #od_space{
            storages = Storages,
            support_parameters_registry = SupportParametersRegistry
        }} = oz_test_utils:get_space(Config, SpaceId),
        ?assertEqual(not ShouldSucceed, maps:is_key(SupportingStorageId, Storages)),
        ExpectedSupportParametersEntry = case ShouldSucceed of
            true ->
                no_entry;
            false ->
                #support_parameters{
                    accounting_enabled = false,
                    dir_stats_service_enabled = false,
                    dir_stats_service_status = disabled
                }
        end,
        ?assertEqual(ExpectedSupportParametersEntry, maps:get(
            SupportingProviderId, SupportParametersRegistry#support_parameters_registry.registry, no_entry
        ))
    end,

    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, SupportingProviderId}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {provider, OtherProviderId}
            ]
        },
        logic_spec = #logic_spec{
            module = storage_logic,
            function = revoke_support,
            args = [auth, SupportingStorageId, spaceId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_storage, id = SupportingStorageId, aspect = {space, spaceId}},
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_spaces_test(Config) ->
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),

    ExpSpaces = lists:map(
        fun(_) ->
            {ok, SpaceId} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
            {ok, _} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, SpaceId),
            SpaceId
        end, lists:seq(1, 5)
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, P2, P2Token},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = storage_logic,
            function = get_spaces,
            args = [auth, St1],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


upgrade_legacy_support_test(Config) ->
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),

    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, St2} = oz_test_utils:create_storage(Config, ?PROVIDER(P2), ?STORAGE_NAME1),
    {ok, S} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, S} = oz_test_utils:support_space_by_legacy_storage(Config, P1, S),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % First check that provider cannot deceitfully support space using this endpoint
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            forbidden = [
                {provider, P2, P2Token}
            ]
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_storage, id = St2, aspect = {upgrade_legacy_support, S}},
            expected_result = ?OK
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that only own storage can be used to support
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            forbidden = [
                {provider, P1, P1Token}
            ]
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_storage, id = St2, aspect = {upgrade_legacy_support, S}},
            expected_result = ?OK
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    VerifyFun = fun(_) ->
        ?assertEqual(true, oz_test_utils:call_oz(Config, storage_logic, supports_space, [St1, S])),
        ?assertEqual(false, oz_test_utils:call_oz(Config, storage_logic, supports_space, [P1, S])),
        true
    end,

    % Finally check successful migration
    ApiTestSpec3 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody]
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_storage, id = St1, aspect = {upgrade_legacy_support, S}},
            expected_result = ?OK_TERM(VerifyFun)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)),

    % check that dummy storage is no longer supporting a space and new storage is
    ?assertEqual(false, oz_test_utils:call_oz(Config, storage_logic, supports_space, [P1, S])),
    ?assertEqual(true, oz_test_utils:call_oz(Config, storage_logic, supports_space, [St1, S])).


support_with_imported_storage_test(Config) ->
    MinSupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME2),
    Providers = [P1, P2],
    {ok, ImportedStorageP1} = oz_test_utils:create_imported_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    CreateTokenFun = fun(SpaceId) ->
        {ok, SpInvProvToken} = oz_test_utils:create_space_support_token(
            Config, ?USER(U1), SpaceId
        ),
        element(2, {ok, _} = tokens:serialize(SpInvProvToken))
    end,

    AddMultipleNotImportedSupportsFun = fun() ->
        lists:foreach(fun(_) ->
            Provider = lists:nth(rand:uniform(length(Providers)), Providers),
            {ok, St} = oz_test_utils:create_storage(Config, ?PROVIDER(Provider), ?STORAGE_NAME1),
            {ok, _} = oz_test_utils:support_space(Config, ?PROVIDER(Provider), St, S1)
        end, lists:seq(1, 8))
    end,

    % add some supports with not imported storages
    AddMultipleNotImportedSupportsFun(),

    {ok, ImportedStorageP2} = oz_test_utils:create_imported_storage(Config, ?PROVIDER(P2), ?STORAGE_NAME1),
    % support space with imported storage
    {ok, _} = oz_test_utils:support_space(Config, ?PROVIDER(P1), ImportedStorageP1, S1),

    % check that adding next support with imported storage fails
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P2, P2Token}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = storage_logic,
            function = support_space,
            args = [auth, ImportedStorageP2, data],
            expected_result = ?ERROR_REASON(?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(S1, ImportedStorageP1))
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_storage, id = ImportedStorageP2, aspect = support},
            expected_result = ?ERROR_REASON(?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(S1, ImportedStorageP1))
        },
        data_spec = #data_spec{
            required = [<<"token">>, <<"size">>],
            correct_values = #{
                <<"token">> => [fun() -> CreateTokenFun(S1) end],
                <<"size">> => [MinSupportSize]
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % but after enabling multi imported storage support in app.config it should be possible
    oz_test_utils:set_env(Config, allow_multiple_imported_storages_supports, true),

    EnvSetUpFun = fun() ->
        {ok, ImportedStorage} = oz_test_utils:create_imported_storage(Config, ?PROVIDER(P2), ?STORAGE_NAME1),
        #{storageId => ImportedStorage}
    end,
    ApiTestSpec1 = ApiTestSpec#api_test_spec{
        logic_spec = #logic_spec{
            module = storage_logic,
            function = support_space,
            args = [auth, storageId, data],
            expected_result = ?OK_BINARY(S1)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_storage, id = storageId, aspect = support},
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = SpaceId} = gri:deserialize(EncodedGri),
                    ?assertEqual(S1, SpaceId)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec1, EnvSetUpFun, undefined, undefined)),

    % supporting second space with imported storage also should fail
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, P1Token}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = storage_logic,
            function = support_space,
            args = [auth, ImportedStorageP1, data],
            expected_result = ?ERROR_REASON(?ERROR_STORAGE_IN_USE)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_storage, id = ImportedStorageP1, aspect = support},
            expected_result = ?ERROR_REASON(?ERROR_STORAGE_IN_USE)
        },
        data_spec = #data_spec{
            required = [<<"token">>, <<"size">>],
            correct_values = #{
                <<"token">> => [CreateTokenFun(S2)],
                <<"size">> => [MinSupportSize]
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % check that space still can be supported with not imported storages
    AddMultipleNotImportedSupportsFun().

modify_imported_storage_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
    {ok, ImportedStorage} = oz_test_utils:create_imported_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, Storage} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % check that modifying imported storage value is prohibited when storage supports a space
    {ok, _} = oz_test_utils:support_space(Config, ?PROVIDER(P1), ImportedStorage, S1),
    {ok, _} = oz_test_utils:support_space(Config, ?PROVIDER(P1), Storage, S1),

    ApiTestSpecFun = fun(St, IsImported) ->
        #api_test_spec{
            client_spec = #client_spec{
                correct = [{provider, P1, P1Token}]
            },
            rest_spec = undefined,
            logic_spec = #logic_spec{
                module = storage_logic,
                function = update,
                args = [auth, St, data],
                expected_result = ?ERROR_REASON(?ERROR_STORAGE_IN_USE)
            },
            gs_spec = #gs_spec{
                operation = update,
                gri = #gri{type = od_storage, id = St, aspect = instance},
                expected_result = ?ERROR_REASON(?ERROR_STORAGE_IN_USE)
            },
            data_spec = #data_spec{
                at_least_one = [<<"imported">>],
                correct_values = #{
                    <<"imported">> => [not IsImported]
                }
            }
        }
    end,
    ?assert(api_test_utils:run_tests(Config, ApiTestSpecFun(ImportedStorage, true))),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpecFun(Storage, false))),

    % check that modifying other values is allowed when storage supports a space
    % also check that not changing imported value will not generate error
    ApiTestSpec1Fun = fun(St, IsImported) ->
        #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {provider, P1, P1Token}
                ]
            },
            logic_spec = #logic_spec{
                module = storage_logic,
                function = update,
                args = [auth, St, data],
                expected_result = ?OK
            },
            gs_spec = #gs_spec{
                operation = update,
                gri = #gri{type = od_storage, id = St, aspect = instance},
                expected_result = ?OK
            },
            data_spec = #data_spec{
                at_least_one = [<<"qos_parameters">>, <<"qosParameters">>, <<"imported">>, <<"name">>],
                correct_values = #{
                    %% @TODO VFS-5856 <<"qos_parameters">> deprecated, included for backward compatibility 
                    <<"qos_parameters">> => [#{<<"key">> => <<"value">>}],
                    <<"qosParameters">> => [#{<<"key">> => <<"value">>}],
                    <<"name">> => [<<"some_other_name">>],
                    <<"imported">> => [IsImported]
                }
            }
        }
    end,
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec1Fun(ImportedStorage, true))),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec1Fun(Storage, false))),

    % check that imported value can be changed, if previously was set to unknown
    EnvSetUpFun = fun() ->
        oz_test_utils:call_oz(Config, od_storage, update,
            [Storage, fun(St) -> {ok, St#od_storage{imported = unknown}} end]),
        #{}
    end,
    VerifyEndFun = fun(ShouldSucceed, _Env, Data) ->
        {ok, St} = oz_test_utils:get_storage(Config, Storage),
        ExpImported = case ShouldSucceed of
            true -> maps:get(<<"imported">>, Data);
            false -> unknown
        end,
        ?assertEqual(ExpImported, St#od_storage.imported)
    end,
    ApiTestSpec2 =
        #api_test_spec{
            client_spec = #client_spec{
                correct = [{provider, P1, P1Token}]
            },
            rest_spec = undefined,
            logic_spec = #logic_spec{
                module = storage_logic,
                function = update,
                args = [auth, Storage, data],
                expected_result = ?OK
            },
            gs_spec = #gs_spec{
                operation = update,
                gri = #gri{type = od_storage, id = Storage, aspect = instance},
                expected_result = ?OK
            },
            data_spec = #data_spec{
                at_least_one = [<<"imported">>],
                correct_values = #{
                    <<"imported">> => [true, false]
                }
            }
        },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    ozt:init_per_suite(Config).


end_per_suite(_Config) ->
    application:stop(hackney),
    ssl:stop().
