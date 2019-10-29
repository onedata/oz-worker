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

    revamp_space_support_test/1
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

        revamp_space_support_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


create_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),

    VerifyFun = fun(StorageId) ->
        {ok, Storage} = oz_test_utils:get_storage(Config, StorageId),
        ?assertEqual(?CORRECT_NAME, Storage#od_storage.name),
        ?assertEqual(P1, Storage#od_storage.provider),
        true
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
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_storage, id = <<"storageId">>, aspect = instance},
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"provider">> => P1,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = StorageId} = gri:deserialize(EncodedGri),
                    VerifyFun(StorageId)
                end
            })
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            correct_values = #{<<"name">> => [?CORRECT_NAME]},
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
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

    ExpectedQosParameters = #{<<"key">> => <<"value">>},
    oz_test_utils:update_storage(Config, St1, #{<<"qos_parameters">> => ExpectedQosParameters}),

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
                    provider = Provider, spaces = Spaces,
                    eff_users = EffUsers, eff_groups = #{},
                    eff_harvesters = #{},
                    eff_providers = EffProviders,
                    eff_spaces = EffSpaces,
                    top_down_dirty = false, bottom_up_dirty = false
                }) ->
                    ?assertEqual(QosParameters, ExpectedQosParameters),
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
                <<"provider">> => P1,
                <<"qos_parameters">> => ExpectedQosParameters,
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
                <<"qos_parameters">> => ExpectedQosParameters
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_storage, id = St1, aspect = instance, scope = shared},
            auth_hint = ?THROUGH_SPACE(S),
            expected_result = ?OK_MAP_CONTAINS(#{
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
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?CORRECT_NAME),
        #{storageId => S1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{storageId := StorageId} = _Env, Data) ->
        {ok, Storage} = oz_test_utils:get_storage(Config, StorageId),
        ExpoQosParams = case ShouldSucceed of
            false -> #{};
            true -> maps:get(<<"qos_parameters">>, Data)
        end,
        ?assertEqual(ExpoQosParams, Storage#od_storage.qos_parameters)
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
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_storage, id = storageId, aspect = instance},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            at_least_one = [<<"qos_parameters">>],
            correct_values = #{
                <<"qos_parameters">> => [#{<<"key">> => <<"value">>}]
            },
            bad_values = [
                {<<"qos_parameters">>, <<"binary">>, ?ERROR_BAD_VALUE_JSON(<<"qos_parameters">>)},
                {<<"qos_parameters">>, #{<<"nested">> => #{<<"key">> => <<"value">>}}, ?ERROR_BAD_VALUE_QOS_PARAMETERS},
                {<<"qos_parameters">>, #{<<"key">> => 1}, ?ERROR_BAD_VALUE_QOS_PARAMETERS}
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
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME2),
    {ok, Storage} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, BadInviteToken} = oz_test_utils:space_invite_user_token(Config, ?USER(U1), S1),
    {ok, BadInviteTokenSerialized} = tokens:serialize(BadInviteToken),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

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
        % Should return space id of the newly supported space
        {ok, #od_space{
            storages = Storages
        }} = oz_test_utils:get_space(Config, SpaceId),

        % Test also storage_logic:supports_space fun
        oz_test_utils:call_oz(
            Config, storage_logic, supports_space, [Storage, SpaceId]
        ) andalso maps:is_key(Storage, Storages)

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
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = storage_logic,
            function = support_space,
            args = [auth, Storage, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_storage, id = Storage, aspect = support},
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

    % storage_logic should also allow using non-serialized macaroons, check it
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
                    {ok, SpInvProvToken} = oz_test_utils:create_space_support_token(
                        Config, ?USER(U1), Space
                    ),
                    SpInvProvToken
                end],
                <<"size">> => [MinSupportSize]
            },
            bad_values = BadValues ++ [
                {<<"token">>, BadToken3, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_NOT_AN_INVITE_TOKEN(?SPACE_SUPPORT_TOKEN))}
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
    DeleteEntityFun = fun(#{spaceId := SpaceId} = _Env) ->
        oz_test_utils:unsupport_space(Config, St1, SpaceId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _Data) ->
        {ok, #od_space{
            storages = Storages
        }} = oz_test_utils:get_space(Config, SpaceId),
        ?assertEqual(not ShouldSucceed, maps:is_key(St1, Storages))
    end,

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
            module = storage_logic,
            function = revoke_support,
            args = [auth, St1, spaceId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_storage, id = St1, aspect = {space, spaceId}},
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
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


revamp_space_support_test(Config) ->
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),

    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, St2} = oz_test_utils:create_storage(Config, ?PROVIDER(P2), ?STORAGE_NAME1),
    {ok, S} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, S} = oz_test_utils:support_space_by_provider(Config, P1, S),

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
            gri = #gri{type = od_storage, id = St2, aspect = {revamp_space_support, S}},
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
            gri = #gri{type = od_storage, id = St2, aspect = {revamp_space_support, S}},
            expected_result = ?OK
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % Finally check correct migration
    VerifyFun = fun(_) ->
        ?assertEqual(true, oz_test_utils:call_oz(Config, storage_logic, supports_space, [St1, S])),
        ?assertEqual(false, oz_test_utils:call_oz(Config, storage_logic, supports_space, [P1, S])),
        true
    end,

    ApiTestSpec3 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, P2, P2Token}
            ]
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_storage, id = St1, aspect = {revamp_space_support, S}},
            expected_result = ?OK_TERM(VerifyFun)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)).




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
