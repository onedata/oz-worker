%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning atm_lambda basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_api_test_SUITE).
-author("Lukasz Opiola").

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/automation/automation.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/errors.hrl").

-include("ozt.hrl").
-include("api_test_utils.hrl").

-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    create_test/1,
    list_test/1,
    get_test/1,
    get_atm_inventories_test/1,
    get_atm_workflow_schemas_test/1,
    dump_test/1,
    update_test/1,
    add_revision_test/1,
    update_revision_lifecycle_state_test/1,
    dump_revision_test/1,
    link_to_inventory_test/1,
    unlink_from_inventory_test/1,
    recreate_atm_lambda_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        get_atm_inventories_test,
        get_atm_workflow_schemas_test,
        dump_test,
        update_test,
        add_revision_test,
        update_revision_lifecycle_state_test,
        dump_revision_test,
        link_to_inventory_test,
        unlink_from_inventory_test,
        recreate_atm_lambda_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    Creator = ozt_users:create(),
    MemberWithNoPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, MemberWithNoPriv, privileges:atm_inventory_admin() -- [?ATM_INVENTORY_MANAGE_LAMBDAS]),

    RevisionNumber = ?RAND_INT(1, 100),

    VerifyFun = fun(AtmLambdaId, Data, CheckCreator) ->
        AtmLambdaRecord = ozt_atm_lambdas:get(AtmLambdaId),
        ExpOriginalAtmLambdaId = maps:get(<<"originalAtmLambdaId">>, Data, undefined),

        InitialRevisionData = maps:get(<<"revision">>, Data),
        RevisionNumber = maps:get(<<"originalRevisionNumber">>, InitialRevisionData),
        AtmLambdaRevision = jsonable_record:from_json(
            maps:get(<<"atmLambdaRevision">>, InitialRevisionData), atm_lambda_revision
        ),
        ExpRevisionRegistry = #{RevisionNumber => AtmLambdaRevision},

        ExpAtmInventories = [AtmInventoryId],
        ExpCreationTime = ozt_mocks:get_frozen_time_seconds(),
        ExpCreator = case CheckCreator of
            {true, UserId} -> ?SUB(user, UserId);
            false -> AtmLambdaRecord#od_atm_lambda.creator
        end,

        ?assertMatch(#od_atm_lambda{
            revision_registry = #atm_lambda_revision_registry{registry = ExpRevisionRegistry},

            original_atm_lambda = ExpOriginalAtmLambdaId,
            atm_inventories = ExpAtmInventories,

            creation_time = ExpCreationTime,
            creator = ExpCreator
        }, AtmLambdaRecord),
        true
    end,

    BadDataValues = [
        {<<"revision">>, bad_spec, ?ERROR_BAD_VALUE_JSON(<<"revision">>)},
        {<<"revision">>, [1234], ?ERROR_BAD_VALUE_JSON(<<"revision">>)},
        {<<"originalAtmLambdaId">>, [1234], ?ERROR_BAD_VALUE_BINARY(<<"originalAtmLambdaId">>)},
        {<<"originalAtmLambdaId">>, <<>>, ?ERROR_BAD_VALUE_EMPTY(<<"originalAtmLambdaId">>)}
    ],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, Creator}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, MemberWithNoPriv},
                {user, NonAdmin}
            ]
        },
        logic_spec = LogicSpec = #logic_spec{
            module = atm_lambda_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(_Env, Data) ->
                ?OK_TERM(fun(Result) -> VerifyFun(Result, Data, {true, Creator}) end)
            end)
        },
        rest_spec = RestSpec = #rest_spec{
            method = post,
            path = <<"/atm_lambdas">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, Data) ->
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/atm_lambdas/">>]),
                    [AtmLambdaId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(AtmLambdaId, Data, {true, Creator})
                end
            end)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = create,
            gri = #gri{type = od_atm_lambda, aspect = instance},
            expected_result_op = ?OK_ENV(fun(_, Data) ->
                ?OK_MAP_CONTAINS(#{
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
                        VerifyFun(Id, Data, {true, Creator})
                    end
                })
            end)
        },
        data_spec = DataSpec = #data_spec{
            required = [
                <<"atmInventoryId">>,
                <<"revision">>
            ],
            optional = [
                <<"originalAtmLambdaId">>
            ],
            correct_values = #{
                <<"atmInventoryId">> => [AtmInventoryId],
                <<"revision">> => [#{
                    <<"originalRevisionNumber">> => RevisionNumber,
                    <<"atmLambdaRevision">> => ozt_atm_lambdas:example_revision_json()
                }],
                <<"originalAtmLambdaId">> => [?RAND_STR(16)]
            },
            bad_values = lists:flatten([
                {<<"atmInventoryId">>, 1234, ?ERROR_FORBIDDEN},
                {<<"atmInventoryId">>, <<"">>, ?ERROR_FORBIDDEN},
                {<<"atmInventoryId">>, <<"asdq4ewfs">>, ?ERROR_FORBIDDEN},
                BadDataValues
            ])
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Root client bypasses authorization checks,
    % hence wrong values of atmInventoryId
    % cause validation errors rather than authorization errors.
    RootApiTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_ATM_INVENTORIES_UPDATE]},
                root
            ]
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?OK_ENV(fun(_Env, Data) ->
                ?OK_TERM(fun(Result) -> VerifyFun(Result, Data, false) end)
            end)
        },
        rest_spec = RestSpec#rest_spec{
            expected_headers = ?OK_ENV(fun(_, Data) ->
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/atm_lambdas/">>]),
                    [AtmLambdaId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(AtmLambdaId, Data, false)
                end
            end)
        },
        gs_spec = GsSpec#gs_spec{
            expected_result_op = ?OK_ENV(fun(_, Data) ->
                ?OK_MAP_CONTAINS(#{
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
                        VerifyFun(Id, Data, false)
                    end
                })
            end)
        },
        data_spec = DataSpec#data_spec{
            bad_values = lists:flatten([
                {<<"atmInventoryId">>, <<"">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"atmInventoryId">>)},
                {<<"atmInventoryId">>, <<"asdq4ewfs">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"atmInventoryId">>)},
                BadDataValues
            ])
        }
    },
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec)).


list_test(Config) ->
    % Make sure that atm_lambdas created in other tests are deleted.
    ozt:delete_all_entities(),

    Creators = lists:map(fun(_) -> ozt_users:create() end, lists:seq(1, rand:uniform(8))),
    NonAdmin = ozt_users:create(),

    AtmInventories = lists:flatmap(fun(Creator) ->
        lists:map(fun(_) -> ozt_users:create_atm_inventory_for(Creator) end, lists:seq(1, rand:uniform(8)))
    end, Creators),

    AtmLambdas = lists:flatmap(fun(AtmInventoryId) ->
        lists:map(fun(_) -> ozt_atm_lambdas:create(AtmInventoryId) end, lists:seq(1, rand:uniform(8)))
    end, AtmInventories),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = lists:flatten([
                [{user, C} || C <- Creators],
                {user, NonAdmin}
            ])
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/atm_lambdas">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_lambdas">> => AtmLambdas}
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = list,
            args = [auth],
            expected_result = ?OK_LIST(AtmLambdas)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also atm_lambda_logic:exist function
    lists:foreach(fun(AtmLambdaId) ->
        ?assert(ozt:rpc(atm_lambda_logic, exists, [AtmLambdaId]))
    end, AtmLambdas),
    ?assert(not ozt:rpc(atm_lambda_logic, exists, [<<"asdiucyaie827346w">>])).


get_test(Config) ->
    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, AnotherMember, []),

    AtmLambdaData = ozt_atm_lambdas:example_data_json(),
    AtmLambdaId = ozt_atm_lambdas:create(?USER(Creator), AtmInventoryId, AtmLambdaData),
    AtmLambdaDataWithInventory = AtmLambdaData#{
        <<"atmInventoryId">> => AtmInventoryId
    },

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW]},
                {user, Creator},
                {user, AnotherMember}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_lambdas/">>, AtmLambdaId],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:private_atm_lambda(rest, AtmLambdaId, AtmLambdaDataWithInventory, ?SUB(user, Creator))
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = get,
            args = [auth, AtmLambdaId],
            expected_result = api_test_expect:private_atm_lambda(logic, AtmLambdaId, AtmLambdaDataWithInventory, ?SUB(user, Creator))
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_atm_lambda, id = AtmLambdaId,
                aspect = instance, scope = private
            },
            auth_hint = ?THROUGH_ATM_INVENTORY(AtmInventoryId),
            expected_result_op = api_test_expect:private_atm_lambda(gs, AtmLambdaId, AtmLambdaDataWithInventory, ?SUB(user, Creator))
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_atm_inventories_test(Config) ->
    Creators = lists:map(fun(_) -> ozt_users:create() end, lists:seq(1, rand:uniform(8))),
    NonAdmin = ozt_users:create(),

    AtmInventories = lists:flatmap(fun(Creator) ->
        lists:map(fun(_) -> ozt_users:create_atm_inventory_for(Creator) end, lists:seq(1, rand:uniform(8)))
    end, Creators),

    AtmLambdaData = ozt_atm_lambdas:example_data_json(),
    AtmLambdaId = ozt_atm_lambdas:create(hd(AtmInventories), AtmLambdaData),
    lists:foreach(fun(AtmInventoryId) ->
        ozt_atm_lambdas:link_to_inventory(AtmLambdaId, AtmInventoryId)
    end, tl(AtmInventories)),

    AnotherMember = ozt_users:create(),
    ozt_atm_inventories:add_user(hd(AtmInventories), AnotherMember, []),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = lists:flatten([
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW]},
                [{user, C} || C <- Creators],
                {user, AnotherMember}
            ]),
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_lambdas/">>, AtmLambdaId, <<"/atm_inventories">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_inventories">> => AtmInventories}
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = get_atm_inventories,
            args = [auth, AtmLambdaId],
            expected_result = ?OK_LIST(AtmInventories)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_atm_workflow_schemas_test(Config) ->
    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AnotherMember = ozt_users:create(),

    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, AnotherMember, []),

    AtmLambdaId = ozt_atm_lambdas:create(AtmInventoryId),

    ExpAtmWorkflowSchemas = lists:map(fun(_) ->
        gen_atm_workflow_schema_with_lambda(AtmInventoryId, AtmLambdaId)
    end, lists:seq(1, rand:uniform(8))),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = lists:flatten([
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW]},
                {user, Creator},
                {user, AnotherMember}
            ]),
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_lambdas/">>, AtmLambdaId, <<"/atm_workflow_schemas">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_workflow_schemas">> => ExpAtmWorkflowSchemas}
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = get_atm_workflow_schemas,
            args = [auth, AtmLambdaId],
            expected_result = ?OK_LIST(ExpAtmWorkflowSchemas)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


dump_test(Config) ->
    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, AnotherMember, []),

    RevisionNumber = ?RAND_REV_NUMBER(),
    AtmLambdaRevision = atm_test_utils:example_lambda_revision(),
    AtmLambdaData = #{
        <<"revision">> => #{
            <<"originalRevisionNumber">> => RevisionNumber,
            <<"atmLambdaRevision">> => jsonable_record:to_json(AtmLambdaRevision, atm_lambda_revision)
        }
    },
    AtmLambdaId = ozt_atm_lambdas:create(?USER(Creator), AtmInventoryId, AtmLambdaData),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW]},
                {user, Creator},
                {user, AnotherMember}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/atm_lambdas/">>, AtmLambdaId, <<"/dump">>],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:json_dump_of_atm_lambda(rest, AtmLambdaId, AtmLambdaRevision, RevisionNumber)
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = dump,
            args = [auth, AtmLambdaId, data],
            expected_result = api_test_expect:json_dump_of_atm_lambda(logic, AtmLambdaId, AtmLambdaRevision, RevisionNumber)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{
                type = od_atm_lambda, id = AtmLambdaId,
                aspect = dump, scope = private
            },
            expected_result_op = api_test_expect:json_dump_of_atm_lambda(gs, AtmLambdaId, AtmLambdaRevision, RevisionNumber)
        },
        data_spec = #data_spec{
            required = [
                <<"includeRevision">>
            ],
            correct_values = #{
                <<"includeRevision">> => [RevisionNumber]
            },
            bad_values = [
                {<<"includeRevision">>, #{<<"bad">> => <<"data">>}, ?ERROR_BAD_VALUE_INTEGER(<<"includeRevision">>)},
                {<<"includeRevision">>, -7, ?ERROR_BAD_VALUE_TOO_LOW(<<"includeRevision">>, 1)},
                {<<"includeRevision">>, 9999999, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"includeRevision">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_test(Config) ->
    update_test_base(Config, new_revision_other_than_initial),
    update_test_base(Config, new_revision_same_as_initial).

update_test_base(Config, ScenarioType) ->
    Creator = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, AnotherMember, []),

    InitialRevisionNumber = ?RAND_REV_NUMBER(),

    NewRevisionNumber = case ScenarioType of
        new_revision_same_as_initial -> InitialRevisionNumber;
        new_revision_other_than_initial -> lists_utils:random_element(lists:seq(1, 100) -- [InitialRevisionNumber])
    end,

    InitialLambdaData = #{
        <<"revision">> => #{
            <<"originalRevisionNumber">> => InitialRevisionNumber,
            <<"atmLambdaRevision">> => ozt_atm_lambdas:example_revision_json()
        }
    },

    EnvSetUpFun = fun() ->
        AtmLambdaId = ozt_atm_lambdas:create(AtmInventoryId, InitialLambdaData),
        AtmLambdaRecord = ozt_atm_lambdas:get(AtmLambdaId),
        #{
            atm_lambda_id => AtmLambdaId,
            initial_revision_registry => AtmLambdaRecord#od_atm_lambda.revision_registry
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{
        atm_lambda_id := AtmLambdaId,
        initial_revision_registry := InitialRevisionRegistry
    }, Data) ->
        AtmLambdaRecord = ozt_atm_lambdas:get(AtmLambdaId),

        ExpRevisionRegistry = case ShouldSucceed of
            false ->
                InitialRevisionRegistry;
            true ->
                NewAtmLambdaRevision = jsonable_record:from_json(
                    kv_utils:get([<<"revision">>, <<"atmLambdaRevision">>], Data),
                    atm_lambda_revision
                ),
                case ScenarioType of
                    new_revision_other_than_initial ->
                        InitialRevisionRegistry#atm_lambda_revision_registry{
                            registry = maps:merge(InitialRevisionRegistry#atm_lambda_revision_registry.registry, #{
                                NewRevisionNumber => NewAtmLambdaRevision
                            })
                        };
                    new_revision_same_as_initial ->
                        InitialRevisionRegistry
                end
        end,

        ?assertEqual(ExpRevisionRegistry, AtmLambdaRecord#od_atm_lambda.revision_registry)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_UPDATE]},
                {user, Creator}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, AnotherMember},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/atm_lambdas/">>, atm_lambda_id],
            expected_code = case ScenarioType of
                new_revision_same_as_initial -> ?HTTP_409_CONFLICT;
                _ -> ?HTTP_204_NO_CONTENT
            end
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = update,
            args = [auth, atm_lambda_id, data],
            expected_result = case ScenarioType of
                new_revision_same_as_initial -> ?ERROR_REASON(?ERROR_ALREADY_EXISTS);
                _ -> ?OK_RES
            end
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_atm_lambda, id = atm_lambda_id, aspect = instance},
            expected_result_op = case ScenarioType of
                new_revision_same_as_initial -> ?ERROR_REASON(?ERROR_ALREADY_EXISTS);
                _ -> ?OK_RES
            end
        },
        data_spec = #data_spec{
            required = [<<"revision">>],
            correct_values = #{
                <<"revision">> => [#{
                    <<"originalRevisionNumber">> => NewRevisionNumber,
                    <<"atmLambdaRevision">> => ozt_atm_lambdas:example_revision_json()
                }]
            },
            bad_values = [
                {<<"revision">>, str_utils:rand_hex(250), ?ERROR_BAD_VALUE_JSON(<<"revision">>)},
                {<<"revision">>, #{
                    <<"originalRevisionNumber">> => NewRevisionNumber,
                    <<"atmLambdaRevision">> => #{<<"a">> => <<"b">>}
                }, ?ERROR_BAD_DATA(<<"atmLambdaRevision">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


add_revision_test(Config) ->
    add_revision_test_base(Config, take_original_revision_number),
    add_revision_test_base(Config, override_original_revision_number),

    % test validation of revision number in the aspect
    Creator = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    AtmLambdaId = ozt_atm_lambdas:create(AtmInventoryId),
    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, Creator}]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/atm_lambdas/">>, AtmLambdaId, <<"/revision/">>, <<"0">>],
            expected_code = ?HTTP_400_BAD_REQUEST
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = add_revision,
            args = [auth, AtmLambdaId, <<"string">>, data],
            expected_result = ?ERROR_REASON(?ERROR_BAD_DATA(<<"targetRevisionNumber">>))
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = {revision, <<"undefined">>}},
            expected_result_op = ?ERROR_REASON(?ERROR_BAD_DATA(<<"targetRevisionNumber">>))
        },
        data_spec = #data_spec{
            required = [
                <<"atmLambdaRevision">>
            ],
            correct_values = #{
                <<"atmLambdaRevision">> => [ozt_atm_lambdas:example_revision_json()]
            },
            bad_values = []
        }
    })).


add_revision_test_base(Config, RevisionNumberProvisionMode) ->
    add_revision_test_base(Config, RevisionNumberProvisionMode, new_revision_other_than_initial),
    add_revision_test_base(Config, RevisionNumberProvisionMode, new_revision_same_as_initial).

add_revision_test_base(Config, RevisionNumberProvisionMode, ScenarioType) ->
    Creator = ozt_users:create(),
    MemberWithNoPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    EnvSetUpFun = fun() ->
        AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
        ozt_atm_inventories:add_user(
            AtmInventoryId, MemberWithNoPriv, privileges:atm_inventory_admin() -- [?ATM_INVENTORY_MANAGE_LAMBDAS]
        ),

        RandomRevisions = lists:usort(lists_utils:random_sublist([
            ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER()
        ], 1, all)),
        {PreexistingRevisionNumbers, RevisionNumberToAdd} = case ScenarioType of
            new_revision_other_than_initial ->
                {RandomRevisions, lists_utils:random_element(lists:seq(1, 100) -- RandomRevisions)};
            new_revision_same_as_initial ->
                {RandomRevisions, lists_utils:random_element(RandomRevisions)}
        end,
        AtmLambdaId = ozt_atm_lambdas:create(AtmInventoryId, #{
            <<"revision">> => #{
                <<"originalRevisionNumber">> => hd(PreexistingRevisionNumbers),
                <<"atmLambdaRevision">> => ozt_atm_lambdas:example_revision_json()
            }
        }),
        lists:foreach(fun(RevisionNumber) ->
            ozt_atm_lambdas:add_revision(AtmLambdaId, RevisionNumber, #{
                <<"atmLambdaRevision">> => ozt_atm_lambdas:example_revision_json()
            })
        end, tl(PreexistingRevisionNumbers)),

        #{
            atm_lambda_id => AtmLambdaId,
            preexisting_revision_numbers => PreexistingRevisionNumbers,
            revision_number_to_add => RevisionNumberToAdd,
            target_revision_number_binary => case RevisionNumberProvisionMode of
                take_original_revision_number -> <<"auto">>;
                override_original_revision_number -> integer_to_binary(RevisionNumberToAdd)
            end,
            original_revision_number => case RevisionNumberProvisionMode of
                take_original_revision_number -> RevisionNumberToAdd;
                override_original_revision_number -> RevisionNumberToAdd + 6 % should be ignored
            end
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{
        atm_lambda_id := AtmLambdaId,
        preexisting_revision_numbers := PreexistingRevisionNumbers,
        revision_number_to_add := RevisionToAdd
    }, Data) ->
        #od_atm_lambda{
            revision_registry = #atm_lambda_revision_registry{
                registry = Registry
            }
        } = ozt_atm_lambdas:get(AtmLambdaId),

        ExpectedFinalRevisionNumbers = case ShouldSucceed of
            false ->
                PreexistingRevisionNumbers;
            true ->
                case lists:member(RevisionToAdd, PreexistingRevisionNumbers) of
                    true -> PreexistingRevisionNumbers;
                    false -> [RevisionToAdd | PreexistingRevisionNumbers]
                end
        end,
        ?assertEqual(lists:sort(ExpectedFinalRevisionNumbers), lists:sort(maps:keys(Registry))),

        case ShouldSucceed of
            false ->
                ok;
            true ->
                ActualNewRevision = maps:get(RevisionToAdd, Registry, undefined),
                RequestedNewRevision = jsonable_record:from_json(maps:get(<<"atmLambdaRevision">>, Data), atm_lambda_revision),
                case ScenarioType of
                    new_revision_same_as_initial ->
                        ?assertNotEqual(ActualNewRevision, RequestedNewRevision);
                    _ ->
                        ?assertEqual(ActualNewRevision, RequestedNewRevision)
                end
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_UPDATE]},
                {user, Creator}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, MemberWithNoPriv},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/atm_lambdas/">>, atm_lambda_id, <<"/revision/">>, target_revision_number_binary],
            expected_code = case ScenarioType of
                new_revision_same_as_initial -> ?HTTP_409_CONFLICT;
                _ -> ?HTTP_204_NO_CONTENT
            end
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = add_revision,
            args = [auth, atm_lambda_id, target_revision_number_binary, data],
            expected_result = case ScenarioType of
                new_revision_same_as_initial -> ?ERROR_REASON(?ERROR_ALREADY_EXISTS);
                _ -> ?OK_RES
            end
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_atm_lambda, id = atm_lambda_id, aspect = {revision, target_revision_number_binary}},
            expected_result_op = case ScenarioType of
                new_revision_same_as_initial -> ?ERROR_REASON(?ERROR_ALREADY_EXISTS);
                _ -> ?OK_RES
            end
        },
        data_spec = #data_spec{
            required = lists:flatten([
                <<"atmLambdaRevision">>,
                case RevisionNumberProvisionMode of
                    override_original_revision_number -> [];
                    take_original_revision_number -> <<"originalRevisionNumber">>
                end
            ]),
            optional = lists:flatten([
                case RevisionNumberProvisionMode of
                    take_original_revision_number -> [];
                    override_original_revision_number -> <<"originalRevisionNumber">>
                end
            ]),
            correct_values = #{
                <<"originalRevisionNumber">> => [fun(#{original_revision_number := OriginalRevisionNumber}) ->
                    OriginalRevisionNumber
                end],
                <<"atmLambdaRevision">> => [ozt_atm_lambdas:example_revision_json()]
            },
            bad_values = lists:flatten([
                case RevisionNumberProvisionMode of
                    override_original_revision_number ->
                        [];
                    take_original_revision_number ->
                        {<<"originalRevisionNumber">>, [<<"a">>], ?ERROR_BAD_VALUE_INTEGER(<<"originalRevisionNumber">>)}
                end,
                {<<"atmLambdaRevision">>, #{<<"k">> => <<"v">>}, ?ERROR_BAD_DATA(<<"atmLambdaRevision">>)}
            ])
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)).


update_revision_lifecycle_state_test(Config) ->
    update_revision_lifecycle_state_test(Config, existent_revision),
    update_revision_lifecycle_state_test(Config, nonexistent_revision).

update_revision_lifecycle_state_test(Config, ScenarioType) ->
    Creator = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, AnotherMember, []),

    EnvSetUpFun = fun() ->
        AtmLambdaId = ozt_atm_lambdas:create(AtmInventoryId),
        InitialAtmLambdaRecord = ozt_atm_lambdas:get(AtmLambdaId),
        AllRevisionNumbers = atm_lambda_revision_registry:get_all_revision_numbers(
            InitialAtmLambdaRecord#od_atm_lambda.revision_registry
        ),
        RevisionToUpdate = case ScenarioType of
            existent_revision -> lists_utils:random_element(AllRevisionNumbers);
            nonexistent_revision -> lists_utils:random_element(lists:seq(1, 100) -- AllRevisionNumbers)
        end,
        #{
            atm_lambda_id => AtmLambdaId,
            initial_atm_lambda => InitialAtmLambdaRecord,
            revision_to_update => RevisionToUpdate,
            revision_to_update_binary => integer_to_binary(RevisionToUpdate)
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{
        atm_lambda_id := AtmLambdaId,
        initial_atm_lambda := InitialAtmLambdaRecord,
        revision_to_update := RevisionToUpdate
    }, Data) ->
        AtmLambdaRecord = ozt_atm_lambdas:get(AtmLambdaId),

        ExpLambdaRecord = case ShouldSucceed of
            false ->
                InitialAtmLambdaRecord;
            true ->
                case ScenarioType of
                    nonexistent_revision ->
                        InitialAtmLambdaRecord;
                    existent_revision ->
                        ExpState = automation:lifecycle_state_from_json(maps:get(<<"state">>, Data)),
                        InitialAtmLambdaRecord#od_atm_lambda{
                            revision_registry = #atm_lambda_revision_registry{
                                registry = maps:update_with(RevisionToUpdate, fun(AtmLambdaRevision) ->
                                    AtmLambdaRevision#atm_lambda_revision{state = ExpState}
                                end, InitialAtmLambdaRecord#od_atm_lambda.revision_registry#atm_lambda_revision_registry.registry)
                            }
                        }
                end
        end,

        ?assertEqual(ExpLambdaRecord, AtmLambdaRecord)
    end,

    AllowedStatesJson = lists:map(fun automation:lifecycle_state_to_json/1, automation:all_lifecycle_states()),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            % existence is checked before authorization - for a nonexistent resource,
            % unauthenticated and unauthorized clients should get ERROR_NOT_FOUND too
            correct = lists:flatten([
                root,
                {admin, [?OZ_ATM_INVENTORIES_UPDATE]},
                {user, Creator},
                case ScenarioType of
                    nonexistent_revision -> [
                        {user, AnotherMember},
                        {user, NonAdmin},
                        nobody
                    ];
                    existent_revision -> []
                end
            ]),
            unauthorized = lists:flatten(
                case ScenarioType of
                    nonexistent_revision -> [];
                    existent_revision -> [nobody]
                end
            ),
            forbidden = lists:flatten(
                case ScenarioType of
                    nonexistent_revision -> [];
                    existent_revision -> [
                        {user, AnotherMember},
                        {user, NonAdmin}
                    ]
                end
            )
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/atm_lambdas/">>, atm_lambda_id, <<"/revision/">>, revision_to_update_binary],
            expected_code = case ScenarioType of
                nonexistent_revision -> ?HTTP_404_NOT_FOUND;
                _ -> ?HTTP_204_NO_CONTENT
            end
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = update_revision_lifecycle_state,
            args = [auth, atm_lambda_id, revision_to_update_binary, data],
            expected_result = case ScenarioType of
                nonexistent_revision -> ?ERROR_REASON(?ERROR_NOT_FOUND);
                _ -> ?OK_RES
            end
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_atm_lambda, id = atm_lambda_id, aspect = {revision, revision_to_update_binary}},
            expected_result_op = case ScenarioType of
                nonexistent_revision -> ?ERROR_REASON(?ERROR_NOT_FOUND);
                _ -> ?OK_RES
            end
        },
        data_spec = #data_spec{
            required = [<<"state">>],
            correct_values = #{
                <<"state">> => AllowedStatesJson
            },
            bad_values = case ScenarioType of
                nonexistent_revision -> [];
                existent_revision -> [
                    {<<"state">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"state">>)},
                    {<<"state">>, str_utils:rand_hex(250), ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"state">>, AllowedStatesJson)}
                ]
            end
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


dump_revision_test(Config) ->
    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, AnotherMember, []),

    RevisionNumber = ?RAND_REV_NUMBER(),
    RevisionNumberBin = integer_to_binary(RevisionNumber),
    AtmLambdaRevision = atm_test_utils:example_lambda_revision(),
    AtmLambdaData = #{
        <<"name">> => atm_test_utils:example_name(),
        <<"summary">> => atm_test_utils:example_summary(),

        <<"revision">> => #{
            <<"originalRevisionNumber">> => RevisionNumber,
            <<"atmLambdaRevision">> => jsonable_record:to_json(AtmLambdaRevision, atm_lambda_revision)
        }
    },
    AtmLambdaId = ozt_atm_lambdas:create(?USER(Creator), AtmInventoryId, AtmLambdaData),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW]},
                {user, Creator},
                {user, AnotherMember}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = post,
            path = [<<"/atm_lambdas/">>, AtmLambdaId, <<"/revision/">>, RevisionNumberBin, <<"/dump">>],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:json_dump_of_atm_lambda_revision(rest, AtmLambdaRevision, RevisionNumber)
        },
        logic_spec = LogicSpec = #logic_spec{
            module = atm_lambda_logic,
            function = dump_revision,
            args = [auth, AtmLambdaId, RevisionNumber],
            expected_result = api_test_expect:json_dump_of_atm_lambda_revision(logic, AtmLambdaRevision, RevisionNumber)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = create,
            gri = #gri{
                type = od_atm_lambda, id = AtmLambdaId,
                aspect = {dump_revision, RevisionNumberBin}, scope = private
            },
            expected_result_op = api_test_expect:json_dump_of_atm_lambda_revision(gs, AtmLambdaRevision, RevisionNumber)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % test validation of revision number in the aspect
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            path = [<<"/atm_lambdas/">>, AtmLambdaId, <<"/revision/">>, <<"null">>, <<"/dump">>],
            expected_code = ?HTTP_400_BAD_REQUEST,
            expected_body = undefined
        },
        logic_spec = LogicSpec#logic_spec{
            args = [auth, AtmLambdaId, 0],
            expected_result = ?ERROR_REASON(?ERROR_BAD_DATA(<<"revisionNumber">>))
        },
        gs_spec = GsSpec#gs_spec{
            gri = #gri{
                type = od_atm_lambda, id = AtmLambdaId,
                aspect = {dump_revision, <<"asdf">>}, scope = private
            },
            expected_result_op = ?ERROR_REASON(?ERROR_BAD_DATA(<<"revisionNumber">>))
        }
    })).


link_to_inventory_test(Config) ->
    Creator = ozt_users:create(),
    MemberWithPrivs = ozt_users:create(),
    MemberWithNoPrivs = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    OriginalAtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    TargetAtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(TargetAtmInventoryId, MemberWithPrivs, [?ATM_INVENTORY_MANAGE_LAMBDAS]),
    ozt_atm_inventories:add_user(OriginalAtmInventoryId, MemberWithPrivs, [?ATM_INVENTORY_MANAGE_LAMBDAS]),
    ozt_atm_inventories:add_user(TargetAtmInventoryId, MemberWithNoPrivs, []),
    ozt_atm_inventories:add_user(OriginalAtmInventoryId, MemberWithNoPrivs, []),

    EnvSetUpFun = fun() ->
        #{atm_lambda_id => ozt_atm_lambdas:create(OriginalAtmInventoryId)}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{atm_lambda_id := SubjectAtmLambda}, _) ->
        ?assertEqual(ShouldSucceed, lists:member(TargetAtmInventoryId, ozt_atm_lambdas:get_atm_inventories(SubjectAtmLambda)))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_UPDATE]},
                {user, Creator},
                {user, MemberWithPrivs}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, MemberWithNoPrivs},
                {user, NonAdmin}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = put,
            path = [<<"/atm_lambdas/">>, atm_lambda_id, <<"/atm_inventories/">>, TargetAtmInventoryId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = LogicSpec = #logic_spec{
            module = atm_lambda_logic,
            function = link_to_inventory,
            args = [auth, atm_lambda_id, TargetAtmInventoryId],
            expected_result = ?OK
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % check that linking is not possible if the lambda already is a member of the inventory
    MemberAtmLambda = ozt_atm_lambdas:create(OriginalAtmInventoryId),
    LinkingErrorApiTestSpec = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            path = [<<"/atm_lambdas/">>, MemberAtmLambda, <<"/atm_inventories/">>, OriginalAtmInventoryId],
            expected_code = ?HTTP_409_CONFLICT
        },
        logic_spec = LogicSpec#logic_spec{
            args = [auth, MemberAtmLambda, OriginalAtmInventoryId],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(
                od_atm_lambda, MemberAtmLambda, od_atm_inventory, OriginalAtmInventoryId
            ))
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_utils:run_tests(
        Config, LinkingErrorApiTestSpec, EnvSetUpFun, undefined, undefined
    )).


unlink_from_inventory_test(Config) ->
    unlink_from_inventory_test_base(Config, last_inventory, not_used),
    unlink_from_inventory_test_base(Config, last_inventory, used),
    unlink_from_inventory_test_base(Config, not_last_inventory, not_used),
    unlink_from_inventory_test_base(Config, not_last_inventory, used).


unlink_from_inventory_test_base(Config, FromWhichInventory, UsageInWorkflowSchemas) ->
    Creator = ozt_users:create(),
    MemberWithPrivs = ozt_users:create(),
    MemberWithNoPrivs = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    OriginalAtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    TargetAtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(TargetAtmInventoryId, MemberWithPrivs, [?ATM_INVENTORY_MANAGE_LAMBDAS]),
    ozt_atm_inventories:add_user(OriginalAtmInventoryId, MemberWithPrivs, [?ATM_INVENTORY_MANAGE_LAMBDAS]),
    ozt_atm_inventories:add_user(TargetAtmInventoryId, MemberWithNoPrivs, []),
    ozt_atm_inventories:add_user(OriginalAtmInventoryId, MemberWithNoPrivs, []),

    EnvSetUpFun = fun() ->
        AtmLambdaId = case FromWhichInventory of
            last_inventory ->
                ozt_atm_lambdas:create(TargetAtmInventoryId);
            not_last_inventory ->
                Id = ozt_atm_lambdas:create(OriginalAtmInventoryId),
                ozt_atm_lambdas:link_to_inventory(Id, TargetAtmInventoryId),
                Id
        end,
        ReferencedAtmWorkflowSchemas = case UsageInWorkflowSchemas of
            not_used ->
                case FromWhichInventory of
                    last_inventory ->
                        ok;
                    not_last_inventory ->
                        % randomly use the lambda in workflow schemas from different,
                        % unrelated inventories, which should not prevent the unlinking
                        UnrelatedAtmInventories = lists:map(fun(_) ->
                            UnrelatedAtmInventoryId = ozt_atm_inventories:create(),
                            ozt_atm_lambdas:link_to_inventory(AtmLambdaId, UnrelatedAtmInventoryId),
                            UnrelatedAtmInventoryId
                        end, lists:seq(1, rand:uniform(8))),
                        lists:map(fun(_) ->
                            gen_atm_workflow_schema_with_lambda(lists_utils:random_element(UnrelatedAtmInventories), AtmLambdaId)
                        end, lists:seq(0, rand:uniform(8) - 1))
                end,
                [];
            used ->
                lists:map(fun(_) ->
                    gen_atm_workflow_schema_with_lambda(TargetAtmInventoryId, AtmLambdaId)
                end, lists:seq(1, rand:uniform(8)))
        end,
        #{atm_lambda_id => AtmLambdaId, referenced_atm_workflow_schemas => ReferencedAtmWorkflowSchemas}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{
        atm_lambda_id := SubjectAtmLambda,
        referenced_atm_workflow_schemas := ReferencedAtmWorkflowSchemas
    }, _) ->
        case {ShouldSucceed, UsageInWorkflowSchemas} of
            {true, not_used} ->
                ?assertNot(lists:member(SubjectAtmLambda, ozt_atm_inventories:get_atm_lambdas(TargetAtmInventoryId))),
                case FromWhichInventory of
                    not_last_inventory ->
                        ?assert(ozt_atm_lambdas:exists(SubjectAtmLambda)),
                        ?assertNot(lists:member(TargetAtmInventoryId, ozt_atm_lambdas:get_atm_inventories(SubjectAtmLambda)));
                    last_inventory ->
                        % if the lambda is unlinked from its last inventory, it should be deleted
                        ?assertNot(ozt_atm_lambdas:exists(SubjectAtmLambda))
                end;
            _ ->
                ?assert(lists:member(TargetAtmInventoryId, ozt_atm_lambdas:get_atm_inventories(SubjectAtmLambda))),
                ?assert(lists:member(SubjectAtmLambda, ozt_atm_inventories:get_atm_lambdas(TargetAtmInventoryId))),
                lists:foreach(fun(AtmWorkflowSchemaId) ->
                    ?assert(lists:member(AtmWorkflowSchemaId, ozt_atm_lambdas:get_atm_workflow_schemas(SubjectAtmLambda)))
                end, ReferencedAtmWorkflowSchemas)
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_UPDATE]},
                {user, Creator},
                {user, MemberWithPrivs}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, MemberWithNoPrivs},
                {user, NonAdmin}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = delete,
            path = [<<"/atm_lambdas/">>, atm_lambda_id, <<"/atm_inventories/">>, TargetAtmInventoryId],
            expected_code = case UsageInWorkflowSchemas of
                not_used -> ?HTTP_204_NO_CONTENT;
                used -> ?HTTP_403_FORBIDDEN
            end
        },
        logic_spec = LogicSpec = #logic_spec{
            module = atm_lambda_logic,
            function = unlink_from_inventory,
            args = [auth, atm_lambda_id, TargetAtmInventoryId],
            expected_result = ?OK_ENV(fun(#{referenced_atm_workflow_schemas := ReferencedAtmWorkflowSchemas}, _) ->
                case UsageInWorkflowSchemas of
                    not_used ->
                        ?OK_RES;
                    used ->
                        % referenced lambdas are sorted during usage checks
                        ?ERROR_REASON(?ERROR_ATM_LAMBDA_IN_USE(lists:sort(ReferencedAtmWorkflowSchemas)))
                end
            end)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % check that unlinking is not possible if the lambda is not a member of the inventory
    UnrelatedAtmLambda = ozt_atm_lambdas:create(OriginalAtmInventoryId),
    UnlinkingErrorApiTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_UPDATE]},
                {user, Creator},
                {user, MemberWithPrivs},
                {user, MemberWithNoPrivs},
                {user, NonAdmin},
                nobody
            ]
        },
        rest_spec = RestSpec#rest_spec{
            path = [<<"/atm_lambdas/">>, UnrelatedAtmLambda, <<"/atm_inventories/">>, TargetAtmInventoryId],
            expected_code = ?HTTP_404_NOT_FOUND
        },
        logic_spec = LogicSpec#logic_spec{
            args = [auth, UnrelatedAtmLambda, TargetAtmInventoryId],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_utils:run_tests(
        Config, UnlinkingErrorApiTestSpec, EnvSetUpFun, undefined, undefined
    )).


recreate_atm_lambda_test(_Config) ->
    Creator = ozt_users:create(),
    OriginalAtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    OriginalAtmLambdaId = ozt_atm_lambdas:create(OriginalAtmInventoryId),
    OriginalAtmLambda = ozt_atm_lambdas:get(OriginalAtmLambdaId),
    AllRevisionNumbers = atm_lambda_revision_registry:get_all_revision_numbers(
        OriginalAtmLambda#od_atm_lambda.revision_registry
    ),

    lists:foreach(fun(TargetAtmInventoryId) ->
        TestedAtmLambdaId = lists:foldl(fun(RevisionNumber, AccCurrentAtmLambdaId) ->
            DumpedOriginalAtmLambda = ozt_atm_lambdas:dump_to_json(OriginalAtmLambdaId, RevisionNumber),
            DuplicateAtmLambdaId = case AccCurrentAtmLambdaId of
                undefined ->
                    ozt_atm_lambdas:create(TargetAtmInventoryId, DumpedOriginalAtmLambda);
                _ ->
                    ozt_atm_lambdas:update(AccCurrentAtmLambdaId, DumpedOriginalAtmLambda),
                    AccCurrentAtmLambdaId
            end,
            ?assertNotEqual(DuplicateAtmLambdaId, OriginalAtmLambdaId),

            DuplicateAtmLambda = ozt_atm_lambdas:get(DuplicateAtmLambdaId),
            OriginalAtmLambda = ozt_atm_lambdas:get(OriginalAtmLambdaId),

            % check if the reference to the original lambda is set
            ?assertMatch(#od_atm_lambda{original_atm_lambda = OriginalAtmLambdaId}, DuplicateAtmLambda),

            lists:foreach(fun(Version) ->
                ?assert(are_lambda_dumps_equal(
                    ozt_atm_lambdas:dump_to_json(DuplicateAtmLambdaId, DuplicateAtmLambda, RevisionNumber, Version),
                    ozt_atm_lambdas:dump_to_json(OriginalAtmLambdaId, OriginalAtmLambda, RevisionNumber, Version)
                ))
            end, [2, 3]),

            ?assertEqual(
                atm_lambda_revision_registry:get_revision(RevisionNumber, DuplicateAtmLambda#od_atm_lambda.revision_registry),
                atm_lambda_revision_registry:get_revision(RevisionNumber, OriginalAtmLambda#od_atm_lambda.revision_registry)
            ),
            DuplicateAtmLambdaId
        end, undefined, lists_utils:shuffle(AllRevisionNumbers)),

        TestedAtmLambda = ozt_atm_lambdas:get(TestedAtmLambdaId),
        ?assertEqual(
            atm_lambda_revision_registry:get_all_revision_numbers(TestedAtmLambda#od_atm_lambda.revision_registry),
            atm_lambda_revision_registry:get_all_revision_numbers(OriginalAtmLambda#od_atm_lambda.revision_registry)
        )
    end, [OriginalAtmInventoryId, ozt_users:create_atm_inventory_for(Creator)]).

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
gen_atm_workflow_schema_with_lambda(AtmInventoryId, AtmLambdaId) ->
    % this procedure internally checks what lambdas are available in the inventory
    % and randomly reference them in task schemas, keep generating until the desired
    % lambda is referenced at least once
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(AtmInventoryId),
    AtmWorkflowSchema = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),
    case maps:is_key(AtmLambdaId, od_atm_workflow_schema:extract_all_atm_lambda_references(AtmWorkflowSchema)) of
        true ->
            AtmWorkflowSchemaId;
        false ->
            gen_atm_workflow_schema_with_lambda(AtmInventoryId, AtmLambdaId)
    end.


%% @private
are_lambda_dumps_equal(DumpA, DumpB) ->
    % the lambda id serves as an additional information and does not impact the
    % functional aspect of the lambda
    maps:without([<<"originalAtmLambdaId">>], DumpA) == maps:without([<<"originalAtmLambdaId">>], DumpB).

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

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time().
