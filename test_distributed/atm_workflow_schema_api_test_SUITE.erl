%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning atm_workflow_schema basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(atm_workflow_schema_api_test_SUITE).
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
    get_atm_lambdas_test/1,
    dump_test/1,
    update_test/1,
    delete_test/1,
    recreate_atm_workflow_schema_in_the_same_inventory_test/1,
    recreate_atm_workflow_schema_in_different_inventory_test/1,
    recreate_atm_workflow_schema_with_mixed_linked_and_duplicated_lambdas/1,
    bad_supplementary_lambdas_data_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        get_atm_lambdas_test,
        dump_test,
        update_test,
        delete_test,
        recreate_atm_workflow_schema_in_the_same_inventory_test,
        recreate_atm_workflow_schema_in_different_inventory_test,
        recreate_atm_workflow_schema_with_mixed_linked_and_duplicated_lambdas,
        bad_supplementary_lambdas_data_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    Creator = ozt_users:create(),

    % test with no supplementary lambdas
    create_test_base(Config, Creator, none),

    % test with supplementary lambdas that should be linked
    % (available in different inventory where the user has privileges to manage lambdas)
    AnotherAtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    AtmLambdasToBeLinked = maps:from_list(lists:map(fun(_) ->
        AtmLambdaId = ozt_atm_lambdas:create(AnotherAtmInventoryId),
        {AtmLambdaId, ozt_atm_lambdas:dump_schema_to_json(AtmLambdaId)}
    end, lists:seq(1, rand:uniform(7)))),
    create_test_base(Config, Creator, {to_link, AtmLambdasToBeLinked}),

    % test with supplementary lambdas that should be duplicated
    % (the user does not have access to them)
    UnrelatedAtmInventoryId = ozt_atm_inventories:create(),
    AtmLambdasToBeDuplicated = maps:from_list(lists:map(fun(_) ->
        AtmLambdaId = ozt_atm_lambdas:create(UnrelatedAtmInventoryId),
        {AtmLambdaId, ozt_atm_lambdas:dump_schema_to_json(AtmLambdaId)}
    end, lists:seq(1, rand:uniform(7)))),
    create_test_base(Config, Creator, {to_duplicate, AtmLambdasToBeDuplicated}).

create_test_base(Config, Creator, SupplementaryAtmLambdas) ->
    MemberWithNoPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    StoreSchemas = ozt_atm_workflow_schemas:gen_example_stores_json(),
    StoreSchemaIds = [StoreSchemaId || #{<<"id">> := StoreSchemaId} <- StoreSchemas],

    EnvSetUpFun = fun() ->
        AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
        ozt_atm_inventories:add_user(
            AtmInventoryId, MemberWithNoPriv, privileges:atm_inventory_admin() -- [?ATM_INVENTORY_MANAGE_WORKFLOW_SCHEMAS]
        ),

        InventoryAtmLambdas = lists:map(fun(_) ->
            ozt_atm_lambdas:create(AtmInventoryId)
        end, lists:seq(1, rand:uniform(7))),

        #{
            atm_inventory_id => AtmInventoryId,
            available_atm_lambdas => case SupplementaryAtmLambdas of
                none -> InventoryAtmLambdas;
                {_, LambdaDefinitions} -> maps:keys(LambdaDefinitions)
            end
        }
    end,

    VerifyFun = fun(AtmWorkflowSchemaId, #{atm_inventory_id := AtmInventoryId}, Data, CreatorType) ->
        AtmWorkflowSchemaRecord = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),

        ExpName = maps:get(<<"name">>, Data),
        ExpDescription = maps:get(<<"description">>, Data, <<"Missing description">>),

        ExpStores = jsonable_record:list_from_json(maps:get(<<"stores">>, Data, []), atm_store_schema),
        RequestedLanes = jsonable_record:list_from_json(maps:get(<<"lanes">>, Data, []), atm_lane_schema),

        ExpLanes = case SupplementaryAtmLambdas of
            none ->
                RequestedLanes;
            {to_link, _} ->
                RequestedLanes;
            {to_duplicate, AtmLambdaDefinitions} ->
                case CreatorType of
                    root_or_admin ->
                        % root and admin clients can always link the lambdas, so no duplicates should be created
                        RequestedLanes;
                    {regular, _} ->
                        ReferencedLambdas = ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(RequestedLanes),
                        ExpSubstitutedLambdas = lists_utils:intersect(ReferencedLambdas, maps:keys(AtmLambdaDefinitions)),
                        ozt_atm_workflow_schemas:substitute_atm_lambdas_for_duplicates_in_lanes(
                            RequestedLanes, ExpSubstitutedLambdas, AtmInventoryId
                        )
                end
        end,

        ExpState = automation:workflow_schema_state_from_json(maps:get(<<"state">>, Data, case {ExpStores, ExpLanes} of
            {[], _} -> <<"incomplete">>;
            {_, []} -> <<"incomplete">>;
            _ -> <<"ready">>
        end)),

        ExpAtmLambdas = ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(ExpLanes),

        ExpCreationTime = ozt_mocks:get_frozen_time_seconds(),
        ExpCreator = case CreatorType of
            {regular, UserId} -> ?SUB(user, UserId);
            root_or_admin -> AtmWorkflowSchemaRecord#od_atm_workflow_schema.creator
        end,

        ?assertEqual(#od_atm_workflow_schema{
            name = ExpName,
            description = ExpDescription,

            stores = ExpStores,
            lanes = ExpLanes,

            state = ExpState,

            atm_inventory = AtmInventoryId,
            atm_lambdas = lists:sort(ExpAtmLambdas),

            creation_time = ExpCreationTime,
            creator = ExpCreator
        }, AtmWorkflowSchemaRecord#od_atm_workflow_schema{
            atm_lambdas = lists:sort(AtmWorkflowSchemaRecord#od_atm_workflow_schema.atm_lambdas)
        }),

        true
    end,

    EnvTearDownFun = fun(#{atm_inventory_id := AtmInventoryId}) ->
        % clean up between tests as the lambdas that were linked / duplicated in the
        % tested inventory may change the behaviour (it belongs to the same user as in all tests)
        ozt_atm_inventories:delete(AtmInventoryId)
    end,

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
            module = atm_workflow_schema_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(Env, Data) ->
                ?OK_TERM(fun(Result) -> VerifyFun(Result, Env, Data, {regular, Creator}) end)
            end)
        },
        rest_spec = RestSpec = #rest_spec{
            method = post,
            path = <<"/atm_workflow_schemas">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(Env, Data) ->
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/atm_workflow_schemas/">>]),
                    [AtmWorkflowSchemaId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(AtmWorkflowSchemaId, Env, Data, {regular, Creator})
                end
            end)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = create,
            gri = #gri{type = od_atm_workflow_schema, aspect = instance},
            expected_result = ?OK_ENV(fun(Env, Data) ->
                ?OK_MAP_CONTAINS(#{
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
                        VerifyFun(Id, Env, Data, {regular, Creator})
                    end
                })
            end)
        },
        data_spec = DataSpec = #data_spec{
            required = lists:flatten([
                <<"atmInventoryId">>,
                <<"name">>,
                case SupplementaryAtmLambdas of
                    none -> [];
                    _ -> [<<"supplementaryAtmLambdas">>]
                end
            ]),
            optional = [
                <<"description">>,
                <<"stores">>,
                <<"state">>
            ],
            correct_values = #{
                <<"atmInventoryId">> => [fun(#{atm_inventory_id := AtmInventoryId}) -> AtmInventoryId end],
                <<"name">> => [ozt_atm:gen_example_name()],
                <<"description">> => [ozt_atm:gen_example_description()],
                <<"stores">> => [StoreSchemas],
                <<"lanes">> => [fun(#{available_atm_lambdas := AvailableAtmLambdas}) ->
                    ozt_atm_workflow_schemas:gen_example_lanes_json(AvailableAtmLambdas, StoreSchemaIds)
                end],
                <<"state">> => [ozt_atm_workflow_schemas:gen_example_state_json()],
                <<"supplementaryAtmLambdas">> => [case SupplementaryAtmLambdas of
                    none -> #{};
                    {to_link, AtmLambdaDefinitions} -> AtmLambdaDefinitions;
                    {to_duplicate, AtmLambdaDefinitions} -> AtmLambdaDefinitions
                end]
            },
            bad_values = lists:flatten([
                {<<"atmInventoryId">>, 1234, ?ERROR_FORBIDDEN},
                {<<"atmInventoryId">>, <<"">>, ?ERROR_FORBIDDEN},
                {<<"atmInventoryId">>, <<"asdq4ewfs">>, ?ERROR_FORBIDDEN},
                create_update_bad_data_values()
            ])
        }
    },
    % lanes can only be provided if stores are, as they reference specific store schema ids
    % first, test without lanes at all (see above spec), then mark stores as required
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, undefined)),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec#api_test_spec{data_spec = DataSpec#data_spec{
        required = DataSpec#data_spec.required ++ [
            <<"stores">>
        ],
        optional = [
            <<"description">>,
            <<"state">>,
            <<"lanes">>
        ]
    }}, EnvSetUpFun, EnvTearDownFun, undefined)),

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
            expected_result = ?OK_ENV(fun(Env, Data) ->
                ?OK_TERM(fun(Result) -> VerifyFun(Result, Env, Data, root_or_admin) end)
            end)
        },
        rest_spec = RestSpec#rest_spec{
            expected_headers = ?OK_ENV(fun(Env, Data) ->
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/atm_workflow_schemas/">>]),
                    [AtmWorkflowSchemaId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(AtmWorkflowSchemaId, Env, Data, root_or_admin)
                end
            end)
        },
        gs_spec = GsSpec#gs_spec{
            expected_result = ?OK_ENV(fun(Env, Data) ->
                ?OK_MAP_CONTAINS(#{
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
                        VerifyFun(Id, Env, Data, root_or_admin)
                    end
                })
            end)
        },
        data_spec = RootDataSpec = DataSpec#data_spec{
            bad_values = lists:flatten([
                {<<"atmInventoryId">>, <<"">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"atmInventoryId">>)},
                {<<"atmInventoryId">>, <<"asdq4ewfs">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"atmInventoryId">>)},
                create_update_bad_data_values()
            ])
        }
    },
    % lanes can only be provided if stores are, as they reference specific store schema ids
    % first, test without lanes at all (see above spec), then mark stores as required
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec, EnvSetUpFun, EnvTearDownFun, undefined)),
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec#api_test_spec{data_spec = RootDataSpec#data_spec{
        required = RootDataSpec#data_spec.required ++ [
            <<"stores">>
        ],
        optional = [
            <<"description">>,
            <<"state">>,
            <<"lanes">>
        ]
    }}, EnvSetUpFun, EnvTearDownFun, undefined)).


list_test(Config) ->
    % Make sure that atm_workflow_schemas created in other tests are deleted.
    ozt:delete_all_entities(),

    Creators = lists:map(fun(_) -> ozt_users:create() end, lists:seq(1, rand:uniform(8))),
    NonAdmin = ozt_users:create(),

    AtmInventories = lists:flatmap(fun(Creator) ->
        lists:map(fun(_) -> ozt_users:create_atm_inventory_for(Creator) end, lists:seq(1, rand:uniform(8)))
    end, Creators),

    AtmWorkflowSchemas = lists:flatmap(fun(AtmInventoryId) ->
        lists:map(fun(_) -> ozt_atm_workflow_schemas:create(AtmInventoryId) end, lists:seq(1, rand:uniform(8)))
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
            path = <<"/atm_workflow_schemas">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_workflow_schemas">> => AtmWorkflowSchemas}
        },
        logic_spec = #logic_spec{
            module = atm_workflow_schema_logic,
            function = list,
            args = [auth],
            expected_result = ?OK_LIST(AtmWorkflowSchemas)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also atm_workflow_schema_logic:exist function
    lists:foreach(fun(AtmWorkflowSchemaId) ->
        ?assert(ozt:rpc(atm_workflow_schema_logic, exists, [AtmWorkflowSchemaId]))
    end, AtmWorkflowSchemas),
    ?assert(not ozt:rpc(atm_workflow_schema_logic, exists, [<<"asdiucyaie827346w">>])).


get_test(Config) ->
    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, AnotherMember, []),

    AtmWorkflowSchemaData = ozt_atm_workflow_schemas:gen_example_data_json(AtmInventoryId),
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(?USER(Creator), AtmInventoryId, AtmWorkflowSchemaData),
    AtmWorkflowSchemaDataWithInventory = AtmWorkflowSchemaData#{
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
            path = [<<"/atm_workflow_schemas/">>, AtmWorkflowSchemaId],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:private_atm_workflow_schema(rest, AtmWorkflowSchemaId, AtmWorkflowSchemaDataWithInventory, ?SUB(user, Creator))
        },
        logic_spec = #logic_spec{
            module = atm_workflow_schema_logic,
            function = get,
            args = [auth, AtmWorkflowSchemaId],
            expected_result = api_test_expect:private_atm_workflow_schema(logic, AtmWorkflowSchemaId, AtmWorkflowSchemaDataWithInventory, ?SUB(user, Creator))
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_atm_workflow_schema, id = AtmWorkflowSchemaId,
                aspect = instance, scope = private
            },
            auth_hint = ?THROUGH_ATM_INVENTORY(AtmInventoryId),
            expected_result = api_test_expect:private_atm_workflow_schema(gs, AtmWorkflowSchemaId, AtmWorkflowSchemaDataWithInventory, ?SUB(user, Creator))
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_atm_lambdas_test(Config) ->
    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AnotherMember = ozt_users:create(),

    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, AnotherMember, []),
    lists:foreach(fun(_) ->
        ozt_atm_lambdas:create(AtmInventoryId)
    end, lists:seq(1, rand:uniform(7))),

    AtmWorkflowSchemaData = ozt_atm_workflow_schemas:gen_example_data_json(AtmInventoryId),
    ExpAtmLambdas = ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(maps:get(<<"lanes">>, AtmWorkflowSchemaData)),
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(AtmInventoryId, AtmWorkflowSchemaData),

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
            path = [<<"/atm_workflow_schemas/">>, AtmWorkflowSchemaId, <<"/atm_lambdas">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_lambdas">> => ExpAtmLambdas}
        },
        logic_spec = #logic_spec{
            module = atm_workflow_schema_logic,
            function = get_atm_lambdas,
            args = [auth, AtmWorkflowSchemaId],
            expected_result = ?OK_LIST(ExpAtmLambdas)
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

    AtmWorkflowSchemaData = ozt_atm_workflow_schemas:gen_example_data_json(AtmInventoryId),
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(?USER(Creator), AtmInventoryId, AtmWorkflowSchemaData),
    AtmWorkflowSchema = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),
    ReferencedAtmLambdas = ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(
        AtmWorkflowSchema#od_atm_workflow_schema.lanes
    ),

    ExpectedJsonDump = AtmWorkflowSchemaData#{
        <<"schemaFormatVersion">> => 1,
        <<"atmWorkflowSchemaId">> => AtmWorkflowSchemaId,
        <<"supplementaryAtmLambdas">> => lists:foldl(fun(AtmLambdaId, Acc) ->
            AtmLambda = ozt_atm_lambdas:get(AtmLambdaId),
            Acc#{AtmLambdaId => #{
                <<"schemaFormatVersion">> => 1,

                <<"name">> => AtmLambda#od_atm_lambda.name,
                <<"summary">> => AtmLambda#od_atm_lambda.summary,
                <<"description">> => AtmLambda#od_atm_lambda.description,

                <<"operationSpec">> => jsonable_record:to_json(AtmLambda#od_atm_lambda.operation_spec, atm_lambda_operation_spec),
                <<"argumentSpecs">> => jsonable_record:list_to_json(AtmLambda#od_atm_lambda.argument_specs, atm_lambda_argument_spec),
                <<"resultSpecs">> => jsonable_record:list_to_json(AtmLambda#od_atm_lambda.result_specs, atm_lambda_result_spec),

                <<"checksum">> => AtmLambda#od_atm_lambda.checksum
            }}
        end, #{}, ReferencedAtmLambdas)
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
            method = post,
            path = [<<"/atm_workflow_schemas/">>, AtmWorkflowSchemaId, <<"/dump">>],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpectedJsonDump
        },
        logic_spec = #logic_spec{
            module = atm_workflow_schema_logic,
            function = dump,
            args = [auth, AtmWorkflowSchemaId],
            expected_result = ?OK_MAP(ExpectedJsonDump)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{
                type = od_atm_workflow_schema, id = AtmWorkflowSchemaId,
                aspect = dump, scope = private
            },
            expected_result = ?OK_MAP(ExpectedJsonDump)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_test(Config) ->
    Creator = ozt_users:create(),

    % test with no supplementary lambdas
    update_test_base(Config, Creator, none),

    % test with supplementary lambdas that should be linked
    % (available in different inventory where the used has privileges to manage lambdas)
    AnotherAtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    AtmLambdasToBeLinked = maps:from_list(lists:map(fun(_) ->
        AtmLambdaId = ozt_atm_lambdas:create(AnotherAtmInventoryId),
        {AtmLambdaId, ozt_atm_lambdas:dump_schema_to_json(AtmLambdaId)}
    end, lists:seq(1, rand:uniform(7)))),
    update_test_base(Config, Creator, {to_link, AtmLambdasToBeLinked}),

    % test with supplementary lambdas that should be duplicated
    % (the user does not have access to them)
    UnrelatedAtmInventoryId = ozt_atm_inventories:create(),
    AtmLambdasToBeDuplicated = maps:from_list(lists:map(fun(_) ->
        AtmLambdaId = ozt_atm_lambdas:create(UnrelatedAtmInventoryId),
        {AtmLambdaId, ozt_atm_lambdas:dump_schema_to_json(AtmLambdaId)}
    end, lists:seq(1, rand:uniform(7)))),
    update_test_base(Config, Creator, {to_duplicate, AtmLambdasToBeDuplicated}).

update_test_base(Config, Creator, SupplementaryAtmLambdas) ->
    update_test_base(Config, Creator, SupplementaryAtmLambdas, {regular, Creator}),
    update_test_base(Config, Creator, SupplementaryAtmLambdas, root_or_admin).

update_test_base(Config, Creator, SupplementaryAtmLambdas, ClientType) ->
    MemberWithNoPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    EnvSetUpFun = fun() ->
        AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
        ozt_atm_inventories:add_user(
            AtmInventoryId, MemberWithNoPriv, privileges:atm_inventory_admin() -- [?ATM_INVENTORY_MANAGE_WORKFLOW_SCHEMAS]
        ),

        InventoryAtmLambdas = lists:map(fun(_) ->
            ozt_atm_lambdas:create(AtmInventoryId)
        end, lists:seq(1, rand:uniform(7))),

        InitialWorkflowSchemaData = ozt_atm_workflow_schemas:gen_example_data_json(AtmInventoryId),
        StoreSchemas = maps:get(<<"stores">>, InitialWorkflowSchemaData),
        StoreSchemaIds = [StoreSchemaId || #{<<"id">> := StoreSchemaId} <- StoreSchemas],

        #{
            atm_inventory_id => AtmInventoryId,
            available_atm_lambdas => case SupplementaryAtmLambdas of
                none -> InventoryAtmLambdas;
                {_, LambdaDefinitions} -> maps:keys(LambdaDefinitions)
            end,
            store_schemas => StoreSchemas,
            store_schema_ids => StoreSchemaIds,
            initial_workflow_schema_data => InitialWorkflowSchemaData,
            atm_workflow_schema_id => ozt_atm_workflow_schemas:create(AtmInventoryId, InitialWorkflowSchemaData)
        }
    end,

    VerifyEndFun = fun(ShouldSucceed, #{
        atm_inventory_id := AtmInventoryId,
        initial_workflow_schema_data := InitialWorkflowSchemaData,
        atm_workflow_schema_id := AtmWorkflowSchemaId
    }, Data) ->
        AtmWorkflowSchemaRecord = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),

        InitialName = maps:get(<<"name">>, InitialWorkflowSchemaData),
        ExpName = case ShouldSucceed of
            false -> InitialName;
            true -> maps:get(<<"name">>, Data, InitialName)
        end,

        InitialDescription = maps:get(<<"description">>, InitialWorkflowSchemaData, <<"Missing description">>),
        ExpDescription = case ShouldSucceed of
            false -> InitialDescription;
            true -> maps:get(<<"description">>, Data, InitialDescription)
        end,

        InitialStores = maps:get(<<"stores">>, InitialWorkflowSchemaData, []),
        ExpStores = case ShouldSucceed of
            false -> InitialStores;
            true -> maps:get(<<"stores">>, Data, InitialStores)
        end,

        InitialLanes = maps:get(<<"lanes">>, InitialWorkflowSchemaData, []),
        ExpLanes = case {ShouldSucceed, maps:find(<<"lanes">>, Data)} of
            {false, _} ->
                InitialLanes;
            {true, error} ->
                InitialLanes;
            {true, {ok, RequestedLanes}} ->
                case SupplementaryAtmLambdas of
                    none ->
                        RequestedLanes;
                    {to_link, _} ->
                        RequestedLanes;
                    {to_duplicate, AtmLambdaDefinitions} ->
                        case ClientType of
                            root_or_admin ->
                                % root and admin clients can always link the lambdas, so no duplicates should be created
                                RequestedLanes;
                            {regular, _} ->
                                DecodedRequestedLanes = jsonable_record:list_from_json(RequestedLanes, atm_lane_schema),
                                ReferencedLambdas = ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(DecodedRequestedLanes),
                                ExpSubstitutedLambdas = lists_utils:intersect(ReferencedLambdas, maps:keys(AtmLambdaDefinitions)),
                                DecodedExpLanes = ozt_atm_workflow_schemas:substitute_atm_lambdas_for_duplicates_in_lanes(
                                    DecodedRequestedLanes, ExpSubstitutedLambdas, AtmInventoryId
                                ),
                                jsonable_record:list_to_json(DecodedExpLanes, atm_lane_schema)
                        end
                end
        end,

        InitialState = maps:get(<<"state">>, InitialWorkflowSchemaData, case {InitialStores, InitialLanes} of
            {[], _} -> incomplete;
            {_, []} -> incomplete;
            _ -> ready
        end),
        ExpState = case ShouldSucceed of
            false -> InitialState;
            true -> maps:get(<<"state">>, Data, InitialState)
        end,

        ExpAtmLambdas = ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(ExpLanes),

        ?assertEqual(ExpName, AtmWorkflowSchemaRecord#od_atm_workflow_schema.name),
        ?assertEqual(ExpDescription, AtmWorkflowSchemaRecord#od_atm_workflow_schema.description),
        ?assertEqual(ExpStores, jsonable_record:list_to_json(AtmWorkflowSchemaRecord#od_atm_workflow_schema.stores, atm_store_schema)),
        ?assertEqual(ExpLanes, jsonable_record:list_to_json(AtmWorkflowSchemaRecord#od_atm_workflow_schema.lanes, atm_lane_schema)),
        ?assertEqual(ExpState, automation:workflow_schema_state_to_json(AtmWorkflowSchemaRecord#od_atm_workflow_schema.state)),
        ?assertEqual(lists:sort(ExpAtmLambdas), lists:sort(AtmWorkflowSchemaRecord#od_atm_workflow_schema.atm_lambdas))
    end,

    EnvTearDownFun = fun(#{atm_inventory_id := AtmInventoryId}) ->
        % clean up between tests as the lambdas that were linked / duplicated in the
        % tested inventory may change the behaviour (it belongs to the same user as in all tests)
        ozt_atm_inventories:delete(AtmInventoryId)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = case ClientType of
                {regular, UserId} -> [
                    {user, UserId}
                ];
                root_or_admin -> [
                    root,
                    {admin, [?OZ_ATM_INVENTORIES_UPDATE]}
                ]
            end,
            unauthorized = [nobody],
            forbidden = [
                {user, MemberWithNoPriv},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/atm_workflow_schemas/">>, atm_workflow_schema_id],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = atm_workflow_schema_logic,
            function = update,
            args = [auth, atm_workflow_schema_id, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_atm_workflow_schema, id = atm_workflow_schema_id, aspect = instance},
            expected_result = ?OK_RES
        },
        data_spec = DataSpec = #data_spec{
            at_least_one = [
                <<"name">>,
                <<"description">>,
                <<"stores">>,
                <<"state">>
            ],
            correct_values = #{
                <<"name">> => [ozt_atm:gen_example_name()],
                <<"description">> => [ozt_atm:gen_example_description()],
                <<"stores">> => [fun(#{store_schemas := StoreSchemas}) ->
                    StoreSchemas
                end],
                <<"lanes">> => [fun(#{available_atm_lambdas := AvailableAtmLambdas, store_schema_ids := StoreSchemaIds}) ->
                    ozt_atm_workflow_schemas:gen_example_lanes_json(AvailableAtmLambdas, StoreSchemaIds)
                end],
                <<"state">> => [ozt_atm_workflow_schemas:gen_example_state_json()],
                <<"supplementaryAtmLambdas">> => [case SupplementaryAtmLambdas of
                    none -> #{};
                    {to_link, AtmLambdaDefinitions} -> AtmLambdaDefinitions;
                    {to_duplicate, AtmLambdaDefinitions} -> AtmLambdaDefinitions
                end]
            },
            bad_values = create_update_bad_data_values()
        }
    },
    % lanes can only be provided if stores are, as they reference specific store schema ids
    % first, test without lanes at all (see above spec), then mark stores as required
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, VerifyEndFun)),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec#api_test_spec{
        data_spec = DataSpec#data_spec{
            required = lists:flatten([
                <<"stores">>,
                case SupplementaryAtmLambdas of
                    none -> [];
                    _ -> [<<"supplementaryAtmLambdas">>]
                end
            ]),
            at_least_one = [
                <<"name">>,
                <<"description">>,
                <<"state">>,
                <<"lanes">>
            ]
        }
    }, EnvSetUpFun, EnvTearDownFun, VerifyEndFun)).


delete_test(Config) ->
    UserWithDeletePrivs = ozt_users:create(),
    UserWithoutDeletePrivs = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AllPrivs = privileges:atm_inventory_privileges(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(UserWithoutDeletePrivs),
    ozt_atm_inventories:set_user_privileges(AtmInventoryId, UserWithoutDeletePrivs, AllPrivs -- [?ATM_INVENTORY_MANAGE_WORKFLOW_SCHEMAS]),
    ozt_atm_inventories:add_user(AtmInventoryId, UserWithDeletePrivs, [?ATM_INVENTORY_MANAGE_WORKFLOW_SCHEMAS]),

    EnvSetUpFun = fun() ->
        AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(AtmInventoryId),
        #od_atm_workflow_schema{atm_lambdas = AtmLambdas} = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),
        #{atm_workflow_schema_id => AtmWorkflowSchemaId, atm_lambdas => AtmLambdas}
    end,
    DeleteEntityFun = fun(#{atm_workflow_schema_id := AtmWorkflowSchemaId} = _Env) ->
        ozt_atm_workflow_schemas:delete(AtmWorkflowSchemaId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{atm_workflow_schema_id := AtmWorkflowSchemaId, atm_lambdas := AtmLambdas}, _Data) ->
        ?assertEqual(lists:member(AtmWorkflowSchemaId, ozt_atm_workflow_schemas:list()), not ShouldSucceed),
        % parent inventory should not be removed along with the atm_workflow_schema
        ?assert(ozt_atm_inventories:exists(AtmInventoryId)),
        % referenced atm_lambdas SHOULD NOT be removed if a atm_workflow_schema is
        [?assert(ozt_atm_lambdas:exists(AL)) || AL <- AtmLambdas]
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_UPDATE]},
                {user, UserWithDeletePrivs}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutDeletePrivs},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/atm_workflow_schemas/">>, atm_workflow_schema_id],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = atm_workflow_schema_logic,
            function = delete,
            args = [auth, atm_workflow_schema_id],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_atm_workflow_schema, id = atm_workflow_schema_id, aspect = instance},
            expected_result = ?OK_RES
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


recreate_atm_workflow_schema_in_the_same_inventory_test(_Config) ->
    Creator = ozt_users:create(),
    OriginalAtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    OriginalAtmWorkflowSchemaId = create_workflow_schema_with_nonempty_tasks(OriginalAtmInventoryId),
    OriginalAtmInventoryLambdas = ozt_atm_inventories:get_atm_lambdas(OriginalAtmInventoryId),
    OriginalReferencedLambdas = ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(
        (ozt_atm_workflow_schemas:get(OriginalAtmWorkflowSchemaId))#od_atm_workflow_schema.lanes
    ),
    DumpedAtmWorkflowSchema = ozt_atm_workflow_schemas:dump_schema_to_json(OriginalAtmWorkflowSchemaId),

    % recreating the schema from a dump in the same inventory should result in exactly the same schema
    DuplicateAtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(OriginalAtmInventoryId, DumpedAtmWorkflowSchema),
    ?assertNotEqual(OriginalAtmWorkflowSchemaId, DuplicateAtmWorkflowSchemaId),
    ?assert(are_workflow_schema_dumps_equal(
        DumpedAtmWorkflowSchema,
        ozt_atm_workflow_schemas:dump_schema_to_json(DuplicateAtmWorkflowSchemaId)
    )),
    ?assert(inventory_references_lambdas(OriginalAtmInventoryId, OriginalAtmInventoryLambdas)),
    ?assert(workflow_schema_references_lambdas(DuplicateAtmWorkflowSchemaId, OriginalReferencedLambdas)).


recreate_atm_workflow_schema_in_different_inventory_test(_Config) ->
    % recreating the schema from a dump in different inventory should result in...
    UserAlpha = ozt_users:create(),
    OriginalAtmInventoryId = ozt_users:create_atm_inventory_for(UserAlpha),
    OriginalAtmWorkflowSchemaId = create_workflow_schema_with_nonempty_tasks(OriginalAtmInventoryId),
    OriginalAtmInventoryLambdas = ozt_atm_inventories:get_atm_lambdas(OriginalAtmInventoryId),
    OriginalAtmWorkflowSchema = ozt_atm_workflow_schemas:get(OriginalAtmWorkflowSchemaId),
    OriginalReferencedLambdas = ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(
        OriginalAtmWorkflowSchema#od_atm_workflow_schema.lanes
    ),
    DumpedAtmWorkflowSchema = ozt_atm_workflow_schemas:dump_schema_to_json(OriginalAtmWorkflowSchemaId),

    % 1) exactly the same schema if the creating user has privileges to manage
    %    used lambdas in both inventories (lambdas are linked to the target inventory)
    FirstAtmInventoryId = ozt_users:create_atm_inventory_for(UserAlpha),
    FirstAtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(
        ?USER(UserAlpha), FirstAtmInventoryId, DumpedAtmWorkflowSchema
    ),
    ?assertNotEqual(OriginalAtmWorkflowSchemaId, FirstAtmWorkflowSchemaId),
    ?assert(are_workflow_schema_dumps_equal(
        DumpedAtmWorkflowSchema,
        ozt_atm_workflow_schemas:dump_schema_to_json(FirstAtmWorkflowSchemaId)
    )),
    ?assert(inventory_references_lambdas(FirstAtmInventoryId, OriginalReferencedLambdas)),
    ?assert(workflow_schema_references_lambdas(FirstAtmWorkflowSchemaId, OriginalReferencedLambdas)),

    % 2) a schema referencing duplicated lambdas if the creating user does not
    %    have privileges to manage lambdas in the original inventory, but does in
    %    the target inventory
    UserBeta = ozt_users:create(),
    AnotherAtmInventoryId = ozt_users:create_atm_inventory_for(UserBeta),
    SecondAtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(
        ?USER(UserBeta), AnotherAtmInventoryId, DumpedAtmWorkflowSchema
    ),
    ?assertNotEqual(OriginalAtmWorkflowSchemaId, SecondAtmWorkflowSchemaId),
    ?assertNot(are_workflow_schema_dumps_equal(
        DumpedAtmWorkflowSchema,
        ozt_atm_workflow_schemas:dump_schema_to_json(SecondAtmWorkflowSchemaId)
    )),
    SecondAtmWorkflowSchema = ozt_atm_workflow_schemas:get(SecondAtmWorkflowSchemaId),
    ?assert(are_workflow_schema_dumps_equal(
        ozt_atm_workflow_schemas:dump_schema_to_json(SecondAtmWorkflowSchemaId),
        ozt_atm_workflow_schemas:dump_schema_to_json(
            SecondAtmWorkflowSchemaId,
            SecondAtmWorkflowSchema#od_atm_workflow_schema{
                lanes = ozt_atm_workflow_schemas:substitute_atm_lambdas_for_duplicates_in_lanes(
                    OriginalAtmWorkflowSchema#od_atm_workflow_schema.lanes,
                    AnotherAtmInventoryId
                )
            }
        )
    )),
    SecondReferencedLambdas = maps:values(ozt_atm_lambdas:find_all_duplicates(
        OriginalReferencedLambdas, AnotherAtmInventoryId
    )),
    ?assert(inventory_references_lambdas(AnotherAtmInventoryId, SecondReferencedLambdas)),
    ?assert(workflow_schema_references_lambdas(SecondAtmWorkflowSchemaId, SecondReferencedLambdas)),

    % 3) a schema referencing linked lambdas if the creating user does not
    %    have privileges to manage lambdas in any inventory, but the lambdas are already linked
    UserGamma = ozt_users:create(),
    ozt_atm_inventories:add_user(OriginalAtmInventoryId, UserGamma, [?ATM_INVENTORY_MANAGE_WORKFLOW_SCHEMAS]),
    ThirdAtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(
        ?USER(UserGamma), OriginalAtmInventoryId, DumpedAtmWorkflowSchema
    ),
    ?assertNotEqual(OriginalAtmWorkflowSchemaId, ThirdAtmWorkflowSchemaId),
    ?assert(are_workflow_schema_dumps_equal(
        DumpedAtmWorkflowSchema,
        ozt_atm_workflow_schemas:dump_schema_to_json(ThirdAtmWorkflowSchemaId)
    )),
    ?assert(inventory_references_lambdas(OriginalAtmInventoryId, OriginalAtmInventoryLambdas)),
    ?assert(workflow_schema_references_lambdas(ThirdAtmWorkflowSchemaId, OriginalReferencedLambdas)),

    % 4) a schema referencing duplicated lambdas if the creating user does not
    %    have privileges to manage lambdas in any inventory, but the duplicates are already created
    UserDelta = ozt_users:create(),
    ozt_atm_inventories:add_user(AnotherAtmInventoryId, UserDelta, [?ATM_INVENTORY_MANAGE_WORKFLOW_SCHEMAS]),
    FourthAtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(
        ?USER(UserDelta), AnotherAtmInventoryId, DumpedAtmWorkflowSchema
    ),
    ?assertNotEqual(OriginalAtmWorkflowSchemaId, FourthAtmWorkflowSchemaId),
    ?assertNot(are_workflow_schema_dumps_equal(
        DumpedAtmWorkflowSchema,
        ozt_atm_workflow_schemas:dump_schema_to_json(FourthAtmWorkflowSchemaId)
    )),
    ?assert(are_workflow_schema_dumps_equal(
        ozt_atm_workflow_schemas:dump_schema_to_json(SecondAtmWorkflowSchemaId),
        ozt_atm_workflow_schemas:dump_schema_to_json(FourthAtmWorkflowSchemaId)
    )),
    ?assert(inventory_references_lambdas(AnotherAtmInventoryId, SecondReferencedLambdas)),
    ?assert(workflow_schema_references_lambdas(FourthAtmWorkflowSchemaId, SecondReferencedLambdas)),

    % 5) a schema referencing duplicated lambdas that were linked to the target inventory
    %    if the creating user had privileges to manage the duplicates, but not the original lambdas
    UserTheta = ozt_users:create(),
    DifferentAtmInventoryId = ozt_users:create_atm_inventory_for(UserTheta),
    ozt_atm_inventories:add_user(AnotherAtmInventoryId, UserTheta, [?ATM_INVENTORY_MANAGE_LAMBDAS]),
    FifthAtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(
        ?USER(UserTheta), DifferentAtmInventoryId, DumpedAtmWorkflowSchema
    ),
    ?assertNotEqual(OriginalAtmWorkflowSchemaId, FifthAtmWorkflowSchemaId),
    ?assertNot(are_workflow_schema_dumps_equal(
        DumpedAtmWorkflowSchema,
        ozt_atm_workflow_schemas:dump_schema_to_json(FifthAtmWorkflowSchemaId)
    )),
    ?assert(are_workflow_schema_dumps_equal(
        ozt_atm_workflow_schemas:dump_schema_to_json(SecondAtmWorkflowSchemaId),
        ozt_atm_workflow_schemas:dump_schema_to_json(FifthAtmWorkflowSchemaId)
    )),
    ?assert(inventory_references_lambdas(DifferentAtmInventoryId, SecondReferencedLambdas)),
    ?assert(workflow_schema_references_lambdas(FifthAtmWorkflowSchemaId, SecondReferencedLambdas)),

    % 6) a forbidden error if the creating user does not have privileges to manage lambdas
    %    in the target inventory, which does not have linked / duplicated lambdas,
    %    despite privileges to manage lambdas in the original inventory
    UserOmega = ozt_users:create(),
    LastAtmInventoryId = ozt_users:create_atm_inventory_for(UserOmega),
    ozt_atm_inventories:set_user_privileges(LastAtmInventoryId, UserOmega, [?ATM_INVENTORY_MANAGE_WORKFLOW_SCHEMAS]),
    ?assertEqual(
        ?ERROR_FORBIDDEN,
        ozt_atm_workflow_schemas:try_create(?USER(UserOmega), LastAtmInventoryId, DumpedAtmWorkflowSchema)
    ).


recreate_atm_workflow_schema_with_mixed_linked_and_duplicated_lambdas(_Config) ->
    OriginalAtmInventoryId = ozt_atm_inventories:create(),
    AtmLambdaToLink = ozt_atm_lambdas:create(OriginalAtmInventoryId),
    AtmLambdaToDuplicate = ozt_atm_lambdas:create(OriginalAtmInventoryId),
    OriginalAtmWorkflowSchemaId = create_workflow_schema_with_tasks_including_lambdas(
        OriginalAtmInventoryId, [AtmLambdaToLink, AtmLambdaToDuplicate]
    ),
    DumpedAtmWorkflowSchema = ozt_atm_workflow_schemas:dump_schema_to_json(OriginalAtmWorkflowSchemaId),

    Creator = ozt_users:create(),
    TargetInventoryId = ozt_users:create_atm_inventory_for(Creator),
    OtherInventoryId = ozt_users:create_atm_inventory_for(Creator),

    % add the lambda to the other user's inventory so that the user has privileges to link it
    ozt_atm_lambdas:add_to_inventory(AtmLambdaToLink, OtherInventoryId),

    % recreate the workflow schema in the target inventory, which should cause one of
    % the lambdas to be linked, and one to be duplicated
    RecreatedAtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(
        ?USER(Creator), TargetInventoryId, DumpedAtmWorkflowSchema
    ),
    ?assertNotEqual(OriginalAtmWorkflowSchemaId, RecreatedAtmWorkflowSchemaId),
    ?assertNot(are_workflow_schema_dumps_equal(
        DumpedAtmWorkflowSchema,
        ozt_atm_workflow_schemas:dump_schema_to_json(RecreatedAtmWorkflowSchemaId)
    )),
    RecreatedAtmWorkflowSchema = ozt_atm_workflow_schemas:get(RecreatedAtmWorkflowSchemaId),
    ?assert(are_workflow_schema_dumps_equal(
        ozt_atm_workflow_schemas:dump_schema_to_json(RecreatedAtmWorkflowSchemaId),
        ozt_atm_workflow_schemas:dump_schema_to_json(
            RecreatedAtmWorkflowSchemaId,
            RecreatedAtmWorkflowSchema#od_atm_workflow_schema{
                lanes = ozt_atm_workflow_schemas:substitute_atm_lambdas_for_duplicates_in_lanes(
                    RecreatedAtmWorkflowSchema#od_atm_workflow_schema.lanes,
                    [AtmLambdaToDuplicate],
                    TargetInventoryId
                )
            }
        )
    )),
    ExpReferencedLambdas = [AtmLambdaToLink, ozt_atm_lambdas:find_duplicate(AtmLambdaToDuplicate, TargetInventoryId)],
    ?assert(inventory_references_lambdas(TargetInventoryId, ExpReferencedLambdas)),
    ?assert(workflow_schema_references_lambdas(RecreatedAtmWorkflowSchemaId, ExpReferencedLambdas)).


bad_supplementary_lambdas_data_test(_Config) ->
    Creator = ozt_users:create(),
    OriginalAtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    TheOnlyAtmLambdaId = ozt_atm_lambdas:create(OriginalAtmInventoryId),
    OriginalAtmWorkflowSchemaId = create_workflow_schema_with_nonempty_tasks(OriginalAtmInventoryId),
    DumpedAtmWorkflowSchema = ozt_atm_workflow_schemas:dump_schema_to_json(OriginalAtmWorkflowSchemaId),

    UserBeta = ozt_users:create(),
    AnotherAtmInventoryId = ozt_users:create_atm_inventory_for(UserBeta),

    % failing to provide the supplementary lambdas should cause bad lambda reference errors
    ExpBadLambdaReferenceError = ?ERROR_BAD_DATA(
        <<"tasks">>,
        <<"The lambda id '", TheOnlyAtmLambdaId/binary, "' referenced by one of the tasks was not found or is "
        "not available for the requesting client. Consider providing supplementary "
        "lambdas so that missing ones can be linked or created along with the workflow schema.">>
    ),
    ?assertEqual(ExpBadLambdaReferenceError, ozt_atm_workflow_schemas:try_create(
        ?USER(UserBeta),
        AnotherAtmInventoryId,
        maps:without([<<"supplementaryAtmLambdas">>], DumpedAtmWorkflowSchema)
    )),

    % providing invalid lambda definitions should cause data validation errors from lambda creation procedures
    ?assertMatch(
        ?ERROR_BAD_DATA(<<"supplementaryAtmLambdas[", _/binary>>, ?ERROR_MISSING_REQUIRED_VALUE(_)),
        ozt_atm_workflow_schemas:try_create(
            ?USER(UserBeta),
            AnotherAtmInventoryId,
            maps:update_with(<<"supplementaryAtmLambdas">>, fun(SupplementaryAtmLambdas) ->
                maps:map(fun(_AtmLambdaId, AtmLambdaData) ->
                    % include ONLY the checksum field and drop the actual lambda data
                    maps:with([<<"checksum">>], AtmLambdaData)
                end, SupplementaryAtmLambdas)
            end, DumpedAtmWorkflowSchema)
        )
    ).

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
create_update_bad_data_values() ->
    lists:flatten([
        {<<"description">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"description">>)},
        {<<"description">>, str_utils:rand_hex(50001), ?ERROR_BAD_VALUE_BINARY_TOO_LARGE(<<"description">>, 100000)},
        {<<"stores">>, bad_spec, ?ERROR_BAD_DATA(<<"stores">>)},
        {<<"stores">>, [1234], ?ERROR_BAD_DATA(<<"stores">>)},
        {<<"lanes">>, #{<<"bad">> => <<"object">>}, ?ERROR_BAD_DATA(<<"lanes">>)},
        {<<"lanes">>, [<<"text">>], ?ERROR_BAD_DATA(<<"lanes">>)},
        {<<"state">>, 78.4, ?ERROR_BAD_VALUE_ATOM(<<"state">>)},
        {<<"state">>, bad, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"state">>, automation:all_workflow_schema_states())},
        {<<"supplementaryAtmLambdas">>, <<"text">>, ?ERROR_BAD_VALUE_JSON(<<"supplementaryAtmLambdas">>)},
        ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
    ]).


%% @private
create_workflow_schema_with_nonempty_tasks(AtmInventoryId) ->
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(AtmInventoryId),
    AtmWorkflowSchema = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),
    case ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(AtmWorkflowSchema#od_atm_workflow_schema.lanes) of
        [] ->
            % workflow schema is randomized and may have empty tasks; repeat if needed
            create_workflow_schema_with_nonempty_tasks(AtmInventoryId);
        _ ->
            AtmWorkflowSchemaId
    end.


%% @private
create_workflow_schema_with_tasks_including_lambdas(AtmInventoryId, TargetAtmLambdas) ->
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(AtmInventoryId),
    AtmWorkflowSchema = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),
    ReferencedAtmLambdas = ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(
        AtmWorkflowSchema#od_atm_workflow_schema.lanes
    ),
    case lists:sort(TargetAtmLambdas) == lists:sort(ReferencedAtmLambdas) of
        true ->
            AtmWorkflowSchemaId;
        false ->
            % workflow schema is randomized and may not include all the lambdas; repeat if needed
            create_workflow_schema_with_tasks_including_lambdas(AtmInventoryId, TargetAtmLambdas)
    end.


%% @private
are_workflow_schema_dumps_equal(DumpA, DumpB) ->
    % the workflow schema id serves as an additional information and does not impact the
    % functional aspect of the workflow schema
    maps:without([<<"atmWorkflowSchemaId">>], DumpA) == maps:without([<<"atmWorkflowSchemaId">>], DumpB).


inventory_references_lambdas(AtmInventoryId, AtmLambdas) ->
    InventoryAtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
    lists:sort(InventoryAtmLambdas) == lists:sort(AtmLambdas).


workflow_schema_references_lambdas(AtmWorkflowSchemaId, AtmLambdas) ->
    WorkflowSchemaAtmLambdas = ozt_atm_workflow_schemas:get_atm_lambdas(AtmWorkflowSchemaId),
    lists:sort(WorkflowSchemaAtmLambdas) == lists:sort(AtmLambdas).

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
