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
    update_test/1,
    delete_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        get_atm_lambdas_test,
        update_test,
        delete_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    Creator = ozt_users:create(),
    MemberWithNoPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, MemberWithNoPriv, privileges:atm_inventory_admin() -- [?ATM_INVENTORY_MANAGE_WORKFLOW_SCHEMAS]),

    AvailableAtmLambdas = lists:map(fun(_) ->
        ozt_atm_lambdas:create(AtmInventoryId)
    end, lists:seq(1, rand:uniform(7))),

    StoreSchemas = ozt_atm_workflow_schemas:gen_example_stores_json(),
    StoreSchemaIds = [StoreSchemaId || #{<<"id">> := StoreSchemaId} <- StoreSchemas],

    VerifyFun = fun(AtmWorkflowSchemaId, Data, CheckCreator) ->
        AtmWorkflowSchemaRecord = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),

        ExpName = maps:get(<<"name">>, Data),
        ExpDescription = maps:get(<<"description">>, Data, <<"Missing description">>),

        ExpStores = jsonable_record:list_from_json(maps:get(<<"stores">>, Data, []), atm_store_schema),
        ExpLanes = jsonable_record:list_from_json(maps:get(<<"lanes">>, Data, []), atm_lane_schema),

        ExpState = automation:workflow_schema_state_from_json(maps:get(<<"state">>, Data, case {ExpStores, ExpLanes} of
            {[], _} -> <<"incomplete">>;
            {_, []} -> <<"incomplete">>;
            _ -> <<"ready">>
        end)),

        ExpAtmLambdas = ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(maps:get(<<"lanes">>, Data, [])),

        ExpCreationTime = ozt_mocks:get_frozen_time_seconds(),
        ExpCreator = case CheckCreator of
            {true, UserId} -> ?SUB(user, UserId);
            false -> AtmWorkflowSchemaRecord#od_atm_workflow_schema.creator
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
            expected_result = ?OK_ENV(fun(_Env, Data) ->
                ?OK_TERM(fun(Result) -> VerifyFun(Result, Data, {true, Creator}) end)
            end)
        },
        rest_spec = RestSpec = #rest_spec{
            method = post,
            path = <<"/atm_workflow_schemas">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, Data) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/atm_workflow_schemas/">>]),
                    [AtmWorkflowSchemaId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(AtmWorkflowSchemaId, Data, {true, Creator})
                end
            end)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = create,
            gri = #gri{type = od_atm_workflow_schema, aspect = instance},
            expected_result = ?OK_ENV(fun(_, Data) ->
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
                <<"name">>
            ],
            optional = [
                <<"description">>,
                <<"stores">>,
                <<"lanes">>,
                <<"state">>
            ],
            correct_values = #{
                <<"atmInventoryId">> => [AtmInventoryId],
                <<"name">> => [ozt_atm:gen_example_name()],
                <<"description">> => [ozt_atm:gen_example_description()],
                <<"stores">> => [StoreSchemas],
                <<"lanes">> => [ozt_atm_workflow_schemas:gen_example_lanes_json(AvailableAtmLambdas, StoreSchemaIds)],
                <<"state">> => [ozt_atm_workflow_schemas:gen_example_state_json()]
            },
            bad_values = lists:flatten([
                {<<"atmInventoryId">>, 1234, ?ERROR_FORBIDDEN},
                {<<"atmInventoryId">>, <<"">>, ?ERROR_FORBIDDEN},
                {<<"atmInventoryId">>, <<"asdq4ewfs">>, ?ERROR_FORBIDDEN},
                create_update_bad_data_values(AtmInventoryId)
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
                fun(#{<<"Location">> := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/atm_workflow_schemas/">>]),
                    [AtmWorkflowSchemaId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(AtmWorkflowSchemaId, Data, false)
                end
            end)
        },
        gs_spec = GsSpec#gs_spec{
            expected_result = ?OK_ENV(fun(_, Data) ->
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
                create_update_bad_data_values(AtmInventoryId)
            ])
        }
    },
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec)).


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


update_test(Config) ->
    Creator = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, AnotherMember, []),

    InitialWorkflowSchemaData = ozt_atm_workflow_schemas:gen_example_data_json(AtmInventoryId),
    AvailableAtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
    StoreSchemas = maps:get(<<"stores">>, InitialWorkflowSchemaData),
    StoreSchemaIds = [StoreSchemaId || #{<<"id">> := StoreSchemaId} <- StoreSchemas],

    EnvSetUpFun = fun() ->
        #{atm_workflow_schema_id => ozt_atm_workflow_schemas:create(AtmInventoryId, InitialWorkflowSchemaData)}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{atm_workflow_schema_id := AtmWorkflowSchemaId}, Data) ->
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
        ExpLanes = case ShouldSucceed of
            false -> InitialLanes;
            true -> maps:get(<<"lanes">>, Data, InitialLanes)
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

        ExpAtmLambdas = ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(case ShouldSucceed of
            false -> InitialLanes;
            true -> maps:get(<<"lanes">>, Data, InitialLanes)
        end),

        ?assertEqual(ExpName, AtmWorkflowSchemaRecord#od_atm_workflow_schema.name),
        ?assertEqual(ExpDescription, AtmWorkflowSchemaRecord#od_atm_workflow_schema.description),
        ?assertEqual(ExpStores, jsonable_record:list_to_json(AtmWorkflowSchemaRecord#od_atm_workflow_schema.stores, atm_store_schema)),
        ?assertEqual(ExpLanes, jsonable_record:list_to_json(AtmWorkflowSchemaRecord#od_atm_workflow_schema.lanes, atm_lane_schema)),
        ?assertEqual(ExpState, automation:workflow_schema_state_to_json(AtmWorkflowSchemaRecord#od_atm_workflow_schema.state)),
        ?assertEqual(lists:sort(ExpAtmLambdas), lists:sort(AtmWorkflowSchemaRecord#od_atm_workflow_schema.atm_lambdas))
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
        data_spec = #data_spec{
            at_least_one = [
                <<"name">>,
                <<"description">>,
                <<"stores">>,
                <<"lanes">>,
                <<"state">>
            ],
            correct_values = #{
                <<"name">> => [ozt_atm:gen_example_name()],
                <<"description">> => [ozt_atm:gen_example_description()],
                <<"stores">> => [StoreSchemas],
                <<"lanes">> => [ozt_atm_workflow_schemas:gen_example_lanes_json(AvailableAtmLambdas, StoreSchemaIds)],
                <<"state">> => [ozt_atm_workflow_schemas:gen_example_state_json()]
            },
            bad_values = create_update_bad_data_values(AtmInventoryId)
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


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

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
create_update_bad_data_values(AtmInventoryId) ->
    {DisallowedLanes, DisallowedLambda} = gen_lanes_containing_disallowed_lambda(),
    lists:flatten([
        {<<"description">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"description">>)},
        {<<"description">>, str_utils:rand_hex(50001), ?ERROR_BAD_VALUE_BINARY_TOO_LARGE(<<"description">>, 100000)},
        {<<"stores">>, bad_spec, ?ERROR_BAD_DATA(<<"stores">>)},
        {<<"stores">>, [1234], ?ERROR_BAD_DATA(<<"stores">>)},
        {<<"lanes">>, #{<<"bad">> => <<"object">>}, ?ERROR_BAD_DATA(<<"lanes">>)},
        {<<"lanes">>, [<<"text">>], ?ERROR_BAD_DATA(<<"lanes">>)},
        {<<"lanes">>, DisallowedLanes, ?ERROR_RELATION_DOES_NOT_EXIST(od_atm_lambda, DisallowedLambda, od_atm_inventory, AtmInventoryId)},
        {<<"state">>, 78.4, ?ERROR_BAD_VALUE_ATOM(<<"state">>)},
        {<<"state">>, bad, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"state">>, automation:all_workflow_schema_states())},
        ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
    ]).


%% @private
gen_lanes_containing_disallowed_lambda() ->
    UnrelatedInventory = ozt_atm_inventories:create(),
    ExampleStores = ozt_atm_workflow_schemas:gen_example_stores_json(),
    ExampleStoreSchemaIds = [StoreSchemaId || #{<<"id">> := StoreSchemaId} <- ExampleStores],
    DisallowedLambda = ozt_atm_lambdas:create(UnrelatedInventory),
    DisallowedLanes = ozt_atm_workflow_schemas:gen_example_lanes_json([DisallowedLambda], ExampleStoreSchemaIds),
    % the generated lanes may randomly be empty - keep generating until some task containing the lambda appear
    case ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(DisallowedLanes) of
        [DisallowedLambda] ->
            {DisallowedLanes, DisallowedLambda};
        _ ->
            gen_lanes_containing_disallowed_lambda()
    end.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    ozt:init_per_suite(Config).

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time().
