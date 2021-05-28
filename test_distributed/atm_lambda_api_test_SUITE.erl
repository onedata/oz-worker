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
    update_test/1,
    add_to_inventory_test/1
    % @TODO VFS-7596 delete test, with checks if inventories or schemas are properly deleted (or not) along with the lambda
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        get_atm_inventories_test,
        get_atm_workflow_schemas_test,
        update_test,
        add_to_inventory_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    Creator = ozt_users:create(),
    MemberWithNoPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AtmInventory = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventory, MemberWithNoPriv, privileges:atm_inventory_admin() -- [?ATM_INVENTORY_MANAGE_LAMBDAS]),

    VerifyFun = fun(AtmLambda, Data, CheckCreator) ->
        AtmLambdaRecord = ozt_atm_lambdas:get(AtmLambda),

        ExpName = maps:get(<<"name">>, Data),
        ExpSummary = maps:get(<<"summary">>, Data, <<"Missing summary">>),
        ExpDescription = maps:get(<<"description">>, Data, <<"Missing description">>),
        ExpOperationSpec = jsonable_record:from_json(maps:get(<<"operationSpec">>, Data), atm_lambda_operation_spec),
        ExpArgumentSpecs = jsonable_record:list_from_json(maps:get(<<"argumentSpecs">>, Data, []), atm_lambda_argument_spec),
        ExpResultSpecs = jsonable_record:list_from_json(maps:get(<<"resultSpecs">>, Data, []), atm_lambda_result_spec),
        ExpAtmInventories = [AtmInventory],
        ExpCreationTime = ozt_mocks:get_frozen_time_seconds(),
        ExpCreator = case CheckCreator of
            {true, UserId} -> ?SUB(user, UserId);
            false -> AtmLambdaRecord#od_atm_lambda.creator
        end,

        ?assertMatch(#od_atm_lambda{
            name = ExpName,
            summary = ExpSummary,
            description = ExpDescription,

            operation_spec = ExpOperationSpec,
            argument_specs = ExpArgumentSpecs,
            result_specs = ExpResultSpecs,

            atm_inventories = ExpAtmInventories,

            creation_time = ExpCreationTime,
            creator = ExpCreator
        }, AtmLambdaRecord),
        true
    end,

    %% Users are allowed to create custom lambdas, but the onedata_function
    %% engine type is restricted to predefined lambdas only.
    DisallowedOperationSpec = jsonable_record:to_json(
        #atm_onedata_function_operation_spec{},
        atm_lambda_operation_spec
    ),
    DisallowedOperationSpecError = ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"operationSpec.engine">>, lists:map(fun(AllowedType) ->
        atm_lambda_operation_spec:engine_to_json(AllowedType)
    end, atm_lambda_operation_spec:allowed_engines_for_custom_lambdas())),
    BadDataValues = [
        {<<"operationSpec">>, bad_spec, ?ERROR_BAD_DATA(<<"operationSpec">>)},
        {<<"operationSpec">>, 1234, ?ERROR_BAD_DATA(<<"operationSpec">>)},
        {<<"operationSpec">>, DisallowedOperationSpec, DisallowedOperationSpecError},
        {<<"argumentSpecs">>, #{<<"bad">> => <<"object">>}, ?ERROR_BAD_DATA(<<"argumentSpecs">>)},
        {<<"resultSpecs">>, undefined, ?ERROR_BAD_DATA(<<"resultSpecs">>)},
        {<<"summary">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"summary">>)},
        {<<"summary">>, str_utils:rand_hex(250), ?ERROR_BAD_VALUE_BINARY_TOO_LARGE(<<"summary">>, 200)},
        {<<"description">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"description">>)},
        {<<"description">>, str_utils:rand_hex(50001), ?ERROR_BAD_VALUE_BINARY_TOO_LARGE(<<"description">>, 100000)}
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
                fun(#{<<"Location">> := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/atm_lambdas/">>]),
                    [AtmLambda] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(AtmLambda, Data, {true, Creator})
                end
            end)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = create,
            gri = #gri{type = od_atm_lambda, aspect = instance},
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
                <<"name">>,
                <<"operationSpec">>,
                <<"argumentSpecs">>,
                <<"resultSpecs">>
            ],
            optional = [
                <<"summary">>,
                <<"description">>
            ],
            correct_values = #{
                <<"atmInventoryId">> => [AtmInventory],
                <<"name">> => [ozt_atm:gen_example_name()],
                <<"operationSpec">> => [ozt_atm_lambdas:gen_example_operation_spec_json()],
                <<"argumentSpecs">> => [ozt_atm_lambdas:gen_example_argument_specs_json()],
                <<"resultSpecs">> => [ozt_atm_lambdas:gen_example_result_specs_json()],
                <<"summary">> => [ozt_atm:gen_example_summary()],
                <<"description">> => [ozt_atm:gen_example_description()]
            },
            bad_values = lists:flatten([
                {<<"atmInventoryId">>, 1234, ?ERROR_FORBIDDEN},
                {<<"atmInventoryId">>, <<"">>, ?ERROR_FORBIDDEN},
                {<<"atmInventoryId">>, <<"asdq4ewfs">>, ?ERROR_FORBIDDEN},
                BadDataValues,
                ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
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
                    BaseURL = ?URL(Config, [<<"/atm_lambdas/">>]),
                    [AtmLambda] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(AtmLambda, Data, false)
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
                BadDataValues,
                ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
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

    AtmLambdas = lists:flatmap(fun(AtmInventory) ->
        lists:map(fun(_) -> ozt_atm_lambdas:create(AtmInventory) end, lists:seq(1, rand:uniform(8)))
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
    lists:foreach(fun(AtmLambda) ->
        ?assert(ozt:rpc(atm_lambda_logic, exists, [AtmLambda]))
    end, AtmLambdas),
    ?assert(not ozt:rpc(atm_lambda_logic, exists, [<<"asdiucyaie827346w">>])).


get_test(Config) ->
    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    AtmInventory = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventory, AnotherMember, []),

    AtmLambdaData = ozt_atm_lambdas:gen_example_data_json(),
    AtmLambda = ozt_atm_lambdas:create(?USER(Creator), AtmInventory, AtmLambdaData),
    AtmLambdaDataWithInventory = AtmLambdaData#{
        <<"atmInventoryId">> => AtmInventory
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
            path = [<<"/atm_lambdas/">>, AtmLambda],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:private_atm_lambda(rest, AtmLambda, AtmLambdaDataWithInventory, ?SUB(user, Creator))
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = get,
            args = [auth, AtmLambda],
            expected_result = api_test_expect:private_atm_lambda(logic, AtmLambda, AtmLambdaDataWithInventory, ?SUB(user, Creator))
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_atm_lambda, id = AtmLambda,
                aspect = instance, scope = private
            },
            auth_hint = ?THROUGH_ATM_INVENTORY(AtmInventory),
            expected_result = api_test_expect:private_atm_lambda(gs, AtmLambda, AtmLambdaDataWithInventory, ?SUB(user, Creator))
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_atm_inventories_test(Config) ->
    Creators = lists:map(fun(_) -> ozt_users:create() end, lists:seq(1, rand:uniform(8))),
    NonAdmin = ozt_users:create(),

    AtmInventories = lists:flatmap(fun(Creator) ->
        lists:map(fun(_) -> ozt_users:create_atm_inventory_for(Creator) end, lists:seq(1, rand:uniform(8)))
    end, Creators),

    AtmLambdaData = ozt_atm_lambdas:gen_example_data_json(),
    AtmLambda = ozt_atm_lambdas:create(hd(AtmInventories), AtmLambdaData),
    lists:foreach(fun(AtmInventory) ->
        ozt_atm_lambdas:add_to_inventory(AtmLambda, AtmInventory)
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
            path = [<<"/atm_lambdas/">>, AtmLambda, <<"/atm_inventories">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_inventories">> => AtmInventories}
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = get_atm_inventories,
            args = [auth, AtmLambda],
            expected_result = ?OK_LIST(AtmInventories)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_atm_workflow_schemas_test(Config) ->
    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AnotherMember = ozt_users:create(),

    AtmInventory = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventory, AnotherMember, []),

    AtmLambda = ozt_atm_lambdas:create(AtmInventory),

    ExpAtmWorkflowSchemas = lists:foldl(fun(_, Acc) ->
        % this procedure internally checks what lambdas are available in the
        % inventory (here, just the one) and reference them in task schemas,
        % but randomly generated lanes may be empty too
        AtmWorkflowSchema = ozt_atm_workflow_schemas:create(AtmInventory),
        #od_atm_workflow_schema{lanes = Lanes} = ozt_atm_workflow_schemas:get(AtmWorkflowSchema),
        case lists:member(AtmLambda, ozt_atm_workflow_schemas:extract_atm_lambdas_from_lanes(Lanes)) of
            true -> lists:usort([AtmWorkflowSchema | Acc]);
            false -> Acc
        end
    end, [], lists:seq(1, rand:uniform(8))),

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
            path = [<<"/atm_lambdas/">>, AtmLambda, <<"/atm_workflow_schemas">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_workflow_schemas">> => ExpAtmWorkflowSchemas}
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = get_atm_workflow_schemas,
            args = [auth, AtmLambda],
            expected_result = ?OK_LIST(ExpAtmWorkflowSchemas)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_test(Config) ->
    Creator = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AtmInventory = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventory, AnotherMember, []),

    InitialLambdaData = ozt_atm_lambdas:gen_example_data_json(),

    EnvSetUpFun = fun() ->
        #{atm_lambda_id => ozt_atm_lambdas:create(AtmInventory, InitialLambdaData)}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{atm_lambda_id := AtmLambda}, Data) ->
        AtmLambdaRecord = ozt_atm_lambdas:get(AtmLambda),

        InitialName = maps:get(<<"name">>, InitialLambdaData),
        ExpName = case ShouldSucceed of
            false -> InitialName;
            true -> maps:get(<<"name">>, Data, InitialName)
        end,

        InitialSummary = maps:get(<<"summary">>, InitialLambdaData, <<"Missing summary">>),
        ExpSummary = case ShouldSucceed of
            false -> InitialSummary;
            true -> maps:get(<<"summary">>, Data, InitialSummary)
        end,

        InitialDescription = maps:get(<<"description">>, InitialLambdaData, <<"Missing description">>),
        ExpDescription = case ShouldSucceed of
            false -> InitialDescription;
            true -> maps:get(<<"description">>, Data, InitialDescription)
        end,

        ?assertEqual(ExpName, AtmLambdaRecord#od_atm_lambda.name),
        ?assertEqual(ExpSummary, AtmLambdaRecord#od_atm_lambda.summary),
        ?assertEqual(ExpDescription, AtmLambdaRecord#od_atm_lambda.description)
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
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = atm_lambda_logic,
            function = update,
            args = [auth, atm_lambda_id, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_atm_lambda, id = atm_lambda_id, aspect = instance},
            expected_result = ?OK_RES
        },
        data_spec = #data_spec{
            at_least_one = [<<"name">>, <<"summary">>, <<"description">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"summary">> => [<<"">>, str_utils:rand_hex(92)],
                <<"description">> => [<<"">>, str_utils:rand_hex(137)]
            },
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME) ++ [
                {<<"summary">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"summary">>)},
                {<<"summary">>, str_utils:rand_hex(250), ?ERROR_BAD_VALUE_BINARY_TOO_LARGE(<<"summary">>, 200)},
                {<<"description">>, #{atom => atom}, ?ERROR_BAD_VALUE_BINARY(<<"description">>)},
                {<<"description">>, str_utils:rand_hex(50001), ?ERROR_BAD_VALUE_BINARY_TOO_LARGE(<<"description">>, 100000)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


add_to_inventory_test(Config) ->
    Creator = ozt_users:create(),
    MemberWithPrivs = ozt_users:create(),
    MemberWithNoPrivs = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    OriginalAtmInventory = ozt_users:create_atm_inventory_for(Creator),
    TargetAtmInventory = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(TargetAtmInventory, MemberWithPrivs, [?ATM_INVENTORY_MANAGE_LAMBDAS]),
    ozt_atm_inventories:add_user(OriginalAtmInventory, MemberWithPrivs, [?ATM_INVENTORY_MANAGE_LAMBDAS]),
    ozt_atm_inventories:add_user(TargetAtmInventory, MemberWithNoPrivs, []),
    ozt_atm_inventories:add_user(OriginalAtmInventory, MemberWithNoPrivs, []),

    EnvSetUpFun = fun() ->
        #{atm_lambda_id => ozt_atm_lambdas:create(OriginalAtmInventory)}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{atm_lambda_id := SubjectAtmLambda}, _) ->
        ?assertEqual(ShouldSucceed, lists:member(TargetAtmInventory, ozt_atm_lambdas:get_atm_inventories(SubjectAtmLambda)))
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
            path = [<<"/atm_lambdas/">>, atm_lambda_id, <<"/atm_inventories/">>, TargetAtmInventory],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = LogicSpec = #logic_spec{
            module = atm_lambda_logic,
            function = add_to_inventory,
            args = [auth, atm_lambda_id, TargetAtmInventory],
            expected_result = ?OK
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % check that adding is not possible if the lambda already is a member of the inventory
    MemberAtmLambda = ozt_atm_lambdas:create(OriginalAtmInventory),
    AddingErrorApiTestSpec = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            path = [<<"/atm_lambdas/">>, MemberAtmLambda, <<"/atm_inventories/">>, OriginalAtmInventory],
            expected_code = ?HTTP_409_CONFLICT
        },
        logic_spec = LogicSpec#logic_spec{
            args = [auth, MemberAtmLambda, OriginalAtmInventory],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(
                od_atm_lambda, MemberAtmLambda, od_atm_inventory, OriginalAtmInventory
            ))
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_utils:run_tests(
        Config, AddingErrorApiTestSpec, EnvSetUpFun, undefined, undefined
    )).

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
