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
    link_to_inventory_test/1,
    unlink_from_inventory_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        get_atm_inventories_test,
        get_atm_workflow_schemas_test,
        update_test,
        link_to_inventory_test,
        unlink_from_inventory_test
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

    VerifyFun = fun(AtmLambdaId, Data, CheckCreator) ->
        AtmLambdaRecord = ozt_atm_lambdas:get(AtmLambdaId),

        ExpName = maps:get(<<"name">>, Data),
        ExpSummary = maps:get(<<"summary">>, Data, <<"Missing summary">>),
        ExpDescription = maps:get(<<"description">>, Data, <<"Missing description">>),
        ExpOperationSpec = jsonable_record:from_json(maps:get(<<"operationSpec">>, Data), atm_lambda_operation_spec),
        ExpArgumentSpecs = jsonable_record:list_from_json(maps:get(<<"argumentSpecs">>, Data, []), atm_lambda_argument_spec),
        ExpResultSpecs = jsonable_record:list_from_json(maps:get(<<"resultSpecs">>, Data, []), atm_lambda_result_spec),
        ExpResourceSpec = case maps:find(<<"resourceSpec">>, Data) of
            {ok, ResourceSpecJson} -> jsonable_record:from_json(ResourceSpecJson, atm_resource_spec);
            error -> ozt_atm_lambdas:default_resource_spec()
        end,
        ExpAtmInventories = [AtmInventoryId],
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

            resource_spec = ExpResourceSpec,

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
        {<<"resourceSpec">>, <<"bad">>, ?ERROR_BAD_DATA(<<"resourceSpec">>)},
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
                <<"description">>,
                <<"resourceSpec">>
            ],
            correct_values = #{
                <<"atmInventoryId">> => [AtmInventoryId],
                <<"name">> => [ozt_atm:gen_example_name()],
                <<"operationSpec">> => [ozt_atm_lambdas:gen_example_operation_spec_json()],
                <<"argumentSpecs">> => [ozt_atm_lambdas:gen_example_argument_specs_json()],
                <<"resultSpecs">> => [ozt_atm_lambdas:gen_example_result_specs_json()],
                <<"summary">> => [ozt_atm:gen_example_summary()],
                <<"description">> => [ozt_atm:gen_example_description()],
                <<"resourceSpec">> => [ozt_atm_lambdas:gen_example_resource_spec_json()]
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
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/atm_lambdas/">>]),
                    [AtmLambdaId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(AtmLambdaId, Data, false)
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

    AtmLambdaData = ozt_atm_lambdas:gen_example_data_json(),
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
            expected_result = api_test_expect:private_atm_lambda(gs, AtmLambdaId, AtmLambdaDataWithInventory, ?SUB(user, Creator))
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


update_test(Config) ->
    Creator = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventoryId, AnotherMember, []),

    InitialLambdaData = ozt_atm_lambdas:gen_example_data_json(),

    EnvSetUpFun = fun() ->
        #{atm_lambda_id => ozt_atm_lambdas:create(AtmInventoryId, InitialLambdaData)}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{atm_lambda_id := AtmLambdaId}, Data) ->
        AtmLambdaRecord = ozt_atm_lambdas:get(AtmLambdaId),

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

%%%===================================================================
%%% Helper functions
%%%===================================================================

gen_atm_workflow_schema_with_lambda(AtmInventoryId, AtmLambdaId) ->
    % this procedure internally checks what lambdas are available in the inventory
    % and randomly reference them in task schemas, keep generating until the desired
    % lambda is referenced at least once
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(AtmInventoryId),
    AtmWorkflowSchema = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),
    case lists:member(AtmLambdaId, od_atm_workflow_schema:extract_all_referenced_atm_lambdas(AtmWorkflowSchema)) of
        true ->
            AtmWorkflowSchemaId;
        false ->
            gen_atm_workflow_schema_with_lambda(AtmInventoryId, AtmLambdaId)
    end.

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
