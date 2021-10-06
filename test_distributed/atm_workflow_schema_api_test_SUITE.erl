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
    get_atm_lambdas_test/1,
    dump_test/1,
    update_test/1,
    insert_revision_test/1,
    delete_revision_test/1,
    dump_revision_test/1,
    delete_test/1,
    recreate_atm_workflow_schema_in_the_same_inventory_test/1,
    recreate_atm_workflow_schema_in_different_inventory_test/1,
    recreate_atm_workflow_schema_with_mixed_linked_and_duplicated_lambdas_test/1,
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
        insert_revision_test,
        delete_revision_test,
        dump_revision_test,
        delete_test,
        recreate_atm_workflow_schema_in_the_same_inventory_test,
        recreate_atm_workflow_schema_in_different_inventory_test,
        recreate_atm_workflow_schema_with_mixed_linked_and_duplicated_lambdas_test,
        bad_supplementary_lambdas_data_test
    ]).

-define(RAND_REV_NUMBER(), ?RAND_INT(1, 100)).

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
        {AtmLambdaId, ozt_atm_lambdas:dump_to_json(AtmLambdaId)}
    end, lists:seq(1, rand:uniform(7)))),
    create_test_base(Config, Creator, {to_link, AtmLambdasToBeLinked}),

    % test with supplementary lambdas that should be duplicated
    % (the user does not have access to them)
    UnrelatedAtmInventoryId = ozt_atm_inventories:create(),
    AtmLambdasToBeDuplicated = maps:from_list(lists:map(fun(_) ->
        AtmLambdaId = ozt_atm_lambdas:create(UnrelatedAtmInventoryId),
        {AtmLambdaId, ozt_atm_lambdas:dump_to_json(AtmLambdaId)}
    end, lists:seq(1, rand:uniform(7)))),
    create_test_base(Config, Creator, {to_duplicate, AtmLambdasToBeDuplicated}).

create_test_base(Config, Creator, SupplementaryAtmLambdas) ->
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

        #{
            atm_inventory_id => AtmInventoryId,
            available_atm_lambdas => case SupplementaryAtmLambdas of
                none -> InventoryAtmLambdas;
                {_, LambdaDefinitions} -> maps:keys(LambdaDefinitions)
            end,
            initial_revision_number => rand:uniform(100)
        }
    end,

    VerifyFun = fun(AtmWorkflowSchemaId, #{
        atm_inventory_id := AtmInventoryId,
        initial_revision_number := InitialRevisionNumber
    }, Data, CreatorType) ->
        AtmWorkflowSchemaRecord = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),

        ExpName = maps:get(<<"name">>, Data),
        ExpSummary = maps:get(<<"summary">>, Data, <<"Missing summary">>),
        ExpOriginalAtmWorkflowSchemaId = maps:get(<<"originalAtmWorkflowSchemaId">>, Data, undefined),

        ExpInitialRevision = case maps:find(<<"revision">>, Data) of
            error ->
                undefined;
            {ok, InitialRevisionData} ->
                AtmWorkflowSchemaRevision = jsonable_record:from_json(
                    maps:get(<<"schema">>, InitialRevisionData), atm_workflow_schema_revision
                ),
                case SupplementaryAtmLambdas of
                    none ->
                        AtmWorkflowSchemaRevision;
                    {to_link, _} ->
                        AtmWorkflowSchemaRevision;
                    {to_duplicate, AtmLambdaDefinitions} ->
                        case CreatorType of
                            root_or_admin ->
                                % root and admin clients can always link the lambdas, so no duplicates should be created
                                AtmWorkflowSchemaRevision;
                            {regular, _} ->
                                ReferencedLambdas = atm_workflow_schema_revision:extract_referenced_atm_lambdas(AtmWorkflowSchemaRevision),
                                ExpSubstitutedLambdas = lists_utils:intersect(ReferencedLambdas, maps:keys(AtmLambdaDefinitions)),
                                ozt_atm_workflow_schemas:substitute_atm_lambdas_for_duplicates(
                                    AtmWorkflowSchemaRevision, ExpSubstitutedLambdas, AtmInventoryId
                                )
                        end
                end
        end,

        ExpAtmLambdas = case ExpInitialRevision of
            undefined ->
                [];
            _ ->
                atm_workflow_schema_revision:extract_referenced_atm_lambdas(ExpInitialRevision)
        end,

        ExpCreationTime = ozt_mocks:get_frozen_time_seconds(),
        ExpCreator = case CreatorType of
            {regular, UserId} -> ?SUB(user, UserId);
            root_or_admin -> AtmWorkflowSchemaRecord#od_atm_workflow_schema.creator
        end,

        ?assertEqual(#od_atm_workflow_schema{
            name = ExpName,
            summary = ExpSummary,

            revision_registry = #atm_workflow_schema_revision_registry{registry = case ExpInitialRevision of
                undefined ->
                    #{};
                _ ->
                    #{InitialRevisionNumber => ExpInitialRevision}
            end},

            original_atm_workflow_schema = ExpOriginalAtmWorkflowSchemaId,
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
                <<"name">>
            ]),
            optional = [
                <<"summary">>,
                <<"revision">>,
                <<"originalAtmWorkflowSchemaId">>
            ],
            correct_values = #{
                <<"atmInventoryId">> => [fun(#{atm_inventory_id := AtmInventoryId}) -> AtmInventoryId end],
                <<"name">> => [ozt_atm:gen_example_name()],
                <<"summary">> => [ozt_atm:gen_example_summary()],
                <<"revision">> => [fun(#{
                    initial_revision_number := RevisionNumber,
                    atm_inventory_id := AtmInventoryId,
                    available_atm_lambdas := AvailableAtmLambdas
                }) -> #{
                    <<"originalRevisionNumber">> => RevisionNumber,
                    <<"schema">> => ozt_atm_workflow_schemas:gen_example_revision_schema_json(AtmInventoryId, AvailableAtmLambdas),
                    <<"supplementaryAtmLambdas">> => case SupplementaryAtmLambdas of
                        none -> #{};
                        {_, AtmLambdaDefinitions} -> AtmLambdaDefinitions
                    end
                } end],
                <<"originalAtmWorkflowSchemaId">> => [?RAND_STR(16)]
            },
            bad_values = lists:flatten([
                {<<"atmInventoryId">>, 1234, ?ERROR_FORBIDDEN},
                {<<"atmInventoryId">>, <<"">>, ?ERROR_FORBIDDEN},
                {<<"atmInventoryId">>, <<"asdq4ewfs">>, ?ERROR_FORBIDDEN},
                {<<"revision">>, bad_spec, ?ERROR_BAD_VALUE_JSON(<<"revision">>)},
                {<<"revision">>, [1234], ?ERROR_BAD_VALUE_JSON(<<"revision">>)},
                {<<"originalAtmWorkflowSchemaId">>, [1234], ?ERROR_BAD_VALUE_BINARY(<<"originalAtmWorkflowSchemaId">>)},
                {<<"originalAtmWorkflowSchemaId">>, <<>>, ?ERROR_BAD_VALUE_EMPTY(<<"originalAtmWorkflowSchemaId">>)},
                name_summary_bad_data_values()
            ])
        }
    },

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
        data_spec = DataSpec#data_spec{
            bad_values = lists:flatten([
                {<<"atmInventoryId">>, <<"">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"atmInventoryId">>)},
                {<<"atmInventoryId">>, <<"asdq4ewfs">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"atmInventoryId">>)},
                {<<"revision">>, bad_spec, ?ERROR_BAD_VALUE_JSON(<<"revision">>)},
                {<<"revision">>, [1234], ?ERROR_BAD_VALUE_JSON(<<"revision">>)},
                name_summary_bad_data_values()
            ])
        }
    },
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec, EnvSetUpFun, EnvTearDownFun, undefined)).


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
    ExpAtmLambdas = case maps:find(<<"revision">>, AtmWorkflowSchemaData) of
        error ->
            [];
        {ok, #{<<"schema">> := RevisionSchema}} ->
            ozt_atm_workflow_schemas:extract_referenced_atm_lambdas_from_revision(RevisionSchema)
    end,
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

    RevisionNumber = ?RAND_REV_NUMBER(),
    AtmWorkflowSchemaData = #{
        <<"name">> => ozt_atm:gen_example_name(),
        <<"summary">> => ozt_atm:gen_example_summary(),

        <<"revision">> => InitialRevisionData = #{
            <<"originalRevisionNumber">> => RevisionNumber,
            <<"schema">> => ozt_atm_workflow_schemas:gen_example_revision_schema_json(AtmInventoryId)
        }
    },
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(?USER(Creator), AtmInventoryId, AtmWorkflowSchemaData),
    AtmWorkflowSchema = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),
    ReferencedAtmLambdas = od_atm_workflow_schema:extract_all_referenced_atm_lambdas(AtmWorkflowSchema),

    ExpectedJsonDump = AtmWorkflowSchemaData#{
        <<"schemaFormatVersion">> => 2,
        <<"originalAtmWorkflowSchemaId">> => AtmWorkflowSchemaId,
        <<"revision">> => InitialRevisionData#{
            <<"schemaFormatVersion">> => 2,
            <<"supplementaryAtmLambdas">> => lists:foldl(fun(AtmLambdaId, Acc) ->
                AtmLambda = ozt_atm_lambdas:get(AtmLambdaId),
                Acc#{AtmLambdaId => #{
                    <<"schemaFormatVersion">> => 2,

                    <<"name">> => AtmLambda#od_atm_lambda.name,
                    <<"summary">> => AtmLambda#od_atm_lambda.summary,
                    <<"description">> => AtmLambda#od_atm_lambda.description,

                    <<"operationSpec">> => jsonable_record:to_json(AtmLambda#od_atm_lambda.operation_spec, atm_lambda_operation_spec),
                    <<"argumentSpecs">> => jsonable_record:list_to_json(AtmLambda#od_atm_lambda.argument_specs, atm_lambda_argument_spec),
                    <<"resultSpecs">> => jsonable_record:list_to_json(AtmLambda#od_atm_lambda.result_specs, atm_lambda_result_spec),

                    <<"resourceSpec">> => jsonable_record:to_json(AtmLambda#od_atm_lambda.resource_spec, atm_resource_spec),

                    <<"checksum">> => AtmLambda#od_atm_lambda.checksum
                }}
            end, #{}, ReferencedAtmLambdas)
        }
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
            args = [auth, AtmWorkflowSchemaId, data],
            expected_result = ?OK_MAP(ExpectedJsonDump)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{
                type = od_atm_workflow_schema, id = AtmWorkflowSchemaId,
                aspect = dump, scope = private
            },
            expected_result = ?OK_MAP(ExpectedJsonDump)
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
                {<<"includeRevision">>, -7, ?ERROR_BAD_VALUE_TOO_LOW(<<"includeRevision">>, 1)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_test(Config) ->
    Creator = ozt_users:create(),

    % test with no supplementary lambdas
    update_test_base(Config, Creator, none),

    % test with supplementary lambdas that should be linked
    % (available in different inventory where the user has privileges to manage lambdas)
    AnotherAtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    AtmLambdasToBeLinked = maps:from_list(lists:map(fun(_) ->
        AtmLambdaId = ozt_atm_lambdas:create(AnotherAtmInventoryId),
        {AtmLambdaId, ozt_atm_lambdas:dump_to_json(AtmLambdaId)}
    end, lists:seq(1, rand:uniform(7)))),
    update_test_base(Config, Creator, {to_link, AtmLambdasToBeLinked}),

    % test with supplementary lambdas that should be duplicated
    % (the user does not have access to them)
    UnrelatedAtmInventoryId = ozt_atm_inventories:create(),
    AtmLambdasToBeDuplicated = maps:from_list(lists:map(fun(_) ->
        AtmLambdaId = ozt_atm_lambdas:create(UnrelatedAtmInventoryId),
        {AtmLambdaId, ozt_atm_lambdas:dump_to_json(AtmLambdaId)}
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

        InitialRevisionNumber = ?RAND_REV_NUMBER(),

        InitialWorkflowSchemaDataWithoutInitialRevision = #{
            <<"name">> => ozt_atm:gen_example_name(),
            <<"summary">> => ozt_atm:gen_example_summary()
        },
        InitialWorkflowSchemaData = case ?RAND_BOOL() of
            true ->
                InitialWorkflowSchemaDataWithoutInitialRevision#{
                    <<"revision">> => #{
                        <<"originalRevisionNumber">> => InitialRevisionNumber,
                        <<"schema">> => ozt_atm_workflow_schemas:gen_example_revision_schema_json(AtmInventoryId)
                    }
                };
            false ->
                InitialWorkflowSchemaDataWithoutInitialRevision
        end,

        #{
            atm_inventory_id => AtmInventoryId,
            available_atm_lambdas => case SupplementaryAtmLambdas of
                none -> InventoryAtmLambdas;
                {_, LambdaDefinitions} -> maps:keys(LambdaDefinitions)
            end,
            initial_workflow_schema_data => InitialWorkflowSchemaData,
            atm_workflow_schema_id => ozt_atm_workflow_schemas:create(AtmInventoryId, InitialWorkflowSchemaData),
            initial_revision_number => InitialRevisionNumber
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

        InitialSummary = maps:get(<<"summary">>, InitialWorkflowSchemaData, <<"Missing summary">>),
        ExpSummary = case ShouldSucceed of
            false -> InitialSummary;
            true -> maps:get(<<"summary">>, Data, InitialSummary)
        end,

        InitialRevisionRegistry = case maps:find(<<"revision">>, InitialWorkflowSchemaData) of
            error ->
                #{};
            {ok, #{<<"originalRevisionNumber">> := InitialRevNumber, <<"schema">> := InitialRevSchema}} ->
                #{
                    InitialRevNumber => jsonable_record:from_json(InitialRevSchema, atm_workflow_schema_revision)
                }
        end,

        ExpRevisionRegistry = case {ShouldSucceed, maps:find(<<"revision">>, Data)} of
            {false, _} ->
                InitialRevisionRegistry;
            {true, error} ->
                InitialRevisionRegistry;
            {true, {ok, InitialRevisionData}} ->
                NewRevisionNumber = maps:get(<<"originalRevisionNumber">>, InitialRevisionData),
                NewRevisionSchema = jsonable_record:from_json(
                    maps:get(<<"schema">>, InitialRevisionData), atm_workflow_schema_revision
                ),
                InitialRevisionRegistry#{NewRevisionNumber => case SupplementaryAtmLambdas of
                    none ->
                        NewRevisionSchema;
                    {to_link, _} ->
                        NewRevisionSchema;
                    {to_duplicate, AtmLambdaDefinitions} ->
                        case ClientType of
                            root_or_admin ->
                                % root and admin clients can always link the lambdas, so no duplicates should be created
                                NewRevisionSchema;
                            {regular, _} ->
                                ReferencedLambdas = atm_workflow_schema_revision:extract_referenced_atm_lambdas(NewRevisionSchema),
                                ExpSubstitutedLambdas = lists_utils:intersect(ReferencedLambdas, maps:keys(AtmLambdaDefinitions)),
                                ozt_atm_workflow_schemas:substitute_atm_lambdas_for_duplicates(
                                    NewRevisionSchema, ExpSubstitutedLambdas, AtmInventoryId
                                )
                        end
                end}
        end,

        ExpAtmLambdas = lists:usort(lists:flatmap(fun(AtmWorkflowSchemaRevision) ->
            atm_workflow_schema_revision:extract_referenced_atm_lambdas(AtmWorkflowSchemaRevision)
        end, maps:values(ExpRevisionRegistry))),

        ?assertEqual(ExpName, AtmWorkflowSchemaRecord#od_atm_workflow_schema.name),
        ?assertEqual(ExpSummary, AtmWorkflowSchemaRecord#od_atm_workflow_schema.summary),
        ?assertEqual(
            #atm_workflow_schema_revision_registry{registry = ExpRevisionRegistry},
            AtmWorkflowSchemaRecord#od_atm_workflow_schema.revision_registry
        ),
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
        data_spec = #data_spec{
            required = [
                <<"name">>,
                <<"summary">>,
                <<"revision">>
            ],
            correct_values = #{
                <<"name">> => [ozt_atm:gen_example_name()],
                <<"summary">> => [ozt_atm:gen_example_summary()],
                <<"revision">> => [fun(#{
                    initial_revision_number := RevisionNumber,
                    atm_inventory_id := AtmInventoryId,
                    available_atm_lambdas := AvailableAtmLambdas
                }) -> #{
                    <<"originalRevisionNumber">> => lists_utils:random_element([RevisionNumber, ?RAND_REV_NUMBER()]),
                    <<"schema">> => ozt_atm_workflow_schemas:gen_example_revision_schema_json(AtmInventoryId, AvailableAtmLambdas),
                    <<"supplementaryAtmLambdas">> => case SupplementaryAtmLambdas of
                        none -> #{};
                        {_, AtmLambdaDefinitions} -> AtmLambdaDefinitions
                    end
                } end]
            },
            bad_values = lists:flatten([
                {<<"revision">>, bad_spec, ?ERROR_BAD_VALUE_JSON(<<"revision">>)},
                {<<"revision">>, [1234], ?ERROR_BAD_VALUE_JSON(<<"revision">>)},
                name_summary_bad_data_values()
            ])
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, VerifyEndFun)).


insert_revision_test(Config) ->
    Creator = ozt_users:create(),

    % test with no supplementary lambdas
    insert_revision_test_base(Config, Creator, none),

    % test with supplementary lambdas that should be linked
    % (available in different inventory where the user has privileges to manage lambdas)
    AnotherAtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
    AtmLambdasToBeLinked = maps:from_list(lists:map(fun(_) ->
        AtmLambdaId = ozt_atm_lambdas:create(AnotherAtmInventoryId),
        {AtmLambdaId, ozt_atm_lambdas:dump_to_json(AtmLambdaId)}
    end, lists:seq(1, rand:uniform(7)))),
    insert_revision_test_base(Config, Creator, {to_link, AtmLambdasToBeLinked}),

    % test with supplementary lambdas that should be duplicated
    % (the user does not have access to them)
    UnrelatedAtmInventoryId = ozt_atm_inventories:create(),
    AtmLambdasToBeDuplicated = maps:from_list(lists:map(fun(_) ->
        AtmLambdaId = ozt_atm_lambdas:create(UnrelatedAtmInventoryId),
        {AtmLambdaId, ozt_atm_lambdas:dump_to_json(AtmLambdaId)}
    end, lists:seq(1, rand:uniform(7)))),
    insert_revision_test_base(Config, Creator, {to_duplicate, AtmLambdasToBeDuplicated}),

    % test validation of revision number in the aspect
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(AnotherAtmInventoryId),
    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, Creator}]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/atm_workflow_schemas/">>, AtmWorkflowSchemaId, <<"/revision/">>, <<"abc">>],
            expected_code = ?HTTP_400_BAD_REQUEST
        },
        logic_spec = #logic_spec{
            module = atm_workflow_schema_logic,
            function = insert_revision,
            args = [auth, AtmWorkflowSchemaId, <<"-17">>, data],
            expected_result = ?ERROR_REASON(?ERROR_BAD_DATA(<<"targetRevisionNumber">>))
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_atm_workflow_schema, id = AtmWorkflowSchemaId, aspect = {revision, <<"undefined">>}},
            expected_result = ?ERROR_REASON(?ERROR_BAD_DATA(<<"targetRevisionNumber">>))
        },
        data_spec = #data_spec{
            required = [
                <<"schema">>
            ],
            correct_values = #{
                <<"schema">> => [ozt_atm_workflow_schemas:gen_example_revision_schema_json(AnotherAtmInventoryId)]
            },
            bad_values = []
        }
    })).

insert_revision_test_base(Config, Creator, SupplementaryAtmLambdas) ->
    insert_revision_test_base(Config, Creator, SupplementaryAtmLambdas, {regular, Creator}),
    insert_revision_test_base(Config, Creator, SupplementaryAtmLambdas, root_or_admin).

insert_revision_test_base(Config, Creator, SupplementaryAtmLambdas, ClientType) ->
    insert_revision_test_base(Config, Creator, SupplementaryAtmLambdas, ClientType, take_original_revision_number),
    insert_revision_test_base(Config, Creator, SupplementaryAtmLambdas, ClientType, override_original_revision_number).

insert_revision_test_base(Config, Creator, SupplementaryAtmLambdas, ClientType, RevisionNumberProvisionMode) ->
    MemberWithNoPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    EnvSetUpFun = fun() ->
        AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
        ozt_atm_inventories:add_user(
            AtmInventoryId, MemberWithNoPriv, privileges:atm_inventory_admin() -- [?ATM_INVENTORY_MANAGE_WORKFLOW_SCHEMAS]
        ),
        AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(AtmInventoryId, #{
            <<"name">> => ozt_atm:gen_example_name(),
            <<"summary">> => ozt_atm:gen_example_summary()
        }),
        PreexistingRevisionNumbers = lists:usort(lists_utils:random_sublist([
            ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER()
        ])),
        lists:foreach(fun(RevisionNumber) ->
            ozt_atm_workflow_schemas:insert_revision(AtmWorkflowSchemaId, RevisionNumber, #{
                <<"schema">> => ozt_atm_workflow_schemas:gen_example_revision_schema_json(AtmInventoryId)
            })
        end, PreexistingRevisionNumbers),
        RevisionNumberToInsert = lists_utils:random_element([?RAND_REV_NUMBER() | PreexistingRevisionNumbers]),
        #{
            atm_inventory_id => AtmInventoryId,
            atm_workflow_schema_id => AtmWorkflowSchemaId,
            preexisting_revision_numbers => PreexistingRevisionNumbers,
            revision_number_to_insert => RevisionNumberToInsert,
            target_revision_number_binary => case RevisionNumberProvisionMode of
                take_original_revision_number -> <<"auto">>;
                override_original_revision_number -> integer_to_binary(RevisionNumberToInsert)
            end,
            original_revision_number => case RevisionNumberProvisionMode of
                take_original_revision_number -> RevisionNumberToInsert;
                override_original_revision_number -> RevisionNumberToInsert + 13 % should be ignored
            end
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{
        atm_inventory_id := AtmInventoryId,
        atm_workflow_schema_id := AtmWorkflowSchemaId,
        preexisting_revision_numbers := PreexistingRevisionNumbers,
        revision_number_to_insert := RevisionToInsert
    }, Data) ->
        #od_atm_workflow_schema{
            revision_registry = #atm_workflow_schema_revision_registry{
                registry = Registry
            }
        } = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),

        ExpectedFinalRevisionNumbers = case ShouldSucceed of
            false ->
                PreexistingRevisionNumbers;
            true ->
                case lists:member(RevisionToInsert, PreexistingRevisionNumbers) of
                    true -> PreexistingRevisionNumbers;
                    false -> [RevisionToInsert | PreexistingRevisionNumbers]
                end
        end,
        ?assertEqual(lists:sort(ExpectedFinalRevisionNumbers), lists:sort(maps:keys(Registry))),

        case ShouldSucceed of
            false ->
                ok;
            true ->
                ActualNewRevision = maps:get(RevisionToInsert, Registry),
                RequestedNewRevision = jsonable_record:from_json(
                    maps:get(<<"schema">>, Data), atm_workflow_schema_revision
                ),
                ExpectedNewRevision = case SupplementaryAtmLambdas of
                    none ->
                        RequestedNewRevision;
                    {to_link, _} ->
                        RequestedNewRevision;
                    {to_duplicate, AtmLambdaDefinitions} ->
                        case ClientType of
                            root_or_admin ->
                                % root and admin clients can always link the lambdas, so no duplicates should be created
                                RequestedNewRevision;
                            {regular, _} ->
                                ReferencedLambdas = atm_workflow_schema_revision:extract_referenced_atm_lambdas(RequestedNewRevision),
                                ExpSubstitutedLambdas = lists_utils:intersect(ReferencedLambdas, maps:keys(AtmLambdaDefinitions)),
                                ozt_atm_workflow_schemas:substitute_atm_lambdas_for_duplicates(
                                    RequestedNewRevision, ExpSubstitutedLambdas, AtmInventoryId
                                )
                        end
                end,
                ?assertEqual(ActualNewRevision, ExpectedNewRevision)
        end
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
            method = put,
            path = [<<"/atm_workflow_schemas/">>, atm_workflow_schema_id, <<"/revision/">>, target_revision_number_binary],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = atm_workflow_schema_logic,
            function = insert_revision,
            args = [auth, atm_workflow_schema_id, target_revision_number_binary, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_atm_workflow_schema, id = atm_workflow_schema_id, aspect = {revision, target_revision_number_binary}},
            expected_result = ?OK_RES
        },
        data_spec = #data_spec{
            required = lists:flatten([
                <<"schema">>,
                case RevisionNumberProvisionMode of
                    override_original_revision_number -> [];
                    take_original_revision_number -> <<"originalRevisionNumber">>
                end,
                case SupplementaryAtmLambdas of
                    none -> [];
                    _ -> <<"supplementaryAtmLambdas">>
                end
            ]),
            optional = lists:flatten([
                case RevisionNumberProvisionMode of
                    take_original_revision_number -> [];
                    override_original_revision_number -> <<"originalRevisionNumber">>
                end,
                case SupplementaryAtmLambdas of
                    none -> <<"supplementaryAtmLambdas">>;
                    _ -> []
                end
            ]),
            correct_values = #{
                <<"originalRevisionNumber">> => [fun(#{original_revision_number := OriginalRevisionNumber}) ->
                    OriginalRevisionNumber
                end],
                <<"schema">> => [fun(#{atm_inventory_id := AtmInventoryId}) ->
                    ozt_atm_workflow_schemas:gen_example_revision_schema_json(AtmInventoryId)
                end],
                <<"supplementaryAtmLambdas">> => [case SupplementaryAtmLambdas of
                    none -> #{};
                    {_, AtmLambdaDefinitions} -> AtmLambdaDefinitions
                end]
            },
            bad_values = lists:flatten([
                case RevisionNumberProvisionMode of
                    override_original_revision_number ->
                        [];
                    take_original_revision_number ->
                        {<<"originalRevisionNumber">>, [<<"a">>], ?ERROR_BAD_VALUE_INTEGER(<<"originalRevisionNumber">>)}
                end,
                {<<"schema">>, #{<<"k">> => <<"v">>}, ?ERROR_BAD_DATA(<<"schema">>)},
                {<<"supplementaryAtmLambdas">>, 987, ?ERROR_BAD_VALUE_JSON(<<"supplementaryAtmLambdas">>)}
            ])
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)).


delete_revision_test(Config) ->
    delete_revision_test_base(Config, existent),
    delete_revision_test_base(Config, nonexistent).

delete_revision_test_base(Config, RevisionExistence) ->
    Creator = ozt_users:create(),
    MemberWithNoPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    EnvSetUpFun = fun() ->
        AtmInventoryId = ozt_users:create_atm_inventory_for(Creator),
        ozt_atm_inventories:add_user(
            AtmInventoryId, MemberWithNoPriv, privileges:atm_inventory_admin() -- [?ATM_INVENTORY_MANAGE_WORKFLOW_SCHEMAS]
        ),
        AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(AtmInventoryId, #{
            <<"name">> => ozt_atm:gen_example_name(),
            <<"summary">> => ozt_atm:gen_example_summary()
        }),
        PreexistingRevisionNumbers = lists:usort(lists_utils:random_sublist([
            ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER(), ?RAND_REV_NUMBER()
        ], 1, all)),
        lists:foreach(fun(RevisionNumber) ->
            ozt_atm_workflow_schemas:insert_revision(AtmWorkflowSchemaId, RevisionNumber, #{
                <<"schema">> => ozt_atm_workflow_schemas:gen_example_revision_schema_json(AtmInventoryId)
            })
        end, PreexistingRevisionNumbers),
        RevisionNumberToDelete = case RevisionExistence of
            existent ->
                lists_utils:random_element(PreexistingRevisionNumbers);
            nonexistent ->
                lists_utils:random_element(lists:seq(1, 100) -- PreexistingRevisionNumbers)
        end,
        #{
            atm_workflow_schema_id => AtmWorkflowSchemaId,
            preexisting_revision_numbers => PreexistingRevisionNumbers,
            revision_number_to_delete => RevisionNumberToDelete,
            revision_number_to_delete_binary => integer_to_binary(RevisionNumberToDelete)
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{
        atm_workflow_schema_id := AtmWorkflowSchemaId,
        preexisting_revision_numbers := PreexistingRevisionNumbers,
        revision_number_to_delete := RevisionNumberToDelete
    }, _Data) ->
        #od_atm_workflow_schema{
            revision_registry = #atm_workflow_schema_revision_registry{
                registry = Registry
            }
        } = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),

        ExpectedFinalRevisionNumbers = case {ShouldSucceed, RevisionExistence} of
            {true, existent} ->
                PreexistingRevisionNumbers -- [RevisionNumberToDelete];
            {_, _} ->
                PreexistingRevisionNumbers
        end,
        ?assertEqual(lists:sort(ExpectedFinalRevisionNumbers), lists:sort(maps:keys(Registry)))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            % existence is checked before authorization - for a nonexistent resource,
            % unauthenticated and unauthorized clients should get ERROR_NOT_FOUND too
            correct = lists:flatten([
                root,
                {admin, [?OZ_ATM_INVENTORIES_UPDATE]},
                {user, Creator},
                case RevisionExistence of
                    existent -> [];
                    nonexistent -> [nobody, {user, MemberWithNoPriv}, {user, NonAdmin}]
                end
            ]),
            unauthorized = lists:flatten(case RevisionExistence of
                existent -> [nobody];
                nonexistent -> []
            end),
            forbidden = lists:flatten([
                case RevisionExistence of
                    existent -> [{user, MemberWithNoPriv}, {user, NonAdmin}];
                    nonexistent -> []
                end
            ])
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/atm_workflow_schemas/">>, atm_workflow_schema_id, <<"/revision/">>, revision_number_to_delete_binary],
            expected_code = case RevisionExistence of
                existent -> ?HTTP_204_NO_CONTENT;
                nonexistent -> ?HTTP_404_NOT_FOUND
            end
        },
        logic_spec = #logic_spec{
            module = atm_workflow_schema_logic,
            function = delete_revision,
            args = [auth, atm_workflow_schema_id, revision_number_to_delete],
            expected_result = case RevisionExistence of
                existent -> ?OK_RES;
                nonexistent -> ?ERROR_REASON(?ERROR_NOT_FOUND)
            end
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_atm_workflow_schema, id = atm_workflow_schema_id, aspect = {revision, revision_number_to_delete_binary}},
            expected_result = case RevisionExistence of
                existent -> ?OK_RES;
                nonexistent -> ?ERROR_REASON(?ERROR_NOT_FOUND)
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
    AtmWorkflowSchemaData = #{
        <<"name">> => ozt_atm:gen_example_name(),
        <<"summary">> => ozt_atm:gen_example_summary(),

        <<"revision">> => InitialRevisionData = #{
            <<"originalRevisionNumber">> => RevisionNumber,
            <<"schema">> => ozt_atm_workflow_schemas:gen_example_revision_schema_json(AtmInventoryId)
        }
    },
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(?USER(Creator), AtmInventoryId, AtmWorkflowSchemaData),
    AtmWorkflowSchema = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),
    ReferencedAtmLambdas = od_atm_workflow_schema:extract_all_referenced_atm_lambdas(AtmWorkflowSchema),

    ExpectedJsonDump = InitialRevisionData#{
        <<"schemaFormatVersion">> => 2,
        <<"supplementaryAtmLambdas">> => lists:foldl(fun(AtmLambdaId, Acc) ->
            AtmLambda = ozt_atm_lambdas:get(AtmLambdaId),
            Acc#{AtmLambdaId => #{
                <<"schemaFormatVersion">> => 2,

                <<"name">> => AtmLambda#od_atm_lambda.name,
                <<"summary">> => AtmLambda#od_atm_lambda.summary,
                <<"description">> => AtmLambda#od_atm_lambda.description,

                <<"operationSpec">> => jsonable_record:to_json(AtmLambda#od_atm_lambda.operation_spec, atm_lambda_operation_spec),
                <<"argumentSpecs">> => jsonable_record:list_to_json(AtmLambda#od_atm_lambda.argument_specs, atm_lambda_argument_spec),
                <<"resultSpecs">> => jsonable_record:list_to_json(AtmLambda#od_atm_lambda.result_specs, atm_lambda_result_spec),

                <<"resourceSpec">> => jsonable_record:to_json(AtmLambda#od_atm_lambda.resource_spec, atm_resource_spec),

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
            path = [<<"/atm_workflow_schemas/">>, AtmWorkflowSchemaId, <<"/revision/">>, RevisionNumberBin, <<"/dump">>],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpectedJsonDump
        },
        logic_spec = #logic_spec{
            module = atm_workflow_schema_logic,
            function = dump_revision,
            args = [auth, AtmWorkflowSchemaId, RevisionNumber],
            expected_result = ?OK_MAP(ExpectedJsonDump)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{
                type = od_atm_workflow_schema, id = AtmWorkflowSchemaId,
                aspect = {dump_revision, RevisionNumberBin}, scope = private
            },
            expected_result = ?OK_MAP(ExpectedJsonDump)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


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
    OriginalReferencedLambdas = od_atm_workflow_schema:extract_all_referenced_atm_lambdas(
        ozt_atm_workflow_schemas:get(OriginalAtmWorkflowSchemaId)
    ),
    DumpedAtmWorkflowSchema = ozt_atm_workflow_schemas:dump_to_json(OriginalAtmWorkflowSchemaId),

    % recreating the schema from a dump in the same inventory should result in exactly the same schema
    DuplicateAtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(OriginalAtmInventoryId, DumpedAtmWorkflowSchema),
    ?assertNotEqual(OriginalAtmWorkflowSchemaId, DuplicateAtmWorkflowSchemaId),
    ?assert(are_workflow_schema_dumps_equal(
        DumpedAtmWorkflowSchema,
        ozt_atm_workflow_schemas:dump_to_json(DuplicateAtmWorkflowSchemaId)
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
    OriginalReferencedLambdas = od_atm_workflow_schema:extract_all_referenced_atm_lambdas(
        OriginalAtmWorkflowSchema
    ),
    DumpedAtmWorkflowSchema = ozt_atm_workflow_schemas:dump_to_json(OriginalAtmWorkflowSchemaId),

    % 1) exactly the same schema if the creating user has privileges to manage
    %    used lambdas in both inventories (lambdas are linked to the target inventory)
    FirstAtmInventoryId = ozt_users:create_atm_inventory_for(UserAlpha),
    FirstAtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(
        ?USER(UserAlpha), FirstAtmInventoryId, DumpedAtmWorkflowSchema
    ),
    ?assertNotEqual(OriginalAtmWorkflowSchemaId, FirstAtmWorkflowSchemaId),
    ?assert(are_workflow_schema_dumps_equal(
        DumpedAtmWorkflowSchema,
        ozt_atm_workflow_schemas:dump_to_json(FirstAtmWorkflowSchemaId)
    )),
    ?assert(inventory_references_lambdas(FirstAtmInventoryId, OriginalReferencedLambdas)),
    ?assert(workflow_schema_references_lambdas(FirstAtmWorkflowSchemaId, OriginalReferencedLambdas)),

    % 2) a schema referencing duplicated lambdas if the creating user does not have privileges
    %    to manage lambdas in the original inventory, but does in the target inventory
    UserBeta = ozt_users:create(),
    AnotherAtmInventoryId = ozt_users:create_atm_inventory_for(UserBeta),
    SecondAtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(
        ?USER(UserBeta), AnotherAtmInventoryId, DumpedAtmWorkflowSchema
    ),
    ?assertNotEqual(OriginalAtmWorkflowSchemaId, SecondAtmWorkflowSchemaId),
    ?assertNot(are_workflow_schema_dumps_equal(
        ozt_atm_workflow_schemas:dump_to_json(SecondAtmWorkflowSchemaId),
        DumpedAtmWorkflowSchema
    )),

    ?assert(are_workflow_schema_dumps_equal(
        ozt_atm_workflow_schemas:dump_to_json(SecondAtmWorkflowSchemaId),
        ozt_atm_workflow_schemas:dump_to_json(
            OriginalAtmWorkflowSchemaId,
            ozt_atm_workflow_schemas:update_revision_with(
                OriginalAtmWorkflowSchema,
                od_atm_workflow_schema:get_latest_revision_number(OriginalAtmWorkflowSchema),
                fun(AtmWorkflowSchemaRevision) ->
                    ozt_atm_workflow_schemas:substitute_atm_lambdas_for_duplicates(
                        AtmWorkflowSchemaRevision,
                        OriginalReferencedLambdas,
                        AnotherAtmInventoryId
                    )
                end
            )
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
        ozt_atm_workflow_schemas:dump_to_json(ThirdAtmWorkflowSchemaId)
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
        ozt_atm_workflow_schemas:dump_to_json(FourthAtmWorkflowSchemaId)
    )),
    ?assert(are_workflow_schema_dumps_equal(
        ozt_atm_workflow_schemas:dump_to_json(SecondAtmWorkflowSchemaId),
        ozt_atm_workflow_schemas:dump_to_json(FourthAtmWorkflowSchemaId)
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
        ozt_atm_workflow_schemas:dump_to_json(FifthAtmWorkflowSchemaId)
    )),
    ?assert(are_workflow_schema_dumps_equal(
        ozt_atm_workflow_schemas:dump_to_json(SecondAtmWorkflowSchemaId),
        ozt_atm_workflow_schemas:dump_to_json(FifthAtmWorkflowSchemaId)
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


recreate_atm_workflow_schema_with_mixed_linked_and_duplicated_lambdas_test(_Config) ->
    OriginalAtmInventoryId = ozt_atm_inventories:create(),
    AtmLambdaToLink = ozt_atm_lambdas:create(OriginalAtmInventoryId),
    AtmLambdaToDuplicate = ozt_atm_lambdas:create(OriginalAtmInventoryId),
    OriginalAtmWorkflowSchemaId = create_workflow_schema_with_tasks_including_lambdas(
        OriginalAtmInventoryId, [AtmLambdaToLink, AtmLambdaToDuplicate]
    ),
    DumpedAtmWorkflowSchema = ozt_atm_workflow_schemas:dump_to_json(OriginalAtmWorkflowSchemaId),

    Creator = ozt_users:create(),
    TargetInventoryId = ozt_users:create_atm_inventory_for(Creator),
    OtherInventoryId = ozt_users:create_atm_inventory_for(Creator),

    % add the lambda to the other user's inventory so that the user has privileges to link it
    ozt_atm_lambdas:link_to_inventory(AtmLambdaToLink, OtherInventoryId),

    % recreate the workflow schema in the target inventory, which should cause one of
    % the lambdas to be linked, and one to be duplicated
    RecreatedAtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(
        ?USER(Creator), TargetInventoryId, DumpedAtmWorkflowSchema
    ),
    ?assertNotEqual(OriginalAtmWorkflowSchemaId, RecreatedAtmWorkflowSchemaId),
    ?assertNot(are_workflow_schema_dumps_equal(
        ozt_atm_workflow_schemas:dump_to_json(RecreatedAtmWorkflowSchemaId),
        DumpedAtmWorkflowSchema
    )),
    OriginalAtmWorkflowSchema = ozt_atm_workflow_schemas:get(OriginalAtmWorkflowSchemaId),
    ?assert(are_workflow_schema_dumps_equal(
        ozt_atm_workflow_schemas:dump_to_json(RecreatedAtmWorkflowSchemaId),
        ozt_atm_workflow_schemas:dump_to_json(
            OriginalAtmWorkflowSchemaId,
            ozt_atm_workflow_schemas:update_revision_with(
                OriginalAtmWorkflowSchema,
                od_atm_workflow_schema:get_latest_revision_number(OriginalAtmWorkflowSchema),
                fun(AtmWorkflowSchemaRevision) ->
                    ozt_atm_workflow_schemas:substitute_atm_lambdas_for_duplicates(
                        AtmWorkflowSchemaRevision,
                        [AtmLambdaToDuplicate],
                        TargetInventoryId
                    )
                end
            )
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
    DumpedAtmWorkflowSchema = ozt_atm_workflow_schemas:dump_to_json(OriginalAtmWorkflowSchemaId),

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
        maps:update_with(<<"revision">>, fun(InitialRevisionData) ->
            maps:without([<<"supplementaryAtmLambdas">>], InitialRevisionData)
        end, DumpedAtmWorkflowSchema)
    )),

    % providing invalid lambda definitions should cause data validation errors from lambda creation procedures
    ?assertMatch(
        ?ERROR_BAD_DATA(<<"supplementaryAtmLambdas[", _/binary>>, ?ERROR_MISSING_REQUIRED_VALUE(_)),
        ozt_atm_workflow_schemas:try_create(
            ?USER(UserBeta),
            AnotherAtmInventoryId,
            maps:update_with(<<"revision">>, fun(InitialRevisionData) ->
                maps:update_with(<<"supplementaryAtmLambdas">>, fun(SupplementaryAtmLambdas) ->
                    maps:map(fun(_AtmLambdaId, AtmLambdaData) ->
                        % include ONLY the checksum field and drop the actual lambda data
                        maps:with([<<"checksum">>], AtmLambdaData)
                    end, SupplementaryAtmLambdas)
                end, InitialRevisionData)
            end, DumpedAtmWorkflowSchema)
        )
    ).

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
name_summary_bad_data_values() ->
    lists:flatten([
        {<<"summary">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"summary">>)},
        {<<"summary">>, str_utils:rand_hex(315), ?ERROR_BAD_VALUE_BINARY_TOO_LARGE(<<"summary">>, 200)},
        ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
    ]).


%% @private
create_workflow_schema_with_nonempty_tasks(AtmInventoryId) ->
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(AtmInventoryId),
    AtmWorkflowSchema = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),
    case AtmWorkflowSchema#od_atm_workflow_schema.atm_lambdas of
        [] ->
            % workflow schema is randomized and may have empty tasks; repeat if needed
            create_workflow_schema_with_nonempty_tasks(AtmInventoryId);
        _ ->
            AtmWorkflowSchemaId
    end.


%% @private
create_workflow_schema_with_tasks_including_lambdas(AtmInventoryId, TargetAtmLambdas) ->
    AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(AtmInventoryId),
    #od_atm_workflow_schema{atm_lambdas = ReferencedAtmLambdas} = ozt_atm_workflow_schemas:get(AtmWorkflowSchemaId),
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
    maps:without([<<"originalAtmWorkflowSchemaId">>], DumpA) == maps:without([<<"originalAtmWorkflowSchemaId">>], DumpB).


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
