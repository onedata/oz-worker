%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning validation of automation schemas -
%%% workflows and lambdas.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_schema_validation_test_SUITE).
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
-include("ozt.hrl").

-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    atm_lambda_non_unique_config_spec_names/1,
    atm_lambda_non_unique_argument_spec_names/1,
    atm_lambda_non_unique_result_spec_names/1,
    atm_lambda_disallowed_config_parameter_default_value/1,
    atm_lambda_disallowed_argument_default_value/1,
    atm_workflow_schema_non_unique_store_ids/1,
    atm_workflow_schema_reserved_store_id/1,
    atm_workflow_schema_non_unique_lane_ids/1,
    atm_workflow_schema_non_unique_parallel_box_ids/1,
    atm_workflow_schema_non_unique_task_ids/1,
    atm_workflow_schema_disallowed_store_default_initial_content/1,
    atm_workflow_schema_disallowed_iterated_store_type/1,
    atm_workflow_schema_bad_store_reference_in_iterator/1,
    atm_workflow_schema_bad_store_reference_in_argument_value_builder/1,
    atm_workflow_schema_bad_store_reference_in_result_mapper/1,
    atm_workflow_schema_bad_current_task_time_series_store_reference_in_result_mapper/1,
    atm_workflow_schema_bad_lambda_reference_in_task/1,
    atm_workflow_schema_bad_config_parameter_reference_in_lambda_config/1,
    atm_workflow_schema_missing_required_lambda_config_value/1,
    atm_workflow_schema_disallowed_lambda_config_value/1,
    atm_workflow_schema_lambda_argument_mapped_more_than_once/1,
    atm_workflow_schema_bad_argument_reference_in_mapper/1,
    atm_workflow_schema_missing_required_argument_mapper/1,
    atm_workflow_schema_bad_result_reference_in_mapper/1
]).

all() ->
    ?ALL([
        atm_lambda_non_unique_config_spec_names,
        atm_lambda_non_unique_argument_spec_names,
        atm_lambda_non_unique_result_spec_names,
        atm_lambda_disallowed_config_parameter_default_value,
        atm_lambda_disallowed_argument_default_value,
        atm_workflow_schema_non_unique_store_ids,
        atm_workflow_schema_reserved_store_id,
        atm_workflow_schema_non_unique_lane_ids,
        atm_workflow_schema_non_unique_parallel_box_ids,
        atm_workflow_schema_non_unique_task_ids,
        atm_workflow_schema_disallowed_store_default_initial_content,
        atm_workflow_schema_disallowed_iterated_store_type,
        atm_workflow_schema_bad_store_reference_in_iterator,
        atm_workflow_schema_bad_store_reference_in_argument_value_builder,
        atm_workflow_schema_bad_store_reference_in_result_mapper,
        atm_workflow_schema_bad_current_task_time_series_store_reference_in_result_mapper,
        atm_workflow_schema_bad_lambda_reference_in_task,
        atm_workflow_schema_bad_config_parameter_reference_in_lambda_config,
        atm_workflow_schema_missing_required_lambda_config_value,
        atm_workflow_schema_disallowed_lambda_config_value,
        atm_workflow_schema_lambda_argument_mapped_more_than_once,
        atm_workflow_schema_bad_argument_reference_in_mapper,
        atm_workflow_schema_missing_required_argument_mapper,
        atm_workflow_schema_bad_result_reference_in_mapper
    ]).


%% @TODO VFS-7755 test non unique parallel box ids in different lanes
%% @TODO VFS-7755 test non unique task ids in different lanes
%% @TODO VFS-7755 test non unique task ids in different parallel boxes
%% @TODO VFS-7755 test non-batch default value when argument spec's is_batch=true


%% @formatter:off
% Record used to expressively define a schema validation test. Each test tries
% to create/update an atm_lambda/atm_workflow_schema, but with invalid input
% data that should cause validation errors. Firstly, a correct input data set
% is generated, and then one of its fields (tested_data_field) is modified
% using 'spoil_data_field_fun' callback. The callback must return the updated
% data field value and the validation error that it should cause when used to
% create/update a schema.
-record(test_spec, {
    schema_type :: atm_lambda | atm_workflow_schema,
    tested_data_field :: binary(),
    spoil_data_field_fun :: fun((term(), od_atm_inventory:id()) -> {term(), errors:error()})
                          % this variant allows peeking into the full data object apart from the tested data field
                          | fun((term(), entity_logic:data(), od_atm_inventory:id()) -> {term(), errors:error()})
}).
%% @formatter:on

-define(TEST_REPEATS, 10).

%%%===================================================================
%%% Test functions
%%%===================================================================

atm_lambda_non_unique_config_spec_names(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_lambda,
        tested_data_field = <<"configSpec">>,
        spoil_data_field_fun = fun(ConfigSpecJson, _AtmInventoryId) ->
            SpecAlpha = maps:merge(ozt_atm_lambdas:example_parameter_spec_json(), #{<<"name">> => <<"same name">>}),
            SpecBeta = maps:merge(ozt_atm_lambdas:example_parameter_spec_json(), #{<<"name">> => <<"same name">>}),
            {
                lists_utils:shuffle([SpecAlpha, SpecBeta | ConfigSpecJson]),
                ?ERROR_BAD_DATA(
                    <<"configSpec">>,
                    <<"The provided list contains duplicate names">>
                )
            }
        end
    }).


atm_lambda_non_unique_argument_spec_names(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_lambda,
        tested_data_field = <<"argumentSpecs">>,
        spoil_data_field_fun = fun(ArgumentSpecsJson, _AtmInventoryId) ->
            SpecAlpha = maps:merge(ozt_atm_lambdas:example_parameter_spec_json(), #{<<"name">> => <<"same name">>}),
            SpecBeta = maps:merge(ozt_atm_lambdas:example_parameter_spec_json(), #{<<"name">> => <<"same name">>}),
            {
                lists_utils:shuffle([SpecAlpha, SpecBeta | ArgumentSpecsJson]),
                ?ERROR_BAD_DATA(
                    <<"argumentSpecs">>,
                    <<"The provided list contains duplicate names">>
                )
            }
        end
    }).


atm_lambda_non_unique_result_spec_names(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_lambda,
        tested_data_field = <<"resultSpecs">>,
        spoil_data_field_fun = fun(ResultSpecsJson, _AtmInventoryId) ->
            SpecAlpha = maps:merge(ozt_atm_lambdas:example_result_spec_json(), #{<<"name">> => <<"same name">>}),
            SpecBeta = maps:merge(ozt_atm_lambdas:example_result_spec_json(), #{<<"name">> => <<"same name">>}),
            {
                lists_utils:shuffle([SpecAlpha, SpecBeta | ResultSpecsJson]),
                ?ERROR_BAD_DATA(
                    <<"resultSpecs">>,
                    <<"The provided list contains duplicate names">>
                )
            }
        end
    }).


atm_lambda_disallowed_config_parameter_default_value(_Config) ->
    lists:foreach(fun({DataSpec, InvalidDefaultValue}) ->
        OffendingConfigParameterSpec = ozt_atm_lambdas:example_parameter_spec_json(DataSpec, InvalidDefaultValue),
        #{<<"name">> := ConfigParameterName} = OffendingConfigParameterSpec,
        DataKeyName = <<"configSpec[", ConfigParameterName/binary, "].defaultValue">>,
        run_validation_tests(#test_spec{
            schema_type = atm_lambda,
            tested_data_field = <<"configSpec">>,
            spoil_data_field_fun = fun(ConfigSpecsJson, _AtmInventoryId) ->
                {
                    lists_utils:shuffle([OffendingConfigParameterSpec | ConfigSpecsJson]),
                    exp_disallowed_predefined_value_error(DataKeyName, DataSpec, InvalidDefaultValue)
                }
            end
        })
    end, example_invalid_data_specs_and_predefined_values()).


atm_lambda_disallowed_argument_default_value(_Config) ->
    lists:foreach(fun({DataSpec, InvalidDefaultValue}) ->
        OffendingArgumentSpec = ozt_atm_lambdas:example_parameter_spec_json(DataSpec, InvalidDefaultValue),
        #{<<"name">> := ArgumentName} = OffendingArgumentSpec,
        DataKeyName = <<"argumentSpecs[", ArgumentName/binary, "].defaultValue">>,
        run_validation_tests(#test_spec{
            schema_type = atm_lambda,
            tested_data_field = <<"argumentSpecs">>,
            spoil_data_field_fun = fun(ArgumentSpecsJson, _AtmInventoryId) ->
                {
                    lists_utils:shuffle([OffendingArgumentSpec | ArgumentSpecsJson]),
                    exp_disallowed_predefined_value_error(DataKeyName, DataSpec, InvalidDefaultValue)
                }
            end
        })
    end, example_invalid_data_specs_and_predefined_values()).


atm_workflow_schema_non_unique_store_ids(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"stores">>,
        spoil_data_field_fun = fun(StoresJson, _AtmInventoryId) ->
            StoreAlpha = maps:merge(ozt_atm_workflow_schemas:example_store_schema_json(), #{<<"id">> => <<"duplicate id">>}),
            StoreBeta = maps:merge(ozt_atm_workflow_schemas:example_store_schema_json(), #{<<"id">> => <<"duplicate id">>}),
            {
                lists_utils:shuffle([StoreAlpha, StoreBeta | StoresJson]),
                ?ERROR_BAD_DATA(
                    <<"stores">>,
                    <<"The provided list contains duplicate ids">>
                )
            }
        end
    }).


atm_workflow_schema_reserved_store_id(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"stores">>,
        spoil_data_field_fun = fun(StoresJson, _AtmInventoryId) ->
            ReservedStoreId = ?RAND_ELEMENT([
                ?CURRENT_TASK_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID,
                ?WORKFLOW_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID,
                ?CURRENT_TASK_TIME_SERIES_STORE_SCHEMA_ID
            ]),
            BadStore = maps:merge(ozt_atm_workflow_schemas:example_store_schema_json(), #{
                <<"id">> => ReservedStoreId
            }),
            {
                lists_utils:shuffle([BadStore | StoresJson]),
                ?ERROR_BAD_DATA(
                    <<"stores[", ReservedStoreId/binary, "].id">>,
                    <<"The provided store schema Id is reserved and cannot be used in store schema definitions">>
                )
            }
        end
    }).


atm_workflow_schema_non_unique_lane_ids(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := StoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            StoreSchemas = jsonable_record:list_from_json(StoreSchemasJson, atm_store_schema),

            LaneAlpha = maps:merge(
                ozt_atm_workflow_schemas:example_lane_schema_json(AtmLambdas, StoreSchemas),
                #{<<"id">> => <<"duplicateId">>}
            ),
            LaneBeta = maps:merge(
                ozt_atm_workflow_schemas:example_lane_schema_json(AtmLambdas, StoreSchemas),
                #{<<"id">> => <<"duplicateId">>}
            ),
            {
                lists_utils:shuffle([LaneAlpha, LaneBeta | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"lanes">>,
                    <<"The provided list contains duplicate ids">>
                )
            }
        end
    }).


atm_workflow_schema_non_unique_parallel_box_ids(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := StoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            StoreSchemas = jsonable_record:list_from_json(StoreSchemasJson, atm_store_schema),
            StoreSchemaIds = [S#atm_store_schema.id || S <- StoreSchemas],

            PBoxAlpha = (
                ozt_atm_workflow_schemas:example_parallel_box_schema(AtmLambdas, StoreSchemaIds)
            )#atm_parallel_box_schema{id = <<"the same">>},
            PBoxBeta = (
                ozt_atm_workflow_schemas:example_parallel_box_schema(AtmLambdas, StoreSchemaIds)
            )#atm_parallel_box_schema{id = <<"the same">>},

            OffendingLane = ozt_atm_workflow_schemas:example_lane_schema_with_parallel_boxes_json(
                lists_utils:shuffle([
                    PBoxAlpha, PBoxBeta | ozt_atm_workflow_schemas:example_parallel_box_schemas(AtmLambdas, StoreSchemaIds)
                ]),
                StoreSchemas
            ),
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"parallelBoxes">>,
                    <<"The provided list contains duplicate ids">>
                )
            }
        end
    }).


atm_workflow_schema_non_unique_task_ids(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := StoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            StoreSchemaIds = store_schemas_json_to_ids(StoreSchemasJson),

            TaskAlpha = (
                ozt_atm_workflow_schemas:example_task_schema(AtmLambdas, StoreSchemaIds)
            )#atm_task_schema{id = <<"the same">>},
            TaskBeta = (
                ozt_atm_workflow_schemas:example_task_schema(AtmLambdas, StoreSchemaIds)
            )#atm_task_schema{id = <<"the same">>},
            OffendingLane = gen_lane_including_tasks([TaskAlpha, TaskBeta], AtmLambdas, StoreSchemasJson),
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"tasks">>,
                    <<"The provided list contains duplicate ids">>
                )
            }
        end
    }).


atm_workflow_schema_disallowed_store_default_initial_content(_Config) ->
    StoreId = ?RAND_STR(16),
    DataKeyName = <<"stores[", StoreId/binary, "].defaultInitialContent">>,
    lists:foreach(fun({StoreType, StoreConfig, DefaultInitialContent, ExpError}) ->
        OffendingStoreSchema = ozt_atm_workflow_schemas:example_store_schema_json(StoreType, StoreConfig, DefaultInitialContent),
        run_validation_tests(#test_spec{
            schema_type = atm_workflow_schema,
            tested_data_field = <<"stores">>,
            spoil_data_field_fun = fun(StoresJson, _AtmInventoryId) ->
                {
                    lists_utils:shuffle([OffendingStoreSchema#{<<"id">> => StoreId} | StoresJson]),
                    ExpError
                }
            end
        })
    end, example_invalid_stores_and_default_initial_contents(DataKeyName)).


atm_workflow_schema_disallowed_iterated_store_type(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"stores">>,
        spoil_data_field_fun = fun(StoresJson, #{<<"lanes">> := LanesJson}, _AtmInventoryId) ->
            FirstLane = hd(LanesJson),
            #atm_lane_schema{
                id = LaneId,
                store_iterator_spec = #atm_store_iterator_spec{
                    store_schema_id = FirstIteratedStoreId
                }
            } = jsonable_record:from_json(FirstLane, atm_lane_schema),

            DisallowedType = ?RAND_ELEMENT([time_series, audit_log]),
            DisallowedStoreJson = ozt_atm_workflow_schemas:example_store_schema_json(DisallowedType),

            SpoiledStoresJson = lists:filtermap(fun(#{<<"id">> := StoreId} = StoreJson) ->
                case StoreId of
                    FirstIteratedStoreId ->
                        {true, DisallowedStoreJson#{<<"id">> => FirstIteratedStoreId}};
                    _ ->
                        {true, StoreJson}
                end
            end, StoresJson),
            {
                lists_utils:shuffle(SpoiledStoresJson),
                ?ERROR_BAD_DATA(
                    <<"lanes[", LaneId/binary, "].storeIteratorSpec.storeSchemaId">>,
                    <<"Iterating over stores of type '", (automation:store_type_to_json(DisallowedType))/binary, "' is disallowed">>
                )
            }
        end
    }).


atm_workflow_schema_bad_store_reference_in_iterator(_Config) ->
    #atm_store_schema{id = BadStoreSchemaId} = BadStoreSchema = atm_test_utils:example_store_schema(
        ?RAND_ELEMENT(automation:all_store_types() -- [time_series, audit_log])
    ),
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            OffendingLane = ozt_atm_workflow_schemas:example_lane_schema_json(AtmLambdas, [BadStoreSchema]),
            #{<<"id">> := LaneId} = OffendingLane,
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"lanes[", LaneId/binary, "].storeIteratorSpec.storeSchemaId">>,
                    <<"The provided storeSchemaId = '", BadStoreSchemaId/binary, "' was not found among defined store schemas">>
                )
            }
        end
    }).


atm_workflow_schema_bad_store_reference_in_argument_value_builder(_Config) ->
    BadStoreSchemaId = <<"another invalid store schema id">>,
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := CorrectStoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            CorrectStoreSchemaIds = store_schemas_json_to_ids(CorrectStoreSchemasJson),

            AtmLambdaRevisionJson = ozt_atm_lambdas:example_revision_json(),
            ReferencedArgumentSpec = #{<<"name">> := OffendingArgumentName} = ozt_atm_lambdas:example_parameter_spec_json(),
            OtherArgumentSpecs = maps:get(<<"argumentSpecs">>, AtmLambdaRevisionJson),
            AtmLambdaId = create_lambda_with_revision(AtmInventoryId, AtmLambdaRevisionJson#{
                <<"argumentSpecs">> => lists_utils:shuffle([ReferencedArgumentSpec | OtherArgumentSpecs])
            }),
            AtmLambdaRevision = ozt_atm_lambdas:get_revision_with_largest_number(AtmLambdaId),

            CorrectArgumentMappings = atm_test_utils:example_argument_mappers_for_specs(
                jsonable_record:list_from_json(OtherArgumentSpecs, atm_parameter_spec), CorrectStoreSchemaIds
            ),
            OffendingArgumentMapping = #atm_task_schema_argument_mapper{
                argument_name = maps:get(<<"name">>, ReferencedArgumentSpec),
                value_builder = #atm_task_argument_value_builder{
                    type = store_credentials, recipe = BadStoreSchemaId
                }
            },

            OffendingTask = #atm_task_schema{
                id = TaskId = atm_test_utils:example_id(),
                name = atm_test_utils:example_name(),
                lambda_id = AtmLambdaId,
                lambda_revision_number = ozt_atm_lambdas:get_largest_revision_number(AtmLambdaId),
                lambda_config = gen_lambda_config(AtmLambdaRevision),
                argument_mappings = lists_utils:shuffle([OffendingArgumentMapping | CorrectArgumentMappings]),
                result_mappings = atm_test_utils:example_result_mappers(AtmLambdaRevision, CorrectStoreSchemaIds)
            },
            OffendingLane = gen_lane_including_tasks([OffendingTask], AtmLambdas, CorrectStoreSchemasJson),
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"tasks[", TaskId/binary, "].argumentMappings[", OffendingArgumentName/binary, "].valueBuilder.recipe">>,
                    <<"The provided storeSchemaId = '", BadStoreSchemaId/binary, "' was not found among defined store schemas">>
                )
            }
        end
    }).


atm_workflow_schema_bad_store_reference_in_result_mapper(_Config) ->
    BadStoreSchemaId = <<"another invalid store schema id">>,
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := StoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            CorrectStoreSchemaIds = store_schemas_json_to_ids(StoreSchemasJson),

            OffendingResultName = atm_test_utils:example_name(),
            {AtmLambdaId, AtmLambdaRevision} = create_random_lambda_with_result_name(AtmInventoryId, OffendingResultName),

            OffendingTask = #atm_task_schema{id = TaskId} = gen_task_with_result_mapping(
                AtmLambdaId, AtmLambdaRevision, CorrectStoreSchemaIds, #atm_task_schema_result_mapper{
                    result_name = OffendingResultName,
                    store_schema_id = [BadStoreSchemaId],
                    store_content_update_options = ?RAND_ELEMENT(atm_test_utils:example_store_content_update_options_records())
                }
            ),
            OffendingLane = gen_lane_including_tasks([OffendingTask], AtmLambdas, StoreSchemasJson),
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"tasks[", TaskId/binary, "].resultMappings[", OffendingResultName/binary, "].storeSchemaId">>,
                    <<"The provided storeSchemaId = '", BadStoreSchemaId/binary, "' was not found among defined store schemas">>
                )
            }
        end
    }).


atm_workflow_schema_bad_current_task_time_series_store_reference_in_result_mapper(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := StoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            CorrectStoreSchemaIds = store_schemas_json_to_ids(StoreSchemasJson),

            OffendingResultName = atm_test_utils:example_name(),
            {AtmLambdaId, AtmLambdaRevision} = create_random_lambda_with_result_name(AtmInventoryId, OffendingResultName),

            ExampleTask = #atm_task_schema{id = TaskId} = gen_task_with_result_mapping(
                AtmLambdaId, AtmLambdaRevision, CorrectStoreSchemaIds, #atm_task_schema_result_mapper{
                    result_name = OffendingResultName,
                    store_schema_id = ?CURRENT_TASK_TIME_SERIES_STORE_SCHEMA_ID,
                    store_content_update_options = ?RAND_ELEMENT(atm_test_utils:example_store_content_update_options_records())
                }
            ),
            OffendingTask = ExampleTask#atm_task_schema{
                time_series_store_config = undefined
            },
            OffendingLane = gen_lane_including_tasks([OffendingTask], AtmLambdas, StoreSchemasJson),
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"tasks[", TaskId/binary, "].resultMappings[", OffendingResultName/binary, "].storeSchemaId">>,
                    <<"The time series store for current task cannot be referenced if its config is not defined in the task.">>
                )
            }
        end
    }).


atm_workflow_schema_bad_lambda_reference_in_task(_Config) ->
    AnotherInventoryId = ozt_atm_inventories:create(),
    BadLambdaId = ozt_atm_lambdas:create(AnotherInventoryId),
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := CorrectStoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            CorrectStoreSchemaIds = store_schemas_json_to_ids(CorrectStoreSchemasJson),

            OffendingTask = ozt_atm_workflow_schemas:example_task_schema([BadLambdaId], CorrectStoreSchemaIds),
            OffendingLane = gen_lane_including_tasks([OffendingTask], AtmLambdas, CorrectStoreSchemasJson),
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"tasks">>,
                    <<"The lambda id '", BadLambdaId/binary, "' referenced by one of the tasks was not found or is "
                    "not available for the requesting client. Consider providing supplementary "
                    "lambdas so that missing ones can be linked or created along with the workflow schema "
                    "(however, this requires lambda management privileges in the target inventory).">>
                )
            }
        end
    }).


atm_workflow_schema_bad_config_parameter_reference_in_lambda_config(_Config) ->
    BadParameterName1 = <<"bad parameter name">>,
    BadParameterName2 = <<"invalid parameter name">>,
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := CorrectStoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            CorrectStoreSchemaIds = store_schemas_json_to_ids(CorrectStoreSchemasJson),
            AtmLambdaId = ?RAND_ELEMENT(AtmLambdas),
            AtmLambdaRevision = ozt_atm_lambdas:get_revision_with_largest_number(AtmLambdaId),

            CorrectLambdaConfigEntries = gen_lambda_config(AtmLambdaRevision),
            OffendingLambdaConfigEntries = gen_lambda_config([BadParameterName1, BadParameterName2]),

            OffendingTask = #atm_task_schema{
                id = TaskId = atm_test_utils:example_id(),
                name = atm_test_utils:example_name(),
                lambda_id = AtmLambdaId,
                lambda_revision_number = ozt_atm_lambdas:get_largest_revision_number(AtmLambdaId),
                lambda_config = maps:merge(OffendingLambdaConfigEntries, CorrectLambdaConfigEntries),
                argument_mappings = atm_test_utils:example_argument_mappers(AtmLambdaRevision, CorrectStoreSchemaIds),
                result_mappings = atm_test_utils:example_result_mappers(AtmLambdaRevision, CorrectStoreSchemaIds)
            },
            OffendingLane = gen_lane_including_tasks([OffendingTask], AtmLambdas, CorrectStoreSchemasJson),
            OffendingNamesStr = <<BadParameterName1/binary, ", ", BadParameterName2/binary>>,
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"tasks[", TaskId/binary, "].lambdaConfig">>,
                    <<"The following names were not recognized (they reference inexistent definitions): ", OffendingNamesStr/binary>>
                )
            }
        end
    }).


atm_workflow_schema_missing_required_lambda_config_value(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := CorrectStoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            CorrectStoreSchemaIds = store_schemas_json_to_ids(CorrectStoreSchemasJson),

            AtmLambdaRevisionJson = ozt_atm_lambdas:example_revision_json(),
            MissingConfigParameterSpecJson = #{<<"name">> := MissingParameterName} = maps:merge(
                ozt_atm_lambdas:example_parameter_spec_json(),
                #{
                    <<"isOptional">> => false,
                    <<"defaultValue">> => null
                }
            ),
            OtherConfigParameterSpecsJson = maps:get(<<"configSpec">>, AtmLambdaRevisionJson),
            AtmLambdaId = create_lambda_with_revision(AtmInventoryId, AtmLambdaRevisionJson#{
                <<"configSpec">> => lists_utils:shuffle([MissingConfigParameterSpecJson | OtherConfigParameterSpecsJson])
            }),
            AtmLambdaRevision = ozt_atm_lambdas:get_revision_with_largest_number(AtmLambdaId),

            LambdaConfigWithPresentNames = gen_lambda_config(jsonable_record:list_from_json(
                OtherConfigParameterSpecsJson, atm_parameter_spec
            )),

            OffendingTask = #atm_task_schema{
                id = atm_test_utils:example_id(),
                name = atm_test_utils:example_name(),
                lambda_id = AtmLambdaId,
                lambda_revision_number = ozt_atm_lambdas:get_largest_revision_number(AtmLambdaId),
                lambda_config = LambdaConfigWithPresentNames,
                argument_mappings = atm_test_utils:example_argument_mappers(AtmLambdaRevision, CorrectStoreSchemaIds),
                result_mappings = atm_test_utils:example_result_mappers(AtmLambdaRevision, CorrectStoreSchemaIds)
            },
            OffendingLane = gen_lane_including_tasks([OffendingTask], AtmLambdas, CorrectStoreSchemasJson),
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"lambdaConfig">>,
                    <<"Missing value for required lambda config parameter '", MissingParameterName/binary, "'">>
                )
            }
        end
    }).


atm_workflow_schema_disallowed_lambda_config_value(_Config) ->
    lists:foreach(fun({DataSpec, InvalidValue}) ->
        OffendingConfigParameterSpec = ozt_atm_lambdas:example_parameter_spec_json(DataSpec, undefined),
        #{<<"name">> := ConfigParameterName} = OffendingConfigParameterSpec,
        run_validation_tests(#test_spec{
            schema_type = atm_workflow_schema,
            tested_data_field = <<"lanes">>,
            spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := CorrectStoreSchemasJson}, AtmInventoryId) ->
                AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
                CorrectStoreSchemaIds = store_schemas_json_to_ids(CorrectStoreSchemasJson),

                AtmLambdaRevisionJson = ozt_atm_lambdas:example_revision_json(),
                OtherConfigParameterSpecsJson = maps:get(<<"configSpec">>, AtmLambdaRevisionJson),
                AtmLambdaId = create_lambda_with_revision(AtmInventoryId, AtmLambdaRevisionJson#{
                    <<"configSpec">> => lists_utils:shuffle([OffendingConfigParameterSpec | OtherConfigParameterSpecsJson])
                }),
                AtmLambdaRevision = ozt_atm_lambdas:get_revision_with_largest_number(AtmLambdaId),

                CorrectLambdaConfig = gen_lambda_config(jsonable_record:list_from_json(
                    OtherConfigParameterSpecsJson, atm_parameter_spec
                )),
                OffendingLambdaConfig = CorrectLambdaConfig#{ConfigParameterName => InvalidValue},

                OffendingTask = #atm_task_schema{
                    id = TaskId = atm_test_utils:example_id(),
                    name = atm_test_utils:example_name(),
                    lambda_id = AtmLambdaId,
                    lambda_revision_number = ozt_atm_lambdas:get_largest_revision_number(AtmLambdaId),
                    lambda_config = OffendingLambdaConfig,
                    argument_mappings = atm_test_utils:example_argument_mappers(AtmLambdaRevision, CorrectStoreSchemaIds),
                    result_mappings = atm_test_utils:example_result_mappers(AtmLambdaRevision, CorrectStoreSchemaIds)
                },
                OffendingLane = gen_lane_including_tasks([OffendingTask], AtmLambdas, CorrectStoreSchemasJson),
                DataKeyName = <<"tasks[", TaskId/binary, "].lambdaConfig[", ConfigParameterName/binary, "]">>,
                {
                    lists_utils:shuffle([OffendingLane | LanesJson]),
                    exp_disallowed_predefined_value_error(DataKeyName, DataSpec, InvalidValue)
                }
            end
        })
    end, example_invalid_data_specs_and_predefined_values()).


atm_workflow_schema_lambda_argument_mapped_more_than_once(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := CorrectStoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            CorrectStoreSchemaIds = store_schemas_json_to_ids(CorrectStoreSchemasJson),

            AtmLambdaRevisionJson = ozt_atm_lambdas:example_revision_json(),
            TwiceMappedArgumentSpec = ozt_atm_lambdas:example_parameter_spec_json(),
            AllArgumentSpecs = [TwiceMappedArgumentSpec | maps:get(<<"argumentSpecs">>, AtmLambdaRevisionJson)],
            AtmLambdaId = create_lambda_with_revision(AtmInventoryId, AtmLambdaRevisionJson#{
                <<"argumentSpecs">> => lists_utils:shuffle(AllArgumentSpecs)
            }),
            AtmLambdaRevision = ozt_atm_lambdas:get_revision_with_largest_number(AtmLambdaId),

            CorrectArgumentMappings = atm_test_utils:example_argument_mappers_for_specs(
                jsonable_record:list_from_json(AllArgumentSpecs, atm_parameter_spec), CorrectStoreSchemaIds
            ),
            OffendingArgumentMapping = #atm_task_schema_argument_mapper{
                argument_name = maps:get(<<"name">>, TwiceMappedArgumentSpec),
                value_builder = atm_test_utils:example_argument_value_builder(CorrectStoreSchemaIds)
            },

            OffendingTask = #atm_task_schema{
                id = TaskId = atm_test_utils:example_id(),
                name = atm_test_utils:example_name(),
                lambda_id = AtmLambdaId,
                lambda_revision_number = ozt_atm_lambdas:get_largest_revision_number(AtmLambdaId),
                lambda_config = gen_lambda_config(AtmLambdaRevision),
                argument_mappings = lists_utils:shuffle([OffendingArgumentMapping | CorrectArgumentMappings]),
                result_mappings = atm_test_utils:example_result_mappers(AtmLambdaRevision, CorrectStoreSchemaIds)
            },
            OffendingLane = gen_lane_including_tasks([OffendingTask], AtmLambdas, CorrectStoreSchemasJson),
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"tasks[", TaskId/binary, "].argumentMappings">>,
                    <<"The provided list contains duplicate names">>
                )
            }
        end
    }).


atm_workflow_schema_bad_argument_reference_in_mapper(_Config) ->
    BadArgumentName1 = <<"bad argument name">>,
    BadArgumentName2 = <<"invalid argument name">>,
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := CorrectStoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            CorrectStoreSchemaIds = store_schemas_json_to_ids(CorrectStoreSchemasJson),
            AtmLambdaId = ?RAND_ELEMENT(AtmLambdas),
            AtmLambdaRevision = ozt_atm_lambdas:get_revision_with_largest_number(AtmLambdaId),

            CorrectArgumentMappings = atm_test_utils:example_argument_mappers(
                AtmLambdaRevision, CorrectStoreSchemaIds
            ),
            OffendingArgumentMappings = lists:map(fun(ArgumentName) ->
                #atm_task_schema_argument_mapper{
                    argument_name = ArgumentName,
                    value_builder = atm_test_utils:example_argument_value_builder(CorrectStoreSchemaIds)
                }
            end, [BadArgumentName1, BadArgumentName2]),

            OffendingTask = #atm_task_schema{
                id = TaskId = atm_test_utils:example_id(),
                name = atm_test_utils:example_name(),
                lambda_id = AtmLambdaId,
                lambda_revision_number = ozt_atm_lambdas:get_largest_revision_number(AtmLambdaId),
                lambda_config = gen_lambda_config(AtmLambdaRevision),
                argument_mappings = lists_utils:shuffle(OffendingArgumentMappings ++ CorrectArgumentMappings),
                result_mappings = atm_test_utils:example_result_mappers(AtmLambdaRevision, CorrectStoreSchemaIds)
            },
            OffendingLane = gen_lane_including_tasks([OffendingTask], AtmLambdas, CorrectStoreSchemasJson),
            OffendingNamesStr = <<BadArgumentName1/binary, ", ", BadArgumentName2/binary>>,
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"tasks[", TaskId/binary, "].argumentMappings">>,
                    <<"The following names were not recognized (they reference inexistent definitions): ", OffendingNamesStr/binary>>
                )
            }
        end
    }).


atm_workflow_schema_missing_required_argument_mapper(_Config) ->
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := CorrectStoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            CorrectStoreSchemaIds = store_schemas_json_to_ids(CorrectStoreSchemasJson),

            AtmLambdaRevisionJson = ozt_atm_lambdas:example_revision_json(),
            MissingArgumentSpec = #{<<"name">> := MissingArgumentName} = maps:merge(
                ozt_atm_lambdas:example_parameter_spec_json(),
                #{
                    <<"isOptional">> => false,
                    <<"defaultValue">> => null
                }
            ),
            OtherArgumentSpecs = maps:get(<<"argumentSpecs">>, AtmLambdaRevisionJson),
            AtmLambdaId = create_lambda_with_revision(AtmInventoryId, AtmLambdaRevisionJson#{
                <<"argumentSpecs">> => lists_utils:shuffle([MissingArgumentSpec | OtherArgumentSpecs])
            }),
            AtmLambdaRevision = ozt_atm_lambdas:get_revision_with_largest_number(AtmLambdaId),

            PresentArgumentMappings = atm_test_utils:example_argument_mappers_for_specs(
                jsonable_record:list_from_json(OtherArgumentSpecs, atm_parameter_spec), CorrectStoreSchemaIds
            ),

            OffendingTask = #atm_task_schema{
                id = atm_test_utils:example_id(),
                name = atm_test_utils:example_name(),
                lambda_id = AtmLambdaId,
                lambda_revision_number = ozt_atm_lambdas:get_largest_revision_number(AtmLambdaId),
                lambda_config = gen_lambda_config(AtmLambdaRevision),
                argument_mappings = lists_utils:shuffle(PresentArgumentMappings),
                result_mappings = atm_test_utils:example_result_mappers(AtmLambdaRevision, CorrectStoreSchemaIds)
            },
            OffendingLane = gen_lane_including_tasks([OffendingTask], AtmLambdas, CorrectStoreSchemasJson),
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"argumentMappings">>,
                    <<"Missing argument mapper for required argument '", MissingArgumentName/binary, "'">>
                )
            }
        end
    }).


atm_workflow_schema_bad_result_reference_in_mapper(_Config) ->
    BadResultName1 = <<"bad result name">>,
    BadResultName2 = <<"invalid result name">>,
    BadResultName3 = <<"wrong result name">>,
    run_validation_tests(#test_spec{
        schema_type = atm_workflow_schema,
        tested_data_field = <<"lanes">>,
        spoil_data_field_fun = fun(LanesJson, #{<<"stores">> := CorrectStoreSchemasJson}, AtmInventoryId) ->
            AtmLambdas = ozt_atm_inventories:get_atm_lambdas(AtmInventoryId),
            CorrectStoreSchemaIds = store_schemas_json_to_ids(CorrectStoreSchemasJson),
            AtmLambdaId = ?RAND_ELEMENT(AtmLambdas),
            AtmLambdaRevision = ozt_atm_lambdas:get_revision_with_largest_number(AtmLambdaId),

            CorrectResultMappings = atm_test_utils:example_result_mappers(
                AtmLambdaRevision, CorrectStoreSchemaIds
            ),
            OffendingResultMappings = lists:map(fun(ResultName) ->
                #atm_task_schema_result_mapper{
                    result_name = ResultName,
                    store_schema_id = ?RAND_ELEMENT(CorrectStoreSchemaIds),
                    store_content_update_options = ?RAND_ELEMENT(atm_test_utils:example_store_content_update_options_records())
                }
            end, [BadResultName1, BadResultName2, BadResultName3]),

            OffendingTask = #atm_task_schema{
                id = TaskId = atm_test_utils:example_id(),
                name = atm_test_utils:example_name(),
                lambda_id = AtmLambdaId,
                lambda_revision_number = ozt_atm_lambdas:get_largest_revision_number(AtmLambdaId),
                lambda_config = gen_lambda_config(AtmLambdaRevision),
                argument_mappings = atm_test_utils:example_argument_mappers(AtmLambdaRevision, CorrectStoreSchemaIds),
                result_mappings = lists_utils:shuffle(OffendingResultMappings ++ CorrectResultMappings)
            },
            OffendingLane = gen_lane_including_tasks([OffendingTask], AtmLambdas, CorrectStoreSchemasJson),
            OffendingNamesStr = <<BadResultName1/binary, ", ", BadResultName2/binary, ", ", BadResultName3/binary>>,
            {
                lists_utils:shuffle([OffendingLane | LanesJson]),
                ?ERROR_BAD_DATA(
                    <<"tasks[", TaskId/binary, "].resultMappings">>,
                    <<"The following names were not recognized (they reference inexistent definitions): ", OffendingNamesStr/binary>>
                )
            }
        end
    }).

%%%===================================================================
%%% Helper functions
%%%===================================================================

run_validation_tests(TestSpec) ->
    lists:foreach(fun(_) ->
        try
            run_validation_test(TestSpec)
        catch Class:Reason:Stacktrace ->
            ct:pal("Validation test crashed due to ~p:~p~nStacktrace: ~s", [
                Class, Reason, lager:pr_stacktrace(Stacktrace)
            ]),
            error(fail)
        end
    end, lists:seq(1, ?TEST_REPEATS)).


%% @private
run_validation_test(#test_spec{schema_type = atm_lambda} = TestSpec) ->
    UserId = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(UserId),
    CorrectAtmLambdaData = ozt_atm_lambdas:example_data_json(),
    CorrectRevisionData = maps:get(<<"revision">>, CorrectAtmLambdaData),
    CorrectAtmLambdaRevisionJson = maps:get(<<"atmLambdaRevision">>, CorrectRevisionData),
    {InvalidAtmLambdaRevisionJson, ExpectedError} = spoil_data_field(
        TestSpec, CorrectAtmLambdaRevisionJson, AtmInventoryId
    ),
    InvalidAtmLambdaData = kv_utils:update_with([<<"revision">>, <<"atmLambdaRevision">>], fun(_) ->
        InvalidAtmLambdaRevisionJson
    end, CorrectAtmLambdaData),
    ?assertEqual(ExpectedError, ozt_atm_lambdas:try_create(
        ?USER(UserId), AtmInventoryId, InvalidAtmLambdaData
    )),

    % inserting an invalid revision into a lambda should fail as well
    AtmLambdaId = ozt_atm_lambdas:create(?USER(UserId), AtmInventoryId, CorrectAtmLambdaData),
    ?assertEqual(ExpectedError, ozt_atm_lambdas:try_add_revision(
        ?USER(UserId), AtmLambdaId, integer_to_binary(?RAND_INT(1, 100)),
        maps:get(<<"revision">>, InvalidAtmLambdaData)
    ));
run_validation_test(#test_spec{schema_type = atm_workflow_schema} = TestSpec) ->
    UserId = ozt_users:create(),
    AtmInventoryId = ozt_users:create_atm_inventory_for(UserId),

    CorrectAtmWorkflowSchemaData = ozt_atm_workflow_schemas:example_data_json(AtmInventoryId),
    InitialRevisionData = maps:get(<<"revision">>, CorrectAtmWorkflowSchemaData),
    % repeat test data generation as needed to make sure that at least one store
    % and one lane is generated - otherwise, validation tests do not make sense
    case maps:get(<<"atmWorkflowSchemaRevision">>, InitialRevisionData) of
        #{<<"stores">> := []} ->
            run_validation_test(TestSpec);
        #{<<"lanes">> := []} ->
            run_validation_test(TestSpec);
        CorrectAtmWorkflowSchemaRevisionJson ->
            {InvalidAtmWorkflowSchemaRevisionJson, ExpectedError} = spoil_data_field(
                TestSpec, CorrectAtmWorkflowSchemaRevisionJson, AtmInventoryId
            ),

            InvalidAtmWorkflowSchemaData = kv_utils:update_with([<<"revision">>, <<"atmWorkflowSchemaRevision">>], fun(_) ->
                InvalidAtmWorkflowSchemaRevisionJson
            end, CorrectAtmWorkflowSchemaData),

            ?assertEqual(ExpectedError, ozt_atm_workflow_schemas:try_create(
                ?USER(UserId), AtmInventoryId, InvalidAtmWorkflowSchemaData
            )),

            % merging a valid schema with invalid one should fail as well
            AtmWorkflowSchemaId = ozt_atm_workflow_schemas:create(
                ?USER(UserId), AtmInventoryId, CorrectAtmWorkflowSchemaData
            ),
            ?assertEqual(ExpectedError, ozt_atm_workflow_schemas:try_update(
                ?USER(UserId), AtmWorkflowSchemaId, InvalidAtmWorkflowSchemaData
            )),

            % inserting an invalid revision into a workflow schema should fail as well
            ?assertEqual(ExpectedError, ozt_atm_workflow_schemas:try_insert_revision(
                ?USER(UserId), AtmWorkflowSchemaId, integer_to_binary(?RAND_INT(1, 100)),
                maps:get(<<"revision">>, InvalidAtmWorkflowSchemaData)
            ))
    end.


%% @private
spoil_data_field(#test_spec{
    tested_data_field = TestedDataField,
    spoil_data_field_fun = SpoilDataFieldFun
}, Data, AtmInventoryId) ->
    OriginalFieldValue = maps:get(TestedDataField, Data),
    {SpoiledDataFieldValue, ExpectedError} = case SpoilDataFieldFun of
        F when is_function(F, 2) -> SpoilDataFieldFun(OriginalFieldValue, AtmInventoryId);
        F when is_function(F, 3) -> SpoilDataFieldFun(OriginalFieldValue, Data, AtmInventoryId)
    end,
    {Data#{TestedDataField => SpoiledDataFieldValue}, ExpectedError}.


%% @private
example_invalid_data_specs_and_predefined_values() ->
    [
        {#atm_data_spec{type = atm_boolean_type}, [true, 157]},
        {#atm_data_spec{type = atm_number_type}, [#{<<"obj1">> => <<"val">>}, #{<<"obj2">> => <<"val">>}]},
        {#atm_data_spec{type = atm_string_type}, 167.87},
        {#atm_data_spec{type = atm_object_type}, <<"text">>},
        {#atm_data_spec{
            type = atm_time_series_measurement_type,
            value_constraints = #{specs => lists_utils:random_sublist(atm_test_utils:example_time_series_measurement_specs())}
        }, #{<<"key">> => <<"val">>}},
        {#atm_data_spec{type = atm_file_type}, -9},
        {#atm_data_spec{
            type = atm_array_type,
            value_constraints = #{item_data_spec => #atm_data_spec{type = atm_string_type}}
        }, <<"string">>},
        {#atm_data_spec{
            type = atm_array_type,
            value_constraints = #{item_data_spec => #atm_data_spec{type = atm_string_type}}
        }, [123456]},
        {#atm_data_spec{
            type = atm_array_type,
            value_constraints = #{item_data_spec => #atm_data_spec{
                type = atm_array_type,
                value_constraints = #{item_data_spec => #atm_data_spec{type = atm_string_type}}
            }}
        }, [[<<"string">>, <<"string">>], #{<<"not-a">> => <<"list">>}]},
        {#atm_data_spec{
            type = atm_array_type,
            value_constraints = #{item_data_spec => #atm_data_spec{
                type = atm_array_type,
                value_constraints = #{item_data_spec => #atm_data_spec{type = atm_string_type}}
            }}
        }, [[<<"string">>, <<"string">>], [#{<<"not-a">> => <<"string">>}]]}
    ].


%% @private
example_invalid_stores_and_default_initial_contents(DataKeyName) ->
    lists:flatmap(fun(StoreType) ->
        example_invalid_default_initial_contents_for_store(DataKeyName, StoreType)
    end, automation:all_store_types()).


%% @private
example_invalid_default_initial_contents_for_store(DataKeyName, single_value) ->
    lists:map(fun({DataSpec, InvalidPredefinedValue}) ->
        {
            single_value,
            #atm_single_value_store_config{item_data_spec = DataSpec},
            InvalidPredefinedValue,
            exp_disallowed_predefined_value_error(DataKeyName, DataSpec, InvalidPredefinedValue)
        }
    end, example_invalid_data_specs_and_predefined_values());
example_invalid_default_initial_contents_for_store(DataKeyName, list) ->
    lists:flatmap(fun({DataSpec, InvalidPredefinedValue}) ->
        lists:flatten([
            case is_list(InvalidPredefinedValue) of
                false ->
                    {
                        list,
                        #atm_list_store_config{item_data_spec = DataSpec},
                        InvalidPredefinedValue,
                        ?ERROR_BAD_DATA(
                            DataKeyName,
                            <<"List store requires default initial content to be an array of values">>
                        )
                    };
                true ->
                    []
            end,
            {
                list,
                #atm_list_store_config{item_data_spec = DataSpec},
                [InvalidPredefinedValue],
                exp_disallowed_predefined_value_error(DataKeyName, DataSpec, InvalidPredefinedValue)
            }
        ])
    end, example_invalid_data_specs_and_predefined_values());
example_invalid_default_initial_contents_for_store(DataKeyName, tree_forest) ->
    lists:flatmap(fun({DataSpec = #atm_data_spec{type = DataType}, InvalidPredefinedValue}) ->
        case DataType == atm_file_type orelse DataType == atm_dataset_type of
            true ->
                lists:flatten([
                    case is_list(InvalidPredefinedValue) of
                        false ->
                            {
                                tree_forest,
                                #atm_tree_forest_store_config{item_data_spec = DataSpec},
                                InvalidPredefinedValue,
                                ?ERROR_BAD_DATA(
                                    DataKeyName,
                                    <<"Tree forest store requires default initial content to be an array of values">>
                                )
                            };
                        true ->
                            []
                    end,
                    {
                        tree_forest,
                        #atm_tree_forest_store_config{item_data_spec = DataSpec},
                        [InvalidPredefinedValue],
                        exp_disallowed_predefined_value_error(DataKeyName, DataSpec, InvalidPredefinedValue)
                    }
                ]);
            false ->
                [{
                    tree_forest,
                    #atm_tree_forest_store_config{item_data_spec = DataSpec},
                    InvalidPredefinedValue,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(
                        <<"treeForestStoreConfig.dataSpec.type">>,
                        [atm_data_type:type_to_json(T) || T <- [atm_file_type, atm_dataset_type]]
                    )
                }]
        end
    end, example_invalid_data_specs_and_predefined_values());
example_invalid_default_initial_contents_for_store(DataKeyName, range) ->
    lists:map(fun({_DataSpec, InvalidPredefinedValue}) ->
        {
            range,
            #atm_range_store_config{},
            InvalidPredefinedValue,
            ?ERROR_BAD_DATA(
                DataKeyName,
                <<"Range store requires default initial content to be an object with the following fields: "
                "\"end\" (required), \"start\" (optional), \"step\" (optional)">>
            )
        }
    end, example_invalid_data_specs_and_predefined_values());
% time_series and audit_log stores have the default initial content implicitly set to undefined
example_invalid_default_initial_contents_for_store(_DataKeyName, time_series) ->
    [];
example_invalid_default_initial_contents_for_store(_DataKeyName, audit_log) ->
    [].


%% @private
exp_disallowed_predefined_value_error(DataKeyName, #atm_data_spec{type = atm_store_credentials_type}, _) ->
    ?ERROR_BAD_DATA(DataKeyName, <<"Predefined value for store credentials is disallowed">>);
exp_disallowed_predefined_value_error(DataKeyName, #atm_data_spec{type = atm_array_type} = AtmDataSpec, Values) ->
    case is_list(Values) of
        false ->
            ?ERROR_BAD_DATA(
                DataKeyName,
                <<"The provided predefined value for type 'array' must be an array of values">>
            );
        true ->
            #atm_data_spec{
                type = NestedItemDataType
            } = NestedItemDataSpec = maps:get(item_data_spec, AtmDataSpec#atm_data_spec.value_constraints),
            {ok, ExpError} = lists_utils:searchmap(fun({Index, Value}) ->
                case atm_data_type:is_instance(NestedItemDataType, Value) of
                    true when NestedItemDataType =/= atm_array_type ->
                        false;
                    _ ->
                        try
                            {true, exp_disallowed_predefined_value_error(
                                <<DataKeyName/binary, "[", (integer_to_binary(Index - 1))/binary, "]">>,
                                NestedItemDataSpec,
                                Value
                            )}
                        catch _:_ ->
                            % currently checked Value may be okay in case of array type
                            % in such case, keep searching
                            false
                        end
                end
            end, lists_utils:enumerate(Values)),
            ExpError
    end;
exp_disallowed_predefined_value_error(DataKeyName, #atm_data_spec{type = DataType}, _) ->
    ?ERROR_BAD_DATA(
        DataKeyName,
        <<"The provided predefined value is invalid for type '", (atm_data_type:type_to_json(DataType))/binary, "'">>
    ).


create_lambda_with_revision(AtmInventoryId, AtmLambdaRevisionJson) ->
    ozt_atm_lambdas:create(AtmInventoryId, #{
        <<"revision">> => #{
            <<"originalRevisionNumber">> => ?RAND_INT(1, 100),
            <<"atmLambdaRevision">> => AtmLambdaRevisionJson
        }
    }).


%% @private
create_random_lambda_with_result_name(AtmInventoryId, OffendingResultName) ->
    AtmLambdaRevisionJson = ozt_atm_lambdas:example_revision_json(),
    ResultSpec = maps:merge(ozt_atm_lambdas:example_result_spec_json(), #{<<"name">> => OffendingResultName}),
    OtherResultSpecs = maps:get(<<"resultSpecs">>, AtmLambdaRevisionJson),
    AtmLambdaId = create_lambda_with_revision(AtmInventoryId, AtmLambdaRevisionJson#{
        <<"resultSpecs">> => lists_utils:shuffle([ResultSpec | OtherResultSpecs])
    }),
    AtmLambdaRevision = ozt_atm_lambdas:get_revision_with_largest_number(AtmLambdaId),
    {AtmLambdaId, AtmLambdaRevision}.


%% @private
gen_task_with_result_mapping(AtmLambdaId, AtmLambdaRevision, CorrectStoreSchemaIds, ResultMapping) ->
    OtherResultMappings = atm_test_utils:example_result_mappers_for_specs(
        AtmLambdaRevision#atm_lambda_revision.result_specs, CorrectStoreSchemaIds
    ),
    #atm_task_schema{
        id = atm_test_utils:example_id(),
        name = atm_test_utils:example_name(),
        lambda_id = AtmLambdaId,
        lambda_revision_number = ozt_atm_lambdas:get_largest_revision_number(AtmLambdaId),
        lambda_config = gen_lambda_config(AtmLambdaRevision),
        argument_mappings = atm_test_utils:example_argument_mappers(AtmLambdaRevision, CorrectStoreSchemaIds),
        result_mappings = lists_utils:shuffle([ResultMapping | OtherResultMappings])
    }.


%% @private
gen_lane_including_tasks(Tasks, AtmLambdas, StoreSchemasJson) ->
    StoreSchemaIds = store_schemas_json_to_ids(StoreSchemasJson),
    OffendingPBox = atm_test_utils:example_parallel_box_schema_with_tasks(lists_utils:shuffle(
        Tasks ++ ozt_atm_workflow_schemas:example_task_schemas(AtmLambdas, StoreSchemaIds)
    )),
    ozt_atm_workflow_schemas:example_lane_schema_with_parallel_boxes_json(
        lists_utils:shuffle([
            OffendingPBox | ozt_atm_workflow_schemas:example_parallel_box_schemas(AtmLambdas, StoreSchemaIds)
        ]),
        jsonable_record:list_from_json(StoreSchemasJson, atm_store_schema)
    ).


%% @private
gen_lambda_config(#atm_lambda_revision{config_spec = ConfigSpec}) ->
    gen_lambda_config(ConfigSpec);
gen_lambda_config(ConfigSpecOrParameterNames) ->
    maps_utils:generate_from_list(fun
        F(#atm_parameter_spec{name = Name, data_spec = DataSpec}) ->
            {Name, atm_test_utils:example_predefined_value(DataSpec)};
        F(Name) ->
            F(#atm_parameter_spec{name = Name, data_spec = atm_test_utils:example_data_spec()})
    end, ConfigSpecOrParameterNames).


%% @private
store_schemas_json_to_ids(StoreSchemasJson) ->
    [Id || #{<<"id">> := Id} <- StoreSchemasJson].

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
