%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles validation of automation workflow schemas, especially
%%% checking the cross references between different objects and definitions.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_workflow_schema_validator).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").

-export([validate/2]).

-record(validator_ctx, {
    atm_workflow_schema_revision :: atm_workflow_schema_revision:record(),
    fetched_lambdas :: #{od_atm_lambda:id() => od_atm_lambda:record()}
}).
-type validator_ctx() :: #validator_ctx{}.

-define(PREDEFINED_SYSTEM_AUDIT_LOG_STORE_SCHEMA_IDS, [
    ?CURRENT_TASK_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID,
    ?WORKFLOW_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec validate(atm_workflow_schema_revision:record(), #{od_atm_lambda:id() => od_atm_lambda:record()}) ->
    ok | no_return().
validate(AtmWorkflowSchemaRevision, KnownAtmLambdas) ->
    Ctx = #validator_ctx{
        atm_workflow_schema_revision = AtmWorkflowSchemaRevision,
        fetched_lambdas = KnownAtmLambdas
    },
    atm_schema_validator:run_validation_procedures(Ctx, [
        fun validate_all_ids_in_lanes/1,
        fun validate_store_schemas/1,
        fun validate_store_schema_references/1,
        fun validate_argument_and_result_mappers/1
    ]).

%%%===================================================================
%%% Validation procedures
%%%===================================================================

%% @private
-spec validate_all_ids_in_lanes(validator_ctx()) ->
    ok | no_return().
validate_all_ids_in_lanes(#validator_ctx{
    atm_workflow_schema_revision = #atm_workflow_schema_revision{
        lanes = Lanes
    } = AtmWorkflowSchemaRevision
}) ->
    LaneIds = [L#atm_lane_schema.id || L <- Lanes],
    ParallelBoxIds = lists:flatmap(fun(#atm_lane_schema{parallel_boxes = ParallelBoxes}) ->
        [P#atm_parallel_box_schema.id || P <- ParallelBoxes]
    end, Lanes),
    TaskIds = atm_workflow_schema_revision:fold_tasks(fun(AtmTaskSchema, Acc) ->
        [AtmTaskSchema#atm_task_schema.id | Acc]
    end, [], AtmWorkflowSchemaRevision),
    atm_schema_validator:assert_unique_identifiers(id, LaneIds, <<"lanes">>),
    atm_schema_validator:assert_unique_identifiers(id, ParallelBoxIds, <<"parallelBoxes">>),
    atm_schema_validator:assert_unique_identifiers(id, TaskIds, <<"tasks">>).


%% @private
-spec validate_store_schemas(validator_ctx()) ->
    ok | no_return().
validate_store_schemas(#validator_ctx{atm_workflow_schema_revision = #atm_workflow_schema_revision{stores = Stores}}) ->
    StoreIds = lists:map(fun(#atm_store_schema{
        id = Id, type = Type, default_initial_value = DefaultInitialValue, data_spec = DataSpec
    } = AtmStoreSchema) ->
        lists:member(Id, ?PREDEFINED_SYSTEM_AUDIT_LOG_STORE_SCHEMA_IDS) andalso
            atm_schema_validator:raise_validation_error(
                str_utils:format_bin("stores[~s].id", [Id]),
                "The provided store schema Id is reserved and cannot be used in store schema definitions"
            ),
        sanitize_store_type_and_data_spec(AtmStoreSchema),
        DataKeyName = str_utils:format_bin("stores[~s].defaultInitialValue", [Id]),
        sanitize_store_default_initial_value(Type, DefaultInitialValue, DataSpec, DataKeyName),
        Id
    end, Stores),
    atm_schema_validator:assert_unique_identifiers(id, StoreIds, <<"stores">>).


%% @private
-spec sanitize_store_default_initial_value(
    automation:store_type(),
    json_utils:json_term(),
    atm_data_spec:record(),
    atm_schema_validator:data_key_name()
) -> ok | no_return().
%% @TODO VFS-7755 proper checks for other store types
sanitize_store_default_initial_value(range, DefaultInitialValue, _DataSpec, DataKeyName) ->
    case DefaultInitialValue of
        #{<<"end">> := Index} when is_integer(Index) ->
            ok;
        _ ->
            atm_schema_validator:raise_validation_error(
                DataKeyName,
                "Range store requires default initial value as an object with the following fields: "
                "\"end\" (required), \"start\" (optional), \"step\" (optional)"
            )
    end;
sanitize_store_default_initial_value(list, DefaultInitialValue, DataSpec, DataKeyName) ->
    case DefaultInitialValue of
        undefined ->
            ok;
        List when is_list(List) ->
            lists:foreach(fun(Element) ->
                atm_schema_validator:sanitize_initial_value(Element, DataSpec, DataKeyName)
            end, DefaultInitialValue);
        _ ->
            atm_schema_validator:raise_validation_error(
                DataKeyName,
                "List store requires default initial value to be a list"
            )
    end;
sanitize_store_default_initial_value(_StoreType, DefaultInitialValue, DataSpec, DataKeyName) ->
    atm_schema_validator:sanitize_initial_value(DefaultInitialValue, DataSpec, DataKeyName).


%% @private
-spec validate_store_schema_references(validator_ctx()) ->
    ok | no_return().
validate_store_schema_references(#validator_ctx{
    atm_workflow_schema_revision = #atm_workflow_schema_revision{
        stores = Stores
    } = AtmWorkflowSchemaRevision
}) ->
    StoreSchemaIds = [Store#atm_store_schema.id || Store <- Stores],

    foreach_lane(fun(#atm_lane_schema{
        id = LaneId,
        store_iterator_spec = #atm_store_iterator_spec{store_schema_id = StoreSchemaId}
    }) ->
        assert_allowed_store_schema_reference(
            str_utils:format_bin("lanes[~s].storeIteratorSpec.storeSchemaId", [LaneId]),
            StoreSchemaId,
            StoreSchemaIds
        )
    end, AtmWorkflowSchemaRevision),

    foreach_task(fun(AtmTaskSchema = #atm_task_schema{id = TaskId}) ->

        lists:foreach(fun(#atm_task_schema_argument_mapper{value_builder = ValueBuilder, argument_name = ArgumentName}) ->
            case ValueBuilder of
                #atm_task_argument_value_builder{type = Type, recipe = StoreSchemaId} when
                    Type == store_credentials;
                    Type == single_value_store_content
                    ->
                    assert_allowed_store_schema_reference(
                        str_utils:format_bin("tasks[~s].argumentMappings[~s].valueBuilder.recipe", [
                            TaskId, ArgumentName
                        ]),
                        StoreSchemaId,
                        StoreSchemaIds
                    );
                _ ->
                    ok
            end
        end, AtmTaskSchema#atm_task_schema.argument_mappings),

        lists:foreach(fun(#atm_task_schema_result_mapper{store_schema_id = StoreSchemaId, result_name = ResultName}) ->
            assert_allowed_store_schema_reference(
                str_utils:format_bin("tasks[~s].resultMappings[~s].storeSchemaId", [
                    TaskId, ResultName
                ]),
                StoreSchemaId,
                % predefined audit log store schemas are allowed only in result mappers
                StoreSchemaIds ++ ?PREDEFINED_SYSTEM_AUDIT_LOG_STORE_SCHEMA_IDS
            )
        end, AtmTaskSchema#atm_task_schema.result_mappings)

    end, AtmWorkflowSchemaRevision).


%% @private
-spec validate_argument_and_result_mappers(validator_ctx()) ->
    ok | no_return().
validate_argument_and_result_mappers(#validator_ctx{
    atm_workflow_schema_revision = #atm_workflow_schema_revision{} = AtmWorkflowSchemaRevision,
    fetched_lambdas = FetchedLambdas
}) ->
    atm_workflow_schema_revision:fold_tasks(fun(AtmTaskSchema = #atm_task_schema{
        id = TaskId,
        lambda_id = AtmLambdaId,
        lambda_revision_number = LambdaRevisionNumber
    }, FetchedLambdasAcc) ->
        MapperArgumentNames = [S#atm_task_schema_argument_mapper.argument_name || S <- AtmTaskSchema#atm_task_schema.argument_mappings],
        MapperResultNames = [S#atm_task_schema_result_mapper.result_name || S <- AtmTaskSchema#atm_task_schema.result_mappings],

        % there may be not more that one mapper per argument name (hence the mapper names must be unique),
        % but any number of result mappers per result name (so there is no need to check for uniqueness)
        atm_schema_validator:assert_unique_identifiers(
            name, MapperArgumentNames, str_utils:format_bin("tasks[~s].argumentMappings", [TaskId])
        ),

        NewFetchedLambdasAcc = case maps:find(AtmLambdaId, FetchedLambdas) of
            {ok, _} ->
                FetchedLambdasAcc;
            error ->
                {ok, #document{value = Record}} = od_atm_lambda:get(AtmTaskSchema#atm_task_schema.lambda_id),
                FetchedLambdasAcc#{AtmLambdaId => Record}
        end,
        #od_atm_lambda{revision_registry = RevisionRegistry} = maps:get(AtmLambdaId, NewFetchedLambdasAcc),
        #atm_lambda_revision{
            argument_specs = ArgumentSpecs,
            result_specs = ResultSpecs
        } = atm_lambda_revision_registry:get_revision(LambdaRevisionNumber, RevisionRegistry),

        SpecArgumentNames = [S#atm_lambda_argument_spec.name || S <- ArgumentSpecs],
        SpecResultNames = [S#atm_lambda_result_spec.name || S <- ResultSpecs],

        atm_schema_validator:assert_known_names(
            MapperArgumentNames, SpecArgumentNames, str_utils:format_bin("tasks[~s].argumentMappings", [TaskId])
        ),
        atm_schema_validator:assert_known_names(
            MapperResultNames, SpecResultNames, str_utils:format_bin("tasks[~s].resultMappings", [TaskId])
        ),

        lists:foreach(fun(#atm_lambda_argument_spec{name = Name} = ArgumentSpec) ->
            case {ArgumentSpec, lists:member(Name, MapperArgumentNames)} of
                % the is_optional flag is ignored if the default_value is specified
                {#atm_lambda_argument_spec{default_value = undefined, is_optional = false}, false} ->
                    raise_missing_required_argument_mapper_error(Name);
                {_, _} ->
                    ok
            end
        end, ArgumentSpecs),

        NewFetchedLambdasAcc
    end, FetchedLambdas, AtmWorkflowSchemaRevision),
    ok.

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
-spec foreach_lane(fun((atm_lane_schema:record()) -> any() | no_return()), atm_workflow_schema_revision:record()) ->
    ok | no_return().
foreach_lane(Callback, AtmWorkflowSchemaRevision) ->
    lists:foreach(Callback, AtmWorkflowSchemaRevision#atm_workflow_schema_revision.lanes).


%% @private
-spec foreach_task(fun((atm_task_schema:record()) -> any() | no_return()), atm_workflow_schema_revision:record()) ->
    ok | no_return().
foreach_task(Callback, AtmWorkflowSchemaRevision) ->
    atm_workflow_schema_revision:fold_tasks(fun(AtmTaskSchema, _) ->
        Callback(AtmTaskSchema)
    end, undefined, AtmWorkflowSchemaRevision),
    ok.


%% @private
-spec sanitize_store_type_and_data_spec(atm_store_schema:record()) -> ok | no_return().
sanitize_store_type_and_data_spec(#atm_store_schema{type = tree_forest, data_spec = AtmDataSpec, id = Id}) ->
    case AtmDataSpec of
        #atm_data_spec{type = atm_file_type} -> ok;
        #atm_data_spec{type = atm_dataset_type} -> ok;
        #atm_data_spec{type = AtmDataType} -> raise_conflicting_store_and_data_type(Id, AtmDataType)
    end;
sanitize_store_type_and_data_spec(#atm_store_schema{type = range, data_spec = AtmDataSpec, id = Id}) ->
    case AtmDataSpec of
        #atm_data_spec{type = atm_integer_type} -> ok;
        #atm_data_spec{type = AtmDataType} -> raise_conflicting_store_and_data_type(Id, AtmDataType)
    end;
sanitize_store_type_and_data_spec(#atm_store_schema{}) ->
    ok.


%% @private
-spec assert_allowed_store_schema_reference(atm_schema_validator:data_key_name(), automation:id(), [automation:id()]) ->
    ok | no_return().
assert_allowed_store_schema_reference(DataKeyName, StoreSchemaId, AllowedStoreSchemaIds) ->
    lists:member(StoreSchemaId, AllowedStoreSchemaIds) orelse atm_schema_validator:raise_validation_error(
        DataKeyName,
        "The provided storeSchemaId = '~s' was not found among defined store schemas",
        [StoreSchemaId]
    ),
    ok.


%% @private
-spec raise_missing_required_argument_mapper_error(automation:name()) -> no_return().
raise_missing_required_argument_mapper_error(ArgumentName) ->
    atm_schema_validator:raise_validation_error(
        <<"argumentMappings">>,
        "Missing argument mapper for required argument '~s'",
        [ArgumentName]
    ).


%% @private
-spec raise_conflicting_store_and_data_type(automation:id(), atm_data_type:type()) -> no_return().
raise_conflicting_store_and_data_type(StoreId, AtmDataType) ->
    atm_schema_validator:raise_validation_error(
        str_utils:format_bin("stores[~s].type", [StoreId]),
        "Provided store type is disallowed for data type ~s",
        [atm_data_type:type_to_json(AtmDataType)]
    ).
