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
-include_lib("ctool/include/logging.hrl").

-export([validate/2]).

% in-memory cache to avoid excessive requests to DB
-type fetched_lambdas() :: #{od_atm_lambda:id() => od_atm_lambda:record()}.

-record(validator_ctx, {
    atm_workflow_schema_revision :: atm_workflow_schema_revision:record(),
    fetched_lambdas :: fetched_lambdas()
}).
-type validator_ctx() :: #validator_ctx{}.

-define(PREDEFINED_SYSTEM_AUDIT_LOG_STORE_SCHEMA_IDS, [
    ?CURRENT_TASK_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID,
    ?WORKFLOW_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID
]).
-define(RESERVED_STORE_SCHEMA_IDS, [
    ?CURRENT_TASK_TIME_SERIES_STORE_SCHEMA_ID | ?PREDEFINED_SYSTEM_AUDIT_LOG_STORE_SCHEMA_IDS
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
        fun validate_task_lambda_config_and_mappers/1
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
        id = Id, type = Type, config = Config, default_initial_content = DefaultInitialContent
    }) ->
        lists:member(Id, ?RESERVED_STORE_SCHEMA_IDS) andalso
            atm_schema_validator:raise_validation_error(
                str_utils:format_bin("stores[~ts].id", [Id]),
                "The provided store schema Id is reserved and cannot be used in store schema definitions"
            ),
        DataKeyName = str_utils:format_bin("stores[~ts].defaultInitialContent", [Id]),
        sanitize_store_default_initial_content(Type, Config, DefaultInitialContent, DataKeyName),
        Id
    end, Stores),
    atm_schema_validator:assert_unique_identifiers(id, StoreIds, <<"stores">>).


%% @private
-spec sanitize_store_default_initial_content(
    automation:store_type(),
    atm_store_config:record(),
    json_utils:json_term(),
    atm_schema_validator:data_key_name()
) -> ok | no_return().
%% @TODO VFS-7755 proper checks for other store types
sanitize_store_default_initial_content(_StoreType, _Config, undefined, _DataKeyName) ->
    % any store can have undefined default initial content
    % time_series and audit_log stores have it implicitly set to undefined
    ok;
sanitize_store_default_initial_content(single_value, Config, DefaultInitialContent, DataKeyName) ->
    atm_schema_validator:sanitize_predefined_value(
        DefaultInitialContent, Config#atm_single_value_store_config.item_data_spec, DataKeyName
    );
sanitize_store_default_initial_content(list, Config, DefaultInitialContent, DataKeyName) ->
    case DefaultInitialContent of
        List when is_list(List) ->
            lists:foreach(fun(Element) ->
                atm_schema_validator:sanitize_predefined_value(
                    Element, Config#atm_list_store_config.item_data_spec, DataKeyName
                )
            end, DefaultInitialContent);
        _ ->
            atm_schema_validator:raise_validation_error(
                DataKeyName,
                "List store requires default initial content to be an array of values"
            )
    end;
sanitize_store_default_initial_content(tree_forest, Config, DefaultInitialContent, DataKeyName) ->
    case DefaultInitialContent of
        List when is_list(List) ->
            lists:foreach(fun(Element) ->
                atm_schema_validator:sanitize_predefined_value(
                    Element, Config#atm_tree_forest_store_config.item_data_spec, DataKeyName
                )
            end, DefaultInitialContent);
        _ ->
            atm_schema_validator:raise_validation_error(
                DataKeyName,
                "Tree forest store requires default initial content to be an array of values"
            )
    end;
sanitize_store_default_initial_content(range, _Config, DefaultInitialContent, DataKeyName) ->
    case DefaultInitialContent of
        #{<<"end">> := Index} when is_integer(Index) ->
            ok;
        _ ->
            atm_schema_validator:raise_validation_error(
                DataKeyName,
                "Range store requires default initial content to be an object with the following fields: "
                "\"end\" (required), \"start\" (optional), \"step\" (optional)"
            )
    end.


%% @private
-spec validate_store_schema_references(validator_ctx()) ->
    ok | no_return().
validate_store_schema_references(#validator_ctx{
    atm_workflow_schema_revision = #atm_workflow_schema_revision{
        stores = Stores
    } = AtmWorkflowSchemaRevision
}) ->
    StoreSchemasById = maps_utils:generate_from_list(fun(#atm_store_schema{id = Id} = Store) ->
        {Id, Store}
    end, Stores),
    StoreSchemaIds = maps:keys(StoreSchemasById),

    foreach_lane(fun(#atm_lane_schema{
        id = LaneId,
        store_iterator_spec = #atm_store_iterator_spec{store_schema_id = StoreSchemaId}
    }) ->
        assert_allowed_store_schema_reference(
            str_utils:format_bin("lanes[~ts].storeIteratorSpec.storeSchemaId", [LaneId]),
            StoreSchemaId,
            StoreSchemaIds
        ),
        #atm_store_schema{type = StoreType} = maps:get(StoreSchemaId, StoreSchemasById),
        (StoreType =:= time_series orelse StoreType =:= audit_log) andalso atm_schema_validator:raise_validation_error(
            str_utils:format_bin("lanes[~ts].storeIteratorSpec.storeSchemaId", [LaneId]),
            "Iterating over stores of type '~ts' is disallowed",
            [automation:store_type_to_json(StoreType)]
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
                        str_utils:format_bin("tasks[~ts].argumentMappings[~ts].valueBuilder.recipe", [
                            TaskId, ArgumentName
                        ]),
                        StoreSchemaId,
                        StoreSchemaIds
                    );
                _ ->
                    ok
            end
        end, AtmTaskSchema#atm_task_schema.argument_mappings),

        % predefined store schemas are allowed in result mappers, but not in argument mappers
        lists:foreach(fun(#atm_task_schema_result_mapper{store_schema_id = StoreSchemaId, result_name = ResultName}) ->
            DataKeyName = str_utils:format_bin("tasks[~ts].resultMappings[~ts].storeSchemaId", [
                TaskId, ResultName
            ]),
            case StoreSchemaId of
                ?CURRENT_TASK_TIME_SERIES_STORE_SCHEMA_ID ->
                    % predefined time series store for current task is allowed if its config was defined
                    AtmTaskSchema#atm_task_schema.time_series_store_config /= undefined orelse
                        atm_schema_validator:raise_validation_error(
                            DataKeyName,
                            "The time series store for current task cannot be referenced if its config is not defined "
                            "in the task."
                        );
                _ ->
                    % predefined audit log stores are created for each task and always allowed
                    assert_allowed_store_schema_reference(
                        DataKeyName,
                        StoreSchemaId,
                        StoreSchemaIds ++ ?PREDEFINED_SYSTEM_AUDIT_LOG_STORE_SCHEMA_IDS
                    )
            end
        end, AtmTaskSchema#atm_task_schema.result_mappings)

    end, AtmWorkflowSchemaRevision).


%% @private
-spec validate_task_lambda_config_and_mappers(validator_ctx()) ->
    ok | no_return().
validate_task_lambda_config_and_mappers(#validator_ctx{
    atm_workflow_schema_revision = #atm_workflow_schema_revision{} = AtmWorkflowSchemaRevision,
    fetched_lambdas = FetchedLambdas
}) ->
    atm_workflow_schema_revision:fold_tasks(fun(#atm_task_schema{
        lambda_id = AtmLambdaId,
        lambda_revision_number = LambdaRevisionNumber
    } = AtmTaskSchema, FetchedLambdasAcc) ->
        {AtmLambda, NewFetchedLambdasAcc} = fetch_or_reuse_lambda(AtmLambdaId, FetchedLambdasAcc),
        AtmLambdaRevision = atm_lambda_revision_registry:get_revision(
            LambdaRevisionNumber, AtmLambda#od_atm_lambda.revision_registry
        ),
        validate_task_lambda_config(AtmTaskSchema, AtmLambdaRevision),
        validate_task_argument_mappers(AtmTaskSchema, AtmLambdaRevision),
        validate_task_result_mappers(AtmTaskSchema, AtmLambdaRevision),
        NewFetchedLambdasAcc
    end, FetchedLambdas, AtmWorkflowSchemaRevision),
    ok.


%% @private
-spec validate_task_lambda_config(atm_task_schema:record(), atm_lambda_revision:record()) -> ok | no_return().
validate_task_lambda_config(#atm_task_schema{
    id = TaskId,
    lambda_config = LambdaConfig
}, #atm_lambda_revision{
    config_parameter_specs = ConfigParameterSpecs
}) ->
    ReferencedConfigParameterNames = maps:keys(LambdaConfig),
    ConfigParameterNames = [S#atm_parameter_spec.name || S <- ConfigParameterSpecs],
    atm_schema_validator:assert_known_names(
        ReferencedConfigParameterNames, ConfigParameterNames, str_utils:format_bin("tasks[~ts].lambdaConfig", [TaskId])
    ),

    lists:foreach(fun(#atm_parameter_spec{name = Name} = ParameterSpec) ->
        case {ParameterSpec, lists:member(Name, ReferencedConfigParameterNames)} of
            % the is_optional flag is ignored if the default_value is specified
            {#atm_parameter_spec{default_value = undefined, is_optional = false}, false} ->
                raise_missing_required_config_parameter_error(Name);
            {_, _} ->
                ok
        end
    end, ConfigParameterSpecs),

    maps:foreach(fun(ParameterName, Value) ->
        {ok, #atm_parameter_spec{
            data_spec = DataSpec
        }} = lists_utils:find(fun(#atm_parameter_spec{name = N}) -> N == ParameterName end, ConfigParameterSpecs),
        atm_schema_validator:sanitize_predefined_value(
            Value, DataSpec, str_utils:format_bin("tasks[~ts].lambdaConfig[~ts]", [TaskId, ParameterName])
        )
    end, LambdaConfig).


%% @private
-spec validate_task_argument_mappers(atm_task_schema:record(), atm_lambda_revision:record()) -> ok | no_return().
validate_task_argument_mappers(#atm_task_schema{
    id = TaskId,
    argument_mappings = ArgumentMappings
}, #atm_lambda_revision{
    argument_specs = ArgumentSpecs
}) ->
    MapperArgumentNames = [S#atm_task_schema_argument_mapper.argument_name || S <- ArgumentMappings],
    % there may be no more that one mapper per argument name (hence the mapper names must be unique)
    atm_schema_validator:assert_unique_identifiers(
        name, MapperArgumentNames, str_utils:format_bin("tasks[~ts].argumentMappings", [TaskId])
    ),

    SpecArgumentNames = [S#atm_parameter_spec.name || S <- ArgumentSpecs],
    atm_schema_validator:assert_known_names(
        MapperArgumentNames, SpecArgumentNames, str_utils:format_bin("tasks[~ts].argumentMappings", [TaskId])
    ),

    lists:foreach(fun(#atm_parameter_spec{name = Name} = ArgumentSpec) ->
        case {ArgumentSpec, lists:member(Name, MapperArgumentNames)} of
            % the is_optional flag is ignored if the default_value is specified
            {#atm_parameter_spec{default_value = undefined, is_optional = false}, false} ->
                raise_missing_required_argument_mapper_error(Name);
            {_, _} ->
                ok
        end
    end, ArgumentSpecs).


%% @private
-spec validate_task_result_mappers(atm_task_schema:record(), atm_lambda_revision:record()) -> ok | no_return().
validate_task_result_mappers(#atm_task_schema{
    id = TaskId,
    result_mappings = ResultMappings
}, #atm_lambda_revision{
    result_specs = ResultSpecs
}) ->
    % any number of result mappers per result name is allowed (no need to check for uniqueness)
    MapperResultNames = [S#atm_task_schema_result_mapper.result_name || S <- ResultMappings],
    SpecResultNames = [S#atm_lambda_result_spec.name || S <- ResultSpecs],
    atm_schema_validator:assert_known_names(
        MapperResultNames, SpecResultNames, str_utils:format_bin("tasks[~ts].resultMappings", [TaskId])
    ).

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
-spec fetch_or_reuse_lambda(od_atm_lambda:id(), fetched_lambdas()) -> {od_atm_lambda:record(), fetched_lambdas()}.
fetch_or_reuse_lambda(AtmLambdaId, FetchedLambdas) ->
    NewFetchedLambdas = case maps:find(AtmLambdaId, FetchedLambdas) of
        {ok, _} ->
            FetchedLambdas;
        error ->
            {ok, #document{value = Record}} = od_atm_lambda:get(AtmLambdaId),
            FetchedLambdas#{AtmLambdaId => Record}
    end,
    {maps:get(AtmLambdaId, NewFetchedLambdas), NewFetchedLambdas}.


%% @private
-spec assert_allowed_store_schema_reference(atm_schema_validator:data_key_name(), automation:id(), [automation:id()]) ->
    ok | no_return().
assert_allowed_store_schema_reference(DataKeyName, StoreSchemaId, AllowedStoreSchemaIds) ->
    lists:member(StoreSchemaId, AllowedStoreSchemaIds) orelse atm_schema_validator:raise_validation_error(
        DataKeyName,
        "The provided storeSchemaId = '~ts' was not found among defined store schemas",
        [StoreSchemaId]
    ),
    ok.


%% @private
-spec raise_missing_required_config_parameter_error(atm_parameter_spec:name()) -> no_return().
raise_missing_required_config_parameter_error(ParameterName) ->
    atm_schema_validator:raise_validation_error(
        <<"lambdaConfig">>,
        "Missing value for required lambda config parameter '~ts'",
        [ParameterName]
    ).


%% @private
-spec raise_missing_required_argument_mapper_error(atm_parameter_spec:name()) -> no_return().
raise_missing_required_argument_mapper_error(ArgumentName) ->
    atm_schema_validator:raise_validation_error(
        <<"argumentMappings">>,
        "Missing argument mapper for required argument '~ts'",
        [ArgumentName]
    ).