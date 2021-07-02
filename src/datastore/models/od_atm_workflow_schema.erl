%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Database model representing an automation workflow schema - a template for
%%% an automation workflow, defining the orchestration of tasks during
%%% workflow execution.
%%% @end
%%%-------------------------------------------------------------------
-module(od_atm_workflow_schema).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).

-export([dump_schema_to_json/2]).
-export([fold_tasks/3, map_tasks/2, extract_atm_lambdas_from_lanes/1]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1]).

-type id() :: binary().
-type record() :: #od_atm_workflow_schema{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-type name() :: binary().

-export_type([id/0, record/0]).
-export_type([name/0]).

-define(CTX, #{
    model => ?MODULE,
    secure_fold_enabled => true,
    sync_enabled => true,
    memory_copies => all
}).

-compile({no_auto_import, [get/1]}).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).


-spec get(id()) -> {ok, doc()} | {error, term()}.
get(AtmWorkflowSchemaId) ->
    datastore_model:get(?CTX, AtmWorkflowSchemaId).


-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(AtmWorkflowSchemaId) ->
    datastore_model:exists(?CTX, AtmWorkflowSchemaId).


-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(AtmWorkflowSchemaId, Diff) ->
    datastore_model:update(?CTX, AtmWorkflowSchemaId, Diff).


%%--------------------------------------------------------------------
%% @doc
%% Deletes an automation workflow schema by Id.
%% WARNING: Must not be used directly, as deleting an workflow schema that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a workflow schema, use atm_workflow_schema_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(AtmWorkflowSchemaId) ->
    datastore_model:delete(?CTX, AtmWorkflowSchemaId).


-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).


-spec to_string(AtmWorkflowSchemaId :: id()) -> binary().
to_string(AtmWorkflowSchemaId) ->
    <<"atmWorkflowSchema:", AtmWorkflowSchemaId/binary>>.


-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    atm_workflow_schema_logic_plugin.


-spec dump_schema_to_json(id(), record()) -> json_utils:json_map().
dump_schema_to_json(AtmWorkflowSchemaId, AtmWorkflowSchema) ->
    #{
        <<"schemaFormatVersion">> => 1,

        <<"atmWorkflowSchemaId">> => AtmWorkflowSchemaId,

        <<"name">> => AtmWorkflowSchema#od_atm_workflow_schema.name,
        <<"description">> => AtmWorkflowSchema#od_atm_workflow_schema.description,

        <<"stores">> => jsonable_record:list_to_json(AtmWorkflowSchema#od_atm_workflow_schema.stores, atm_store_schema),
        <<"lanes">> => jsonable_record:list_to_json(AtmWorkflowSchema#od_atm_workflow_schema.lanes, atm_lane_schema),

        <<"state">> => automation:workflow_schema_state_to_json(AtmWorkflowSchema#od_atm_workflow_schema.state),

        <<"supplementaryAtmLambdas">> => lists:foldl(fun(AtmLambdaId, Acc) ->
            {ok, #document{value = AtmLambda}} = od_atm_lambda:get(AtmLambdaId),
            Acc#{
                AtmLambdaId => od_atm_lambda:dump_schema_to_json(AtmLambda)
            }
        end, #{}, extract_atm_lambdas_from_lanes(AtmWorkflowSchema#od_atm_workflow_schema.lanes))
    }.


-spec fold_tasks(
    fun((atm_task_schema:record(), AccIn :: term()) -> AccOut :: term()),
    InitialAcc :: term(),
    od_atm_workflow_schema:record() | [atm_lane_schema:record()]
) -> FinalAcc :: term().
fold_tasks(Callback, InitialAcc, #od_atm_workflow_schema{lanes = Lanes}) ->
    fold_tasks(Callback, InitialAcc, Lanes);
fold_tasks(Callback, InitialAcc, Lanes) ->
    lists:foldl(fun(#atm_lane_schema{parallel_boxes = ParallelBoxes}, TopAcc) ->
        lists:foldl(fun(#atm_parallel_box_schema{tasks = Tasks}, MiddleAcc) ->
            lists:foldl(fun(#atm_task_schema{} = AtmTaskSchema, BottomAcc) ->
                Callback(AtmTaskSchema, BottomAcc)
            end, MiddleAcc, Tasks)
        end, TopAcc, ParallelBoxes)
    end, InitialAcc, Lanes).


-spec map_tasks(
    fun((atm_task_schema:record()) -> atm_task_schema:record()),
    od_atm_workflow_schema:record() | [atm_lane_schema:record()]
) -> od_atm_workflow_schema:record() | [atm_lane_schema:record()].
map_tasks(MappingFunction, #od_atm_workflow_schema{lanes = Lanes} = AtmWorkflowSchema) ->
    AtmWorkflowSchema#od_atm_workflow_schema{
        lanes = map_tasks(MappingFunction, Lanes)
    };
map_tasks(MappingFunction, Lanes) ->
    lists:map(fun(Lane = #atm_lane_schema{parallel_boxes = ParallelBoxes}) ->
        Lane#atm_lane_schema{
            parallel_boxes = lists:map(fun(ParallelBox = #atm_parallel_box_schema{tasks = Tasks}) ->
                ParallelBox#atm_parallel_box_schema{
                    tasks = lists:map(MappingFunction, Tasks)
                }
            end, ParallelBoxes)
        }
    end, Lanes).


-spec extract_atm_lambdas_from_lanes([atm_lane_schema:record()]) -> [od_atm_lambda:id()].
extract_atm_lambdas_from_lanes(Lanes) ->
    ordsets:to_list(fold_tasks(fun(#atm_task_schema{lambda_id = LambdaId}, Acc) ->
        ordsets:add_element(LambdaId, Acc)
    end, ordsets:new(), Lanes)).

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

-spec get_record_version() -> datastore_model:record_version().
get_record_version() ->
    1.

-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {name, string},
        {description, string},

        {stores, [{custom, string, {persistent_record, encode, decode, atm_store_schema}}]},
        {lanes, [{custom, string, {persistent_record, encode, decode, atm_lane_schema}}]},

        {state, {custom, string, {automation, workflow_schema_state_to_json, workflow_schema_state_from_json}}},

        {atm_inventory, string},
        {atm_lambdas, [string]},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}}
    ]}.