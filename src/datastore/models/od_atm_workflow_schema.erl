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

-export([extract_atm_lambdas_from_lanes/1]).

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


-spec extract_atm_lambdas_from_lanes([atm_lane_schema:record()]) -> [od_atm_lambda:id()].
extract_atm_lambdas_from_lanes(Lanes) ->
    ordsets:from_list(lists:foldl(fun(#atm_lane_schema{parallel_boxes = ParallelBoxes}, TopAcc) ->
        lists:foldl(fun(#atm_parallel_box_schema{tasks = Tasks}, MiddleAcc) ->
            lists:foldl(fun(#atm_task_schema{lambda_id = LambdaId}, BottomAcc) ->
                ordsets:add_element(LambdaId, BottomAcc)
            end, MiddleAcc, Tasks)
        end, TopAcc, ParallelBoxes)
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