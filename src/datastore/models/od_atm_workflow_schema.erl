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
-export([critical_section/2]).

-export([dump_to_json/3, dump_revision_to_json/2]).
-export([extract_all_atm_lambda_references/1]).

%%% field encoding/decoding procedures
-export([legacy_state_to_json/1, legacy_state_from_json/1]).
%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_atm_workflow_schema{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-type name() :: binary().

-export_type([id/0, record/0, doc/0]).
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


%%--------------------------------------------------------------------
%% @doc
%% Every operation that modifies relations of a workflow schema should be wrapped
%% in this critical section to avoid race conditions caused by concurrent
%% modifications.
%% @end
%%--------------------------------------------------------------------
-spec critical_section(id(), fun(() -> Result)) -> Result.
critical_section(AtmWorkflowSchemaId, Fun) ->
    critical_section:run({?MODULE, ?FUNCTION_NAME, AtmWorkflowSchemaId}, Fun).


-spec dump_to_json(id(), record(), atm_workflow_schema_revision:revision_number()) -> json_utils:json_map().
dump_to_json(AtmWorkflowSchemaId, AtmWorkflowSchema, IncludedRevisionNumber) ->
    #{
        <<"schemaFormatVersion">> => 2,

        <<"originalAtmWorkflowSchemaId">> => AtmWorkflowSchemaId,

        <<"name">> => AtmWorkflowSchema#od_atm_workflow_schema.name,
        <<"summary">> => AtmWorkflowSchema#od_atm_workflow_schema.summary,

        <<"revision">> => dump_revision_to_json(AtmWorkflowSchema, IncludedRevisionNumber)
    }.


-spec dump_revision_to_json(record(), atm_workflow_schema_revision:revision_number()) -> json_utils:json_map().
dump_revision_to_json(#od_atm_workflow_schema{revision_registry = RevisionRegistry}, RevisionNumber) ->
    IncludedRevision = atm_workflow_schema_revision_registry:get_revision(RevisionNumber, RevisionRegistry),
    #{
        <<"schemaFormatVersion">> => 2,
        <<"originalRevisionNumber">> => RevisionNumber,
        <<"atmWorkflowSchemaRevision">> => jsonable_record:to_json(IncludedRevision, atm_workflow_schema_revision),
        <<"supplementaryAtmLambdas">> => maps:map(fun(AtmLambdaId, ReferencedRevisionNumbers) ->
            {ok, #document{value = AtmLambda}} = od_atm_lambda:get(AtmLambdaId),
            maps_utils:build_from_list(fun(ReferencedRevisionNumber) ->
                Key = integer_to_binary(ReferencedRevisionNumber),
                Value = od_atm_lambda:dump_to_json(AtmLambdaId, AtmLambda, ReferencedRevisionNumber),
                {Key, Value}
            end, ReferencedRevisionNumbers)
        end, atm_workflow_schema_revision:extract_atm_lambda_references(IncludedRevision))
    }.


-spec extract_all_atm_lambda_references(record()) -> atm_workflow_schema_revision:atm_lambda_references().
extract_all_atm_lambda_references(#od_atm_workflow_schema{revision_registry = RevisionRegistry}) ->
    atm_workflow_schema_revision_registry:fold_revisions(fun(AtmWorkflowSchemaRevision, AccExternal) ->
        maps:fold(fun(AtmLambdaId, CurrentReferencedRevisionNumbers, AccInternal) ->
            maps:update_with(AtmLambdaId, fun(AllReferencedRevisionNumbers) ->
                lists_utils:union(CurrentReferencedRevisionNumbers, AllReferencedRevisionNumbers)
            end, CurrentReferencedRevisionNumbers, AccInternal)
        end, AccExternal, atm_workflow_schema_revision:extract_atm_lambda_references(AtmWorkflowSchemaRevision))
    end, #{}, RevisionRegistry).

%%%===================================================================
%%% field encoding/decoding procedures
%%%===================================================================

%% NOTE: used only in record version 1
-spec legacy_state_to_json(atom()) -> json_utils:json_term().
legacy_state_to_json(incomplete) -> <<"incomplete">>;
legacy_state_to_json(ready) -> <<"ready">>;
legacy_state_to_json(deprecated) -> <<"deprecated">>.


%% NOTE: used only in record version 1
-spec legacy_state_from_json(json_utils:json_term()) -> atom().
legacy_state_from_json(<<"incomplete">>) -> incomplete;
legacy_state_from_json(<<"ready">>) -> ready;
legacy_state_from_json(<<"deprecated">>) -> deprecated.

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

-spec get_record_version() -> datastore_model:record_version().
get_record_version() ->
    2.

-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {name, string},
        {description, string},

        {stores, [{custom, string, {persistent_record, encode, decode, atm_store_schema}}]},
        {lanes, [{custom, string, {persistent_record, encode, decode, atm_lane_schema}}]},

        {state, {custom, string, {?MODULE, legacy_state_to_json, legacy_state_from_json}}},

        {atm_inventory, string},
        {atm_lambdas, [string]},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}}
    ]};
get_record_struct(2) ->
    % new field - summary
    % new field - original_atm_workflow_schema
    % description, stores, lane and state are now stored per revision in the revision registry
    {record, [
        {name, string},
        {summary, string},

        {revision_registry, {custom, string, {persistent_record, encode, decode, atm_workflow_schema_revision_registry}}},

        {original_atm_workflow_schema, string},
        {atm_inventory, string},
        {atm_lambdas, [string]},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}}
    ]}.


%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, OdAtmWorkflowSchema) ->
    {
        od_atm_workflow_schema,
        Name,
        Description,

        Stores,
        Lanes,

        State,

        AtmInventory,
        AtmLambdas,

        CreationTime,
        Creator
    } = OdAtmWorkflowSchema,
    {2, #od_atm_workflow_schema{
        name = Name,
        summary = ?DEFAULT_SUMMARY,

        revision_registry = atm_workflow_schema_revision_registry:insert_revision(
            1,
            #atm_workflow_schema_revision{
                description = Description,
                stores = Stores,
                lanes = Lanes,
                state = case State of
                    incomplete -> draft;
                    ready -> stable;
                    deprecated -> deprecated
                end
            },
            atm_workflow_schema_revision_registry:empty()
        ),

        original_atm_workflow_schema = undefined,
        atm_inventory = AtmInventory,
        atm_lambdas = AtmLambdas,

        creation_time = CreationTime,
        creator = Creator
    }}.
