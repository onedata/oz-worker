%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Database model representing an automation lambda - an abstraction of a
%%% granular operation that is used as a building block for automation workflow
%%% schemas.
%%% @end
%%%-------------------------------------------------------------------
-module(od_atm_lambda).
-author("Lukasz Opiola").

-include("automation.hrl").
-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).
-export([critical_section/2]).
-export([dump_to_json/3]).
-export([dump_revision_to_json/2]).
-export([default_resource_spec/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1]).

-type id() :: binary().
-type record() :: #od_atm_lambda{}.
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
get(AtmLambdaId) ->
    datastore_model:get(?CTX, AtmLambdaId).


-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(AtmLambdaId) ->
    datastore_model:exists(?CTX, AtmLambdaId).


-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(AtmLambdaId, Diff) ->
    datastore_model:update(?CTX, AtmLambdaId, Diff).


%%--------------------------------------------------------------------
%% @doc
%% Deletes an automation lambda by Id.
%% WARNING: Must not be used directly, as deleting an lambda that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a lambda, use atm_lambda_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(AtmLambdaId) ->
    datastore_model:delete(?CTX, AtmLambdaId).


-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).


-spec to_string(AtmLambdaId :: id()) -> binary().
to_string(AtmLambdaId) ->
    <<"atmLambda:", AtmLambdaId/binary>>.


-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    atm_lambda_logic_plugin.


%%--------------------------------------------------------------------
%% @doc
%% Every operation that modifies relations of a lambda should be wrapped
%% in this critical section to avoid race conditions caused by concurrent
%% modifications.
%% @end
%%--------------------------------------------------------------------
-spec critical_section(id(), fun(() -> Result)) -> Result.
critical_section(AtmLambdaId, Fun) ->
    critical_section:run({?MODULE, ?FUNCTION_NAME, AtmLambdaId}, Fun).


-spec dump_to_json(id(), record(), atm_lambda_revision:revision_number()) -> json_utils:json_map().
dump_to_json(AtmLambdaId, AtmLambda, IncludedRevisionNumber) ->
    #{
        <<"schemaFormatVersion">> => ?CURRENT_SCHEMA_FORMAT_VERSION,

        <<"originalAtmLambdaId">> => AtmLambdaId,

        <<"revision">> => dump_revision_to_json(AtmLambda, IncludedRevisionNumber)
    }.


-spec dump_revision_to_json(record(), atm_lambda_revision:revision_number()) -> json_utils:json_map().
dump_revision_to_json(#od_atm_lambda{revision_registry = RevisionRegistry}, RevisionNumber) ->
    IncludedRevision = atm_lambda_revision_registry:get_revision(RevisionNumber, RevisionRegistry),
    #{
        <<"schemaFormatVersion">> => ?CURRENT_SCHEMA_FORMAT_VERSION,
        <<"originalRevisionNumber">> => RevisionNumber,
        <<"atmLambdaRevision">> => persistent_record:encode(IncludedRevision, atm_lambda_revision)
    }.


-spec default_resource_spec() -> atm_resource_spec:record().
default_resource_spec() ->
    jsonable_record:from_json(oz_worker:get_env(default_atm_resource_spec), atm_resource_spec).

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
        {revision_registry, {custom, string, {persistent_record, encode, decode, atm_lambda_revision_registry}}},

        {original_atm_lambda, string},
        {atm_inventories, [string]},
        {atm_workflow_schemas, [string]},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}}
    ]}.
