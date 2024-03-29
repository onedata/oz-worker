%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all Automation Workflow Schema logic functionality.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_workflow_schema_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create/2,
    dump/3
]).
-export([
    get/2,
    get_atm_lambdas/2,
    list/1
]).
-export([
    update/3,
    insert_revision/4,
    delete_revision/3,
    dump_revision/3
]).
-export([
    delete/2
]).

-export([
    exists/1
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(aai:auth(), od_atm_workflow_schema:name() | entity_logic:data()) ->
    {ok, od_atm_workflow_schema:id()} | errors:error().
create(Auth, Name) when is_binary(Name) ->
    create(Auth, #{<<"name">> => Name});
create(Auth, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_workflow_schema, id = undefined, aspect = instance},
        data = Data
    })).


-spec dump(
    aai:auth(),
    od_atm_workflow_schema:id(),
    entity_logic:data() | atm_workflow_schema_revision:revision_number()
) ->
    {ok, json_utils:json_map()} | errors:error().
dump(Auth, AtmWorkflowSchemaId, Revision) when is_integer(Revision) ->
    dump(Auth, AtmWorkflowSchemaId, #{<<"includeRevision">> => Revision});
dump(Auth, AtmWorkflowSchemaId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_workflow_schema, id = AtmWorkflowSchemaId, aspect = dump},
        data = Data
    })).


-spec get(aai:auth(), od_atm_workflow_schema:id()) ->
    {ok, od_atm_workflow_schema:record()} | errors:error().
get(Auth, AtmWorkflowSchemaId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_workflow_schema, id = AtmWorkflowSchemaId, aspect = instance}
    }).


-spec get_atm_lambdas(aai:auth(), od_atm_workflow_schema:id()) ->
    {ok, od_atm_lambda:id()} | errors:error().
get_atm_lambdas(Auth, AtmWorkflowSchemaId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_workflow_schema, id = AtmWorkflowSchemaId, aspect = atm_lambdas}
    }).


-spec list(aai:auth()) ->
    {ok, [od_atm_workflow_schema:id()]} | errors:error().
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_workflow_schema, id = undefined, aspect = list}
    }).


-spec update(aai:auth(), od_atm_workflow_schema:id(), entity_logic:data()) ->
    ok | errors:error().
update(Auth, AtmWorkflowSchemaId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_atm_workflow_schema, id = AtmWorkflowSchemaId, aspect = instance},
        data = Data
    }).


-spec insert_revision(
    aai:auth(),
    od_atm_workflow_schema:id(),
    atm_workflow_schema_revision:revision_number() | binary(),
    entity_logic:data()
) ->
    ok | errors:error().
insert_revision(Auth, AtmWorkflowSchemaId, TargetRevisionNumber, Data) when is_integer(TargetRevisionNumber) ->
    insert_revision(Auth, AtmWorkflowSchemaId, integer_to_binary(TargetRevisionNumber), Data);
insert_revision(Auth, AtmWorkflowSchemaId, TargetRevisionNumberBin, Data) when is_binary(TargetRevisionNumberBin) ->
    entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{
            type = od_atm_workflow_schema,
            id = AtmWorkflowSchemaId,
            aspect = {revision, TargetRevisionNumberBin}
        },
        data = Data
    }).


-spec delete_revision(
    aai:auth(),
    od_atm_workflow_schema:id(),
    atm_workflow_schema_revision:revision_number() | binary()
) ->
    ok | errors:error().
delete_revision(Auth, AtmWorkflowSchemaId, RevisionNumber) when is_integer(RevisionNumber) ->
    delete_revision(Auth, AtmWorkflowSchemaId, integer_to_binary(RevisionNumber));
delete_revision(Auth, AtmWorkflowSchemaId, RevisionNumberBin) when is_binary(RevisionNumberBin) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{
            type = od_atm_workflow_schema,
            id = AtmWorkflowSchemaId,
            aspect = {revision, RevisionNumberBin}
        }
    }).


-spec dump_revision(
    aai:auth(),
    od_atm_workflow_schema:id(),
    atm_workflow_schema_revision:revision_number() | binary()
) ->
    {ok, json_utils:json_map()} | errors:error().
dump_revision(Auth, AtmWorkflowSchemaId, RevisionNumber) when is_integer(RevisionNumber) ->
    dump_revision(Auth, AtmWorkflowSchemaId, integer_to_binary(RevisionNumber));
dump_revision(Auth, AtmWorkflowSchemaId, RevisionNumberBin) when is_binary(RevisionNumberBin) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{
            type = od_atm_workflow_schema,
            id = AtmWorkflowSchemaId,
            aspect = {dump_revision, RevisionNumberBin}
        }
    })).


-spec delete(aai:auth(), od_atm_workflow_schema:id()) ->
    ok | errors:error().
delete(Auth, AtmWorkflowSchemaId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_atm_workflow_schema, id = AtmWorkflowSchemaId, aspect = instance}
    }).


-spec exists(od_atm_workflow_schema:id()) -> boolean().
exists(AtmWorkflowSchemaId) ->
    {ok, Exists} = od_atm_workflow_schema:exists(AtmWorkflowSchemaId),
    Exists.

