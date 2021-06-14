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
    create/2
]).
-export([
    get/2,
    get_atm_lambdas/2,
    list/1
]).
-export([
    update/3
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


-spec delete(Auth :: aai:auth(), od_atm_workflow_schema:id()) ->
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

