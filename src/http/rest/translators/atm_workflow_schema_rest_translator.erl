%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% atm_workflow_schema entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_workflow_schema_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include("http/rest.hrl").

-export([create_response/4, get_response/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback create_response/4.
%% @end
%%--------------------------------------------------------------------
-spec create_response(entity_logic:gri(), entity_logic:auth_hint(),
    entity_logic:data_format(), Result :: term() | {entity_logic:gri(), term()} |
    {entity_logic:gri(), entity_logic:auth_hint(), term()}) -> rest_handler:rest_resp().
create_response(#gri{id = undefined, aspect = instance}, _, resource, {#gri{id = AtmWorkflowSchemaId}, _}) ->
    rest_translator:created_reply_with_location([<<"atm_workflow_schemas">>, AtmWorkflowSchemaId]);

create_response(#gri{aspect = dump}, _, value, JsonMap) ->
    rest_translator:ok_body_reply(JsonMap);

create_response(#gri{aspect = {dump_revision, _}}, _, value, JsonMap) ->
    rest_translator:ok_body_reply(JsonMap).

%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> rest_handler:rest_resp().
get_response(#gri{id = undefined, aspect = list}, AtmWorkflowSchemas) ->
    rest_translator:ok_body_reply(#{<<"atm_workflow_schemas">> => AtmWorkflowSchemas});

get_response(#gri{id = AtmWorkflowSchemaId, aspect = instance, scope = private}, AtmWorkflowSchema) ->
    #od_atm_workflow_schema{
        name = Name,
        summary = Summary,

        revision_registry = RevisionRegistry,

        original_atm_workflow_schema = OriginalAtmWorkflowSchemaId,
        atm_inventory = AtmInventoryId,
        atm_lambdas = AtmLambdaIds,

        creation_time = CreationTime,
        creator = Creator
    } = AtmWorkflowSchema,
    rest_translator:ok_body_reply(#{
        <<"atmWorkflowSchemaId">> => AtmWorkflowSchemaId,

        <<"name">> => Name,
        <<"summary">> => Summary,

        <<"revisionRegistry">> => jsonable_record:to_json(RevisionRegistry, atm_workflow_schema_revision_registry),

        <<"originalAtmWorkflowSchemaId">> => utils:undefined_to_null(OriginalAtmWorkflowSchemaId),
        <<"atmInventoryId">> => AtmInventoryId,
        <<"atmLambdas">> => AtmLambdaIds,

        <<"creator">> => aai:subject_to_json(utils:ensure_defined(Creator, undefined, ?SUB(nobody))),
        <<"creationTime">> => CreationTime
    });

get_response(#gri{aspect = atm_lambdas}, AtmLambdas) ->
    rest_translator:ok_body_reply(#{<<"atm_lambdas">> => AtmLambdas}).
