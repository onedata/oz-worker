%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% atm_lambda entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_rest_translator).
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
create_response(#gri{id = undefined, aspect = instance}, _, resource, {#gri{id = AtmLambdaId}, _}) ->
    rest_translator:created_reply_with_location([<<"atm_lambdas">>, AtmLambdaId]).

%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> rest_handler:rest_resp().
get_response(#gri{id = undefined, aspect = list}, AtmLambdas) ->
    rest_translator:ok_body_reply(#{<<"atm_lambdas">> => AtmLambdas});

get_response(#gri{id = AtmLambdaId, aspect = instance, scope = private}, AtmLambda) ->
    #od_atm_lambda{
        name = Name,
        summary = Summary,
        description = Description,

        operation_spec = OperationSpec,
        argument_specs = ArgumentSpecs,
        result_specs = ResultSpecs,

        creation_time = CreationTime,
        creator = Creator
    } = AtmLambda,
    rest_translator:ok_body_reply(#{
        <<"atmLambdaId">> => AtmLambdaId,

        <<"name">> => Name,
        <<"summary">> => Summary,
        <<"description">> => Description,

        <<"operationSpec">> => jsonable_record:to_json(OperationSpec, atm_lambda_operation_spec),
        <<"argumentSpecs">> => jsonable_record:list_to_json(ArgumentSpecs, atm_lambda_argument_spec),
        <<"resultSpecs">> => jsonable_record:list_to_json(ResultSpecs, atm_lambda_result_spec),

        <<"creator">> => aai:subject_to_json(utils:ensure_defined(Creator, undefined, ?SUB(nobody))),
        <<"creationTime">> => CreationTime
    });

get_response(#gri{aspect = atm_inventories}, AtmInventories) ->
    rest_translator:ok_body_reply(#{<<"atm_inventories">> => AtmInventories});

get_response(#gri{aspect = atm_workflow_schemas}, AtmWorkflowSchemas) ->
    rest_translator:ok_body_reply(#{<<"atm_workflow_schemas">> => AtmWorkflowSchemas}).
