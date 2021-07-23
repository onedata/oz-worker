%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of automation lambda REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(atm_lambda_routes).

-include("http/rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of atm_lambda REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% Create new atm_lambda
    %% This operation requires one of the following privileges:
    %% - atm_inventory_manage_lambdas
    %% - oz_atm_inventories_update
    {<<"/atm_lambdas">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_atm_lambda, id = undefined, aspect = instance}
    }},
    %% List all atm_lambdas
    %% This operation requires one of the following privileges:
    %% - oz_atm_inventories_view
    {<<"/atm_lambdas">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_lambda, id = undefined, aspect = list}
    }},
    %% Get atm_lambda details
    %% This operation requires one of the following privileges:
    %% - oz_atm_inventories_view
    {<<"/atm_lambdas/:id">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_lambda, id = ?BINDING(id), aspect = instance, scope = private}
    }},
    %% Modify atm_lambda details
    %% This operation requires one of the following privileges:
    %% - atm_inventory_manage_lambdas
    %% - oz_atm_inventories_update
    {<<"/atm_lambdas/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_atm_lambda, id = ?BINDING(id), aspect = instance}
    }},
    %% Delete atm_lambda
    %% This operation requires one of the following privileges:
    %% - atm_inventory_manage_lambdas
    %% - oz_atm_inventories_update
    {<<"/atm_lambdas/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_atm_lambda, id = ?BINDING(id), aspect = instance}
    }},
    %% Get atm_inventories referencing atm_lambda
    %% This operation requires one of the following privileges:
    %% - oz_atm_inventories_view
    {<<"/atm_lambdas/:id/atm_inventories">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_lambda, id = ?BINDING(id), aspect = atm_inventories, scope = private}
    }},
    %% Link atm_lambda to atm_inventory
    %% This operation requires one of the following privileges:
    %% - atm_inventory_manage_lambdas
    %% - oz_atm_inventories_update
    {<<"/atm_lambdas/:id/atm_inventories/:aiid">>, #rest_req{
        method = 'PUT',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_lambda, id = ?BINDING(id), aspect = {atm_inventory, ?BINDING(aiid)}}
    }},
    %% Unlink atm_lambda from atm_inventory
    %% This operation requires one of the following privileges:
    %% - atm_inventory_manage_lambdas
    %% - oz_atm_inventories_update
    {<<"/atm_lambdas/:id/atm_inventories/:aiid">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_lambda, id = ?BINDING(id), aspect = {atm_inventory, ?BINDING(aiid)}}
    }},
    %% Get atm_workflow_schemas referencing atm_lambda
    %% This operation requires one of the following privileges:
    %% - oz_atm_inventories_view
    {<<"/atm_lambdas/:id/atm_workflow_schemas">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_lambda, id = ?BINDING(id), aspect = atm_workflow_schemas, scope = private}
    }}
].
