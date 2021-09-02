%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of automation workflow schema REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(atm_workflow_schema_routes).

-include("http/rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of atm_workflow_schema REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% Create new atm_workflow_schema
    %% This operation requires one of the following privileges:
    %% - atm_inventory_manage_workflow_schemas
    %% - oz_atm_inventories_update
    {<<"/atm_workflow_schemas">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_atm_workflow_schema, id = undefined, aspect = instance}
    }},
    %% List all atm_workflow_schemas
    %% This operation requires one of the following privileges:
    %% - oz_atm_inventories_view
    {<<"/atm_workflow_schemas">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_workflow_schema, id = undefined, aspect = list}
    }},
    %% Get atm_workflow_schema details
    %% This operation requires one of the following privileges:
    %% - oz_atm_inventories_view
    {<<"/atm_workflow_schemas/:id">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_workflow_schema, id = ?BINDING(id), aspect = instance, scope = private}
    }},
    %% Modify atm_workflow_schema details
    %% This operation requires one of the following privileges:
    %% - atm_inventory_manage_workflow_schemas
    %% - oz_atm_inventories_update
    {<<"/atm_workflow_schemas/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_atm_workflow_schema, id = ?BINDING(id), aspect = instance}
    }},
    %% Delete atm_workflow_schema
    %% This operation requires one of the following privileges:
    %% - atm_inventory_manage_workflow_schemas
    %% - oz_atm_inventories_update
    {<<"/atm_workflow_schemas/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_atm_workflow_schema, id = ?BINDING(id), aspect = instance}
    }},
    %% Get atm_lambdas referenced by atm_workflow_schema
    %% This operation requires one of the following privileges:
    %% - oz_atm_inventories_view
    {<<"/atm_workflow_schemas/:id/atm_lambdas">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_workflow_schema, id = ?BINDING(id), aspect = atm_lambdas, scope = private}
    }},
    %% Dump atm_workflow_schema to JSON
    %% This operation requires one of the following privileges:
    %% - oz_atm_inventories_view
    {<<"/atm_workflow_schemas/:id/dump">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_workflow_schema, id = ?BINDING(id), aspect = dump, scope = private}
    }}
].
