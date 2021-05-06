%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of automation inventory REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(atm_inventory_routes).

-include("http/rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of atm_inventory REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% Create new atm_inventory
    %% This operation requires one of the following privileges:
    %% - oz_inventories_create
    {<<"/atm_inventories">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_atm_inventory, id = undefined, aspect = instance}
    }},
    %% List all atm_inventories
    %% This operation requires one of the following privileges:
    %% - oz_inventories_list
    {<<"/atm_inventories">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = undefined, aspect = list}
    }},
    %% List all atm_inventory privileges
    %% This operation requires one of the following privileges:
    {<<"/atm_inventories/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = undefined, aspect = privileges}
    }},
    %% Get atm_inventory details
    %% This operation requires one of the following privileges:
    %% - oz_inventories_view
    {<<"/atm_inventories/:id">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    %% Modify atm_inventory details
    %% This operation requires one of the following privileges:
    %% - inventory_update
    %% - oz_inventories_update
    {<<"/atm_inventories/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = instance}
    }},
    %% Remove atm_inventory
    %% This operation requires one of the following privileges:
    %% - inventory_delete
    %% - oz_inventories_delete
    {<<"/atm_inventories/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = instance}
    }},
    %% List atm_inventory users
    %% This operation requires one of the following privileges:
    %% - inventory_view
    %% - oz_inventories_list_relationships
    {<<"/atm_inventories/:id/users">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = users}
    }},
    %% Add user to atm_inventory
    %% This operation requires one of the following privileges:
    %% - inventory_invite_user
    %% - inventory_set_privileges
    %% - oz_inventories_add_relationships
    %% - oz_users_add_relationships
    %% - oz_inventories_set_privileges
    {<<"/atm_inventories/:id/users/:uid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% Get atm_inventory user details
    %% This operation requires one of the following privileges:
    %% - inventory_view
    %% - oz_users_view
    {<<"/atm_inventories/:id/users/:uid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_ATM_INVENTORY(?BINDING(id))
    }},
    %% Remove user from atm_inventory
    %% This operation requires one of the following privileges:
    %% - inventory_remove_user
    %% - oz_inventories_remove_relationships
    %% - oz_users_remove_relationships
    {<<"/atm_inventories/:id/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% List user's atm_inventory privileges
    %% This operation requires one of the following privileges:
    %% - inventory_view_privileges
    %% - oz_inventories_view_privileges
    {<<"/atm_inventories/:id/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% Update user's atm_inventory privileges
    %% This operation requires one of the following privileges:
    %% - inventory_set_privileges
    %% - oz_set_privileges
    {<<"/atm_inventories/:id/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% List effective atm_inventory users
    %% This operation requires one of the following privileges:
    %% - inventory_view
    %% - oz_inventories_list_relationships
    {<<"/atm_inventories/:id/effective_users">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = eff_users}
    }},
    %% Get effective atm_inventory user details
    %% This operation requires one of the following privileges:
    %% - inventory_view
    %% - oz_users_view
    {<<"/atm_inventories/:id/effective_users/:uid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_ATM_INVENTORY(?BINDING(id))
    }},
    %% List effective user's atm_inventory privileges
    %% This operation requires one of the following privileges:
    %% - inventory_view_privileges
    %% - oz_inventories_view_privileges
    {<<"/atm_inventories/:id/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    %% Get effective user's atm_inventory membership intermediaries
    %% This operation requires one of the following privileges:
    %% - inventory_view
    %% - oz_inventories_view
    {<<"/atm_inventories/:id/effective_users/:uid/membership">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {eff_user_membership, ?BINDING(uid)}}
    }},
    %% Create group in atm_inventory
    %% This operation requires one of the following privileges:
    %% - inventory_add_group
    %% - oz_groups_create
    %% - oz_inventories_add_relationships
    {<<"/atm_inventories/:id/groups">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = group},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    %% List atm_inventory groups
    %% This operation requires one of the following privileges:
    %% - inventory_view
    %% - oz_inventories_list_relationships
    {<<"/atm_inventories/:id/groups">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = groups}
    }},
    %% Add group to atm_inventory
    %% This operation requires one of the following privileges:
    %% - inventory_add_group
    %% - inventory_set_privileges
    %% - oz_inventories_add_relationships
    %% - oz_groups_add_relationships
    %% - oz_inventory_set_privileges
    {<<"/atm_inventories/:id/groups/:gid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% Get atm_inventory's group details
    %% This operation requires one of the following privileges:
    %% - inventory_view
    %% - oz_groups_view
    {<<"/atm_inventories/:id/groups/:gid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_ATM_INVENTORY(?BINDING(id))
    }},
    %% Remove group from atm_inventory
    %% This operation requires one of the following privileges:
    %% - inventory_remove_group
    %% - oz_inventories_remove_relationships
    {<<"/atm_inventories/:id/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% List group's atm_inventory privileges
    %% This operation requires one of the following privileges:
    %% - inventory_view_privileges
    %% - oz_inventories_view_privileges
    {<<"/atm_inventories/:id/groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% Update group privileges to atm_inventory
    %% This operation requires one of the following privileges:
    %% - inventory_set_privileges
    %% - oz_inventories_set_privileges
    {<<"/atm_inventories/:id/groups/:gid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% List effective atm_inventory groups
    %% This operation requires one of the following privileges:
    %% - inventory_view
    %% - oz_inventories_list_relationships
    {<<"/atm_inventories/:id/effective_groups">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = eff_groups}
    }},
    %% Get effective atm_inventory group details
    %% This operation requires one of the following privileges:
    %% - inventory_view
    %% - oz_groups_view
    {<<"/atm_inventories/:id/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_ATM_INVENTORY(?BINDING(id))
    }},
    %% List effective group's atm_inventory privileges
    %% This operation requires one of the following privileges:
    %% - inventory_view_privileges
    %% - oz_inventories_view_privileges
    {<<"/atm_inventories/:id/effective_groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }},
    %% Get effective group's atm_inventory membership intermediaries
    %% This operation requires one of the following privileges:
    %% - inventory_view
    %% - oz_inventories_view
    {<<"/atm_inventories/:id/effective_groups/:gid/membership">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = {eff_group_membership, ?BINDING(gid)}}
    }},
    %% List atm_lambdas of atm_inventory
    %% This operation requires one of the following privileges:
    %% - oz_inventories_view
    {<<"/atm_inventories/:id/atm_lambdas">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_atm_inventory, id = ?BINDING(id), aspect = atm_lambdas}
    }}
].
