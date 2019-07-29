%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of handle REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(handle_routes).

-include("http/rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of handle REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% Register handle
    %% This operation requires one of the following privileges:
    %% - handle_service_register_handle
    %% - oz_handles_create
    {<<"/handles">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle, id = undefined, aspect = instance}
    }},
    %% List handles
    %% This operation requires one of the following privileges:
    %% - oz_handles_list
    {<<"/handles">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = undefined, aspect = list}
    }},
    %% Get all handle privileges.
    %% This operation requires one of the following privileges:
    {<<"/handles/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = undefined, aspect = privileges}
    }},
    %% Get handle
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_handles_view
    {<<"/handles/:id">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    %% Modify handle
    %% This operation requires one of the following privileges:
    %% - handle_update
    %% - oz_handles_update
    {<<"/handles/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = instance}
    }},
    %% Unregister handle
    %% This operation requires one of the following privileges:
    %% - handle_delete
    %% - oz_handles_delete
    {<<"/handles/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = instance}
    }},
    %% List handle users
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_handles_list_relationships
    {<<"/handles/:id/users">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = users}
    }},
    %% Add handle user
    %% This operation requires one of the following privileges:
    %% - handle_update
    %% - oz_handles_add_relationships
    %% - oz_users_add_relationships
    {<<"/handles/:id/users/:uid">>, #rest_req{
        method = 'PUT',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% Get handle user
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_users_view
    {<<"/handles/:id/users/:uid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(id))
    }},
    %% Remove handle user
    %% This operation requires one of the following privileges:
    %% - handle_update
    %% - oz_handles_remove_relationships
    %% - oz_users_remove_relationships
    {<<"/handles/:id/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% List user handle privileges
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_handles_view_privileges
    {<<"/handles/:id/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% Update user handle privileges
    %% This operation requires one of the following privileges:
    %% - handle_update
    %% - oz_handles_set_privileges
    {<<"/handles/:id/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% List effective handle users
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_handles_list_relationships
    {<<"/handles/:id/effective_users">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = eff_users}
    }},
    %% Get effective handle user
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_users_view
    {<<"/handles/:id/effective_users/:uid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(id))
    }},
    %% List effective user's handle privileges
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_handles_view_privileges
    {<<"/handles/:id/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    %% List handle groups
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_handles_list_relationships
    {<<"/handles/:id/groups">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = groups}
    }},
    %% Add handle group
    %% This operation requires one of the following privileges:
    %% - handle_update
    %% - oz_handles_add_relationships
    %% - oz_groups_add_relationships
    {<<"/handles/:id/groups/:gid">>, #rest_req{
        method = 'PUT',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% Get handle group
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_groups_view
    {<<"/handles/:id/groups/:gid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(id))
    }},
    %% Remove handle group
    %% This operation requires one of the following privileges:
    %% - handle_update
    %% - oz_handles_remove_relationships
    %% - oz_groups_remove_relationships
    {<<"/handles/:id/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% List group's handle privileges
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_handles_view_privileges
    {<<"/handles/:id/groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% Update handle groups privileges
    %% This operation requires one of the following privileges:
    %% - handle_update
    %% - oz_handles_set_privileges
    {<<"/handles/:id/groups/:gid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% Get effective handle groups
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_handles_list_relationships
    {<<"/handles/:id/effective_groups">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = eff_groups}
    }},
    %% Get effective handle group
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_groups_view
    {<<"/handles/:id/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(id))
    }},
    %% List effective group's handle privileges
    %% This operation requires one of the following privileges:
    %% - handle_view
    %% - oz_handles_view_privileges
    {<<"/handles/:id/effective_groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }}
].
