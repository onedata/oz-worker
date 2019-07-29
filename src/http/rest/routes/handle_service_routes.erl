%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of handle_service REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(handle_service_routes).

-include("http/rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of handle_service REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% Add handle service
    %% This operation requires one of the following privileges:
    %% - oz_handle_services_create
    {<<"/handle_services">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle_service, id = undefined, aspect = instance}
    }},
    %% List handle services
    %% This operation requires one of the following privileges:
    %% - oz_handle_services_list
    {<<"/handle_services">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = undefined, aspect = list}
    }},
    %% Get all handle service privileges.
    %% This operation requires one of the following privileges:
    {<<"/handle_services/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = undefined, aspect = privileges}
    }},
    %% Get handle service
    %% This operation requires one of the following privileges:
    %% - oz_handle_services_view
    {<<"/handle_services/:id">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    %% Modify handle service
    %% This operation requires one of the following privileges:
    %% - handle_service_update
    %% - oz_handle_services_update
    {<<"/handle_services/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = instance}
    }},
    %% Unregister handle service
    %% This operation requires one of the following privileges:
    %% - handle_service_delete
    %% - oz_handle_services_delete
    {<<"/handle_services/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = instance}
    }},
    %% Get handle service users
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_handle_services_list_relationships
    {<<"/handle_services/:id/users">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = users}
    }},
    %% Add handle service user
    %% This operation requires one of the following privileges:
    %% - handle_service_update
    %% - oz_handle_services_add_relationships
    %% - oz_users_add_relationships
    {<<"/handle_services/:id/users/:uid">>, #rest_req{
        method = 'PUT',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% Get handle service user
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_users_view
    {<<"/handle_services/:id/users/:uid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    %% Remove handle service user
    %% This operation requires one of the following privileges:
    %% - handle_service_update
    %% - oz_handle_services_remove_relationships
    %% - oz_users_remove_relationships
    {<<"/handle_services/:id/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% List user's handle service privileges
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_handle_services_view_privileges
    {<<"/handle_services/:id/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% Update user's handle service privileges
    %% This operation requires one of the following privileges:
    %% - handle_service_update
    %% - oz_handle_services_set_privileges
    {<<"/handle_services/:id/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% Get effective handle service users
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_handle_services_list_relationships
    {<<"/handle_services/:id/effective_users">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = eff_users}
    }},
    %% Get effective handle service user
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_users_view
    {<<"/handle_services/:id/effective_users/:uid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    %% List effective user's handle service privileges
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_handle_services_view_privileges
    {<<"/handle_services/:id/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    %% List handle service groups
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_handle_services_list_relationships
    {<<"/handle_services/:id/groups">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = groups}
    }},
    %% Add handle service group
    %% This operation requires one of the following privileges:
    %% - handle_service_update
    %% - oz_handle_services_add_relationships
    %% - oz_groups_add_relationships
    {<<"/handle_services/:id/groups/:gid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% Get handle service group details
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_groups_view
    {<<"/handle_services/:id/groups/:gid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    %% Remove handle service group
    %% This operation requires one of the following privileges:
    %% - handle_service_update
    %% - oz_handle_services_remove_relationships
    %% - oz_users_remove_relationships
    {<<"/handle_services/:id/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% List group's handle service privileges
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_handle_services_view_privileges
    {<<"/handle_services/:id/groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% Update group's handle service privileges
    %% This operation requires one of the following privileges:
    %% - handle_service_update
    %% - oz_handle_services_set_privileges
    {<<"/handle_services/:id/groups/:gid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% List effective handle service groups
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_handle_services_list_relationships
    {<<"/handle_services/:id/effective_groups">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = eff_groups}
    }},
    %% Get effective handle service group
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_groups_view
    {<<"/handle_services/:id/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    %% List effective group's handle service privileges
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_handle_services_view_privileges
    {<<"/handle_services/:id/effective_groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }},
    %% List handle service handles
    %% This operation requires one of the following privileges:
    %% - handle_service_list_handles
    %% - oz_handle_services_list_relationships
    {<<"/handle_services/:id/handles">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = handles}
    }},
    %% Get handle from handle service
    %% This operation requires one of the following privileges:
    %% - handle_service_view
    %% - oz_handles_view
    {<<"/handle_services/:id/handles/:hid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }}
].
