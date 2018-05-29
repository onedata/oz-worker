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

-include("rest.hrl").

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
    {<<"/handles">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle, id = undefined, aspect = instance}
    }},
    %% List handles
    %% This operation requires one of the following privileges:
    %% - oz_handles_list
    {<<"/handles">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = undefined, aspect = list}
    }},
    %% Get handle
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = instance, scope = protected}
    }},
    %% Modify handle
    %% This operation requires one of the following privileges:
    %% - handle_update
    {<<"/handles/:hndl">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = instance}
    }},
    %% Unregister handle
    %% This operation requires one of the following privileges:
    %% - handle_delete
    {<<"/handles/:hndl">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = instance}
    }},
    %% List handle users
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = users}
    }},
    %% Add handle user
    %% This operation requires one of the following privileges:
    %% - handle_update
    {<<"/handles/:hndl/users/:uid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {user, ?BINDING(uid)}}
    }},
    %% Get handle user
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(hndl))
    }},
    %% Remove handle user
    %% This operation requires one of the following privileges:
    %% - handle_update
    {<<"/handles/:hndl/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {user, ?BINDING(uid)}}
    }},
    %% List handle user privileges
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% Set handle user privileges
    %% This operation requires one of the following privileges:
    %% - handle_update
    {<<"/handles/:hndl/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% List effective handle users
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/effective_users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = eff_users}
    }},
    %% Get effective handle user
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/effective_users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(hndl))
    }},
    %% List effective handle user privileges
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    %% List handle groups
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = groups}
    }},
    %% Add handle group
    %% This operation requires one of the following privileges:
    %% - handle_update
    {<<"/handles/:hndl/groups/:gid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {group, ?BINDING(gid)}}
    }},
    %% Get handle group
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(hndl))
    }},
    %% Remove handle group
    %% This operation requires one of the following privileges:
    %% - handle_update
    {<<"/handles/:hndl/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {group, ?BINDING(gid)}}
    }},
    %% List handle group privileges
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% Set handle groups privileges
    %% This operation requires one of the following privileges:
    %% - handle_update
    {<<"/handles/:hndl/groups/:gid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% Get effective handle groups
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/effective_groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = eff_groups}
    }},
    %% Get effective handle group
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(hndl))
    }},
    %% List effective handle group privileges
    %% This operation requires one of the following privileges:
    %% - handle_view
    {<<"/handles/:hndl/effective_groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }}
].
