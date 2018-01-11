%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of user REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(user_routes).

-include("rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of user REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% List all users
    %% This operation requires one of the following privileges:
    %% - oz_users_list
    {<<"/users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = undefined, aspect = list}
    }},
    %% Get user details
    %% This operation requires one of the following privileges:
    %% - oz_users_list
    {<<"/users/:id">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    %% Remove user
    %% This operation requires one of the following privileges:
    %% - oz_users_delete
    {<<"/users/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = instance}
    }},
    %% List user Onezone privileges
    %% This operation requires one of the following privileges:
    %% - oz_view_privileges
    {<<"/users/:id/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = oz_privileges}
    }},
    %% Remove user's Onezone privileges
    %% This operation requires one of the following privileges:
    %% - oz_set_privileges
    {<<"/users/:id/privileges">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = oz_privileges}
    }},
    %% Set user's Onezone privileges
    %% This operation requires one of the following privileges:
    %% - oz_set_privileges
    {<<"/users/:id/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = oz_privileges}
    }},
    %% List user's effective Onezone privileges
    %% This operation requires one of the following privileges:
    %% - oz_view_privileges
    {<<"/users/:id/effective_privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = eff_oz_privileges}
    }},
    %% Get current user details
    %% This operation does not require any specific privileges.
    {<<"/user">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = instance, scope = protected}
    }},
    %% Modify current user
    %% This operation does not require any specific privileges.
    {<<"/user">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = instance}
    }},
    %% Remove current user
    %% This operation does not require any specific privileges.
    {<<"/user">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = instance}
    }},
    %% Authenticate user
    %% This operation does not require any specific privileges.
    {<<"/user/authorize">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_user, id = undefined, aspect = authorize}
    }},
    %% List current user privileges
    %% This operation requires one of the following privileges:
    %% - oz_view_privileges
    {<<"/user/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = oz_privileges}
    }},
    %% Remove current user privileges
    %% This operation requires one of the following privileges:
    %% - oz_set_privileges
    {<<"/user/privileges">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = oz_privileges}
    }},
    %% Set current user privileges
    %% This operation requires one of the following privileges:
    %% - oz_set_privileges
    {<<"/user/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = oz_privileges}
    }},
    %% List current user effective privileges
    %% This operation requires one of the following privileges:
    %% - oz_view_privileges
    {<<"/user/effective_privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_oz_privileges}
    }},
    %% Generate user access token
    %% This operation does not require any specific privileges.
    {<<"/user/client_tokens">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = client_tokens}
    }},
    %% List user access tokens
    %% This operation does not require any specific privileges.
    {<<"/user/client_tokens">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = client_tokens}
    }},
    %% Delete access token
    %% This operation does not require any specific privileges.
    {<<"/user/client_tokens/:tid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {client_token, ?BINDING(tid)}}
    }},
    %% Get default space
    %% This operation does not require any specific privileges.
    {<<"/user/default_space">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_space}
    }},
    %% Unset default space
    %% This operation does not require any specific privileges.
    {<<"/user/default_space">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_space}
    }},
    %% Set default space
    %% This operation does not require any specific privileges.
    {<<"/user/default_space">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_space}
    }},
    %% Get default provider
    %% This operation does not require any specific privileges.
    {<<"/user/default_provider">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_provider}
    }},
    %% Unset default provider
    %% This operation does not require any specific privileges.
    {<<"/user/default_provider">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_provider}
    }},
    %% Set default provider
    %% This operation does not require any specific privileges.
    {<<"/user/default_provider">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_provider}
    }},
    %% Create new group for the current user
    %% This operation does not require any specific privileges.
    {<<"/user/groups">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    %% List user groups
    %% This operation does not require any specific privileges.
    {<<"/user/groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = groups}
    }},
    %% Join group
    %% This operation does not require any specific privileges.
    {<<"/user/groups/join">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = undefined, aspect = join},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    %% Get group details
    %% This operation does not require any specific privileges.
    {<<"/user/groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    %% Leave group
    %% This operation does not require any specific privileges.
    {<<"/user/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {group, ?BINDING(gid)}}
    }},
    %% List effective user groups
    %% This operation does not require any specific privileges.
    {<<"/user/effective_groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_groups}
    }},
    %% Get effective group details
    %% This operation does not require any specific privileges.
    {<<"/user/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    %% Create new user space
    %% This operation does not require any specific privileges.
    {<<"/user/spaces">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    %% List user spaces
    %% This operation does not require any specific privileges.
    {<<"/user/spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = spaces}
    }},
    %% Join space
    %% This operation does not require any specific privileges.
    {<<"/user/spaces/join">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = undefined, aspect = join},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    %% Get space details
    %% This operation does not require any specific privileges.
    {<<"/user/spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    %% Leave space
    %% This operation does not require any specific privileges.
    {<<"/user/spaces/:sid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space, ?BINDING(sid)}}
    }},
    %% Get user space alias
    %% This operation does not require any specific privileges.
    {<<"/user/spaces/:sid/alias">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space_alias, ?BINDING(sid)}}
    }},
    %% Remove space alias
    %% This operation does not require any specific privileges.
    {<<"/user/spaces/:sid/alias">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space_alias, ?BINDING(sid)}}
    }},
    %% Set user space alias
    %% This operation does not require any specific privileges.
    {<<"/user/spaces/:sid/alias">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space_alias, ?BINDING(sid)}}
    }},
    %% List effective user spaces
    %% This operation does not require any specific privileges.
    {<<"/user/effective_spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_spaces}
    }},
    %% Get effective space details
    %% This operation does not require any specific privileges.
    {<<"/user/effective_spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    %% List user effective providers
    %% This operation does not require any specific privileges.
    {<<"/user/effective_providers">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_providers}
    }},
    %% Get effective provider details
    %% This operation does not require any specific privileges.
    {<<"/user/effective_providers/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    %% Add user handle service
    %% This operation requires one of the following privileges:
    %% - oz_handle_service_create
    {<<"/user/handle_services">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle_service, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    %% List user handle services
    %% This operation does not require any specific privileges.
    {<<"/user/handle_services">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = handle_services}
    }},
    %% Get user handle service details
    %% This operation does not require any specific privileges.
    {<<"/user/handle_services/:hsid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    %% Remove user handle service
    %% This operation does not require any specific privileges.
    {<<"/user/handle_services/:hsid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {handle_service, ?BINDING(hsid)}}
    }},
    %% List user effective handle services
    %% This operation does not require any specific privileges.
    {<<"/user/effective_handle_services">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_handle_services}
    }},
    %% Get effective handle service details
    %% This operation does not require any specific privileges.
    {<<"/user/effective_handle_services/:hsid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    %% Create new user handle
    %% This operation requires one of the following privileges:
    %% - handle_service_register_handle
    {<<"/user/handles">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    %% List user handles
    %% This operation does not require any specific privileges.
    {<<"/user/handles">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = handles}
    }},
    %% Get handle details
    %% This operation does not require any specific privileges.
    {<<"/user/handles/:hid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    %% Remove handle
    %% This operation does not require any specific privileges.
    {<<"/user/handles/:hid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {handle, ?BINDING(hid)}}
    }},
    %% Get user effective handles
    %% This operation does not require any specific privileges.
    {<<"/user/effective_handles">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_handles}
    }},
    %% Get effective handle details
    %% This operation does not require any specific privileges.
    {<<"/user/effective_handles/:hid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }}
].
