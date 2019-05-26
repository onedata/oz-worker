%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of group REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(group_routes).

-include("rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of group REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% Create new group
    %% This operation does not require any specific privileges.
    {<<"/groups">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    %% List all groups
    %% This operation requires one of the following privileges:
    %% - oz_groups_list
    {<<"/groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = undefined, aspect = list}
    }},
    %% Get group details
    %% This operation requires one of the following privileges:
    %% - oz_groups_list
    {<<"/groups/:id">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    %% Modify group details
    %% This operation requires one of the following privileges:
    %% - group_update
    {<<"/groups/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = instance}
    }},
    %% Remove group
    %% This operation requires one of the following privileges:
    %% - group_delete
    {<<"/groups/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = instance}
    }},
    %% List group's Onezone privileges
    %% This operation requires one of the following privileges:
    %% - oz_view_privileges
    {<<"/groups/:id/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = oz_privileges}
    }},
    %% Remove group's Onezone privileges
    %% This operation requires one of the following privileges:
    %% - oz_set_privileges
    {<<"/groups/:id/privileges">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = oz_privileges}
    }},
    %% Set group's Onezone privileges
    %% This operation requires one of the following privileges:
    %% - oz_set_privileges
    {<<"/groups/:id/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = oz_privileges}
    }},
    %% List group's effective privileges
    %% This operation requires one of the following privileges:
    %% - oz_view_privileges
    {<<"/groups/:id/effective_privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_oz_privileges}
    }},
    %% List group users
    %% This operation requires one of the following privileges:
    %% - group_view
    %% - oz_groups_list_users
    {<<"/groups/:id/users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = users}
    }},
    %% Create user invite token for group
    %% This operation requires one of the following privileges:
    %% - group_invite_user
    {<<"/groups/:id/users/token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = invite_user_token}
    }},
    %% Add user to group
    %% This operation requires one of the following privileges:
    %% - oz_groups_add_members
    {<<"/groups/:id/users/:uid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% Get group user details
    %% This operation requires one of the following privileges:
    %% - group_view,
    %% - oz_groups_list_users
    {<<"/groups/:id/users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% Remove user from group
    %% This operation requires one of the following privileges:
    %% - group_remove_user
    %% - oz_groups_remove_members
    {<<"/groups/:id/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% List user's group privileges
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% Set user's group privileges
    %% This operation requires one of the following privileges:
    %% - group_set_privileges
    {<<"/groups/:id/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% List effective group users
    %% This operation requires one of the following privileges:
    %% - group_view
    %% - oz_groups_list_users
    {<<"/groups/:id/effective_users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_users}
    }},
    %% Get effective group user details
    %% This operation requires one of the following privileges:
    %% - group_view
    %% - oz_group_list_users
    {<<"/groups/:id/effective_users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% List user group privileges
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    %% Create new group for the current group
    %% This operation does not require any specific privileges.
    {<<"/groups/:id/parents">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = undefined, aspect = instance},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    %% List parent groups
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/parents">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = parents}
    }},
    %% Join parent group
    %% This operation requires one of the following privileges:
    %% - group_join_group
    {<<"/groups/:id/parents/join">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = undefined, aspect = join},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    %% Get parent group details
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/parents/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% Leave parent group
    %% This operation requires one of the following privileges:
    %% - group_update
    {<<"/groups/:id/parents/:pid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {parent, ?BINDING(pid)}}
    }},
    %% List effective parent groups
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/effective_parents">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_parents}
    }},
    %% Get effective parent group details
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/effective_parents/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% Get subgroups
    %% This operation requires one of the following privileges:
    %% - group_view
    %% - oz_groups_list_groups
    {<<"/groups/:id/children">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = children}
    }},
    %% Create child group invitation token
    %% This operation requires one of the following privileges:
    %% - group_invite_group
    {<<"/groups/:id/children/token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = invite_group_token}
    }},
    %% Add child group
    %% This operation requires one of the following privileges:
    %% - oz_groups_add_members
    {<<"/groups/:id/children/:cid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child, ?BINDING(cid)}}
    }},
    %% Get subgroup details
    %% This operation requires one of the following privileges:
    %% - group_view
    %% - oz_groups_list_groups
    {<<"/groups/:id/children/:cid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(cid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% Remove subgroup
    %% This operation requires one of the following privileges:
    %% - group_remove_group
    %% - oz_groups_remove_members
    {<<"/groups/:id/children/:cid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child, ?BINDING(cid)}}
    }},
    %% List child group privileges
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/children/:cid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child_privileges, ?BINDING(cid)}}
    }},
    %% Set subgroup privileges
    %% This operation requires one of the following privileges:
    %% - group_set_privileges
    {<<"/groups/:id/children/:cid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child_privileges, ?BINDING(cid)}}
    }},
    %% Get effective child groups
    %% This operation requires one of the following privileges:
    %% - group_view
    %% - oz_groups_list_groups
    {<<"/groups/:id/effective_children">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_children}
    }},
    %% Get effective child group details
    %% This operation requires one of the following privileges:
    %% - group_view
    %% - oz_group_list_groups
    {<<"/groups/:id/effective_children/:cid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(cid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% List effective child group privileges
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/effective_children/:cid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {eff_child_privileges, ?BINDING(cid)}}
    }},
    %% Create new space for group
    %% This operation requires one of the following privileges:
    %% - group_create_space
    {<<"/groups/:id/spaces">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = undefined, aspect = instance},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    %% List group's spaces
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = spaces}
    }},
    %% Join space by group
    %% This operation requires one of the following privileges:
    %% - group_join_space
    {<<"/groups/:id/spaces/join">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = undefined, aspect = join},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    %% Get group's space details
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% Remove group from space
    %% This operation requires one of the following privileges:
    %% - group_leave_space
    {<<"/groups/:id/spaces/:sid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {space, ?BINDING(sid)}}
    }},
    %% List effective group's spaces
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/effective_spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_spaces}
    }},
    %% Get effective group space details
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/effective_spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% List effective group's providers
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/effective_providers">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_providers}
    }},
    %% Get effective group's provider details
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/effective_providers/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% Add group handle service
    %% This operation requires one of the following privileges:
    %% - oz_handle_services_create
    {<<"/groups/:id/handle_services">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle_service, id = undefined, aspect = instance},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    %% List group handle services
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/handle_services">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = handle_services}
    }},
    %% Get group handle service details
    %% This operation does not require any specific privileges.
    {<<"/groups/:id/handle_services/:hsid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% Remove group handle service
    %% This operation requires one of the following privileges:
    %% - group_update
    {<<"/groups/:id/handle_services/:hsid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {handle_service, ?BINDING(hsid)}}
    }},
    %% List effective group handle services
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/effective_handle_services">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_handle_services}
    }},
    %% Get effective group handle service details
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/effective_handle_services/:hsid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% Create new group handle
    %% This operation requires one of the following privileges:
    %% - handle_service_register_handle
    {<<"/groups/:id/handles">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle, id = undefined, aspect = instance},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    %% List group handles
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/handles">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = handles}
    }},
    %% Get group handle details
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/handles/:hid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    %% Remove group handle
    %% This operation requires one of the following privileges:
    %% - group_update
    {<<"/groups/:id/handles/:hid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {handle, ?BINDING(hid)}}
    }},
    %% List effective group handles
    %% This operation requires one of the following privileges:
    %% - group_view
    {<<"/groups/:id/effective_handles">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_handles}
    }},
    %% Get effective group handle details
    %% This operation does not require any specific privileges.
    {<<"/groups/:id/effective_handles/:hid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }}
].
