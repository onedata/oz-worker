%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of space REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(space_routes).

-include("http/rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of space REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% Create new space
    %% This operation requires one of the following privileges:
    %% - oz_spaces_create
    {<<"/spaces">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = undefined, aspect = instance}
    }},
    %% List all spaces
    %% This operation requires one of the following privileges:
    %% - oz_spaces_list
    {<<"/spaces">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = undefined, aspect = list}
    }},
    %% List all space privileges
    %% This operation requires one of the following privileges:
    {<<"/spaces/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = undefined, aspect = privileges}
    }},
    %% List space marketplace
    %% This operation does not require any specific privileges.
    {<<"/spaces/marketplace/list">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = undefined, aspect = list_marketplace, scope = protected}
    }},
    %% Get space details in marketplace
    %% This operation does not require any specific privileges.
    {<<"/spaces/marketplace/:id">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = marketplace_data, scope = protected}
    }},
    %% TODO VFS-10687 swaggers for space marketplace
    {<<"/spaces/marketplace/:id/request">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = membership_request}
    }},
    %% TODO VFS-10687 swaggers for space marketplace
    {<<"/spaces/marketplace/:id/request/:rid/requester_info">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {membership_requester_info, ?BINDING(rid)}}
    }},
    %% TODO VFS-10687 swaggers for space marketplace
    {<<"/spaces/marketplace/:id/request/:rid/resolve">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {resolve_membership_request, ?BINDING(rid)}}
    }},
    %% Get space details
    %% This operation requires one of the following privileges:
    %% - oz_spaces_view
    {<<"/spaces/:id">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    %% Modify space details
    %% This operation requires one of the following privileges:
    %% - space_update
    %% - oz_spaces_update
    {<<"/spaces/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = instance}
    }},
    %% Remove space
    %% This operation requires one of the following privileges:
    %% - space_delete
    %% - oz_spaces_delete
    {<<"/spaces/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = instance}
    }},
    %% List space users
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_spaces_list_relationships
    {<<"/spaces/:id/users">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = users}
    }},
    %% Create space user invite token
    %% This operation requires one of the following privileges:
    %% - space_add_user
    %% - oz_spaces_add_relationships
    {<<"/spaces/:id/users/token">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = invite_user_token}
    }},
    %% Add user to space
    %% This operation requires one of the following privileges:
    %% - space_add_user
    %% - space_set_privileges
    %% - oz_spaces_add_relationships
    %% - oz_users_add_relationships
    %% - oz_spaces_set_privileges
    {<<"/spaces/:id/users/:uid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% Get space user details
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_users_view
    {<<"/spaces/:id/users/:uid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    %% Remove user from space
    %% This operation requires one of the following privileges:
    %% - space_remove_user
    %% - oz_spaces_remove_relationships
    %% - oz_users_remove_relationships
    {<<"/spaces/:id/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% List user's space privileges
    %% This operation requires one of the following privileges:
    %% - space_view_privileges
    %% - oz_spaces_view_privileges
    {<<"/spaces/:id/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% Update user's space privileges
    %% This operation requires one of the following privileges:
    %% - space_set_privileges
    %% - oz_spaces_set_privileges
    {<<"/spaces/:id/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% List effective space users
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_spaces_list_relationships
    {<<"/spaces/:id/effective_users">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = eff_users}
    }},
    %% Get effective space user details
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_users_view
    {<<"/spaces/:id/effective_users/:uid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    %% List effective user's space privileges
    %% This operation requires one of the following privileges:
    %% - space_view_privileges
    %% - oz_spaces_view_privileges
    {<<"/spaces/:id/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    %% Get effective user's space membership intermediaries
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_spaces_view
    {<<"/spaces/:id/effective_users/:uid/membership">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {eff_user_membership, ?BINDING(uid)}}
    }},
    %% Create group in space
    %% This operation requires one of the following privileges:
    %% - space_add_group
    %% - oz_groups_create
    %% - oz_spaces_add_relationships
    {<<"/spaces/:id/groups">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = group},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    %% List space groups
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_spaces_list_relationships
    {<<"/spaces/:id/groups">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = groups}
    }},
    %% Create space invite token for group
    %% This operation requires one of the following privileges:
    %% - space_add_group
    %% - oz_spaces_add_relationships
    {<<"/spaces/:id/groups/token">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = invite_group_token}
    }},
    %% Add group to space
    %% This operation requires one of the following privileges:
    %% - space_add_group
    %% - space_set_privileges
    %% - oz_spaces_add_relationships
    %% - oz_groups_add_relationships
    %% - oz_space_set_privileges
    {<<"/spaces/:id/groups/:gid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% Get space's group details
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_groups_view
    {<<"/spaces/:id/groups/:gid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    %% Remove group from space
    %% This operation requires one of the following privileges:
    %% - space_remove_group
    %% - oz_spaces_remove_relationships
    {<<"/spaces/:id/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% List group's space privileges
    %% This operation requires one of the following privileges:
    %% - space_view_privileges
    %% - oz_spaces_view_privileges
    {<<"/spaces/:id/groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% Update group privileges to space
    %% This operation requires one of the following privileges:
    %% - space_set_privileges
    %% - oz_spaces_set_privileges
    {<<"/spaces/:id/groups/:gid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% List effective space groups
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_spaces_list_relationships
    {<<"/spaces/:id/effective_groups">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = eff_groups}
    }},
    %% Get effective space group details
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_groups_view
    {<<"/spaces/:id/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    %% List effective group's space privileges
    %% This operation requires one of the following privileges:
    %% - space_view_privileges
    %% - oz_spaces_view_privileges
    {<<"/spaces/:id/effective_groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }},
    %% Get effective group's space membership intermediaries
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_spaces_view
    {<<"/spaces/:id/effective_groups/:gid/membership">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {eff_group_membership, ?BINDING(gid)}}
    }},
    %% List space shares
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_spaces_list_relationships
    {<<"/spaces/:id/shares">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = shares}
    }},
    %% Get space share
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_shares_view
    {<<"/spaces/:id/shares/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_share, id = ?BINDING(sid), aspect = instance},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    %% List space providers
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_spaces_list_relationships
    {<<"/spaces/:id/providers">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = eff_providers}
    }},
    %% Create space support token
    %% This operation requires one of the following privileges:
    %% - space_add_support
    %% - oz_spaces_add_relationships
    {<<"/spaces/:id/providers/token">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = space_support_token}
    }},
    %% Ceases space support by provider
    %% This operation requires one of the following privileges:
    %% - space_remove_provider
    %% - oz_spaces_remove_relationships
    {<<"/spaces/:id/providers/:pid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {provider, ?BINDING(pid)}}
    }},
    %% Get space provider details
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_providers_view
    {<<"/spaces/:id/providers/:pid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    %% Update space support parameters of provider
    %% This operation requires one of the following privileges:
    %% - space_update
    %% - cluster_update
    %% - oz_spaces_update
    {<<"/spaces/:id/providers/:pid/support_parameters">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {support_parameters, ?BINDING(pid)}}
    }},
    %% List space harvesters
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_spaces_list_relationships
    {<<"/spaces/:id/harvesters">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = harvesters}
    }},
    %% Join space to a harvester
    %% This operation requires one of the following privileges:
    %% - space_add_harvester
    %% - oz_harvesters_add_relationships
    %% - oz_spaces_add_relationships
    {<<"/spaces/:id/harvesters/join">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_harvester, id = undefined, aspect = join},
        b_auth_hint = ?AS_SPACE(?BINDING(id))
    }},
    %% Get harvester details
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_harvesters_view
    {<<"/spaces/:id/harvesters/:hid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(hid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    %% Remove space from harvester.
    %% This operation requires one of the following privileges:
    %% - space_remove_harvester
    %% - oz_spaces_remove_relationships
    %% - oz_harvesters_remove_relationships
    {<<"/spaces/:id/harvesters/:hid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {harvester, ?BINDING(hid)}}
    }},
    %% List space owners
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_spaces_view
    {<<"/spaces/:id/owners">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = owners}
    }},
    %% Add space owner
    %% This operation requires one of the following privileges:
    %% - oz_spaces_set_privileges
    {<<"/spaces/:id/owners/:uid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {owner, ?BINDING(uid)}}
    }},
    %% Remove space owner
    %% This operation requires one of the following privileges:
    %% - oz_spaces_set_privileges
    {<<"/spaces/:id/owners/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {owner, ?BINDING(uid)}}
    }}
].
