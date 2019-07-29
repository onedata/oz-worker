%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of cluster REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(cluster_routes).

-include("http/rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of cluster REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% List all clusters
    %% This operation requires one of the following privileges:
    %% - oz_clusters_list
    {<<"/clusters">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = undefined, aspect = list}
    }},
    %% Get all cluster privileges.
    %% This operation requires one of the following privileges:
    {<<"/clusters/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = undefined, aspect = privileges}
    }},
    %% Get cluster details
    %% This operation requires one of the following privileges:
    %% - oz_clusters_view
    {<<"/clusters/:id">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    %% Modify cluster details
    %% This operation requires one of the following privileges:
    %% - cluster_update
    %% - oz_clusters_update
    {<<"/clusters/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = instance}
    }},
    %% List cluster's users
    %% This operation requires one of the following privileges:
    %% - cluster_view
    %% - oz_clusters_list_relationships
    {<<"/clusters/:id/users">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = users}
    }},
    %% Create cluster user invite token
    %% This operation requires one of the following privileges:
    %% - cluster_add_user
    %% - oz_clusters_add_relationships
    {<<"/clusters/:id/users/token">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = invite_user_token}
    }},
    %% Add user to cluster
    %% This operation requires one of the following privileges:
    %% - cluster_add_user
    %% - cluster_set_privileges
    %% - oz_clusters_add_relationships
    %% - oz_users_add_relationships
    %% - oz_clusters_set_privileges
    {<<"/clusters/:id/users/:uid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% Get cluster's user details
    %% This operation requires one of the following privileges:
    %% - cluster_view
    %% - oz_users_view
    {<<"/clusters/:id/users/:uid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_CLUSTER(?BINDING(id))
    }},
    %% Remove user from cluster
    %% This operation requires one of the following privileges:
    %% - cluster_remove_user
    %% - oz_clusters_remove_relationships
    %% - oz_users_remove_relationships
    {<<"/clusters/:id/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% List user's cluster privileges
    %% This operation requires one of the following privileges:
    %% - cluster_view_privileges
    %% - oz_clusters_view_privileges
    {<<"/clusters/:id/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% Update user's cluster privileges
    %% This operation requires one of the following privileges:
    %% - cluster_set_privileges
    %% - oz_clusters_set_privileges
    {<<"/clusters/:id/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% List cluster's effective users
    %% This operation requires one of the following privileges:
    %% - cluster_view
    %% - oz_clusters_list_relationships
    {<<"/clusters/:id/effective_users">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = eff_users}
    }},
    %% Get cluster's effective user details
    %% This operation requires one of the following privileges:
    %% - cluster_view
    %% - oz_users_view
    {<<"/clusters/:id/effective_users/:uid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_CLUSTER(?BINDING(id))
    }},
    %% List effective user's cluster privileges
    %% This operation requires one of the following privileges:
    %% - cluster_view_privileges
    %% - oz_clusters_view_privileges
    {<<"/clusters/:id/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    %% Get effective user's cluster membership intermediaries
    %% This operation requires one of the following privileges:
    %% - cluster_view
    %% - oz_clusters_view
    {<<"/clusters/:id/effective_users/:uid/membership">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {eff_user_membership, ?BINDING(uid)}}
    }},
    %% Create group in cluster
    %% This operation requires one of the following privileges:
    %% - cluster_add_group
    %% - oz_groups_create
    %% - oz_clusters_add_relationships
    {<<"/clusters/:id/groups">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = group},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    %% List cluster's groups
    %% This operation requires one of the following privileges:
    %% - cluster_view
    %% - oz_clusters_list_relationships
    {<<"/clusters/:id/groups">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = groups}
    }},
    %% Create cluster invite token for group
    %% This operation requires one of the following privileges:
    %% - cluster_add_group
    %% - oz_clusters_add_relationships
    {<<"/clusters/:id/groups/token">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = invite_group_token}
    }},
    %% Add group to cluster
    %% This operation requires one of the following privileges:
    %% - cluster_add_group
    %% - cluster_set_privileges
    %% - oz_clusters_add_relationships
    %% - oz_groups_add_relationships
    %% - oz_clusters_set_privileges
    {<<"/clusters/:id/groups/:gid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% Get cluster group details
    %% This operation requires one of the following privileges:
    %% - cluster_view
    %% - oz_groups_view
    {<<"/clusters/:id/groups/:gid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_CLUSTER(?BINDING(id))
    }},
    %% Remove group from cluster
    %% This operation requires one of the following privileges:
    %% - cluster_remove_group
    %% - oz_clusters_remove_relationships
    {<<"/clusters/:id/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% List group's cluster privileges
    %% This operation requires one of the following privileges:
    %% - cluster_view_privileges
    %% - oz_clusters_view_privileges
    {<<"/clusters/:id/groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% Update group's privileges in a cluster
    %% This operation requires one of the following privileges:
    %% - cluster_set_privileges
    %% - oz_clusters_set_privileges
    {<<"/clusters/:id/groups/:gid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% List cluster's effective groups
    %% This operation requires one of the following privileges:
    %% - cluster_view
    %% - oz_clusters_list_relationships
    {<<"/clusters/:id/effective_groups">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = eff_groups}
    }},
    %% Get cluster's effective group details
    %% This operation requires one of the following privileges:
    %% - cluster_view
    %% - oz_groups_view
    {<<"/clusters/:id/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_CLUSTER(?BINDING(id))
    }},
    %% List effective group's cluster privileges
    %% This operation requires one of the following privileges:
    %% - cluster_view_privileges
    %% - oz_clusters_view_privileges
    {<<"/clusters/:id/effective_groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }},
    %% Get effective group's cluster membership intermediaries
    %% This operation requires one of the following privileges:
    %% - cluster_view
    %% - oz_clusters_view
    {<<"/clusters/:id/effective_groups/:gid/membership">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_cluster, id = ?BINDING(id), aspect = {eff_group_membership, ?BINDING(gid)}}
    }}
].
