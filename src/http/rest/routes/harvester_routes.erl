%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of harvester REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(harvester_routes).

-include("rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of harvester REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% Create new harvester
    %% This operation requires one of the following privileges:
    %% - oz_harvesters_create
    {<<"/harvesters">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_harvester, id = undefined, aspect = instance}
    }},
    %% List all harvesters
    %% This operation requires one of the following privileges:
    %% - oz_harvesters_list
    {<<"/harvesters">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = undefined, aspect = list}
    }},
    %% Get harvester details
    %% This operation requires one of the following privileges:
    %% - oz_harvesters_view
    {<<"/harvesters/:id">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    %% Modify harvester details
    %% This operation requires one of the following privileges:
    %% - harvester_update
    %% - oz_harvesters_update
    {<<"/harvesters/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = instance}
    }},
    %% Remove harvester
    %% This operation requires one of the following privileges:
    %% - harvester_delete
    %% - oz_harvesters_delete
    {<<"/harvesters/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = instance}
    }},
    %% List harvester users
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_harvesters_list_relationships
    {<<"/harvesters/:id/users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = users}
    }},
    %% Create harvester user invite token
    %% This operation requires one of the following privileges:
    %% - harvester_invite_user
    %% - oz_harvesters_add_relationships
    {<<"/harvesters/:id/users/token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = invite_user_token}
    }},
    %% Add user to harvester
    %% This operation requires one of the following privileges:
    %% - harvester_invite_user
    %% - harvester_set_privileges
    %% - oz_harvesters_add_relationships
    %% - oz_users_add_relationships
    %% - oz_harvesters_set_privileges
    {<<"/harvesters/:id/users/:uid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% Get harvester user details
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_users_view
    {<<"/harvesters/:id/users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HARVESTER(?BINDING(id))
    }},
    %% Remove user from harvester
    %% This operation requires one of the following privileges:
    %% - harvester_remove_user
    %% - oz_harvesters_remove_relationships
    %% - oz_users_remove_relationships
    {<<"/harvesters/:id/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    %% List user's harvester privileges
    %% This operation requires one of the following privileges:
    %% - harvester_view_privileges
    %% - oz_harvesters_view_privileges
    {<<"/harvesters/:id/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% Update user's harvester privileges
    %% This operation requires one of the following privileges:
    %% - harvester_set_privileges
    %% - oz_set_privileges
    {<<"/harvesters/:id/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    %% List effective harvester users
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_harvesters_list_relationships
    {<<"/harvesters/:id/effective_users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = eff_users}
    }},
    %% Get effective harvester user details
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_users_view
    {<<"/harvesters/:id/effective_users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HARVESTER(?BINDING(id))
    }},
    %% List effective user's harvester privileges
    %% This operation requires one of the following privileges:
    %% - harvester_view_privileges
    %% - oz_harvesters_view_privileges
    {<<"/harvesters/:id/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    %% Get effective user's harvester membership intermediaries
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_harvesters_view
    {<<"/harvesters/:id/effective_users/:uid/membership">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {eff_user_membership, ?BINDING(uid)}}
    }},
    %% Create group in harvester
    %% This operation requires one of the following privileges:
    %% - harvester_add_group
    %% - oz_groups_create
    %% - oz_harvesters_add_relationships
    {<<"/harvesters/:id/groups">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = group},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    %% List harvester groups
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_harvesters_list_relationships
    {<<"/harvesters/:id/groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = groups}
    }},
    %% Create harvester invite token for group
    %% This operation requires one of the following privileges:
    %% - harvester_add_group
    %% - oz_harvesters_add_relationships
    {<<"/harvesters/:id/groups/token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = invite_group_token}
    }},
    %% Add group to harvester
    %% This operation requires one of the following privileges:
    %% - harvester_add_group
    %% - harvester_set_privileges
    %% - oz_harvesters_add_relationships
    %% - oz_groups_add_relationships
    %% - oz_harvester_set_privileges
    {<<"/harvesters/:id/groups/:gid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% Get harvester's group details
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_groups_view
    {<<"/harvesters/:id/groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HARVESTER(?BINDING(id))
    }},
    %% Remove group from harvester
    %% This operation requires one of the following privileges:
    %% - harvester_remove_group
    %% - oz_harvesters_remove_relationships
    {<<"/harvesters/:id/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    %% List group's harvester privileges
    %% This operation requires one of the following privileges:
    %% - harvester_view_privileges
    %% - oz_harvesters_view_privileges
    {<<"/harvesters/:id/groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% Update group privileges to harvester
    %% This operation requires one of the following privileges:
    %% - harvester_set_privileges
    %% - oz_harvesters_set_privileges
    {<<"/harvesters/:id/groups/:gid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    %% List effective harvester groups
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_harvesters_list_relationships
    {<<"/harvesters/:id/effective_groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = eff_groups}
    }},
    %% Get effective harvester group details
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_groups_view
    {<<"/harvesters/:id/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HARVESTER(?BINDING(id))
    }},
    %% List effective group's harvester privileges
    %% This operation requires one of the following privileges:
    %% - harvester_view_privileges
    %% - oz_harvesters_view_privileges
    {<<"/harvesters/:id/effective_groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }},
    %% Get effective group's harvester membership intermediaries
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_harvesters_view
    {<<"/harvesters/:id/effective_groups/:gid/membership">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {eff_group_membership, ?BINDING(gid)}}
    }},
    %% List harvester spaces
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_harvesters_list_relationships
    {<<"/harvesters/:id/spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = spaces}
    }},
    %% Create harvester invite token for space
    %% This operation requires one of the following privileges:
    %% - harvester_invite_space
    %% - oz_harvesters_add_relationships
    {<<"/harvesters/:id/spaces/token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = invite_space_token}
    }},
    %% Add space to harvester
    %% This operation requires one of the following privileges:
    %% - harvester_add_space
    %% - oz_harvesters_add_relationships
    %% - oz_spaces_add_relationships
    {<<"/harvesters/:id/spaces/:sid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {space, ?BINDING(sid)}}
    }},
    %% Get harvester space details
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_spaces_view
    {<<"/harvesters/:id/spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_HARVESTER(?BINDING(id))
    }},
    %% Remove harvester space
    %% This operation requires one of the following privileges:
    %% - harvester_remove_space
    %% - oz_harvesters_remove_relationships
    {<<"/harvesters/:id/spaces/:sid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {space, ?BINDING(sid)}}
    }},
    %% Get harvester configuration
    %% This operation requires one of the following privileges:
    %% - oz_harvesters_view
    {<<"/harvesters/:id/config">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = gui_plugin_config, scope = private}
    }},
    %% Modify harvester configuration
    %% This operation requires one of the following privileges:
    %% - harvester_update
    %% - oz_harvesters_update
    {<<"/harvesters/:id/config">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = gui_plugin_config}
    }},
    %% Remove harvester with data
    %% This operation requires one of the following privileges:
    %% - harvester_delete
    %% - oz_harvesters_delete
    {<<"/harvesters/:id/data">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = instance_with_data}
    }},
    %% Create new index in harvester
    %% This operation requires one of the following privileges:
    %% - oz_harvesters_update
    {<<"/harvesters/:id/indices">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = index}
    }},
    %% List harvester indices
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_harvesters_view
    {<<"/harvesters/:id/indices">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = indices}
    }},
    %% Get harvester index details
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_harvesters_view
    {<<"/harvesters/:id/indices/:idx">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {index, ?BINDING(idx)}, scope = private}
    }},
    %% Modify harvester index
    %% This operation requires one of the following privileges:
    %% - harvester_update
    %% - oz_harvesters_update
    {<<"/harvesters/:id/indices/:idx">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {index, ?BINDING(idx)}}
    }},
    %% Remove harvester index
    %% This operation requires one of the following privileges:
    %% - harvester_update
    %% - oz_harvesters_update
    {<<"/harvesters/:id/indices/:idx">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {index, ?BINDING(idx)}}
    }},
    %% Query harvester index.
    %% This operation requires one of the following privileges:
    %% - oz_harvesters_view
    {<<"/harvesters/:id/indices/:idx/query">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {query, ?BINDING(idx)}}
    }},
    %% Remove harvester index with data
    %% This operation requires one of the following privileges:
    %% - harvester_update
    %% - oz_harvesters_update
    {<<"/harvesters/:id/indices/:idx/data">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {index_with_data, ?BINDING(idx)}}
    }},
    %% Get harvester index details
    %% This operation requires one of the following privileges:
    %% - harvester_view
    %% - oz_harvesters_view
    {<<"/harvesters/:id/indices/:idx/progress">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_harvester, id = ?BINDING(id), aspect = {index_progress, ?BINDING(idx)}, scope = private}
    }}
].
