%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of space REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(space_routes).

-include("rest.hrl").

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
    {<<"/spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, aspect = list}
    }},
    {<<"/spaces">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/spaces/:id">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    {<<"/spaces/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/spaces/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = instance}
    }},

    {<<"/spaces/:id/users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = users}
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/users/token">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = deprecated_invite_user_token}
    }},
    {<<"/spaces/:id/users/token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = invite_user_token}
    }},
    {<<"/spaces/:id/users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    {<<"/spaces/:id/users/:uid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/spaces/:id/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/spaces/:id/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    {<<"/spaces/:id/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/users/:uid/privileges">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {deprecated_user_privileges, ?BINDING(uid)}}
    }},
    {<<"/spaces/:id/effective_users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = eff_users}
    }},
    {<<"/spaces/:id/effective_users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    {<<"/spaces/:id/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    {<<"/spaces/:id/groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = groups}
    }},
    {<<"/spaces/:id/groups/token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = invite_group_token}
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/groups/token">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = deprecated_invite_group_token}
    }},
    {<<"/spaces/:id/groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    {<<"/spaces/:id/groups/:gid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/spaces/:id/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/spaces/:id/groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    {<<"/spaces/:id/groups/:gid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/groups/:gid/privileges">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {deprecated_group_privileges, ?BINDING(gid)}}
    }},
    {<<"/spaces/:id/effective_groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = eff_groups}
    }},
    {<<"/spaces/:id/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    {<<"/spaces/:id/effective_groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }},
    {<<"/spaces/:id/shares">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = shares}
    }},
    {<<"/spaces/:id/shares/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_share, id = ?BINDING(sid), aspect = instance, scope = private},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/shares/:shid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {deprecated_create_share, ?BINDING(shid)}}
    }},

    {<<"/spaces/:id/providers">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = providers}
    }},
    {<<"/spaces/:id/providers/token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = invite_provider_token}
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/providers/token">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = deprecated_invite_provider_token}
    }},
    {<<"/spaces/:id/providers/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    {<<"/spaces/:id/providers/:pid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {provider, ?BINDING(pid)}}
    }}
].
