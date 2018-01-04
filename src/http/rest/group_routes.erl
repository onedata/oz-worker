%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
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
    {<<"/groups">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, aspect = list}
    }},
    {<<"/groups/:id">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    {<<"/groups/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/groups/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/groups/:id/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/groups/:id/privileges">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/groups/:id/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/groups/:id/effective_privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_oz_privileges}
    }},
    {<<"/groups/:id/users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = users}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/users/token">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = deprecated_invite_user_token}
    }},
    {<<"/groups/:id/users/token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = invite_user_token}
    }},
    {<<"/groups/:id/users/:uid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/groups/:id/users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/groups/:id/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    {<<"/groups/:id/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/users/:uid/privileges">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {deprecated_user_privileges, ?BINDING(uid)}}
    }},
    {<<"/groups/:id/effective_users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_users}
    }},
    {<<"/groups/:id/effective_users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/parent">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = parents}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/parent/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/parents">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = parents}
    }},
    {<<"/groups/:id/parents/join">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = undefined, aspect = join},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/parents/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/parents/:pid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {parent, ?BINDING(pid)}}
    }},
    {<<"/groups/:id/effective_parents">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_parents}
    }},
    {<<"/groups/:id/effective_parents/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = children}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/token">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = deprecated_invite_group_token}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/join">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = undefined, aspect = join},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/:nid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(nid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/:nid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child, ?BINDING(nid)}}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/:nid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child_privileges, ?BINDING(nid)}}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/:nid/privileges">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {deprecated_child_privileges, ?BINDING(nid)}}
    }},
    {<<"/groups/:id/children">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = children}
    }},
    {<<"/groups/:id/children/token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = invite_group_token}
    }},
    {<<"/groups/:id/children/:cid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child, ?BINDING(cid)}}
    }},
    {<<"/groups/:id/children/:cid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(cid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/children/:cid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child, ?BINDING(cid)}}
    }},
    {<<"/groups/:id/children/:cid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child_privileges, ?BINDING(cid)}}
    }},
    {<<"/groups/:id/children/:cid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child_privileges, ?BINDING(cid)}}
    }},
    {<<"/groups/:id/effective_children">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_children}
    }},
    {<<"/groups/:id/effective_children/:cid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(cid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/effective_children/:cid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {eff_child_privileges, ?BINDING(cid)}}
    }},
    {<<"/groups/:id/spaces">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = undefined, aspect = instance},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = spaces}
    }},
    {<<"/groups/:id/spaces/join">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = undefined, aspect = join},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/spaces/:sid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {space, ?BINDING(sid)}}
    }},
    {<<"/groups/:id/effective_spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_spaces}
    }},
    {<<"/groups/:id/effective_spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/effective_providers">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_providers}
    }},
    {<<"/groups/:id/effective_providers/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/handle_services">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle_service, id = undefined, aspect = instance},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/handle_services">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = handle_services}
    }},
    {<<"/groups/:id/handle_services/:hsid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/handle_services/:hsid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {handle_service, ?BINDING(hsid)}}
    }},
    {<<"/groups/:id/effective_handle_services">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_handle_services}
    }},
    {<<"/groups/:id/effective_handle_services/:hsid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/handles">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle, id = undefined, aspect = instance},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/handles">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = handles}
    }},
    {<<"/groups/:id/handles/:hid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/handles/:hid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {handle, ?BINDING(hid)}}
    }},
    {<<"/groups/:id/effective_handles">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_handles}
    }},
    {<<"/groups/:id/effective_handles/:hid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }}
].
