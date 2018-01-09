%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
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
    {<<"/users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, aspect = list}
    }},
    {<<"/users/:id">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    {<<"/users/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/users/:id/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/users/:id/privileges">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/users/:id/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/users/:id/effective_privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = eff_oz_privileges}
    }},
    {<<"/user">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = instance, scope = protected}
    }},
    {<<"/user">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = instance}
    }},
    {<<"/user">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = instance}
    }},
    {<<"/user/authorize">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_user, aspect = authorize}
    }},
    {<<"/user/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = oz_privileges}
    }},
    {<<"/user/privileges">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = oz_privileges}
    }},
    {<<"/user/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = oz_privileges}
    }},
    {<<"/user/effective_privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_oz_privileges}
    }},
    {<<"/user/client_tokens">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = client_tokens}
    }},
    {<<"/user/client_tokens">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = client_tokens}
    }},
    {<<"/user/client_tokens/:tid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {client_token, ?BINDING(tid)}}
    }},
    {<<"/user/default_space">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_space}
    }},
    {<<"/user/default_space">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_space}
    }},
    {<<"/user/default_space">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_space}
    }},
    {<<"/user/default_provider">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_provider}
    }},
    {<<"/user/default_provider">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_provider}
    }},
    {<<"/user/default_provider">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_provider}
    }},
    {<<"/user/groups">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/user/groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = groups}
    }},
    {<<"/user/groups/join">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_group, id = undefined, aspect = join},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/user/groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/user/effective_groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_groups}
    }},
    {<<"/user/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/spaces">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/user/spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = spaces}
    }},
    {<<"/user/spaces/join">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_space, id = undefined, aspect = join},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/user/spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/spaces/:sid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space, ?BINDING(sid)}}
    }},
    {<<"/user/spaces/:sid/alias">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space_alias, ?BINDING(sid)}}
    }},
    {<<"/user/spaces/:sid/alias">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space_alias, ?BINDING(sid)}}
    }},
    {<<"/user/spaces/:sid/alias">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space_alias, ?BINDING(sid)}}
    }},
    {<<"/user/effective_spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_spaces}
    }},
    {<<"/user/effective_spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/effective_providers">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_providers}
    }},
    {<<"/user/effective_providers/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/handle_services">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle_service, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/user/handle_services">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = handle_services}
    }},
    {<<"/user/handle_services/:hsid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/handle_services/:hsid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {handle_service, ?BINDING(hsid)}}
    }},
    {<<"/user/effective_handle_services">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_handle_services}
    }},
    {<<"/user/effective_handle_services/:hsid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/handles">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/user/handles">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = handles}
    }},
    {<<"/user/handles/:hid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/handles/:hid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {handle, ?BINDING(hid)}}
    }},
    {<<"/user/effective_handles">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_handles}
    }},
    {<<"/user/effective_handles/:hid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }}
].
