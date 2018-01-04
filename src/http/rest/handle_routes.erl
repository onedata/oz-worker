%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
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
    {<<"/handles">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, aspect = list}
    }},
    {<<"/handles">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/handles/:hndl">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = instance, scope = protected}
    }},
    {<<"/handles/:hndl">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = instance}
    }},
    {<<"/handles/:hndl">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = instance}
    }},
    {<<"/handles/:hndl/users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = users}
    }},
    {<<"/handles/:hndl/users/:uid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/handles/:hndl/users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(hndl))
    }},
    {<<"/handles/:hndl/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/handles/:hndl/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    {<<"/handles/:hndl/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    % TODO VFS-2918
    {<<"/handles/:hndl/users/:uid/privileges">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {deprecated_user_privileges, ?BINDING(uid)}}
    }},
    {<<"/handles/:hndl/effective_users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = eff_users}
    }},
    {<<"/handles/:hndl/effective_users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(hndl))
    }},
    {<<"/handles/:hndl/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    {<<"/handles/:hndl/groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = groups}
    }},
    {<<"/handles/:hndl/groups/:gid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/handles/:hndl/groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(hndl))
    }},
    {<<"/handles/:hndl/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/handles/:hndl/groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    {<<"/handles/:hndl/groups/:gid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    % TODO VFS-2918
    {<<"/handles/:hndl/groups/:gid/privileges">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {deprecated_group_privileges, ?BINDING(gid)}}
    }},
    {<<"/handles/:hndl/effective_groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = eff_groups}
    }},
    {<<"/handles/:hndl/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(hndl))
    }},
    {<<"/handles/:hndl/effective_groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hndl), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }}
].
