%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of handle_service REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(handle_service_routes).

-include("rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of handle_service REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    {<<"/handle_services">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, aspect = list}
    }},
    {<<"/handle_services">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_handle_service, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/handle_services/:id">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    {<<"/handle_services/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/handle_services/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/handle_services/:id/users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = users}
    }},
    {<<"/handle_services/:id/users/:uid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/handle_services/:id/users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    {<<"/handle_services/:id/users/:uid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/handle_services/:id/users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    {<<"/handle_services/:id/users/:uid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    {<<"/handle_services/:id/effective_users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = eff_users}
    }},
    {<<"/handle_services/:id/effective_users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    {<<"/handle_services/:id/effective_users/:uid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},
    {<<"/handle_services/:id/groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = groups}
    }},
    {<<"/handle_services/:id/groups/:gid">>, #rest_req{
        method = 'PUT',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/handle_services/:id/groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    {<<"/handle_services/:id/groups/:gid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/handle_services/:id/groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    {<<"/handle_services/:id/groups/:gid/privileges">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    {<<"/handle_services/:id/effective_groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = eff_groups}
    }},
    {<<"/handle_services/:id/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    {<<"/handle_services/:id/effective_groups/:gid/privileges">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }},
    {<<"/handle_services/:id/handles">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = handles}
    }},
    {<<"/handle_services/:id/handles/:hid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }}
].
