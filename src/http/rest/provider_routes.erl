%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of provider REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(provider_routes).

-include("rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of provider REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    {<<"/providers">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, aspect = list}
    }},
    {<<"/providers/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance, scope = protected}
    }},
    {<<"/providers/:pid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance}
    }},
    {<<"/providers/:pid/effective_users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = eff_users}
    }},
    {<<"/providers/:pid/effective_users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(pid))
    }},
    {<<"/providers/:pid/effective_groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = eff_groups}
    }},
    {<<"/providers/:pid/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(pid))
    }},
    {<<"/providers/:pid/spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = spaces}
    }},
    {<<"/providers/:pid/spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(pid))
    }},
    {<<"/provider">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, aspect = instance}
    }},
    {<<"/provider">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = instance, scope = protected}
    }},
    {<<"/provider">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = instance}
    }},
    {<<"/provider">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = instance}
    }},
    {<<"/provider_dev">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, aspect = instance_dev}
    }},
    {<<"/provider/spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = spaces}
    }},
    {<<"/provider/spaces/support">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = support}
    }},
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?CLIENT_ID)
    }},
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = {space, ?BINDING(sid)}}
    }},
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = {space, ?BINDING(sid)}}
    }},
    {<<"/provider/test/check_my_ip">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, aspect = {check_my_ip, ?CLIENT_IP}}
    }},
    {<<"/provider/test/get_current_time">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, aspect = current_time}
    }},
    {<<"/provider/test/check_my_ports">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, aspect = check_my_ports}
    }},
    {<<"/provider/test/map_idp_group">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, aspect = map_idp_group}
    }}
].
