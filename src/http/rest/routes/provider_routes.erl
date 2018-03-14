%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2018 ACK CYFRONET AGH
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
    %% List providers
    %% This operation requires one of the following privileges:
    %% - oz_providers_list
    {<<"/providers">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = list}
    }},
    %% Register provider
    %% This operation does not require any specific privileges.
    {<<"/providers">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = instance}
    }},
    {<<"/providers/dev">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, aspect = instance_dev}
    }},
    %% Create provider registration token
    %% This operation requires one of the following privileges:
    %% - oz_providers_invite
    {<<"/providers/token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = provider_registration_token}
    }},
    %% Get provider details
    %% This operation requires one of the following privileges:
    %% - oz_providers_list
    {<<"/providers/:pid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance, scope = protected}
    }},
    %% Remove provider
    %% This operation requires one of the following privileges:
    %% - oz_providers_delete
    {<<"/providers/:pid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance}
    }},
    %% List effective users of provider
    %% This operation requires one of the following privileges:
    %% - oz_providers_list_users
    {<<"/providers/:pid/effective_users">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = eff_users}
    }},
    %% Get effective user of provider
    %% This operation requires one of the following privileges:
    %% - oz_providers_list_users
    {<<"/providers/:pid/effective_users/:uid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(pid))
    }},
    %% List effective groups of provider
    %% This operation requires one of the following privileges:
    %% - oz_providers_list_groups
    {<<"/providers/:pid/effective_groups">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = eff_groups}
    }},
    %% Get group of provider
    %% This operation requires one of the following privileges:
    %% - oz_providers_list_groups
    {<<"/providers/:pid/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(pid))
    }},
    %% List spaces supported by provider
    %% This operation requires one of the following privileges:
    %% - oz_providers_list_spaces
    {<<"/providers/:pid/spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = spaces}
    }},
    %% Get space supported by provider
    %% This operation requires one of the following privileges:
    %% - oz_providers_list_spaces
    {<<"/providers/:pid/spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(pid))
    }},
    %% Get provider details
    %% This operation does not require any specific privileges.
    {<<"/provider">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = instance, scope = protected}
    }},
    %% Modify provider details
    %% This operation does not require any specific privileges.
    {<<"/provider">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = instance}
    }},
    %% Unregister provider
    %% This operation does not require any specific privileges.
    {<<"/provider">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = instance}
    }},
    %% List spaces at provider
    %% This operation does not require any specific privileges.
    {<<"/provider/spaces">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = spaces}
    }},
    %% Add space storage support
    %% This operation does not require any specific privileges.
    {<<"/provider/spaces/support">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = support}
    }},
    %% Get space details by provider
    %% This operation does not require any specific privileges.
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?CLIENT_ID)
    }},
    %% Modify supported space
    %% This operation does not require any specific privileges.
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = {space, ?BINDING(sid)}}
    }},
    %% Remove space support
    %% This operation does not require any specific privileges.
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = {space, ?BINDING(sid)}}
    }},
    %% Show client IP address
    %% This operation does not require any specific privileges.
    {<<"/provider/public/check_my_ip">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = {check_my_ip, ?CLIENT_IP}}
    }},
    %% Check ports availability
    %% This operation does not require any specific privileges.
    {<<"/provider/public/check_my_ports">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = check_my_ports}
    }},
    %% Show current clock time
    %% This operation does not require any specific privileges.
    {<<"/provider/public/get_current_time">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = current_time}
    }},
    %% Maps external IdP group id into internal group id in OneZone
    %% This operation does not require any specific privileges.
    {<<"/provider/public/map_idp_group">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = map_idp_group}
    }},
    %% Verifies the identity of given provider
    %% This operation does not require any specific privileges.
    {<<"/provider/public/verify_provider_identity">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = verify_provider_identity}
    }}
].
