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

-include("http/rest.hrl").

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
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = list}
    }},
    %% Register provider
    %% This operation does not require any specific privileges.
    {<<"/providers">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = instance}
    }},
    %% Get provider details
    %% This operation requires one of the following privileges:
    %% - oz_providers_view
    {<<"/providers/:id">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    %% Remove provider
    %% This operation requires one of the following privileges:
    %% - oz_providers_delete
    {<<"/providers/:id">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = instance}
    }},
    %% List effective users of provider
    %% This operation requires one of the following privileges:
    %% - oz_providers_list_relationships
    {<<"/providers/:id/effective_users">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = eff_users}
    }},
    %% Get effective user of provider
    %% This operation requires one of the following privileges:
    %% - oz_users_view
    {<<"/providers/:id/effective_users/:uid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(id))
    }},
    %% Get effective user's provider membership intermediaries
    %% This operation requires one of the following privileges:
    %% - provider_view
    %% - oz_providers_view
    {<<"/providers/:id/effective_users/:uid/membership">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = {eff_user_membership, ?BINDING(uid)}}
    }},
    %% List effective groups of provider
    %% This operation requires one of the following privileges:
    %% - oz_providers_list_relationships
    {<<"/providers/:id/effective_groups">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = eff_groups}
    }},
    %% Get group of provider
    %% This operation requires one of the following privileges:
    %% - oz_groups_view
    {<<"/providers/:id/effective_groups/:gid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(id))
    }},
    %% Get effective group's provider membership intermediaries
    %% This operation requires one of the following privileges:
    %% - provider_view
    %% - oz_providers_view
    {<<"/providers/:id/effective_groups/:gid/membership">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = {eff_group_membership, ?BINDING(gid)}}
    }},
    %% List provider's supported spaces
    %% This operation requires one of the following privileges:
    %% - oz_providers_list_relationships
    {<<"/providers/:id/spaces">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = spaces}
    }},
    %% Get space supported by provider
    %% This operation requires one of the following privileges:
    %% - oz_spaces_view
    {<<"/providers/:id/spaces/:sid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(id))
    }},
    %% Get provider's domain config
    %% This operation requires one of the following privileges:
    %% - oz_providers_view
    {<<"/providers/:id/domain_config">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = domain_config}
    }},
    %% Get current provider details
    %% This operation does not require any specific privileges.
    {<<"/provider">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
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
    %% List current provider's supported spaces
    %% This operation does not require any specific privileges.
    {<<"/provider/spaces">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = spaces}
    }},
    %% Add space storage support
    %% This operation does not require any specific privileges.
    {<<"/provider/spaces/support">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = support}
    }},
    %% Get space details by provider
    %% This operation does not require any specific privileges.
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?CLIENT_ID)
    }},
    %% Modify supported space
    %% This operation does not require any specific privileges.
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = 'PATCH',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = {space, ?BINDING(sid)}}
    }},
    %% Remove space support
    %% This operation does not require any specific privileges.
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = {space, ?BINDING(sid)}}
    }},
    %% Show client IP address
    %% This operation does not require any specific privileges.
    {<<"/provider/public/check_my_ip">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = {check_my_ip, ?CLIENT_IP}}
    }},
    %% Check ports availability
    %% This operation does not require any specific privileges.
    {<<"/provider/public/check_my_ports">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = check_my_ports}
    }},
    %% Show current clock time
    %% This operation does not require any specific privileges.
    {<<"/provider/public/get_current_time">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = current_time}
    }},
    %% Maps external IdP group id into internal group id in OneZone
    %% This operation does not require any specific privileges.
    {<<"/provider/public/map_idp_group">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = map_idp_group}
    }},
    %% Verifies the identity of given provider
    %% This operation does not require any specific privileges.
    {<<"/provider/public/verify_provider_identity">>, #rest_req{
        method = 'POST',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = undefined, aspect = verify_provider_identity}
    }},
    %% Get current provider's domain config
    %% This operation requires one of the following privileges:
    %% - oz_providers_view
    {<<"/provider/domain_config">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = domain_config}
    }}
].
