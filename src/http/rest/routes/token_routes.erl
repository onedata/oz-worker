%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of token REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(token_routes).

-include("http/rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of token REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% Examine a token
    %% This operation does not require any specific privileges.
    {<<"/tokens/examine">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = examine, scope = public}
    }},
    %% Confine a token
    %% This operation does not require any specific privileges.
    {<<"/tokens/confine">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = confine, scope = public}
    }},
    %% Verify an access token
    %% This operation does not require any specific privileges.
    {<<"/tokens/verify_access_token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = verify_access_token, scope = public}
    }},
    %% Verify an identity token
    %% This operation does not require any specific privileges.
    {<<"/tokens/verify_identity_token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = verify_identity_token, scope = public}
    }},
    %% Verify an invite token
    %% This operation does not require any specific privileges.
    {<<"/tokens/verify_invite_token">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = verify_invite_token, scope = public}
    }},
    %% List all named tokens
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/tokens/named">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = list}
    }},
    %% Delete named token
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/tokens/named/:id">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = ?BINDING(id), aspect = instance}
    }},
    %% Get named token
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/tokens/named/:id">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = ?BINDING(id), aspect = instance}
    }},
    %% Modify named token
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/tokens/named/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_token, id = ?BINDING(id), aspect = instance}
    }},
    %% Create temporary token for a user
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/users/:id/tokens/temporary">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {user_temporary_token, ?BINDING(id)}}
    }},
    %% Revoke all temporary tokens of a user
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/users/:id/tokens/temporary">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {user_temporary_tokens, ?BINDING(id)}}
    }},
    %% Create named token for a user
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/users/:id/tokens/named">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {user_named_token, ?BINDING(id)}}
    }},
    %% Delete named tokens of a user
    %% This operation does not require any specific privileges.
    {<<"/users/:id/tokens/named">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {user_named_tokens, ?BINDING(id)}}
    }},
    %% List named tokens of a user
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/users/:id/tokens/named">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {user_named_tokens, ?BINDING(id)}}
    }},
    %% Get named token of a user by name
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/users/:id/tokens/named/name/:name">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = ?BINDING(name), aspect = {user_named_token, ?BINDING(id)}}
    }},
    %% Create temporary token for current user
    %% This operation does not require any specific privileges.
    {<<"/user/tokens/temporary">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {user_temporary_token, ?CLIENT_ID}}
    }},
    %% Revoke all temporary tokens of current user
    %% This operation does not require any specific privileges.
    {<<"/user/tokens/temporary">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {user_temporary_tokens, ?CLIENT_ID}}
    }},
    %% Create named token for current user
    %% This operation does not require any specific privileges.
    {<<"/user/tokens/named">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {user_named_token, ?CLIENT_ID}}
    }},
    %% Delete named tokens of current user
    %% This operation does not require any specific privileges.
    {<<"/user/tokens/named">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {user_named_tokens, ?CLIENT_ID}}
    }},
    %% List named tokens of current user
    %% This operation does not require any specific privileges.
    {<<"/user/tokens/named">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {user_named_tokens, ?CLIENT_ID}}
    }},
    %% Get named token of current user by name
    %% This operation does not require any specific privileges.
    {<<"/user/tokens/named/name/:name">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = ?BINDING(name), aspect = {user_named_token, ?CLIENT_ID}}
    }},
    %% Create temporary token for a provider
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/providers/:id/tokens/temporary">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {provider_temporary_token, ?BINDING(id)}}
    }},
    %% Revoke all temporary tokens of a provider
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/providers/:id/tokens/temporary">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {provider_temporary_tokens, ?BINDING(id)}}
    }},
    %% Create named token for a provider
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/providers/:id/tokens/named">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {provider_named_token, ?BINDING(id)}}
    }},
    %% Delete named tokens of a provider
    %% This operation does not require any specific privileges.
    {<<"/providers/:id/tokens/named">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {provider_named_tokens, ?BINDING(id)}}
    }},
    %% List named tokens of a provider
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/providers/:id/tokens/named">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {provider_named_tokens, ?BINDING(id)}}
    }},
    %% Get named token of a provider by name
    %% This operation requires one of the following privileges:
    %% - oz_tokens_manage
    {<<"/providers/:id/tokens/named/name/:name">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = ?BINDING(name), aspect = {provider_named_token, ?BINDING(id)}}
    }},
    %% Create temporary token for current provider
    %% This operation does not require any specific privileges.
    {<<"/provider/tokens/temporary">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {provider_temporary_token, ?CLIENT_ID}}
    }},
    %% Revoke all temporary tokens of current provider
    %% This operation does not require any specific privileges.
    {<<"/provider/tokens/temporary">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {provider_temporary_tokens, ?CLIENT_ID}}
    }},
    %% Create named token for current provider
    %% This operation does not require any specific privileges.
    {<<"/provider/tokens/named">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {provider_named_token, ?CLIENT_ID}}
    }},
    %% Delete named tokens of current provider
    %% This operation does not require any specific privileges.
    {<<"/provider/tokens/named">>, #rest_req{
        method = 'DELETE',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {provider_named_tokens, ?CLIENT_ID}}
    }},
    %% List named tokens of current provider
    %% This operation does not require any specific privileges.
    {<<"/provider/tokens/named">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = undefined, aspect = {provider_named_tokens, ?CLIENT_ID}}
    }},
    %% Get named token of current provider by name
    %% This operation does not require any specific privileges.
    {<<"/provider/tokens/named/name/:name">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_token, id = ?BINDING(name), aspect = {provider_named_token, ?CLIENT_ID}}
    }}
].
