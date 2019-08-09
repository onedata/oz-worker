%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017-2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module present a high level API for creating and validating access
%%% tokens issued by the Onezone service.
%%% @end
%%%-------------------------------------------------------------------
-module(access_tokens).
-author("Lukasz Opiola").

-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/api_errors.hrl").

-define(NOW(), time_utils:cluster_time_seconds()).
-define(SUPPORTED_CAVEATS, [cv_time]).

-export([create_provider_root_token/1]).
-export([verify_provider_auth/1]).
-export([verify_provider_identity/1]).
-export([invalidate_provider_root_token/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new provider root token that will be used by it for
%% authentication and authorization.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_root_token(od_provider:id()) ->
    {ok, tokens:token(), tokens:nonce()}.
create_provider_root_token(ProviderId) ->
    Secret = tokens:generate_secret(),
    {ok, Identifier} = macaroon_auth:create(
        Secret, ?PROVIDER(ProviderId)
    ),
    Token = create_provider_token(Identifier, Secret),
    {ok, Token, Identifier}.


%%--------------------------------------------------------------------
%% @doc
%% Verifies if given token carries valid provider authorization.
%% @end
%%--------------------------------------------------------------------
-spec verify_provider_auth(tokens:token()) ->
    {ok, od_provider:id()} | {error, term()}.
verify_provider_auth(Token) ->
    verify_provider_token(Token, ?SUPPORTED_CAVEATS).


%%--------------------------------------------------------------------
%% @doc
%% Verifies if given token carries valid provider identity. Can be used by
%% providers to verify each other's identity.
%% @end
%%--------------------------------------------------------------------
-spec verify_provider_identity(tokens:token()) ->
    {ok, od_provider:id()} | {error, term()}.
verify_provider_identity(Token) ->
    verify_provider_token(Token, [cv_authorization_none | ?SUPPORTED_CAVEATS]).


%%--------------------------------------------------------------------
%% @doc
%% Irreversibly invalidates a provider root token by its nonce.
%% @end
%%--------------------------------------------------------------------
-spec invalidate_provider_root_token(tokens:nonce()) -> ok.
invalidate_provider_root_token(Nonce) ->
    macaroon_auth:delete(Nonce).

%%%===================================================================
%%% Internal functions
%%%===================================================================

% @todo VFS-5554 for backward compatibility, providers should gradually
% switch to new tokens.
-spec create_provider_token(tokens:nonce(), tokens:secret()) -> tokens:token().
create_provider_token(Identifier, Secret) ->
    Prototype = #auth_token{
        version = 1,
        onezone_domain = oz_worker:get_domain(),
        nonce = Identifier,
        persistent = true,
        type = ?ACCESS_TOKEN
    },
    tokens:construct(Prototype, Secret, []).


% @todo VFS-5554 for backward compatibility, providers should gradually
% switch to new tokens.
-spec verify_provider_token(tokens:token(), [caveats:type()]) ->
    {ok, od_provider:id()} | {error, term()}.
verify_provider_token(Token = #auth_token{version = 1, type = ?ACCESS_TOKEN}, SupportedCaveats) ->
    AuthCtx = #auth_ctx{current_timestamp = ?NOW()},
    case macaroon_auth:get(Token#auth_token.nonce) of
        {ok, Secret, ?PROVIDER(ProviderId)} ->
            case tokens:verify(Token, Secret, AuthCtx, SupportedCaveats) of
                {ok, _} -> {ok, ProviderId};
                Error = {error, _} -> Error
            end;
        _ ->
            ?ERROR_TOKEN_INVALID
    end;
verify_provider_token(_, _) ->
    ?ERROR_TOKEN_INVALID.
