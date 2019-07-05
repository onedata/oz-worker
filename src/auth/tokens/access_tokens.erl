%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
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
-define(MAX_PROVIDER_MACAROON_TTL, oz_worker:get_env(max_provider_macaroon_ttl, 3600)).

-export([create_provider_root_macaroon/1]).
-export([verify_provider_auth/1]).
-export([verify_provider_identity/1]).
-export([invalidate_provider_root_macaroon/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new provider root macaroon that will be used by it for
%% authentication and authorization.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_root_macaroon(od_provider:id()) ->
    {ok, tokens:token(), tokens:nonce()}.
create_provider_root_macaroon(ProviderId) ->
    Secret = tokens:generate_secret(),
    {ok, Identifier} = macaroon_auth:create(
        Secret, ?PROVIDER(ProviderId)
    ),
    Token = create_provider_token(Identifier, Secret, []),
    {ok, Token, Identifier}.


%%--------------------------------------------------------------------
%% @doc
%% Verifies if given macaroon carries valid provider authorization.
%% @end
%%--------------------------------------------------------------------
-spec verify_provider_auth(tokens:token()) ->
    {ok, od_provider:id()} | {error, term()}.
verify_provider_auth(Token) ->
    verify_provider_token(Token, [
        ?TIME_CAVEAT(?NOW(), ?MAX_PROVIDER_MACAROON_TTL)
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Verifies if given macaroon carries valid provider identity. Can be used by
%% providers to verify each other's identity.
%% @end
%%--------------------------------------------------------------------
-spec verify_provider_identity(tokens:token()) ->
    {ok, od_provider:id()} | {error, term()}.
verify_provider_identity(Token) ->
    verify_provider_token(Token, [
        ?TIME_CAVEAT(?NOW(), ?MAX_PROVIDER_MACAROON_TTL),
        ?AUTHORIZATION_NONE_CAVEAT
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Irreversibly invalidates a provider root macaroon by its identifier.
%% @end
%%--------------------------------------------------------------------
-spec invalidate_provider_root_macaroon(tokens:nonce()) -> ok.
invalidate_provider_root_macaroon(Identifier) ->
    macaroon_auth:delete(Identifier).

%%%===================================================================
%%% Internal functions
%%%===================================================================

% @todo VFS-5554 for backward compatibility, providers should gradually
% switch to new tokens.
-spec create_provider_token(tokens:nonce(), tokens:secret(), [tokens:caveat()]) ->
    tokens:token().
create_provider_token(Identifier, Secret, Caveats) ->
    Prototype = #auth_token{
        version = 1,
        onezone_domain = oz_worker:get_domain(),
        nonce = Identifier,
        persistent = true,
        type = ?ACCESS_TOKEN
    },
    tokens:construct(Prototype, Secret, Caveats).


% @todo VFS-5554 for backward compatibility, providers should gradually
% switch to new tokens.
-spec verify_provider_token(tokens:token(), [tokens:caveat()]) ->
    {ok, od_provider:id()} | {error, term()}.
verify_provider_token(Token = #auth_token{version = 1, type = ?ACCESS_TOKEN}, Caveats) ->
    case macaroon_auth:get(Token#auth_token.nonce) of
        {ok, Secret, ?PROVIDER(ProviderId)} ->
            case tokens:verify(Token, Secret, [], Caveats) of
                {ok, _} -> {ok, ProviderId};
                Error = {error, _} -> Error
            end;
        _ ->
            ?ERROR_MACAROON_INVALID
    end;
verify_provider_token(_, _) ->
    ?ERROR_MACAROON_INVALID.
