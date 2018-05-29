%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module present a high level API for creating and validating macaroons
%%% issued by the OneZone service.
%%% @end
%%%-------------------------------------------------------------------
-module(macaroon_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/auth/onedata_macaroons.hrl").

-define(MAX_PROVIDER_MACAROON_TTL, oz_worker:get_env(max_provider_macaroon_ttl, 3600)).

-export([create_provider_root_macaroon/1]).
-export([verify_provider_auth/1]).
-export([verify_provider_identity/1]).

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
    {ok, {macaroon:macaroon(), macaroon_auth:id()}}.
create_provider_root_macaroon(ProviderId) ->
    Secret = generate_secret(),
    {ok, Identifier} = macaroon_auth:create(
        Secret, authorization, ?PROVIDER(ProviderId)
    ),
    Macaroon = create(Identifier, Secret, []),
    {ok, {Macaroon, Identifier}}.


%%--------------------------------------------------------------------
%% @doc
%% Verifies if given macaroon carries valid provider authorization.
%% @end
%%--------------------------------------------------------------------
-spec verify_provider_auth(macaroon:macaroon()) ->
    {ok, od_provider:id()} | {error, term()}.
verify_provider_auth(Macaroon) ->
    Timestamp = time_utils:cluster_time_seconds(),
    verify_provider_issuer(Macaroon, [
        ?TIME_CAVEAT(Timestamp, ?MAX_PROVIDER_MACAROON_TTL)
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Verifies if given macaroon carries valid provider identity. Can be used by
%% providers to each other's identity.
%% @end
%%--------------------------------------------------------------------
-spec verify_provider_identity(macaroon:macaroon()) ->
    {ok, od_provider:id()} | {error, term()}.
verify_provider_identity(Macaroon) ->
    Timestamp = time_utils:cluster_time_seconds(),
    verify_provider_issuer(Macaroon, [
        ?TIME_CAVEAT(Timestamp, ?MAX_PROVIDER_MACAROON_TTL),
        ?AUTHORIZATION_NONE_CAVEAT
    ]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec create(macaroon_auth:id(), macaroon_auth:secret(), [onedata_macaroons:caveat()]) ->
    macaroon:macaroon().
create(Identifier, Secret, Caveats) ->
    onedata_macaroons:create(oz_worker:get_domain(), Secret, Identifier, Caveats).


-spec verify_provider_issuer(macaroon:macaroon(), [onedata_macaroons:caveat()]) ->
    {ok, od_provider:id()} | {error, term()}.
verify_provider_issuer(Macaroon, Caveats) ->
    case verify_issuer(Macaroon, Caveats) of
        {ok, ?PROVIDER(ProviderId)} -> {ok, ProviderId};
        {ok, _} -> ?ERROR_MACAROON_INVALID;
        Error = {error, _} -> Error
    end.


-spec verify_issuer(macaroon:macaroon(), [onedata_macaroons:caveat()]) ->
    {ok, macaroon_auth:issuer()} | {error, term()}.
verify_issuer(Macaroon, Caveats) ->
    Identifier = macaroon:identifier(Macaroon),
    case macaroon_auth:get(Identifier) of
        {ok, #macaroon_auth{secret = Secret, type = authorization, issuer = Issuer}} ->
            case onedata_macaroons:verify(Macaroon, Secret, [], Caveats) of
                ok -> {ok, Issuer};
                Error = {error, _} -> Error
            end;
        _ ->
            ?ERROR_MACAROON_INVALID
    end.


-spec generate_secret() -> binary().
generate_secret() ->
    BinSecret = crypto:strong_rand_bytes(macaroon:suggested_secret_length()),
    <<<<Y>> || <<X:4>> <= BinSecret, Y <- integer_to_list(X, 16)>>.
