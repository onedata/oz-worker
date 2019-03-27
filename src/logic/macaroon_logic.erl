%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module present a high level API for creating and validating macaroons
%%% issued by the Onezone service.
%%% @end
%%%-------------------------------------------------------------------
-module(macaroon_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/auth/onedata_macaroons.hrl").

-type id() :: binary().
-type secret() :: binary().
-type type() :: authorization.
-type issuer() :: entity_logic:client() | {entity_logic:client(), session:id()}.
% Timestamp (in seconds) when the macaroon expires
-type expires() :: non_neg_integer().
-export_type([id/0, secret/0, type/0, issuer/0]).

-type gui_macaroon() :: {id(), macaroon:macaroon(), expires()}.
-type session_verify_fun() :: fun((session:id(), id()) -> boolean()).
-export_type([gui_macaroon/0]).

-define(NOW(), time_utils:cluster_time_seconds()).

-define(MAX_PROVIDER_MACAROON_TTL, oz_worker:get_env(max_provider_macaroon_ttl, 3600)).
-define(GUI_MACAROON_TTL, oz_worker:get_env(gui_macaroon_ttl, 600)).
% Refresh when current TTL is less than 1/3 of the original TTL
-define(GUI_MACAROON_EXPIRATION_THRESHOLD, ?GUI_MACAROON_TTL div 3).

-export([create_provider_root_macaroon/1]).
-export([verify_provider_auth/1]).
-export([verify_provider_identity/1]).

-export([create_gui_macaroon/4]).
-export([verify_gui_macaroon/4]).
-export([should_refresh_gui_macaroon/1]).
-export([delete_gui_macaroon/1]).

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
    {ok, {macaroon:macaroon(), id()}}.
create_provider_root_macaroon(ProviderId) ->
    Secret = generate_secret(),
    {ok, Identifier} = macaroon_auth:create(
        Secret, ?PROVIDER(ProviderId)
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
    verify_provider_issuer(Macaroon, [
        ?TIME_CAVEAT(?NOW(), ?MAX_PROVIDER_MACAROON_TTL)
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Verifies if given macaroon carries valid provider identity. Can be used by
%% providers to verify each other's identity.
%% @end
%%--------------------------------------------------------------------
-spec verify_provider_identity(macaroon:macaroon()) ->
    {ok, od_provider:id()} | {error, term()}.
verify_provider_identity(Macaroon) ->
    verify_provider_issuer(Macaroon, [
        ?TIME_CAVEAT(?NOW(), ?MAX_PROVIDER_MACAROON_TTL),
        ?AUTHORIZATION_NONE_CAVEAT
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new GUI macaroon for given combination:
%%  * UserId of the client
%%  * Id of the user's session for which the macaroon is issued
%%  * Type of the cluster for which the macaroon will be valid
%%  * Id of the service corresponding to the cluster
%% GUI macaroons are volatile - kept in Onezone's in-memory database.
%% @end
%%--------------------------------------------------------------------
-spec create_gui_macaroon(od_user:id(), session:id(), onedata:cluster_type(), od_cluster:service_id()) ->
    {ok, gui_macaroon()}.
create_gui_macaroon(UserId, SessionId, ClusterType, ServiceId) ->
    Secret = generate_secret(),
    {ok, Identifier} = volatile_macaroon:create(
        Secret, {?USER(UserId), SessionId}
    ),
    Now = ?NOW(),
    TTL = ?GUI_MACAROON_TTL,
    Expires = Now + TTL,
    Macaroon = create(Identifier, Secret, [
        ?SESSION_ID_CAVEAT(SessionId),
        ?CLUSTER_TYPE_CAVEAT(ClusterType),
        ?SERVICE_ID_CAVEAT(ServiceId),
        ?TIME_CAVEAT(Now, TTL)
    ]),
    {ok, {Identifier, Macaroon, Expires}}.


%%--------------------------------------------------------------------
%% @doc
%% Verifies if given macaroon carries valid user authorization in the context
%% of given cluster type and service id. SessionVerifyFun is used to verify if
%% the session id embedded in the macaroon is valid.
%% @end
%%--------------------------------------------------------------------
-spec verify_gui_macaroon(macaroon:macaroon(), onedata:cluster_type(), od_cluster:service_id(), session_verify_fun()) ->
    {ok, od_user:id(), session:id()} | {error, term()}.
verify_gui_macaroon(SubjectMacaroon, ClusterType, ServiceId, SessionVerifyFun) ->
    Identifier = macaroon:identifier(SubjectMacaroon),
    case volatile_macaroon:get(Identifier) of
        {ok, Secret, {?USER(UserId), IssuerSessionId}} ->
            SessionCaveatVerifyFun = fun(CaveatSessionId) ->
                CaveatSessionId == IssuerSessionId andalso
                    SessionVerifyFun(CaveatSessionId, Identifier)
            end,
            CaveatVerifiers = [
                ?SESSION_ID_VERIFIER(SessionCaveatVerifyFun),
                ?CLUSTER_TYPE_CAVEAT(ClusterType),
                ?SERVICE_ID_CAVEAT(ServiceId),
                ?TIME_CAVEAT(?NOW(), ?GUI_MACAROON_TTL)
            ],
            case onedata_macaroons:verify(SubjectMacaroon, Secret, [], CaveatVerifiers) of
                ok -> {ok, UserId, IssuerSessionId};
                Error = {error, _} -> Error
            end;
        _ ->
            ?ERROR_MACAROON_INVALID
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns true if the refresh threshold has been exceeded for given expiration
%% time and the corresponding macaroon should be refreshed.
%% @end
%%--------------------------------------------------------------------
-spec should_refresh_gui_macaroon(expires()) -> boolean().
should_refresh_gui_macaroon(Expires) ->
    TTL = Expires - ?NOW(),
    TTL < ?GUI_MACAROON_EXPIRATION_THRESHOLD.


%%--------------------------------------------------------------------
%% @doc
%% Deletes a GUI macaroon from Onezone's in-memory database.
%% @end
%%--------------------------------------------------------------------
-spec delete_gui_macaroon(macaroon:macaroon()) -> ok.
delete_gui_macaroon(Macaroon) ->
    Identifier = macaroon:identifier(Macaroon),
    ok = volatile_macaroon:delete(Identifier).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec create(id(), secret(), [onedata_macaroons:caveat()]) ->
    macaroon:macaroon().
create(Identifier, Secret, Caveats) ->
    onedata_macaroons:create(oz_worker:get_domain(), Secret, Identifier, Caveats).


-spec verify_provider_issuer(macaroon:macaroon(), [onedata_macaroons:caveat()]) ->
    {ok, issuer()} | {error, term()}.
verify_provider_issuer(Macaroon, Caveats) ->
    Identifier = macaroon:identifier(Macaroon),
    case macaroon_auth:get(Identifier) of
        {ok, Secret, ?PROVIDER(ProviderId)} ->
            case onedata_macaroons:verify(Macaroon, Secret, [], Caveats) of
                ok -> {ok, ProviderId};
                Error = {error, _} -> Error
            end;
        _ ->
            ?ERROR_MACAROON_INVALID
    end.


-spec generate_secret() -> binary().
generate_secret() ->
    BinSecret = crypto:strong_rand_bytes(macaroon:suggested_secret_length()),
    <<<<Y>> || <<X:4>> <= BinSecret, Y <- integer_to_list(X, 16)>>.
