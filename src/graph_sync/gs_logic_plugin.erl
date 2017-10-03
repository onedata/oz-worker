%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This model implements gs_logic_plugin_behaviour and is called by gs_server
%%% to handle application specific Graph Sync logic.
%%% @end
%%%-------------------------------------------------------------------
-module(gs_logic_plugin).
-behaviour(gs_logic_plugin_behaviour).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include("entity_logic.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("cluster_worker/include/api_errors.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").

%% API
-export([authorize_by_session_cookie/1, authorize_by_token/1, authorize_by_macaroons/2]).
-export([authorize_by_basic_auth/1, authorize_by_provider_cert/1]).
-export([client_to_identity/1, root_client/0, guest_client/0]).
-export([client_connected/1, client_disconnected/1]).
-export([is_authorized/5]).
-export([handle_rpc/4]).
-export([handle_graph_request/6]).
-export([subscribable_resources/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Tries to authorize requesting client by session cookie. Will be called only
%% if a session cookie was sent in the request.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_session_cookie(SessionCookie :: binary()) ->
    false | {true, gs_protocol:client()} | {error, term()}.
authorize_by_session_cookie(SessionCookie) ->
    case session:get(SessionCookie) of
        {ok, #document{value = #session{user_id = UserId}}} ->
            {true, ?USER(UserId)};
        _ ->
            ?ERROR_UNAUTHORIZED
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to authorize requesting client by X-Auth-Token. Will be called
%% only if a token was sent in the request.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_token(Token :: binary()) ->
    false | {true, gs_protocol:client()} | {error, term()}.
authorize_by_token(Token) ->
    case auth_utils:authorize_by_oauth_provider(Token) of
        {true, Client} ->
            {ok, Client};
        false ->
            authorize_by_macaroons(Token, <<"">>);
        {error, {bad_access_token, ProviderId}} ->
            ?ERROR_BAD_EXTERNAL_ACCESS_TOKEN(ProviderId)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to authorize requesting client by macaroons. Will be called
%% only if macaroons were sent in the request.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_macaroons(Macaroon :: binary(),
    DischargeMacaroons :: [binary()]) ->
    false | {true, gs_protocol:client()} | {error, term()}.
authorize_by_macaroons(Macaroon, DischargeMacaroons) ->
    case auth_utils:authorize_by_macaroons(Macaroon, DischargeMacaroons) of
        {true, Client} ->
            {ok, Client};
        {error, _} ->
            ?ERROR_BAD_MACAROON
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to authorize requesting client by basic auth credentials. Will be
%% called only if credentials were sent in the request, in the format
%% b64(user:password).
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_basic_auth(UserPasswdB64 :: binary()) ->
    false | {true, gs_protocol:client()} | {error, term()}.
authorize_by_basic_auth(UserPasswdB64) ->
    case auth_utils:authorize_by_basic_auth(UserPasswdB64) of
        {true, Client} ->
            {ok, Client};
        {error, _} ->
            ?ERROR_BAD_BASIC_CREDENTIALS
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to authorize requesting client by provider certificate. Will be called
%% only if a certificate was sent in the request.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_provider_cert(PeerCert :: public_key:der_encoded()) ->
    false | {true, gs_protocol:client()} | {error, term()}.
authorize_by_provider_cert(PeerCert) ->
    case auth_utils:authorize_by_provider_certs(PeerCert) of
        {true, Client} ->
            {true, Client};
        {error, bad_cert} ->
            ?ERROR_UNAUTHORIZED
    end.


%%--------------------------------------------------------------------
%% @doc
%% Converts client, which is an opaque term for gs_server, into identity of
%% the client.
%% @end
%%--------------------------------------------------------------------
-spec client_to_identity(gs_protocol:client()) -> gs_protocol:identity().
client_to_identity(?NOBODY) -> nobody;
client_to_identity(?USER(UserId)) -> {user, UserId};
client_to_identity(?PROVIDER(ProviderId)) -> {provider, ProviderId}.


%%--------------------------------------------------------------------
%% @doc
%% Returns the ROOT client as understood by gs_logic_plugin, i.e. a client that
%% is authorized to do everything. ROOT client can be used only in internal
%% code (i.e. cannot be accessed via any API).
%% @end
%%--------------------------------------------------------------------
-spec root_client() -> gs_protocol:client().
root_client() ->
    ?ROOT.


%%--------------------------------------------------------------------
%% @doc
%% Returns the GUEST client as understood by gs_logic_plugin, i.e. a client that
%% was not identified as anyone and can only access public resources.
%% @end
%%--------------------------------------------------------------------
-spec guest_client() -> gs_protocol:client().
guest_client() ->
    ?NOBODY.


%%--------------------------------------------------------------------
%% @doc
%% Callback called when a new client connects to the Graph Sync server.
%% @end
%%--------------------------------------------------------------------
-spec client_connected(gs_protocol:client()) -> ok.
client_connected(?PROVIDER(ProvId)) ->
    {ok, #{<<"name">> := Name}} = provider_logic:get_protected_data(?ROOT, ProvId),
    ?info("Provider '~s' (~s) has connected", [Name, ProvId]),
    provider_logic:set_online(ProvId, true);
client_connected(_) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Callback called when a client disconnects from the Graph Sync server.
%% @end
%%--------------------------------------------------------------------
-spec client_disconnected(gs_protocol:client()) -> ok.
client_disconnected(?PROVIDER(ProvId)) ->
    {ok, #{<<"name">> := Name}} = provider_logic:get_protected_data(?ROOT, ProvId),
    ?info("Provider '~s' (~s) went offline", [Name, ProvId]),
    provider_logic:set_online(ProvId, false);
client_disconnected(_) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given client is authorized to perform certain operation.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(gs_protocol:client(), gs_protocol:auth_hint(),
    gs_protocol:gri(), gs_protocol:operation(), gs_protocol:data()) -> boolean().
is_authorized(Client, AuthHint, GRI, Operation, Entity) ->
    ElReq = #el_req{
        client = Client,
        operation = Operation,
        gri = GRI,
        auth_hint = AuthHint
    },
    entity_logic:is_authorized(ElReq, Entity).


%%--------------------------------------------------------------------
%% @doc
%% Handles an RPC request and returns the result.
%% @end
%%--------------------------------------------------------------------
-spec handle_rpc(gs_protocol:protocol_version(), gs_protocol:client(),
    gs_protocol:rpc_function(), gs_protocol:rpc_args()) ->
    gs_protocol:rpc_result().
handle_rpc(1, _, <<"authorizeUser">>, Args) ->
    user_logic:authorize(Args);
handle_rpc(1, _, _, _) ->
    ?ERROR_RPC_UNDEFINED.


%%--------------------------------------------------------------------
%% @doc
%% Handles a graph request and returns the result.
%% @end
%%--------------------------------------------------------------------
-spec handle_graph_request(gs_protocol:client(), gs_protocol:auth_hint(),
    gs_protocol:gri(), gs_protocol:operation(), gs_protocol:data(),
    gs_protocol:entity()) -> gs_protocol:graph_request_result().
handle_graph_request(Client, AuthHint, GRI, Operation, Data, Entity) ->
    ElReq = #el_req{
        client = Client,
        operation = Operation,
        gri = GRI,
        data = Data,
        auth_hint = AuthHint
    },
    entity_logic:handle(ElReq, Entity).


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of subscribable resources for given entity type, identified
%% by {Aspect, Scope} pairs.
%% @end
%%--------------------------------------------------------------------
-spec subscribable_resources(gs_protocol:entity_type()) ->
    [{gs_protocol:aspect(), gs_protocol:scope()}].
subscribable_resources(od_user) -> [
    {instance, private},
    {instance, protected},
    {instance, shared}
];
subscribable_resources(od_group) -> [
    {instance, private},
    {instance, protected},
    {instance, shared}
];
subscribable_resources(od_space) -> [
    {instance, private},
    {instance, protected}
];
subscribable_resources(od_share) -> [
    {instance, private},
    {instance, public}
];
subscribable_resources(od_provider) -> [
    {instance, private},
    {instance, protected}
];
subscribable_resources(od_handle_service) -> [
    {instance, private},
    {instance, protected}
];
subscribable_resources(od_handle) -> [
    {instance, private},
    {instance, public}
].

