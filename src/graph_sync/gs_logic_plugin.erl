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

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("entity_logic.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").

%% API
-export([authorize_by_session_cookie/1, authorize_by_token/1]).
-export([authorize_by_macaroons/2, authorize_by_basic_auth/1]).
-export([client_to_identity/1, root_client/0, guest_client/0]).
-export([client_connected/2, client_disconnected/2]).
-export([is_authorized/5]).
-export([handle_rpc/4]).
-export([handle_graph_request/6]).
-export([is_subscribable/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback authorize_by_session_cookie/1.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_session_cookie(SessionCookie :: binary()) ->
    false | {true, gs_protocol:client()} | {error, term()}.
authorize_by_session_cookie(SessionCookie) ->
    case oz_gui_session:get_user_id(SessionCookie) of
        {ok, UserId} ->
            {true, ?USER(UserId)};
        _ ->
            ?ERROR_UNAUTHORIZED
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback authorize_by_token/1.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_token(Token :: binary()) ->
    false | {true, gs_protocol:client()} | {error, term()}.
authorize_by_token(Token) ->
    case auth_utils:authorize_by_oauth_provider(Token) of
        {true, Client} ->
            {true, Client};
        false ->
            authorize_by_macaroons(Token, <<"">>);
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback authorize_by_macaroons/2.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_macaroons(Macaroon :: binary(),
    DischargeMacaroons :: [binary()]) ->
    false | {true, gs_protocol:client()} | {error, term()}.
authorize_by_macaroons(Macaroon, DischargeMacaroons) ->
    auth_utils:authorize_by_macaroons(Macaroon, DischargeMacaroons).


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback authorize_by_basic_auth/1.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_basic_auth(UserPasswdB64 :: binary()) ->
    false | {true, gs_protocol:client()} | {error, term()}.
authorize_by_basic_auth(UserPasswdB64) ->
    auth_utils:authorize_by_basic_auth(UserPasswdB64).


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback client_to_identity/1.
%% @end
%%--------------------------------------------------------------------
-spec client_to_identity(gs_protocol:client()) -> gs_protocol:identity().
client_to_identity(?NOBODY) -> nobody;
client_to_identity(?USER(UserId)) -> {user, UserId};
client_to_identity(?PROVIDER(ProviderId)) -> {provider, ProviderId}.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback root_client/0.
%% @end
%%--------------------------------------------------------------------
-spec root_client() -> gs_protocol:client().
root_client() ->
    ?ROOT.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback guest_client/0.
%% @end
%%--------------------------------------------------------------------
-spec guest_client() -> gs_protocol:client().
guest_client() ->
    ?NOBODY.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback client_connected/2.
%% @end
%%--------------------------------------------------------------------
-spec client_connected(gs_protocol:client(), gs_server:conn_ref()) -> ok.
client_connected(?PROVIDER(ProvId), ConnectionRef) ->
    {ok, ProviderRecord = #od_provider{
        name = Name
    }} = provider_logic:get(?ROOT, ProvId),
    ?info("Provider '~s' (~s) has connected", [Name, ProvId]),
    provider_connection:add_connection(ProvId, ConnectionRef),
    % Generate a dummy update which will cause a push to GUI clients so that
    % they can learn the provider is now online.
    gs_server:updated(
        od_provider,
        ProvId,
        ProviderRecord
    );
client_connected(_, _) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback client_disconnected/2.
%% @end
%%--------------------------------------------------------------------
-spec client_disconnected(gs_protocol:client(), gs_server:conn_ref()) -> ok.
client_disconnected(?PROVIDER(ProvId), _ConnectionRef) ->
    case provider_logic:get_protected_data(?ROOT, ProvId) of
        {ok, #{<<"name">> := Name}} ->
            ?info("Provider '~s' (~s) went offline", [Name, ProvId]);
        _ ->
            % Provider could have been deleted in the meantime, in such case do
            % not retrieve its name.
            ?info("Provider '~s' went offline", [ProvId])
    end,
    provider_connection:remove_connection(ProvId);
client_disconnected(_, _) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback is_authorized/5.
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
%% {@link gs_translator_behaviour} callback handle_rpc/4.
%% @end
%%--------------------------------------------------------------------
-spec handle_rpc(gs_protocol:protocol_version(), gs_protocol:client(),
    gs_protocol:rpc_function(), gs_protocol:rpc_args()) ->
    gs_protocol:rpc_result().
handle_rpc(1, _, <<"authorizeUser">>, Args) ->
    user_logic:authorize(Args);
handle_rpc(1, _, <<"getLoginEndpoint">>, Data = #{<<"idp">> := IdP}) ->
    case oz_worker:get_env(dev_mode) of
        {ok, true} ->
            {ok, #{
                <<"method">> => <<"get">>,
                <<"url">> => <<"/dev_login">>,
                <<"formData">> => null
            }};
        _ ->
            LinkAccount = maps:get(<<"linkAccount">>, Data, false),
            auth_utils:get_redirect_url(binary_to_atom(IdP, utf8), LinkAccount)
    end;
handle_rpc(1, ?USER(UserId), <<"getProviderRedirectURL">>, Args) ->
    ProviderId = maps:get(<<"providerId">>, Args),
    RedirectPath = case maps:get(<<"path">>, Args, <<"/">>) of
        null -> <<"/">>;
        P -> P
    end,
    {ok, URL} = auth_logic:get_redirection_uri(UserId, ProviderId, RedirectPath),
    {ok, #{<<"url">> => URL}};
handle_rpc(1, _, _, _) ->
    ?ERROR_RPC_UNDEFINED.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback handle_graph_request/6.
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
%% {@link gs_translator_behaviour} callback is_subscribable/1.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(gs_protocol:gri()) -> boolean().
is_subscribable(#gri{type = EntityType, aspect = Aspect, scope = Scope}) ->
    ElPlugin = EntityType:entity_logic_plugin(),
    ElPlugin:is_subscribable(Aspect, Scope).

