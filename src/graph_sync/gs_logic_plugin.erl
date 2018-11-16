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

-include("http/gui_paths.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("entity_logic.hrl").
-include_lib("gui/include/new_gui.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").

%% API
-export([authorize/1]).
-export([client_to_identity/1, root_client/0, guest_client/0]).
-export([client_connected/2, client_disconnected/2]).
-export([verify_auth_override/1]).
-export([is_authorized/5]).
-export([handle_rpc/4]).
-export([handle_graph_request/6]).
-export([subscribable_resources/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Authorizes a client based on cowboy request, used during handshake.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cowboy_req:req()) -> {ok, gs_protocol:client()} | gs_protocol:error().
authorize(Req) ->
    case authorize_by_session_cookie(Req) of
        {true, CookieClient} ->
            {ok, CookieClient};
        ?ERROR_UNAUTHORIZED ->
            ?ERROR_UNAUTHORIZED;
        false ->
            case auth_logic:authorize_by_macaroons(Req) of
                {true, MacaroonClient} ->
                    {ok, MacaroonClient};
                {error, _} ->
                    ?ERROR_UNAUTHORIZED;
                false ->
                    {ok, ?NOBODY}
            end
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
%% Callback called when a client disconnects from the Graph Sync server.
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
%% {@link gs_logic_plugin_behaviour} callback verify_auth_override/1.
%% @end
%%--------------------------------------------------------------------
-spec verify_auth_override(gs_protocol:auth_override()) ->
    {ok, gs_protocol:client()} | gs_protocol:error().
verify_auth_override({macaroon, Macaroon, DischMacaroons}) ->
    case auth_logic:authorize_by_macaroons(Macaroon, DischMacaroons) of
        {true, Client} -> {ok, Client};
        {error, _} = Error -> Error
    end;
verify_auth_override(_) ->
    ?ERROR_UNAUTHORIZED.


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
handle_rpc(1, Client, <<"getLoginEndpoint">>, Data = #{<<"idp">> := IdPBin}) ->
    case oz_worker:get_env(dev_mode) of
        {ok, true} ->
            {ok, #{
                <<"method">> => <<"get">>,
                <<"url">> => <<"/dev_login">>,
                <<"formData">> => null
            }};
        _ ->
            IdP = binary_to_atom(IdPBin, utf8),
            LinkAccount = case maps:get(<<"linkAccount">>, Data, false) of
                false ->
                    false;
                true ->
                    ?USER(UserId) = Client,
                    {true, UserId}
            end,
            RedirectAfterLogin = maps:get(<<"redirectUrl">>, Data, <<?AFTER_LOGIN_PAGE_PATH>>),
            TestMode = maps:get(<<"testMode">>, Data, false),
            auth_logic:get_login_endpoint(IdP, LinkAccount, RedirectAfterLogin, TestMode)
    end;
handle_rpc(1, ?USER(UserId), <<"getProviderRedirectURL">>, Args) ->
    ProviderId = maps:get(<<"providerId">>, Args),
    RedirectPath = case maps:get(<<"path">>, Args, <<"/">>) of
        null -> <<"/">>;
        P -> P
    end,
    {ok, URL} = auth_tokens:get_redirection_uri(UserId, ProviderId, RedirectPath),
    {ok, #{<<"url">> => URL}};
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
    {instance, shared},
    {client_tokens, private}
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


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to authorize client by HTTP cookie.
%% {true, Client} - client was authorized
%% false - this method cannot verify authorization, other methods should be tried
%% {error, term()} - authorization invalid
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_session_cookie(cowboy_req:req()) ->
    {true, gs_protocol:client()} | false | gs_protocol:error().
authorize_by_session_cookie(Req) ->
    case get_cookie(?SESSION_COOKIE_KEY, Req) of
        undefined ->
            false;
        SessionCookie ->
            case oz_gui_session:get_user_id(SessionCookie) of
                {ok, UserId} ->
                    case gui_route_plugin:check_ws_origin(Req) of
                        true ->
                            {true, ?USER(UserId)};
                        _ ->
                            ?ERROR_UNAUTHORIZED
                    end;
                _ ->
                    ?ERROR_UNAUTHORIZED
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns cookie value for given cookie name.
%% Undefined if no such cookie was sent.
%% NOTE! This should be used instead of cowboy_req:cookie as it contains a bug.
%% @end
%%--------------------------------------------------------------------
-spec get_cookie(Name :: binary(), cowboy_req:req()) -> binary() | undefined.
get_cookie(Name, Req) ->
    proplists:get_value(Name, cowboy_req:parse_cookies(Req)).
