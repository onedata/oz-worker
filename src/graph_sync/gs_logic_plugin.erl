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
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").

%% API
-export([verify_handshake_auth/1]).
-export([client_to_identity/1, root_client/0]).
-export([client_connected/3, client_disconnected/3]).
-export([verify_auth_override/2]).
-export([is_authorized/5]).
-export([handle_rpc/4]).
-export([handle_graph_request/6]).
-export([is_subscribable/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback verify_handshake_auth/1.
%% @end
%%--------------------------------------------------------------------
-spec verify_handshake_auth(gs_protocol:auth()) ->
    {ok, gs_protocol:client(), gs_server:connection_info()} | gs_protocol:error().
verify_handshake_auth(undefined) ->
    {ok, ?NOBODY, undefined};
verify_handshake_auth({macaroon, Macaroon, DischargeMacaroons}) ->
    case auth_logic:authorize_by_onezone_gui_macaroon(Macaroon) of
        {true, Client, SessionId} ->
            {ok, Client, SessionId};
        {error, _} ->
            case auth_logic:authorize_by_macaroons(Macaroon, DischargeMacaroons) of
                {true, Client} -> {ok, Client, undefined};
                {error, _} = Error -> Error
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback client_to_identity/1.
%% @end
%%--------------------------------------------------------------------
-spec client_to_identity(gs_protocol:client()) -> gs_protocol:identity().
client_to_identity(?NOBODY) -> nobody;
client_to_identity(?USER(UserId)) -> {user, UserId};
client_to_identity(?PROVIDER(ProviderId)) -> {provider, ProviderId}.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback root_client/0.
%% @end
%%--------------------------------------------------------------------
-spec root_client() -> gs_protocol:client().
root_client() ->
    ?ROOT.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback client_connected/2.
%% @end
%%--------------------------------------------------------------------
-spec client_connected(gs_protocol:client(), gs_server:connection_info(), gs_server:conn_ref()) ->
    ok.
client_connected(?PROVIDER(ProvId), _, ConnectionRef) ->
    {ok, ProviderRecord} = provider_logic:get(?ROOT, ProvId),
    ?info("Provider '~ts' has connected (~s)", [ProviderRecord#od_provider.name, ProvId]),
    provider_connection:add_connection(ProvId, ConnectionRef),
    % Generate a dummy update which will cause a push to GUI clients so that
    % they can learn the provider is now online.
    gs_server:updated(
        od_provider,
        ProvId,
        ProviderRecord
    );
client_connected(?USER, SessionId, ConnectionRef) ->
    user_connections:add(SessionId, ConnectionRef);
client_connected(_, _, _) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback client_disconnected/2.
%% @end
%%--------------------------------------------------------------------
-spec client_disconnected(gs_protocol:client(), gs_server:connection_info(), gs_server:conn_ref()) ->
    ok.
client_disconnected(?PROVIDER(ProvId), _, _ConnectionRef) ->
    case provider_logic:get(?ROOT, ProvId) of
        {ok, ProviderRecord = #od_provider{name = Name}} ->
            ?info("Provider '~ts' went offline (~s)", [Name, ProvId]),
            % Generate a dummy update which will cause a push to GUI clients so that
            % they can learn the provider is now offline.
            gs_server:updated(
                od_provider,
                ProvId,
                ProviderRecord
            );
        _ ->
            % Provider could have been deleted in the meantime, in such case do
            % not retrieve its name.
            ?info("Provider '~s' went offline", [ProvId])
    end,
    provider_connection:remove_connection(ProvId);
client_disconnected(?USER, SessionId, ConnectionRef) ->
    user_connections:remove(SessionId, ConnectionRef);
client_disconnected(_, _, _) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback verify_auth_override/2.
%% @end
%%--------------------------------------------------------------------
-spec verify_auth_override(gs_protocol:client(), gs_protocol:auth_override()) ->
    {ok, gs_protocol:client()} | gs_protocol:error().
verify_auth_override(Client, {macaroon, Macaroon, DischMacaroons}) ->
    case auth_logic:authorize_by_macaroons(Macaroon, DischMacaroons) of
        {true, OverrideClient1} ->
            {ok, OverrideClient1};
        {error, _} = Error1 ->
            case Client of
                ?PROVIDER(ProviderId) ->
                    case auth_logic:authorize_by_gui_macaroon(Macaroon, ?ONEPROVIDER, ProviderId) of
                        {true, OverrideClient2, _SessionId} ->
                            {ok, OverrideClient2};
                        {error, _} = Error2 ->
                            Error2
                    end;
                _ ->
                    Error1
            end

    end;
verify_auth_override(_, _) ->
    ?ERROR_UNAUTHORIZED.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback is_authorized/5.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(gs_protocol:client(), gs_protocol:auth_hint(),
    gs_protocol:gri(), gs_protocol:operation(), gs_protocol:data()) ->
    {true, gs_protocol:gri()} | false.
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
%% {@link gs_logic_plugin_behaviour} callback handle_rpc/4.
%% @end
%%--------------------------------------------------------------------
-spec handle_rpc(gs_protocol:protocol_version(), gs_protocol:client(),
    gs_protocol:rpc_function(), gs_protocol:rpc_args()) ->
    gs_protocol:rpc_result().
handle_rpc(_, _, <<"authorizeUser">>, Args) ->
    user_logic:authorize(Args);
handle_rpc(_, _, <<"getSupportedIdPs">>, Data) ->
    TestMode = maps:get(<<"testMode">>, Data, false),
    TestMode andalso auth_test_mode:process_enable_test_mode(),
    case oz_worker:get_env(dev_mode, false) of
        true ->
            % If dev mode is enabled, always return basic auth and just one
            % dummy provider which will redirect to /dev_login page.
            {ok, #{
                <<"idps">> => [
                    #{
                        <<"id">> => <<"basicAuth">>,
                        <<"displayName">> => <<"username & password">>,
                        <<"iconPath">> => gui_static:oz_worker_gui_path(
                            <<"/assets/images/auth-providers/basicauth.svg">>
                        ),
                        <<"iconBackgroundColor">> => <<"#4BD187">>
                    },
                    #{
                        <<"id">> => <<"devLogin">>,
                        <<"displayName">> => <<"Developer Login">>,
                        <<"iconPath">> => gui_static:oz_worker_gui_path(
                            <<"/assets/images/auth-providers/default.svg">>
                        )
                    }
                ]}
            };
        _ ->
            {ok, #{
                <<"idps">> => auth_config:get_supported_idps_in_gui_format()
            }}
    end;
handle_rpc(_, Client, <<"getLoginEndpoint">>, Data = #{<<"idp">> := IdPBin}) ->
    case oz_worker:get_env(dev_mode, false) of
        true ->
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
handle_rpc(_, ?USER(UserId), <<"getProviderRedirectURL">>, Args) ->
    ProviderId = maps:get(<<"providerId">>, Args),
    RedirectPath = case maps:get(<<"path">>, Args, <<"/">>) of
        null -> <<"/">>;
        P -> P
    end,
    {ok, URL} = auth_tokens:get_redirection_uri(UserId, ProviderId, RedirectPath),
    {ok, #{<<"url">> => URL}};
handle_rpc(_, _, _, _) ->
    ?ERROR_RPC_UNDEFINED.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback handle_graph_request/6.
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
%% {@link gs_logic_plugin_behaviour} callback is_subscribable/1.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(gs_protocol:gri()) -> boolean().
is_subscribable(#gri{type = EntityType, aspect = Aspect, scope = Scope}) ->
    ElPlugin = EntityType:entity_logic_plugin(),
    ElPlugin:is_subscribable(Aspect, Scope).
