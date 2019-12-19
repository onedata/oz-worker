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
-include_lib("ctool/include/errors.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").

%% API
-export([verify_handshake_auth/2]).
-export([client_connected/2, client_disconnected/2]).
-export([verify_auth_override/2]).
-export([is_authorized/5]).
-export([handle_rpc/4]).
-export([handle_graph_request/6]).
-export([is_subscribable/1]).
-export([is_type_supported/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback verify_handshake_auth/2.
%% @end
%%--------------------------------------------------------------------
-spec verify_handshake_auth(gs_protocol:client_auth(), ip_utils:ip()) ->
    {ok, aai:auth()} | errors:error().
verify_handshake_auth(undefined, PeerIp) ->
    {ok, #auth{subject = ?SUB(nobody), peer_ip = PeerIp}};
verify_handshake_auth(nobody, PeerIp) ->
    {ok, #auth{subject = ?SUB(nobody), peer_ip = PeerIp}};
verify_handshake_auth({token, Token}, PeerIp) ->
    AuthCtx = token_auth:build_auth_ctx(graphsync, PeerIp),
    case token_auth:authenticate(Token, AuthCtx) of
        {true, Auth} -> {ok, Auth};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback client_connected/2.
%% @end
%%--------------------------------------------------------------------
-spec client_connected(aai:auth(), gs_server:conn_ref()) ->
    ok.
client_connected(?PROVIDER(ProvId), ConnectionRef) ->
    {ok, #document{value = ProvRecord, revs = [DbRev | _]}} = od_provider:get(ProvId),
    ?info("Provider '~ts' has connected (~s)", [ProvRecord#od_provider.name, ProvId]),
    provider_connection:add_connection(ProvId, ConnectionRef),
    % Generate a dummy update which will cause a push to GUI clients so that
    % they can learn the provider is now online.
    {Revision, _Hash} = datastore_rev:parse(DbRev),
    gs_server:updated(
        od_provider,
        ProvId,
        {ProvRecord, Revision}
    );
client_connected(?USER = #auth{session_id = undefined}, _) ->
    ok;
client_connected(?USER = #auth{session_id = SessionId}, ConnectionRef) ->
    user_connections:add(SessionId, ConnectionRef);
client_connected(_, _) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback client_disconnected/2.
%% @end
%%--------------------------------------------------------------------
-spec client_disconnected(aai:auth(), gs_server:conn_ref()) ->
    ok.
client_disconnected(?PROVIDER(ProvId), _ConnectionRef) ->
    case od_provider:get(ProvId) of
        {ok, #document{value = ProvRecord, revs = [DbRev | _]}} ->
            ?info("Provider '~ts' went offline (~s)", [ProvRecord#od_provider.name, ProvId]),
            % Generate a dummy update which will cause a push to GUI clients so that
            % they can learn the provider is now offline.
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            gs_server:updated(
                od_provider,
                ProvId,
                {ProvRecord, Revision}
            );
        _ ->
            % Provider could have been deleted in the meantime, in such case do
            % not retrieve its name.
            ?info("Provider '~s' went offline", [ProvId])
    end,
    provider_connection:remove_connection(ProvId);
client_disconnected(?USER = #auth{session_id = undefined}, _) ->
    ok;
client_disconnected(?USER = #auth{session_id = SessionId}, ConnectionRef) ->
    user_connections:remove(SessionId, ConnectionRef);
client_disconnected(_, _) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback verify_auth_override/2.
%% @end
%%--------------------------------------------------------------------
-spec verify_auth_override(aai:auth(), gs_protocol:auth_override()) ->
    {ok, aai:auth()} | errors:error().
verify_auth_override(?PROVIDER(ProviderId), #auth_override{client_auth = {token, Token}} = AuthOverride) ->
    #auth_override{
        peer_ip = PeerIp,
        interface = Interface,
        audience_token = AudienceToken,
        data_access_caveats_policy = DataAccessCaveatsPolicy
    } = AuthOverride,

    % If no audience token is given, audience defaults to the provider (GS channel owner)
    ResolvedAudience = case AudienceToken of
        undefined ->
            {ok, ?AUD(?OP_WORKER, ProviderId)};
        SerializedAudienceToken ->
            AudienceAuthCtx = token_auth:build_auth_ctx(graphsync, PeerIp),
            token_auth:verify_audience_token(SerializedAudienceToken, AudienceAuthCtx)
    end,

    case ResolvedAudience of
        {ok, Audience} ->
            AuthCtx = token_auth:build_auth_ctx(Interface, PeerIp, Audience, DataAccessCaveatsPolicy),
            case token_auth:authenticate(Token, AuthCtx) of
                {true, OverridenAuth = ?USER(UserId)} ->
                    % Provided token is valid; allow only user tokens and check
                    % that the GS channel owning provider actually supports the user
                    case provider_logic:has_eff_user(ProviderId, UserId) of
                        true -> {ok, OverridenAuth};
                        false -> ?ERROR_UNAUTHORIZED
                    end;
                {true, _} ->
                    ?ERROR_UNAUTHORIZED;
                {error, _} = Err1 ->
                    Err1
            end;
        {error, _} = Err2 ->
            Err2
    end;
verify_auth_override(?PROVIDER(_), #auth_override{client_auth = nobody}) ->
    {ok, ?NOBODY};
verify_auth_override(_, _) ->
    ?ERROR_UNAUTHORIZED.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback is_authorized/5.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(aai:auth(), gs_protocol:auth_hint(),
    gri:gri(), gs_protocol:operation(), gs_protocol:versioned_entity()) ->
    {true, gri:gri()} | false.
is_authorized(Auth, AuthHint, GRI, Operation, VersionedEntity) ->
    ElReq = #el_req{
        auth = Auth,
        operation = Operation,
        gri = GRI,
        auth_hint = AuthHint
    },
    entity_logic:is_authorized(ElReq, VersionedEntity).


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback handle_rpc/4.
%% @end
%%--------------------------------------------------------------------
-spec handle_rpc(gs_protocol:protocol_version(), aai:auth(),
    gs_protocol:rpc_function(), gs_protocol:rpc_args()) ->
    gs_protocol:rpc_result().
handle_rpc(_, _, <<"getSupportedIdPs">>, Data) ->
    TestMode = maps:get(<<"testMode">>, Data, false),
    TestMode andalso idp_auth_test_mode:process_enable_test_mode(),
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
handle_rpc(_, Auth, <<"getLoginEndpoint">>, Data = #{<<"idp">> := IdPBin}) ->
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
                    ?USER(UserId) = Auth,
                    {true, UserId}
            end,
            RedirectAfterLogin = maps:get(<<"redirectUrl">>, Data, <<?AFTER_LOGIN_PAGE_PATH>>),
            TestMode = maps:get(<<"testMode">>, Data, false),
            idp_auth:get_login_endpoint(IdP, LinkAccount, RedirectAfterLogin, TestMode)
    end;
handle_rpc(_, _, _, _) ->
    ?ERROR_RPC_UNDEFINED.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback handle_graph_request/6.
%% @end
%%--------------------------------------------------------------------
-spec handle_graph_request(aai:auth(), gs_protocol:auth_hint(),
    gri:gri(), gs_protocol:operation(), gs_protocol:data(),
    gs_protocol:versioned_entity()) -> gs_protocol:graph_request_result().
handle_graph_request(Auth, AuthHint, GRI, Operation, Data, VersionedEntity) ->
    ElReq = #el_req{
        auth = Auth,
        operation = Operation,
        gri = GRI,
        data = Data,
        auth_hint = AuthHint,
        return_revision = true
    },
    entity_logic:handle(ElReq, VersionedEntity).


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback is_subscribable/1.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(gri:gri()) -> boolean().
is_subscribable(#gri{type = EntityType, aspect = Aspect, scope = Scope}) ->
    ElPlugin = EntityType:entity_logic_plugin(),
    ElPlugin:is_subscribable(Aspect, Scope).


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_logic_plugin_behaviour} callback is_type_supported/1.
%% @end
%%--------------------------------------------------------------------
-spec is_type_supported(gri:gri()) -> boolean().
is_type_supported(#gri{type = od_user}) -> true;
is_type_supported(#gri{type = od_group}) -> true;
is_type_supported(#gri{type = od_space}) -> true;
is_type_supported(#gri{type = od_share}) -> true;
is_type_supported(#gri{type = od_storage}) -> true;
is_type_supported(#gri{type = od_provider}) -> true;
is_type_supported(#gri{type = od_token}) -> true;
is_type_supported(#gri{type = od_handle_service}) -> true;
is_type_supported(#gri{type = od_handle}) -> true;
is_type_supported(#gri{type = od_cluster}) -> true;
is_type_supported(#gri{type = od_harvester}) -> true;
is_type_supported(#gri{type = oz_worker}) -> true;
is_type_supported(#gri{type = _}) -> false.