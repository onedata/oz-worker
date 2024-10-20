%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for GUI listener starting and stopping.
%%% @end
%%%--------------------------------------------------------------------
-module(https_listener).
-author("Lukasz Opiola").

-behaviour(listener_behaviour).

-include("registered_names.hrl").
-include("http/gui_paths.hrl").
-include("graph_sync/oz_graph_sync.hrl").
-include_lib("gui/include/gui.hrl").
-include_lib("ctool/include/logging.hrl").

% Listener config
-define(PORT, oz_worker:get_env(https_server_port, 443)).
-define(ACCEPTORS_NUM, oz_worker:get_env(https_acceptors, 100)).
-define(REQUEST_TIMEOUT, oz_worker:get_env(https_request_timeout, timer:minutes(5))).
-define(MAX_KEEPALIVE, oz_worker:get_env(https_max_keepalive, 30)).

-define(ONEPANEL_CONNECT_OPTS, fun() -> [
    {recv_timeout, timer:seconds(oz_worker:get_env(onepanel_proxy_recv_timeout_sec, 30))},
    {ssl_options, [
        {secure, only_verify_peercert},
        {cacerts, get_cert_chain_ders()}
    ]}
] end).
-define(OAI_PMH_PATH, oz_worker:get_env(oai_pmh_api_prefix, "/oai_pmh")).

%% listener_behaviour callbacks
-export([port/0, start/0, stop/0, reload_web_certs/0, healthcheck/0]).
-export([get_cert_chain_ders/0]).


%%%===================================================================
%%% listener_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback port/0.
%% @end
%%--------------------------------------------------------------------
-spec port() -> integer().
port() ->
    ?PORT.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback start/0.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, Reason :: term()}.
start() ->
    gui:start(gui_config()).


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback stop/0.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    gui:stop().


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback reload_web_certs/0.
%% @end
%%--------------------------------------------------------------------
-spec reload_web_certs() -> ok | {error, term()}.
reload_web_certs() ->
    gui:reload_web_certs(gui_config()).


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback healthcheck/0.
%% @end
%%--------------------------------------------------------------------
-spec healthcheck() -> ok | {error, server_not_responding}.
healthcheck() ->
    gui:healthcheck().


%%--------------------------------------------------------------------
%% @doc
%% Returns intermediate CA chain in DER format for gui web cert.
%% @end
%%--------------------------------------------------------------------
-spec get_cert_chain_ders() -> [public_key:der_encoded()].
get_cert_chain_ders() ->
    gui:get_cert_chain_ders().


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec gui_config() -> gui:gui_config().
gui_config() ->
    % Get certs
    KeyFile = oz_worker:get_env(web_key_file),
    CertFile = oz_worker:get_env(web_cert_file),
    ChainFile = oz_worker:get_env(web_cert_chain_file, undefined),

    CompatRegPath = filename:absname(ctool:get_env(current_compatibility_registry_file)),

    CustomCowboyRoutes = lists:flatten([
        {?OAI_PMH_PATH ++ "/[...]", oai_handler, []},
        {?NAGIOS_PATH, nagios_handler, []},
        {?PANEL_REST_PROXY_PATH ++ "[...]", http_port_forwarder, [9443, ?ONEPANEL_CONNECT_OPTS]},
        {?PROVIDER_GRAPH_SYNC_WS_PATH, gs_ws_handler, [provider_gs_translator]},
        {?GUI_GRAPH_SYNC_WS_PATH, gs_ws_handler, [gui_gs_translator]},
        {?COMPATIBILITY_REG_PATH, cowboy_static, {file, CompatRegPath, [{mimetypes, {<<"application">>, <<"json">>, []}}]}},
        rest_handler:rest_routes(),
        gui_static:routes()
    ]),

    DynamicPageRoutes = [
        {"/", [<<"GET">>], page_redirect_to_oz_worker},
        {?GUI_UPLOAD_PATH, [<<"POST">>], page_gui_upload},
        {?GUI_CONTEXT_PATH, [<<"GET">>], page_gui_context},
        {?GUI_PREAUTHORIZE_PATH, [<<"POST">>], page_gui_preauthorize},
        {?CONFIGURATION_PATH, [<<"GET">>], page_configuration},
        {?PUBLIC_SHARE_COWBOY_ROUTE, [<<"GET">>], page_public_share},
        {?LOGIN_PATH, [<<"POST">>], page_basic_auth_login},
        {?LOGOUT_PATH, [<<"POST">>], page_logout},
        {?OIDC_CONSUME_PATH_DEPRECATED, [<<"GET">>], page_consume_login},
        {?OIDC_CONSUME_PATH, [<<"GET">>], page_consume_login},
        {?SAML_CONSUME_PATH, [<<"POST">>], page_consume_login},
        {?SAML_METADATA_PATH, [<<"GET">>], page_saml_metadata},
        {?SAML_CERT_PATH, [<<"GET">>], page_saml_cert},
        {?DEV_LOGIN_PATH, [<<"GET">>], page_dev_login},
        {?VALIDATE_DEV_LOGIN_PATH, [<<"GET">>], page_validate_dev_login}
    ],

    #gui_config{
        port = port(),
        key_file = KeyFile,
        cert_file = CertFile,
        chain_file = ChainFile,
        number_of_acceptors = ?ACCEPTORS_NUM,
        max_keepalive = ?MAX_KEEPALIVE,
        request_timeout = ?REQUEST_TIMEOUT,
        dynamic_pages = DynamicPageRoutes,
        custom_cowboy_routes = CustomCowboyRoutes,
        custom_response_headers = fun gui_static:maybe_add_cache_control_headers/1
    }.
