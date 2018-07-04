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
-include_lib("gui/include/new_gui.hrl").
-include_lib("ctool/include/logging.hrl").

% Listener config
-define(PORT, oz_worker:get_env(https_server_port, 443)).
-define(ACCEPTORS_NUM, oz_worker:get_env(https_acceptors, 10)).
-define(REQUEST_TIMEOUT, oz_worker:get_env(https_request_timeout, timer:seconds(30))).
-define(MAX_KEEPALIVE, oz_worker:get_env(https_max_keepalive, 30)).

%% listener_behaviour callbacks
-export([port/0, start/0, stop/0, healthcheck/0]).
-export([get_cert_chain_pems/0]).

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
    % Get certs
    {ok, KeyFile} = oz_worker:get_env(web_key_file),
    {ok, CertFile} = oz_worker:get_env(web_cert_file),
    ChainFile = oz_worker:get_env(web_cert_chain_file, undefined),

    {ok, CustomRoot} = oz_worker:get_env(gui_custom_static_root),
    {ok, DefaultRoot} = oz_worker:get_env(gui_default_static_root),

    CustomCowboyRoutes = lists:flatten([
        {?NAGIOS_PATH, nagios_handler, []},
        {?WEBSOCKET_PREFIX_PATH ++ "[...]", gui_ws_handler, []},
        {?PROVIDER_GRAPH_SYNC_WS_PATH, gs_ws_handler, [provider_gs_translator]},
        {?GUI_GRAPH_SYNC_WS_PATH, gs_ws_handler, [gui_gs_translator]},
        rest_handler:rest_routes()
    ]),

    DynamicPageRoutes = [
        {?CONFIGURATION_PATH, [<<"GET">>], page_configuration},
        {?ZONE_VERSION_PATH, [<<"GET">>], page_zone_version},
        {?PUBLIC_SHARE_PATH ++ "/:share_id", [<<"GET">>], page_public_share},
        {?LOGIN_PATH, [<<"POST">>], page_basic_auth_login},
        {"/do_login", [<<"POST">>], page_basic_auth_login}, % @todo deprecated
        {?LOGOUT_PATH, [<<"GET">>], page_logout},
        {"/do_logout", [<<"GET">>], page_logout}, % @todo deprecated
        {?OIDC_CONSUME_PATH_DEPRECATED, [<<"GET">>], page_consume_login},
        {?OIDC_CONSUME_PATH, [<<"GET">>], page_consume_login},
        {?SAML_CONSUME_PATH, [<<"POST">>], page_consume_login},
        {?SAML_METADATA_PATH, [<<"GET">>], page_saml_metadata},
        {?SAML_CERT_PATH, [<<"GET">>], page_saml_cert},
        {?DEV_LOGIN_PATH, [<<"GET">>], page_dev_login},
        {?VALIDATE_DEV_LOGIN_PATH, [<<"GET">>], page_validate_dev_login}
    ],

    new_gui:start(#gui_config{
        port = port(),
        key_file = KeyFile,
        cert_file = CertFile,
        chain_file = ChainFile,
        number_of_acceptors = ?ACCEPTORS_NUM,
        max_keepalive = ?MAX_KEEPALIVE,
        request_timeout = ?REQUEST_TIMEOUT,
        dynamic_pages = DynamicPageRoutes,
        custom_cowboy_routes = CustomCowboyRoutes,
        default_static_root = DefaultRoot,
        custom_static_root = CustomRoot
    }).


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback stop/0.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    new_gui:stop().


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback healthcheck/0.
%% @end
%%--------------------------------------------------------------------
-spec healthcheck() -> ok | {error, server_not_responding}.
healthcheck() ->
    new_gui:healthcheck().


%%--------------------------------------------------------------------
%% @doc
%% Returns intermediate CA chain in PEM format for gui web cert.
%% @end
%%--------------------------------------------------------------------
-spec get_cert_chain_pems() -> [public_key:der_encoded()].
get_cert_chain_pems() ->
    new_gui:get_cert_chain_pems().
