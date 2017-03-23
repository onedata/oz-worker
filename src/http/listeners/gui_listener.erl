%%%--------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module is responsible for GUI listener starting and stopping.
%%% @end
%%%--------------------------------------------------------------------
-module(gui_listener).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("gui/include/gui.hrl").

-behaviour(listener_behaviour).

%% listener_behaviour callbacks
-export([port/0, start/0, stop/0, healthcheck/0]).

-define(gui_https_listener, https).

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
    {ok, Port} = application:get_env(?APP_NAME, gui_port),
    Port.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback start/0.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, Reason :: term()}.
start() ->
    try
        % Get gui config
        GuiPort = port(),
        {ok, GuiNbAcceptors} =
            application:get_env(?APP_NAME, gui_https_acceptors),
        {ok, Timeout} = application:get_env(?APP_NAME, gui_socket_timeout),
        {ok, MaxKeepAlive} = application:get_env(?APP_NAME, gui_max_keepalive),

        % Get certs
        {ok, KeyFile} = application:get_env(?APP_NAME, web_key_file),
        {ok, CertFile} = application:get_env(?APP_NAME, web_cert_file),
        {ok, CaCertsDir} = application:get_env(?APP_NAME, cacerts_dir),
        {ok, CaCerts} = file_utils:read_files({dir, CaCertsDir}),

        % Initialize auth handler
        auth_config:load_auth_config(),

        Dispatch = cowboy_router:compile([
            % Matching requests will be redirected to the same address without
            % leading 'www.'. Cowboy does not have a mechanism to match every
            % hostname starting with 'www.' This will match hostnames with up
            % to 6 segments e. g. www.seg2.seg3.seg4.seg5.com
            {"www.:_[.:_[.:_[.:_[.:_]]]]", [
                % redirector_handler is defined in cluster_worker
                {'_', redirector_handler, []}
            ]},
            {'_', lists:flatten([
                {"/nagios/[...]", nagios_handler, []},
                {"/share/:share_id", public_share_handler, []},
                {?WEBSOCKET_PREFIX_PATH ++ "[...]", gui_ws_handler, []},
                rest_listener:routes(),
                static_routes()
            ])}
        ]),

        % Call gui init, which will call init on all modules that might need state.
        gui:init(),
        % Start the listener for web gui and nagios handler
        {ok, _} = ranch:start_listener(?gui_https_listener, GuiNbAcceptors,
            ranch_etls, [
                {port, GuiPort},
                {keyfile, KeyFile},
                {certfile, CertFile},
                {cacerts, CaCerts},
                {ciphers, ssl:cipher_suites() -- ssl_utils:weak_ciphers()},
                {versions, ['tlsv1.2', 'tlsv1.1']}
            ], cowboy_protocol, [
                {env, [{dispatch, Dispatch}]},
                {max_keepalive, MaxKeepAlive},
                {timeout, Timeout},
                % On every request, add headers that improve
                % security to the response
                {onrequest, fun gui:response_headers/1}
            ]),
        ok
    catch
        _Type:Error ->
            ?error_stacktrace("Could not start gui, error: ~p", [Error]),
            {error, Error}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback stop/0.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    case catch ranch:stop_listener(?gui_https_listener) of
        (ok) ->
            ok;
        (Error) ->
            ?error("Error on stopping listener ~p: ~p", [?gui_https_listener, Error]),
            {error, redirector_stop_error}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback healthcheck/0.
%% @end
%%--------------------------------------------------------------------
-spec healthcheck() -> ok | {error, server_not_responding}.
healthcheck() ->
    Endpoint = "https://127.0.0.1:" ++ integer_to_list(port()),
    case http_client:get(Endpoint, #{}, <<>>, [insecure]) of
        {ok, _, _, _} -> ok;
        _ -> {error, server_not_responding}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a Cowboy-understandable PathList of static routes.
%% @end
%%--------------------------------------------------------------------
-spec static_routes() -> [{Path :: string(), Module :: module(), State :: term()}].
static_routes() ->
    % Resolve static files root. First, check if there is a non-empty dir
    % located in gui_custom_static_root. If not, use default.
    {ok, CustomRoot} = application:get_env(?APP_NAME, gui_custom_static_root),
    {ok, DefRoot} = application:get_env(?APP_NAME, gui_default_static_root),
    DocRoot = case file:list_dir_all(CustomRoot) of
        {error, enoent} -> DefRoot;
        {ok, []} -> DefRoot;
        {ok, _} -> CustomRoot
    end,

    Routes = [{"/[...]", gui_static_handler, {dir, DocRoot}}],

    case application:get_env(?APP_NAME, gui_docs_proxy_enabled) of
        {ok, true} ->
            {ok, DocsPath} = application:get_env(?APP_NAME, gui_docs_static_root),
            [{DocsPath ++ "/[...]", static_docs_handler, []} | Routes];
        _ ->
            Routes
    end.
