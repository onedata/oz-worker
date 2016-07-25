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

-include("gui_config.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("gui/include/gui.hrl").

-behaviour(listener_behaviour).

%% listener_behaviour callbacks
-export([port/0, start/0, stop/0, healthcheck/0]).

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
    {ok, Port} = application:get_env(?APP_Name, gui_port),
    Port.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback start/0.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, Reason :: term()}.
start() ->
    try
        %% TODO for development
        %% This is needed for the redirection to provider to work, as
        %% we don't use legit server certificates on providers.
        application:set_env(ctool, verify_server_cert, false),

        % Resolve static files root. First, check if there is a non-empty dir
        % located in gui_custom_static_root. If not, use default.
        {ok, CstmRoot} = application:get_env(?APP_Name, gui_custom_static_root),
        {ok, DefRoot} = application:get_env(?APP_Name, gui_default_static_root),
        DocRoot = case file:list_dir_all(CstmRoot) of
            {error, enoent} -> DefRoot;
            {ok, []} -> DefRoot;
            {ok, _} -> CstmRoot
        end,

        % Get gui config
        GuiPort = port(),
        {ok, GuiNbAcceptors} = application:get_env(?APP_Name, gui_https_acceptors),
        {ok, Timeout} = application:get_env(?APP_Name, gui_socket_timeout),
        {ok, MaxKeepAlive} = application:get_env(?APP_Name, gui_max_keepalive),
        % Get cert paths
        {ok, CaCertFile} = application:get_env(?APP_Name, gui_cacert_file),
        {ok, CertFile} = application:get_env(?APP_Name, gui_cert_file),
        {ok, KeyFile} = application:get_env(?APP_Name, gui_key_file),
        % Get path to static docs files
        {ok, DocsPath} = application:get_env(?APP_Name, gui_docs_static_root),

        GRHostname = dns_query_handler:get_canonical_hostname(),

        % Setup GUI dispatch opts for cowboy
        GUIDispatch = [
            % Matching requests will be redirected
            % to the same address without leading 'www.'
            % Cowboy does not have a mechanism to match
            % every hostname starting with 'www.'
            % This will match hostnames with up to 6 segments
            % e. g. www.seg2.seg3.seg4.seg5.com
            {"www.:_[.:_[.:_[.:_[.:_]]]]", [
                % redirector_handler is defined in cluster_worker
                {'_', redirector_handler, []}
            ]},
            % Redirect requests in form: alias.onedata.org
            {":alias." ++ GRHostname, [{'_', client_redirect_handler, [GuiPort]}]},
            % Proper requests are routed to handler modules
            % Proper requests are routed to handler modules
            {'_', [
                {<<"/google97a2428c78c25c27.html">>, cowboy_static,
                    {file, <<"resources/gui_static/google_analytics/google97a2428c78c25c27.html">>}},
                {"/nagios/[...]", nagios_handler, []},
                {?WEBSOCKET_PREFIX_PATH ++ "[...]", gui_ws_handler, []},
                {DocsPath ++ "/[...]", static_docs_handler, []},
                {"/[...]", gui_static_handler, {dir, DocRoot}}
            ]}
        ],

        % Initialize auth handler
        auth_config:load_auth_config(),

        % Call gui init, which will call init on all modules that might need state.
        gui:init(),
        % Start the listener for web gui and nagios handler
        {ok, _} = ranch:start_listener(?gui_https_listener, GuiNbAcceptors,
            ranch_etls, [
                {ip, {127, 0, 0, 1}},
                {port, GuiPort},
                {cacertfile, CaCertFile},
                {certfile, CertFile},
                {keyfile, KeyFile},
                {ciphers, ssl:cipher_suites() -- weak_ciphers()},
                {versions, ['tlsv1.2', 'tlsv1.1']}
            ], cowboy_protocol, [
                {env, [{dispatch, cowboy_router:compile(GUIDispatch)}]},
                {max_keepalive, MaxKeepAlive},
                {timeout, Timeout},
                % On every request, add headers that improve security to the response
                {onrequest, fun gui_utils:onrequest_adjust_headers/1}
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
    case catch cowboy:stop_listener(?gui_https_listener) of
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
    case http_client:get(Endpoint, [], <<>>, [insecure]) of
        {ok, _, _, _} -> ok;
        _ -> {error, server_not_responding}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns list of weak ciphers.
%% @end
-spec weak_ciphers() -> list().
%%--------------------------------------------------------------------
weak_ciphers() ->
    [{dhe_rsa, des_cbc, sha}, {rsa, des_cbc, sha}].
