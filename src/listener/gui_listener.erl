%%%--------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc redirector listener starting & stopping
%%% @end
%%%--------------------------------------------------------------------
-module(gui_listener).
-author("Michal Zmuda").

-include("gui_config.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-behaviour(listener_behaviour).

%% listener_behaviour callbacks
-export([port/0, start/0, stop/0, healthcheck/0]).

%%%===================================================================
%%% listener_starter_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link listener_starter_behaviour} callback port/0.
%% @end
%%--------------------------------------------------------------------
-spec port() -> integer().
port() ->
    {ok, Port} = application:get_env(?APP_Name, gui_port),
    Port.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_starter_behaviour} callback start/1.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, Reason :: term()}.
start() ->
    try
        %% TODO for development
        %% This is needed for the redirection to provider to work, as
        %% we don't use legit server certificates on providers.
        application:set_env(ctool, verify_server_cert, false),

        % Get gui config
        GuiPort = port(),
        {ok, GuiHttpsAcceptors} = application:get_env(?APP_Name, gui_https_acceptors),
        {ok, GuiSocketTimeout} = application:get_env(?APP_Name, gui_socket_timeout),
        {ok, GuiMaxKeepalive} = application:get_env(?APP_Name, gui_max_keepalive),
        % Get cert paths
        {ok, GuiCaCertFile} = application:get_env(?APP_Name, gui_cacert_file),
        {ok, GuiCertFile} = application:get_env(?APP_Name, gui_cert_file),
        {ok, GuiKeyFile} = application:get_env(?APP_Name, gui_key_file),

        GRHostname = dns_query_handler:get_canonical_hostname(),

        % Setup GUI dispatch opts for cowboy
        GUIDispatch = [
            % Matching requests will be redirected to the same address without leading 'www.'
            % Cowboy does not have a mechanism to match every hostname starting with 'www.'
            % This will match hostnames with up to 8 segments
            % e. g. www.seg2.seg3.seg4.seg5.seg6.seg7.com
            {"www.:_[.:_[.:_[.:_[.:_[.:_[.:_]]]]]]", [{'_', https_redirect_handler, []}]},
            % Redirect requests in form: alias.onedata.org
            {":alias." ++ GRHostname, [{'_', client_redirect_handler, []}]},
            % Proper requests are routed to handler modules
            {'_', static_dispatches(?gui_static_root, ?static_paths) ++ [
                {<<"/google97a2428c78c25c27.html">>, cowboy_static,
                    {file, <<"resources/gui_static/google_analytics/google97a2428c78c25c27.html">>}},
                {"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]},
                {'_', n2o_cowboy, []}
            ]}
        ],

        % Create ets tables and set envs needed by n2o
        gui_utils:init_n2o_ets_and_envs(GuiPort, ?gui_routing_module, ?session_logic_module, ?cowboy_bridge_module),

        % Initilize auth handler
        auth_config:load_auth_config(),

        {ok, _} = cowboy:start_https(?gui_https_listener, GuiHttpsAcceptors,
            [
                {port, GuiPort},
                {cacertfile, GuiCaCertFile},
                {certfile, GuiCertFile},
                {keyfile, GuiKeyFile},
                {ciphers, ssl:cipher_suites() -- weak_ciphers()},
                {versions, ['tlsv1.2', 'tlsv1.1']}
            ],
            [
                {env, [{dispatch, cowboy_router:compile(GUIDispatch)}]},
                {max_keepalive, GuiMaxKeepalive},
                {timeout, GuiSocketTimeout},
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
%% {@link listener_starter_behaviour} callback stop/1.
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
%% Returns the status of a listener.
%% @end
%%--------------------------------------------------------------------
-spec healthcheck() -> ok | {error, server_not_responding}.
healthcheck() ->
    Port = port(),
    case http_client:get("https://127.0.0.1:" ++ integer_to_list(Port), [], <<>>, [insecure]) of
        {ok, _, _, _} ->
            ok;
        _ ->
            {error, server_not_responding}
    end.

%% ====================================================================
%% @private
%% @doc
%% Returns list of weak ciphers.
%% @end
-spec weak_ciphers() -> list().
%% ====================================================================
weak_ciphers() ->
    [{dhe_rsa, des_cbc, sha}, {rsa, des_cbc, sha}].


%%--------------------------------------------------------------------
%% @private
%% @doc Generates static file routing for cowboy.
%% @end
%%--------------------------------------------------------------------
-spec static_dispatches(DocRoot :: string(), StaticPaths :: [string()]) -> term().
static_dispatches(DocRoot, StaticPaths) ->
    _StaticDispatches = lists:map(fun(Dir) ->
        {Dir ++ "[...]", cowboy_static, {dir, DocRoot ++ Dir}}
    end, StaticPaths).
