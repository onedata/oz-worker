%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Applicatin main app file
%% @end
%% ===================================================================
-module(globalregistry_app).
-author("Tomasz Lichon").

-behaviour(application).

%% Includes
-include_lib("ctool/include/logging.hrl").
-include("rest_config.hrl").
-include("gui_config.hrl").
-include("registered_names.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% start/2
%% ===================================================================
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
%% ===================================================================
start(_StartType, _StartArgs) ->
    {RestAns, _, State} = start_rest(),
    {GuiAns, _} = start_n2o(),
    {RedirectorAns, _} = start_redirector(),
    case {RestAns, GuiAns, RedirectorAns} of
        {ok, ok, ok} ->
            case globalregistry_sup:start_link() of
                {ok, Pid} ->
                    {ok, Pid, State};
                Error ->
                    Error
            end;
        {error, _, _} ->
            {error, cannot_start_rest};
        {_, error, _} ->
            {error, cannot_start_gui};
        {_, _, error} ->
            {error, cannot_start_redirector}
    end.

%% stop/1
%% ===================================================================
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
-spec(stop(State :: term()) -> term()).
%% ===================================================================
stop(State) ->
    cowboy:stop_listener(?rest_listener),
    cowboy:stop_listener(?gui_https_listener),
    cowboy:stop_listener(?gui_redirector_listener),
    stop_rest(State),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% start_rest/0
%% ===================================================================
%% @doc Starts cowboy with rest api
-spec start_rest() -> {ok, pid(), State :: term()} | {error, any()}.
%% ===================================================================
start_rest() ->
    try
        % Get rest config
        {ok, RestPort} = application:get_env(?APP_Name, rest_port),
        {ok, RestHttpsAcceptors} = application:get_env(?APP_Name, rest_https_acceptors),

        % Get cert paths
        {ok, GRPCADir} = application:get_env(?APP_Name, grpca_dir),
        {ok, RestCertFile} = application:get_env(?APP_Name, rest_cert_file),
        {ok, RestKeyFile} = application:get_env(?APP_Name, rest_key_file),
        {ok, RestCertDomain} = application:get_env(?APP_Name, rest_cert_domain),

        grpca:start(GRPCADir, RestCertFile, RestKeyFile, RestCertDomain),
        State = auth_logic:start(),

        Dispatch = cowboy_router:compile([
            {'_', lists:append([
                user_rest_module:routes(),
                provider_rest_module:routes(),
                spaces_rest_module:routes(),
                groups_rest_module:routes(),
                auth_rest_module:routes()
            ])}
        ]),

        {ok, Pid} = cowboy:start_https(?rest_listener, RestHttpsAcceptors,
            [
                {port, RestPort},
                {cacertfile, grpca:cacert_path(GRPCADir)},
                {certfile, RestCertFile},
                {keyfile, RestKeyFile},
                {verify, verify_peer}
            ],
            [
                {env, [{dispatch, Dispatch}]}
            ]),

        {ok, Pid, State}
    catch
        _Type:Error ->
            ?error_stacktrace("Could not start rest, error: ~p", [Error]),
            {error, Error}
    end.

stop_rest(State) ->
    auth_logic:stop(State),
    grpca:stop().


%% start_n2o/0
%% ===================================================================
%% @doc Starts n2o server
-spec start_n2o() -> {ok, pid()} | {error, any()}.
%% ===================================================================
start_n2o() ->
    try
        % Get gui config
        {ok, GuiPort} = application:get_env(?APP_Name, gui_port),
        {ok, GuiHttpsAcceptors} = application:get_env(?APP_Name, gui_https_acceptors),
        {ok, GuiSocketTimeout} = application:get_env(?APP_Name, gui_socket_timeout),
        {ok, GuiMaxKeepalive} = application:get_env(?APP_Name, gui_max_keepalive),
        % Get cert paths
        {ok, CaCertFile} = application:get_env(?APP_Name, ca_cert_file),
        {ok, CertFile} = application:get_env(?APP_Name, cert_file),
        {ok, KeyFile} = application:get_env(?APP_Name, key_file),

        % Setup GUI dispatch opts for cowboy
        GUIDispatch = [
            % Matching requests will be redirected to the same address without leading 'www.'
            % Cowboy does not have a mechanism to match every hostname starting with 'www.'
            % This will match hostnames with up to 6 segments
            % e. g. www.seg2.seg3.seg4.seg5.com
            {"www.:_[.:_[.:_[.:_[.:_]]]]", [{'_', redirect_handler, []}]},
            % Proper requests are routed to handler modules
            {'_', static_dispatches(?gui_static_root, ?static_paths) ++ [
                {"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]},
                {'_', n2o_cowboy, []}
            ]}
        ],

        % Create ets tables and set envs needed by n2o
        gui_utils:init_n2o_ets_and_envs(GuiPort, ?gui_routing_module, ?session_logic_module, n2o_cowboy),

        % Initilize auth handler
        auth_config:load_auth_config(),

        {ok, _} = cowboy:start_https(?gui_https_listener, GuiHttpsAcceptors,
            [
                {port, GuiPort},
                {cacertfile, CaCertFile},
                {certfile, CertFile},
                {keyfile, KeyFile}
            ],
            [
                {env, [{dispatch, cowboy_router:compile(GUIDispatch)}]},
                {max_keepalive, GuiMaxKeepalive},
                {timeout, GuiSocketTimeout},
                % On every request, add headers that improve security to the response
                {onrequest, fun gui_utils:onrequest_adjust_headers/1}
            ])
    catch
        _Type:Error ->
            ?error_stacktrace("Could not start gui, error: ~p", [Error]),
            {error, Error}
    end.


%% Generates static file routing for cowboy.
static_dispatches(DocRoot, StaticPaths) ->
    _StaticDispatches = lists:map(fun(Dir) ->
        {Dir ++ "[...]", cowboy_static, {dir, DocRoot ++ Dir}}
    end, StaticPaths).


%% start_redirector_listener/0
%% ====================================================================
%% @doc Starts a cowboy listener that will redirect all requests of http to https.
%% @end
-spec start_redirector() -> ok | no_return().
%% ====================================================================
start_redirector() ->
    try
        {ok, RedirectPort} = application:get_env(?APP_Name, gui_redirect_port),
        {ok, RedirectNbAcceptors} = application:get_env(?APP_Name, gui_redirect_acceptors),
        {ok, Timeout} = application:get_env(?APP_Name, gui_socket_timeout),

        RedirectDispatch = [
            {'_', [
                {'_', redirect_handler, []}
            ]}
        ],

        {ok, _} = cowboy:start_http(?gui_redirector_listener, RedirectNbAcceptors,
            [
                {port, RedirectPort}
            ],
            [
                {env, [{dispatch, cowboy_router:compile(RedirectDispatch)}]},
                {max_keepalive, 1},
                {timeout, Timeout}
            ])
    catch
        _Type:Error ->
            ?error_stacktrace("Could not start redirector listener, error: ~p", [Error]),
            {error, Error}
    end.
