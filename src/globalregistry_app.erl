%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Applicatin main app file
%%% @end
%%%-------------------------------------------------------------------
-module(globalregistry_app).
-author("Tomasz Lichon").

-behaviour(application).

-include_lib("ctool/include/logging.hrl").
-include_lib("gui/include/gui.hrl").
-include("rest_config.hrl").
-include("gui_config.hrl").
-include("registered_names.hrl").
-include("messages_white_list.hrl").
-include("op_channel/op_channel.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    % DNS config needs to be loaded here as n2o listener needs to access it.
    dns_query_handler:load_config(),
    test_node_starter:maybe_start_cover(),
    activate_white_lists(),
    case {start_rest(), start_op_channel(), start_gui(), start_redirector()} of
        {ok, ok, ok, ok} ->
            case globalregistry_sup:start_link() of
                {ok, Pid} ->
                    case start_dns() of
                        ok ->
                            {ok, Pid};
                        Err ->
                            Err
                    end;
                Error ->
                    Error
            end;
        {{error, Reason}, _, _, _} ->
            {error, {cannot_start_rest, Reason}};
        {_, {error, Reason}, _, _} ->
            {error, {cannot_start_op_channel, Reason}};
        {_, _, {error, Reason}, _} ->
            {error, {cannot_start_gui, Reason}};
        {_, _, _, {error, Reason}} ->
            {error, {cannot_start_redirector, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    cowboy:stop_listener(?rest_listener),
    cowboy:stop_listener(?op_channel_listener),
    cowboy:stop_listener(?gui_https_listener),
    cowboy:stop_listener(?gui_redirector_listener),
    stop_dns(),
    stop_rest(),
    test_node_starter:maybe_stop_cover(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Starts cowboy with rest api
%%--------------------------------------------------------------------
-spec start_rest() -> ok | {error, term()}.
start_rest() ->
    try
        % Get rest config
        {ok, RestPort} = application:get_env(?APP_Name, rest_port),
        {ok, RestHttpsAcceptors} = application:get_env(?APP_Name, rest_https_acceptors),

        % Get cert paths
        {ok, GrpCADir} = application:get_env(?APP_Name, grpca_dir),
        {ok, GrpKeyFile} = application:get_env(?APP_Name, grpkey_file),
        {ok, GrpCertFile} = application:get_env(?APP_Name, grpcert_file),
        {ok, GrpCertDomain} = application:get_env(?APP_Name, http_domain),

        {ok, GuiCertFile} = application:get_env(?APP_Name, gui_cert_file),
        {ok, GuiKeyFile} = application:get_env(?APP_Name, gui_key_file),
        {ok, GuiCaCertFile} = application:get_env(?APP_Name, gui_cacert_file),

        grpca:start(GrpCADir, GrpCertFile, GrpKeyFile, GrpCertDomain),
        auth_logic:start(),

        {ok, GrpCABin} = file:read_file(grpca:cacert_path(GrpCADir)),
        [{_, GrpCADER, _} | _] = public_key:pem_decode(GrpCABin),

        {ok, GuiCABin} = file:read_file(GuiCaCertFile),
        [{_, GuiCADER, _} | _] = public_key:pem_decode(GuiCABin),

        Dispatch = cowboy_router:compile([
            {'_', lists:append([
                [{<<"/crl.pem">>, cowboy_static, {file, filename:join(GrpCADir, "crl.pem")}}],
                user_rest_module:routes(),
                provider_rest_module:routes(),
                spaces_rest_module:routes(),
                groups_rest_module:routes()
            ])}
        ]),

        {ok, _} = cowboy:start_https(?rest_listener, RestHttpsAcceptors,
            [
                {port, RestPort},
                % @todo Use gui cert files rather than certs generated by GR, since
                % we don't yet have a mechanism of distributing the CA cert.
                {certfile, GuiCertFile},
                {keyfile, GuiKeyFile},
                {cacerts, [GuiCADER, GrpCADER]},
                {verify, verify_peer},
                {ciphers, ssl:cipher_suites() -- weak_ciphers()},
                {versions, ['tlsv1.2', 'tlsv1.1']}
            ],
            [
                {env, [{dispatch, Dispatch}]}
            ]),
        ok
    catch
        _Type:Error ->
            ?error_stacktrace("Could not start rest, error: ~p", [Error]),
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Stops rest dependencies
%%--------------------------------------------------------------------
-spec stop_rest() -> ok.
stop_rest() ->
    auth_logic:stop(),
    grpca:stop().

%%--------------------------------------------------------------------
%% @private
%% @doc Starts communication channel for providers
%%--------------------------------------------------------------------
-spec start_op_channel() -> ok | {error, term()}.
start_op_channel() ->
    try
        % Get provider channel config
        {ok, ProviderChannelPort} = application:get_env(?APP_Name, op_channel_port),
        {ok, ProviderChannelHttpsAcceptors} = application:get_env(?APP_Name, op_channel_https_acceptors),

        % Get cert paths
        {ok, GrpCADir} = application:get_env(?APP_Name, grpca_dir),
        {ok, GuiCaCertFile} = application:get_env(?APP_Name, gui_cacert_file),
        {ok, GuiCertFile} = application:get_env(?APP_Name, gui_cert_file),
        {ok, GuiKeyFile} = application:get_env(?APP_Name, gui_key_file),

        {ok, GrpCABin} = file:read_file(grpca:cacert_path(GrpCADir)),
        [{_, GrpCADER, _} | _] = public_key:pem_decode(GrpCABin),

        {ok, GuiCABin} = file:read_file(GuiCaCertFile),
        [{_, GuiCADER, _} | _] = public_key:pem_decode(GuiCABin),

        Dispatch = cowboy_router:compile([{'_', [{?op_channel_endpoint, op_channel_handler, []}]}]),

        {ok, _} = cowboy:start_https(?op_channel_listener, ProviderChannelHttpsAcceptors,
            [
                {port, ProviderChannelPort},
                % @todo Use gui cert files rather than certs generated by GR, since
                % we don't yet have a mechanism of distributing the CA cert.
                {certfile, GuiCertFile},
                {keyfile, GuiKeyFile},
                {cacerts, [GuiCADER, GrpCADER]},
                {verify, verify_peer},
                {ciphers, ssl:cipher_suites() -- weak_ciphers()},
                {versions, ['tlsv1.2', 'tlsv1.1']}
            ],
            [
                {env, [{dispatch, Dispatch}]}
            ]),
        ok
    catch
        _Type:Error ->
            ?error_stacktrace("Could not start provider channel, error: ~p", [Error]),
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Starts n2o server
%%--------------------------------------------------------------------
-spec start_gui() -> ok | {error, term()}.
start_gui() ->
    try
        %% TODO for development
        %% This is needed for the redirection to provider to work, as
        %% we don't use legit server certificates on providers.
        application:set_env(ctool, verify_server_cert, false),

        % Get gui config
        {ok, DocRoot} = {ok, "data/gui_static"},
        {ok, GuiPort} = application:get_env(?APP_Name, gui_port),
        {ok, GuiNbAcceptors} = application:get_env(?APP_Name, gui_https_acceptors),
        {ok, Timeout} = application:get_env(?APP_Name, gui_socket_timeout),
        {ok, MaxKeepAlive} = application:get_env(?APP_Name, gui_max_keepalive),
        % Get cert paths
        {ok, CaCertFile} = application:get_env(?APP_Name, gui_cacert_file),
        {ok, CertFile} = application:get_env(?APP_Name, gui_cert_file),
        {ok, KeyFile} = application:get_env(?APP_Name, gui_key_file),

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
                % @todo use redirector_handler from cluster_worker
                {'_', https_redirect_handler, []}
            ]},
            % Redirect requests in form: alias.onedata.org
            {":alias." ++ GRHostname, [{'_', client_redirect_handler, []}]},
            % Proper requests are routed to handler modules
            % Proper requests are routed to handler modules
            {'_', [
                {<<"/google97a2428c78c25c27.html">>, cowboy_static,
                    {file, <<"resources/gui_static/google_analytics/google97a2428c78c25c27.html">>}},
                {"/nagios/[...]", nagios_handler, []},
                {?WEBSOCKET_PREFIX_PATH ++ "[...]", gui_ws_handler, []},
                {"/[...]", gui_static_handler, {dir, DocRoot}}
            ]}
        ],

        % Initilize auth handler
        auth_config:load_auth_config(),

        % Call gui init, which will call init on all modules that might need state.
        gui:init(),
        % Start the listener for web gui and nagios handler
        {ok, _} = ranch:start_listener(?gui_https_listener, GuiNbAcceptors,
            ranch_ssl2, [
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
%% @private
%% @doc Generates static file routing for cowboy.
%% @end
%%--------------------------------------------------------------------
-spec static_dispatches(DocRoot :: string(), StaticPaths :: [string()]) -> term().
static_dispatches(DocRoot, StaticPaths) ->
    _StaticDispatches = lists:map(fun(Dir) ->
        {Dir ++ "[...]", cowboy_static, {dir, DocRoot ++ Dir}}
                                  end, StaticPaths).

%%--------------------------------------------------------------------
%% @private
%% @doc Starts a cowboy listener that will redirect all requests of http to https.
%% @end
%%--------------------------------------------------------------------
-spec start_redirector() -> ok | {error, term()}.
start_redirector() ->
    try
        {ok, RedirectPort} = application:get_env(?APP_Name, gui_redirect_port),
        {ok, RedirectNbAcceptors} = application:get_env(?APP_Name, gui_redirect_acceptors),
        {ok, Timeout} = application:get_env(?APP_Name, gui_socket_timeout),

        RedirectDispatch = [
            {'_', [
                {'_', https_redirect_handler, []}
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
            ]),
        ok
    catch
        _Type:Error ->
            ?error_stacktrace("Could not start redirector listener, error: ~p", [Error]),
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Starts the DNS server.
%% @end
%%--------------------------------------------------------------------
-spec start_dns() -> ok | {error, term()}.
start_dns() ->
    {ok, DNSPort} = application:get_env(?APP_Name, dns_port),
    {ok, EdnsMaxUdpSize} = application:get_env(?APP_Name, edns_max_udp_size),
    {ok, TCPNumAcceptors} = application:get_env(?APP_Name, dns_tcp_acceptor_pool_size),
    {ok, TCPTImeout} = application:get_env(?APP_Name, dns_tcp_timeout),
    % DNS config is loaded in start function
    OnFailureFun = fun() -> ?error("DNS Server failed to start!") end,
    ?info("Starting DNS server..."),
    case dns_server:start(globalregistry_sup, DNSPort, dns_query_handler, EdnsMaxUdpSize, TCPNumAcceptors, TCPTImeout, OnFailureFun) of
        ok ->
            ok;
        Error ->
            ?error("Cannot start DNS server - ~p", [Error]),
            OnFailureFun()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Stops the DNS server.
%% @end
%%--------------------------------------------------------------------
-spec stop_dns() -> ok | {error, term()}.
stop_dns() ->
    dns_server:stop(globalregistry_sup).

%%--------------------------------------------------------------------
%% @private
%% @doc Activates white lists of messages that can be processed by
%% Global Registry.
%% @end
%%--------------------------------------------------------------------
-spec activate_white_lists() -> ok.
activate_white_lists() ->
    lists:foreach(fun(Decoder) ->
        list_to_atom(atom_to_list(Decoder) ++ "_pb")
                  end, ?DecodersList),

    lists:foreach(fun(Message) ->
        {list_to_atom("decode_" ++ atom_to_list(Message)), list_to_atom("encode_" ++ atom_to_list(Message))}
                  end, ?MessagesWhiteList),

    ok.


%% weak_ciphers/0
%% ====================================================================
%% @doc Returns list of weak ciphers.
%% @end
-spec weak_ciphers() -> list().
%% ====================================================================
weak_ciphers() ->
    [{dhe_rsa, des_cbc, sha}, {rsa, des_cbc, sha}].
