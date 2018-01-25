%%%--------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module is responsible for REST listener starting and stopping.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_listener).
-author("Michal Zmuda").

-include("rest.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-behaviour(listener_behaviour).

%% listener_behaviour callbacks
-export([port/0, start/0, stop/0, healthcheck/0]).

%% API
-export([routes/0]).

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
    {ok, RestPort} = application:get_env(?APP_NAME, rest_port),
    RestPort.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback start/0.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, Reason :: term()}.
start() ->
    try
        % Get rest config
        RestPort = port(),
        {ok, RestHttpsAcceptors} = application:get_env(?APP_NAME, rest_https_acceptors),

        % Get cert paths
        {ok, ZoneCADir} = application:get_env(?APP_NAME, ozpca_dir),
        {ok, ZoneCaCertPem} = file:read_file(ozpca:cacert_path(ZoneCADir)),

        {ok, KeyFile} = application:get_env(?APP_NAME, web_key_file),
        {ok, CertFile} = application:get_env(?APP_NAME, web_cert_file),
        {ok, CaCertsDir} = application:get_env(?APP_NAME, cacerts_dir),
        {ok, CaCertPems} = file_utils:read_files({dir, CaCertsDir}),
        CaCerts = lists:map(fun cert_decoder:pem_to_der/1, [ZoneCaCertPem | CaCertPems]),

%%        {ok, Hostname} = application:get_env(oz_worker, http_domain),
        Dispatch = cowboy_router:compile([
            % TODO VFS-2873 Currently unused
            % Redirect requests in form: alias.onedata.org
%%            {":alias." ++ Hostname, [{'_', client_redirect_handler, [RestPort]}]},
            {'_', routes()}
        ]),

        {ok, _} = ranch:start_listener(?REST_LISTENER, RestHttpsAcceptors,
            ranch_ssl, [
                {port, RestPort},
                % @todo Use gui cert files rather than certs generated by GR, since
                % we don't yet have a mechanism of distributing the CA cert.
                {keyfile, KeyFile},
                {certfile, CertFile},
                {cacerts, CaCerts},
                {verify, verify_peer},
                {ciphers, ssl_utils:safe_ciphers()}
            ], cowboy_protocol,
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
%% @doc
%% {@link listener_behaviour} callback stop/0.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    case catch ranch:stop_listener(?REST_LISTENER) of
        (ok) ->
            ok;
        (Error) ->
            ?error("Error on stopping listener ~p: ~p", [?REST_LISTENER, Error]),
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
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a Cowboy-understandable PathList of routes.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{Path :: binary(), Module :: module(), State :: term()}].
routes() ->
    {ok, ZoneCADir} = application:get_env(?APP_NAME, ozpca_dir),
    [
        {<<"/crl.pem">>, cowboy_static, {file, filename:join(ZoneCADir, "crl.pem")}} |
        rest_handler:rest_routes()
    ].
