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

-include("rest_config.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

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
    {ok, RestPort} = application:get_env(?APP_Name, rest_port),
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
        {ok, RestHttpsAcceptors} = application:get_env(?APP_Name, rest_https_acceptors),
        {ok, RESTAPIPrefixStr} = application:get_env(?APP_Name, rest_api_prefix),
        RESTAPIPrefix = str_utils:to_binary(RESTAPIPrefixStr),

        % Get cert paths
        {ok, ZoneCADir} = application:get_env(?APP_Name, ozpca_dir),
        {ok, ZoneKeyFile} = application:get_env(?APP_Name, oz_key_file),
        {ok, ZoneCertFile} = application:get_env(?APP_Name, oz_cert_file),
        {ok, ZoneCertDomain} = application:get_env(?APP_Name, http_domain),

        {ok, GuiCertFile} = application:get_env(?APP_Name, gui_cert_file),
        {ok, GuiKeyFile} = application:get_env(?APP_Name, gui_key_file),
        {ok, GuiCaCertFile} = application:get_env(?APP_Name, gui_cacert_file),

        ozpca:start(ZoneCADir, ZoneCertFile, ZoneKeyFile, ZoneCertDomain),
        auth_logic:start(),

        {ok, ZoneCABin} = file:read_file(ozpca:cacert_path(ZoneCADir)),
        [{_, ZoneCADER, _} | _] = public_key:pem_decode(ZoneCABin),

        {ok, GuiCABin} = file:read_file(GuiCaCertFile),
        [{_, GuiCADER, _} | _] = public_key:pem_decode(GuiCABin),

        GRHostname = dns_query_handler:get_canonical_hostname(),

        RESTRoutes = lists:append([
            identities_rest_module:routes(),
            user_rest_module:routes(),
            provider_rest_module:routes(),
            spaces_rest_module:routes(),
            groups_rest_module:routes(),
            privileges_rest_module:routes(),
            handle_services_rest_module:routes(),
            handles_rest_module:routes()
        ]),
        RESTRoutesWithPrefix = lists:map(
            fun({Path, Module, InitialState}) ->
                {<<RESTAPIPrefix/binary, Path/binary>>, Module, InitialState}
            end, RESTRoutes),

        Dispatch = cowboy_router:compile([
            % Redirect requests in form: alias.onedata.org
            {":alias." ++ GRHostname, [{'_', client_redirect_handler, [RestPort]}]},
            {'_', lists:append([
                [{<<"/crl.pem">>, cowboy_static, {file, filename:join(ZoneCADir, "crl.pem")}}],
                RESTRoutesWithPrefix
            ])}
        ]),

        {ok, _} = cowboy:start_https(?rest_listener, RestHttpsAcceptors,
            [
                {port, RestPort},
                % @todo Use gui cert files rather than certs generated by GR, since
                % we don't yet have a mechanism of distributing the CA cert.
                {certfile, GuiCertFile},
                {keyfile, GuiKeyFile},
                {cacerts, [GuiCADER, ZoneCADER]},
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
%% @doc
%% {@link listener_behaviour} callback stop/0.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    case catch cowboy:stop_listener(?rest_listener) of
        (ok) ->
            ok;
        (Error) ->
            ?error("Error on stopping listener ~p: ~p", [?rest_listener, Error]),
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
%%--------------------------------------------------------------------
-spec weak_ciphers() -> list().
weak_ciphers() ->
    [{dhe_rsa, des_cbc, sha}, {rsa, des_cbc, sha}].
