%%%--------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module is responsible for redirector listener starting and stopping.
%%% @end
%%%--------------------------------------------------------------------
-module(oz_redirector_listener).
-author("Tomasz Lichon").
-author("Michal Zmuda").
-author("Jakub Kudzia").

-include_lib("cluster_worker/include/global_definitions.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

% Session logic module
-define(SESSION_LOGIC_MODULE, session_logic).

% Cowboy listener references
-define(HTTP_REDIRECTOR_LISTENER, http).

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
    {ok, RedirectPort} = application:get_env(?CLUSTER_WORKER_APP_NAME,
        http_redirect_port),
    RedirectPort.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback start/0.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, Reason :: term()}.
start() ->
    {ok, RedirectNbAcceptors} = application:get_env(?CLUSTER_WORKER_APP_NAME,
        http_number_of_http_acceptors),
    {ok, Timeout} = application:get_env(?CLUSTER_WORKER_APP_NAME,
        http_socket_timeout_seconds),
    {ok, OAI_PMH_PATH} = application:get_env(?APP_NAME, oai_pmh_api_prefix),

    Dispatch = cowboy_router:compile([
        {'_', [
            {OAI_PMH_PATH ++ "/[...]", oai_handler, []},
            {"/[...]", redirector_handler, []}
        ]}
    ]),
    Result = cowboy:start_clear(?HTTP_REDIRECTOR_LISTENER,
        [
            {port, port()},
            {num_acceptors, RedirectNbAcceptors}
        ],
        #{
            env => #{dispatch => Dispatch},
            max_keepalive => 1,
            request_timeout => timer:seconds(Timeout)
        }),
    ?critical("~n~nASD~n~n~p~n~n", [Result]),
    case Result of
        {ok, _} -> ok;
        _ -> Result
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback stop/0.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    case cowboy:stop_listener(?HTTP_REDIRECTOR_LISTENER) of
        ok ->
            ok;
        {error, Error} ->
            ?error("Error on stopping listener ~p: ~p",
                [?HTTP_REDIRECTOR_LISTENER, Error]),
            {error, redirector_stop_error}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback healthcheck/0.
%% @end
%%--------------------------------------------------------------------
-spec healthcheck() -> ok | {error, server_not_responding}.
healthcheck() ->
    Endpoint = str_utils:format_bin("http://127.0.0.1:~B", [port()]),
    case http_client:get(Endpoint, #{}, <<>>, []) of
        {ok, _, _, _} -> ok;
        _ -> {error, server_not_responding}
    end.
