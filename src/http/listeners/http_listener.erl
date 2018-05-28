%%%--------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% @doc This module is responsible for starting and stopping
%%% HTTP listener that serves OAI PMH requests and redirects clients
%%% from HTTP to HTTPS in case of other requests.
%%% @end
%%%--------------------------------------------------------------------
-module(http_listener).
-author("Jakub Kudzia").

-behaviour(listener_behaviour).

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("cluster_worker/include/global_definitions.hrl").

% Cowboy listener references
-define(HTTP_LISTENER, http_listener).

% Listener config
-define(PORT, oz_worker:get_env(http_server_port, 80)).
-define(ACCEPTORS_NUM, oz_worker:get_env(http_acceptors, 10)).
-define(REQUEST_TIMEOUT, oz_worker:get_env(http_request_timeout, timer:seconds(30))).


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
    ?PORT.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback start/0.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, Reason :: term()}.
start() ->
    {ok, OAI_PMH_PATH} = oz_worker:get_env(oai_pmh_api_prefix),

    Dispatch = cowboy_router:compile([
        {'_', [
            {OAI_PMH_PATH ++ "/[...]", oai_handler, []},
            {"/[...]", redirector_handler, https_listener:port()}
        ]}
    ]),
    Result = cowboy:start_clear(?HTTP_LISTENER,
        [
            {port, port()},
            {num_acceptors, ?ACCEPTORS_NUM}
        ],
        #{
            env => #{dispatch => Dispatch},
            max_keepalive => 1,
            request_timeout => ?REQUEST_TIMEOUT
        }),
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
    case cowboy:stop_listener(?HTTP_LISTENER) of
        ok ->
            ok;
        {error, Error} ->
            ?error("Error on stopping listener ~p: ~p",
                [?HTTP_LISTENER, Error]),
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
