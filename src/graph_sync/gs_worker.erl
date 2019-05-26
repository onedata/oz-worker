%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is responsible for starting and monitoring a single, global
%%% gs_server_worker instance.
%%% @end
%%%-------------------------------------------------------------------
-module(gs_worker).
-author("Lukasz Opiola").

-behaviour(worker_plugin_behaviour).

-include("registered_names.hrl").
-include("graph_sync/oz_graph_sync.hrl").
-include_lib("ctool/include/logging.hrl").

%% worker_plugin_behaviour callbacks
-export([init/1, handle/1, cleanup/0]).

%% API
-export([supervisor_flags/0]).

-define(GS_WORKER_SUP, gs_worker_sup).
-define(STREAM_HEALTHCHECK_INTERVAL, oz_worker:get_env(
    gs_server_stream_healthcheck_interval, timer:seconds(5))).

%%%===================================================================
%%% worker_plugin_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link worker_plugin_behaviour} callback init/1.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, worker_host:plugin_state()} | {error, Reason :: term()}.
init(_Args) ->
    erlang:send_after(?STREAM_HEALTHCHECK_INTERVAL, self(),
        {sync_timer, stream_healthcheck}
    ),
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @doc
%% {@link worker_plugin_behaviour} callback handle/1.
%% @end
%%--------------------------------------------------------------------
-spec handle(ping | healthcheck | stream_healthcheck) -> pong | ok.
handle(ping) ->
    pong;
handle(healthcheck) ->
    ok;
handle(stream_healthcheck) ->
    try
        stream_healthcheck()
    catch
        _:Reason ->
            ?error_stacktrace(
                "Failed to start Graph Sync server stream due to: ~p", [Reason]
            )
    end,
    erlang:send_after(?STREAM_HEALTHCHECK_INTERVAL, self(),
        {sync_timer, stream_healthcheck}
    ),
    ok;
handle(Request) ->
    ?log_bad_request(Request).

%%--------------------------------------------------------------------
%% @doc
%% {@link worker_plugin_behaviour} callback cleanup/0
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok | {error, timeout | term()}.
cleanup() ->
    ok.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns supervisor flags for gs_worker.
%% @end
%%--------------------------------------------------------------------
-spec supervisor_flags() -> supervisor:sup_flags().
supervisor_flags() ->
    #{strategy => one_for_one, intensity => 0, period => 1}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec stream_healthcheck() -> ok.
stream_healthcheck() ->
    Pid = global:whereis_name(?GS_SERVER_WORKER_GLOBAL_NAME),
    Node = consistent_hasing:get_node(?GS_SERVER_WORKER_GLOBAL_NAME),
    case {Pid, Node =:= node()} of
        {undefined, true} ->
            start_gs_server_worker(),
            ok;
        _ ->
            ok
    end.


-spec start_gs_server_worker() -> supervisor:startchild_ret().
start_gs_server_worker() ->
    supervisor:start_child(?GS_WORKER_SUP, gs_server_worker_connection_spec()).


-spec gs_server_worker_connection_spec() -> supervisor:child_spec().
gs_server_worker_connection_spec() -> #{
    id => ?GS_SERVER_WORKER_GLOBAL_NAME,
    start => {gs_server_worker, start_link, []},
    restart => temporary,
    shutdown => timer:seconds(10),
    type => worker,
    modules => [gs_server_worker]
}.
