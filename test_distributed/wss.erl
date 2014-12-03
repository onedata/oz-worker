%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides simple WebSocket client that allows connecting
%%      to Global Registry with given host.
%% @end
%% ===================================================================
-module(wss).
-author("Krzysztof Trzepla").

%% API
-export([connect/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% connect/3
%% ====================================================================
%% @doc Connects to cluster with given host, port and transport options.
%% NOTE! Some options may conflict with websocket_client's options,
%% so don't pass any options but certificate configuration.
-spec connect(Host :: string(), Port :: non_neg_integer(), Opts :: [term()]) ->
    {ok, Socket :: pid()} | {error, timout} | {error, Reason :: term()}.
%% ====================================================================
connect(Host, Port, Opts) ->
    process_flag(trap_exit, true),
    wss_handler:flush_errors(),
    ssl:start(),
    case websocket_client:start_link("wss://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/oneprovider",
        wss_handler, [self()], Opts ++ [{reuse_sessions, false}]) of
        {ok, Socket} ->
            receive
                {Socket, connected} -> {ok, Socket};
                {error, Reason} -> {error, Reason};
                {'EXIT', Socket, Reason} -> {error, Reason}
            after 5000 ->
                {error, timeout}
            end;
        {error, Reason} ->
            {error, Reason}
    end.