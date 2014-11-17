%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides raw communication layer between Global
%% Registry and provider based on WebSocket Secure protocol
%% @end
%% ===================================================================
-module(op_channel_handler).
-author("Krzysztof Trzepla").

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

%% Cowboy WebSocket handler callbacks
-export([init/3,
    websocket_init/3,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3]).

-record(state, {provider}).

%% ====================================================================
%% Cowboy WebSocket handler callbacks
%% ====================================================================

%% init/3
%% ====================================================================
%% @doc Upgrades the protocol to WebSocket.
%% @end
-spec init({TransportName, ProtocolName}, Req, Opts) ->
    {upgrade, protocol, cowboy_websocket} when
    TransportName :: tcp | ssl | atom(),
    ProtocolName :: http | atom(),
    Req :: cowboy_req:req(),
    Opts :: any().
%% ====================================================================
init(_Protocol, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


%% websocket_init/3
%% ====================================================================
%% @doc Initializes the state for a session.
%% @end
-spec websocket_init(TransportName, Req, Opts) ->
    {ok, Req, State} | {ok, Req, State, hibernate} |
    {ok, Req, State, Timeout} | {ok, Req, State, Timeout, hibernate} |
    {shutdown, Req} when
    TransportName :: tcp | ssl | atom(),
    Req :: cowboy_req:req(),
    Opts :: any(),
    State :: any(),
    Timeout :: timeout().
%% ====================================================================
websocket_init(ssl, Req, []) ->
    try
        {ok, PeerCert} = ssl:peercert(cowboy_req:get(socket, Req)),
        {ok, Provider} = grpca:verify_provider(PeerCert),
        gen_server:cast(?OpChannel, {add_connection, Provider, self()}),
        {ok, Req, #state{provider = Provider}}
    catch
        _:Reason ->
            ?warning("Attempted authentication with bad peer certificate: ~p", [Reason]),
            {shutdown, Req}
    end;

websocket_init(_TransportName, Req, _Opts) ->
    {shutdown, Req}.


%% websocket_handle/3
%% ====================================================================
%% @doc Handles the data received from the Websocket connection.
%% @end
-spec websocket_handle(InFrame, Req, State) ->
    {ok, Req, State} | {ok, Req, State, hibernate} |
    {reply, OutFrame | [OutFrame], Req, State} |
    {reply, OutFrame | [OutFrame], Req, State, hibernate} |
    {shutdown, Req, State} when
    InFrame :: {text | binary | ping | pong, binary()},
    Req :: cowboy_req:req(),
    State :: any(),
    OutFrame :: cowboy_websocket:frame().
%% ====================================================================
websocket_handle({binary, Data}, Req, State) ->
    ?dump(Data),
    {ok, Req, State};

websocket_handle(_InFrame, Req, State) ->
    {ok, Req, State}.


%% websocket_info/3
%% ====================================================================
%% @doc Handles the Erlang message received.
%% @end
-spec websocket_info(Info, Req, State) ->
    {ok, Req, State} | {ok, Req, State, hibernate} |
    {reply, OutFrame | [OutFrame], Req, State} |
    {reply, OutFrame | [OutFrame], Req, State, hibernate} |
    {shutdown, Req, State} when
    Info :: any(),
    Req :: cowboy_req:req(),
    State :: any(),
    OutFrame :: cowboy_websocket:frame().
%% ====================================================================
websocket_info(terminate, Req, State) ->
    {shutdown, Req, State};

websocket_info({push, Msg}, Req, State) ->
    {reply, {binary, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.


%% websocket_terminate/3
%% ====================================================================
%% @doc Performs any necessary cleanup of the state.
%% @end
-spec websocket_terminate(Reason, Req, State) -> ok when
    Reason :: {normal, shutdown | timeout} | {remote, closed} |
    {remote, cowboy_websocket:close_code(), binary()} |
    {error, badencoding | badframe | closed | atom()},
    Req :: cowboy_req:req(),
    State :: any().
%% ====================================================================
websocket_terminate(_Reason, _Req, _State) ->
    ok.