%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module coordinates access to Round Robin Database.
%% @end
%% ===================================================================
-module(op_channel).
-behaviour(gen_server).

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0, push/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {providers = maps:new(), connections = maps:new()}).

%%%===================================================================
%%% API
%%%===================================================================

%% start_link/1
%% ====================================================================
%% @doc Starts the server.
%% @end
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?OpChannel}, ?MODULE, [], []).


%% push/2
%% ====================================================================
%% @doc Pushes message to providers.
%% @end
-spec push(Providers :: [binary()], Msg :: term()) -> ok.
%% ====================================================================
push(Providers, Msg) ->
    gen_server:cast(?OpChannel, {push, Providers, Msg}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
%% @end
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
    | {ok, State, Timeout}
    | {ok, State, hibernate}
    | {stop, Reason :: term()}
    | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(_) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
%% @end
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
    | {reply, Reply, NewState, Timeout}
    | {reply, Reply, NewState, hibernate}
    | {noreply, NewState}
    | {noreply, NewState, Timeout}
    | {noreply, NewState, hibernate}
    | {stop, Reason, Reply, NewState}
    | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
%% @end
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
    | {noreply, NewState, Timeout}
    | {noreply, NewState, hibernate}
    | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({add_connection, Provider, Connection}, #state{providers = Providers, connections = Connections} = State) ->
    ?info("Adding connection to provider: ~p", [Provider]),
    erlang:link(Connection),
    {noreply, State#state{
        providers = maps:put(Connection, Provider, Providers),
        connections = maps:put(Provider, Connection, Connections)
    }};
handle_cast({remove_connection, Provider, Connection}, #state{providers = Providers, connections = Connections} = State) ->
    ?info("Removing connection to provider: ~p", [Provider]),
    {noreply, State#state{
        providers = maps:remove(Connection, Providers),
        connections = maps:remove(Provider, Connections)
    }};
handle_cast({push, Providers, Msg}, #state{connections = Connections} = State) ->
    utils:pforeach(fun(Provider) ->
        Connection = maps:get(Provider, Connections, undefined),
        case Connection of
            undefined -> ok;
            _ -> Connection ! {push, Msg}
        end
    end, Providers),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
%% @end
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
    | {noreply, NewState, Timeout}
    | {noreply, NewState, hibernate}
    | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', Connection, Reason}, #state{providers = Providers, connections = Connections} = State) ->
    Provider = maps:get(Connection, Providers, undefined),
    ?error("Connection to provider ~p lost due to: ~p.", [Provider, Reason]),
    {noreply, State#state{
        providers = maps:remove(Connection, Providers),
        connections = maps:remove(Provider, Connections)
    }};
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
%% @end
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
    | shutdown
    | {shutdown, term()}
    | term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
%% @end
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
