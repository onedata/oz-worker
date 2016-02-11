%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module manage communication with oneproviders.
%%% @end
%%%-------------------------------------------------------------------
-module(op_channel).
-behaviour(gen_server).

-include("registered_names.hrl").
-include("op_channel/op_channel.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?OpChannel}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init(_) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles call messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast({add_connection, Provider, Connection}, #state{providers = Providers, connections = Connections} = State) ->
    ?info("Provider ~p connected successfully.", [Provider]),
    link(Connection),
    case maps:find(Provider, Connections) of
        {ok, ProviderConnections} ->
            {noreply, State#state{
                providers = maps:put(Connection, Provider, Providers),
                connections = maps:put(Provider, [Connection | ProviderConnections], Connections)
            }};
        _ ->
            {noreply, State#state{
                providers = maps:put(Connection, Provider, Providers),
                connections = maps:put(Provider, [Connection], Connections)
            }}
    end;

handle_cast({push, Providers, Msg}, #state{connections = Connections} = State) ->
    utils:pforeach(fun(Provider) ->
        case maps:find(Provider, Connections) of
            {ok, ProviderConnections} ->
                Connection = lists:nth(crypto:rand_uniform(1, length(ProviderConnections) + 1), ProviderConnections),
                Connection ! {push, Msg};
            Other ->
                ?warning("Cannot find provider ~p connections: ~p", [Provider, Other])
        end
    end, Providers),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles all non call/cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info({'EXIT', Connection, Reason}, #state{providers = Providers, connections = Connections} = State) ->
    case maps:find(Connection, Providers) of
        {ok, Provider} ->
            ?warning("Connection to provider ~p lost due to: ~p", [Provider, Reason]),
            case maps:find(Provider, Connections) of
                {ok, [Connection]} ->
                    {noreply, State#state{
                        providers = maps:remove(Connection, Providers),
                        connections = maps:remove(Provider, Connections)
                    }};
                {ok, ProviderConnections} ->
                    {noreply, State#state{
                        providers = maps:remove(Connection, Providers),
                        connections = maps:put(Provider, ProviderConnections -- [Connection], Connections)
                    }};
                Other ->
                    ?error("Cannot find provider ~p connections: ~p", [Provider, Other]),
                    {noreply, State}
            end;
        Other ->
            ?error("Cannot find provider for connection ~p: ~p", [Connection, Other]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts process state when code is changed.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) -> {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
