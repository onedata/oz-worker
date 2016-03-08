%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module fetches recent changes from the couchbase. It is designed
%%% to be singleton element of the system.
%%% @todo do not monitor global name
%%% @todo use solution introduced in VFS-1748
%%% @end
%%%-------------------------------------------------------------------
-module(changes_bridge).
-author("Michal Zmuda").

-behaviour(gen_server).

-include("registered_names.hrl").
-include("subscriptions/subscriptions.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0, change_callback/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server but does not registers it yet.
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Handles changes & dispatches them to be processed.
%% @end
%%--------------------------------------------------------------------

-spec change_callback(Seq :: non_neg_integer(), datastore:document() | stream_ended,
    model_behaviour:model_type() | undefined) -> ok.

change_callback(_Seq, stream_ended, _Type) ->
    gen_server:cast(?MODULE, {stop, stream_ended});

change_callback(Seq, Doc, Type) ->
    worker_proxy:cast(?SUBSCRIPTIONS_WORKER_NAME, {handle_change, Seq, Doc, Type}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #{}} | {ok, State :: #{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    await_for_name_possibly_free(),
    case register_name_or_exit() of
        ok ->
            gen_server:cast({global, ?MODULE}, start_changes_stream),
            {ok, #{}};
        {error, Reason} -> {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #{}) ->
    {reply, Reply :: term(), NewState :: #{}} |
    {reply, Reply :: term(), NewState :: #{}, timeout() | hibernate} |
    {noreply, NewState :: #{}} |
    {noreply, NewState :: #{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #{}} |
    {stop, Reason :: term(), NewState :: #{}}).
handle_call(_Request, _From, State) ->
    ?log_bad_request(_Request),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #{}) ->
    {noreply, NewState :: #{}} |
    {noreply, NewState :: #{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #{}}).
handle_cast(start_changes_stream, State) ->
    try
        {ok, CurrentSeq} = fetch_last_seq(),
        ?info("Starting changes stream using changes from ~p", [CurrentSeq]),
        start_changes_stream(CurrentSeq),
        {noreply, State}
    catch E:R ->
        ?info("Changes stream failed to start ~p:~p", [E, R]),
        {ok, Timeout} = application:get_env(?APP_Name,
            changes_stream_restart_delay_seconds, 20),
        timer:sleep(timer:seconds(Timeout)),
        {stop, changes_stream_not_available, State}
    end;
handle_cast({stop, Reason}, State) ->
    ?info("Stopping changes bridge ~p due to ~p", [self(), Reason]),
    {stop, Reason, State};
handle_cast(stop, State) ->
    ?info("Stopping changes bridge ~p", [self()]),
    {stop, normal, State};
handle_cast(_Request, State) ->
    ?log_bad_request(_Request),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #{}) ->
    {noreply, NewState :: #{}} |
    {noreply, NewState :: #{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate.
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #{},
    Extra :: term()) ->
    {ok, NewState :: #{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialize changes stream, which starts from given sequence number.
%% @end
%%--------------------------------------------------------------------

-spec start_changes_stream(Seq :: non_neg_integer()) -> no_return().
start_changes_stream(Start) ->
    couchdb_datastore_driver:changes_start_link(fun change_callback/3, Start, infinity).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get latest sequence number from datastore.
%% @end
%%--------------------------------------------------------------------

-spec fetch_last_seq() -> {ok, non_neg_integer()}| {error, term()}.
fetch_last_seq() ->
    try
        {ok, LastSeq, _} = couchdb_datastore_driver:db_run(couchbeam_changes,
            follow_once, [], 30),
        {ok, binary_to_integer(LastSeq)}
    catch
        E:R ->
            ?error_stacktrace("Last sequence unavailable as ~p:~p", [E, R]),
            {error, {E, R}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to register a name. When it fails, bridge should be restarted.
%% @end
%%--------------------------------------------------------------------

-spec register_name_or_exit() -> ok | {error, Reason :: term()}.
register_name_or_exit() ->
    global:trans({?MODULE, node()}, fun() ->
        case global:register_name(?MODULE, self()) of
            yes ->
                ?info("Becoming a leader"),
                ok;
            no ->
                ?info("Unsuccessful leader takeover"),
                {error, unsuccessful_takeover}
        end
    end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Awaits until global name *may* be available. There remains slight
%% risk of missing name becoming available - to overcome that waiting
%% time is limited.
%% @end
%%--------------------------------------------------------------------

-spec await_for_name_possibly_free() -> no_return().
await_for_name_possibly_free() ->
    AwaitLimit = application:get_env(?APP_Name,
        changes_bridge_name_await_seconds, 300),

    case global:whereis_name(?MODULE) of
        undefined ->
            ?info("No leader detected");
        Pid ->
            ?info("Leader ~p detected", [Pid]),
            erlang:monitor(process, Pid),
            receive
                {'DOWN', _MonitorRef, _Type, _Object, _Info} ->
                    ?info("Detected leader failure")
            after
                timer:seconds(AwaitLimit) ->
                    ?info("Scheduled takeover")
            end
    end.