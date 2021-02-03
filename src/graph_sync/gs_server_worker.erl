%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is responsible for receiving changes from changes stream and
%%% processing them. One instance is started globally and monitored by
%%% gs_worker using periodic healthchecks.
%%% @end
%%%-------------------------------------------------------------------
-module(gs_server_worker).
-author("Lukasz Opiola").

-behaviour(gen_server).

-include("graph_sync/oz_graph_sync.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("cluster_worker/include/modules/datastore/datastore.hrl").

-define(DEFAULT_BUCKET, <<"onedata">>).
-define(DEFAULT_SCOPE, <<"">>).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts changes stream for Graph Sync records and registers it globally.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server2:start_link({global, ?GS_SERVER_WORKER_GLOBAL_NAME}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the worker.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    Stream = self(),
    Since = gs_server_state:get_seq(),
    Callback = fun(Change) -> gen_server2:cast(Stream, {change, Change}) end,
    couchbase_changes:enable([?DEFAULT_BUCKET]),
    couchbase_changes_worker:start_link(?DEFAULT_BUCKET, ?DEFAULT_SCOPE, undefined, undefined),
    {ok, _} = couchbase_changes_stream:start_link(
        ?DEFAULT_BUCKET, ?DEFAULT_SCOPE, Callback,
        [{since, Since}, {until, infinity}], []
    ),
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @doc
%% Handles call messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {reply, Reply :: term(), NewState :: state(), timeout() | hibernate} |
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
    {stop, Reason :: term(), NewState :: state()}.
handle_call(Request, _From, #state{} = State) ->
    ?log_bad_request(Request),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}.
handle_cast({change, {ok, Docs}}, State) when is_list(Docs) ->
    [handle_change(Doc) || Doc <- Docs],
    {noreply, State};
handle_cast({change, {ok, #document{} = Doc}}, State) ->
    handle_change(Doc),
    {noreply, State};
handle_cast({change, {ok, end_of_stream}}, State) ->
    {stop, normal, State};
handle_cast({change, {error, _Seq, Reason}}, State) ->
    {stop, Reason, State};
handle_cast(Request, #state{} = State) ->
    ?log_bad_request(Request),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles all non call/cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}.
handle_info(Info, #state{} = State) ->
    ?log_bad_request(Info),
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
    State :: state()) -> term().
terminate(Reason, #state{} = State) ->
    ?log_terminate(Reason, State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts process state when code is changed.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: state(),
    Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec handle_change(datastore:doc()) -> ok.
handle_change(Doc = #document{key = Key, seq = Seq, value = Value}) ->
    case change_to_entity_type_and_id(Key, Value) of
        ignore ->
            ok;
        {Type, EntityId} ->
            case Doc of
                #document{deleted = true} ->
                    gs_server:deleted(Type, EntityId);
                #document{value = Entity, revs = [DbRev | _]} ->
                    {Revision, _Hash} = datastore_rev:parse(DbRev),
                    gs_server:updated(Type, EntityId, {Entity, Revision})
            end
    end,
    gs_server_state:set_seq(Seq).


%% @private
-spec change_to_entity_type_and_id(datastore_key:key(), tuple()) ->
    ignore | {gri:entity_type(), gri:entity_id()}.
change_to_entity_type_and_id(Key, #od_user{}) ->
    {od_user, Key};
change_to_entity_type_and_id(Key, #od_group{}) ->
    {od_group, Key};
change_to_entity_type_and_id(Key, #od_space{}) ->
    {od_space, Key};
change_to_entity_type_and_id(Key, #od_share{}) ->
    {od_share, Key};
change_to_entity_type_and_id(Key, #od_provider{}) ->
    {od_provider, Key};
change_to_entity_type_and_id(Key, #od_storage{}) ->
    {od_storage, Key};
change_to_entity_type_and_id(Key, #od_cluster{}) ->
    {od_cluster, Key};
change_to_entity_type_and_id(Key, #od_handle_service{}) ->
    {od_handle_service, Key};
change_to_entity_type_and_id(Key, #od_handle{}) ->
    {od_handle, Key};
change_to_entity_type_and_id(Key, #od_harvester{}) ->
    {od_harvester, Key};
change_to_entity_type_and_id(Key, #od_token{}) ->
    {od_token, Key};
change_to_entity_type_and_id(Key, #space_stats{}) ->
    {space_stats, Key};
change_to_entity_type_and_id(Key, #temporary_token_secret{}) ->
    ?SUB(_, SubjectId) = temporary_token_secret:key_to_subject(Key),
    {temporary_token_secret, SubjectId};
change_to_entity_type_and_id(_, _) ->
    ignore.
