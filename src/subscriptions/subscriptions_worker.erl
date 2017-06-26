%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This worker resolves recipients for given updates, and sends updates.
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions_worker).
-author("Michal Zmuda").

-behaviour(worker_plugin_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% worker_plugin_behaviour callbacks
-export([init/1, handle/1, cleanup/0]).

%% API
-export([push_messages/2, fetch_from_cache_and_db/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Pushes messages to the provider via (wss) connection.
%% If there is no connection messages are discarded.
%% @end
%%--------------------------------------------------------------------
-spec push_messages(ProviderID :: binary(), Messages :: [term()]) -> ok.
push_messages(_, []) -> ok;
push_messages(ProviderID, Messages) ->
    {ok, #document{value = #provider_subscription{connections = Conns}}}
        = subscriptions:get_doc(ProviderID),

    case Conns of
        [] ->
            ?info("No connection ~p ~p", [ProviderID, Messages]);
        _ ->
            Conn = utils:random_element(Conns),
            UniqueMessages = lists:usort(Messages),
            Encoded = json_utils:encode({array, UniqueMessages}),
            try
                Conn ! {push, Encoded},
                ok
            catch
                E:R ->
                    ?info("Removing unusable connection due to ~p:~p", [E, R]),
                    subscriptions:remove_connection(ProviderID, Conn),
                    push_messages(ProviderID, UniqueMessages)
            end
    end.

%%%===================================================================
%%% worker_plugin_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialises the worker and ensures cache is available.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.
init(_Args) ->
    changes_cache:ensure_initialised(),
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @doc
%% Handles various requests connected with subscriptions.
%% @end
%%--------------------------------------------------------------------
-spec handle(Request :: term()) -> ok | {error, Reason :: term()} | no_return().
handle(healthcheck) ->
    ok;

handle({handle_change, Seq, Doc, Type}) ->
    changes_cache:put(Seq, Doc, Type),
    handle_change(Seq, Doc, Type),
    ok;

handle({add_connection, ProviderID, Connection}) ->
    subscriptions:add_connection(ProviderID, Connection),
    ok;

handle({remove_connection, ProviderID, Connection}) ->
    subscriptions:remove_connection(ProviderID, Connection),
    ok;

handle({update_missing_seq, ProviderID, ResumeAt, Missing}) ->
    subscriptions:update_missing_seq(ProviderID, ResumeAt, Missing),
    fetch_history(ResumeAt, Missing),
    ok;

handle({update_users, ProviderID, Users}) ->
    NewUsers = subscriptions:update_users(ProviderID, Users),
    Updates = user_subscriptions:updates(ProviderID, NewUsers),
    handle_change(Updates),
    ok;

handle(_Request) ->
    ?log_bad_request(_Request).

%%--------------------------------------------------------------------
%% @doc
%% Cleans up the worker.
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @private
%% Fetches old changes and sends them to the providers.
%% @end
%%--------------------------------------------------------------------
-spec fetch_history(ResumeAt :: subscriptions:seq(),
    Missing :: [subscriptions:seq()]) -> ok.
fetch_history(ResumeAt, Missing) ->
    case changes_cache:newest_seq() of
        {ok, Newest} ->
            case get_seq_to_fetch(Newest, ResumeAt, Missing) of
                [] -> ok;
                ToFetch ->
                    case fetch_from_cache(ToFetch) of
                        [] -> ok;
                        Misses ->
                            {ok, Backoff} = application:get_env(?APP_NAME,
                                wait_for_latest_changes_in_cache),
                            {ok, _} = timer:apply_after(Backoff, ?MODULE,
                                fetch_from_cache_and_db, [Misses]),
                            ok
                    end
            end;
        _ ->
            fetch_from_db(Missing)
    end.

%%--------------------------------------------------------------------
%% @doc @private
%% Transforms provider declaration to historical sequence numbers.
%% @end
%%--------------------------------------------------------------------
-spec get_seq_to_fetch(Newest :: subscriptions:seq(),
    ResumeAt :: subscriptions:seq(), Missing :: [subscriptions:seq()]) ->
    ToFetch :: [subscriptions:seq()].
get_seq_to_fetch(Newest, ResumeAt, Missing) ->
    case Newest < ResumeAt of
        true -> Missing;
        false -> Missing ++ lists:seq(ResumeAt, Newest)
    end.


%%--------------------------------------------------------------------
%% @doc @private
%% Fetches old changes from both cache and db.
%% @end
%%--------------------------------------------------------------------

-spec fetch_from_cache_and_db(Seqs :: [subscriptions:seq()]) -> ok.
fetch_from_cache_and_db([]) -> ok;
fetch_from_cache_and_db(Seqs) ->
    Misses = fetch_from_cache(Seqs),
    fetch_from_db(Misses).

%%--------------------------------------------------------------------
%% @doc @private
%% Fetches old changes from cache and sends them to the providers.
%% @end
%%--------------------------------------------------------------------

-spec fetch_from_cache(Seqs :: ordsets:ordset(subscriptions:seq())) ->
    Misses :: ordsets:ordset(subscriptions:seq()).
fetch_from_cache(Seqs) ->
    {Hits, Misses} = changes_cache:query(Seqs),
    Updates = lists:map(fun({Seq, {Doc, Type}}) ->
        {Seq, Doc, Type}
    end, Hits),
    handle_change(Updates),
    Misses.

%%--------------------------------------------------------------------
%% @doc @private
%% Fetches changes from db and sends them to the providers.
%% Only one attempt is to be made as some sequence numbers may not be present
%% in couchbase. In that scenario ignore messages are sent.
%% When fetching is taking too long it is aborted (provider will include
%% remaining missing sequences in next subscription renewals).
%% @end
%%--------------------------------------------------------------------

-spec fetch_from_db(Seqs :: ordsets:ordset(subscriptions:seq())) -> ok.
fetch_from_db([]) -> ok;
fetch_from_db(Seqs) ->
    From = hd(Seqs) - 1,
    To = lists:last(Seqs) + 1,

    spawn(fun() ->
        process_flag(trap_exit, true),

        Caller = self(),

        {ok, Pid} = couchbase_changes:stream(subscriptions:bucket(), <<"">>, fun(Feed) ->
            case Feed of
                {ok, end_of_stream} ->
                    Caller ! {self(), end_of_stream},
                    ok;
                {ok, Doc} ->
                    Caller ! {self(), Doc},
                    ok
            end
        end, [{since, From}, {until, To}]),

        {Changes, SeqsNotFetched} = accumulate_changes(Pid, Seqs, []),
        ignore_all(SeqsNotFetched),
        handle_change(Changes)
    end), ok.

accumulate_changes(Pid, SeqsToFetch, Changes) ->
    {ok, Timeout} = application:get_env(?APP_NAME,
        history_changes_stream_life_limit_seconds),
    receive
        {Pid, end_of_stream} ->
            couchbase_changes:cancel_stream(Pid),
            {Changes, SeqsToFetch};
        {Pid, Doc = #document{seq = Seq, value = Value}} ->
            Type = element(1, Value),
            accumulate_changes(Pid, SeqsToFetch -- [Seq], [{Seq, Doc, Type} | Changes]);
        {'EXIT', Pid, _Reason} ->
            ignore_all(SeqsToFetch)
    after
        timer:seconds(Timeout) ->
            ?warning("Fetch from DB taking too long - kill"),
            exit(Pid, fetch_taking_too_long),
            exit(fetch_taking_too_long)
    end.


%%--------------------------------------------------------------------
%% @doc @private
%% Sends to all providers information to skip the sequence numbers.
%% @end
%%--------------------------------------------------------------------
-spec ignore_all(Seqs :: ordsets:ordset(subscriptions:seq())) -> ok.
ignore_all([]) ->
    ok;
ignore_all(Seqs) ->
    ?debug("Ignoring sequence numbers ~p", [Seqs]),
    Subscriptions = subscriptions:all(),
    lists:foreach(fun(#document{value = #provider_subscription{provider = ID}}) ->
        outboxes:put(ID, fun push_messages/2, lists:map(fun(Seq) ->
            translator:get_ignore_msg(Seq)
        end, Seqs))
    end, Subscriptions), ok.

%%--------------------------------------------------------------------
%% @doc @private
%% Sends to all providers information about the update. If provider shouldn't
%% be informed about given change, only 'ignore' message is sent to inform the
%% provider, that he shouldn't expect update with given sequence number.
%% @end
%%--------------------------------------------------------------------
-spec handle_change(Seq :: subscriptions:seq(), Doc :: datastore:document(),
    Model :: atom()) -> ok.
handle_change(Seq, Doc, Model) ->
    handle_change([{Seq, Doc, Model}]).

-spec handle_change([{Seq :: subscriptions:seq(), Doc :: datastore:document(),
    Model :: atom()}]) -> ok.
handle_change(Updates) ->
    {Supported, NotSupported} = lists:partition(fun({_Seq, Doc, Model}) ->
        lists:member(Model, subscriptions:supported_models())
    end, Updates),
    ignore_all([S || {S, _, _} <- NotSupported]),
    handle_change_filtered(Supported).


-spec handle_change_filtered([{Seq :: subscriptions:seq(), Doc :: datastore:document(),
    Model :: atom()}]) -> ok.
handle_change_filtered(Updates) ->
    UpdatesWithProviders = utils:pmap(fun({Seq, Doc, Model}) ->
        {Seq, Doc, Model, sets:from_list(eligible:providers(Doc, Model))}
    end, Updates),

    utils:pforeach(fun(#document{value = Subscription}) ->
        try
            #provider_subscription{provider = ID} = Subscription,
            Messages = lists:filtermap(fun({Seq, Doc, _, ProvidersSet}) ->
                case subscriptions:seen(Subscription, Seq) of
                    true -> false;
                    false ->
                        ToIgnore = not sets:is_element(ID, ProvidersSet),
                        {true, translator:as_msg(Seq, Doc, ToIgnore)}
                end
            end, UpdatesWithProviders),
            outboxes:put(ID, fun push_messages/2, Messages)
        catch
            E:R ->
                ?error_stacktrace("Problem with handler ~p:~p", [E, R])
        end
    end, subscriptions:all()),
    ok.