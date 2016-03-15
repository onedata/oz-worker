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
-export([push_messages/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Pushes messages to the provider via (wss) connection.
%% If there is no connection messages are discarded.
%% @end
%%--------------------------------------------------------------------
-spec push_messages(ProviderID :: binary(), Messages :: [term()])
        -> any().
push_messages(ProviderID, Messages) ->
    {ok, #document{value = #provider_subscription{connections = Conns}}}
        = subscriptions:get_doc(ProviderID),
    case Conns of
        [Conn | _] ->
            UniqueMessages = lists:usort(Messages),
            Encoded = json_utils:encode({array, UniqueMessages}),
            Conn ! {push, Encoded};
        [] -> ?info("No connection ~p ~p", [ProviderID, Messages])
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
    case couchdb_datastore_driver:db_run(couchbeam_changes, follow_once, [], 30) of
        {ok, _, _} -> ok;
        _ -> {error, couchbeam_not_reachable}
    end;

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
    lists:foreach(fun({Seq, Doc, Model}) ->
        handle_change(Seq, Doc, Model)
    end, Updates),
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
-spec fetch_history(ResumeAt :: seq(), Missing :: [seq()]) -> any().
fetch_history(ResumeAt, Missing) ->

    case changes_cache:newest_seq() of
        {ok, Newest} ->
            case get_seq_to_fetch(Newest, ResumeAt, Missing) of
                [] -> ok;
                ToFetch ->
                    Misses = fetch_from_cache(ToFetch),
                    fetch_from_db(Misses)
            end;
        _ ->
            fetch_from_db(Missing)
    end.

%%--------------------------------------------------------------------
%% @doc @private
%% Transforms provider declaration to historical sequence numbers.
%% @end
%%--------------------------------------------------------------------
-spec get_seq_to_fetch(Newest :: seq(), ResumeAt :: seq(), Missing :: [seq()])
        -> ToFetch :: [seq()].
get_seq_to_fetch(Newest, ResumeAt, Missing) ->
    case Newest < ResumeAt of
        true -> Missing;
        false -> Missing ++ lists:seq(ResumeAt, Newest)
    end.

%%--------------------------------------------------------------------
%% @doc @private
%% Fetches old changes from cache and sends them to the providers.
%% @end
%%--------------------------------------------------------------------

-spec fetch_from_cache(Seqs :: ordsets:ordset(seq())) -> any().
fetch_from_cache(Seqs) ->
    {Hits, Misses} = changes_cache:query(Seqs),
    lists:foreach(fun({Seq, {Doc, Type}}) ->
        handle_change(Seq, Doc, Type)
    end, Hits),
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

-spec fetch_from_db(Seqs :: ordsets:ordset(seq())) -> any().
fetch_from_db([]) -> ok;
fetch_from_db(Seqs) ->
    From = hd(Seqs) - 1,
    To = lists:last(Seqs),

    spawn(fun() ->
        {ok, Timeout} = application:get_env(?APP_Name,
            history_changes_stream_life_limit_seconds),
        process_flag(trap_exit, true),

        {ok, Pid} = couchdb_datastore_driver:changes_start_link(fun
            (_Seq, stream_ended, _Type) -> ok;
            (Seq, Doc, Type) ->
                handle_change(Seq, Doc, Type)
        end, From, To),

        receive
            {'EXIT', Pid, _Reason} ->
                ignore_all([hd(Seqs)])
        after
            timer:seconds(Timeout) ->
                ?warning("Fetch from DB taking too long - kill"),
                exit(Pid, fetch_taking_too_long),
                exit(fetch_taking_too_long)
        end
    end).


%%--------------------------------------------------------------------
%% @doc @private
%% Sends to all providers information to skip the sequence numbers.
%% @end
%%--------------------------------------------------------------------
-spec ignore_all(Seqs :: ordsets:ordset(seq())) -> any().
ignore_all(Seqs) ->
    ?warning("Ignoring ~p", [Seqs]),
    Subscriptions = subscriptions:all(),
    lists:foreach(fun(#document{value = Subscription}) ->
        lists:foreach(fun(Seq) ->
            IgnoreMessage = translator:get_ignore_msg(Seq),
            outbox:put(Subscription#provider_subscription.provider,
                fun push_messages/2, IgnoreMessage)
        end, Seqs)
    end, Subscriptions).

%%--------------------------------------------------------------------
%% @doc @private
%% Sends to all providers information about the update. If provider shouldn't
%% be informed about given change, only 'ignore' message is sent to inform the
%% provider, that he shouldn't expect update with given sequence number.
%% @end
%%--------------------------------------------------------------------

-spec handle_change(Seq :: seq(), Doc :: datastore:document(), Model :: atom())
        -> any().

handle_change(Seq, Doc, Model) ->
    spawn_link(fun() ->
        Message = translator:get_msg(Seq, Doc, Model),
        IgnoreMessage = translator:get_ignore_msg(Seq),

        Providers = eligible:providers(Doc, Model),
        ProvidersSet = sets:from_list(Providers),
        Subscriptions = subscriptions:all(),

        lists:foreach(fun(#document{value = Subscription}) ->
            case subscriptions:seen(Subscription, Seq) of
                true -> ok;
                false ->
                    ProviderID = Subscription#provider_subscription.provider,
                    MessageToSend = case sets:is_element(ProviderID, ProvidersSet) of
                        true -> Message;
                        false -> IgnoreMessage
                    end,
                    outbox:put(ProviderID, fun push_messages/2, MessageToSend)
            end
        end, Subscriptions)
    end).