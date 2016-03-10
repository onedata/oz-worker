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
        -> no_return().
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
%%% gen_server callbacks
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
    handle_change(Seq, Doc, Type, fun(_) -> true end);

handle({add_connection, ProviderID, Connection}) ->
    subscriptions:add_connection(ProviderID, Connection);

handle({remove_connection, ProviderID, Connection}) ->
    subscriptions:remove_connection(ProviderID, Connection);

handle({update_missing_seq, ProviderID, ResumeAt, Missing}) ->
    subscriptions:update_missing_seq(ProviderID, ResumeAt, Missing),
    fetch_history(ProviderID, ResumeAt, Missing);

handle({update_users, ProviderID, Users}) ->
    NewUsers = subscriptions:update_users(ProviderID, Users),
    Updates = user_subscriptions:updates(ProviderID, NewUsers),
    Filter = fun(P) -> P =:= ProviderID end,
    lists:foreach(fun({Seq, Doc, Model}) ->
        handle_change(Seq, Doc, Model, Filter)
    end, Updates);

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
-spec fetch_history(ProviderID :: binary(), ResumeAt :: seq(), Missing :: [seq()])
        -> no_return().
fetch_history(ProviderID, ResumeAt, Missing) ->
    Filter = fun(P) -> P =:= ProviderID end,


    case {changes_cache:oldest_seq(), changes_cache:newest_seq()} of
        {{ok, Oldest}, {ok, Newest}} ->
            case get_seq_to_fetch(Newest, ResumeAt, Missing) of
                [] -> ok;
                [SmallestToFetch | _] = ToFetch ->
                    fetch_from_cache(ToFetch, Filter),
                    case Oldest >= SmallestToFetch of
                        true -> ok;
                        false -> fetch_from_db(SmallestToFetch, Oldest, Filter)
                    end
            end;
        _ ->
            [Start | _] = Missing ++ [ResumeAt],
            fetch_from_db(Start, current_seq(), Filter)
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

fetch_from_cache(Seqs, Filter) ->
    lists:foreach(fun({Seq, {Doc, Type}}) ->
        handle_change(Seq, Doc, Type, Filter)
    end, changes_cache:query(Seqs)).

%%--------------------------------------------------------------------
%% @doc @private
%% Fetches changes from db and sends them to the providers.
%% @end
%%--------------------------------------------------------------------

-spec fetch_from_db(From :: seq(), To :: seq(),
    Filter :: fun((ProviderID :: binary()) -> boolean()))
        -> no_return().
fetch_from_db(From, To, Filter) ->
    spawn(fun() ->
        couchdb_datastore_driver:changes_start_link(fun
            (_Seq, stream_ended, _Type) -> ok;
            (Seq, Doc, Type) ->
                handle_change(Seq, Doc, Type, Filter)
        end, From, To)
    end).

%%--------------------------------------------------------------------
%% @doc @private
%% Sends to all providers information about the update. If provider shouldn't
%% be informed about given change, only 'ignore' message is sent to inform the
%% provider, that he shouldn't expect update with given sequence number.
%% @end
%%--------------------------------------------------------------------

-spec handle_change(Seq :: seq(), Doc :: datastore:document(), Model :: atom(),
    Filter :: fun((ProviderID :: binary()) -> boolean()))
        -> no_return().

handle_change(Seq, Doc, Model, Filter) ->
    Message = translator:get_msg(Seq, Doc, Model),
    IgnoreMessage = translator:get_ignore_msg(Seq),

    Providers = lists:filter(Filter, eligible:providers(Doc, Model)),
    Subscriptions = subscriptions:all(),
    ProvidersSet = sets:from_list(Providers),

    lists:foreach(fun(#document{value = Subscription}) ->
        case subscriptions:seen(Subscription, Seq) of
            false -> ok;
            true ->
                ProviderID = Subscription#provider_subscription.provider,
                MessageToSend = case sets:is_element(ProviderID, ProvidersSet) of
                    true -> Message;
                    false -> IgnoreMessage
                end,
                outbox:put(ProviderID, fun push_messages/2, MessageToSend)
        end
    end, Subscriptions).

%%--------------------------------------------------------------------
%% @doc @private
%% Retrieves current sequence number from the db.
%% @end
%%--------------------------------------------------------------------

-spec current_seq() -> non_neg_integer().
current_seq() ->
    try
        {ok, LastSeq, _} = couchdb_datastore_driver:db_run(couchbeam_changes, follow_once, [], 30),
        binary_to_integer(LastSeq)
    catch
        E:R ->
            ?error("Last sequence number unknown (assuming 1) due to ~p:~p", [E, R]),
            1
    end.