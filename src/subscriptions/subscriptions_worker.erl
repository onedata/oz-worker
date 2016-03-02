%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions_worker).
-author("Michal Zmuda").

-behaviour(worker_plugin_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/1, handle/1, cleanup/0]).

-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.
init(_Args) ->
    process_flag(trap_exit, true),
    changes_cache:initialize(),
    {ok, #{}}.


handle(healthcheck) ->
    case couchdb_datastore_driver:db_run(couchbeam_changes, follow_once, [], 30) of
        {ok, _, _} -> ok;
        _ -> {error, couchbeam_not_reachable}
    end;

handle({send_update, ProviderSubscriptions, Message}) ->
    ?info("Sending ~p", [[ProviderSubscriptions, Message]]),
    lists:foreach(fun(Subscription) ->
        Provider = Subscription#provider_subscription.provider,
        Endpoint = Subscription#provider_subscription.endpoint,
        outbox:put(Provider, Endpoint, Message)
    end, ProviderSubscriptions);

handle({handle_change, Seq, Doc, Type}) ->
    changes_cache:put(Seq, Doc, Type),
    handle_change(Seq, Doc, Type, fun(_) -> true end);

handle({subscribe_provider, ProviderID, Endpoint, LastSeenSeq}) ->
    subscriptions:put(ProviderID, Endpoint, LastSeenSeq),
    fetch_history(ProviderID, Endpoint, LastSeenSeq);

handle({subscribe_client, ClientID, ProviderID, TTL}) ->
    subscriptions:put_user(ClientID, ProviderID, TTL);

handle(_Request) ->
    ?log_bad_request(_Request).

-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

fetch_history(ProviderID, Endpoint, LastSeenSeq) ->
    NewestCached = changes_cache:newest_seq(),
    OldestCached = changes_cache:oldest_seq(),
    ?info("Fetching from cache; {provider:~p, from:~p, cache_start:~p, cache_end:~p}",
        [ProviderID, LastSeenSeq, OldestCached, NewestCached]),

    Filter = fun(P) -> P =:= ProviderID end,
    case {OldestCached, LastSeenSeq >= OldestCached} of
        {cache_empty, _} ->
            fetch_from_db(LastSeenSeq, last_seq(), Filter);
        {_, true} ->
            fetch_from_cache(LastSeenSeq, NewestCached, Filter);
        {_, false} ->
            fetch_from_cache(LastSeenSeq, NewestCached, Filter),
            fetch_from_db(LastSeenSeq, OldestCached - 1, Filter)
    end.


fetch_from_cache(From, To, Filter) ->
    lists:foreach(fun({Seq, {Doc, Type}}) ->
        handle_change(Seq, Doc, Type, Filter)
    end, changes_cache:slice(From, To)).

fetch_from_db(From, To, Filter) ->
    couchdb_datastore_driver:changes_start_link(fun
        (_Seq, stream_ended, _Type) -> ok;
        (Seq, Doc, Type) ->
            handle_change(Seq, Doc, Type, Filter)
    end, From, To).

handle_change(Seq, Doc, Type, Filter) ->
    Message = translator:get_msg(Seq, Doc, Type),
    Entitled = get_entitled_subscriptions(Doc, Type, Filter),

    NodeToSubscriptions = lists:foldr(fun(Subscription, Acc) ->
        Node = Subscription#provider_subscription.node,
        dict:append(Node, Subscription, Acc)
    end, dict:new(), Entitled),

    lists:foreach(fun({Node, Subscriptions}) ->
        worker_proxy:cast({?MODULE, Node}, {send_update, Subscriptions, Message})
    end, dict:to_list(NodeToSubscriptions)).

get_entitled_subscriptions(Doc, Type, Filter) ->
    Providers = allowed:providers(Doc, Type, Filter),
    Now = erlang:system_time(seconds),

    Subscriptions = lists:map(fun(Provider) ->
        case subscriptions:find(Provider) of
            {ok, #document{value = Val = #provider_subscription{expires = E}}}
                when E > Now -> Val;
            _ -> no_active_subscription
        end
    end, Providers),

    lists:filter(fun(X) -> X =/= no_active_subscription end, Subscriptions).


-spec last_seq() -> non_neg_integer().
last_seq() ->
    try
        {ok, LastSeq, _} = couchdb_datastore_driver:db_run(couchbeam_changes, follow_once, [], 30),
        binary_to_integer(LastSeq)
    catch
        E:R ->
            ?error("Last sequence number unknown (assuming 1) due to ~p:~p", [E, R]),
            1
    end.