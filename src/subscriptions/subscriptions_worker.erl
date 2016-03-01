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

-export([init/1, handle/1, cleanup/0, push_updates/2]).

-record(outbox, {
    timer :: timer:tref(),
    buffer :: [term()]
}).


push_updates(Provider, Endpoint) ->
    worker_host:state_update(?MODULE, {msg_buffer, Provider}, fun
        (Outbox = #outbox{buffer = Buffer}) ->
            Messages = json_utils:encode({array, Buffer}),
            ?info("Pushing ~p to ~p", [Messages, Endpoint]),
            http_client:post(Endpoint, [{async, once}], Messages),
            Outbox#outbox{buffer = [], timer = undefined}
    end).

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
    Subscriptions = subscriptions:as_map(),
    handle_change(Seq, Doc, Type, Subscriptions);

handle({provider_subscribe, ProviderID, Endpoint, LastSeenSeq}) ->
    ?info("Request ~p", [{provider_subscribe, ProviderID, Endpoint, LastSeenSeq}]),
    subscriptions:put(ProviderID, Endpoint, LastSeenSeq),
    fetch_history(ProviderID, Endpoint, LastSeenSeq);

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

    Subscriptions = #{ProviderID => #provider_subscription{
        provider = ProviderID, endpoint = Endpoint, node = node(),
        expires = infinity, clients = #{}
    }},
    case {OldestCached, LastSeenSeq >= OldestCached} of
        {cache_empty, _} ->
            fetch_from_db(LastSeenSeq, last_seq(), Subscriptions);
        {_, true} ->
            fetch_from_cache(LastSeenSeq, NewestCached, Subscriptions);
        {_, false} ->
            fetch_from_cache(LastSeenSeq, NewestCached, Subscriptions),
            fetch_from_db(LastSeenSeq, OldestCached - 1, Subscriptions)
    end.


fetch_from_cache(From, To, Subscriptions) ->
    lists:foreach(fun({Seq, {Doc, Type}}) ->
        handle_change(Seq, Doc, Type, Subscriptions)
    end, changes_cache:slice(From, To)).

fetch_from_db(From, To, Subscriptions) ->
    couchdb_datastore_driver:changes_start_link(fun
        (_Seq, stream_ended, _Type) -> ok;
        (Seq, Doc, Type) ->
            handle_change(Seq, Doc, Type, Subscriptions)
    end, From, To).

handle_change(Seq, Doc, Type, Subscriptions) ->
    NoOp = fun(_Seq, _Doc, _Type) -> ok end,
    Providers = allowed:providers(Seq, Doc, Type),
    ?info("Dispatching {seq=~p, doc=~p, type=~p} to ~p having ~p", [Seq, Doc, Type, Providers, Subscriptions]),
    Message = translator:get_msg(Seq, Doc, Type),

    NodeToSubscriptions = lists:foldr(fun(Provider, Acc) ->
        Subscription = maps:get(Provider, Subscriptions, {NoOp, infinity}),
        Node = Subscription#provider_subscription.node,
        dict:append(Node, Subscription, Acc)
    end, dict:new(), Providers),
    ?info("NodeToSubscriptions  ~p \n ~p", [dict:to_list(NodeToSubscriptions), Subscriptions]),

    lists:foreach(fun({Node, Subscriptions}) ->
        ?info("Dispatching ~p to ~p having ~p", [Message, Node, Subscriptions]),
        worker_proxy:cast({?MODULE, Node}, {send_update, Subscriptions, Message})
    end, dict:to_list(NodeToSubscriptions)).

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