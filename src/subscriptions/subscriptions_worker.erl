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

-export([init/1, handle/1, cleanup/0, common_change_callback/3]).

-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.
init(_Args) ->
    timer:sleep(timer:seconds(2)),% todo wait for couchdb
    process_flag(trap_exit, true),

    CurrentSeq = last_seq(),
    ?info("Starting changes stream using changes from ~p", [CurrentSeq]),
    start_changes_stream(CurrentSeq),

    {CacheKey, CacheVal} = changes_cache:state_entry(),
    {SubKey, SubVal} = subscribers:state_entry(),

    {ok, #{
        user_to_provider => #{},
        CacheKey => CacheVal,
        SubKey => SubVal
    }}.


handle(healthcheck) ->
    case couchdb_datastore_driver:db_run(couchbeam_changes, follow_once, [], 30) of
        {ok, _, _} -> ok;
        _ -> {error, couchbeam_not_reachible}
    end;

handle({provider_subscribe, ProviderID, Callback, From}) ->
    ?info("Request ~p", [{provider_subscribe, ProviderID, Callback, From}]),
    subscribers:add(ProviderID, Callback),
    fetch_history(ProviderID, Callback, From);

handle(_Request) ->
    ?log_bad_request(_Request).

-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

fetch_history(ProviderID, Callback, From) ->
    NewestCached = changes_cache:newest_seq(),
    OldestCached = changes_cache:oldest_seq(),
    ?info("Fetching from cache; {provider:~p, from:~p, cache_start:~p, cache_end:~p}",
        [ProviderID, From, OldestCached, NewestCached]),

    case {OldestCached, From >= OldestCached} of
        {cache_empty, _} ->
            fetch_from_db(From, last_seq(), ProviderID, Callback);
        {_, true} ->
            fetch_from_cache(From, NewestCached, ProviderID, Callback);
        {_, false} ->
            fetch_from_cache(From, NewestCached, ProviderID, Callback),
            fetch_from_db(From, OldestCached - 1, ProviderID, Callback)
    end.


fetch_from_cache(From, To, ProviderID, Callback) ->
    lists:foreach(fun({Seq, {Doc, Type}}) ->
        handle_change(Seq, Doc, Type, #{ProviderID => {Callback, infinity}})
    end, changes_cache:slice(From, To)).

fetch_from_db(From, To, ProviderID, Callback) ->
    couchdb_datastore_driver:changes_start_link(fun
        (_Seq, stream_ended, _Type) -> ok;
        (Seq, Doc, Type) -> handle_change(Seq, Doc, Type, #{ProviderID => {Callback, infinity}})
    end, From, To).

-spec common_change_callback(Seq :: non_neg_integer(), datastore:document() | stream_ended, model_behaviour:model_type() | undefined) -> ok.
common_change_callback(_Seq, stream_ended, _Type) ->
    CurrentSeq = changes_cache:newest_seq(),
    ?error("Changes stream broken - restarting at ~p", [CurrentSeq]),
    start_changes_stream(CurrentSeq),
    ok;

common_change_callback(Seq, Doc, Type) ->
    subscribers:cleanup(),
    changes_cache:put(Seq, Doc, Type),
    Callbacks = subscribers:callbacks(),
    handle_change(Seq, Doc, Type, Callbacks).

handle_change(Seq, Doc, Type, Callbacks) ->
    NoOp = fun(_Seq, _Doc, _Type) -> ok end,
    Providers = allowed:providers(Seq, Doc, Type),
    ?info("Dispatching {seq=~p, doc=~p, type=~p} to ~p having ~p", [Seq, Doc, Type, Providers, Callbacks]),

    lists:foreach(fun(Provider) ->
        Callback = maps:get(Provider, Callbacks, NoOp),
        spawn(fun() -> Callback(Seq, Doc, Type) end)
    end, Providers).

-spec start_changes_stream(Seq :: non_neg_integer()) -> no_return().
start_changes_stream(StartSeq) ->
    couchdb_datastore_driver:changes_start_link(fun common_change_callback/3, StartSeq, infinity).

-spec last_seq() -> non_neg_integer().
last_seq() ->
    try
        {ok, LastSeq, _} = couchdb_datastore_driver:db_run(couchbeam_changes, follow_once, [], 30),
        binary_to_integer(LastSeq)
    catch
        E:R -> ?error("Last sequence number unknown (assuming 1) due to ~p:~p", [E, R]), 1
    end.