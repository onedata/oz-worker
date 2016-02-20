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
-module(subscriptions).
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

    CacheMaxSize = cache_size_limit(),
    ?info("Cache max size ~p", [CacheMaxSize]),

    CurrentSeq = last_seq(),
    ?info("Starting changes stream using changes from ~p", [CurrentSeq]),
    start_changes_stream(CurrentSeq),

    {ok, #{
        provider_callbacks => #{},
        cache => gb_trees:empty(),
        user_to_provider => #{}
    }}.


handle(healthcheck) ->
    case couchdb_datastore_driver:db_run(couchbeam_changes, follow_once, [], 30) of
        {ok, _, _} -> ok;
        _ -> {error, couchbeam_not_reachible}
    end;

handle({provider_subscribe, ProviderID, Callback, From}) ->
    ?info("Request ~p", [{provider_subscribe, ProviderID, Callback, From}]),
    add_subscription(ProviderID, Callback),
    fetch_history(ProviderID, Callback, From);

handle(_Request) ->
    ?log_bad_request(_Request).

-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_cache(Seq, Doc, Type) ->
    worker_host:state_update(?MODULE, cache, fun(Cache) ->
        UpdatedCache = gb_trees:insert(Seq, {Doc, Type}, Cache),
        Size = gb_trees:size(Cache),
        Limit = cache_size_limit(),
        case Size > Limit of
            true ->
                {_, _, FinalCache} = gb_trees:take_smallest(UpdatedCache),
                FinalCache;
            false ->
                ?warning("Cache not saturated - size ~p of ~p", [Size, Limit]),
                UpdatedCache
        end
    end).

add_subscription(ID, Callback) ->
    TTL = application:get_env(?APP_Name, subscription_ttl_seconds, 120),
    ExpiresAt = now_seconds() + TTL,
    ?info("Adding subscription ~p", [{ID, Callback, ExpiresAt, TTL}]),
    worker_host:state_update(?MODULE, provider_callbacks, fun(Callbacks) ->
        maps:put(ID, {Callback, ExpiresAt}, Callbacks)
    end),
    ok.

fetch_history(ProviderID, Callback, From) ->
    NewestCached = newest_cached_seq(),
    OldestCached = oldest_cached_seq(),
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
    Cache = worker_host:state_get(?MODULE, cache),
    CachedList = gb_trees:values(Cache),
    Shifted = lists:dropwhile(fun({Seq, _}) -> Seq < From end, CachedList),
    Trimmed = lists:takewhile(fun({Seq, _}) -> Seq =< To end, Shifted),
    lists:foreach(fun({Seq, {Doc, Type}}) ->
        handle_change(Seq, Doc, Type, #{ProviderID => {Callback, infinity}})
    end, Trimmed).

fetch_from_db(From, To, ProviderID, Callback) ->
    couchdb_datastore_driver:changes_start_link(fun
        (_Seq, stream_ended, _Type) -> ok;
        (Seq, Doc, Type) -> handle_change(Seq, Doc, Type, #{ProviderID => {Callback, infinity}})
    end, From, To).

-spec common_change_callback(Seq :: non_neg_integer(), datastore:document() | stream_ended, model_behaviour:model_type() | undefined) -> ok.
common_change_callback(_Seq, stream_ended, _Type) ->
    CurrentSeq = newest_cached_seq(),
    ?error("Changes stream broken - restarting at ~p", [CurrentSeq]),
    start_changes_stream(CurrentSeq),
    ok;

common_change_callback(Seq, Doc, Type) ->
    cleanup_expired_callbacks(),
    update_cache(Seq, Doc, Type),
    Callbacks = worker_host:state_get(?MODULE, provider_callbacks),
    handle_change(Seq, Doc, Type, Callbacks).

handle_change(Seq, Doc, Type, Callbacks) ->
    NoOp = fun(_Seq, _Doc, _Type) -> ok end,
    Providers = get_providers(Seq, Doc, Type),
    ?info("Dispatching {seq=~p, doc=~p, type=~p} to ~p having ~p", [Seq, Doc, Type, Providers, Callbacks]),

    lists:foreach(fun(Provider) ->
        {Callback, _} = maps:get(Provider, Callbacks, NoOp),
        spawn(fun() -> Callback(Seq, Doc, Type) end)
    end, Providers).

cleanup_expired_callbacks() ->
    worker_host:state_update(?MODULE, provider_callbacks, fun(Callbacks) ->
        Now = now_seconds(),
        maps:filter(fun(_ID, {_Callback, ExpiresAt}) ->
            Now < ExpiresAt
        end, Callbacks)
    end),
    ok.

cache_size_limit() ->
    application:get_env(?APP_Name, subscription_cache_size, 100).

now_seconds() ->
    erlang:system_time(seconds).

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

newest_cached_seq() ->
    Cache = worker_host:state_get(?MODULE, cache),
    case gb_trees:is_empty(Cache) of
        true -> cache_empty;
        false -> {Seq, _} = gb_trees:largest(Cache), Seq
    end.

oldest_cached_seq() ->
    Cache = worker_host:state_get(?MODULE, cache),
    case gb_trees:is_empty(Cache) of
        true -> cache_empty;
        false -> {Seq, _} = gb_trees:smallest(Cache), Seq
    end.


get_providers(_Seq, Doc, space) ->
    #document{value = Value} = Doc,
    #space{providers = SpaceProviders} = Value,
    SpaceProviders;

get_providers(_Seq, _Doc, user_group) ->
    [];

get_providers(_Seq, _Doc, onedata_user) ->
    [];

get_providers(_Seq, _Doc, _Type) ->
    [].

