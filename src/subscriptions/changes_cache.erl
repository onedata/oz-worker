%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides simple cache for document updates.
%%% @end
%%%-------------------------------------------------------------------
-module(changes_cache).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("subscriptions/subscriptions.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([put/3, newest_seq/0, oldest_seq/0, ensure_initialised/0, query/1]).

%%--------------------------------------------------------------------
%% @doc
%% Ensures state is operable after this function returns.
%% @end
%%--------------------------------------------------------------------

-spec ensure_initialised() -> no_return().
ensure_initialised() ->
    case (subscriptions_state:create(#document{
        key = ?SUBSCRIPTIONS_STATE_KEY,
        value = #subscriptions_state{cache = gb_trees:empty()}
    })) of
        {ok, ?SUBSCRIPTIONS_STATE_KEY} -> ok;
        {error, already_exists} -> ?info("State already exists")
    end.

%%--------------------------------------------------------------------
%% @doc
%% Ensures element is in cache if it should be included.
%% Cache size limit is guaranteed.
%% @end
%%--------------------------------------------------------------------

-spec put(Seq :: seq(), Doc :: #document{}, Model :: atom()) -> no_return().
put(Seq, Doc, Model) ->
    {ok, _} = subscriptions_state:update(?SUBSCRIPTIONS_STATE_KEY, fun(State) ->
        Cache = State#subscriptions_state.cache,
        UpdatedCache = gb_trees:enter(Seq, {Doc, Model}, Cache),
        Size = gb_trees:size(Cache),
        Limit = size_limit(),
        case Size > Limit of
            true ->
                {_, _, FinalCache} = gb_trees:take_smallest(UpdatedCache),
                {ok, State#subscriptions_state{cache = FinalCache}};
            false ->
                ?warning("Cache not saturated - size ~p of ~p", [Size, Limit]),
                {ok, State#subscriptions_state{cache = UpdatedCache}}
        end
    end).

%%--------------------------------------------------------------------
%% @doc
%% Returns ordered changes using given range.
%% @end
%%--------------------------------------------------------------------

-spec query(Seqs :: ordsets:ordset(seq())) -> {Hits, Misses} when
    Hits :: [{Seq :: seq(), {Doc :: datastore:document(), Model :: atom()}}],
    Misses :: [seq()].

query(Seqs) ->
    Cache = get_cache(),
    Lookup = lists:map(fun(Seq) ->
        case gb_trees:lookup(Seq, Cache) of
            none -> Seq;
            {value, Val} -> {Seq, Val}
        end
    end, Seqs),
    lists:partition(fun({_, _}) -> true; (_) -> false end, Lookup).

%%--------------------------------------------------------------------
%% @doc
%% Returns ordered changes using given range.
%% @end
%%--------------------------------------------------------------------

size_limit() ->
    {ok, Limit} = application:get_env(?APP_Name, subscription_cache_size),
    Limit.


%%--------------------------------------------------------------------
%% @doc
%% Returns largest sequence number in cache.
%% @end
%%--------------------------------------------------------------------
-spec newest_seq() -> {ok, seq()} | {error, cache_empty}.
newest_seq() ->
    Cache = get_cache(),
    case gb_trees:is_empty(Cache) of
        true -> {error, cache_empty};
        false ->
            {Seq, _} = gb_trees:largest(Cache),
            {ok, Seq}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns smallest sequence number in cache.
%% @end
%%--------------------------------------------------------------------
-spec oldest_seq() -> {ok, seq()} | {error, cache_empty}.
oldest_seq() ->
    Cache = get_cache(),
    case gb_trees:is_empty(Cache) of
        true ->
            {error, cache_empty};
        false ->
            {Seq, _} = gb_trees:smallest(Cache),
            {ok, Seq}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches cache from datastore (mnesia).
%% @end
%%--------------------------------------------------------------------
-spec get_cache() -> gb_trees:tree().
get_cache() ->
    {ok, Doc} = subscriptions_state:get(?SUBSCRIPTIONS_STATE_KEY),
    #document{value = #subscriptions_state{cache = Cache}} = Doc,
    Cache.