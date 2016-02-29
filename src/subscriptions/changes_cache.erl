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
-module(changes_cache).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(KEY, cache).
-define(WORKER_NAME, subscriptions_worker).

-export([put/3, slice/2, newest_seq/0, oldest_seq/0]).

put(Seq, Doc, Type) ->
    {ok, _} = subscriptions_state:update(?SUBSCRIPTIONS_STATE_KEY, fun(State) ->
        Cache = State#subscriptions_state.cache,
        UpdatedCache = gb_trees:enter(Seq, {Doc, Type}, Cache),
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

slice(From, To) ->
    Cache = get_cache(),
    CachedList = gb_trees:values(Cache),
    Shifted = lists:dropwhile(fun({Seq, _}) -> Seq < From end, CachedList),
    lists:takewhile(fun({Seq, _}) -> Seq =< To end, Shifted).

size_limit() ->
    application:get_env(?APP_Name, subscription_cache_size, 100).

newest_seq() ->
    Cache = get_cache(),
    case gb_trees:is_empty(Cache) of
        true -> cache_empty;
        false -> {Seq, _} = gb_trees:largest(Cache), Seq
    end.

oldest_seq() ->
    Cache = get_cache(),
    case gb_trees:is_empty(Cache) of
        true -> cache_empty;
        false -> {Seq, _} = gb_trees:smallest(Cache), Seq
    end.

get_cache() ->
    {ok, Doc} = subscriptions_state:get(?SUBSCRIPTIONS_STATE_KEY),
    #document{value = #subscriptions_state{cache = Cache}} = Doc,
    Cache.