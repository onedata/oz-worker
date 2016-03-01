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
-module(subscribers).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(KEY, provider_callbacks).
-define(WORKER_NAME, subscriptions_worker).

-export([subscriptions_map/0, add/3]).


subscriptions_map() ->
    maps:from_list(lists:map(fun(#document{key = P, value = V}) -> {P, V}
    end, get_subscriptions())).

add(ProviderID, Endpoint, LastSeenSeq) ->
    TTL = application:get_env(?APP_Name, subscription_ttl_seconds, 120),
    ExpiresAt = now_seconds() + TTL,
    {ok, ProviderID} = provider_subscription:save(#document{
        key = ProviderID,
        value = #provider_subscription{
            node = node(),
            endpoint = Endpoint,
            seq = LastSeenSeq,
            clients = #{},
            expires = ExpiresAt
        }
    }).

now_seconds() ->
    erlang:system_time(seconds).

get_subscriptions() ->
    provider_subscription:non_expired().