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

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(KEY, provider_callbacks).
-define(WORKER_NAME, subscriptions_worker).

-export([as_map/0, put/3, put_client/3]).


as_map() ->
    maps:from_list(lists:map(fun(#document{key = P, value = V}) -> {P, V}
    end, get_subscriptions())).

put(ProviderID, Endpoint, LastSeenSeq) ->
    TTL = application:get_env(?APP_Name, subscription_ttl_seconds, 120),
    ExpiresAt = now_seconds() + TTL,

    provider_subscription:create_or_update(
        #document{key = ProviderID, value = #provider_subscription{
            provider = ProviderID,
            endpoint = Endpoint,
            clients = #{},
            node = node(),
            expires = ExpiresAt,
            seq = LastSeenSeq
        }}, fun(Record) -> {ok, Record#provider_subscription{
            node = node(),
            expires = ExpiresAt,
            seq = LastSeenSeq
        }}
        end).

put_client(ClientID, ProviderID, TTL) ->
    ExpiresAt = now_seconds() + TTL,
    {ok, ProviderID} = provider_subscription:update(ProviderID, fun
        (undefined) ->
            ?error("Not registered provider is declared by a provider"),
            {error, provider_id_not_registered};
        (Record) ->
            Clients = Record#provider_subscription.clients,
            {ok, Record#provider_subscription{
                clients = maps:put(ClientID, ExpiresAt, Clients)
            }}
    end).

now_seconds() ->
    erlang:system_time(seconds).

get_subscriptions() ->
    provider_subscription:non_expired().