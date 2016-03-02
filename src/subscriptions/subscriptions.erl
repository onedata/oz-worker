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

-export([put/3, put_user/3, find/1]).

find(ID) ->
    provider_subscription:get(ID).

put(ProviderID, Endpoint, LastSeenSeq) ->
    TTL = application:get_env(?APP_Name, subscription_ttl_seconds, 120),
    ExpiresAt = now_seconds() + TTL,

    provider_subscription:create_or_update(
        #document{key = ProviderID, value = #provider_subscription{
            provider = ProviderID,
            endpoint = Endpoint,
            node = node(),
            expires = ExpiresAt,
            seq = LastSeenSeq
        }}, fun(Record) -> {ok, Record#provider_subscription{
            node = node(),
            expires = ExpiresAt,
            seq = LastSeenSeq
        }}
        end).

put_user(UserID, ProviderID, TTL) ->
    ExpiresAt = now_seconds() + TTL,
    user_subscription:create_or_update(
        #document{key = UserID, value = #user_subscription{
            provider = ProviderID,
            expires = ExpiresAt,
            user = UserID
        }}, fun(Record) -> {ok, Record#provider_subscription{
            provider = ProviderID,
            expires = ExpiresAt
        }}
        end).

now_seconds() ->
    erlang:system_time(seconds).

get_subscriptions() ->
    {ok, S} = provider_subscription:non_expired(), S.