%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Manages subscriptions of providers and users.
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

%%--------------------------------------------------------------------
%% @doc
%% Fetches provider subscription from datastore (mnesia).
%% @end
%%--------------------------------------------------------------------
-spec find(ID :: term()) -> {ok, datastore:document()} | {error, Reason :: term()}.
find(ID) ->
    provider_subscription:get(ID).

%%--------------------------------------------------------------------
%% @doc
%% Adds provider subscription.
%% @end
%%--------------------------------------------------------------------
-spec put(ProviderID :: term(), Endpoint :: binary(), LastSeq :: pos_integer())
        -> no_return().
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


%%--------------------------------------------------------------------
%% @doc
%% Adds user subscription.
%% @end
%%--------------------------------------------------------------------
-spec put_user(UserID :: term(), ProviderID :: term(), TTL :: pos_integer())
        -> no_return().
put_user(UserID, ProviderID, TTL) ->
    ExpiresAt = now_seconds() + TTL,
    user_subscription:create_or_update(
        #document{key = UserID, value = #user_subscription{
            provider = ProviderID,
            expires = ExpiresAt,
            user = UserID
        }}, fun(Record) -> {ok, Record#user_subscription{
            provider = ProviderID,
            expires = ExpiresAt
        }}
        end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec now_seconds() -> pos_integer().
now_seconds() ->
    erlang:system_time(seconds).