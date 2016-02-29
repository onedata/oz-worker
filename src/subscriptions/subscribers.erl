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

-export([add/2, callbacks/0, initialize/0]).

initialize() ->
    try
        {ok, ?SUBSCRIPTIONS_STATE_KEY} = subscriptions_state:create(#document{
            key = ?SUBSCRIPTIONS_STATE_KEY,
            value = #subscriptions_state{cache = gb_trees:empty()}
        })
    catch
        E:R -> ?info("State not created (may be already present) ~p:~p", [E, R])
    end.

callbacks() ->
    State = get_subscriptions(),

    maps:from_list(lists:map(fun(Doc) ->
        #document{
            key = P,
            value = #provider_subscription{callback = C, expires = E}} = Doc,
        {P, {C,E}}
    end, State)).

add(ProviderID, Callback) ->
    TTL = application:get_env(?APP_Name, subscription_ttl_seconds, 120),
    ExpiresAt = now_seconds() + TTL,
    {ok, ProviderID} = provider_subscription:save(#document{
        key = ProviderID,
        value = #provider_subscription{
            callback = Callback,
            expires = ExpiresAt
        }
    }).

now_seconds() ->
    erlang:system_time(seconds).

get_subscriptions() ->
    provider_subscription:non_expired().