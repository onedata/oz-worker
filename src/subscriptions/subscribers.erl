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
-include_lib("ctool/include/logging.hrl").

-define(KEY, provider_callbacks).
-define(WORKER_NAME, subscriptions_worker).

-export([state_entry/0, cleanup/0, add/2, callbacks/0]).

state_entry() ->
    {?KEY, #{}}.

callbacks() ->
    State = worker_host:state_get(?WORKER_NAME, ?KEY),
    maps:map(fun(_, {Callback, _}) -> Callback end, State).

cleanup() ->
    worker_host:state_update(?WORKER_NAME, ?KEY, fun(Callbacks) ->
        Now = now_seconds(),
        maps:filter(fun(_ID, {_Callback, ExpiresAt}) ->
            Now < ExpiresAt
        end, Callbacks)
    end).

add(ProviderID, Callback) ->
    TTL = application:get_env(?APP_Name, subscription_ttl_seconds, 120),
    ExpiresAt = now_seconds() + TTL,
    worker_host:state_update(?WORKER_NAME, ?KEY, fun(Callbacks) ->
        maps:put(ProviderID, {Callback, ExpiresAt}, Callbacks)
    end).

now_seconds() ->
    erlang:system_time(seconds).