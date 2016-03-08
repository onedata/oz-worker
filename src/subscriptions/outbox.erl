%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module accepts messages and pushes them in small batches.
%%% @end
%%%-------------------------------------------------------------------
-module(outbox).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("subscriptions/subscriptions.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([push/2, put/3]).

-record(outbox, {
    timer_expires :: pos_integer(),
    timer :: timer:tref(),
    buffer :: [term()]
}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
push(ID, PushFun) ->
    worker_host:state_update(?SUBSCRIPTIONS_WORKER_NAME, {msg_buffer, ID}, fun
        (Outbox = #outbox{buffer = Buffer}) ->
            PushFun(ID, Buffer),
            Outbox#outbox{buffer = [], timer = undefined}
    end).

%%--------------------------------------------------------------------
%% @doc
%% Buffers the message ans schedules the push.
%% @end
%%--------------------------------------------------------------------
put(ID, PushFun, Message) ->
    Now = erlang:system_time(),
    TimerTTL = batch_ttl(),
    TimerExpires = Now + TimerTTL,

    worker_host:state_update(?SUBSCRIPTIONS_WORKER_NAME, {msg_buffer, ID}, fun
        (undefined) ->
            TRef = setup_timer(ID, PushFun),
            #outbox{buffer = [Message],
                timer = TRef, timer_expires = TimerExpires};
        (Outbox = #outbox{buffer = Buffer, timer = OldTimer, timer_expires =
        % timer expires after twice the ttl
        OldExpires}) when OldTimer =:= undefined; OldExpires < (Now - TimerTTL) ->
            TRef = setup_timer(ID, PushFun),
            Outbox#outbox{buffer = [Message | Buffer],
                timer = TRef, timer_expires = TimerExpires};
        (Outbox = #outbox{buffer = Buffer}) ->
            Outbox#outbox{buffer = [Message | Buffer]}
    end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Schedules buffer push.
%% @end
%%--------------------------------------------------------------------
setup_timer(ID, PushFun) ->
    {ok, TRef} = timer:apply_after(batch_ttl(), ?MODULE, push, [ID, PushFun]),
    TRef.

batch_ttl() ->
    {ok, TTL} = application:get_env(?APP_Name, subscription_batch_ttl),
    TTL.