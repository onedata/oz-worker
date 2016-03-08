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
    TimerTTL = application:get_env(?APP_Name, subscription_batch_ttl, 2000),
    TimerExpires = Now + TimerTTL,

    ?info("Putting ~p", [[ID, Message, Now, TimerTTL, PushFun]]),
    worker_host:state_update(?SUBSCRIPTIONS_WORKER_NAME, {msg_buffer, ID}, fun
        (undefined) ->
            TRef = setup_timer(ID, PushFun),
            #outbox{buffer = [Message],
                timer = TRef, timer_expires = TimerExpires};
        (Outbox = #outbox{buffer = Buffer, timer = OldTimer, timer_expires =
        OldExpires}) when OldTimer =:= undefined; OldExpires < Now ->
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
    TTL = application:get_env(?APP_Name, subscriptions_buffer_millis, 1000),
    {ok, TRef} = timer:apply_after(TTL, ?MODULE, push, [ID, PushFun]),
    TRef.