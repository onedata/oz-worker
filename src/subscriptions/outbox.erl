%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Accepts messages and pushes them in small batches.
%%% This module relies on subscriptions_worker running.
%%% Each subscriptions_worker maintains separate batches.
%%% @end
%%%-------------------------------------------------------------------
-module(outbox).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("subscriptions/subscriptions.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([push/2, put/3]).

% Describes state of batch.
-record(outbox, {
    timer_expires :: pos_integer(),
    timer :: timer:tref(),
    buffer :: [term()]
}).

%%--------------------------------------------------------------------
%% @doc
%% Pushes current batch.
%% This function is used internally but can be used to force immediate batch push.
%% @end
%%--------------------------------------------------------------------
-spec push(ID, PushFun) -> any() when
    ID :: term(),
    PushFun :: fun((ID1 :: term(), Buffer :: [term()]) -> any()).

push(ID, PushFun) ->
    worker_host:state_update(?SUBSCRIPTIONS_WORKER_NAME, {msg_buffer, ID}, fun
        (undefined) -> undefined;
        (Outbox = #outbox{buffer = Buffer, timer = TRef}) ->
            case TRef of undefined -> ok; _ -> timer:cancel(TRef) end,
            PushFun(ID, Buffer),
            Outbox#outbox{buffer = [], timer = undefined}
    end).

%%--------------------------------------------------------------------
%% @doc
%% Buffers the message ans schedules the push (if needed).
%% @end
%%--------------------------------------------------------------------
-spec put(ID, PushFun, Message) -> any() when
    ID :: term(),
    Message :: term(),
    PushFun :: fun((ID1 :: term(), Buffer :: [term()]) -> any()).

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
%% Schedules buffer push. Returns timer reference responsible for the push.
%% @end
%%--------------------------------------------------------------------
-spec setup_timer(ID, PushFun) -> TRef when
    ID :: term(),
    TRef :: timer:tref(),
    PushFun :: fun((ID1 :: term(), Buffer :: [term()]) -> any()).

setup_timer(ID, PushFun) ->
    {ok, TRef} = timer:apply_after(batch_ttl(), ?MODULE, push, [ID, PushFun]),
    TRef.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns batching period (a.k.a. batch time to live)
%% @end
%%--------------------------------------------------------------------
-spec batch_ttl() -> pos_integer().

batch_ttl() ->
    {ok, TTL} = application:get_env(?APP_Name, subscription_batch_ttl),
    TTL.