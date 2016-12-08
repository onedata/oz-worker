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
-module(outboxes).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("subscriptions/subscriptions.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([push/2, put/3]).

%%--------------------------------------------------------------------
%% @doc
%% Pushes current batch.
%% This function is used internally but can be used to force immediate batch push.
%% @end
%%--------------------------------------------------------------------
-spec push(ID, PushFun) -> ok when
    ID :: term(),
    PushFun :: fun((ID1 :: term(), Buffer :: [term()]) -> ok).
push(ID, PushFun) ->
    outbox:create_or_update(#document{key = ID, value = #outbox{
        buffer = [], timer = undefined, timer_expires = undefined
    }}, fun(Outbox = #outbox{buffer = Buffer, timer = TRef}) ->
        case TRef of undefined -> ok; _ -> timer:cancel(TRef) end,
        PushFun(ID, Buffer),
        {ok, Outbox#outbox{buffer = [], timer = undefined}}
    end), ok.

%%--------------------------------------------------------------------
%% @doc
%% Buffers the messages and schedules the push (if needed).
%% @end
%%--------------------------------------------------------------------
-spec put(ID, PushFun, Messages) -> ok when
    ID :: term(),
    Messages :: [term()],
    PushFun :: fun((ID1 :: term(), Buffer :: [term()]) -> ok).
put(_, _, []) -> ok;
put(ID, PushFun, Messages) ->
    Now = erlang:system_time(),
    TimerTTL = batch_ttl(),
    TimerExpires = Now + TimerTTL,

    TRef = setup_timer(ID, PushFun),
    {ok, _} = outbox:create_or_update(#document{key = ID, value = #outbox{
        buffer = Messages, timer = TRef, timer_expires = TimerExpires
    }}, fun
        (Outbox = #outbox{buffer = Buffer, timer = OldTimer, timer_expires =
        % timer expires after twice the ttl
        OldExpires}) when OldTimer =:= undefined; OldExpires < (Now - TimerTTL) ->
            {ok, Outbox#outbox{buffer = Messages ++ Buffer,
                timer = TRef, timer_expires = TimerExpires}};
        (Outbox = #outbox{buffer = Buffer}) ->
            timer:cancel(TRef),
            {ok, Outbox#outbox{buffer = Messages ++ Buffer}};
        (_Outbox) -> ?warning("Unexpected document ~p", [_Outbox])
    end), ok.

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
    PushFun :: fun((ID1 :: term(), Buffer :: [term()]) -> ok).
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
    {ok, TTL} = application:get_env(?APP_NAME, subscription_batch_ttl),
    TTL.