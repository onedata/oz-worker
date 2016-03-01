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
-module(outbox).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([push/2, put/3]).

-record(outbox, {
    timer :: timer:tref(),
    buffer :: [term()]
}).

-define(WORKER_MODULE, subscriptions_worker).

push(Provider, Endpoint) ->
    worker_host:state_update(?WORKER_MODULE, {msg_buffer, Provider}, fun
        (Outbox = #outbox{buffer = Buffer}) ->
            Messages = json_utils:encode({array, Buffer}),
            http_client:post(Endpoint, [{async, once}], Messages),
            Outbox#outbox{buffer = [], timer = undefined}
    end).

put(Provider, Endpoint, Message) ->
    worker_host:state_update(?WORKER_MODULE, {msg_buffer, Provider}, fun
        (undefined) ->
            TRef = setup_timer(Provider, Endpoint),
            #outbox{timer = TRef, buffer = [Message]};
        (Outbox = #outbox{buffer = Buffer, timer = undefined}) ->
            TRef = setup_timer(Provider, Endpoint),
            Outbox#outbox{buffer = [Message | Buffer], timer = TRef};
        (Outbox = #outbox{buffer = Buffer}) ->
            Outbox#outbox{buffer = [Message | Buffer]}
    end).

setup_timer(Provider, Endpoint) ->
    TTL = application:get_env(?APP_Name, subscriptions_buffer_millis, 1000),
    {ok, TRef} = timer:apply_after(TTL, ?MODULE, push, [Provider, Endpoint]),
    TRef.