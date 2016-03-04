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
    timer :: timer:tref(),
    buffer :: [term()]
}).

%%--------------------------------------------------------------------
%% @doc
%% Sends current batch of provider to provided endpoint and initialize
%% next batch. Should not be called explicitly as outbox internally
%% schedules pushes.
%% @end
%%--------------------------------------------------------------------
-spec push(ProviderID :: term(), Endpoint :: binary()) -> no_return().
push(ProviderID, Endpoint) ->
    worker_host:state_update(?SUBSCRIPTIONS_WORKER_NAME, {msg_buffer, ProviderID}, fun
        (Outbox = #outbox{buffer = Buffer}) ->
            Messages = json_utils:encode({array, Buffer}),
            Headers = [{<<"content-type">>, <<"application/json">>}],
            %% todo provider cert should be verified 9remove insecure)
            Options = [{async, once}, insecure],
            http_client:post(Endpoint, Headers, Messages, Options),
            Outbox#outbox{buffer = [], timer = undefined}
    end).

%%--------------------------------------------------------------------
%% @doc
%% Adds message which should be send to given provider via given endpoint.
%% Buffers the message ans schedules the push.
%% @end
%%--------------------------------------------------------------------
-spec put(ProviderID :: term(), Endpoint :: binary(), Message :: term()) -> no_return().
put(ProviderID, Endpoint, Message) ->
    ?info("Putting ~p", [[ProviderID, Endpoint, Message]]), %todo
    worker_host:state_update(?SUBSCRIPTIONS_WORKER_NAME, {msg_buffer, ProviderID}, fun
        (undefined) ->
            TRef = setup_timer(ProviderID, Endpoint),
            #outbox{timer = TRef, buffer = [Message]};
        (Outbox = #outbox{buffer = Buffer, timer = undefined}) ->
            TRef = setup_timer(ProviderID, Endpoint),
            Outbox#outbox{buffer = [Message | Buffer], timer = TRef};
        (Outbox = #outbox{buffer = Buffer}) ->
            Outbox#outbox{buffer = [Message | Buffer]};
        (X) -> ?info("XXX ~p", [X]) %todo
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
setup_timer(Provider, Endpoint) ->
    TTL = application:get_env(?APP_Name, subscriptions_buffer_millis, 1000),
    {ok, TRef} = timer:apply_after(TTL, ?MODULE, push, [Provider, Endpoint]),
    TRef.