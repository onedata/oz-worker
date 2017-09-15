%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for subscriptions_state record - used to store the state for
%%% subscriptions mechanism.
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions_state).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/1, get/1, update/2]).

%% datastore_model callbacks
-export([init/0]).

-type id() :: binary().
-type record() :: #subscriptions_state{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates subscription.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns subscription by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(SubscriptionId) ->
    datastore_model:get(?CTX, SubscriptionId).

%%--------------------------------------------------------------------
%% @doc
%% Updates subscription by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff())-> {ok, doc()} | {error, term()}.
update(SubscriptionId, Diff) ->
    datastore_model:update(?CTX, SubscriptionId, Diff).

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes model.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | {error, term()}.
init() ->
    datastore_model:init(?CTX).
