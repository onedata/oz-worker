%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for provider_subscription record - used to store subscription details.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_subscription).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([get/1, update/2, update/3, list/0]).

%% datastore_model callbacks
-export([init/0]).

-type id() :: binary().
-type record() :: #provider_subscription{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined,
    fold_enabled => true
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns subscription by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(ProviderId) ->
    datastore_model:get(?CTX, ProviderId).

%%--------------------------------------------------------------------
%% @doc
%% Updates provider's subscription by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff())-> {ok, doc()} | {error, term()}.
update(ProviderId, Diff) ->
    datastore_model:update(?CTX, ProviderId, Diff).

%%--------------------------------------------------------------------
%% @doc
%% Updates provider's subscription by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff(), record())-> {ok, doc()} | {error, term()}.
update(ProviderId, Diff, Default) ->
    datastore_model:update(?CTX, ProviderId, Diff, Default).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all providers' subscriptions.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

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

