%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for outbox record - used to enclose updates sent by
%%% subscriptions mechanism.
%%% @end
%%%-------------------------------------------------------------------
-module(outbox).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([update/3]).

%% datastore_model callbacks
-export([init/0]).

-type id() :: binary().
-type record() :: #outbox{}.
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
%% Updates outbox by ID or creates default one.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff(), record()) -> {ok, doc()} | {error, term()}.
update(Id, Diff, Default) ->
    datastore_model:update(?CTX, Id, Diff, Default).

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes lock model.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | {error, term()}.
init() ->
    datastore_model:init(?CTX).