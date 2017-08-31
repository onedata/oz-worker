%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This document contains info needed to calculate effective graph of entities.
%%% Should not be used directly (see entity_graph module).
%%% @end
%%%-------------------------------------------------------------------
-module(entity_graph_state).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/1, get/1, update/2]).

%% datastore_model callbacks
-export([init/0]).

-type id() :: binary().
-type record() :: #entity_graph_state{}.
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
%% Creates entity graph.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns entity graph by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(GraphId) ->
    datastore_model:get(?CTX, GraphId).

%%--------------------------------------------------------------------
%% @doc
%% Updates entity graph by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff())-> {ok, doc()} | {error, term()}.
update(GraphId, Diff) ->
    datastore_model:update(?CTX, GraphId, Diff).

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