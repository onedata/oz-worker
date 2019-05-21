%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This document contains entity graph state singleton, carrying information
%%% about dirty entities. It is used during recalculation of entity graph.
%%% @end
%%%-------------------------------------------------------------------
-module(entity_graph_state).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([initialize/0, get/0, update/1]).

%% datastore_model callbacks
-export([init/0]).

-type state() :: #entity_graph_state{}.
-type diff() :: datastore_doc:diff(state()).
-export_type([state/0, diff/0]).

-type dirty_queue() :: ordsets:ordset({Priority :: integer(), entity_logic:entity_type(), entity_logic:entity_id()}).
-export_type([dirty_queue/0]).

-define(STATE_KEY, <<"entity_graph_state">>).

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new entity graph state singleton if it does not exist.
%% @end
%%--------------------------------------------------------------------
-spec initialize() -> ok.
initialize() ->
    {ok, _} = datastore_model:update(?CTX, ?STATE_KEY,
        fun(State) -> {ok, State} end,
        #document{key = ?STATE_KEY, value = #entity_graph_state{}}
    ),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Returns the entity graph state singleton.
%% @end
%%--------------------------------------------------------------------
-spec get() -> state().
get() ->
    case datastore_model:get(?CTX, ?STATE_KEY) of
        {ok, #document{value = State}} ->
            State;
        Error ->
            ?error("Cannot retrieve state of effective graph: ~p", [Error]),
            error(cannot_get_state)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Updates entity graph state singleton.
%% @end
%%--------------------------------------------------------------------
-spec update(diff()) -> ok | {error, term()}.
update(Diff) ->
    {ok, _} = datastore_model:update(?CTX, ?STATE_KEY, Diff),
    ok.


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