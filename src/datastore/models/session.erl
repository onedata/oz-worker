%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for session record - used to store session details.
%%% @end
%%%-------------------------------------------------------------------
-module(session).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/1, get/1, update/2, delete/1]).

%% datastore_model callbacks
-export([init/0]).

-type id() :: binary().
-type record() :: #session{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-type memory() :: maps:map().
-export_type([memory/0]).

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates session and set access time.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc = #document{value = Sess = #session{}}) ->
    datastore_model:create(?CTX, Doc#document{
        value = Sess#session{accessed = os:timestamp()}
    }).

%%--------------------------------------------------------------------
%% @doc
%% Returns session by ID and updates access time.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(SessId) ->
    case datastore_model:get(?CTX, SessId) of
        {ok, Doc} ->
            update(SessId, fun(Sess) -> {ok, Sess} end),
            {ok, Doc};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Updates session by ID. Updates access time as well.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(SessId, Diff) ->
    datastore_model:update(?CTX, SessId, fun(Sess = #session{}) ->
        case Diff(Sess) of
            {ok, Sess2} -> {ok, Sess2#session{accessed = os:timestamp()}};
            {error, Reason} -> {error, Reason}
        end
    end).

%%--------------------------------------------------------------------
%% @doc
%% Deletes session by ID.
%% @end
%%--------------------------------------------------------------------
-spec delete(id()) -> ok | {error, term()}.
delete(SessId) ->
    datastore_model:delete(?CTX, SessId).

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