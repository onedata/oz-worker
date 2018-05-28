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
-export([create/1, get/1, update/2, delete/1, list/0]).
-export([session_ttl/0]).

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
    disc_driver => undefined,
    fold_enabled => true
}).

-define(SESSION_TTL, oz_worker:get_env(session_ttl, 3600)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new session for given user.
%% @end
%%--------------------------------------------------------------------
-spec create(od_user:id()) -> {ok, id()} | {error, term()}.
create(UserId) ->
    Doc = #document{value = #session{
        user_id = UserId, accessed = time_utils:cluster_time_millis()
    }},
    case datastore_model:create(?CTX, Doc) of
        {ok, #document{key = SessionId}} -> {ok, SessionId};
        {error, Reason} -> {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns session by ID and updates access time.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(SessionId) ->
    case datastore_model:get(?CTX, SessionId) of
        {ok, _Doc} ->
            update(SessionId, fun(Sess) -> {ok, Sess} end);
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Deletes session by ID.
%% @end
%%--------------------------------------------------------------------
-spec delete(id()) -> ok | {error, term()}.
delete(SessId) ->
    datastore_model:delete(?CTX, SessId).

%%--------------------------------------------------------------------
%% @doc
%% Updates session by ID. Updates access time as well.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(SessId, Diff) ->
    datastore_model:update(?CTX, SessId, fun(Sess = #session{}) ->
        case Diff(Sess) of
            {ok, Sess2} -> {ok, Sess2#session{accessed = time_utils:cluster_time_millis()}};
            {error, Reason} -> {error, Reason}
        end
    end).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all sessions.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

%%--------------------------------------------------------------------
%% @doc
%% Returns session Time To Live in seconds.
%% @end
%%--------------------------------------------------------------------
-spec session_ttl() -> integer().
session_ttl() ->
    ?SESSION_TTL.

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
