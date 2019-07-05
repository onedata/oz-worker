%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for session record - used to store session details.
%%% @end
%%%-------------------------------------------------------------------
-module(session).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/global_definitions.hrl").

%% API
-export([
    create/2,
    get/1, get_user_id/1,
    exists/1,
    update/2,
    delete/1, delete/2, delete/3,
    list/0
]).

%% datastore_model callbacks
-export([init/0]).

-type id() :: binary().
-type record() :: #session{}.
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

-spec create(id(), record()) -> ok | {error, term()}.
create(SessionId, Session) ->
    case datastore_model:create(?CTX, #document{key = SessionId, value = Session}) of
        {ok, _} ->
            od_user:add_session(Session#session.user_id, SessionId),
            ok;
        {error, _} = Error ->
            Error
    end.


-spec get(id()) -> {ok, doc()} | {error, term()}.
get(Id) ->
    datastore_model:get(?CTX, Id).


-spec exists(id()) -> {ok, doc()} | {error, term()}.
exists(Id) ->
    datastore_model:exists(?CTX, Id).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user id - the owner of given session.
%% @end
%%--------------------------------------------------------------------
-spec get_user_id(id()) -> {ok, doc()} | {error, term()}.
get_user_id(Id) ->
    case ?MODULE:get(Id) of
        {ok, #document{value = #session{user_id = UserId}}} ->
            {ok, UserId};
        {error, _} = Error ->
            Error
    end.


-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(Id, Diff) ->
    datastore_model:update(?CTX, Id, Diff).


%%--------------------------------------------------------------------
%% @doc
%% Deletes session by Id and clears all expired user sessions with no grace
%% period (terminates the connections immediately).
%% @end
%%--------------------------------------------------------------------
-spec delete(id()) -> ok | {error, term()}.
delete(SessionId) ->
    delete(SessionId, undefined, true).


%%--------------------------------------------------------------------
%% @doc
%% Deletes session by Id and clears all expired user sessions with given grace
%% period (delay before connections are terminated).
%% @end
%%--------------------------------------------------------------------
-spec delete(id(), GracePeriod :: undefined | non_neg_integer()) -> ok | {error, term()}.
delete(SessionId, GracePeriod) ->
    delete(SessionId, GracePeriod, true).


%%--------------------------------------------------------------------
%% @doc
%% Deletes session by Id, allows to decide if expired user sessions should be
%% cleared.
%% @end
%%--------------------------------------------------------------------
-spec delete(id(), GracePeriod :: undefined | non_neg_integer(),
    ClearExpiredUserSessions :: boolean()) -> ok | {error, term()}.
delete(SessionId, GracePeriod, ClearExpiredUserSessions) ->
    case ?MODULE:get(SessionId) of
        {ok, #document{value = #session{user_id = UserId}}} ->
            delete_user_session(SessionId, UserId, GracePeriod, ClearExpiredUserSessions),
            datastore_model:delete(?CTX, SessionId);
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of all sessions.
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes session by Id and performs required cleanup.
%% @end
%%--------------------------------------------------------------------
-spec delete_user_session(id(), od_user:id(), GracePeriod :: undefined | non_neg_integer(),
    ClearExpiredUserSessions :: boolean()) -> ok | {error, term()}.
delete_user_session(SessionId, UserId, GracePeriod, false = _ClearExpiredUserSessions) ->
    % Terminate all user connections related to this session
    user_connections:clear(SessionId, GracePeriod),
    od_user:remove_session(UserId, SessionId);

delete_user_session(SessionId, UserId, GracePeriod, true = _ClearExpiredUserSessions) ->
    delete_user_session(SessionId, UserId, GracePeriod, false),
    clear_expired_sessions(UserId, GracePeriod).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes expired sessions of given user.
%% @end
%%--------------------------------------------------------------------
-spec clear_expired_sessions(od_user:id(), GracePeriod :: undefined | non_neg_integer()) ->
    ok | {error, term()}.
clear_expired_sessions(UserId, GracePeriod) ->
    {ok, ActiveUserSessions} = od_user:get_all_sessions(UserId),
    lists:foreach(fun(UserSessionId) ->
        case ?MODULE:get(UserSessionId) of
            {ok, #document{value = #session{last_refresh = LastRefresh}}} ->
                case gui_session:is_expired(LastRefresh) of
                    true ->
                        delete_user_session(UserSessionId, UserId, GracePeriod, false);
                    _ ->
                        ok
                end;
            {error, _} ->
                delete_user_session(UserSessionId, UserId, GracePeriod, false)
        end
    end, ActiveUserSessions).
