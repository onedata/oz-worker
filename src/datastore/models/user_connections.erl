%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Volatile record (memory only) holding information about users' Graph Sync
%%% connections per session id.
%%% @end
%%%-------------------------------------------------------------------
-module(user_connections).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([
    add/3,
    remove/3,
    get_all/2,
    get_last_activity/1,
    close_all/2
]).

%% datastore_model callbacks
-export([init/0]).

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec add(od_user:id(), session:id(), gs_server:conn_ref()) -> ok | {error, term()}.
add(UserId, SessionId, ConnectionRef) ->
    update(UserId, fun(ConnectionsPerSession) ->
        Connections = maps:get(SessionId, ConnectionsPerSession, []),
        ConnectionsPerSession#{
            SessionId => [ConnectionRef | Connections]
        }
    end).


-spec remove(od_user:id(), session:id(), gs_server:conn_ref()) -> ok | {error, term()}.
remove(UserId, SessionId, ConnectionRef) ->
    update(UserId, fun(ConnectionsPerSession) ->
        Connections = maps:get(SessionId, ConnectionsPerSession, []),
        case lists:delete(ConnectionRef, Connections) of
            [] -> maps:remove(SessionId, ConnectionsPerSession);
            List -> maps:put(SessionId, List, ConnectionsPerSession)
        end
    end).


-spec get_all(od_user:id(), session:id()) -> [gs_server:conn_ref()].
get_all(UserId, SessionId) ->
    case datastore_model:get(?CTX, UserId) of
        {ok, #document{value = #user_connections{connections_per_session = ConnectionsPerSession}}} ->
            maps:get(SessionId, ConnectionsPerSession, []);
        {error, _} ->
            []
    end.


-spec get_last_activity(od_user:id()) -> now | time_utils:seconds().
get_last_activity(UserId) ->
    case datastore_model:get(?CTX, UserId) of
        {ok, #document{value = #user_connections{connections_per_session = C}}} when map_size(C) > 0 ->
            now;
        _ ->
            case od_user:get(UserId) of
                {ok, #document{value = #od_user{last_activity = LastActivity}}} ->
                    LastActivity;
                _ ->
                    0
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Closes all connections of given user within a specific session. Closing the
%% connections fires on_terminate events, which result in calls to remove/3
%% function.
%% @end
%%--------------------------------------------------------------------
-spec close_all(od_user:id(), session:id()) -> ok.
close_all(UserId, SessionId) ->
    Connections = get_all(UserId, SessionId),
    lists:foreach(fun(Connection) ->
        gs_server:terminate_connection(Connection)
    end, Connections).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec update(od_user:id(), fun((#{session:id() => gs_server:conn_ref()}) -> #{session:id() => gs_server:conn_ref()})) ->
    ok | {error, term()}.
update(UserId, ConnectionsDiff) ->
    Diff = fun(Record = #user_connections{connections_per_session = ConnectionsPerSession}) ->
        {ok, Record#user_connections{
            connections_per_session = ConnectionsDiff(ConnectionsPerSession)
        }}
    end,
    Default = #user_connections{
        connections_per_session = ConnectionsDiff(#{})
    },
    case datastore_model:update(?CTX, UserId, Diff, Default) of
        {ok, _} ->
            % update can fail, e.g. when the user has been deleted
            od_user:update(UserId, fun(User) ->
                {ok, User#od_user{last_activity = time_utils:cluster_time_seconds()}}
            end),
            ok;
        {error, _} = Error ->
            Error
    end.

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