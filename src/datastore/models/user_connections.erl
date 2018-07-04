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
    add/2,
    get_all/1,
    remove/2,
    clear/2
]).

%% datastore_model callbacks
-export([init/0]).

-type id() :: session:id().
-type record() :: #user_connections{}.
-type doc() :: datastore_doc:doc(record()).
-export_type([id/0, record/0]).

-define(OK_ON_SUCCESS(__Term), case __Term of
    {ok, _} -> ok;
    {error, _} = Error -> Error
end).

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Adds a connection to the list of user connections.
%% @end
%%--------------------------------------------------------------------
-spec add(id(), gs_server:conn_ref()) -> ok | {error, term()}.
add(SessionId, ConnectionRef) ->
    Diff = fun(Record = #user_connections{connections = Connections}) ->
        {ok, Record#user_connections{connections = [ConnectionRef | Connections]}}
    end,
    Default = #document{
        key = SessionId, value = #user_connections{
            connections = [ConnectionRef]
        }},
    ?OK_ON_SUCCESS(datastore_model:update(?CTX, SessionId, Diff, Default)).


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of user connections.
%% @end
%%--------------------------------------------------------------------
-spec get_all(id()) -> {ok, [gs_server:conn_ref()]}.
get_all(SessionId) ->
    case datastore_model:get(?CTX, SessionId) of
        {ok, #document{value = #user_connections{connections = Connections}}} ->
            {ok, Connections};
        {error, _} ->
            {ok, []}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Removes a connection from the list of user connections.
%% @end
%%--------------------------------------------------------------------
-spec remove(id(), gs_server:conn_ref()) -> ok | {error, term()}.
remove(SessionId, ConnectionRef) ->
    Diff = fun(Record = #user_connections{connections = Connections}) ->
        {ok, Record#user_connections{connections = Connections -- [ConnectionRef]}}
    end,
    Default = #document{
        key = SessionId, value = #user_connections{
            connections = []
        }},
    ?OK_ON_SUCCESS(datastore_model:update(?CTX, SessionId, Diff, Default)).


%%--------------------------------------------------------------------
%% @doc
%% Removes information about all connections related to given session.
%% @end
%%--------------------------------------------------------------------
-spec clear(id(), GracePeriodMillis :: undefined | non_neg_integer()) ->
    ok | {error, term()}.
clear(SessionId, GracePeriod) ->
    {ok, Connections} = get_all(SessionId),
    case GracePeriod of
        undefined ->
            terminate_connections(Connections);
        Milliseconds ->
            spawn(fun() ->
                timer:sleep(Milliseconds),
                terminate_connections(Connections)
            end)
    end,
    % Disconnect the connections with a delay to allow for flushing
    datastore_model:delete(?CTX, SessionId).


%% @private
-spec terminate_connections(Connections :: [gs_server:conn_ref()]) -> ok.
terminate_connections(Connections) ->
    lists:foreach(fun(Connection) ->
        gs_server:terminate_connection(Connection)
    end, Connections).


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