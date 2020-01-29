%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Volatile record (memory only) holding information about providers'
%%% connections.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_connections).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([
    add/2,
    remove/2,
    get_all/1,
    get_last_activity/1,
    is_online/1,
    close_all/1
]).

%% datastore_model callbacks
-export([init/0]).

-type connections_count() :: non_neg_integer().

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec add(od_provider:id(), gs_server:conn_ref()) -> {ok, connections_count()} | {error, term()}.
add(ProviderId, ConnectionRef) ->
    update(ProviderId, fun(Connections) ->
        [ConnectionRef | Connections]
    end).


-spec remove(od_provider:id(), gs_server:conn_ref()) -> {ok, connections_count()} | {error, term()}.
remove(ProviderId, ConnectionRef) ->
    update(ProviderId, fun(Connections) ->
        lists:delete(ConnectionRef, Connections)
    end).


-spec get_all(od_provider:id()) -> [gs_server:conn_ref()].
get_all(ProviderId) ->
    case datastore_model:get(?CTX, ProviderId) of
        {ok, #document{value = #provider_connections{connections = Connections}}} ->
            Connections;
        {error, _} ->
            []
    end.


-spec get_last_activity(od_provider:id()) -> now | time_utils:seconds().
get_last_activity(ProviderId) ->
    case datastore_model:get(?CTX, ProviderId) of
        {ok, #document{value = #provider_connections{connections = C}}} when length(C) > 0 ->
            now;
        {ok, #document{value = #provider_connections{last_activity = LastActivity}}} ->
            LastActivity;
        {error, _} ->
            0
    end.


-spec is_online(od_provider:id()) -> boolean().
is_online(ProviderId) ->
    get_all(ProviderId) /= [].


-spec close_all(od_provider:id()) -> ok.
close_all(ProviderId) ->
    case datastore_model:get(?CTX, ProviderId) of
        {ok, #document{value = #provider_connections{connections = Connections}}} ->
            lists:foreach(fun(ConnRef) ->
                gs_server:terminate_connection(ConnRef)
            end, Connections);
        Error = {error, _} ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec update(od_provider:id(), fun(([gs_server:conn_ref()]) -> [gs_server:conn_ref()])) ->
    {ok, connections_count()} | {error, term()}.
update(ProviderId, ConnectionsDiff) ->
    Diff = fun(Record = #provider_connections{connections = Connections}) ->
        {ok, Record#provider_connections{
            connections = ConnectionsDiff(Connections),
            last_activity = time_utils:cluster_time_seconds()
        }}
    end,
    Default = #provider_connections{
        connections = ConnectionsDiff([]),
        last_activity = time_utils:cluster_time_seconds()
    },
    case datastore_model:update(?CTX, ProviderId, Diff, Default) of
        {ok, #document{value = #provider_connections{connections = Connections}}} ->
            {ok, length(Connections)};
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