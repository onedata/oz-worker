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


-spec get_last_activity(od_provider:id()) -> now | clock:seconds().
get_last_activity(ProviderId) ->
    case is_online(ProviderId) of
        true ->
            now;
        false ->
            case od_provider:get(ProviderId) of
                {ok, #document{value = #od_provider{last_activity = LastActivity}}} ->
                    LastActivity;
                _ ->
                    0
            end
    end.


-spec is_online(od_provider:id()) -> boolean().
is_online(ProviderId) ->
    get_all(ProviderId) /= [].


%%--------------------------------------------------------------------
%% @doc
%% Closes all connections of given provider. Closing the connections fires
%% on_terminate events, which result in calls to remove/2 function.
%% @end
%%--------------------------------------------------------------------
-spec close_all(od_provider:id()) -> ok.
close_all(ProviderId) ->
    lists:foreach(fun gs_server:terminate_connection/1, get_all(ProviderId)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec update(od_provider:id(), fun(([gs_server:conn_ref()]) -> [gs_server:conn_ref()])) ->
    {ok, connections_count()} | {error, term()}.
update(ProviderId, ConnectionsDiff) ->
    Diff = fun(Record = #provider_connections{connections = Connections}) ->
        {ok, Record#provider_connections{
            connections = ConnectionsDiff(Connections)
        }}
    end,
    Default = #provider_connections{
        connections = ConnectionsDiff([])
    },
    case datastore_model:update(?CTX, ProviderId, Diff, Default) of
        {ok, #document{value = #provider_connections{connections = Connections}}} ->
            % update can fail, e.g. when the provider has been deregistered
            od_provider:update(ProviderId, fun(Provider) ->
                {ok, Provider#od_provider{last_activity = clock:timestamp_seconds()}}
            end),
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