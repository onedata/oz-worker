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
    is_online/1,
    close_all/1
]).

%% datastore_model callbacks
-export([init/0]).

-type id() :: od_provider:id().
-type record() :: #provider_connections{}.
-export_type([id/0, record/0]).

-type connections_count() :: non_neg_integer().

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec add(id(), gs_server:conn_ref()) -> {ok, connections_count()} | {error, term()}.
add(ProviderId, ConnectionRef) ->
    Diff = fun(Record = #provider_connections{connections = Connections}) ->
        {ok, Record#provider_connections{
            connections = [ConnectionRef | Connections]
        }}
    end,
    Default = #provider_connections{connections = [ConnectionRef]},
    update(ProviderId, Diff, Default).


-spec remove(id(), gs_server:conn_ref()) -> {ok, connections_count()} | {error, term()}.
remove(ProviderId, ConnectionRef) ->
    Diff = fun(Record = #provider_connections{connections = Connections}) ->
        {ok, Record#provider_connections{
            connections = Connections -- [ConnectionRef]
        }}
    end,
    Default = #provider_connections{connections = []},
    update(ProviderId, Diff, Default).


-spec is_online(id()) -> boolean().
is_online(ProviderId) ->
    case datastore_model:get(?CTX, ProviderId) of
        {ok, #document{value = #provider_connections{connections = [_ | _]}}} -> true;
        _ -> false
    end.


-spec close_all(id()) -> ok.
close_all(ProviderId) ->
    case datastore_model:get(?CTX, ProviderId) of
        {ok, #document{value = #provider_connections{connections = Connections}}} ->
            lists:foreach(fun(ConnRef) ->
                gs_server:terminate_connection(ConnRef)
            end, Connections);
        Error = {error, _} ->
            Error
    end.


%% @private
-spec update(id(), datastore_model:diff(), record()) -> {ok, connections_count()} | {error, term()}.
update(ProviderId, Diff, Default) ->
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