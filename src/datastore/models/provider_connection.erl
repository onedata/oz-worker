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
-module(provider_connection).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([
    add_connection/2,
    get_connection_ref/1,
    is_online/1,
    remove_connection/1
]).

%% datastore_model callbacks
-export([init/0]).

-type id() :: od_provider:id().
-type record() :: #provider_connection{}.
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
%% Creates new provider connection record.
%% @end
%%--------------------------------------------------------------------
-spec add_connection(id(), gs_server:conn_ref()) ->
    {ok, doc()} | {error, term()}.
add_connection(ProviderId, ConnectionRef) ->
    datastore_model:save(?CTX, #document{
        key = ProviderId, value = #provider_connection{
            connection_ref = ConnectionRef
        }}).

%%--------------------------------------------------------------------
%% @doc
%% Returns connection ref by provider id.
%% @end
%%--------------------------------------------------------------------
-spec get_connection_ref(id()) -> {ok, doc()} | {error, term()}.
get_connection_ref(ProviderId) ->
    case datastore_model:get(?CTX, ProviderId) of
        {ok, #document{value = #provider_connection{connection_ref = ConnRef}}} ->
            {ok, ConnRef};
        Error = {error, _} ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns whether given provider is connected.
%% @end
%%--------------------------------------------------------------------
-spec is_online(id()) -> boolean().
is_online(ProviderId) ->
    case get_connection_ref(ProviderId) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Removes a provider connection record.
%% @end
%%--------------------------------------------------------------------
-spec remove_connection(id()) -> ok | {error, term()}.
remove_connection(ProviderId) ->
    datastore_model:delete(?CTX, ProviderId).

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