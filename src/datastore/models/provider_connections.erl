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
-include_lib("ctool/include/logging.hrl").

%% API
-export([
    report_connected/2,
    report_heartbeat/1,
    report_disconnected/2,
    is_online/1, is_online/2,
    inspect_status/2, get_last_activity/1,
    close_all/1
]).

%% datastore_model callbacks
-export([init/0]).

-type online() :: boolean().
-type connections_count() :: non_neg_integer().

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec report_connected(od_provider:id(), gs_server:conn_ref()) -> {ok, connections_count()} | {error, term()}.
report_connected(ProviderId, ConnectionRef) ->
    Result = update_connections(ProviderId, fun(Connections) ->
        [ConnectionRef | Connections]
    end),
    case Result of
        {ok, 1} ->
            {ok, Name} = od_provider:get_name(ProviderId),
            ?info("Provider '~ts' has connected (~s)", [Name, ProviderId]);
        _ ->
            ok
    end,
    Result.


-spec report_heartbeat(od_provider:id()) -> ok.
report_heartbeat(ProviderId) ->
    transaction_on_provider(ProviderId, fun() ->
        od_provider:update(ProviderId, fun(Provider = #od_provider{connection_status = PrevStatus}) ->
            {ok, Provider#od_provider{connection_status = provider_connection_status:report_heartbeat(PrevStatus)}}
        end)
    end).


-spec report_disconnected(od_provider:id(), gs_server:conn_ref()) -> {ok, connections_count()} | {error, term()}.
report_disconnected(ProviderId, ConnectionRef) ->
    Result = update_connections(ProviderId, fun(Connections) ->
        lists:delete(ConnectionRef, Connections)
    end),
    case Result of
        {ok, 0} ->
            case od_provider:get_name(ProviderId) of
                {ok, Name} -> ?info("Provider '~ts' went offline (~s)", [Name, ProviderId]);
                _ -> ?info("Provider '~s' went offline", [ProviderId])
            end;
        _ ->
            ok
    end,
    Result.


-spec is_online(od_provider:id()) -> online().
is_online(ProviderId) when is_binary(ProviderId) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = ProviderRecord}} ->
            is_online(ProviderId, ProviderRecord);
        _ ->
            false
    end.

-spec is_online(od_provider:id(), od_provider:record()) -> online().
is_online(ProviderId, ProviderRecord) ->
    {Online, _} = inspect_status(ProviderId, ProviderRecord),
    Online.


-spec inspect_status(od_provider:id(), od_provider:record()) ->
    {online(), Since :: time_utils:seconds()}.
inspect_status(ProviderId, #od_provider{connection_status = Status}) ->
    case provider_connection_status:inspect(Status) of
        {connected, Since} ->
            {true, Since};
        {disconnected, Since} ->
            {false, Since};
        {unresponsive, Since} ->
            async_purge_connections_for_unresponsive_provider(ProviderId),
            {false, Since}
    end.


-spec get_last_activity(od_provider:doc()) -> now | time_utils:seconds().
get_last_activity(#document{key = ProviderId, value = ProviderRecord}) ->
    case inspect_status(ProviderId, ProviderRecord) of
        {true, _} -> now;
        {false, Since} -> Since
    end.


%%--------------------------------------------------------------------
%% @doc
%% Closes all connections of given provider. Closing the connections fires
%% on_terminate events (provided that the processes are alive), which result in
%% calls to remove/2 function.
%% @end
%%--------------------------------------------------------------------
-spec close_all(od_provider:id()) -> ok.
close_all(ProviderId) ->
    lists:foreach(fun gs_server:terminate_connection/1, get_all(ProviderId)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec get_all(od_provider:id()) -> [gs_server:conn_ref()].
get_all(ProviderId) ->
    case datastore_model:get(?CTX, ProviderId) of
        {ok, #document{value = #provider_connections{connections = Connections}}} ->
            Connections;
        {error, _} ->
            []
    end.


%% @private
-spec update_connections(od_provider:id(), fun(([gs_server:conn_ref()]) -> [gs_server:conn_ref()])) ->
    {ok, connections_count()} | {error, term()}.
update_connections(ProviderId, ConnectionsDiff) ->
    transaction_on_provider(ProviderId, fun() -> unsafe_update_connections(ProviderId, ConnectionsDiff) end).


%% @private
-spec unsafe_update_connections(od_provider:id(), fun(([gs_server:conn_ref()]) -> [gs_server:conn_ref()])) ->
    {ok, connections_count()} | {error, term()}.
unsafe_update_connections(ProviderId, ConnectionsDiff) ->
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
            ConnectionCount = length(Connections),
            % update can fail, e.g. when the provider has been deregistered
            od_provider:update(ProviderId, fun(Provider = #od_provider{connection_status = PrevStatus}) ->
                {ok, Provider#od_provider{connection_status = case ConnectionCount of
                    0 -> provider_connection_status:report_disconnected(PrevStatus);
                    _ -> provider_connection_status:report_connected(PrevStatus)
                end}}
            end),
            {ok, ConnectionCount};
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to clean all existing connections of an unresponsive provider,
%% forcing them to close (if possible) and nullifying the list of connections,
%% which results in the provider being marked as disconnected.
%% The procedure is run asynchronously so as not to block the calling process.
%% @end
%%--------------------------------------------------------------------
-spec async_purge_connections_for_unresponsive_provider(od_provider:id()) -> ok | {error, term()}.
async_purge_connections_for_unresponsive_provider(ProviderId) ->
    spawn(fun() ->
        transaction_on_provider(ProviderId, fun() ->
            % make sure the provider is still unresponsive inside the
            % transaction to avoid race conditions
            case is_unresponsive(ProviderId) of
                true ->
                    close_all(ProviderId),
                    unsafe_update_connections(ProviderId, fun(_) -> [] end);
                false ->
                    ok
            end
        end)
    end),
    ok.


%% @private
-spec is_unresponsive(od_provider:id()) -> boolean().
is_unresponsive(ProviderId) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = #od_provider{connection_status = Status}}} ->
            case provider_connection_status:inspect(Status) of
                {unresponsive, _} -> true;
                {_, _} -> false
            end;
        _ ->
            false
    end.


%% @private
-spec transaction_on_provider(od_provider:id(), fun(() -> Result)) -> Result when Result :: term().
transaction_on_provider(ProviderId, Fun) ->
    critical_section:run({provider_connections, ProviderId}, Fun).

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