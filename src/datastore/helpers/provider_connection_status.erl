%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module used for tracking and storing the connection status - information
%%% whether a Oneprovider service is online and for how long has it been in
%%% this state.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_connection_status).
-author("Lukasz Opiola").

-type summary() :: connected | disconnected | unresponsive.
-type timestamp() :: time_utils:seconds().
-export_type([summary/0]).

%% Holds information required to track provider's connection status. The status
%% is inferred based on two pieces of information:
%%  * last_transition - holds the last known transition of provider's connection
%%    status and its timestamp, but is not enough to determine if the provider
%%    is currently online, because some transitions might fail to be reported
%%    (e.g. during node failure or system shutdown) - hence heartbeats are used
%%    for verification
%%
%%  * last_heartbeat - timestamp of the last heartbeat, helps with inferring
%%    the connection status when the last known transition is "connected".
%%    If the last heartbeat took place within the ?INACTIVITY_PERIOD, the
%%    provider is considered active and online. Otherwise, the connection is
%%    considered unresponsive.
-record(status, {
    last_transition :: {connected | disconnected, timestamp()},
    last_heartbeat :: timestamp()
}).
-opaque record() :: #status{}.
-export_type([record/0]).

-define(NOW(), time_utils:timestamp_seconds()).
% Time after which a non-heartbeating connection is considered unresponsive
-define(INACTIVITY_PERIOD, 2 * gs_ws_handler:keepalive_interval()).

-export([default/0]).
-export([new_by_last_activity/1]).
-export([inspect/1]).
-export([report_connected/1, report_heartbeat/1, report_disconnected/1]).
-export([encode/1, decode/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec default() -> record().
default() ->
    #status{
        last_transition = {disconnected, 0},
        last_heartbeat = 0
    }.


-spec new_by_last_activity(timestamp()) -> record().
new_by_last_activity(Timestamp) ->
    #status{
        last_transition = {disconnected, Timestamp},
        last_heartbeat = 0
    }.


-spec inspect(record()) -> {summary(), Since :: timestamp()}.
inspect(#status{last_transition = {connected, TransitionTimestamp}, last_heartbeat = HeartbeatTimestamp}) ->
    case HeartbeatTimestamp + ?INACTIVITY_PERIOD >= ?NOW() of
        true -> {connected, TransitionTimestamp};
        false -> {unresponsive, HeartbeatTimestamp}
    end;
inspect(#status{last_transition = {disconnected, TransitionTimestamp}}) ->
    {disconnected, TransitionTimestamp}.


-spec report_connected(record()) -> record().
report_connected(Previous) ->
    Timestamp = ?NOW(),
    case inspect(Previous) of
        {connected, _} ->
            % do not overwrite the transition timestamp if this is another connection
            Previous#status{
                last_heartbeat = Timestamp
            };
        {_, _} ->
            Previous#status{
                last_transition = {connected, Timestamp},
                last_heartbeat = Timestamp
            }
    end.


-spec report_heartbeat(record()) -> record().
report_heartbeat(Previous) ->
    Previous#status{
        last_heartbeat = ?NOW()
    }.


-spec report_disconnected(record()) -> record().
report_disconnected(Previous) ->
    Previous#status{
        last_transition = {disconnected, ?NOW()}
    }.


-spec encode(record()) -> binary().
encode(#status{last_transition = {Transition, TransitionTimestamp}, last_heartbeat = HeartbeatTimestamp}) ->
    json_utils:encode(#{
        <<"lastTransitionValue">> => Transition,
        <<"lastTransitionTimestamp">> => TransitionTimestamp,
        <<"lastHeartbeatTimestamp">> => HeartbeatTimestamp
    }).


-spec decode(binary()) -> record().
decode(Encoded) ->
    ConnectionStatusJson = json_utils:decode(Encoded),
    Transition = binary_to_atom(maps:get(<<"lastTransitionValue">>, ConnectionStatusJson), utf8),
    TransitionTimestamp = maps:get(<<"lastTransitionTimestamp">>, ConnectionStatusJson),
    HeartbeatTimestamp = maps:get(<<"lastHeartbeatTimestamp">>, ConnectionStatusJson),
    #status{last_transition = {Transition, TransitionTimestamp}, last_heartbeat = HeartbeatTimestamp}.