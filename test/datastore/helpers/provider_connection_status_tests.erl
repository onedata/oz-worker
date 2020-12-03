%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% This module contains eunit tests of provider_connection_status module.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_connection_status_tests).
-author("Lukasz Opiola").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup and test generator
%%%===================================================================

% configurable value in gs_ws_handler, mocked in these tests
-define(KEEPALIVE_INTERVAL, 30).
% as defined in provider_connection_status
-define(INACTIVITY_PERIOD, 2 * ?KEEPALIVE_INTERVAL).

provider_connection_status_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"default connection status", fun default_status/0},
            {"connected status", fun connected_status/0},
            {"new status by last activity", fun new_status_by_last_activity/0},
            {"disconnected status", fun disconnected_status/0}
        ]
    }.


setup() ->
    node_cache:init(),
    clock_freezer_mock:setup_locally([provider_connection_status]),
    meck:new(gs_ws_handler, [non_strict]),
    meck:expect(gs_ws_handler, keepalive_interval, fun() -> ?KEEPALIVE_INTERVAL end).

teardown(_) ->
    clock_freezer_mock:teardown_locally(),
    node_cache:destroy(),
    ?assert(meck:validate([gs_ws_handler])),
    meck:unload(gs_ws_handler).

%%%===================================================================
%%% Tests functions
%%%===================================================================

default_status() ->
    Status = provider_connection_status:default(),
    ?assertEqual({disconnected, 0}, provider_connection_status:inspect(Status)),

    ?assertEqual(Status, provider_connection_status:decode(provider_connection_status:encode(Status))).


new_status_by_last_activity() ->
    Status = provider_connection_status:new_by_last_activity(1508419013),
    ?assertEqual({disconnected, 1508419013}, provider_connection_status:inspect(Status)),

    ?assertEqual(Status, provider_connection_status:decode(provider_connection_status:encode(Status))).


connected_status() ->
    StatusAlpha = provider_connection_status:default(),
    ConnectionTimestamp = clock_freezer_mock:current_time_seconds(),
    StatusBeta = provider_connection_status:report_connected(StatusAlpha),
    ?assertEqual({connected, ConnectionTimestamp}, provider_connection_status:inspect(StatusBeta)),
    % the "connected" status is considered up to date within the ?INACTIVITY_PERIOD
    clock_freezer_mock:simulate_seconds_passing(?INACTIVITY_PERIOD - 1),
    ?assertEqual({connected, ConnectionTimestamp}, provider_connection_status:inspect(StatusBeta)),
    % consecutive heartbeat prolongs the connected status
    StatusGamma = provider_connection_status:report_heartbeat(StatusBeta),
    HeartbeatTimestamp = clock_freezer_mock:current_time_seconds(),
    clock_freezer_mock:simulate_seconds_passing(?INACTIVITY_PERIOD - 1),
    ?assertEqual({connected, ConnectionTimestamp}, provider_connection_status:inspect(StatusGamma)),
    % after the ?INACTIVITY_PERIOD without heartbeating, the provider is considered unresponsive
    clock_freezer_mock:simulate_seconds_passing(2),
    ?assertEqual({unresponsive, HeartbeatTimestamp}, provider_connection_status:inspect(StatusGamma)),
    % consecutive heartbeat prolongs the connected status, even in case of temporary instabilities
    StatusDelta = provider_connection_status:report_heartbeat(StatusGamma),
    ?assertEqual({connected, ConnectionTimestamp}, provider_connection_status:inspect(StatusDelta)),

    ?assertEqual(StatusAlpha, provider_connection_status:decode(provider_connection_status:encode(StatusAlpha))),
    ?assertEqual(StatusBeta, provider_connection_status:decode(provider_connection_status:encode(StatusBeta))),
    ?assertEqual(StatusGamma, provider_connection_status:decode(provider_connection_status:encode(StatusGamma))),
    ?assertEqual(StatusDelta, provider_connection_status:decode(provider_connection_status:encode(StatusDelta))).


disconnected_status() ->
    StatusAlpha = provider_connection_status:default(),
    FirstTimestamp = clock_freezer_mock:current_time_seconds(),
    StatusBeta = provider_connection_status:report_disconnected(StatusAlpha),
    ?assertEqual({disconnected, FirstTimestamp}, provider_connection_status:inspect(StatusBeta)),
    clock_freezer_mock:simulate_seconds_passing(124151),
    SecondTimestamp = clock_freezer_mock:current_time_seconds(),
    ?assertEqual({disconnected, FirstTimestamp}, provider_connection_status:inspect(StatusBeta)),
    StatusGamma = provider_connection_status:report_disconnected(StatusBeta),
    ?assertEqual({disconnected, SecondTimestamp}, provider_connection_status:inspect(StatusGamma)),
    % heartbeats should not be considered when the provider is disconnected
    % (although it is possible that such heartbeat is reported just after
    % the provider becomes disconnected as heartbeats are asynchronous)
    StatusDelta = provider_connection_status:report_heartbeat(StatusGamma),
    ?assertEqual({disconnected, SecondTimestamp}, provider_connection_status:inspect(StatusDelta)),

    ?assertEqual(StatusAlpha, provider_connection_status:decode(provider_connection_status:encode(StatusAlpha))),
    ?assertEqual(StatusBeta, provider_connection_status:decode(provider_connection_status:encode(StatusBeta))),
    ?assertEqual(StatusGamma, provider_connection_status:decode(provider_connection_status:encode(StatusGamma))),
    ?assertEqual(StatusDelta, provider_connection_status:decode(provider_connection_status:encode(StatusDelta))).


-endif.