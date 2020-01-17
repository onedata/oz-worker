%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common mocks used in oz-worker's CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_mocks).
-author("Lukasz Opiola").

-include("ozt.hrl").

-define(TIME_MOCK_STARTING_TIMESTAMP, 1500000000).

%% API
-export([mock_time/0, unmock_time/0, simulate_time_passing/1, get_mocked_time/0]).

-export([mock_gui_static/0, unmock_gui_static/0]).

-export([mock_peer_ip_of_all_connections/1, unmock_peer_ip_of_all_connections/0]).

-export([mock_geo_db_entry_for_all_ips/3, unmock_geo_db_entry_for_all_ips/0]).

-export([mock_harvester_plugins/0, mock_harvester_plugins/1]).
-export([unmock_harvester_plugins/0, unmock_harvester_plugins/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Mocks the time - stops the clock at one value and allows to manually
%% simulate time passing.
%% @end
%%--------------------------------------------------------------------
-spec mock_time() -> ok.
mock_time() ->
    ok = test_utils:mock_new(ozt:get_nodes(), time_utils, [passthrough]),
    ok = test_utils:mock_expect(ozt:get_nodes(), time_utils, cluster_time_seconds, fun() ->
        oz_worker:get_env(mocked_time, ?TIME_MOCK_STARTING_TIMESTAMP)
    end).


-spec unmock_time() -> ok.
unmock_time() ->
    ok = test_utils:mock_unload(ozt:get_nodes(), time_utils).


-spec simulate_time_passing(time_utils:seconds()) -> ok.
simulate_time_passing(Seconds) ->
    ozt:set_env(mocked_time, get_mocked_time() + Seconds).


-spec get_mocked_time() -> time_utils:seconds().
get_mocked_time() ->
    ozt:get_env(mocked_time, ?TIME_MOCK_STARTING_TIMESTAMP).


%%--------------------------------------------------------------------
%% @doc
%% Mocks the gui_static module - useful in tests that create / delete a lot of
%% Oneprovider instances to skip these intensive IO operations.
%% @end
%%--------------------------------------------------------------------
-spec mock_gui_static() -> ok.
mock_gui_static() ->
    ok = test_utils:mock_new(ozt:get_nodes(), gui_static, [passthrough]),
    ok = test_utils:mock_expect(ozt:get_nodes(), gui_static, gui_exists, fun(_, _) ->
        true
    end),
    ok = test_utils:mock_expect(ozt:get_nodes(), gui_static, link_gui, fun(_, _, _) ->
        ok
    end),
    ok = test_utils:mock_expect(ozt:get_nodes(), gui_static, unlink_gui, fun(_, _) ->
        ok
    end).


-spec unmock_gui_static() -> ok.
unmock_gui_static() ->
    ok = test_utils:mock_unload(ozt:get_nodes(), gui_static).


-spec mock_peer_ip_of_all_connections(ip_utils:ip()) -> ok.
mock_peer_ip_of_all_connections(Ip) ->
    ok = test_utils:mock_new(ozt:get_nodes(), cowboy_req, [passthrough]),
    ok = test_utils:mock_expect(ozt:get_nodes(), cowboy_req, peer, fun(_) ->
        {Ip, _Port = 10000}  % port is not relevant
    end).


-spec unmock_peer_ip_of_all_connections() -> ok.
unmock_peer_ip_of_all_connections() ->
    ok = test_utils:mock_unload(ozt:get_nodes(), cowboy_req).


-spec mock_geo_db_entry_for_all_ips(ip_utils:ans(), ip_utils:country_code(), [ip_utils:region()]) ->
    ok.
mock_geo_db_entry_for_all_ips(Asn, Country, Regions) ->
    ok = test_utils:mock_new(ozt:get_nodes(), ip_utils, [passthrough]),
    ok = test_utils:mock_expect(ozt:get_nodes(), ip_utils, lookup_asn, fun
        (undefined) -> {error, invalid_address};
        (_) -> {ok, Asn}
    end),
    ok = test_utils:mock_expect(ozt:get_nodes(), ip_utils, lookup_country, fun
        (undefined) -> {error, invalid_address};
        (_) -> {ok, Country}
    end),
    ok = test_utils:mock_expect(ozt:get_nodes(), ip_utils, lookup_region, fun
        (undefined) -> {error, invalid_address};
        (_) -> {ok, Regions}
    end).


-spec unmock_geo_db_entry_for_all_ips() -> ok.
unmock_geo_db_entry_for_all_ips() ->
    ok = test_utils:mock_unload(ozt:get_nodes(), ip_utils).


-spec mock_harvester_plugins() -> ok.
mock_harvester_plugins() ->
    mock_harvester_plugins(?HARVESTER_MOCK_PLUGIN).

-spec mock_harvester_plugins(Plugins :: atom() | list()) -> ok.
mock_harvester_plugins(Plugins) ->
    oz_test_utils:mock_harvester_plugins(ozt:get_test_config(), Plugins).


-spec unmock_harvester_plugins() -> ok.
unmock_harvester_plugins() ->
    unmock_harvester_plugins(?HARVESTER_MOCK_PLUGIN).

-spec unmock_harvester_plugins(Plugins :: atom() | list()) -> ok.
unmock_harvester_plugins(Plugins) ->
    oz_test_utils:unmock_harvester_plugins(ozt:get_test_config(), Plugins).
