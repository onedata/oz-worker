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
-export([mock_time/0, unmock_time/0, get_mocked_time/0, simulate_time_passing/1]).

-export([mock_gui_static/0, unmock_gui_static/0]).

-export([mock_peer_ip_of_all_connections/1, unmock_peer_ip_of_all_connections/0]).

-export([mock_geo_db_entry_for_all_ips/3, unmock_geo_db_entry_for_all_ips/0]).

-export([mock_handle_proxy/0, unmock_handle_proxy/0]).

-export([mock_harvesting_backends/0, mock_harvesting_backends/1]).
-export([unmock_harvesting_backends/0, unmock_harvesting_backends/1]).

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
    set_mocked_time(?TIME_MOCK_STARTING_TIMESTAMP),
    ok = test_utils:mock_new(ozt:get_nodes(), time_utils, [passthrough]),
    ok = test_utils:mock_expect(ozt:get_nodes(), time_utils, timestamp_seconds, fun() ->
        oz_worker:get_env(mocked_time)
    end).


-spec unmock_time() -> ok.
unmock_time() ->
    set_mocked_time(undefined),
    ok = test_utils:mock_unload(ozt:get_nodes(), time_utils).


%% @private
-spec set_mocked_time(time_utils:seconds() | undefined) -> ok.
set_mocked_time(Time) ->
    ozt:set_env(mocked_time, Time).


-spec get_mocked_time() -> time_utils:seconds() | no_return().
get_mocked_time() ->
    case ozt:get_env(mocked_time, undefined) of
        Time when is_integer(Time) -> Time;
        undefined -> error("~p:mock_time/0 must be called first to use get_mocked_time/0", [?MODULE])
    end.


-spec simulate_time_passing(time_utils:seconds()) -> ok.
simulate_time_passing(Seconds) ->
    set_mocked_time(get_mocked_time() + Seconds).


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


-spec mock_handle_proxy() -> ok.
mock_handle_proxy() ->
    oz_test_utils:mock_handle_proxy(ozt:get_test_config()).

-spec unmock_handle_proxy() -> ok.
unmock_handle_proxy() ->
    oz_test_utils:unmock_handle_proxy(ozt:get_test_config()).


-spec mock_harvesting_backends() -> ok.
mock_harvesting_backends() ->
    mock_harvesting_backends([?HARVESTER_MOCK_BACKEND, ?HARVESTER_MOCK_BACKEND2]).

-spec mock_harvesting_backends(Backends :: atom() | list()) -> ok.
mock_harvesting_backends(Backends) ->
    oz_test_utils:mock_harvesting_backends(ozt:get_test_config(), Backends).


-spec unmock_harvesting_backends() -> ok.
unmock_harvesting_backends() ->
    unmock_harvesting_backends([?HARVESTER_MOCK_BACKEND, ?HARVESTER_MOCK_BACKEND2]).

-spec unmock_harvesting_backends(Plugins :: atom() | list()) -> ok.
unmock_harvesting_backends(Backends) ->
    oz_test_utils:unmock_harvesting_backends(ozt:get_test_config(), Backends).
