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

%% API
-export([freeze_time/0, unfreeze_time/0]).
-export([get_frozen_time_seconds/0, set_frozen_time_seconds/1, simulate_seconds_passing/1]).

-export([mock_new/1, mock_new/2]).
-export([mock_expect/3]).
-export([mock_unload/1]).

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
%% Stops the clock at one value and allows to manually simulate time passing.
%% @end
%%--------------------------------------------------------------------
-spec freeze_time() -> ok.
freeze_time() ->
    oz_test_utils:freeze_time(ozt:get_test_config()).


-spec unfreeze_time() -> ok.
unfreeze_time() ->
    oz_test_utils:unfreeze_time(ozt:get_test_config()).


-spec get_frozen_time_seconds() -> time:seconds() | no_return().
get_frozen_time_seconds() ->
    oz_test_utils:get_frozen_time_seconds().


-spec set_frozen_time_seconds(time:seconds()) -> ok.
set_frozen_time_seconds(Seconds) ->
    clock_freezer_mock:set_current_time_millis(timer:seconds(Seconds)).


-spec simulate_seconds_passing(time:seconds()) -> ok.
simulate_seconds_passing(Seconds) ->
    oz_test_utils:simulate_seconds_passing(Seconds).


-spec mock_new(module() | [module()]) -> ok.
mock_new(ModuleOrList) ->
    mock_new(ModuleOrList, [passthrough, no_history]).

-spec mock_new(module() | [module()], [mock_opt()]) -> ok.
mock_new(ModuleOrList, Options) ->
    ok = test_utils:mock_new(ozt:get_nodes(), ModuleOrList, Options).


-spec mock_expect(module(), atom(), function()) -> ok.
mock_expect(Module, FunctionName, Expectation) ->
    ok = test_utils:mock_expect(ozt:get_nodes(), Module, FunctionName, Expectation).


-spec mock_unload(module() | [module()]) -> ok.
mock_unload(ModuleOrList) ->
    ok = test_utils:mock_unload(ozt:get_nodes(), ModuleOrList).


%%--------------------------------------------------------------------
%% @doc
%% Mocks the gui_static module - useful in tests that create / delete a lot of
%% Oneprovider instances to skip these intensive IO operations.
%% @end
%%--------------------------------------------------------------------
-spec mock_gui_static() -> ok.
mock_gui_static() ->
    mock_new(gui_static, [passthrough]),
    mock_expect(gui_static, gui_exists, fun(_, _) -> true end),
    mock_expect(gui_static, link_gui, fun(_, _, _) -> ok end),
    mock_expect(gui_static, unlink_gui, fun(_, _) -> ok end).


-spec unmock_gui_static() -> ok.
unmock_gui_static() ->
    mock_unload(gui_static).


-spec mock_peer_ip_of_all_connections(ip_utils:ip()) -> ok.
mock_peer_ip_of_all_connections(Ip) ->
    mock_new(cowboy_req, [passthrough]),
    mock_expect(cowboy_req, peer, fun(_) ->
        {Ip, _Port = 10000}  % port is not relevant
    end).


-spec unmock_peer_ip_of_all_connections() -> ok.
unmock_peer_ip_of_all_connections() ->
    mock_unload(cowboy_req).


-spec mock_geo_db_entry_for_all_ips(ip_utils:ans(), ip_utils:country_code(), [ip_utils:region()]) ->
    ok.
mock_geo_db_entry_for_all_ips(Asn, Country, Regions) ->
    mock_new(ip_utils, [passthrough]),
    mock_expect(ip_utils, lookup_asn, fun
        (undefined) -> {error, invalid_address};
        (_) -> {ok, Asn}
    end),
    mock_expect(ip_utils, lookup_country, fun
        (undefined) -> {error, invalid_address};
        (_) -> {ok, Country}
    end),
    mock_expect(ip_utils, lookup_region, fun
        (undefined) -> {error, invalid_address};
        (_) -> {ok, Regions}
    end).


-spec unmock_geo_db_entry_for_all_ips() -> ok.
unmock_geo_db_entry_for_all_ips() ->
    mock_unload(ip_utils).


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
