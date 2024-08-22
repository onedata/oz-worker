%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Tests of the oz-worker circuit breaker mechanism.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_worker_circuit_breaker_test_SUITE).
-author("Katarzyna Such").

-include_lib("ctool/include/http/headers.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").
-include("api_test_utils.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    oai_handler_circuit_breaker_test/1,
    rest_handler_circuit_breaker_test/1,
    gs_circuit_breaker_test/1
]).

all() -> ?ALL([
    oai_handler_circuit_breaker_test,
    rest_handler_circuit_breaker_test,
    gs_circuit_breaker_test
]).


%%%===================================================================
%%% Tests
%%%===================================================================


oai_handler_circuit_breaker_test(_Config) ->
    set_circuit_breaker_state(closed),
    ?assertMatch(ok, get_oai_response()),

    set_circuit_breaker_state(open),
    ?assertMatch(?ERROR_SERVICE_UNAVAILABLE, get_oai_response()),

    set_circuit_breaker_state(closed),
    ?assertMatch(ok, get_oai_response()).


rest_handler_circuit_breaker_test(_Config) ->
    set_circuit_breaker_state(closed),
    ?assertMatch(ok, get_rest_response()),

    set_circuit_breaker_state(open),
    ?assertMatch(?ERROR_SERVICE_UNAVAILABLE, get_rest_response()),

    set_circuit_breaker_state(closed),
    ?assertMatch(ok, get_rest_response()).


gs_circuit_breaker_test(_Config) ->
    gs_circuit_breaker_test_base(oneprovider, ?PROVIDER(ozt_providers:create())),
    gs_circuit_breaker_test_base(gui, ?USER(ozt_users:create())).


gs_circuit_breaker_test_base(Endpoint, Auth = #auth{subject = Subject}) ->
    RequestSpec = example_gs_request(Endpoint, Auth),
    ClientAuth = {token, ozt_tokens:create(named, Subject, ?ACCESS_TOKEN, [])},
    Self = self(),
    PushCallback = fun(Push) -> Self ! Push end,

    set_circuit_breaker_state(closed),
    {ok, GsClient, _} = ?assertMatch({ok, _, _}, ozt_gs:connect(Endpoint, ClientAuth, PushCallback)),
    ?assertMatch({ok, _}, gs_client:sync_request(GsClient, RequestSpec)),

    set_circuit_breaker_state(open),
    ?assertMatch(?ERROR_SERVICE_UNAVAILABLE, gs_client:sync_request(GsClient, RequestSpec)),
    ?assertMatch(?ERROR_SERVICE_UNAVAILABLE, ozt_gs:connect(Endpoint, ClientAuth, PushCallback)),

    set_circuit_breaker_state(closed),
    ?assertMatch({ok, _}, gs_client:sync_request(GsClient, RequestSpec)),
    ?assertMatch({ok, _, _}, ozt_gs:connect(Endpoint, ClientAuth)).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ozt:init_per_suite(Config),
    oct_background:init_per_suite(NewConfig, #onenv_test_config{
        onenv_scenario = "1oz",
        posthook = ?config(?ENV_UP_POSTHOOK, NewConfig)
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_, Config) ->
    Config.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
get_oai_response() ->
    Url = ozt_http:build_url([
        list_to_binary(ozt:get_env(oai_pmh_api_prefix)),
        <<"?verb=Identify">>
    ]),
    ?extract_ok(ozt_http:request(get, Url, #{})).


%% @private
get_rest_response() ->
    ?extract_ok(ozt_http:rest_call(undefined, get, <<"/configuration">>)).


%% @private
example_gs_request(Endpoint, ClientAuth) ->
    #gs_req{
        subtype = graph,
        request = #gs_req_graph{
            gri = case {Endpoint, ClientAuth} of
                {gui, ?USER(UserId)} -> ?GRI(od_user, UserId, instance, protected);
                {oneprovider, ?PROVIDER(ProviderId)} -> ?GRI(od_provider, ProviderId, instance, protected)
            end,
            operation = get,
            subscribe = false
        }
    }.


%% @private
set_circuit_breaker_state(State) ->
    ok = ozw_test_rpc:set_env(service_circuit_breaker_state, State).