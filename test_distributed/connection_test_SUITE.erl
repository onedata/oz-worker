%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Basic tests that check connection to main parts of application
%%% @end
%%%-------------------------------------------------------------------
-module(connection_test_SUITE).
-author("Tomasz Lichon").

-include("registered_names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1]).
-export([rest_api_connection_test/1, datastore_connection_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([rest_api_connection_test, datastore_connection_test]).

rest_api_connection_test(Config) ->
    [Node1 | _] = ?config(oz_worker_nodes, Config),
    {ok, RestPort} = oz_test_utils:get_rest_port(Config),
    {ok, Domain} = test_utils:get_env(Node1, ?APP_NAME, http_domain),
    URL = str_utils:format("https://~s:~B/provider/public/check_my_ip", [Domain, RestPort]),
    Opts = [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}],
    ?assertMatch({ok, _, _, _}, http_client:get(URL, #{}, <<>>, Opts)).

datastore_connection_test(Config) ->
    [Node1, Node2] = ?config(oz_worker_nodes, Config),
    ?assertEqual(pong, rpc:call(Node1, worker_proxy, call, [datastore_worker, ping])),
    ?assertEqual(pong, rpc:call(Node2, worker_proxy, call, [datastore_worker, ping])).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(rest_api_connection_test, Config) ->
    ssl:start(),
    hackney:start(),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(rest_api_connection_test, _Config) ->
    hackney:stop(),
    ssl:stop();
end_per_testcase(_, _Config) ->
    ok.

end_per_suite(_) ->
    ok.
