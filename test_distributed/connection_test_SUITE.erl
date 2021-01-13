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

-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1]).
-export([
    rest_api_connection_test/1,
    datastore_connection_test/1,
    compatibility_registry_endpoint_test/1
]).

all() -> ?ALL([
    rest_api_connection_test,
    datastore_connection_test,
    compatibility_registry_endpoint_test
]).

-define(SSL_OPTS(Config), [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}]).

%%%===================================================================
%%% Test functions
%%%===================================================================

rest_api_connection_test(Config) ->
    URL = oz_test_utils:oz_url(Config, [<<"/provider/public/check_my_ip">>]),
    ?assertMatch({ok, _, _, _}, http_client:get(URL, #{}, <<>>, ?SSL_OPTS(Config))).


datastore_connection_test(Config) ->
    [Node1, Node2] = ?config(oz_worker_nodes, Config),
    ?assertEqual(pong, rpc:call(Node1, worker_proxy, call, [datastore_worker, ping])),
    ?assertEqual(pong, rpc:call(Node2, worker_proxy, call, [datastore_worker, ping])).


compatibility_registry_endpoint_test(Config) ->
    URL = oz_test_utils:oz_url(Config, [<<"/compatibility.json">>]),
    {ok, _, Headers, Body} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, http_client:get(
        URL, #{}, <<>>, ?SSL_OPTS(Config)
    )),
    ?assertMatch(#{?HDR_CONTENT_TYPE := <<"application/json">>}, Headers),
    CompatibilityRegistryPath = oz_test_utils:call_oz(Config, ctool, get_env, [current_compatibility_registry_file]),
    {ok, FileContent} = oz_test_utils:call_oz(Config, file, read_file, [CompatibilityRegistryPath]),
    ?assertEqual(FileContent, Body),
    ?assertMatch(#{<<"revision">> := _}, json_utils:decode(Body)).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(_, Config) ->
    ssl:start(),
    hackney:start(),
    Config.

end_per_testcase(_, _Config) ->
    hackney:stop(),
    ssl:stop(),
    ok.

end_per_suite(_) ->
    ok.
