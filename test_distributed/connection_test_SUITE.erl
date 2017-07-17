%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
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
-export([init_per_testcase/2, end_per_testcase/2]).
-export([rest_api_connection_test/1, datastore_connection_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([rest_api_connection_test, datastore_connection_test]).

rest_api_connection_test(Config) ->
    [Node1, Node2] = ?config(oz_worker_nodes, Config),
    {ok, RestPort} = rpc:call(Node1, application, get_env, [?APP_NAME, rest_port]),
    URL1 = str_utils:format("https://~s:~B/provider/test/check_my_ip", [utils:get_host(Node1), RestPort]),
    URL2 = str_utils:format("https://~s:~B/provider/test/check_my_ip", [utils:get_host(Node2), RestPort]),
    ?assertMatch({ok, _, _, _}, http_client:get(URL1, #{}, <<>>, [insecure])),
    ?assertMatch({ok, _, _, _}, http_client:get(URL2, #{}, <<>>, [insecure])).

datastore_connection_test(Config) ->
    [Node1, Node2] = ?config(oz_worker_nodes, Config),
    ?assertEqual(pong, rpc:call(Node1, worker_proxy, call, [datastore_worker, ping])),
    ?assertEqual(pong, rpc:call(Node2, worker_proxy, call, [datastore_worker, ping])).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

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
