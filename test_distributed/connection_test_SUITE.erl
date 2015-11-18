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
-include_lib("annotations/include/annotations.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([rest_api_connection_test/1, dao_connection_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-performance({test_cases, []}).
all() -> [rest_api_connection_test, dao_connection_test].

rest_api_connection_test(Config) ->
    hackney:start(),
    [Node] = ?config(gr_nodes, Config),
    {ok, RestPort} = rpc:call(Node, application, get_env,
        [?APP_Name, rest_port]),
    URL = str_utils:format("https://~s:~B/provider/test/check_my_ip",
        [utils:get_host(Node), RestPort]),
    ?assertMatch({ok, _, _, _}, http_client:get(URL, [], <<>>, [insecure])),
    hackney:stop().

dao_connection_test(Config) ->
    [Node] = ?config(gr_nodes, Config),
    ?assertMatch({ok, _}, rpc:call(Node, dao_lib, apply,
        [dao_helper, list_dbs, [], 1])).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    timer:sleep(60000), % TODO add nagios to GR and delete sleep
    NewConfig.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).