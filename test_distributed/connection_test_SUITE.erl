%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Basic tests that check connection to main parts of application
%% @end
%% ===================================================================
-module(connection_test_SUITE).
-author("Tomasz Lichon").

%% Includes
-include("registered_names.hrl").
-include("gr_test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([rest_api_connection_test/1, dao_connection_test/1]).

-perf_test({perf_cases, []}).
all() -> [rest_api_connection_test, dao_connection_test].

rest_api_connection_test(Config) ->
    ibrowse:start(),
    ssl:start(),
    [Node] = ?config(gr_nodes, Config),
    {ok, RestPort} = rpc:call(Node, application, get_env, [?APP_Name, rest_port]),
    Ans = ibrowse:send_req("https://" ++ atom_to_list(?GET_HOST(Node)) ++ ":" ++ integer_to_list(RestPort) ++ "/provider/test/check_my_ip", [], get, [], [{ssl_options, [{verify, verify_none}]}]),
    ?assertMatch({ok, _, _, _}, Ans),
    ssl:stop(),
    ibrowse:stop().

dao_connection_test(Config) ->
    [Node] = ?config(gr_nodes, Config),

    ?assertMatch({ok, _}, rpc:call(Node, dao_lib, apply, [dao_helper, list_dbs, [], 1])).

%% ====================================================================
%% SetUp and TearDown functions
%% ====================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    timer:sleep(30000), % TODO add nagios to GR and delete sleep
    NewConfig.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).