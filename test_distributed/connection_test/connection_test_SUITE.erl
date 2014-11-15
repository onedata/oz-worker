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
-include("test_utils.hrl").
-include_lib("ctool/include/test/test_node_starter.hrl").
-include_lib("ctool/include/test/assertions.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([rest_api_connection_test/1, dao_connection_test/1]).

all() -> [rest_api_connection_test, dao_connection_test].

rest_api_connection_test(Config) ->
    ibrowse:start(),
    ssl:start(),
    [Node] = ?config(nodes, Config),
    {ok, RestPort} = rpc:call(Node, application, get_env, [?APP_Name, rest_port]),
    Ans = ibrowse:send_req("https://127.0.0.1:" ++ integer_to_list(RestPort) ++ "/provider/test/check_my_ip", [], get, [], [{ssl_options, [{verify, verify_none}]}]),
    ?assertMatch({ok, _, _, _}, Ans),
    ssl:stop(),
    ibrowse:stop().

dao_connection_test(Config) ->
    [Node] = ?config(nodes, Config),
    ?assertMatch({ok, _}, rpc:call(Node, dao_lib, apply, [dao_helper, list_dbs, [], 1])).

%% ====================================================================
%% SetUp and TearDown functions
%% ====================================================================

init_per_suite(Config) ->
    ?INIT_CODE_PATH,
    ?CREATE_DUMMY_AUTH,
    {Certs, CACertsDir, GRPCADir} = ?PREPARE_CERT_FILES(Config),

    DbNodesEnv = {db_nodes, [?DB_NODE]},
    Nodes = test_node_starter:start_test_nodes(1),
    test_node_starter:start_app_on_nodes(?APP_Name, ?GR_DEPS, Nodes,
        [[
            DbNodesEnv,
            ?cert_paths(Certs, CACertsDir, GRPCADir)
        ]]
    ),
    Config ++ [{nodes, Nodes}].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    test_node_starter:stop_app_on_nodes(?APP_Name, ?GR_DEPS, Nodes),
    test_node_starter:stop_test_nodes(Nodes).
