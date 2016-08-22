%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(oai_test_SUITE).
-author("Jakub Kudzia").

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1, oai_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([oai_test]).

oai_test(Config) ->
    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    NewConfig.

init_per_testcase(_, Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    tracer:start(Node),
    tracer:trace_calls(node_manager_plugin, listeners),
    tracer:trace_calls(oz_redirector_listener),
    tracer:trace_calls(redirector_handler, handle),
    Config.

end_per_testcase(_, _Config) ->
    ok.

end_per_suite(Config) ->
    ok.
%%    test_node_starter:clean_environment(Config).