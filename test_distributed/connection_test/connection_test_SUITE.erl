%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% Connection test suite
%%% @end
%%% Created : 29. Apr 2014 2:53 PM
%%%-------------------------------------------------------------------
-module(connection_test_SUITE).
-author("Tomasz Lichon").

%% Includes
-include("registered_names.hrl").
-include("testing/test_utils.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ctool/include/test/test_node_starter.hrl").
-include_lib("ctool/include/test/assertions.hrl").

%% API
-export([all/0,init_per_suite/1,end_per_suite/1]).
-export([gen_server_connection_test/1,rest_api_connection_test/1,dao_connection_test/1]).

all() -> [gen_server_connection_test,rest_api_connection_test,dao_connection_test].


gen_server_connection_test(Config) ->
	[Node] = ?config(nodes,Config),
	?assertEqual(pong, gen_server:call({?Global_Registry,Node},ping)).

rest_api_connection_test(_Config) ->
	ibrowse:start(),
	ssl:start(),
	Ans = ibrowse:send_req("https://127.0.0.1:8080/hello_world",[],get),
	?assertMatch({ok,"200",_,"<html>REST Hello World as HTML!</html>"}, Ans),
	ssl:stop(),
	ibrowse:stop().

dao_connection_test(Config) ->
	[Node] = ?config(nodes,Config),
	?assertMatch({ok,_},rpc:call(Node,dao_lib,apply,[dao_helper,list_dbs,[],1])).

%% ====================================================================
%% SetUp and TearDown functions
%% ====================================================================

init_per_suite(Config) ->
	?INIT_CODE_PATH,
	DbNodesEnv = {db_nodes,[?DB_NODE]},
    Nodes = test_node_starter:start_test_nodes(1),
	test_node_starter:start_app_on_nodes(?APP_Name,?GR_DEPS,Nodes,
		[[
			DbNodesEnv,
			{ca_cert_file,"../../../cacerts/ca.crt"},
			{cert_file,"../../../cacerts/server.crt"},
	 		{key_file,"../../../cacerts/server.key"}
		]]
	),
	Config ++ [{nodes,Nodes}].

end_per_suite(Config) ->
	Nodes = ?config(nodes,Config),
	test_node_starter:stop_app_on_nodes(?APP_Name,?GR_DEPS,Nodes),
	test_node_starter:stop_test_nodes(Nodes).
