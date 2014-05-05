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
-include_lib("common_test/include/ct.hrl").
-include("registered_names.hrl").
-include("testing/test_node_starter.hrl").
-include("testing/assertions.hrl").

%% API
-export([all/0,init_per_suite/1,end_per_suite/1]).
-export([gen_server_connection_test/1,rest_api_connection_test/1]).

all() -> [gen_server_connection_test,rest_api_connection_test].


gen_server_connection_test(Config) ->
	Node = ?config(node,Config),
	?assertEqual(pong, gen_server:call({?Global_Registry,Node},ping)).

rest_api_connection_test(_Config) ->
	ibrowse:start(),
 	?assertMatch({ok,"200",_,"<html>REST Hello World as HTML!</html>"}, ibrowse:send_req("http://127.0.0.1:8080/hello_world",[],get)),
	ibrowse:stop().

%% ====================================================================
%% SetUp and TearDown functions
%% ====================================================================

init_per_suite(Config) ->
	?INIT_CODE_PATH,
	Node = test_node_starter:start_globalregistry_node(globalregistry_test_node,?CURRENT_HOST,false),
	Config ++ [{node,Node}].

end_per_suite(Config) ->
	Node = ?config(node,Config),
	test_node_starter:stop_globalregistry_node(Node).