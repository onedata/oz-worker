%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% Functions used by ct test to start nodes for testing
%%% @end
%%% Created : 03. May 2014 8:26 PM
%%%-------------------------------------------------------------------
-module(test_node_starter).
-author("Tomasz Lichon").

-include("registered_names.hrl").

%% API
-export([start_globalregistry_node/2,stop_globalregistry_node/1,start_deps/0,stop_deps/0]).

%% start_globalregistry_node/2
%% ====================================================================
%% @doc Starts new node with globalregistry.
-spec start_globalregistry_node(NodeName :: atom(), Host :: atom()) -> node() | no_return().
%% ====================================================================
start_globalregistry_node(NodeName,Host) -> % todo add verbose and env setting options
	{ok,Node} = slave:start(Host, NodeName,make_code_path() ++ " -noshell"),
	rpc:call(Node,test_node_starter,start_deps,[]),
	rpc:call(Node,application,start,[?APP_Name]),
	Node.

%% stop_globalregistry_node/2
%% ====================================================================
%% @doc Stops globalregistry node.
-spec stop_globalregistry_node(Node :: node()) -> ok | no_return().
%% ====================================================================
stop_globalregistry_node(Node) ->
	rpc:call(Node,test_node_starter,stop_deps,[]),
	rpc:call(Node,application,stop,[?APP_Name]),
	slave:stop(Node).

%% make_code_path/0
%% ====================================================================
%% @doc Returns current code path string, formatted as erlang slave node argument.
%% @end
-spec make_code_path() -> string().
%% ====================================================================
make_code_path() ->
	lists:foldl(fun(Node, Path) -> " -pa " ++ Node ++ Path end,
		[], code:get_path()).

%% stop_deps/0
%% ====================================================================
%% @doc This function clears after the test.
-spec stop_deps() -> ok.
%% ====================================================================
stop_deps() ->
	application:stop(cowboy),
	application:stop(ranch),
	application:stop(crypto),
	application:stop(sasl).

%% start_deps/0
%% ====================================================================
%% @doc This function sets environment for application.
-spec start_deps() -> ok.
%% ====================================================================
start_deps() ->
	application:start(sasl),
	application:start(ranch),
	application:start(crypto),
	application:start(cowboy).