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
-include("testing/test_node_starter.hrl").

%% API
-export([start_globalregistry_node/3,start_globalregistry_node/4,stop_globalregistry_node/1,set_env_vars/1,start_deps/0,stop_deps/0]).


%% start_globalregistry_node/3
%% ====================================================================
%% @doc Starts new node with globalregistry, with silent mode
-spec start_globalregistry_node(NodeName :: atom(), Host :: atom(), EnvVars :: list(Env)) -> node() | no_return() when
	Env :: {Name,Value},
	Name :: atom(),
	Value :: term().
%% ====================================================================
start_globalregistry_node(NodeName,Host,EnvVars) ->
	start_globalregistry_node(NodeName,Host,EnvVars,false).

%% start_globalregistry_node/4
%% ====================================================================
%% @doc Starts new node with globalregistry.
-spec start_globalregistry_node(NodeName :: atom(), Host :: atom(),EnvVars :: list(Env), Verbose :: boolean()) -> Result when
	Env :: {Name,Value},
	Name :: atom(),
	Value :: term(),
	Result :: node() | no_return().
%% ====================================================================
start_globalregistry_node(NodeName,Host,EnvVars,Verbose) ->
	% Prepare opts
	CodePathOpt = make_code_path(),
	VerboseOpt = case Verbose of
					 true -> "";
					 false -> " -noshell "
	             end,
	CookieOpt = " -setcookie "++atom_to_list(erlang:get_cookie())++" ",

	% Start node
	stop_globalregistry_node(?NODE(Host,NodeName)),
	{ok,Node} = slave:start(Host, NodeName,CodePathOpt++VerboseOpt++CookieOpt),

	% Prepare environment
	rpc:call(Node,test_node_starter,start_deps,[]),
	rpc:call(Node,test_node_starter,set_env_vars,[EnvVars]),
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
	application:stop(mimetypes),
	application:stop(lager),
	application:stop(sasl),
	application:unload(?APP_Name).

%% start_deps/0
%% ====================================================================
%% @doc This function sets environment for application.
-spec start_deps() -> ok.
%% ====================================================================
start_deps() ->
	application:start(sasl),
	lager:start(),
	application:start(mimetypes),
	application:start(ranch),
	application:start(crypto),
	application:start(cowboy),
	application:load(?APP_Name).

%% set_env_vars/1
%% ====================================================================
%% @doc This function sets environment variables for application.
-spec set_env_vars(EnvVars :: list()) -> ok.
%% ====================================================================
set_env_vars([]) ->
	ok;
set_env_vars([{Variable, Value} | Vars]) ->
	application:set_env(?APP_Name, Variable, Value),
	set_env_vars(Vars).