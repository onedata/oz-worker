%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Applicatin main app file
%% @end
%% ===================================================================
-module(globalregistry_app).
-author("Tomasz Lichon").

-behaviour(application).

%% Includes
-include("rest_config.hrl").
-include("gui_config.hrl").
-include("registered_names.hrl").

%% Application callbacks
-export([start/2,
	stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% start/2
%% ===================================================================
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
	StartArgs :: term()) ->
	{ok, pid()} |
	{ok, pid(), State :: term()} |
	{error, Reason :: term()}).
%% ===================================================================
start(_StartType, _StartArgs) ->
	start_rest(),
	start_n2o(),
	case globalregistry_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.

%% stop/1
%% ===================================================================
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
-spec(stop(State :: term()) -> term()).
%% ===================================================================
stop(_State) ->
	cowboy:stop_listener(?rest_listener),
	cowboy:stop_listener(?gui_https_listener),
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% start_rest/0
%% ===================================================================
%% @doc Starts cowboy with rest api
-spec start_rest() -> {ok,pid()}.
%% ===================================================================
start_rest() ->
  % Get rest config
  {ok,RestPort} = application:get_env(?APP_Name,rest_port),
  {ok,RestHttpsAcceptors} = application:get_env(?APP_Name,rest_https_acceptors),

  % Get cert paths
  {ok,CaCertFile} = application:get_env(?APP_Name,ca_cert_file),
  {ok,CertFile} = application:get_env(?APP_Name,cert_file),
  {ok,KeyFile} = application:get_env(?APP_Name,key_file),

  Dispatch = cowboy_router:compile([
    {'_', lists:append([
      user_rest_module:routes(),
      provider_rest_module:routes(),
      spaces_rest_module:routes(),
      groups_rest_module:routes()
    ])}
  ]),
  {ok, _Ans} = cowboy:start_https(?rest_listener, RestHttpsAcceptors,
    [
      {port, RestPort},
      {cacertfile, CaCertFile},
      {certfile, CertFile},
      {keyfile, KeyFile}
    ],
    [
      {env, [{dispatch, Dispatch}]}
    ]).

%% start_n2o/0
%% ===================================================================
%% @doc Starts n2o server
-spec start_n2o() -> {ok,pid()}.
%% ===================================================================
start_n2o() ->
  % Get gui config
  {ok,GuiPort} = application:get_env(?APP_Name,gui_port),
  {ok,GuiHttpsAcceptors} = application:get_env(?APP_Name,gui_https_acceptors),
  {ok,GuiSocketTimeout} = application:get_env(?APP_Name,gui_socket_timeout),
  {ok,GuiMaxKeepalive} = application:get_env(?APP_Name,gui_max_keepalive),
  % Get cert paths
  {ok,CaCertFile} = application:get_env(?APP_Name,ca_cert_file),
  {ok,CertFile} = application:get_env(?APP_Name,cert_file),
  {ok,KeyFile} = application:get_env(?APP_Name,key_file),

	% Set envs needed by n2o
	% Port - gui port
	ok = application:set_env(n2o, port, GuiPort),
	% Transition port - the same as gui port
	ok = application:set_env(n2o, transition_port, GuiPort),
	% Custom route handler
	ok = application:set_env(n2o, route, gui_routes),

	Dispatch = cowboy_router:compile(
		[{'_',
				static_dispatches(?gui_static_root, ?static_paths) ++ [
				{"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]},
				{'_', n2o_cowboy, []}
			]}
		]),

	{ok, _} = cowboy:start_https(?gui_https_listener, GuiHttpsAcceptors,
		[
			{port, GuiPort},
			{cacertfile, CaCertFile},
			{certfile, CertFile},
			{keyfile, KeyFile}
		],
		[
			{env, [{dispatch, Dispatch}]},
			{max_keepalive, GuiMaxKeepalive},
			{timeout, GuiSocketTimeout}
		]).


%% static_dispatches/2
%% ===================================================================
%% @doc Generates static file routing for cowboy.
-spec static_dispatches(DocRoot :: string(),StaticPaths :: list(string())) -> {ok,pid()}.
%% ===================================================================
static_dispatches(DocRoot, StaticPaths) ->
	_StaticDispatches = lists:map(fun(Dir) ->
		Opts = [
			{mimetypes, {fun mimetypes:path_to_mimes/2, default}},
			{directory, DocRoot ++ Dir}
		],
		{Dir ++ "[...]", cowboy_static, Opts}
	end, StaticPaths).
