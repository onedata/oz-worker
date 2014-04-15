%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2014 5:55 PM
%%%-------------------------------------------------------------------
-module(globalregistry_app).
-author("Tomasz Lichon").

-behaviour(application).

%% Includes
-include("rest_config.hrl").

%% Application callbacks
-export([start/2,
	stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
	StartArgs :: term()) ->
	{ok, pid()} |
	{ok, pid(), State :: term()} |
	{error, Reason :: term()}).
start(_StartType, _StartArgs) ->
	start_cowboy(),
	case globalregistry_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_cowboy() ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{?TEST_URL, hello_world, []}
		]}
	]),
	{ok, _} = cowboy:start_http(?TEST_REF, ?HTTP_ACCEPTORS, [{port, ?REST_PORT}], [
		{env, [{dispatch, Dispatch}]}
	]).