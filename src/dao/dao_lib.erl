%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: DAO helper/utility functional methods. Those can be used in other modules
%% bypassing worker_host and gen_server.
%% @end
%% ===================================================================
-module(dao_lib).

-include("dao/dao.hrl").
-include("registered_names.hrl").

%% API
-export([apply/4, apply/5]).

%% ===================================================================
%% API functions
%% ===================================================================

%% apply/4
%% ====================================================================
%% @doc Same as apply/5 but with default Timeout
%% @end
-spec apply(Module :: module(), Method :: atom() | {synch, atom()} | {asynch, atom()},
    Args :: [term()], ProtocolVersion :: number()) -> any() | {error, worker_not_found}.
%% ====================================================================
apply(Module, Method, Args, ProtocolVersion) ->
    apply(Module, Method, Args, ProtocolVersion, ?DAO_REQUEST_TIMEOUT).

%% apply/5
%% ====================================================================
%% @doc Behaves similar to erlang:apply/3 but works only with DAO worker<br/>.
%% Method calls are made through random gen_server. <br/>
%% Method should be tuple {synch, Method} or {asynch, Method}<br/>
%% but if its simple atom(), {synch, Method} is assumed<br/>
%% Timeout argument defines how long should this method wait for response
%% @end
-spec apply(Module :: module(), Method :: atom() | {synch, atom()} | {asynch, atom()},
    Args :: [term()], ProtocolVersion :: number(), Timeout :: pos_integer()) -> any() | {error, timeout}.
%% ====================================================================
apply(Module, {Mode, Method}, Args, ProtocolVersion, _Timeout) ->
	try
		case Mode of
			synch ->
				gen_server:call(?Dao, {ProtocolVersion, Module, Method, Args});
			asynch ->
				spawn( fun() -> gen_server:call(?Dao, {ProtocolVersion, Module, Method, Args}) end),
				ok
		end
    catch
        Type:Error ->
            lager:error("Cannot make call to dao, Reason: ~p", [{Type, Error}]),
            {error, {Type, Error}}
    end;
apply(Module, Method, Args, ProtocolVersion, Timeout) ->
    apply(Module, {synch, Method}, Args, ProtocolVersion, Timeout).

%% ===================================================================
%% Internal functions
%% ===================================================================

