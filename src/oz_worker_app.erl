%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Applicatin main app file
%%% @end
%%%-------------------------------------------------------------------
-module(oz_worker_app).
-author("Tomasz Lichon").

-behaviour(application).

-include_lib("ctool/include/logging.hrl").
-include("registered_names.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    % DNS config needs to be loaded here as gui listener needs to access it.
    dns_query_handler:load_config(),
    test_node_starter:maybe_start_cover(),
    case application:start(cluster_worker, permanent) of
        ok ->
             oz_worker_sup:start_link();
        {error, Reason} ->
            {error, {cannot_start_worker_sup, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    auth_logic:stop(),
    test_node_starter:maybe_stop_cover(),
    ok.
