%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Application main supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(oz_worker_sup).
-author("Tomasz Lichon").

-behaviour(supervisor).

-include("registered_names.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 1000, period => 3600}, [
        cluster_worker_specs:main_worker_sup_spec(),
        changes_bridge_spec()
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec changes_bridge_spec() -> supervisor:child_spec().
changes_bridge_spec() ->
    #{
        id => changes_bridge,
        start => {changes_bridge, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [changes_bridge]
    }.
