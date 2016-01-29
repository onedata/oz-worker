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
-module(globalregistry_sup).
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
        dao_spec(),
        op_channel_spec()
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a worker child_spec for dao.
%% @end
%%--------------------------------------------------------------------
-spec dao_spec() -> supervisor:child_spec().
dao_spec() ->
    #{
        id => ?Dao,
        start => {dao_worker, start_link, []},
        restart => permanent,
        shutdown => timer:seconds(2),
        type => worker,
        modules => [dao_worker]
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a worker child_spec for provider channel.
%% @end
%%--------------------------------------------------------------------
-spec op_channel_spec() -> supervisor:child_spec().
op_channel_spec() ->
    #{
        id => ?OpChannel,
        start => {op_channel, start_link, []},
        restart => permanent,
        shutdown => timer:seconds(2),
        type => worker,
        modules => [op_channel]
    }.