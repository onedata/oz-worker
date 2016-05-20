%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This worker periodically checks what group changed and refreshes
%%% effective users and groups across all the group graph and related users.
%%% @end
%%%-------------------------------------------------------------------
-module(groups_graph_caches_worker).
-author("Michal Zmuda").

-behaviour(worker_plugin_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% worker_plugin_behaviour callbacks
-export([init/1, handle/1, cleanup/0]).

-define(STATE_KEY, <<"groups_graph_caches_state">>).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialises the worker and schedules stream presence checks.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.
init(_Args) ->
    schedule_graph_refresh(),
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @doc
%% Handles requests.
%% @end
%%--------------------------------------------------------------------
-spec handle(Request :: term()) -> ok | {error, Reason :: term()} | no_return().
handle(healthcheck) ->
    case groups_graph_caches_state:get(?STATE_KEY) of
        {ok, #document{value = #groups_graph_caches_state{last_rebuild = Last}}} ->
            HighTimeForUpdate = Last + (3 * refresh_interval()),
            case erlang:system_time() > HighTimeForUpdate of
                true -> {error, no_recent_updates};
                false -> ok
            end,
            ok;
        _ -> {error, state_not_accessible}
    end;

handle(refresh_group_graph) ->
    ?debug("Refreshing effective in group graph"),
    group_graph:refresh_effective_caches();

handle(_Request) ->
    ?log_bad_request(_Request).

%%--------------------------------------------------------------------
%% @doc
%% Cleans up the worker.
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Schedules group graph refresh.
%% @end
%%--------------------------------------------------------------------
-spec schedule_graph_refresh() -> ok.
schedule_graph_refresh() ->
    {ok, _} = timer:send_interval(refresh_interval(), whereis(?MODULE),
        {timer, refresh_group_graph}),
    ok.

-spec refresh_interval() -> integer().
refresh_interval() ->
    {ok, Interval} = application:get_env(?APP_Name, group_graph_refresh_interval),
    Interval.