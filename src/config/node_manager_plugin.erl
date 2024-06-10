%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin which extends node manager for op_worker
%%% @end
%%%-------------------------------------------------------------------
-module(node_manager_plugin).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include_lib("cluster_worker/include/elements/node_manager/node_manager.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/global_definitions.hrl").
-include_lib("ctool/include/onedata.hrl").

%% node_manager_plugin_default callbacks
-export([cluster_generations/0]).
-export([oldest_upgradable_cluster_generation/0]).
-export([app_name/0, release_version/0, build_version/0]).
-export([cm_nodes/0, db_nodes/0]).
-export([before_init/0]).
-export([before_cluster_upgrade/0]).
-export([upgrade_cluster/1]).
-export([custom_workers/0]).
-export([before_listeners_start/0]).
-export([listeners/0]).
-export([handle_call/3, handle_cast/2]).

-export([reconcile_dns_config/0]).

-type state() :: #state{}.

-define(DNS_UPDATE_RETRY_INTERVAL, 5000).

% List of all known cluster generations.
% When cluster is not in newest generation it will be upgraded during initialization.
% This can be used to e.g. move models between services.
% Oldest upgradable generation is the lowest one that can be directly upgraded to newest.
% Human readable version is included to for logging purposes.
-define(CLUSTER_GENERATIONS, [
    {1, ?LINE_19_02},
    {2, ?LINE_20_02},
    {3, ?LINE_21_02(<<"4">>)},
    {4, oz_worker:get_release_version()}
]).
-define(OLDEST_UPGRADABLE_CLUSTER_GENERATION, 2).

%%%===================================================================
%%% node_manager_plugin_default callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:cluster_generations/0}.
%% @end
%%--------------------------------------------------------------------
-spec cluster_generations() ->
    [{node_manager:cluster_generation(), onedata:release_version()}].
cluster_generations() ->
    ?CLUSTER_GENERATIONS.

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:oldest_upgradable_cluster_generation/0}.
%% @end
%%--------------------------------------------------------------------
-spec oldest_upgradable_cluster_generation() ->
    node_manager:cluster_generation().
oldest_upgradable_cluster_generation() ->
    ?OLDEST_UPGRADABLE_CLUSTER_GENERATION.

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:app_name/0}.
%% @end
%%--------------------------------------------------------------------
-spec app_name() -> atom().
app_name() ->
    oz_worker.

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:release_version/0}.
%% @end
%%--------------------------------------------------------------------
-spec release_version() -> string() | binary().
release_version() ->
    oz_worker:get_release_version().

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:app_name/0}.
%% @end
%%--------------------------------------------------------------------
-spec build_version() -> string() | binary().
build_version() ->
    oz_worker:get_build_version().

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:cm_nodes/0}.
%% @end
%%--------------------------------------------------------------------
-spec cm_nodes() -> {ok, Nodes :: [atom()]}.
cm_nodes() ->
    {ok, oz_worker:get_env(cm_nodes)}.

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:db_nodes/0}.
%% @end
%%--------------------------------------------------------------------
-spec db_nodes() -> {ok, Nodes :: [atom()]}.
db_nodes() ->
    {ok, oz_worker:get_env(db_nodes)}.

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:before_init/0}.
%% This callback is executed on all cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec before_init() -> Result :: ok | {error, Reason :: term()}.
before_init() ->
    try
        oz_worker_sup:start_link(),
        ok
    catch
        Class:Reason:Stacktrace ->
            ?error_exception(Class, Reason, Stacktrace),
            {error, cannot_start_node_manager_plugin}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Callback executed before cluster upgrade so that any required preparation
%% can be done.
%% @end
%%--------------------------------------------------------------------
-spec before_cluster_upgrade() -> ok.
before_cluster_upgrade() -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:upgrade_cluster/1}.
%% This callback is executed only on one cluster node.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_cluster(node_manager:cluster_generation()) ->
    {ok, node_manager:cluster_generation()}.
upgrade_cluster(2) ->
    % no model upgrades are required, but a new version must be
    % forced along with new major release version
    {ok, 3};
upgrade_cluster(3) ->
    od_handle:migrate_legacy_handles(),
    {ok, 4}.

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:custom_workers/0}.
%% @end
%%--------------------------------------------------------------------
-spec custom_workers() -> Models :: [{atom(), [any()]}].
custom_workers() ->
    [{gs_worker, [
        {supervisor_flags, gs_worker:supervisor_flags()}
    ]}].

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:before_listeners_start/0}.
%%
%% NOTE: this callback blocks the application supervisor and must not be used to
%% interact with the main supervision tree.
%%
%% This callback is executed on all cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec before_listeners_start() -> ok | {error, Reason :: term()}.
before_listeners_start() ->
    try
        % Logic that should be run on every node of the cluster
        onezone_plugins:init(),

        % Logic that should be run on a single node
        case is_dedicated_node(cluster_setup) of
            false ->
                ok;
            true ->
                cluster_logic:set_up_oz_worker_service(),
                harvester_logic:deploy_default_gui_package(),
                entity_graph:init_state(),
                broadcast_dns_config(),
                group_logic:ensure_predefined_groups()
        end
    catch
        Class:Reason:Stacktrace ->
            ?error_exception(Class, Reason, Stacktrace),
            {error, cannot_start_node_manager_plugin}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:listeners/0}.
%% @end
%%--------------------------------------------------------------------
-spec listeners() -> Listeners :: [atom()].
listeners() -> [
    http_listener,
    https_listener
].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Overrides {@link node_manager_plugin_default:handle_call/3}.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {reply, Reply :: term(), NewState :: state(), timeout() | hibernate} |
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
    {stop, Reason :: term(), NewState :: state()}.
handle_call({update_dns_config, DnsZone}, _From, State) ->
    Result = dns_config:insert_config(DnsZone),
    {reply, Result, State};
handle_call(Request, _From, State) ->
    ?log_bad_request(Request),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Overrides {@link node_manager_plugin_default:handle_cast/2}.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}.
handle_cast(broadcast_dns_config, State) ->
    broadcast_dns_config(),
    {noreply, State};
handle_cast(Request, State) ->
    ?log_bad_request(Request),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @doc
%% Trigger broadcasting dns update from this node
%% @end
%%--------------------------------------------------------------------
-spec reconcile_dns_config() -> ok.
reconcile_dns_config() ->
    gen_server2:cast({?NODE_MANAGER_NAME, dedicated_node(dns)}, broadcast_dns_config).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if the current node is dedicated to perform the operation symbolized
%% by given (arbitrary) identifier. Used to prevent race conditions by choosing
%% one cluster node for the operation.
%% @end
%%--------------------------------------------------------------------
-spec is_dedicated_node(Identifier :: atom()) -> boolean().
is_dedicated_node(Identifier) ->
    node() =:= dedicated_node(Identifier).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the dedicated node to perform the operation symbolized by given
%% (arbitrary) identifier.
%% @end
%%--------------------------------------------------------------------
-spec dedicated_node(Identifier :: atom()) -> node().
dedicated_node(Identifier) ->
    consistent_hashing:get_assigned_node(Identifier).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds up-to-date dns zone and broadcasts it to all nodes in the cluster.
%% Returns error and schedules a retry if inserting zone does not succeed
%% on all nodes.
%% @end
%%--------------------------------------------------------------------
-spec broadcast_dns_config() -> ok | error.
broadcast_dns_config() ->
    try
        DnsConfig = dns_config:build_config(),

        lists:map(fun(Node) ->
            case Node == node() of
                true -> ok = dns_config:insert_config(DnsConfig);
                false -> ok = gen_server2:call({?NODE_MANAGER_NAME, Node},
                    {update_dns_config, DnsConfig},
                    timer:seconds(30))
            end
        end, consistent_hashing:get_all_nodes()),
        ok
    catch
        Class:Reason:Stacktrace ->
            ?error_exception(
                "Error sending dns zone update, scheduling retry after ~p seconds",
                [?DNS_UPDATE_RETRY_INTERVAL div 1000],
                Class, Reason, Stacktrace
            ),
            erlang:send_after(?DNS_UPDATE_RETRY_INTERVAL, self(), {timer, broadcast_dns_config}),
            error
    end.
