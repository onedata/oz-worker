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

%% node_manager_plugin_default callbacks
-export([app_name/0, cm_nodes/0, db_nodes/0]).
-export([listeners/0, modules_with_args/0]).
-export([before_init/1, on_cluster_initialized/1, after_init/1]).
-export([handle_call/3, handle_cast/2]).

-export([reconcile_dns_config/0]).

-type state() :: #state{}.

-define(DNS_UPDATE_RETRY_INTERVAL, 5000).

%%%===================================================================
%%% node_manager_plugin_default callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:app_name/0}.
%% @end
%%--------------------------------------------------------------------
-spec app_name() -> {ok, Name :: atom()}.
app_name() ->
    {ok, oz_worker}.

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:cm_nodes/0}.
%% @end
%%--------------------------------------------------------------------
-spec cm_nodes() -> {ok, Nodes :: [atom()]} | undefined.
cm_nodes() ->
    oz_worker:get_env(cm_nodes).

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:db_nodes/0}.
%% @end
%%--------------------------------------------------------------------
-spec db_nodes() -> {ok, Nodes :: [atom()]} | undefined.
db_nodes() ->
    oz_worker:get_env(db_nodes).

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
%% @doc
%% Overrides {@link node_manager_plugin_default:modules_with_args/0}.
%% @end
%%--------------------------------------------------------------------
-spec modules_with_args() -> Models :: [{atom(), [any()]}].
modules_with_args() ->
    [{gs_worker, [
        {supervisor_flags, gs_worker:supervisor_flags()}
    ]}].

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:before_init/1}.
%% @end
%%--------------------------------------------------------------------
-spec before_init(Args :: term()) -> Result :: ok | {error, Reason :: term()}.
before_init([]) ->
    try
        oz_worker_sup:start_link(),
        ok
    catch
        _:Error ->
            ?error_stacktrace("Error in node_manager_plugin:before_init: ~p",
                [Error]),
            {error, cannot_start_node_manager_plugin}
    end.

%%--------------------------------------------------------------------
%% @doc
%% This callback is executed when the cluster has been initialized, i.e. all
%% nodes have connected to cluster manager.
%% @end
%%--------------------------------------------------------------------
-spec on_cluster_initialized(Nodes :: [node()]) -> Result :: ok | {error, Reason :: term()}.
on_cluster_initialized(_Nodes) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:after_init/1}.
%% @end
%%--------------------------------------------------------------------
-spec after_init(Args :: term()) -> Result :: ok | {error, Reason :: term()}.
after_init([]) ->
    try
        auth_config:load_auth_config(),

        entity_graph:init_state(),

        % build dns zone on one node and broadcast to others
        case get_dns_dedicated_node() =:= node() of
            true -> ok = broadcast_dns_config();
            _ -> ok
        end,

        %% This code will be run on every node_manager, so we need a
        %% transaction here that will prevent duplicates.
        critical_section:run(create_predefined_groups, fun() ->
            group_logic:create_predefined_groups()
        end)
    catch
        _:Error ->
            ?error_stacktrace("Error in node_manager_plugin:after_init: ~p",
                [Error]),
            {error, cannot_start_node_manager_plugin}
    end.


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
    DedicatedNode = get_dns_dedicated_node(),
    gen_server2:cast({?NODE_MANAGER_NAME, DedicatedNode}, broadcast_dns_config).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get node responsible for building dns zone to prevent race conditions.
%% @end
%%--------------------------------------------------------------------
-spec get_dns_dedicated_node() -> node().
get_dns_dedicated_node() ->
    consistent_hasing:get_node(build_dns_zone).


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

        {ok, Nodes} = node_manager:get_cluster_nodes(),
        lists:map(fun(Node) ->
            case Node == node() of
                true -> ok = dns_config:insert_config(DnsConfig);
                false -> ok = gen_server2:call({?NODE_MANAGER_NAME, Node},
                                               {update_dns_config, DnsConfig},
                                               timer:seconds(30))
            end
        end, Nodes),
        ok
    catch
        Type:Message ->
            ?error_stacktrace(
                "Error sending dns zone update, scheduling retry after ~p seconds: ~p:~p",
                [?DNS_UPDATE_RETRY_INTERVAL div 1000, Type, Message]),
            erlang:send_after(?DNS_UPDATE_RETRY_INTERVAL, self(), {timer, broadcast_dns_config}),
            error
    end.
