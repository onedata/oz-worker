%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2013 ACK CYFRONET AGH
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

-behaviour(node_manager_plugin_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("cluster_worker/include/elements/node_manager/node_manager.hrl").
-include_lib("cluster_worker/include/elements/worker_host/worker_protocol.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/global_definitions.hrl").

%% node_manager_plugin_behaviour callbacks
-export([before_init/1, after_init/1, on_terminate/2, on_code_change/3,
    handle_call_extension/3, handle_cast_extension/2, handle_info_extension/2,
    modules_with_args/0, listeners/0, cm_nodes/0, db_nodes/0,
    check_node_ip_address/0, app_name/0, clear_memory/1]).

%%%===================================================================
%%% node_manager_plugin_behaviour callbacks
%%%===================================================================

%% @doc
%% Returns the name of the application that bases on cluster worker.
%% @end
%%--------------------------------------------------------------------
-spec app_name() -> {ok, Name :: atom()}.
app_name() ->
    {ok, oz_worker}.

%%--------------------------------------------------------------------
%% @doc
%% List ccm nodes to be used by node manager.
%% @end
%%--------------------------------------------------------------------
-spec cm_nodes() -> {ok, Nodes :: [atom()]} | undefined.
cm_nodes() ->
    application:get_env(?APP_Name, cm_nodes).

%%--------------------------------------------------------------------
%% @doc
%% List db nodes to be used by node manager.
%% @end
%%--------------------------------------------------------------------
-spec db_nodes() -> {ok, Nodes :: [atom()]} | undefined.
db_nodes() ->
    application:get_env(?APP_Name, db_nodes).

%%--------------------------------------------------------------------
%% @doc
%% {@link node_manager_plugin_behaviour} callback listeners/0.
%% @end
%%--------------------------------------------------------------------
-spec listeners() -> Listeners :: [atom()].
listeners() -> node_manager:cluster_worker_listeners() ++ [
    subscriptions_wss_listener,
    rest_listener,
    gui_listener
].

%%--------------------------------------------------------------------
%% @doc
%% {@link node_manager_plugin_behaviour} callback modules_with_args/0.
%% @end
%%--------------------------------------------------------------------
-spec modules_with_args() -> Models :: [{atom(), [any()]}].
modules_with_args() ->
    Base = node_manager:cluster_worker_modules() ++ [
        {groups_graph_caches_worker, []},
        {changes_worker, []},
        {ozpca_worker, []},
        {subscriptions_worker, []}
    ],
    case application:get_env(?APP_Name, location_service_enabled) of
        {ok, false} -> Base;
        {ok, true} -> Base ++ [
            {location_service_worker, []},
            {identity_publisher_worker, []}
        ]
    end.

%%--------------------------------------------------------------------
%% @doc
%% {@link node_manager_plugin_behaviour} callback before_init/0.
%% @end
%%--------------------------------------------------------------------
-spec before_init(Args :: term()) -> Result :: ok | {error, Reason :: term()}.
before_init([]) ->
    try
        ok
    catch
        _:Error ->
            ?error_stacktrace("Error in node_manager_plugin:before_init: ~p",
                [Error]),
            {error, cannot_start_node_manager_plugin}
    end.

%%--------------------------------------------------------------------
%% @doc
%% {@link node_manager_plugin_behaviour} callback after_init/0.
%% @end
%%--------------------------------------------------------------------
-spec after_init(Args :: term()) -> Result :: ok | {error, Reason :: term()}.
after_init([]) ->
    try
        %% This cannot be started before all workers are up
        %% and critical section is running
        %% todo: once critical section works in worker init, move it there
        identity_publisher_worker:start_refreshing(),

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
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call_extension(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
    | {reply, Reply, NewState, Timeout}
    | {reply, Reply, NewState, hibernate}
    | {noreply, NewState}
    | {noreply, NewState, Timeout}
    | {noreply, NewState, hibernate}
    | {stop, Reason, Reply, NewState}
    | {stop, Reason, NewState},
    Reply :: nagios_handler:healthcheck_response() | term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().

handle_call_extension(_Request, _From, State) ->
    ?log_bad_request(_Request),
    {reply, wrong_request, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast_extension(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
    | {noreply, NewState, Timeout}
    | {noreply, NewState, hibernate}
    | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.

handle_cast_extension(_Request, State) ->
    ?log_bad_request(_Request),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info_extension(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
    | {noreply, NewState, Timeout}
    | {noreply, NewState, hibernate}
    | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.

handle_info_extension(_Request, State) ->
    ?log_bad_request(_Request),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec on_terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
    | shutdown
    | {shutdown, term()}
    | term().
on_terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec on_code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
on_code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% Get node IP. If there is no IP info in the env, then use 127.0.0.1.
%% @end
%%--------------------------------------------------------------------
-spec check_node_ip_address() -> IPV4Addr :: {A :: byte(), B :: byte(), C :: byte(), D :: byte()}.
check_node_ip_address() ->
    case application:get_env(?APP_Name, external_ip, undefined) of
        undefined ->
            ?alert_stacktrace("Cannot check external IP of node, defaulting to 127.0.0.1"),
            {127, 0, 0, 1};
        Ip ->
            {ok, Address} = inet_parse:ipv4_address(str_utils:to_list(Ip)),
            ?info("External IP: ~p", [Address]),
            Address
    end.

%%--------------------------------------------------------------------
%% @doc
%% Clears memory when node_manager sees that usage of memory is too high.
%% @end
%%--------------------------------------------------------------------
-spec clear_memory(HighMemUse :: boolean()) -> ok.
clear_memory(_) ->
    ok.
