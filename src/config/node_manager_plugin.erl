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
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/global_definitions.hrl").

%% node_manager_plugin_default callbacks
-export([app_name/0, cm_nodes/0, db_nodes/0]).
-export([listeners/0, modules_with_args/0]).
-export([before_init/1, on_cluster_initialized/0, after_init/1]).
-export([check_node_ip_address/0]).

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
    application:get_env(?APP_NAME, cm_nodes).

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:db_nodes/0}.
%% @end
%%--------------------------------------------------------------------
-spec db_nodes() -> {ok, Nodes :: [atom()]} | undefined.
db_nodes() ->
    application:get_env(?APP_NAME, db_nodes).

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:listeners/0}.
%% @end
%%--------------------------------------------------------------------
-spec listeners() -> Listeners :: [atom()].
listeners() -> [
    oz_redirector_listener,
    rest_listener,
    gui_listener |
        node_manager:cluster_worker_listeners() -- [redirector_listener]
].

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:modules_with_args/0}.
%% @end
%%--------------------------------------------------------------------
-spec modules_with_args() -> Models :: [{atom(), [any()]}].
modules_with_args() ->
    Base = [
        {singleton, ozpca_worker, [
            {supervisor_flags, ozpca_worker:supervisor_flags()},
            {supervisor_children_spec, [ozpca_worker:supervisor_children_spec()]}
        ]},
        {gs_worker, [
            {supervisor_flags, gs_worker:supervisor_flags()}
        ]}
    ],
    case application:get_env(?APP_NAME, location_service_enabled) of
        {ok, false} -> Base;
        {ok, true} -> Base ++ [
            {location_service_worker, []},
            {identity_publisher_worker, []}
        ]
    end.

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:before_init/1}.
%% @end
%%--------------------------------------------------------------------
-spec before_init(Args :: term()) -> Result :: ok | {error, Reason :: term()}.
before_init([]) ->
    try
        dns_query_handler:load_config(),
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
-spec on_cluster_initialized() -> Result :: ok | {error, Reason :: term()}.
on_cluster_initialized() ->
    ozpca:ensure_oz_ca_cert_present(),
    maybe_generate_web_cert().

%%--------------------------------------------------------------------
%% @doc
%% Overrides {@link node_manager_plugin_default:after_init/1}.
%% @end
%%--------------------------------------------------------------------
-spec after_init(Args :: term()) -> Result :: ok | {error, Reason :: term()}.
after_init([]) ->
    try
        %% This cannot be started before all workers are up
        %% and critical section is running
        %% todo: once critical section works in worker init, move it there
        case application:get_env(?APP_NAME, location_service_enabled) of
            {ok, false} ->
                ok;
            {ok, true} ->
                identity_publisher_worker:start_refreshing()
        end,

        entity_graph:init_state(),

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
%% Overrides {@link node_manager_plugin_default:check_node_ip_address/0}.
%% @end
%%--------------------------------------------------------------------
-spec check_node_ip_address() -> IPV4Addr :: {A :: byte(), B :: byte(), C :: byte(), D :: byte()}.
check_node_ip_address() ->
    case application:get_env(?APP_NAME, external_ip, undefined) of
        undefined ->
            ?alert_stacktrace("Cannot check external IP of node, defaulting to 127.0.0.1"),
            {127, 0, 0, 1};
        Ip ->
            {ok, Address} = inet_parse:ipv4_address(str_utils:to_list(Ip)),
            ?info("External IP: ~p", [Address]),
            Address
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates a new test web server cert if none is found under expected path,
%% given that this option is enabled in env config. The new cert is then
%% distributed among cluster nodes. The procedure is run within critical section
%% to avoid race conditions across multiple nodes.
%% @end
%%--------------------------------------------------------------------
-spec maybe_generate_web_cert() -> ok.
maybe_generate_web_cert() ->
    critical_section:run(oz_web_cert, fun maybe_generate_web_cert_unsafe/0).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates a new test web server cert if none is found under expected path,
%% given that this option is enabled in env config. The new cert is then
%% distributed among cluster nodes.
%% Should not be called in parallel to prevent race conditions.
%% @end
%%--------------------------------------------------------------------
-spec maybe_generate_web_cert_unsafe() -> ok.
maybe_generate_web_cert_unsafe() ->
    GenerateIfAbsent = application:get_env(
        ?APP_NAME, generate_web_cert_if_absent, false
    ),
    {ok, WebKeyPath} = application:get_env(?APP_NAME, web_key_file),
    {ok, WebCertPath} = application:get_env(?APP_NAME, web_cert_file),
    CertExists = filelib:is_regular(WebKeyPath) andalso
        filelib:is_regular(WebCertPath),
    case GenerateIfAbsent andalso not CertExists of
        false ->
            ok;
        true ->
            % Both key and cert are expected in the same file
            {ok, CAPath} = application:get_env(?APP_NAME, test_web_cert_ca_path),
            {ok, Hostname} = application:get_env(?APP_NAME, http_domain),
            cert_utils:create_signed_webcert(
                WebKeyPath, WebCertPath, Hostname, CAPath, CAPath
            ),
            ?warning(
                "Web server cert not found (~s). Generated a new cert for "
                "hostname '~s'. Use only for test purposes.",
                [WebCertPath, Hostname]
            ),
            NodeList = gen_server2:call({global, ?CLUSTER_MANAGER}, get_nodes),
            OtherWorkers = NodeList -- [node()],
            {ok, Key} = file:read_file(WebKeyPath),
            {ok, Cert} = file:read_file(WebCertPath),
            ok = utils:save_file_on_hosts(OtherWorkers, WebKeyPath, Key),
            ok = utils:save_file_on_hosts(OtherWorkers, WebCertPath, Cert),
            ?info("Synchronized the new web server cert across all nodes")
    end.
