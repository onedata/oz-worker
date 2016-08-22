%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This worker controls lifetime of Location Service client and provides
%%% basic API for Location Service.
%%% @end
%%%-------------------------------------------------------------------
-module(dht_worker).
-author("Michal Zmuda").

-behaviour(worker_plugin_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("cluster_worker/include/global_definitions.hrl").

%% worker_plugin_behaviour callbacks
-export([init/1, handle/1, cleanup/0]).

%% API
-export([get_value/1, set_value/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves value for the given key from the Location Service.
%% Returns error on REST request failure or tuple with result of
%% the operation itself.
%% @end
%%--------------------------------------------------------------------
-spec get_value(ID :: binary()) -> {error, term()} | {ok, Status, Value, Message} when
    Status :: ErrorStatus :: atom() | 'OK',
    Value :: [{binary(), term()}],
    Message :: binary().
get_value(ID) ->
    case http_client:request(get, get_dht_endpoint(ID)) of
        {ok, 200, _, ResponseBody} ->
            Response = json_utils:decode(ResponseBody),
            Value = proplists:get_value(<<"value">>, Response, undefined),
            Message = proplists:get_value(<<"message">>, Response, undefined),
            StatusBin = proplists:get_value(<<"status">>, Response, <<"empty_response">>),
            {ok, binary_to_atom(StatusBin, latin1), Value, Message};
        {error, _Term} ->
            ?warning("Get request (for ~p) failed due to ~p", [ID, _Term]),
            {error, request_failed}
    end.
%%--------------------------------------------------------------------
%% @doc
%% Saves value for the given key in the Location Service.
%% Returns error on REST request failure or tuple with result of
%% the operation itself.
%% @end
%%--------------------------------------------------------------------
-spec set_value(ID :: binary(), Value :: term()) -> {error, term()} | {ok, Status, Message} when
    Status :: ErrorStatus :: atom() | 'OK',
    Message :: binary().
set_value(ID, Value) ->
    Body = json_utils:encode({struct, [{value, Value}]}),
    Headers = [{<<"content-type">>, <<"application/json">>}],
    case http_client:request(post, get_dht_endpoint(ID), Headers, Body) of
        {ok, 200, _, ResponseBody} ->
            Response = json_utils:decode(ResponseBody),
            Message = proplists:get_value(<<"message">>, Response, undefined),
            StatusBin = proplists:get_value(<<"status">>, Response, <<"empty_response">>),
            {ok, binary_to_atom(StatusBin, latin1), Message};
        {error, _Term} ->
            ?warning("Post request (id: ~p, value ~p) failed due to ~p", [ID, Value, _Term]),
            {error, request_failed}
    end.

%%%===================================================================
%%% worker_plugin_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialises the worker and ensures cache is available.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.
init(_Args) ->
    {ok, Host} = application:get_env(oz_worker, http_domain),
    {ok, ServicePort} = application:get_env(?APP_Name, location_service_port),
    {ok, RestPort} = application:get_env(?APP_Name, location_service_rest_port),
    {ok, Nodes} = application:get_env(?APP_Name, location_service_bootstrap_nodes),
    {ok, [DBNode | _]} = application:get_env(?APP_Name, db_nodes),
    BootstrapArgs = lists:append([" -b " ++ atom_to_list(Node) || Node <- Nodes]),

    Pid = spawn_link(
        fun() ->
            process_flag(trap_exit, true),

            Command = "node nodejs/location-service/start.js"
                ++ " -h " ++ Host
                ++ " -p " ++ integer_to_list(ServicePort)
                ++ " -r " ++ integer_to_list(RestPort)
                ++ " -c " ++ hd(string:tokens(atom_to_list(DBNode), ":"))
                ++ " -vv"
                ++ BootstrapArgs,

            ?info("Running DHT port by command ~p", [Command]),
            Port = erlang:open_port({spawn, Command}, [{line, 1000}, stderr_to_stdout]),
            ?info("DHT port started"),
            handle_port(Port)
        end),
    {ok, #{handler => Pid}}.

%%--------------------------------------------------------------------
%% @doc
%% Handles various requests connected with subscriptions.
%% @end
%%--------------------------------------------------------------------
-spec handle(Request :: term()) -> ok | {error, Reason :: term()} | no_return().
handle(healthcheck) ->
    ok;

handle({'EXIT', Source, Reason}) ->
    Handler = worker_host:state_get(?MODULE, handler),
    case erlang:process_info(Handler) of
        undefined ->
            ?info("Port handler exited with reason ~p - restarting the worker", [Reason]),
            supervisor:terminate_child(?MAIN_WORKER_SUPERVISOR_NAME, ?MODULE),
            supervisor:restart_child(?MAIN_WORKER_SUPERVISOR_NAME, ?MODULE);
        _ ->
            ?warning("Ignoring exit of ~p with reason ~p as handler is ok", [Source, Reason])
    end;

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
%% @doc @private
%% Handles messages form port. Also handles exits.
%% @end
%%--------------------------------------------------------------------
-spec handle_port(Port :: port()) -> no_return().
handle_port(Port) ->
    receive
        {'EXIT', Port, Reason} ->
            exit({port_terminated, Reason});
        {Port, {data, {eol, Msg}}} ->
            process_log(Msg),
            handle_port(Port)
    end.

%%--------------------------------------------------------------------
%% @doc @private
%% Handles DHT output, which consists of log entries. The log entries
%% are dispatched to the OZ logger. Log entry has the following form:
%% <node id>: {<log level>} Actual message.
%% @end
%%--------------------------------------------------------------------
-spec process_log(string()) -> ok.
process_log(Msg) ->
    ?info(Msg).

%%--------------------------------------------------------------------
%% @doc @private
%% Returns address of DHT endpoint corresponding to given ID.
%% @end
%%--------------------------------------------------------------------
-spec get_dht_endpoint(ID :: binary()) -> binary().
get_dht_endpoint(ID) ->
    {ok, RestPort} = application:get_env(?APP_Name, location_service_rest_port),
    Address = "localhost:" ++ integer_to_list(RestPort) ++ "/" ++ binary_to_list(ID),
    list_to_binary(Address).
