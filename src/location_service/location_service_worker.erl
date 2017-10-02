%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This worker controls lifetime of Location Service client and provides
%%% basic API for Location Service.
%%% @end
%%%-------------------------------------------------------------------
-module(location_service_worker).
-author("Michal Zmuda").

-behaviour(worker_plugin_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("cluster_worker/include/global_definitions.hrl").

%% worker_plugin_behaviour callbacks
-export([init/1, handle/1, cleanup/0]).

%% API
-export([get/1, set/2]).

-type(dht_data() :: [{binary(), term()}]).
-type(error_code() :: 'CONFLICT' | 'TIMEOUT' | 'NOT_FOUND' | 'UNKNOWN_ERROR' | atom()).

-define(DATA_KEY, <<"value">>).

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
-spec get(locations:id()) -> {error, Reason} | {ok, dht_data()} when
    Reason :: request_failed | {error_code(), Message :: binary()}.
get(ID) ->
    case http_client:request(get, get_dht_endpoint(ID)) of
        {ok, 200, _, ResponseBody} ->
            Response = json_utils:decode(ResponseBody),
            StatusBin = proplists:get_value(<<"status">>, Response, <<"empty_response">>),
            Status = binary_to_atom(StatusBin, latin1),
            case Status of
                'OK' ->
                    Data = proplists:get_value(?DATA_KEY, Response, undefined),
                    {ok, Data};
                _ ->
                    Message = proplists:get_value(<<"message">>, Response, undefined),
                    {error, {Status, Message}}
            end;
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
-spec set(locations:id(), dht_data()) -> ok | {error, Reason} when
    Reason :: request_failed | {error_code(), Message :: binary()}.
set(ID, Data) ->
    Body = json_utils:encode({struct, [{?DATA_KEY, Data}]}),
    Headers = #{<<"content-type">> => <<"application/json">>},
    case http_client:request(post, get_dht_endpoint(ID), Headers, Body) of
        {ok, 200, _, ResponseBody} ->
            Response = json_utils:decode(ResponseBody),
            StatusBin = proplists:get_value(<<"status">>, Response, <<"empty_response">>),
            Status = binary_to_atom(StatusBin, latin1),
            case Status of
                'OK' -> ok;
                _ ->
                    Message = proplists:get_value(<<"message">>, Response, undefined),
                    {error, {Status, Message}}
            end;
        {error, _Term} ->
            ?warning("Post request (id: ~p, value ~p) failed due to ~p", [ID, Data, _Term]),
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
    {ok, ServicePort} = application:get_env(?APP_NAME, location_service_port),
    {ok, RestPort} = application:get_env(?APP_NAME, location_service_rest_port),
    {ok, Nodes} = application:get_env(?APP_NAME, location_service_bootstrap_nodes),
    {ok, [DBNode | _]} = application:get_env(?APP_NAME, db_nodes),
    BootstrapArgs = lists:append([" -b " ++ atom_to_list(Node) || Node <- Nodes]),

    Pid = spawn_link(
        fun() ->
            process_flag(trap_exit, true),
            {ok, LibPath} = application:get_env(?APP_NAME, location_service_lib_path),

            Command = "node " ++ filename:join(LibPath, "start.js")
                ++ " -h " ++ Host
                ++ " -p " ++ integer_to_list(ServicePort)
                ++ " -r " ++ integer_to_list(RestPort)
                ++ " -c " ++ hd(string:tokens(atom_to_list(DBNode), ":"))
                ++ " " ++ get_log_level_opt()
                ++ BootstrapArgs,

            ?info("Running DHT port by command ~p", [Command]),
            Port = erlang:open_port({spawn, Command}, [{line, 1000}, stderr_to_stdout]),
            ?info("DHT port started"),
            handle_port(Port)
        end),
    {ok, #{handler => Pid}}.

%%--------------------------------------------------------------------
%% @doc
%% Handles requests connected with Location Service.
%% @end
%%--------------------------------------------------------------------
-spec handle(Request :: term()) -> ok | {error, Reason :: term()} | no_return().
handle(healthcheck) ->
    locations:claim(node(), <<"helthcheck_record">>),
    case locations:resolve(node(), <<"helthcheck_record">>) of
        {error, _Reason} -> {error, {not_allowed_result, _Reason}};
        {ok, _} -> ok
    end;

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
    {ok, RestPort} = application:get_env(?APP_NAME, location_service_rest_port),
    Address = "localhost:" ++ integer_to_list(RestPort) ++ "/" ++ binary_to_list(ID),
    list_to_binary(Address).

%%--------------------------------------------------------------------
%% @doc @private
%% Returns log level option for location service script.
%% @end
%%--------------------------------------------------------------------
-spec get_log_level_opt() -> string().
get_log_level_opt() ->
    case application:get_env(?APP_NAME, location_service_log_level) of
        {ok, 1} -> "-v";
        {ok, 2} -> "-vv";
        {ok, 3} -> "-vvv";
        {ok, 4} -> "-vvvv";
        _ -> ""
    end.