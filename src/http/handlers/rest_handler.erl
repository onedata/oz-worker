%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling the common RESTful logic. It implements
%%% Cowboy's rest pseudo-behavior, delegating specifics to submodules.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_handler).
-author("Lukasz Opiola").

-behaviour(cowboy_rest).

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").

-type method() :: 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE'.
-type binding() :: {binding, atom()} | client_id | client_ip.
-type bound_gri() :: #b_gri{}.
-type bound_auth_hint() :: undefined | {
    throughUser | throughGroup | throughSpace | throughProvider |
    throughHandleService | throughHandle | throughHarvester |
    throughCluster | throughInventory |
    asUser | asGroup | asSpace | asHarvester,
    binding()
}.

% State of REST handler
-record(state, {
    auth = #auth{} :: aai:auth(),
    rest_req = undefined :: #rest_req{} | undefined,
    allowed_methods :: [method()]
}).
-type opts() :: #{method() => #rest_req{}}.

-type rest_resp() :: #rest_resp{}.

-export_type([method/0, binding/0, bound_gri/0, bound_auth_hint/0, opts/0, rest_resp/0]).

%% cowboy rest handler API
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    is_authorized/2,
    accept_resource/2,
    provide_resource/2,
    delete_resource/2
]).
-export([
    rest_routes/0
]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Initialize the state for this request.
%% @end
%%--------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), Opts :: opts()) ->
    {cowboy_rest, cowboy_req:req(), #state{}}.
init(#{method := MethodBin} = InitialReq, Opts) ->
    % The REST API accepts only tokens (rather than session cookies) so
    % it's safe to allow it to be called from other origins.
    Req = http_cors:allow_origin(<<"*">>, InitialReq),
    Method = binary_to_method(MethodBin),
    % If given method is not allowed, it is not in the map. Such request
    % will stop execution on allowed_methods/2 callback. Use undefined if
    % the method does not exist.
    {cowboy_rest, Req, #state{
        rest_req = maps:get(Method, Opts, undefined),
        allowed_methods = maps:keys(Opts)
    }}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of allowed methods.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(Req :: cowboy_req:req(), State :: #state{}) ->
    {[binary()], cowboy_req:req(), #state{}}.
allowed_methods(Req, #state{allowed_methods = AllowedMethods} = State) ->
    {[method_to_binary(M) || M <- AllowedMethods], Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of content-types the resource accepts.
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(Req :: cowboy_req:req(), State :: #state{}) ->
    {Value, cowboy_req:req(), #state{}} when
    Value :: [{binary() | {Type, SubType, Params}, AcceptResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    AcceptResource :: atom().
content_types_accepted(Req, State) ->
    case cowboy_req:has_body(Req) of
        true -> {[{<<"application/json">>, accept_resource}], Req, State};
        false -> {[{'*', accept_resource}], Req, State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of content-types the resource provides.
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(Req :: cowboy_req:req(), State :: #state{}) ->
    {Value, cowboy_req:req(), #state{}} when
    Value :: [{binary() | {Type, SubType, Params}, ProvideResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    ProvideResource :: atom().
content_types_provided(Req, #state{rest_req = #rest_req{produces = Produces}} = State) ->
    {lists:map(fun(ContentType) -> {ContentType, provide_resource} end, Produces), Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether the user is authorized to perform the action.
%% NOTE: The name and description of this function is actually misleading;
%% 401 Unauthorized is returned when there's been an *authentication* error,
%% and 403 Forbidden is returned when the already-authenticated client
%% is unauthorized to perform an operation.
%%
%% This function checks authentication, authorization is checked
%% by entity_logic later.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), State :: #state{}) ->
    {true | {false, binary()} | stop, cowboy_req:req(), #state{}}.
is_authorized(Req, State) ->
    Result = try
        case token_auth:authenticate_for_rest_interface(Req) of
            {true, TokenAuth} ->
                {ok, TokenAuth};
            {error, _} = Error1 ->
                Error1;
            false ->
                {PeerIp, _} = cowboy_req:peer(Req),
                case basic_auth:authenticate(Req) of
                    {true, BasicAuth} ->
                        {ok, BasicAuth#auth{peer_ip = PeerIp}};
                    {error, _} = Error2 ->
                        Error2;
                    false ->
                        {ok, #auth{subject = ?SUB(nobody), peer_ip = PeerIp}}
                end
        end
    catch
        throw:Error3 ->
            ?ERROR_UNAUTHORIZED(Error3);
        Type:Message:Stacktrace ->
            ?error_stacktrace(
                "Unexpected error in ~tp:is_authorized - ~tp:~tp",
                [?MODULE, Type, Message],
                Stacktrace
            ),
            ?ERROR_UNAUTHORIZED(?ERROR_INTERNAL_SERVER_ERROR)
    end,

    case Result of
        {ok, Auth} ->
            {true, Req, State#state{auth = Auth}};
        {error, _} = Err4 ->
            {stop, send_error_response(Err4, Req), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body of application/json content type.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), State :: #state{}) ->
    {stop, cowboy_req:req(), #state{}}.
accept_resource(Req, State) ->
    process_request(Req, State).


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: #state{}) ->
    {stop, cowboy_req:req(), #state{}}.
provide_resource(Req, State) ->
    process_request(Req, State).


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Delete the resource.
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: #state{}) ->
    {stop, cowboy_req:req(), #state{}}.
delete_resource(Req, State) ->
    process_request(Req, State).


%%--------------------------------------------------------------------
%% @doc
%% Returns all REST routes in the cowboy router format.
%% @end
%%--------------------------------------------------------------------
-spec rest_routes() -> [{binary(), module(), rest_handler:opts()}].
rest_routes() ->
    AllRoutes = lists:flatten([
        dev_utils:dev_provider_registration_route(),
        user_routes:routes(),
        group_routes:routes(),
        space_routes:routes(),
        share_routes:routes(),
        provider_routes:routes(),
        token_routes:routes(),
        handle_service_routes:routes(),
        handle_routes:routes(),
        harvester_routes:routes(),
        cluster_routes:routes(),
        atm_inventory_routes:routes(),
        atm_lambda_routes:routes(),
        atm_workflow_schema_routes:routes(),
        zone_routes:routes()
    ]),
    % Aggregate routes that share the same path
    AggregatedRoutes = lists:foldl(
        fun({Path, #rest_req{method = Method} = RestReq}, AccProps) ->
            RoutesForPath = proplists:get_value(Path, AccProps, #{}),
            lists:keystore(
                Path, 1, AccProps,
                {Path, RoutesForPath#{Method => RestReq}}
            )
        end, [], AllRoutes),
    % Convert all routes to cowboy-compliant routes
    % - prepend REST prefix to every route
    % - rest handler module must be added as second element to the tuples
    % - RoutesForPath will serve as Opts to rest handler init.
    lists:map(fun({Path, RoutesForPath}) ->
        {oz_worker:get_rest_api_path(Path), ?REST_HANDLER_MODULE, RoutesForPath}
    end, AggregatedRoutes).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Processes a REST request (of any type) by calling entity logic.
%% Return new Req and State (after setting cowboy response).
%% @end
%%--------------------------------------------------------------------
-spec process_request(Req :: cowboy_req:req(), State :: #state{}) ->
    {stop, NewReq :: cowboy_req:req(), NewState :: #state{}}.
process_request(Req, State) ->
    try
        oz_worker_circuit_breaker:assert_closed(),
        #state{auth = Client, rest_req = #rest_req{
            method = Method,
            b_gri = GriWithBindings,
            b_auth_hint = AuthHintWithBindings
        }} = State,
        Operation = method_to_operation(Method),
        GRI = resolve_gri_bindings(GriWithBindings, Client, Req),
        AuthHint = resolve_auth_hint_bindings(AuthHintWithBindings, Client, Req),
        {Data, Req2} = case Operation of
            create -> get_data(Req);
            get -> {#{}, Req};
            update -> get_data(Req);
            delete -> {#{}, Req}
        end,
        ElReq = #el_req{
            operation = Operation,
            auth = Client,
            gri = GRI,
            auth_hint = AuthHint,
            data = Data
        },
        RestResp = route_to_proper_handler(ElReq, Req2),
        {stop, send_response(RestResp, Req2), State}
    catch
        throw:Error ->
            {stop, send_error_response(Error, Req), State};
        Type:Message:Stacktrace ->
            ?error_stacktrace(
                "Unexpected error in ~tp:process_request - ~tp:~tp",
                [?MODULE, Type, Message],
                Stacktrace
            ),
            NewReq = cowboy_req:reply(?HTTP_500_INTERNAL_SERVER_ERROR, Req),
            {stop, NewReq, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends given response and returns modified cowboy_req record.
%% @end
%%--------------------------------------------------------------------
-spec send_response(rest_resp(), Req :: cowboy_req:req()) ->
    NewReq :: cowboy_req:req().
send_response(#rest_resp{code = Code, headers = Headers, body = Body}, Req) ->
    RespBody = case Body of
        {binary, Bin} ->
            Bin;
        Map ->
            json_utils:encode(Map)
    end,
    cowboy_req:reply(Code, Headers, RespBody, Req).


-spec send_error_response(errors:error(), cowboy_req:req()) -> cowboy_req:req().
send_error_response(Error = {error, _}, Req) ->
    send_response(#rest_resp{
        code = errors:to_http_code(Error),
        body = #{<<"error">> => errors:to_json(Error)}
    }, Req).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Transforms bindings included in a #gri{} record into actual data that was
%% sent with the request.
%% @end
%%--------------------------------------------------------------------
-spec resolve_gri_bindings(bound_gri(), aai:auth(),
    cowboy_req:req()) -> entity_logic:gri().
resolve_gri_bindings(#b_gri{type = Tp, id = Id, aspect = As, scope = Sc}, Client, Req) ->
    IdBinding = resolve_bindings(Id, Client, Req),
    AspectBinding = case As of
        {Atom, Asp} -> {Atom, resolve_bindings(Asp, Client, Req)};
        Atom -> Atom
    end,
    #gri{type = Tp, id = IdBinding, aspect = AspectBinding, scope = Sc}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Transforms bindings included in an authHint expression into actual data that
%% was sent with the request.
%% @end
%%--------------------------------------------------------------------
-spec resolve_auth_hint_bindings(bound_auth_hint(), aai:auth(),
    cowboy_req:req()) -> entity_logic:auth_hint().
resolve_auth_hint_bindings({Key, Value}, Client, Req) ->
    {Key, resolve_bindings(Value, Client, Req)};
resolve_auth_hint_bindings(undefined, _Client, _Req) ->
    undefined.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Transforms bindings as specified in rest routes into actual data that was
%% sent with the request.
%% @end
%%--------------------------------------------------------------------
-spec resolve_bindings(binding() | {atom(), binding()} | term(),
    aai:auth(), cowboy_req:req()) -> binary() | {atom(), binary()}.
resolve_bindings(?BINDING(Key), _Client, Req) ->
    cowboy_req:binding(Key, Req);
resolve_bindings(?CLIENT_ID, #auth{subject = #subject{id = Id}}, _Req) ->
    Id;
resolve_bindings(?CLIENT_IP, _Client, #{peer := {Ip, _Port}} = _Req) ->
    list_to_binary(inet_parse:ntoa(Ip));
resolve_bindings({Atom, PossibleBinding}, Client, Req) when is_atom(Atom) ->
    {Atom, resolve_bindings(PossibleBinding, Client, Req)};
resolve_bindings(Other, _Client, _Req) ->
    Other.


%% @private
-spec route_to_proper_handler(#el_req{}, cowboy_req:req()) -> rest_resp().
route_to_proper_handler(#el_req{operation = get, gri = #gri{type = od_share, aspect = {shared_data, ObjectId}}}, Req) ->
    shared_data_redirector:handle(ObjectId, Req);
route_to_proper_handler(ElReq, _Req) ->
    call_entity_logic_and_translate_response(ElReq).


%% @private
-spec call_entity_logic_and_translate_response(#el_req{}) -> rest_resp().
call_entity_logic_and_translate_response(ElReq) ->
    Result = entity_logic:handle(ElReq),
    try
        rest_translator:response(ElReq, Result)
    catch
        Type:Message:Stacktrace ->
            #el_req{operation = Operation, gri = GRI, auth_hint = AuthHint} = ElReq,
            ?error_stacktrace(
                "Cannot translate REST result for:~n"
                "Operation: ~tp~n"
                "GRI: ~tp~n"
                "AuthHint: ~tp~n"
                "Result: ~tp~n"
                "---------~n"
                "Error was: ~tp:~tp",
                [Operation, GRI, AuthHint, Result, Type, Message],
                Stacktrace
            ),
            rest_translator:response(ElReq, ?ERROR_INTERNAL_SERVER_ERROR)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns body that was sent in HTTP request. Empty body is treated as empty
%% JSON object (without harm for server logic).
%% @end
%%--------------------------------------------------------------------
-spec get_data(Req :: cowboy_req:req()) ->
    {Data :: entity_logic:data(), cowboy_req:req()}.
get_data(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Data = try
        case Body of
            <<"">> -> #{};
            _ -> json_utils:decode(Body)
        end
    catch _:_ ->
        throw(?ERROR_MALFORMED_DATA)
    end,
    {Data, Req2}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts a binary representing a REST method to an atom representing
%% the method.
%% @end
%%--------------------------------------------------------------------
-spec binary_to_method(BinMethod :: binary()) -> method().
binary_to_method(<<"POST">>) -> 'POST';
binary_to_method(<<"PUT">>) -> 'PUT';
binary_to_method(<<"GET">>) -> 'GET';
binary_to_method(<<"PATCH">>) -> 'PATCH';
binary_to_method(<<"DELETE">>) -> 'DELETE'.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts an atom representing a REST method to a binary representing
%% the method.
%% @end
%%--------------------------------------------------------------------
-spec method_to_binary(Method :: method()) -> binary().
method_to_binary('POST') -> <<"POST">>;
method_to_binary('PUT') -> <<"PUT">>;
method_to_binary('GET') -> <<"GET">>;
method_to_binary('PATCH') -> <<"PATCH">>;
method_to_binary('DELETE') -> <<"DELETE">>.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts an atom representing a REST method into entity logic operation
%% that should be called to handle it.
%% @end
%%--------------------------------------------------------------------
-spec method_to_operation(method()) -> entity_logic:operation().
method_to_operation('POST') -> create;
method_to_operation('PUT') -> create;
method_to_operation('GET') -> get;
method_to_operation('PATCH') -> update;
method_to_operation('DELETE') -> delete.
