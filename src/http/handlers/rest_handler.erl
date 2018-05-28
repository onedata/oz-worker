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

-include("rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").

-type method() :: 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE'.
-type binding() :: {binding, atom()} | client_id | client_ip.
-type bound_gri() :: #b_gri{}.
-type bound_auth_hint() :: undefined | {
    throughUser | throughGroup | throughSpace | throughProvider |
    throughHandleService | throughHandle | asUser | asGroup,
    binding()
}.

-export_type([method/0, binding/0, bound_gri/0, bound_auth_hint/0]).

% State of REST handler
-record(state, {
    client = #client{} :: entity_logic:client(),
    rest_req = undefined :: #rest_req{} | undefined,
    allowed_methods :: [method()]
}).
-type opts() :: #{method() => #rest_req{}}.

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
init(#{method := MethodBin} = Req, Opts) ->
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
content_types_provided(Req, State) ->
    {[{<<"application/json">>, provide_resource}], Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether the user is authorized to perform the action.
%% NOTE: The name and description of this function is actually misleading;
%% 401 Unauthorized is returned when there's been an *authentication* error,
%% and 403 Forbidden is returned when the already-authenticated client
%% is unauthorized to perform an operation.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), State :: #state{}) ->
    {true | {false, binary()}, cowboy_req:req(), #state{}}.
is_authorized(Req, State) ->
    % Check if the request carries any authorization
    try
        % Try to authorize the client using several methods.
        Client = authorize(Req, [
            fun authorize_by_oauth_provider/1,
            fun authorize_by_basic_auth/1,
            fun authorize_by_macaroons/1
        ]),
        % Always return true - authorization is checked by entity_logic later.
        {true, Req, State#state{client = Client}}
    catch
        throw:Error ->
            RestResp = error_rest_translator:response(Error),
            {stop, send_response(RestResp, Req), State};
        Type:Message ->
            ?error_stacktrace("Unexpected error in ~p:is_authorized - ~p:~p", [
                ?MODULE, Type, Message
            ]),
            RestResp = error_rest_translator:response(?ERROR_INTERNAL_SERVER_ERROR),
            {stop, send_response(RestResp, Req), State}
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
-spec rest_routes() -> [{binary(), module(), maps:map()}].
rest_routes() ->
    AllRoutes = lists:flatten([
        user_routes:routes(),
        group_routes:routes(),
        space_routes:routes(),
        share_routes:routes(),
        provider_routes:routes(),
        handle_service_routes:routes(),
        handle_routes:routes()
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
    {ok, PrefixStr} = oz_worker:get_env(rest_api_prefix),
    Prefix = str_utils:to_binary(PrefixStr),
    lists:map(fun({Path, RoutesForPath}) ->
        {<<Prefix/binary, Path/binary>>, ?REST_HANDLER_MODULE, RoutesForPath}
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
        #state{client = Client, rest_req = #rest_req{
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
            client = Client,
            gri = GRI,
            auth_hint = AuthHint,
            data = Data
        },
        RestResp = call_entity_logic_and_translate_response(ElReq),
        {stop, send_response(RestResp, Req2), State}
    catch
        throw:Error ->
            ErrorResp = error_rest_translator:response(Error),
            {stop, send_response(ErrorResp, Req), State};
        Type:Message ->
            ?error_stacktrace("Unexpected error in ~p:process_request - ~p:~p", [
                ?MODULE, Type, Message
            ]),
            NewReq = cowboy_req:reply(?HTTP_500_INTERNAL_SERVER_ERROR, Req),
            {stop, NewReq, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends given response (#rest_resp{}) and returns modified cowboy_req record.
%% @end
%%--------------------------------------------------------------------
-spec send_response(RestResp :: #rest_resp{}, Req :: cowboy_req:req()) ->
    NewReq :: cowboy_req:req().
send_response(#rest_resp{code = Code, headers = Headers, body = Body}, Req) ->
    RespBody = case Body of
        {binary, Bin} ->
            Bin;
        Map ->
            json_utils:encode(Map)
    end,
    cowboy_req:reply(Code, Headers, RespBody, Req).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Transforms bindings included in a #gri{} record into actual data that was
%% sent with the request.
%% @end
%%--------------------------------------------------------------------
-spec resolve_gri_bindings(bound_gri(), entity_logic:client(),
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
-spec resolve_auth_hint_bindings(bound_auth_hint(), entity_logic:client(),
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
    entity_logic:client(), cowboy_req:req()) -> binary() | {atom(), binary()}.
resolve_bindings(?BINDING(Key), _Client, Req) ->
    cowboy_req:binding(Key, Req);
resolve_bindings(?CLIENT_ID, #client{id = Id}, _Req) ->
    Id;
resolve_bindings(?CLIENT_IP, _Client, #{peer := {Ip, _Port}} = _Req) ->
    list_to_binary(inet_parse:ntoa(Ip));
resolve_bindings({Atom, PossibleBinding}, Client, Req) when is_atom(Atom) ->
    {Atom, resolve_bindings(PossibleBinding, Client, Req)};
resolve_bindings(Other, _Client, _Req) ->
    Other.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates entity logic response into REST response using TranslatorModule.
%% @end
%%--------------------------------------------------------------------
-spec call_entity_logic_and_translate_response(#el_req{}) -> #rest_resp{}.
call_entity_logic_and_translate_response(ElReq) ->
    Result = entity_logic:handle(ElReq),
    try
        rest_translator:response(ElReq, Result)
    catch
        Type:Message ->
            #el_req{operation = Operation, gri = GRI, auth_hint = AuthHint} = ElReq,
            ?error_stacktrace("Cannot translate REST result for:~n"
            "Operation: ~p~n"
            "GRI: ~p~n"
            "AuthHint: ~p~n"
            "Result: ~p~n"
            "---------~n"
            "Error was: ~p:~p", [
                Operation, GRI, AuthHint, Result, Type, Message
            ]),
            rest_translator:response(ElReq, ?ERROR_INTERNAL_SERVER_ERROR)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to authorize REST client using provided auth methods expressed
%% as functions to use.
%% @end
%%--------------------------------------------------------------------
-spec authorize(Req :: cowboy_req:req(),
    AuthFuns :: [fun((cowboy_req:req()) -> false | {true, #client{}})]) ->
    #client{}.
authorize(_Req, []) ->
    ?NOBODY;
authorize(Req, [AuthMethod | Rest]) ->
    case AuthMethod(Req) of
        {true, Client} ->
            Client;
        false ->
            authorize(Req, Rest)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to authorize client by provided token, if its prefix matches
%% any of the configured oauth providers supporting authority delegation.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_oauth_provider(Req :: cowboy_req:req()) ->
    false | {true, #client{}}.
authorize_by_oauth_provider(Req) ->
    case cowboy_req:header(<<"x-auth-token">>, Req) of
        undefined ->
            false;
        AccessToken ->
            case auth_utils:authorize_by_oauth_provider(AccessToken) of
                false ->
                    false;
                {true, Client} ->
                    {true, Client};
                ?ERROR_BAD_EXTERNAL_ACCESS_TOKEN(ProviderId) ->
                    throw(?ERROR_BAD_EXTERNAL_ACCESS_TOKEN(ProviderId))
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to authorize client by basic auth headers.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_basic_auth(Req :: cowboy_req:req()) ->
    false | {true, #client{}}.
authorize_by_basic_auth(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Basic ", UserPasswdB64/binary>> ->
            case auth_utils:authorize_by_basic_auth(UserPasswdB64) of
                {true, Client} ->
                    {true, Client};
                _ ->
                    throw(?ERROR_BAD_BASIC_CREDENTIALS)
            end;
        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to authorize client by macaroons.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_macaroons(Req :: cowboy_req:req()) ->
    false | {true, #client{}}.
authorize_by_macaroons(Req) ->
    {Macaroon, DischargeMacaroons} = parse_macaroons_from_headers(Req),
    case Macaroon of
        undefined ->
            false;
        _ ->
            case auth_utils:authorize_by_macaroons(
                Macaroon, DischargeMacaroons
            ) of
                {true, Client} ->
                    {true, Client};
                {error, _} ->
                    throw(?ERROR_UNAUTHORIZED)
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses macaroon and discharge macaroons out of request's headers.
%% @end
%%--------------------------------------------------------------------
-spec parse_macaroons_from_headers(Req :: cowboy_req:req()) ->
    {Macaroon :: binary() | undefined, DischargeMacaroons :: [binary()]} |
    no_return().
parse_macaroons_from_headers(Req) ->
    MacaroonHeader = cowboy_req:header(<<"macaroon">>, Req),
    XAuthTokenHeader = cowboy_req:header(<<"x-auth-token">>, Req),
    % X-Auth-Token is an alias for macaroon header, check if any of them
    % is given.
    SerializedMacaroon = case MacaroonHeader of
        <<_/binary>> ->
            MacaroonHeader;
        _ ->
            XAuthTokenHeader % binary() or undefined
    end,

    DischargeMacaroons = case cowboy_req:header(<<"discharge-macaroons">>, Req) of
        undefined ->
            [];
        <<"">> ->
            [];
        SerializedDischarges ->
            binary:split(SerializedDischarges, <<" ">>, [global])
    end,

    {SerializedMacaroon, DischargeMacaroons}.


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
