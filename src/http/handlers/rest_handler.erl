%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
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

-include("rest.hrl").
-include("errors.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-type method() :: get | post | put | patch | delete.
-export_type([method/0]).

% State of REST handler
-record(state, {
    client = #client{} :: entity_logic:client(),
    rest_req = undefined :: #rest_req{} | undefined,
    allowed_methods :: [method()]
}).
-type opts() :: #{method() => #rest_req{}}.

%% cowboy rest handler API
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    resource_exists/2,
    forbidden/2,
    is_authorized/2,
    accept_resource/2,
    provide_resource/2,
    delete_resource/2
]).
-export([
    rest_routes/0
]).
%% Convenience functions for rest translators
-export([
    created_reply/1,
    ok_no_content_reply/0,
    ok_body_reply/1,
    updated_reply/0,
    deleted_reply/0
]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Upgrade the protocol to cowboy_rest.
%% @end
%%--------------------------------------------------------------------
-spec init({TransportName :: atom(), ProtocolName :: http},
    Req :: cowboy_req:req(), Opts :: any()) ->
    {upgrade, protocol, cowboy_rest}.
init({_, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Initialize the state for this request.
%% @end
%%--------------------------------------------------------------------
-spec rest_init(Req :: cowboy_req:req(), Opts :: opts()) ->
    {ok, cowboy_req:req(), #state{}}.
rest_init(Req, Opts) ->
    {MethodBin, _} = cowboy_req:method(Req),
    Method = binary_to_method(MethodBin),
    % If given method is not allowed, it in not in the map. Such request
    % will stop execution on allowed_methods/2 callback. Use undefined if
    % the method does not exist.
    RestReq = maps:get(Method, Opts, undefined),
    AllowedMethods = maps:keys(Opts),
    {ok, Req, #state{rest_req = RestReq, allowed_methods = AllowedMethods}}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of allowed methods.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(Req :: cowboy_req:req(), State :: #state{}) ->
    {[binary()], cowboy_req:req(), #state{}}.
allowed_methods(Req, #state{allowed_methods = AllowedMethods} = State) ->
    BinMethods = [method_to_binary(M) || M <- AllowedMethods],
    {BinMethods, Req, State}.


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
    {[{<<"application/json">>, accept_resource}], Req, State}.


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
%% Return whether the resource exists.
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Req :: cowboy_req:req(), State :: #state{}) ->
    {boolean(), cowboy_req:req(), #state{}}.
resource_exists(Req, State) ->
    % Always return true - this is checked by entity_logic later.
    {true, Req, State}.


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
        % Try to authenticate the client using several methods.
        Client = authenticate(Req, [
            fun authenticate_by_oauth_provider/1,
            fun authenticate_by_basic_auth/1,
            fun authenticate_by_macaroons/1,
            fun authenticate_by_provider_certs/1
        ]),
        % Always return true - authorization is checked by entity_logic later.
        {true, Req, State#state{client = Client}}
    catch
        throw:Error ->
            RestResp = error_rest_translator:response(Error),
            {halt, send_response(RestResp, Req), State};
        Type:Message ->
            ?error_stacktrace("Unexpected error in ~p:is_authorized - ~p:~p", [
                ?MODULE, Type, Message
            ]),
            RestResp = error_rest_translator:response(?ERROR_INTERNAL_SERVER_ERROR),
            {halt, send_response(RestResp, Req), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether access to the resource is forbidden.
%% @see is_authorized/2
%% @end
%%--------------------------------------------------------------------
-spec forbidden(Req :: cowboy_req:req(), State :: #state{}) ->
    {boolean(), cowboy_req:req(), #state{}}.
forbidden(Req, State) ->
    % Always return false - this is checked by entity_logic later.
    {false, Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body of application/json content type.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), State :: #state{}) ->
    {halt, cowboy_req:req(), #state{}}.
accept_resource(Req, State) ->
    process_request(Req, State).


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: #state{}) ->
    {halt, cowboy_req:req(), #state{}}.
provide_resource(Req, State) ->
    process_request(Req, State).


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Delete the resource.
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: #state{}) ->
    {halt, cowboy_req:req(), #state{}}.
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
        rest_routes:user_routes(),
        rest_routes:group_routes(),
        rest_routes:space_routes(),
        rest_routes:share_routes(),
        rest_routes:provider_routes(),
        rest_routes:handle_service_routes(),
        rest_routes:handle_routes(),
        rest_routes:identity_routes()
    ]),
    % Aggregate routes that share the same path
    AggregatedRoutes = lists:foldl(
        fun({Path, RestReq}, AccProps) ->
            #rest_req{method = Method} = RestReq,
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
    {ok, PrefixStr} = application:get_env(?APP_NAME, rest_api_prefix),
    Prefix = str_utils:to_binary(PrefixStr),
    lists:map(fun({Path, RoutesForPath}) ->
        {<<Prefix/binary, Path/binary>>, ?REST_HANDLER_MODULE, RoutesForPath}
    end, AggregatedRoutes).


%%--------------------------------------------------------------------
%% @doc
%% REST reply that should be used for successful REST operations that send
%% a body in response.
%% @end
%%--------------------------------------------------------------------
-spec ok_body_reply(Body :: jiffy:json_value()) -> #rest_resp{}.
ok_body_reply(Body) ->
    #rest_resp{code = ?HTTP_200_OK, body = Body}.


%%--------------------------------------------------------------------
%% @doc
%% REST reply that should be used for successful REST operations that do not
%% send any body in response.
%% @end
%%--------------------------------------------------------------------
-spec ok_no_content_reply() -> #rest_resp{}.
ok_no_content_reply() ->
    #rest_resp{code = ?HTTP_204_NO_CONTENT}.


%%--------------------------------------------------------------------
%% @doc
%% REST reply that should be used for successful create REST calls.
%% Returns 201 CREATED with proper location headers.
%% @end
%%--------------------------------------------------------------------
-spec created_reply(PathTokens :: [binary()]) -> #rest_resp{}.
% Make sure there is no leading slash (so filename can be used for joining path)
created_reply([<<"/", Path/binary>> | Tail]) ->
    created_reply([Path | Tail]);
created_reply(PathTokens) ->
    % TODO VFS-2918 do not add rest prefix for now
%%    {ok, RestPrefix} = application:get_env(?APP_NAME, rest_api_prefix),
    RestPrefix = "/",
    LocationHeader = #{
        % TODO VFS-2918
%%        <<"Location">> => filename:join([RestPrefix | PathTokens])
        <<"location">> => filename:join([RestPrefix | PathTokens])
    },
    #rest_resp{code = ?HTTP_201_CREATED, headers = LocationHeader}.


%%--------------------------------------------------------------------
%% @doc
%% REST reply that should be used for successful REST updates.
%% @end
%%--------------------------------------------------------------------
-spec updated_reply() -> #rest_resp{}.
updated_reply() ->
    #rest_resp{code = ?HTTP_204_NO_CONTENT}.


%%--------------------------------------------------------------------
%% @doc
%% REST reply that should be used for successful REST deletions.
%% @end
%%--------------------------------------------------------------------
-spec deleted_reply() -> #rest_resp{}.
deleted_reply() ->
    % TODO VFS-2918
%%    #rest_resp{code = ?HTTP_204_NO_CONTENT}.
    #rest_resp{code = ?HTTP_202_ACCEPTED}.


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
    {halt, NewReq :: cowboy_req:req(), NewState :: #state{}}.
process_request(Req, State) ->
    try
        #state{client = Client, rest_req = #rest_req{
            method = Method,
            el_plugin = LogicPlugin,
            entity_id = EntityIdVal,
            resource = ResourceVal,
            translator = TranslatorModule
        }} = State,
        Operation = method_to_operation(Method),
        EntityId = resolve_bindings(EntityIdVal, Client, Req),
        Resource = resolve_bindings(ResourceVal, Client, Req),
        {Data, Req2} = case Operation of
            create -> get_data(Req);
            get -> {#{}, Req};
            update -> get_data(Req);
            delete -> {#{}, Req}
        end,
        Result = call_entity_logic(
            Operation, Client, LogicPlugin, EntityId, Resource, Data
        ),
        RestResp = translate_response(
            TranslatorModule, Operation, EntityId, Resource, Result
        ),
        {halt, send_response(RestResp, Req2), State}
    catch
        throw:Error ->
            ErrorResp = error_rest_translator:response(Error),
            {halt, send_response(ErrorResp, Req), State};
        Type:Message ->
            ?error_stacktrace("Unexpected error in ~p:process_request - ~p:~p", [
                ?MODULE, Type, Message
            ]),
            {ok, NewReq} = cowboy_req:reply(?HTTP_500_INTERNAL_SERVER_ERROR, Req),
            {halt, NewReq, State}
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
    HeadersList = maps:to_list(Headers),
    RespBody = case Body of
        {binary, Bin} ->
            Bin;
        Map ->
            json_utils:encode_map(Map)
    end,
    {ok, Req2} = cowboy_req:reply(Code, HeadersList, RespBody, Req),
    Req2.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Transforms binding as specified in rest routes into actual data that was
%% sent with the request.
%% @end
%%--------------------------------------------------------------------
-spec resolve_bindings(Binding | {atom(), Binding},
    Client :: entity_logic:client(), Req :: cowboy_req:req()) ->
    binary() |  {atom(), binary()} | cowboy_req:req() when
    Binding :: {binding, atom()} | client_id | cowboy_req.
resolve_bindings(?BINDING(Key), _Client, Req) ->
    {Binding, _} = cowboy_req:binding(Key, Req),
    Binding;
resolve_bindings(?CLIENT_ID, #client{id = Id}, _Req) ->
    Id;
resolve_bindings(?COWBOY_REQ, _Client, Req) ->
    Req;
resolve_bindings({Atom, PossibleBinding}, Client, Req) when is_atom(Atom) ->
    {Atom, resolve_bindings(PossibleBinding, Client, Req)};
resolve_bindings(Other, _Client, _Req) ->
    Other.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calls entity logic function to handle a request.
%% @end
%%--------------------------------------------------------------------
-spec call_entity_logic(method(), entity_logic:client(), entity_logic:el_plugin(),
    entity_logic:entity_id(), entity_logic:resource(), entity_logic:data()) ->
    ok | {ok, term()} | {error, term()}.
call_entity_logic(create, Client, LogicPlugin, EntityId, Resource, Data) ->
    entity_logic:create(Client, LogicPlugin, EntityId, Resource, Data);
call_entity_logic(get, Client, LogicPlugin, EntityId, Resource, _Data) ->
    entity_logic:get(Client, LogicPlugin, EntityId, Resource);
call_entity_logic(update, Client, LogicPlugin, EntityId, Resource, Data) ->
    entity_logic:update(Client, LogicPlugin, EntityId, Resource, Data);
call_entity_logic(delete, Client, LogicPlugin, EntityId, Resource, _Data) ->
    entity_logic:delete(Client, LogicPlugin, EntityId, Resource).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates entity logic response into REST response using TranslatorModule.
%% @end
%%--------------------------------------------------------------------
-spec translate_response(TranslatorModule :: module(), entity_logic:operation(),
    entity_logic:entity_id(), entity_logic:resource(), entity_logic:result()) ->
    #rest_resp{}.
translate_response(TranslatorModule, Operation, EntityId, Resource, Result) ->
    try
        case Result of
            {error, Reason} ->
                error_rest_translator:response({error, Reason});
            _ ->
                TranslatorModule:response(Operation, EntityId, Resource, Result)
        end
    catch
        Type:Message ->
            ?error_stacktrace("Cannot translate REST result for:~n"
            "TranslatorModule: ~p~n"
            "Operation: ~p~n"
            "EntityId: ~p~n"
            "Resource: ~p~n"
            "Result: ~p~n"
            "---------~n"
            "Error was: ~p:~p", [
                TranslatorModule, Operation, EntityId,
                Resource, Result, Type, Message
            ]),
            error_rest_translator:response(?ERROR_INTERNAL_SERVER_ERROR)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to authenticate REST client using provided auth methods expressed
%% as functions to use.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(Req :: cowboy_req:req(),
    AuthFuns :: [fun((cowboy_req:req()) -> false | {true, #client{}})]) ->
    #client{}.
authenticate(_Req, []) ->
    ?NOBODY;
authenticate(Req, [AuthMethod | Rest]) ->
    case AuthMethod(Req) of
        {true, Client} ->
            Client;
        false ->
            authenticate(Req, Rest)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to authenticate client by provided token, if its prefix matches
%% any of the configured oauth providers supporting authority delegation.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_oauth_provider(Req :: cowboy_req:req()) ->
    false | {true, #client{}}.
authenticate_by_oauth_provider(Req) ->
    case cowboy_req:header(<<"x-auth-token">>, Req) of
        {undefined, _} ->
            false;
        {AccessToken, _} ->
            AuthDelegationProviders = auth_config:get_providers_with_auth_delegation(),
            MatchingProviders = lists:dropwhile(
                fun({_ProvId, TokenPrefix}) ->
                    % If prefix matches, return false:
                    % dropwhile will stop and return the rest
                    % of the list with matched provider at the beginning
                    not bin_starts_with(AccessToken, TokenPrefix)
                end, AuthDelegationProviders),
            case MatchingProviders of
                [] ->
                    false;
                [{ProviderId, TokPrefix} | _] ->
                    Len = byte_size(TokPrefix),
                    <<TokPrefix:Len/binary, AccessTokenNoPrefix/binary>> = AccessToken,
                    case auth_utils:acquire_user_by_external_access_token(
                        ProviderId, AccessTokenNoPrefix
                    ) of
                        {error, bad_access_token} ->
                            throw(?ERROR_BAD_EXTERNAL_ACCESS_TOKEN(ProviderId));
                        {_, #document{key = UserId}} ->
                            {true, #client{type = user, id = UserId}}
                    end
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to authenticate client by basic auth headers.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_basic_auth(Req :: cowboy_req:req()) ->
    false | {true, #client{}}.
authenticate_by_basic_auth(Req) ->
    {Header, _} = cowboy_req:header(<<"authorization">>, Req),
    case Header of
        <<"Basic ", UserPasswdB64/binary>> ->
            UserPasswd = base64:decode(UserPasswdB64),
            [User, Passwd] = binary:split(UserPasswd, <<":">>),
            case user_logic:authenticate_by_basic_credentials(User, Passwd) of
                {ok, #document{key = UserId}, _} ->
                    Client = #client{type = user, id = UserId},
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
%% Tries to authenticate client by macaroons.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_macaroons(Req :: cowboy_req:req()) ->
    false | {true, #client{}}.
authenticate_by_macaroons(Req) ->
    {Macaroon, DischargeMacaroons, _} = parse_macaroons_from_headers(Req),
    %% @todo: VFS-1869
    %% Pass empty string as providerId because we do
    %% not expect the macaroon to have provider caveat
    %% (it is an authorization code for client).
    case Macaroon of
        undefined ->
            false;
        _ ->
            case auth_logic:validate_token(<<>>, Macaroon,
                DischargeMacaroons, <<"">>, undefined) of
                {ok, UserId} ->
                    Client = #client{type = user, id = UserId},
                    {true, Client};
                {error, _} ->
                    throw(?ERROR_BAD_MACAROON)
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to authenticate client by provider certs.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_provider_certs(Req :: cowboy_req:req()) ->
    false | {true, #client{}}.
authenticate_by_provider_certs(Req) ->
    case ssl:peercert(cowboy_req:get(socket, Req)) of
        {ok, PeerCert} ->
            case worker_proxy:call(ozpca_worker,
                {verify_provider, PeerCert}) of
                {ok, ProviderId} ->
                    Client = #client{type = provider, id = ProviderId},
                    {true, Client};
                {error, {bad_cert, Reason}} ->
                    ?warning("Attempted authentication with "
                    "bad peer certificate: ~p", [Reason]),
                    false;
                {error, _} ->
                    false
            end;
        {error, no_peercert} ->
            false
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses macaroon and discharge macaroons out of request's headers.
%% @end
%%--------------------------------------------------------------------
-spec parse_macaroons_from_headers(Req :: cowboy_req:req()) ->
    {Macaroon :: macaroon:macaroon() | undefined,
        DischargeMacaroons :: [macaroon:macaroon()], cowboy_req:req()} |
    no_return().
parse_macaroons_from_headers(Req) ->
    {MacaroonHeader, _} = cowboy_req:header(<<"macaroon">>, Req),
    {XAuthTokenHeader, _} = cowboy_req:header(<<"x-auth-token">>, Req),
    % X-Auth-Token is an alias for macaroon header, check if any of them
    % is given.
    SerializedMacaroon = case MacaroonHeader of
        <<_/binary>> -> MacaroonHeader;
        _ -> XAuthTokenHeader
    end,
    Macaroon = case SerializedMacaroon of
        <<_/binary>> -> deserialize_macaroon(SerializedMacaroon);
        _ -> undefined
    end,

    {SerializedDischarges, _} =
        cowboy_req:header(<<"discharge-macaroons">>, Req),
    DischargeMacaroons = case SerializedDischarges of
        <<>> ->
            [];

        <<_/binary>> ->
            Split = binary:split(SerializedDischarges, <<" ">>, [global]),
            [deserialize_macaroon(S) || S <- Split];

        _ ->
            []
    end,

    {Macaroon, DischargeMacaroons, Req}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deserializes a macaroon, throws on error.
%% @end
%%--------------------------------------------------------------------
-spec deserialize_macaroon(Data :: binary()) ->
    macaroon:macaroon() | no_return().
deserialize_macaroon(Data) ->
    case token_utils:deserialize(Data) of
        {ok, M} -> M;
        {error, _} -> throw(?ERROR_BAD_MACAROON)
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
    {ok, Body, Req2} = cowboy_req:body(Req),
    Data = try
        case Body of
            <<"">> -> #{};
            _ -> json_utils:decode_map(Body)
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
binary_to_method(<<"POST">>) -> post;
binary_to_method(<<"PUT">>) -> put;
binary_to_method(<<"GET">>) -> get;
binary_to_method(<<"PATCH">>) -> patch;
binary_to_method(<<"DELETE">>) -> delete.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts an atom representing a REST method to a binary representing
%% the method.
%% @end
%%--------------------------------------------------------------------
-spec method_to_binary(Method :: method()) -> binary().
method_to_binary(post) -> <<"POST">>;
method_to_binary(put) -> <<"PUT">>;
method_to_binary(get) -> <<"GET">>;
method_to_binary(patch) -> <<"PATCH">>;
method_to_binary(delete) -> <<"DELETE">>.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts an atom representing a REST method into entity logic operation
%% that should be called to handle it.
%% @end
%%--------------------------------------------------------------------
-spec method_to_operation(method()) -> entity_logic:operation().
method_to_operation(post) -> create;
method_to_operation(put) -> create;
method_to_operation(get) -> get;
method_to_operation(patch) -> update;
method_to_operation(delete) -> delete.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if given binary starts with given prefix.
%% @end
%%--------------------------------------------------------------------
-spec bin_starts_with(Subject :: binary(), Prefix :: binary()) -> boolean().
bin_starts_with(Subject, Prefix) ->
    FirstChars = binary:part(Subject, 0, byte_size(Prefix)),
    FirstChars =:= Prefix.
