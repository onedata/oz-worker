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
-module(n_rest_handler).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("errors.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-type method() :: get | post | put | patch | delete.

-type rest_req() :: #rest_req{}.
-export_type([method/0, rest_req/0]).

-record(state, {
    client = #client{} :: n_entity_logic:client(),
    rest_req = undefined :: rest_req() | undefined,
    allowed_methods :: [method()]
}).
-type opts() :: #{method() => rest_req()}.
-type state() :: #state{}.

%% API
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
%% Convenience functions for rest translators
-export([created_reply/1, ok_body_reply/1, updated_reply/0, deleted_reply/0]).


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
    {ok, cowboy_req:req(), state()}.
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
-spec allowed_methods(Req :: cowboy_req:req(), State :: state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, #state{allowed_methods = AllowedMethods} = State) ->
    BinMethods = [method_to_binary(M) || M <- AllowedMethods],
    {BinMethods, Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of content-types the resource accepts.
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(Req :: cowboy_req:req(), State :: state()) ->
    {Value, cowboy_req:req(), state()} when
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
-spec content_types_provided(Req :: cowboy_req:req(), State :: state()) ->
    {Value, cowboy_req:req(), state()} when
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
-spec resource_exists(Req :: cowboy_req:req(), State :: state()) ->
    {boolean(), cowboy_req:req(), state()}.
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
-spec is_authorized(Req :: cowboy_req:req(), State :: state()) ->
    {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
    % Check if the request carries any authorization
    try
        % First, check for basic auth headers.
        Client = case authenticate_by_basic_auth(Req) of
            {true, BasicClient} ->
                BasicClient;
            false ->
                % Basic auth not found, try macaroons
                case authenticate_by_macaroons(Req) of
                    {true, MacaroonClient} ->
                        MacaroonClient;
                    false ->
                        % Macaroon auth not found,
                        % try to authorize as provider
                        case authenticate_by_provider_certs(Req) of
                            {true, ProviderClient} ->
                                ProviderClient;
                            false ->
                                % No auth found, client is nobody.
                                ?NOBODY
                        end
                end
        end,
        % Always return true - this is checked by entity_logic later.
        {true, Req, State#state{client = Client}}
    catch
        {silent_error, ReqX} -> %% As per RFC 6750 section 3.1
            {{false, <<"">>}, ReqX, State};

        {Error, Description1, ReqX} when is_atom(Error) ->
            Body = json_utils:encode([
                {error, Error},
                {error_description, str_utils:to_binary(Description1)}
            ]),

            WWWAuthenticate =
                <<"error=", (atom_to_binary(Error, latin1))/binary>>,

            ReqY = cowboy_req:set_resp_body(Body, ReqX),
            {{false, WWWAuthenticate}, ReqY, State};

        {Error, StatusCode, Description1, ReqX} when is_atom(Error) ->
            Body = json_utils:encode([
                {error, Error},
                {error_description, str_utils:to_binary(Description1)}
            ]),

            {ok, ReqY} = cowboy_req:reply(StatusCode, [], Body, ReqX),
            {halt, ReqY, State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether access to the resource is forbidden.
%% @see is_authorized/2
%% @end
%%--------------------------------------------------------------------
-spec forbidden(Req :: cowboy_req:req(), State :: state()) ->
    {boolean(), cowboy_req:req(), state()}.
forbidden(Req, State) ->
    % Always return false - this is checked by entity_logic later.
    {false, Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body of application/json content type.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), State :: state()) ->
    {halt, cowboy_req:req(), state()}.
accept_resource(Req, State) ->
    process_request(Req, State).


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: state()) ->
    {halt, cowboy_req:req(), state()}.
provide_resource(Req, State) ->
    process_request(Req, State).


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Delete the resource.
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: state()) ->
    {halt, cowboy_req:req(), state()}.
delete_resource(Req, State) ->
    process_request(Req, State).


% Make sure there is no leading slash (so filename can be used for joining path)
created_reply([<<"/", Path/binary>> | Tail]) ->
    created_reply([Path | Tail]);
created_reply(PathTokens) ->
    {ok, RestPrefix} = application:get_env(?APP_NAME, rest_api_prefix),
    LocationHeader = #{
        <<"location">> => filename:join([RestPrefix | PathTokens])
    },
    #rest_resp{code = ?HTTP_201_CREATED, headers = LocationHeader}.


ok_body_reply(Body) ->
    #rest_resp{code = ?HTTP_200_OK, body = Body}.




updated_reply() ->
    #rest_resp{code = ?HTTP_204_NO_CONTENT}.


deleted_reply() ->
    #rest_resp{code = ?HTTP_204_NO_CONTENT}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

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
        #rest_resp{code = Code, headers = Headers, body = Body} = RestResp,
        HeadersList = maps:to_list(Headers),
        BodyBinary = case Body of
            Bin when is_binary(Bin) -> Bin;
            Map when is_map(Map) -> json_utils:encode_map(Map)
        end,
        {ok, Req3} = cowboy_req:reply(Code, HeadersList, BodyBinary, Req2),
        {halt, Req3, State}
    catch
        Type:Message ->
            ?error_stacktrace("Unexpected error in ~p:process_request - ~p:~p", [
                ?MODULE, Type, Message
            ]),
            {halt, State, Req}
    end.


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


call_entity_logic(create, Client, LogicPlugin, EntityId, Resource, Data) ->
    n_entity_logic:create(Client, LogicPlugin, EntityId, Resource, Data);
call_entity_logic(get, Client, LogicPlugin, EntityId, Resource, _Data) ->
    n_entity_logic:get(Client, LogicPlugin, EntityId, Resource);
call_entity_logic(update, Client, LogicPlugin, EntityId, Resource, Data) ->
    n_entity_logic:update(Client, LogicPlugin, EntityId, Resource, Data);
call_entity_logic(delete, Client, LogicPlugin, EntityId, Resource, _Data) ->
    n_entity_logic:delete(Client, LogicPlugin, EntityId, Resource).


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
            ?error("Cannot translate REST result for:~n"
            "TranslatorModule: ~p~n"
            "Operation: ~p~n"
            "EntityId: ~p~n"
            "Resource: ~p~n"
            "Result: ~p~n"
            "---------~n"
            "Error was: ~p:~p~n"
            "Stacktrace: ~p", [
                TranslatorModule, Operation, EntityId,
                Resource, Result, Type, Message, erlang:get_stacktrace()
            ]),
            error_rest_translator:response(?ERROR_INTERNAL_SERVER_ERROR)
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
                    false
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
                DischargeMacaroons, undefined, undefined) of
                {ok, UserId} ->
                    Client = #client{type = user, id = UserId},
                    {true, Client};
                {error, Error} ->
                    throw({error, Error})
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
    case etls:peercert(cowboy_req:get(socket, Req)) of
        {ok, PeerCert} ->
            case worker_proxy:call(ozpca_worker,
                {verify_provider, PeerCert}) of
                {ok, ProviderId} ->
                    Client = #client{type = provider, id = ProviderId},
                    {true, Client};
                {error, {bad_cert, Reason}} ->
                    ?warning("Attempted authentication with "
                    "bad peer certificate: ~p", [Reason]),
                    false
            end;
        {error, no_peer_certificate} ->
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
        <<_/binary>> -> deserialize_macaroon(SerializedMacaroon, Req);
        _ -> undefined
    end,

    {SerializedDischarges, _} =
        cowboy_req:header(<<"discharge-macaroons">>, Req),
    DischargeMacaroons = case SerializedDischarges of
        <<>> ->
            [];

        <<_/binary>> ->
            Split = binary:split(SerializedDischarges, <<" ">>, [global]),
            [deserialize_macaroon(S, Req) || S <- Split];

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
-spec deserialize_macaroon(Data :: binary(), Req :: cowboy_req:req()) ->
    macaroon:macaroon() | no_return().
deserialize_macaroon(Data, Req) ->
    case token_utils:deserialize(Data) of
        {ok, M} -> M;
        {error, Reason} ->
            throw({invalid_token, atom_to_binary(Reason, latin1), Req})
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns body that was sent in HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec get_data(Req :: cowboy_req:req()) ->
    {Data :: n_entity_logic:data(), cowboy_req:req()}.
get_data(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Data = try
        json_utils:decode_map(Body)
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
-spec method_to_operation(method()) -> n_entity_logic:operation().
method_to_operation(post) -> create;
method_to_operation(put) -> create;
method_to_operation(get) -> get;
method_to_operation(patch) -> update;
method_to_operation(delete) -> delete.
