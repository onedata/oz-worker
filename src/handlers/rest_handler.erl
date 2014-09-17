%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module handling the common RESTful logic. It implements
%% Cowboy's rest pseudo-behavior, delegating specifics to submodules.
%% @end
%% ===================================================================
-module(rest_handler).
-author("Konrad Zemek").

-include("handlers/rest_handler.hrl").
-include_lib("ctool/include/logging.hrl").


%% API
-export([init/3, allowed_methods/2, content_types_accepted/2, is_authorized/2,
    content_types_provided/2, delete_resource/2, delete_completed/2,
    accept_resource_json/2, accept_resource_form/2, provide_resource/2,
    rest_init/2, forbidden/2, resource_exists/2]).


%% init/3
%% ====================================================================
%% @doc Cowboy callback function.
%% Upgrade the protocol to cowboy_rest.
%% @end
%% ====================================================================
-spec init({TransportName :: ssl, ProtocolName :: http},
           Req :: cowboy_req:req(), Opts :: any()) ->
    {upgrade, protocol, cowboy_rest}.
%% ====================================================================
init({ssl, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.


%% rest_init/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Initialize the state for this request.
%% @end
%% ====================================================================
-spec rest_init(Req :: cowboy_req:req(), Opts :: rstate()) ->
    {ok, cowboy_req:req(), rstate()}.
%% ====================================================================
rest_init(Req, #rstate{} = Opts) ->
    {ok, Req, Opts}.


%% allowed_methods/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return the list of allowed methods.
%% @end
%% ====================================================================
-spec allowed_methods(Req :: cowboy_req:req(), State :: rstate()) ->
    {[binary()], cowboy_req:req(), rstate()}.
%% ====================================================================
allowed_methods(Req, #rstate{methods = Methods} = State) ->
    BinMethods = [method_to_binary(M) || M <- Methods],
    {BinMethods, Req, State}.


%% content_types_accepted/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return whether POST is allowed when the resource doesn't exist.
%% @end
%% ====================================================================
-spec content_types_accepted(Req :: cowboy_req:req(), State :: rstate()) ->
    {Value, cowboy_req:req(), rstate()} when
    Value :: [{binary() | {Type, SubType, Params}, AcceptResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    AcceptResource :: atom().
%% ====================================================================
content_types_accepted(Req, #rstate{} = State) ->
    {[
        {<<"application/json">>, accept_resource_json},
        {<<"application/x-www-form-urlencoded">>, accept_resource_form}
    ], Req, State}.


%% content_types_provided/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return the list of content-types the resource provides.
%% @end
%% ====================================================================
-spec content_types_provided(Req :: cowboy_req:req(), State :: rstate()) ->
    {Value, cowboy_req:req(), rstate()} when
    Value :: [{binary() | {Type, SubType, Params}, ProvideResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    ProvideResource :: atom().
%% ====================================================================
content_types_provided(Req, #rstate{} = State) ->
    {[{<<"application/json">>, provide_resource}], Req, State}.


%% delete_resource/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Delete the resource.
%% @end
%% ====================================================================
-spec delete_resource(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
%% ====================================================================
delete_resource(Req, #rstate{module = Mod, resource = Resource} = State) ->
    {ResId, Req2} = get_res_id(Req, State),
    {Result, Req3} = Mod:delete_resource(Resource, ResId, Req2),
    {Result, Req3, State}.


%% delete_completed/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Returns if there is a guarantee that the resource gets deleted immediately
%% from the system.
%% @end
%% ====================================================================
-spec delete_completed(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
%% ====================================================================
delete_completed(Req, State) ->
    {false, Req, State}.


%% forbidden/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return whether access to the resource is forbidden.
%% @see is_authorized/2
%% @end
%% ====================================================================
-spec forbidden(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
%% ====================================================================
forbidden(Req, #rstate{module = Mod, resource = Resource, client = Client} = State) ->
    {ResId, Req2} = get_res_id(Req, State),
    {BinMethod, Req3} = cowboy_req:method(Req2),
    Method = binary_to_method(BinMethod),

    Forbidden = not Mod:is_authorized(Resource, Method, ResId, Client),
    {Forbidden, Req3, State}.


%% is_authorized/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return whether the user is authorized to perform the action.
%% NOTE: The name and description of this function is actually misleading;
%% 401 Unauthorized is returned when there's been an *authentication* error,
%% and 403 Forbidden is returned when the already-authenticated client
%% is unauthorized to perform an operation.
%% @end
%% ====================================================================
-spec is_authorized(Req :: cowboy_req:req(), State :: rstate()) ->
    {true | {false, binary()}, cowboy_req:req(), rstate()}.
%% ====================================================================
is_authorized(Req, #rstate{noauth = NoAuth} = State) ->
    {BinMethod, Req2} = cowboy_req:method(Req),
    Method = binary_to_method(BinMethod),

    try
        case lists:member(Method, NoAuth) of
            true -> {true, Req, State#rstate{client = #client{}}};
            false ->
                PeerCert = case ssl:peercert(cowboy_req:get(socket, Req2)) of
                    {ok, PeerCert1} -> PeerCert1;
                    {error, no_peercert} -> throw({silent_error, Req2})
                end,

                ProviderId = case grpca:verify_provider(PeerCert) of
                    {ok, ProviderId1} -> ProviderId1;
                    {error, {bad_cert, Reason}} when is_atom(Reason) ->
                        ?warning("Attempted authentication with bad peer certificate: ~p", [Reason]),
                        throw({silent_error, Req2})
                end,

                {Authorization, Req3} = cowboy_req:header(<<"authorization">>, Req2),
                case Authorization of
                    undefined ->
                        Client = #client{type = provider, id = ProviderId},
                        {true, Req3, State#rstate{client = Client}};

                    <<"Bearer ", Token/binary>> ->
                        case auth_logic:validate_token({provider, ProviderId}, Token) of
                            {ok, UserId} ->
                                Client = #client{type = user, id = UserId},
                                {true, Req3, State#rstate{client = Client}};

                            {error, Reason1} ->
                                Description = case Reason1 of
                                    not_found -> <<"access token not found">>;
                                    expired -> <<"access token expired">>;
                                    bad_audience -> <<"token issued to a different audience">>
                                end,
                                throw({invalid_token, Description, Req3})
                        end;

                    _ ->
                        Description = <<"unknown authorization type: ", Authorization/binary>>,
                        throw({invalid_request, 400, Description, Req3})
                end
        end
    catch
        {silent_error, ReqX} -> %% As per RFC 6750 section 3.1
            {{false, <<"">>}, ReqX, State};

        {Error, Description1, ReqX} when is_atom(Error), is_binary(Description1) ->
            Body = mochijson2:encode([{error, Error}, {error_description, Description1}]),
            WWWAuthenticate = <<"error=", (atom_to_binary(Error, latin1))/binary>>,
            ReqY = cowboy_req:set_resp_body(Body, ReqX),
            {{false, WWWAuthenticate}, ReqY, State};

        {Error, StatusCode, Description1, ReqX} when is_atom(Error), is_binary(Description1) ->
            Body = mochijson2:encode([{error, Error}, {error_description, Description1}]),
            {ok, ReqY} = cowboy_req:reply(StatusCode, [], Body, ReqX),
            {halt, ReqY, State}
    end.


%% resource_exists/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return whether the resource exists.
%% @end
%% ====================================================================
-spec resource_exists(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
%% ====================================================================
resource_exists(Req, #rstate{module = Mod, resource = Resource} = State) ->
    {Method, Req2} = cowboy_req:method(Req),
    case Method of
        %% Global Registry REST API always creates new resource on POST
        <<"POST">> -> {false, Req2, State};
        _ ->
            {ResId, Req3} = get_res_id(Req2, State),
            {Exists, Req4} = Mod:resource_exists(Resource, ResId, Req3),
            {Exists, Req4, State}
    end.


%% accept_resource_json/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Process the request body of application/json content type.
%% @end
%% ====================================================================
-spec accept_resource_json(Req :: cowboy_req:req(), State :: rstate()) ->
    {{true, URL :: binary()} | boolean(), cowboy_req:req(), rstate()}.
%% ====================================================================
accept_resource_json(Req, #rstate{} = State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Data = try
        mochijson2:decode(Body, [{format, proplist}])
    catch
        _:_ -> malformed
    end,

    case Data =:= malformed of
        true ->
            Body = mochijson2:encode([
            {error, invalid_request},
            {error_description, <<"malformed JSON data">>}]),
            Req3 = cowboy_req:set_resp_body(Body, Req2),
            {false, Req3, State};

        false ->
            accept_resource(Data, Req2, State)
    end.


%% accept_resource_form/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Process the request body of application/x-www-form-urlencoded content type.
%% @end
%% ====================================================================
-spec accept_resource_form(Req :: cowboy_req:req(), State :: rstate()) ->
    {{true, URL :: binary()} | boolean(), cowboy_req:req(), rstate()}.
%% ====================================================================
accept_resource_form(Req, #rstate{} = State) ->
    {ok, Data, Req2} = cowboy_req:body_qs(Req),
    accept_resource(Data, Req2, State).


%% accept_resource/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
%% ====================================================================
-spec accept_resource(Data :: [proplists:property()], Req :: cowboy_req:req(),
                      State :: rstate()) ->
    {{true, URL :: binary()} | boolean(), cowboy_req:req(), rstate()}.
%% ====================================================================
accept_resource(Data, Req, #rstate{module = Mod, resource = Resource, client = Client} = State) ->
    {ResId, Req2} = get_res_id(Req, State),
    {BinMethod, Req3} = cowboy_req:method(Req2),
    Method = binary_to_method(BinMethod),

    try
        {Result, Req4} = Mod:accept_resource(Resource, Method, ResId, Data, Client, Req3),
        {Result, Req4, State}
    catch
        {rest_error, Error, ReqX} when is_atom(Error) ->
            Body = mochijson2:encode([{error, Error}]),
            ReqY = cowboy_req:set_resp_body(Body, ReqX),
            {false, ReqY, State};

        {rest_error, Error, Description, ReqX} when is_atom(Error), is_binary(Description) ->
            Body = mochijson2:encode([
                {error, Error},
                {error_description, Description}
            ]),
            ReqY = cowboy_req:set_resp_body(Body, ReqX),
            {false, ReqY, State}
    end.


%% provide_resource/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
%% ====================================================================
-spec provide_resource(Req :: cowboy_req:req(), State :: rstate()) ->
    {iodata(), cowboy_req:req(), rstate()}.
%% ====================================================================
provide_resource(Req, #rstate{module = Mod, resource = Resource, client = Client} = State) ->
    {ResId, Req2} = get_res_id(Req, State),
    {Data, Req3} = Mod:provide_resource(Resource, ResId, Client, Req2),
    JSON = mochijson2:encode(Data),
    {JSON, Req3, State}.


%% method_to_binary/1
%% ====================================================================
%% @doc Converts an atom representing a REST method to a binary representing
%% the method.
%% @end
%% ====================================================================
-spec method_to_binary(Method :: method()) -> binary().
%% ====================================================================
method_to_binary(post)   -> <<"POST">>;
method_to_binary(patch)  -> <<"PATCH">>;
method_to_binary(get)    -> <<"GET">>;
method_to_binary(put)    -> <<"PUT">>;
method_to_binary(delete) -> <<"DELETE">>.


%% binary_to_method/1
%% ====================================================================
%% @doc Converts a binary representing a REST method to an atom representing
%% the method.
%% @end
%% ====================================================================
-spec binary_to_method(BinMethod :: binary()) -> method().
%% ====================================================================
binary_to_method(<<"POST">>)   -> post;
binary_to_method(<<"PATCH">>)  -> patch;
binary_to_method(<<"GET">>)    -> get;
binary_to_method(<<"PUT">>)    -> put;
binary_to_method(<<"DELETE">>) -> delete.


%% get_res_id/2
%% ====================================================================
%% @doc Returns the resource id for the request or client's id if the resource
%% id is undefined.
%% @end
%% ====================================================================
-spec get_res_id(Req :: cowboy_req:req(), State :: rstate()) ->
    {ResId :: binary(), cowboy_req:req()}.
%% ====================================================================
get_res_id(Req, #rstate{client = #client{id = ClientId}}) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    ResId = case proplists:get_value(id, Bindings) of
        undefined -> ClientId;
        X -> X
    end,
    {ResId, Req2}.
