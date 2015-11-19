%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling the common RESTful logic. It implements
%%% Cowboy's rest pseudo-behavior, delegating specifics to submodules.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_handler).
-author("Konrad Zemek").

-include("handlers/rest_handler.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([init/3, allowed_methods/2, content_types_accepted/2, is_authorized/2,
    content_types_provided/2, delete_resource/2, delete_completed/2,
    accept_resource_json/2, accept_resource_form/2, provide_resource/2,
    rest_init/2, forbidden/2, resource_exists/2, requests_effective_state/1]).

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
-spec rest_init(Req :: cowboy_req:req(), Opts :: rstate()) ->
    {ok, cowboy_req:req(), rstate()}.
rest_init(Req, #rstate{} = Opts) ->
    {ok, Req, Opts}.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of allowed methods.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(Req :: cowboy_req:req(), State :: rstate()) ->
    {[binary()], cowboy_req:req(), rstate()}.
allowed_methods(Req, #rstate{methods = Methods} = State) ->
    BinMethods =
        case requests_effective_state(Req) of
            true -> [method_to_binary(M) || M <- Methods, M =:= get];
            false -> [method_to_binary(M) || M <- Methods]
        end,
    {BinMethods, Req, State}.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether POST is allowed when the resource doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(Req :: cowboy_req:req(), State :: rstate()) ->
    {Value, cowboy_req:req(), rstate()} when
    Value :: [{binary() | {Type, SubType, Params}, AcceptResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    AcceptResource :: atom().
content_types_accepted(Req, #rstate{} = State) ->
    {[
        {<<"application/json">>, accept_resource_json},
        {<<"application/x-www-form-urlencoded">>, accept_resource_form}
    ], Req, State}.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of content-types the resource provides.
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(Req :: cowboy_req:req(), State :: rstate()) ->
    {Value, cowboy_req:req(), rstate()} when
    Value :: [{binary() | {Type, SubType, Params}, ProvideResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    ProvideResource :: atom().
content_types_provided(Req, #rstate{} = State) ->
    {[{<<"application/json">>, provide_resource}], Req, State}.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Delete the resource.
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
delete_resource(Req, #rstate{module = Mod, resource = Resource} = State) ->
    {ResId, Req2} = get_res_id(Req, State),
    {Result, Req3} = Mod:delete_resource(Resource, ResId, Req2),
    {Result, Req3, State}.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Returns if there is a guarantee that the resource gets deleted immediately
%% from the system.
%% @end
%%--------------------------------------------------------------------
-spec delete_completed(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
delete_completed(Req, State) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether access to the resource is forbidden.
%% @see is_authorized/2
%% @end
%%--------------------------------------------------------------------
-spec forbidden(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
forbidden(Req, State) ->
    #rstate{module = Mod, resource = Res, client = Client} = State,

    {ResId, Req2} = get_res_id(Req, State),
    {BinMethod, Req3} = cowboy_req:method(Req2),
    Method = binary_to_method(BinMethod),

    Forbidden = not Mod:is_authorized(Res, Method, ResId, Client),
    {Forbidden, Req3, State}.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether the user is authorized to perform the action.
%% NOTE: The name and description of this function is actually misleading;
%% 401 Unauthorized is returned when there's been an *authentication* error,
%% and 403 Forbidden is returned when the already-authenticated client
%% is unauthorized to perform an operation.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), State :: rstate()) ->
    {true | {false, binary()}, cowboy_req:req(), rstate()}.
is_authorized(Req, #rstate{noauth = NoAuth, root = Root} = State) ->
    {BinMethod, Req2} = cowboy_req:method(Req),
    Method = binary_to_method(BinMethod),

    try
        case lists:member(Method, NoAuth) of
            true -> {true, Req, State#rstate{client = #client{}}};
            false ->
                PeerCert = case ssl:peercert(cowboy_req:get(socket, Req2)) of
                               {ok, PeerCert1} -> PeerCert1;
                               {error, no_peercert} ->
                                   throw({silent_error, Req2})
                           end,

                ProviderId = case grpca:verify_provider(PeerCert) of
                                 {ok, ProviderId1} -> ProviderId1;
                                 {error, {bad_cert, Reason}} ->
                                     ?warning("Attempted authentication with "
                                     "bad peer certificate: ~p", [Reason]),
                                     throw({silent_error, Req2})
                             end,

                {Macaroon, DischargeMacaroons, Req3} =
                    parse_macaroons_from_headers(Req2),

                case Macaroon of
                    undefined ->
                        Client = #client{type = provider, id = ProviderId},
                        {true, Req3, State#rstate{client = Client}};

                    _ ->
                        case auth_logic:validate_token(ProviderId, Macaroon,
                            DischargeMacaroons, BinMethod, Root) of

                            {ok, UserId} ->
                                Client = #client{type = user, id = UserId},
                                {true, Req3, State#rstate{client = Client}};

                            {error, Reason1} ->
                                ?info("Bad auth: ~p", [Reason1]),
                                throw({invalid_token, <<"access denied">>,
                                    Req3})
                        end
                end
        end
    catch
        {silent_error, ReqX} -> %% As per RFC 6750 section 3.1
            {{false, <<"">>}, ReqX, State};

        {Error, <<Description1>>, ReqX} when is_atom(Error) ->
            Body = json_utils:encode([{error, Error},
                {error_description, Description1}]),

            WWWAuthenticate =
                <<"error=", (atom_to_binary(Error, latin1))/binary>>,

            ReqY = cowboy_req:set_resp_body(Body, ReqX),
            {{false, WWWAuthenticate}, ReqY, State};

        {Error, StatusCode, <<Description1>>, ReqX} when is_atom(Error) ->
            Body = json_utils:encode([{error, Error},
                {error_description, Description1}]),

            {ok, ReqY} = cowboy_req:reply(StatusCode, [], Body, ReqX),
            {halt, ReqY, State}
    end.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether the resource exists.
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
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

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body of application/json content type.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource_json(Req :: cowboy_req:req(), State :: rstate()) ->
    {{true, URL :: binary()} | boolean(), cowboy_req:req(), rstate()}.
accept_resource_json(Req, #rstate{} = State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Data = try
               json_utils:decode(Body)
           catch
               _:_ -> malformed
           end,

    case Data =:= malformed of
        true ->
            Body = json_utils:encode([
                {error, invalid_request},
                {error_description, <<"malformed JSON data">>}]),
            Req3 = cowboy_req:set_resp_body(Body, Req2),
            {false, Req3, State};

        false ->
            accept_resource(Data, Req2, State)
    end.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body of application/x-www-form-urlencoded content type.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource_form(Req :: cowboy_req:req(), State :: rstate()) ->
    {{true, URL :: binary()} | boolean(), cowboy_req:req(), rstate()}.
accept_resource_form(Req, #rstate{} = State) ->
    {ok, Data, Req2} = cowboy_req:body_qs(Req),
    accept_resource(Data, Req2, State).

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Data :: [proplists:property()], Req :: cowboy_req:req(),
    State :: rstate()) ->
    {{true, URL :: binary()} | boolean(), cowboy_req:req(), rstate()}.
accept_resource(Data, Req, State) ->
    #rstate{module = Mod, resource = Resource, client = Client} = State,

    {ResId, Req2} = get_res_id(Req, State),
    {BinMethod, Req3} = cowboy_req:method(Req2),
    Method = binary_to_method(BinMethod),

    try
        {Result, Req4} = Mod:accept_resource(Resource, Method, ResId, Data,
            Client, Req3),

        {Result, Req4, State}
    catch
        {rest_error, Error, ReqX}
            when is_atom(Error) ->

            Body = json_utils:encode([{error, Error}]),
            ReqY = cowboy_req:set_resp_body(Body, ReqX),
            {false, ReqY, State};


        {rest_error, Error, Description, ReqX}
            when is_atom(Error), is_binary(Description) ->

            Body = json_utils:encode([
                {error, Error},
                {error_description, Description}
            ]),
            ReqY = cowboy_req:set_resp_body(Body, ReqX),
            {false, ReqY, State}
    end.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rstate()) ->
    {iodata(), cowboy_req:req(), rstate()}.
provide_resource(Req, State) ->
    #rstate{module = Mod, resource = Resource, client = Client} = State,

    {ResId, Req2} = get_res_id(Req, State),
    {Data, Req3} = Mod:provide_resource(Resource, ResId, Client, Req2),
    JSON = json_utils:encode(Data),
    {JSON, Req3, State}.

%%--------------------------------------------------------------------
%% @doc Determines whether an "effective" state has been requested through
%% a query parameter. Effective state can mean different things for different
%% rest modules, but the state universally reduces allowed methods to GET only.
%% @end
%%--------------------------------------------------------------------
-spec requests_effective_state(Req :: cowboy_req:req()) ->
    boolean().
requests_effective_state(Req) ->
    % @todo change to qs_parse on Cowboy upgrade
    {QsVals, _} = cowboy_req:qs_vals(Req),
    case lists:keyfind(<<"effective">>, 1, QsVals) of
        {_, <<"true">>} -> true;
        {_, true} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Converts an atom representing a REST method to a binary representing
%% the method.
%% @end
%%--------------------------------------------------------------------
-spec method_to_binary(Method :: method()) -> binary().
method_to_binary(post) -> <<"POST">>;
method_to_binary(patch) -> <<"PATCH">>;
method_to_binary(get) -> <<"GET">>;
method_to_binary(put) -> <<"PUT">>;
method_to_binary(delete) -> <<"DELETE">>.

%%--------------------------------------------------------------------
%% @doc Converts a binary representing a REST method to an atom representing
%% the method.
%% @end
%%--------------------------------------------------------------------
-spec binary_to_method(BinMethod :: binary()) -> method().
binary_to_method(<<"POST">>) -> post;
binary_to_method(<<"PATCH">>) -> patch;
binary_to_method(<<"GET">>) -> get;
binary_to_method(<<"PUT">>) -> put;
binary_to_method(<<"DELETE">>) -> delete.

%%--------------------------------------------------------------------
%% @doc Returns the resource id for the request or client's id if the resource
%% id is undefined.
%% @end
%%--------------------------------------------------------------------
-spec get_res_id(Req :: cowboy_req:req(), State :: rstate()) ->
    {ResId :: binary(), cowboy_req:req()}.
get_res_id(Req, #rstate{client = #client{id = ClientId}}) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    ResId = case proplists:get_value(id, Bindings) of
                undefined -> ClientId;
                X -> X
            end,
    {ResId, Req2}.

%%--------------------------------------------------------------------
%% @doc Parses macaroon and discharge macaroons out of request's
%% headers.
%% @end
%%--------------------------------------------------------------------
-spec parse_macaroons_from_headers(Req :: cowboy_req:req()) ->
    {Macaroon :: macaroon:macaroon() | undefined,
        DischargeMacaroons :: [macaroon:macaroon()], cowboy_req:req()} |
    no_return().
parse_macaroons_from_headers(Req) ->
    {SerializedMacaroon, Req2} = cowboy_req:header(<<"macaroon">>, Req),
    {SerializedDischarges, Req3} =
        cowboy_req:header(<<"discharge-macaroons">>, Req2),

    Macaroon =
        case SerializedMacaroon of
            <<_/binary>> -> deserialize_macaroon(SerializedMacaroon, Req3);
            _ -> undefined
        end,

    DischargeMacaroons =
        case SerializedDischarges of
            <<>> ->
                [];

            <<_/binary>> ->
                Split = binary:split(SerializedDischarges, <<" ">>, [global]),
                [deserialize_macaroon(S, Req3) || S <- Split];

            _ ->
                []
        end,

    {Macaroon, DischargeMacaroons, Req3}.

%%--------------------------------------------------------------------
%% @doc Deserializes a macaroon and throws on error.
%% @end
%%--------------------------------------------------------------------
-spec deserialize_macaroon(Data :: binary(), Req :: cowboy_req:req()) ->
    macaroon:macaroon() | no_return().
deserialize_macaroon(Data, Req) ->
    case macaroon:deserialize(Data) of
        {ok, M} -> M;
        {error, Reason} ->
            throw({invalid_token, atom_to_binary(Reason, latin1), Req})
    end.
