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


%% API
-export([init/3, allowed_methods/2, content_types_accepted/2, is_authorized/2,
    content_types_provided/2, delete_resource/2, accept_resource/2,
    provide_resource/2, rest_init/2, forbidden/2, resource_exists/2]).


%% init/3
%% ====================================================================
%% @doc Cowboy callback function.
%% Upgrade the protocol to cowboy_rest.
%% @end
%% ====================================================================
-spec init({TransportName :: tcp | ssl | atom(), ProtocolName :: http | atom()},
           Req :: cowboy_req:req(), Opts :: any()) ->
    {upgrade, protocol, cowboy_rest}.
%% ====================================================================
init(_Transport, _Req, _Opts) -> %% @todo: Only accept ssl
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
    {[{<<"application/json">>, accept_resource}], Req, State}.


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
    Result = Mod:delete_resource(Resource, ResId, Req2),
    {Result, Req2, State}.


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
is_authorized(Req, #rstate{} = State) -> %% @todo: proper certificate-based authentication
    {UserId, Req2} = cowboy_req:header(<<"userid">>, Req),
    {ProviderId, Req3} = cowboy_req:header(<<"providerid">>, Req2),
    Client = if
        UserId =/= undefined -> #client{type = user, id = UserId};
        ProviderId =/= undefined -> #client{type = provider, id = ProviderId};
        true -> #client{}
    end,
    {true, Req3, State#rstate{client = Client}}.


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
            Exists = Mod:resource_exists(Resource, ResId, Req3),
            {Exists, Req3, State}
    end.


%% accept_resource/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
%% ====================================================================
-spec accept_resource(Req :: cowboy_req:req(), State :: rstate()) ->
    {{true, URL :: binary()} | boolean(), cowboy_req:req(), rstate()}.
%% ====================================================================
accept_resource(Req, #rstate{module = Mod, resource = Resource, client = Client} = State) ->
    {ResId, Req2} = get_res_id(Req, State),
    {BinMethod, Req3} = cowboy_req:method(Req2),
    {ok, Body, Req4} = cowboy_req:body(Req3),
    Method = binary_to_method(BinMethod),
    Data = mochijson2:decode(Body, [{format, proplist}]),

    AcceptResult = Mod:accept_resource(Resource, Method, ResId, Data, Client, Req4),
    {AcceptResult, Req4, State}.


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
    Data = Mod:provide_resource(Resource, ResId, Client, Req2),
    JSON = mochijson2:encode(Data),
    {JSON, Req2, State}.


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
