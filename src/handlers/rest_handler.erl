%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: The module handling the common RESTful logic. It implements
%% Cowboy's rest pseudo-behavior, delegating specifics to submodules.
%% @end
%% ===================================================================
-module(rest_handler).
-author("Konrad Zemek").

-include("handlers/rest_handler.hrl").


%% API
-export([init/3, allowed_methods/2, content_types_accepted/2, is_authorized/2,
    content_types_provided/2, delete_resource/2, accept_resource/2,
    provide_resource/2, rest_init/2, forbidden/2]).


%% init/3
%% ====================================================================
%% @doc Cowboy callback function.
%% Upgrade the protocol to cowboy_rest.
%% @end
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
-spec rest_init(Req :: cowboy_req:req(), Opts :: #reqstate{}) ->
    {ok, cowboy_req:req(), #reqstate{}}.
%% ====================================================================
rest_init(Req, #reqstate{} = Opts) ->
    {ok, Req, Opts}.


%% allowed_methods/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return the list of allowed methods.
%% @end
-spec allowed_methods(Req :: cowboy_req:req(), State :: #reqstate{}) ->
    {[binary()], cowboy_req:req(), #reqstate{}}.
%% ====================================================================
allowed_methods(Req, #reqstate{resource = main} = State) ->
    {[<<"POST">>, <<"GET">>, <<"DELETE">>], Req, State};
allowed_methods(Req, #reqstate{} = State) ->
    {[<<"POST">>], Req, State}.


%% content_types_accepted/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return whether POST is allowed when the resource doesn't exist.
%% @end
-spec content_types_accepted(Req :: cowboy_req:req(), State :: #reqstate{}) ->
    {Value, cowboy_req:req(), #reqstate{}} when
    Value :: [{binary() | {Type, SubType, Params}, AcceptResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    AcceptResource :: atom().
%% ====================================================================
content_types_accepted(Req, #reqstate{} = State) ->
    {[{<<"application/json">>, accept_resource}], Req, State}.


%% content_types_provided/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return the list of content-types the resource provides.
%% @end
-spec content_types_provided(Req :: cowboy_req:req(), State :: #reqstate{}) ->
    {Value, cowboy_req:req(), #reqstate{}} when
    Value :: [{binary() | {Type, SubType, Params}, ProvideResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    ProvideResource :: atom().
%% ====================================================================
content_types_provided(Req, #reqstate{} = State) ->
    {[{<<"application/json">>, provide_resource}], Req, State}.


%% delete_resource/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Delete the resource.
%% @end
-spec delete_resource(Req :: cowboy_req:req(), State :: #reqstate{}) ->
    {boolean(), cowboy_req:req(), #reqstate{}}.
%% ====================================================================
delete_resource(Req, #reqstate{module = Mod} = State) ->
    {Id, Req2} = cowboy_req:binding(id, Req),
    ok = Mod:delete_resource(Id, State),
    {true, Req2, State}.


%% forbidden/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return whether access to the resource is forbidden.
%% @see is_authorized/2
%% @end
-spec forbidden(Req :: cowboy_req:req(), State :: #reqstate{}) ->
    {boolean(), cowboy_req:req(), #reqstate{}}.
%% ====================================================================
forbidden(Req, #reqstate{module = Mod} = State) ->
    {Id, Req2} = cowboy_req:binding(id, Req),
    {Mod:is_authorized(Id, State), Req2, State}.


%% is_authorized/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return whether the user is authorized to perform the action.
%% NOTE: The name and description of this function is actually misleading;
%% 401 Unauthorized is returned when there's been an *authentication* error,
%% and 403 Forbidden is returned when the already-authenticated client
%% is unauthorized to perform an operation.
%% @end
-spec is_authorized(Req :: cowboy_req:req(), State :: #reqstate{}) ->
    {true | {false, binary()}, cowboy_req:req(), #reqstate{}}.
%% ====================================================================
is_authorized(Req, #reqstate{} = State) -> %% @todo: proper SSL authentication
    {UserId, Req2} = cowboy_req:header(<<"userid">>, Req),
    {ProviderId, Req3} = cowboy_req:header(<<"providerid">>, Req2),
    Client = if
        UserId =/= undefined -> {user, UserId};
        ProviderId =/= undefined -> {provider, ProviderId}
    end,
    {true, Req3, State#reqstate{client = Client}}.


%% accept_resource/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
-spec accept_resource(Req :: cowboy_req:req(), State :: #reqstate{}) ->
    {true | false, cowboy_req:req(), #reqstate{}}.
%% ====================================================================
accept_resource(Req, #reqstate{module = Mod} = State) ->
    {ok, JSON, Req2} = cowboy_req:body(Req),
    {Id, Req3} = cowboy_req:binding(id, Req2),
    Data = mochijson2:decode(JSON, [{format, proplist}]),
    case Mod:accept_resource(Id, Data, State) of
        {ok, Response} ->
            ResponseJSON = mochijson2:encode(Response),
            Req4 = cowboy_req:set_resp_body(ResponseJSON, Req3),
            {true, Req4, State};
        ok ->
            {true, Req3, State}
    end.


%% provide_resource/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
-spec provide_resource(Req :: cowboy_req:req(), State :: #reqstate{}) ->
    {iodata(), cowboy_req:req(), #reqstate{}}.
%% ====================================================================
provide_resource(Req, #reqstate{module = Mod} = State) ->
    {Id, Req2} = cowboy_req:binding(id, Req),
    {ok, Data} = Mod:provide_resource(Id, State),
    JSON = mochijson2:encode(Data),
    {JSON, Req2, State}.
