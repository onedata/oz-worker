%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: The module handling /provider/* rest endpoints.
%% @end
%% ===================================================================
-module(provider_handler).
-author("Konrad Zemek").

-include("registered_names.hrl").

%% The state of the request.
%% `resource` :: atom() is the name of the requested resource
%% `provider_id` :: binary() is the binary string containing provider's id.
-record(state, {resource, provider_id}).

%% API
-export([init/3, allowed_methods/2, content_types_accepted/2, is_authorized/2,
    content_types_provided/2, delete_resource/2,
    accept_resource/2, provide_resource/2, rest_init/2, routes/0]).


%% routes/0
%% ====================================================================
%% @doc Returns a PathList understood by Cowboy of routes supported by this
%% handler.
%% @end
-spec routes() -> [Path] when Path :: {PathMatch :: string() | binary(),
                                       Handler :: module(), Opts :: #state{}}.
%% ====================================================================
routes() ->
    [
        {"/provider", ?MODULE, #state{resource = main}},
        {"/provider/create", ?MODULE, #state{resource = create}},
        {"/provider/supportSpace", ?MODULE, #state{resource = support_space}}
    ].


%% init/3
%% ====================================================================
%% @doc Cowboy callback function.
%% Upgrade the protocol to cowboy_rest.
%% @end
-spec init({TransportName :: tcp | ssl | atom(), ProtocolName :: http | atom()},
           Req :: cowboy_req:req(), Opts :: any()) ->
    {upgrade, protocol, cowboy_rest}.
%% ====================================================================
init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.


%% rest_init/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Initialize the state for this request.
%% @end
-spec rest_init(Req :: cowboy_req:req(), Opts :: #state{}) ->
    {ok, cowboy_req:req(), #state{}}.
%% ====================================================================
rest_init(Req, #state{} = Opts) ->
    {ok, Req, Opts}.


%% allowed_methods/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return the list of allowed methods.
%% @end
-spec allowed_methods(Req :: cowboy_req:req(), State :: #state{}) ->
    {[binary()], cowboy_req:req(), #state{}}.
%% ====================================================================
allowed_methods(Req, #state{resource = main} = State) ->
    {[<<"POST">>, <<"GET">>, <<"DELETE">>], Req, State};
allowed_methods(Req, #state{} = State) ->
    {[<<"POST">>], Req, State}.


%% content_types_accepted/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return whether POST is allowed when the resource doesn't exist.
%% @end
-spec content_types_accepted(Req :: cowboy_req:req(), State :: #state{}) ->
    {Value, cowboy_req:req(), #state{}} when
    Value :: [{binary() | {Type, SubType, Params}, AcceptResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    AcceptResource :: atom().
%% ====================================================================
content_types_accepted(Req, #state{} = State) ->
    {[{<<"application/json">>, accept_resource}], Req, State}.


%% content_types_provided/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return the list of content-types the resource provides.
%% @end
-spec content_types_provided(Req :: cowboy_req:req(), State :: #state{}) ->
    {Value, cowboy_req:req(), #state{}} when
    Value :: [{binary() | {Type, SubType, Params}, ProvideResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    ProvideResource :: atom().
%% ====================================================================
content_types_provided(Req, #state{} = State) ->
    {[{<<"application/json">>, provide_resource}], Req, State}.


%% delete_resource/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Delete the resource.
%% @end
-spec delete_resource(Req :: cowboy_req:req(), State :: #state{}) ->
    {boolean(), cowboy_req:req(), #state{}}.
%% ====================================================================
delete_resource(Req, #state{provider_id = ProviderId} = State) ->
    ok = user_logic:unregister_user(ProviderId),
    {true, Req, State}.


%% is_authorized/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Return whether the user is authorized to perform the action.
%% @end
-spec is_authorized(Req :: cowboy_req:req(), State :: #state{}) ->
    {true | {false, binary()}, cowboy_req:req(), #state{}}.
%% ====================================================================
%% @todo: Check authentication. If the user is not authenticated, then he
%% can only access POST user/create method; otherwise return true.
is_authorized(Req, #state{resource = main} = State) ->
    {true, Req, State};
is_authorized(Req, #state{} = State) ->
    {UserID, Req2} = authenticate(Req),
    case UserID of
        undefined ->
            {{false, <<"wut">>}, Req2, State}; %% @todo
        X ->
            {true, Req2, State#state{provider_id = X}}
    end.


%% accept_resource/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
-spec accept_resource(Req :: cowboy_req:req(), State :: #state{}) ->
    {true | false, cowboy_req:req(), #state{}}.
%% ====================================================================
accept_resource(Req, #state{} = State) ->
    {ok, JSON, Req2} = cowboy_req:body(Req),
    Data = mochijson2:decode(JSON, [{format, proplist}]),
    case process_data(Data, State) of
        {true, Response} ->
            ResponseJSON = mochijson2:encode(Response),
            Req3 = cowboy_req:set_resp_body(ResponseJSON, Req2),
            {true, Req3, State};
        Boolean ->
            {Boolean, Req2, State}
    end.


%% provide_resource/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
-spec provide_resource(Req :: cowboy_req:req(), State :: #state{}) ->
    {iodata(), cowboy_req:req(), #state{}}.
%% ====================================================================
provide_resource(Req, #state{resource = main, provider_id = ProviderId} = State) ->
    {ok, Data} = provider_logic:get_data(ProviderId),
    JSON = mochijson2:encode(Data),
    {JSON, Req, State}.


%% process_data/2
%% ====================================================================
%% @doc Cowboy callback function.
%% Act upon data submitted in POST request.
%% @end
-spec process_data(Data :: [proplists:property()], State :: #state{}) ->
    {true, [proplists:property()]} | true | false.
%% ====================================================================
process_data(Data, #state{resource = create}) ->
    [{<<"url">>, URL}] = Data,
    {ok, ProviderId} = provider_logic:register_provider(URL),
    {true, [{providerId, ProviderId}]};
process_data(Data, #state{resource = support_space, provider_id = ProviderId}) ->
    [{<<"token">>, Token}] = Data,
    {ok, SpaceId} = provider_logic:support_space(ProviderId, Token),
    {true, [{spaceId, SpaceId}]};
process_data(Data, #state{resource = main, provider_id = ProviderId}) ->
    ok = provider_logic:modify_data(ProviderId, Data),
    true.


%% @todo: stub for proper authentication
-spec authenticate(Req :: cowboy_req:req()) -> {binary() | undefined, cowboy_req:req()}.
authenticate(Req) ->
    cowboy_req:header(<<"providerid">>, Req).
