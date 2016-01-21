%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /provider REST resource.
%%%-------------------------------------------------------------------
-module(provider_rest_module).
-author("Konrad Zemek").

-include("handlers/rest_handler.hrl").
-include("registered_names.hrl").

-behavior(rest_module_behavior).


-type provided_resource() :: provider | provider_dev | spaces | nprovider | space | ip | ports.
-type accepted_resource() :: provider | spaces | ssupport.
-type removable_resource() :: provider | space.
-type resource() :: provided_resource() | accepted_resource() | removable_resource().

%% API
-export([routes/0, is_authorized/4, accept_resource/6, provide_resource/4,
    delete_resource/3, resource_exists/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a Cowboy-understandable PathList of routes supported by a module
%% implementing this behavior.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec routes() ->
    [{PathMatch :: binary(), rest_handler, State :: rstate()}].
routes() ->
    S = #rstate{module = ?MODULE, root = provider},
    M = rest_handler,
    [
        {<<"/provider">>, M, S#rstate{resource = provider, methods = [get, post, patch, delete], noauth = [post]}},
        {<<"/provider_dev">>, M, S#rstate{resource = provider_dev, methods = [post], noauth = [post]}},
        {<<"/provider/spaces">>, M, S#rstate{resource = spaces, methods = [get, post]}},
        {<<"/provider/:pid">>, M, S#rstate{resource = nprovider, methods = [get]}},
        {<<"/provider/spaces/support">>, M, S#rstate{resource = ssupport, methods = [post]}},
        {<<"/provider/spaces/:sid">>, M, S#rstate{resource = space, methods = [get, delete]}},
        {<<"/provider/token/:token">>, M, S#rstate{resource = token, methods = [get]}},
        {<<"/provider/test/check_my_ip">>, M, S#rstate{resource = ip, methods = [get], noauth = [get]}},
        {<<"/provider/test/check_my_ports">>, M, S#rstate{resource = ports, methods = [post], noauth = [post]}}
    ].

%%--------------------------------------------------------------------
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Resource :: resource(), Method :: method(),
    ProviderId :: binary() | undefined, Client :: rest_handler:client()) ->
    boolean().
is_authorized(ip, get, _, _) ->
    true;
is_authorized(ports, post, _, _) ->
    true;
is_authorized(provider, post, _, #client{type = undefined}) ->
    true;
is_authorized(provider_dev, _, _, _) ->
    {ok, true} =:= application:get_env(?APP_Name, dev_mode);
is_authorized(_, _, _, #client{type = provider}) ->
    true;
is_authorized(_, _, _, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: resource(), ProviderId :: binary() | undefined,
    Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
resource_exists(space, ProviderId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {sid, SID} = lists:keyfind(sid, 1, Bindings),
    {space_logic:has_provider(SID, ProviderId), Req2};
resource_exists(nprovider, _, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {pid, PID} = lists:keyfind(pid, 1, Bindings),
    {provider_logic:exists(PID), Req2};
resource_exists(_, _, Req) ->
    {true, Req}.

%%--------------------------------------------------------------------
%% @doc Processes data submitted by a client through POST, PATCH, PUT on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: accepted_resource(), Method :: accept_method(),
    ProviderId :: binary() | undefined, Data :: data(),
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().
accept_resource(provider_dev, post, _ProviderId, Data, _Client, Req) ->
    ClientName = rest_module_helper:assert_key(<<"clientName">>, Data, binary, Req),
    URLs = rest_module_helper:assert_key(<<"urls">>, Data, list_of_bin, Req),
    CSR = rest_module_helper:assert_key(<<"csr">>, Data, binary, Req),
    RedirectionPoint = rest_module_helper:assert_key(<<"redirectionPoint">>, Data, binary, Req),
    UUID = rest_module_helper:assert_key(<<"uuid">>, Data, binary, Req),

    % Create provider with given UUID - UUID is the same as the provider name.
    {ok, ProviderId, SignedPem} =
        dev_utils:create_provider_with_uuid(ClientName, URLs, RedirectionPoint, CSR, UUID),
    Body = json_utils:encode([{<<"providerId">>, ProviderId}, 
        {<<"certificate">>, SignedPem}]),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {true, Req2};
accept_resource(provider, post, _ProviderId, Data, _Client, Req) ->
    ClientName = rest_module_helper:assert_key(<<"clientName">>, Data, binary, Req),
    URLs = rest_module_helper:assert_key(<<"urls">>, Data, list_of_bin, Req),
    CSR = rest_module_helper:assert_key(<<"csr">>, Data, binary, Req),
    RedirectionPoint = rest_module_helper:assert_key(<<"redirectionPoint">>, Data, binary, Req),

    {ok, ProviderId, SignedPem} = provider_logic:create(ClientName, URLs, RedirectionPoint, CSR),
    Body = json_utils:encode([{<<"providerId">>, ProviderId},
        {<<"certificate">>, SignedPem}]),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {true, Req2};
accept_resource(provider, patch, ProviderId, Data, _Client, Req) ->
    rest_module_helper:assert_type(<<"clientName">>, Data, binary, Req),
    rest_module_helper:assert_type(<<"urls">>, Data, list_of_bin, Req),
    rest_module_helper:assert_type(<<"redirectionPoint">>, Data, binary, Req),
    ok = provider_logic:modify(ProviderId, Data),
    {true, Req};
accept_resource(spaces, post, _ProviderId, Data, Client, Req) ->
    spaces_rest_module:accept_resource(spaces, post, undefined, Data, Client, Req);
accept_resource(ssupport, post, ProviderId, Data, _Client, Req) ->
    Token = rest_module_helper:assert_key(<<"token">>, Data, binary, Req),
    Size = rest_module_helper:assert_key(<<"size">>, Data, pos_integer, Req),
    case token_logic:validate(Token, space_support_token) of
        false ->
            rest_module_helper:report_invalid_value(<<"token">>, Token, Req);
        {true, Macaroon} ->
            {ok, SpaceId} = space_logic:support(ProviderId, Macaroon, Size),
            {{true, <<"/provider/spaces/", SpaceId/binary>>}, Req}
    end;
accept_resource(ports, post, _ProviderId, Data, _Client, Req) ->
    case provider_logic:test_connection(Data) of
        {ok, ResultList} ->
            Body = json_utils:encode(ResultList),
            Req2 = cowboy_req:set_resp_body(Body, Req),
            {true, Req2};

        {error, bad_data} ->
            rest_module_helper:report_error(
                invalid_request, <<"bad data">>, Req)
    end.

%%--------------------------------------------------------------------
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: provided_resource(), ProviderId :: binary() | undefined,
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.
provide_resource(provider, ProviderId, _Client, Req) ->
    {ok, Provider} = provider_logic:get_data(ProviderId),
    {Provider, Req};
provide_resource(nprovider, _ProviderId, _Client, Req) ->
    {Bindings, _Req2} = cowboy_req:bindings(Req),
    {pid, PID} = lists:keyfind(pid, 1, Bindings),
    {ok, Provider} = provider_logic:get_data(PID),
    {Provider, Req};
provide_resource(spaces, ProviderId, _Client, Req) ->
    {ok, Spaces} = provider_logic:get_spaces(ProviderId),
    {Spaces, Req};
provide_resource(space, _ProviderId, _Client, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {sid, SID} = lists:keyfind(sid, 1, Bindings),
    {ok, Space} = space_logic:get_data(SID, provider),
    {Space, Req2};
provide_resource(ip, _ProviderId, _Client, Req) ->
    {{Ip, _Port}, Req2} = cowboy_req:peer(Req),
    {list_to_binary(inet_parse:ntoa(Ip)), Req2};
provide_resource(token, _UserId, _Client, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {token, Token} = lists:keyfind(token, 1, Bindings),
    {ok, Issuer} = token_logic:get_issuer(Token),
    {Issuer, Req2}.

%%--------------------------------------------------------------------
%% @doc Deletes the resource identified by the SpaceId parameter.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: removable_resource(),
    ProviderId :: binary() | undefined, Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
delete_resource(provider, ProviderId, Req) ->
    {provider_logic:remove(ProviderId), Req};
delete_resource(space, ProviderId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {sid, SID} = lists:keyfind(sid, 1, Bindings),
    {space_logic:remove_provider(SID, ProviderId), Req2}.
