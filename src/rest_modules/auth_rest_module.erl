%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module handling logic behind /auth REST resources.
%% ===================================================================
-module(auth_rest_module).
-author("Konrad Zemek").

-include("handlers/rest_handler.hrl").

-behavior(rest_module_behavior).


-type provided_resource()  :: authcode | ctokens | ptokens.
-type accepted_resource()  :: ctoken | ptoken | ctokens | ptokens | verify.
-type removable_resource() :: ctoken | ptoken.
-type resource() :: provided_resource() | accepted_resource() | removable_resource().


%% API
-export([routes/0, is_authorized/4, accept_resource/6, provide_resource/4,
    delete_resource/3, resource_exists/3]).


%% routes/0
%% ====================================================================
%% @doc Returns a Cowboy-understandable PathList of routes supported by a module
%% implementing this behavior.
%% @see rest_module_behavior
%% @end
-spec routes() ->
    [{PathMatch :: binary(), rest_handler, State :: rstate()}].
%% ====================================================================
routes() ->
    S = #rstate{module = ?MODULE},
    M = rest_handler,
    [
        {<<"/openid/client/authorization_code">>, M, S#rstate{resource = authcode, methods = [get]}},
        {<<"/openid/client/tokens">>,             M, S#rstate{resource = ctokens,  methods = [post, get], noauth = [post]}},
        {<<"/openid/client/tokens/:id">>,         M, S#rstate{resource = ctoken,   methods = [patch, delete]}},
        {<<"/openid/client/verify">>,             M, S#rstate{resource = verify,   methods = [post]}},
        {<<"/openid/provider/tokens">>,           M, S#rstate{resource = ptokens,  methods = [post, get]}},
        {<<"/openid/provider/tokens/:id">>,       M, S#rstate{resource = ptoken,   methods = [patch, delete]}}
    ].


%% is_authorized/4
%% ====================================================================
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec is_authorized(Resource :: resource(), Method :: method(),
                    Id :: binary() | undefined, Client :: client()) ->
    boolean().
%% ====================================================================
is_authorized(Resource, _Method, _Id, #client{type = user})
        when Resource =:= authcode orelse Resource =:= ctokens
        orelse Resource =:= ctoken orelse Resource =:= ptoken ->
    true;
is_authorized(ptokens, get, _id, #client{type = user}) ->
    true;
is_authorized(Resource, post, _Id, #client{type = provider})
        when Resource =:= verify orelse Resource =:= ptokens ->
    true;
is_authorized(ctokens, post, _, _) ->
    true;
is_authorized(_, _, _, _) ->
    false.


%% resource_exists/3
%% ====================================================================
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec resource_exists(Resource :: resource(), Id :: binary() | undefined,
                      Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
%% ====================================================================
resource_exists(Resource, UserId, Req) when Resource =:= ptoken orelse Resource =:= ctoken  ->
    AccessType = case Resource of ptoken -> provider; ctoken -> client end,
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {accessId, AccessId} = lists:keyfind(accessId, 1, Bindings),
    {auth_logic:has_access(UserId, AccessId, AccessType), Req2};
resource_exists(_, _Id, Req) ->
    {true, Req}.


%% accept_resource/6
%% ====================================================================
%% @doc Processes data submitted by a client through POST, PATCH, PUT on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec accept_resource(Resource :: accepted_resource(), Method :: accept_method(),
                      ResourceId :: binary() | undefined, Data :: data(),
                      Client :: client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().
%% ====================================================================
accept_resource(Resource, post, Id, Data, _Client, Req)
        when Resource =:= ptokens orelse Resource =:= ctokens ->
    TokenClient = case Resource of ptokens -> {provider, Id}; ctokens -> native end,

    GrantType = rest_module_helper:assert_key(<<"grant_type">>, Data, binary, Req),
    Result = case GrantType of
        <<"authorization_code">> ->
            Code = rest_module_helper:assert_key(<<"code">>, Data, binary, Req),
            ClientName =
                case proplists:get_value(<<"client_name">>, Data) of
                    undefined when Resource =:= ptokens -> ok;
                    undefined when Resource =:= ctokens -> rest_module_helper:report_missing_key(<<"client_name">>, Req);
                    Value when is_binary(Value) -> Value;
                    Value -> rest_module_helper:report_invalid_value(<<"client_name">>, Value, Req)
                end,

            auth_logic:grant_tokens(TokenClient, Code, ClientName);

        <<"refresh_token">> ->
            Token = rest_module_helper:assert_key(<<"refresh_token">>, Data, binary, Req),
            auth_logic:refresh_tokens(TokenClient, Token);

        _ ->
            Description = <<"the authorization grant type '", GrantType/binary,
                            "' is not supported by the authorization server">>,
            rest_module_helper:report_error(unsupported_grant_type, Description, Req)
    end,
    case Result of
        {ok, Data1} ->
            Body = mochijson2:encode(Data1),
            Req2 = cowboy_req:set_resp_body(Body, Req),
            Req3 = cowboy_req:set_resp_header(<<"Cache-Control">>, <<"no-store">>, Req2),
            Req4 = cowboy_req:set_resp_header(<<"Pragma">>, <<"no-cache">>, Req3),
            {true, Req4};

        {error, Reason} ->
            Description1 = case Reason of
                refresh_invalid_or_revoked -> <<"refresh token invalid or revoked">>;
                refresh_wrong_client       -> <<"refresh token issued to another client">>;
                invalid_or_expired -> <<"authorization code invalid or expired">>;
                expired            -> <<"authorization code expired">>;
                wrong_client       -> <<"authorization code issued to another client">>
            end,
            rest_module_helper:report_error(invalid_grant, Description1, Req)
    end;
accept_resource(Resource, patch, AccessId, Data, _Client, Req)
        when Resource =:= ptoken orelse Resource =:= ctoken ->
    ok = auth_logic:modify_access(AccessId, Data),
    {true, Req};
accept_resource(verify, post, _ProviderId, Data, _Client, Req) ->
    UserId = rest_module_helper:assert_key(<<"userId">>, Data, binary, Req),
    Secret = rest_module_helper:assert_key(<<"secret">>, Data, binary, Req),
    Verified = auth_logic:verify(UserId, Secret),
    Body = mochijson2:encode([{verified, Verified}]),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {true, Req2}.


%% provide_resource/4
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(Resource :: provided_resource(), Id :: binary() | undefined,
                       Client :: client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.
%% ====================================================================
provide_resource(authcode, UserId, _Client, Req) ->
    {[{authorizationCode, auth_logic:gen_auth_code(UserId)}], Req};
provide_resource(Resource, UserId, _Client, Req) when Resource =:= ptokens orelse Resource =:= ctokens ->
    AccessType = case Resource of ptokens -> provider; ctokens -> client end,
    {[{tokenInfo, auth_logic:get_user_tokens(UserId, AccessType)}], Req}.


%% delete_resource/3
%% ====================================================================
%% @doc Deletes the resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(Resource :: removable_resource(),
                      ResId :: binary() | undefined, Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
%% ====================================================================
delete_resource(Resource, AccessId, Req) when Resource =:= ptoken orelse Resource =:= ctoken ->
    ok = auth_logic:delete_access(AccessId),
    {true, Req}.
