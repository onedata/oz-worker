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


-type provided_resource()  :: authcode | ctokens.
-type accepted_resource()  :: ctokens | verify | ptokens.
-type removable_resource() :: ctoken.
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
        {<<"/openid/client/tokens/:accessId">>,   M, S#rstate{resource = ctoken,   methods = [delete]}},
        {<<"/openid/client/verify">>,             M, S#rstate{resource = verify,   methods = [post]}},
        {<<"/openid/provider/tokens">>,           M, S#rstate{resource = ptokens,  methods = [post]}}
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
        when Resource =:= authcode orelse Resource =:= ctokens orelse Resource =:= ctoken ->
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
resource_exists(ctoken, UserId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {accessId, AccessId} = lists:keyfind(accessId, 1, Bindings),
    {auth_logic:has_access(UserId, AccessId), Req2};
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
                      ProviderId :: binary() | undefined, Data :: data(),
                      Client :: client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().
%% ====================================================================
accept_resource(Resource, post, Id, Data, _Client, Req)
        when Resource =:= ptokens orelse Resource =:= ctokens ->
    TokenClient = case Resource of ptokens -> {provider, Id}; ctokens -> native end,

    GrantType = rest_module_helper:assert_key(<<"grant_type">>, Data, binary, Req),
    case GrantType of
        <<"authorization_code">> ->
            Code = rest_module_helper:assert_key(<<"code">>, Data, binary, Req),
            case auth_logic:grant_tokens(TokenClient, Code) of
                {ok, Data1} ->
                    Body = mochijson2:encode(Data1),
                    Req2 = cowboy_req:set_resp_body(Body, Req),
                    Req3 = cowboy_req:set_resp_header(<<"Cache-Control">>, <<"no-store">>, Req2),
                    Req4 = cowboy_req:set_resp_header(<<"Pragma">>, <<"no-cache">>, Req3),
                    {true, Req4};
                {error, Reason} ->
                    Description = case Reason of
                        invalid_or_expired -> <<"authorization code invalid or expired">>;
                        expired            -> <<"authorization code expired">>;
                        wrong_client       -> <<"authorization code issued to another client">>
                    end,
                    rest_module_helper:report_error(invalid_grant, Description, Req)
            end;

        <<"refresh_token">> ->
            error(not_implemented); %% @TODO: VFS-679

        _ ->
            Description = <<"the authorization grant type '", GrantType/binary,
                            "' is not supported by the authorization server">>,
            rest_module_helper:report_error(unsupported_grant_type, Description, Req)
    end;
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
provide_resource(ctokens, UserId, _Client, Req) ->
    {[{tokenInfo, auth_logic:get_user_tokens(UserId)}], Req}.


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
delete_resource(ctoken, _UserId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {_, AccessId} = lists:keyfind(accessId, 1, Bindings),
    ok = auth_logic:delete_access(AccessId),
    {true, Req2}.
