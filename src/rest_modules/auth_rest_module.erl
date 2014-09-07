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
        {<<"/openid/client/access_code">>, M, S#rstate{resource = ascode, methods = [get]}},
        {<<"/openid/client/tokens">>, M, S#rstate{resource = ctokens, methods = [post, get], noauth = [post]}},
        {<<"/openid/client/tokens/:accessId">>, M, S#rstate{resource = ctoken, methods = [delete]}},
        {<<"/openid/client/verify">>, M, S#rstate{resource = verify, methods = [post]}},
        {<<"/openid/provider/tokens">>, M, S#rstate{resource = ptokens, methods = [post]}}
    ].


%% is_authorized/4
%% ====================================================================
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec is_authorized(Resource :: atom(), Method :: method(),
                    Id :: binary() | undefined, Client :: client()) ->
    boolean().
%% ====================================================================
is_authorized(Resource, _Method, _Id, #client{type = user})
        when Resource =:= ascode orelse Resource =:= ctokens orelse Resource =:= ctoken ->
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
-spec resource_exists(Resource :: atom(), Id :: binary() | undefined,
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
-spec accept_resource(Resource :: atom(), Method :: method(),
                      ProviderId :: binary() | undefined,
                      Data :: [proplists:property()], Client :: client(),
                      Req :: cowboy_req:req()) ->
    {{true, {url, URL :: binary()} | {data, Data :: [proplists:property()]}} |
        boolean(), cowboy_req:req()} | no_return().
%% ====================================================================
accept_resource(Resource, post, Id, Data, _Client, Req)
        when Resource =:= ptokens orelse Resource =:= ctokens ->
    TokenClient = case Resource of ptokens -> {provider, Id}; ctokens -> native end,

    GrantType = rest_module_helper:assert_key(<<"grant_type">>, Data),
    case GrantType of
        <<"authorization_code">> ->
            Code = rest_module_helper:assert_key(<<"code">>, Data),
            {{true, {data, auth_logic:grant_tokens(TokenClient, Code)}}, Req};

        <<"refresh_token">> ->
            error(not_implemented);

        _ ->
            rest_module_helper:report_invalid_value(<<"grant_type">>, GrantType)
    end;
accept_resource(verify, post, _ProviderId, Data, _Client, Req) ->
    UserId = rest_module_helper:assert_key(<<"userId">>, Data),
    Secret = rest_module_helper:assert_key(<<"secret">>, Data),
    Verified = auth_logic:verify(UserId, Secret),
    {{true, {data, [{verified, Verified}]}}, Req}.


%% provide_resource/4
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(Resource :: atom(), Id :: binary() | undefined,
                       Client :: client(), Req :: cowboy_req:req()) ->
    {Data :: [proplists:property()], cowboy_req:req()}.
%% ====================================================================
provide_resource(ascode, UserId, _Client, Req) ->
    {[{accessCode, auth_logic:gen_auth_code(UserId)}], Req};
provide_resource(ctokens, UserId, _Client, Req) ->
    {[{tokenInfo, auth_logic:get_user_tokens(UserId)}], Req}.


%% delete_resource/3
%% ====================================================================
%% @doc Deletes the resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(Resource :: atom(), ResId :: binary() | undefined,
                      Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
%% ====================================================================
delete_resource(ctoken, _UserId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {_, AccessId} = lists:keyfind(accessId, 1, Bindings),
    ok = auth_logic:delete_access(AccessId),
    {true, Req2}.
