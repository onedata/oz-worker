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
        {<<"/openid/token">>, M, S#rstate{resource = token, methods = [post]}}
    ].


%% is_authorized/4
%% ====================================================================
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec is_authorized(Resource :: atom(), Method :: method(),
                    ProviderId :: binary() | undefined, Client :: client()) ->
    boolean().
%% ====================================================================
is_authorized(_, _, _ProviderId, #client{type = provider}) ->
    true.


%% resource_exists/3
%% ====================================================================
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec resource_exists(Resource :: atom(), ProviderId :: binary() | undefined,
                      Req :: cowboy_req:req()) -> boolean().
%% ====================================================================
resource_exists(_, _ProviderId, _Req) ->
    true.


%% accept_resource/6
%% ====================================================================
%% @doc Processes data submitted by a client through POST, PATCH on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec accept_resource(Resource :: atom(), Method :: method(),
                      ProviderId :: binary() | undefined,
                      Data :: [proplists:property()], Client :: client(),
                      Req :: cowboy_req:req()) ->
    {true, {url, URL :: binary()} | {data, Data :: [proplists:property()]}} |
        boolean().
%% ====================================================================
accept_resource(token, post, ProviderId, Data, _Client, Req) ->
%%     {ok, {<<"application">>, <<"x-www-form-urlencoded">>}, _Req2} =
%%         cowboy_req:parse_header(<<"content-type">>, Req), %%@todo: req2, return instead of breaking
    GrantType = proplists:get_value(<<"grant_type">>, Data),
    Code = proplists:get_value(<<"code">>, Data),
    if
        GrantType =/= <<"authorization_code">> -> false; %% @todo: refresh
        Code =:= undefined -> false;
        true ->
            {true, {data, auth_logic:grant_token(ProviderId, Code)}}
    end.


%% provide_resource/4
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(Resource :: atom(), ProviderId :: binary() | undefined,
                       Client :: client(), Req :: cowboy_req:req()) ->
    Data :: [proplists:property()].
%% ====================================================================
provide_resource(auth, _ProviderId, _Client, _Req) ->
    []. %% @todo: reply with not-implemented


%% delete_resource/3
%% ====================================================================
%% @doc Deletes the resource identified by the SpaceId parameter.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(Resource :: atom(), ProviderId :: binary() | undefined,
                      Req :: cowboy_req:req()) -> boolean().
%% ====================================================================
delete_resource(_, _ProviderId, _Req) ->
    true.
