%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module handling logic behind /provider REST resource.
%% ===================================================================
-module(provider_rest_module).
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
        {<<"/provider">>,                   M, S#rstate{resource = provider,    methods = [get, post, patch, delete]}},
        {<<"/provider/spaces/">>,           M, S#rstate{resource = spaces,      methods = [get, post]   }},
        {<<"/provider/spaces/support">>,    M, S#rstate{resource = ssupport,    methods = [post]        }},
        {<<"/provider/spaces/:sid">>,       M, S#rstate{resource = space,       methods = [get, delete] }}
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
is_authorized(provider, post, _, #client{type = undefined}) ->
    true;
is_authorized(_, _, _, #client{type = provider}) ->
    true;
is_authorized(_, _, _, _) ->
    false.


%% resource_exists/3
%% ====================================================================
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec resource_exists(Resource :: atom(), ProviderId :: binary() | undefined,
                      Bindings :: [{atom(), any()}]) -> boolean().
%% ====================================================================
resource_exists(space, ProviderId, Bindings) ->
    SID = proplists:get_value(sid, Bindings),
    space_logic:has_provider(SID, ProviderId);
resource_exists(_, _, _) ->
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
                      Bindings :: [{atom(), any()}]) ->
    {true, URL :: binary()} | boolean().
%% ====================================================================
accept_resource(provider, post, _ProviderId, Data, _Client, _Bindings) ->
    URL = proplists:get_value(<<"url">>, Data),
    if
        URL =:= undefined -> false;
        true ->
            {ok, _} = provider_logic:create(URL),
            {true, <<"/provider">>}
    end;
accept_resource(provider, patch, ProviderId, Data, _Client, _Bindings) ->
    URL = proplists:get_value(<<"url">>, Data),
    if
        URL =:= undefined -> false;
        true ->
            ok = provider_logic:modify(ProviderId, URL),
            true
    end;
accept_resource(spaces, post, _ProviderId, Data, Client, Bindings) ->
    spaces_rest_module:accept_resource(spaces, post, undefined, Data, Client, Bindings);
accept_resource(ssupport, post, ProviderId, Data, _Client, _Bindings) ->
    Token = proplists:get_value(<<"token">>, Data),
    case token_logic:is_valid(Token, space_support_token) of
        false -> false;
        true ->
            {ok, SpaceId} = space_logic:support(ProviderId, Token),
            {true, <<"/provider/spaces/", SpaceId/binary>>}
    end.


%% provide_resource/4
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(Resource :: atom(), ProviderId :: binary() | undefined,
                       Client :: client(), Bindings :: [{atom(), any()}]) ->
    Data :: [proplists:property()].
%% ====================================================================
provide_resource(provider, ProviderId, _Client, _Bindings) ->
    {ok, Provider} = provider_logic:get_data(ProviderId),
    Provider;
provide_resource(spaces, ProviderId, _Client, _Bindings) ->
    {ok, Spaces} = provider_logic:get_spaces(ProviderId),
    Spaces;
provide_resource(space, _ProviderId, _Client, Bindings) ->
    SID = proplists:get_value(sid, Bindings),
    {ok, Space} = space_logic:get_data(SID, provider),
    Space.


%% delete_resource/3
%% ====================================================================
%% @doc Deletes the resource identified by the SpaceId parameter.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(Resource :: atom(), ProviderId :: binary() | undefined,
                      Bindings :: [{atom(), any()}]) -> boolean().
%% ====================================================================
delete_resource(provider, ProviderId, _Bindings) ->
    provider_logic:remove(ProviderId);
delete_resource(space, ProviderId, Bindings) ->
    SID = proplists:get_value(sid, Bindings),
    space_logic:remove_provider(SID, ProviderId).
