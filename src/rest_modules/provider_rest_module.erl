%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: The module handling logic behind /provider REST resource.
%% @end
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


%% is_authorized/2
%% ====================================================================
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec is_authorized(Method :: binary(), State :: #reqstate{}) -> boolean().
%% ====================================================================
is_authorized(_Method, #reqstate{resource = create, client = #reqclient{type = undefined}}) ->
    true;
is_authorized(_Method, #reqstate{resource = support_space, client = #reqclient{type = provider, id = ProviderId}, data = Data}) ->
    Token = proplists:get_value(<<"token">>, Data),
    provider_logic:can_support_space(ProviderId, Token);
is_authorized(_Method, #reqstate{resource = Res, client = #reqclient{type = provider}}) when Res =/= create ->
    true.


%% accept_resource/1
%% ====================================================================
%% @doc Processes data submitted by a client through POST on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec accept_resource(State :: #reqstate{}) ->
    {ok, Response :: reqdata()} | ok | {error, Reason :: term()}.
%% ====================================================================
accept_resource(#reqstate{resource = create, data = Data}) ->
    URL = proplists:get_value(<<"url">>, Data),
    {ok, ProviderId} = provider_logic:register(URL),
    {ok, [{providerId, ProviderId}]};
accept_resource(#reqstate{resource = support_space, client = #reqclient{id = ProviderId}, data = Data}) ->
    Token = proplists:get_value(<<"token">>, Data),
    {ok, SpaceId} = provider_logic:support_space(ProviderId, Token),
    {ok, [{spaceId, SpaceId}]};
accept_resource(#reqstate{resource = main, client = #reqclient{id = ProviderId}, data = Data}) ->
    provider_logic:modify_data(ProviderId, Data).


%% provide_resource/2
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(State :: #reqstate{}) ->
    {ok, Data :: reqdata()} | {error, Reason :: term()}.
%% ====================================================================
provide_resource(#reqstate{resource = main, client = #reqclient{id = ProviderId}}) ->
    provider_logic:get_data(ProviderId).


%% delete_resource/2
%% ====================================================================
%% @doc Deletes the resource identified by the Id parameter.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(State :: #reqstate{}) -> ok | {error, Reason :: term()}.
%% ====================================================================
delete_resource(#reqstate{resource = main, client = #reqclient{id = ProviderId}}) ->
    provider_logic:unregister(ProviderId).
