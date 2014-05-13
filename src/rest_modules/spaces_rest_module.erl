%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: The module handling logic behind /spaces REST resources.
%% @end
%% ===================================================================
-module(spaces_rest_module).
-author("Konrad Zemek").

-include("handlers/rest_handler.hrl").

-behavior(rest_module_behavior).


%% API
-export([routes/0, is_authorized/2, accept_resource/1, provide_resource/1,
    delete_resource/1]).


%% routes/0
%% ====================================================================
%% @doc Returns a Cowboy-understandable PathList of routes supported by a module
%% implementing this behavior.
%% @see rest_module_behavior
%% @end
-spec routes() ->
    [{PathMatch :: string() | binary(), rest_handler, State :: #reqstate{}}].
%% ====================================================================
routes() ->
    S = #reqstate{module = ?MODULE},
    [
        {"/spaces/create", rest_handler, S#reqstate{resource = create}},
        {"/spaces/:id", rest_handler, S#reqstate{resource = main}},
        {"/spaces/:id/tokens/userInvite/create", rest_handler, S#reqstate{resource = invite_token}},
        {"/spaces/:id/tokens/providerSupport/create", rest_handler, S#reqstate{resource = provider_support_token}}
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
is_authorized(<<"POST">>, #reqstate{resource = create, client = #reqclient{type = user}}) ->
    true;
is_authorized(<<"POST">>, #reqstate{resource = create, client = #reqclient{type = provider, id = ProviderId}, data = Data}) ->
    Token = proplists:get_value(<<"token">>, Data),
    space_logic:can_provider_create(ProviderId, Token);
is_authorized(<<"POST">>, #reqstate{resource = main, client = #reqclient{type = user, id = UserId}, data = Data, resid = SpaceId}) ->
    space_logic:can_modify(SpaceId, UserId, Data);
is_authorized(<<"GET">>, #reqstate{resource = main, client = #reqclient{type = user, id = UserId}, resid = SpaceId}) ->
    space_logic:can_user_view(SpaceId, UserId);
is_authorized(<<"GET">>, #reqstate{resource = main, client = #reqclient{type = provider, id = ProviderId}, resid = SpaceId}) ->
    space_logic:can_provider_view(SpaceId, ProviderId);
is_authorized(<<"POST">>, #reqstate{resource = invite_token, client = #reqclient{type = user, id = UserId}, resid = SpaceId}) ->
    space_logic:can_invite(SpaceId, UserId);
is_authorized(<<"POST">>, #reqstate{resource = provider_support_token, client = #reqclient{type = user, id = UserId}, resid = SpaceId}) ->
    space_logic:can_add_providers(SpaceId, UserId);
is_authorized(<<"DELETE">>, #reqstate{resource = main, client = #reqclient{type = user, id = UserId}, resid = SpaceId}) ->
    space_logic:can_delete(SpaceId, UserId).


%% accept_resource/1
%% ====================================================================
%% @doc Processes data submitted by a client through POST on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec accept_resource(State :: #reqstate{}) ->
    {ok, Response :: reqdata()} | ok | {error, Reason :: term()}.
%% ====================================================================
accept_resource(#reqstate{resource = create, client = #reqclient{type = user, id = UserId}, data = Data}) ->
    Name = proplists:get_value(<<"name">>, Data),
    {ok, SpaceId} = space_logic:user_create(Name, UserId),
    {ok, [{spaceId, SpaceId}]};
accept_resource(#reqstate{resource = create, client = #reqclient{type = provider, id = ProviderId}, data = Data}) ->
    Name = proplists:get_value(<<"name">>, Data),
    Token = proplists:get_value(<<"token">>, Data),
    {ok, SpaceId} = space_logic:provider_create(Name, ProviderId, Token),
    {ok, [{spaceId, SpaceId}]};
accept_resource(#reqstate{resource = main, client = #reqclient{id = UserId}, data = Data, resid = SpaceId}) ->
    space_logic:modify(SpaceId, UserId, Data);
accept_resource(#reqstate{resource = invite_token, client = #reqclient{id = UserId}, data = Data, resid = SpaceId}) ->
    case proplists:get_value(<<"groupId">>, Data) of
        undefined -> space_logic:new_user_invite_token(SpaceId, UserId);
        GroupId -> space_logic:new_group_invite_token(SpaceId, GroupId, UserId)
    end;
accept_resource(#reqstate{resource = provider_support_token, client = #reqclient{id = UserId}, resid = SpaceId}) ->
    space_logic:new_support_token(SpaceId, UserId).


%% provide_resource/1
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(State :: #reqstate{}) ->
    {ok, Data :: reqdata()} | {error, Reason :: term()}.
%% ====================================================================
provide_resource(#reqstate{resource = main, client = #reqclient{type = user, id = UserId}, resid = SpaceId}) ->
    space_logic:user_view(SpaceId, UserId);
provide_resource(#reqstate{resource = main, client = #reqclient{type = provider, id = ProviderId}, resid = SpaceId}) ->
    space_logic:provider_view(SpaceId, ProviderId).


%% delete_resource/2
%% ====================================================================
%% @doc Deletes the resource identified by the SpaceId parameter.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(State :: #reqstate{}) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
delete_resource(#reqstate{resource = main, client = #reqclient{id = UserId}, resid = SpaceId}) ->
    space_logic:delete(SpaceId, UserId).
