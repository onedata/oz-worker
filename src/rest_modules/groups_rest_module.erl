%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: The module handling logic behind /groups REST resources.
%% @end
%% ===================================================================
-module(groups_rest_module).
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
        {"/groups/create", rest_handler, S#reqstate{resource = create}},
        {"/groups/:id", rest_handler, S#reqstate{resource = main}},
        {"/groups/:id/joinSpace", rest_handler, S#reqstate{resource = join_space}},
        {"/groups/:id/tokens/userInvite/create", rest_handler, S#reqstate{resource = user_invite_token}},
        {"/groups/:id/tokens/spaceCreate/create", rest_handler, S#reqstate{resource = space_create_token}}
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
is_authorized(_, #reqstate{resource = create, client = #reqclient{type = user}}) ->
    true;
is_authorized(<<"POST">>, #reqstate{resource = main, client = #reqclient{type = user, id = UserId}, data = Data, resid = GroupId}) ->
    group_logic:can_modify(GroupId, UserId, Data);
is_authorized(<<"GET">>, #reqstate{resource = main, client = #reqclient{type = user, id = UserId}, resid = GroupId}) ->
    group_logic:can_view(GroupId, UserId);
is_authorized(<<"DELETE">>, #reqstate{resource = main, client = #reqclient{type = user, id = UserId}, resid = GroupId}) ->
    group_logic:can_delete(GroupId, UserId);
is_authorized(_, #reqstate{resource = user_invite_token, client = #reqclient{type = user, id = UserId}, resid = GroupId}) ->
    group_logic:can_invite_users(GroupId, UserId);
is_authorized(_, #reqstate{resource = space_create_token, client = #reqclient{type = user, id = UserId}, resid = GroupId}) ->
    group_logic:can_create_spaces(GroupId, UserId).


%% accept_resource/1
%% ====================================================================
%% @doc Processes data submitted by a client through POST on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec accept_resource(State :: #reqstate{}) ->
    {ok, Response :: reqdata()} | ok | {error, Reason :: term()}.
%% ====================================================================
accept_resource(#reqstate{resource = create, client = #reqclient{id = UserId}, data = Data}) ->
    Name = proplists:get_value(<<"name">>, Data),
    {ok, GroupId} = group_logic:create(Name, UserId),
    {ok, [{groupId, GroupId}]};
accept_resource(#reqstate{resource = main, client = #reqclient{id = UserId}, data = Data, resid = GroupId}) ->
    group_logic:modify(GroupId, UserId, Data);
accept_resource(#reqstate{resource = join_space, client = #reqclient{id = UserId}, data = Data, resid = GroupId}) ->
    Token = proplists:get_value(<<"token">>, Data),
    group_logic:join_space(GroupId, UserId, Token);
accept_resource(#reqstate{resource = user_invite_token, client = #reqclient{id = UserId}, resid = GroupId}) ->
    group_logic:new_user_invite_token(GroupId, UserId);
accept_resource(#reqstate{resource = space_create_token, client = #reqclient{id = UserId}, resid = GroupId}) ->
    group_logic:new_space_create_token(GroupId, UserId).


%% provide_resource/1
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(State :: #reqstate{}) ->
    {ok, Data :: reqdata()} | {error, Reason :: term()}.
%% ====================================================================
provide_resource(#reqstate{resource = main, client = #reqclient{id = UserId}, resid = GroupId}) ->
    group_logic:view(GroupId, UserId).


%% delete_resource/2
%% ====================================================================
%% @doc Deletes the resource identified by the SpaceId parameter.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(State :: #reqstate{}) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
delete_resource(#reqstate{resource = main, client = #reqclient{id = UserId}, resid = GroupId}) ->
    group_logic:delete(GroupId, UserId).
