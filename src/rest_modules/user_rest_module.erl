%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module handling logic behind /user REST resources.
%% ===================================================================
-module(user_rest_module).
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
        {<<"/user">>,               M, S#rstate{resource = user,    methods = [get, post, patch, delete]}},
        {<<"/user/spaces">>,        M, S#rstate{resource = spaces,  methods = [get, post]   }},
        {<<"/user/spaces/join">>,   M, S#rstate{resource = sjoin,   methods = [post]        }},
        {<<"/user/spaces/token">>,  M, S#rstate{resource = screate, methods = [get]         }},
        {<<"/user/spaces/:sid">>,   M, S#rstate{resource = space,   methods = [get, delete] }},
        {<<"/user/groups">>,        M, S#rstate{resource = groups,  methods = [get, post]   }},
        {<<"/user/groups/join">>,   M, S#rstate{resource = gjoin,   methods = [post]        }},
        {<<"/user/groups/:gid">>,   M, S#rstate{resource = group,   methods = [get, delete] }},
        {<<"/user/merge">>,         M, S#rstate{resource = merge,   methods = [post]        }},
        {<<"/user/merge/token">>,   M, S#rstate{resource = mtoken,  methods = [get]         }}
    ].


%% is_authorized/4
%% ====================================================================
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec is_authorized(Resource :: atom(), Method :: method(),
                    UserId :: binary() | undefined, Client :: client()) ->
    boolean().
%% ====================================================================
is_authorized(user, post, _UserId, #client{type = undefined}) ->
    true;
is_authorized(_, _, _, #client{type = user}) ->
    true;
is_authorized(_, _, _, _) ->
    false.


%% resource_exists/3
%% ====================================================================
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec resource_exists(Resource :: atom(), UserId :: binary() | undefined,
                      Bindings :: [{atom(), any()}]) -> boolean().
%% ====================================================================
resource_exists(space, UserId, Bindings) ->
    SID = proplists:get_value(sid, Bindings),
    space_logic:has_user(SID, UserId);
resource_exists(group, UserId, Bindings) ->
    GID = proplists:get_value(gid, Bindings),
    group_logic:has_user(GID, UserId);
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
                      UserId :: binary() | undefined,
                      Data :: [proplists:property()], Client :: client(),
                      Bindings :: [{atom(), any()}]) ->
    {true, URL :: binary()} | boolean().
%% ====================================================================
accept_resource(user, post, _UserId, Data, _Client, _Bindings) ->
    Name = proplists:get_value(<<"name">>, Data),
    if
        Name =:= undefined -> false;
        true ->
            {ok, _} = user_logic:create(Name),
            {true, <<"/user">>}
    end;
accept_resource(user, patch, UserId, Data, _Client, _Bindings) ->
    Name = proplists:get_value(<<"name">>, Data),
    if
        Name =:= undefined -> false;
        true ->
            ok = user_logic:modify(UserId, Name),
            true
    end;
accept_resource(spaces, post, _UserId, Data, Client, Bindings) ->
    spaces_rest_module:accept_resource(spaces, post, undefined, Data, Client, Bindings);
accept_resource(sjoin, post, UserId, Data, _Client, _Bindings) ->
    Token = proplists:get_value(<<"token">>, Data),
    case token_logic:is_valid(Token, space_invite_user_token) of
        false -> false;
        true ->
            {ok, SpaceId} = space_logic:join({user, UserId}, Token),
            {true, <<"/user/spaces/", SpaceId/binary>>}
    end;
accept_resource(groups, post, _UserId, Data, Client, Bindings) ->
    groups_rest_module:accept_resource(groups, post, undefined, Data, Client, Bindings);
accept_resource(gjoin, post, UserId, Data, _Client, _Bindings) ->
    Token = proplists:get_value(<<"token">>, Data),
    case token_logic:is_valid(Token, group_invite_token) of
        false -> false;
        true ->
            {ok, GroupId} = group_logic:join(UserId, Token),
            {true, <<"/user/groups/", GroupId/binary>>}
    end;
accept_resource(merge, post, UserId, Data, _Client, _Bindings) ->
    Token = proplists:get_value(<<"token">>, Data),
    case token_logic:is_valid(Token, accounts_merge_token) of
        false -> false;
        true ->
            ok = user_logic:merge(UserId, Token),
            true
    end.



%% provide_resource/4
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(Resource :: atom(), UserId :: binary() | undefined,
                       Client :: client(), Req :: cowboy_req:req()) ->
    Data :: [proplists:property()].
%% ====================================================================
provide_resource(user, UserId, _Client, _Req) ->
    {ok, User} = user_logic:get_data(UserId),
    User;
provide_resource(spaces, UserId, _Client, _Req) ->
    {ok, Spaces} = user_logic:get_spaces(UserId),
    Spaces;
provide_resource(screate, UserId, _Client, _Req) ->
    {ok, Token} = token_logic:create(space_create_token, {user, UserId}),
    [{token, Token}];
provide_resource(space, _UserId, _Client, Req) ->
    {Bindings, _Req2} = cowboy_req:bindings(Req),
    SID = proplists:get_value(sid, Bindings),
    {ok, Space} = space_logic:get_data(SID, user),
    Space;
provide_resource(groups, UserId, _Client, _Req) ->
    {ok, Groups} = user_logic:get_groups(UserId),
    Groups;
provide_resource(group, _UserId, _Client, Req) ->
    {Bindings, _Req2} = cowboy_req:bindings(Req),
    SID = proplists:get_value(sid, Bindings),
    {ok, Group} = group_logic:get_data(SID),
    Group;
provide_resource(mtoken, UserId, _Client, _Req) ->
    {ok, Token} = token_logic:create(accounts_merge_token, {user, UserId}),
    [{token, Token}].


%% delete_resource/3
%% ====================================================================
%% @doc Deletes the resource identified by the SpaceId parameter.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(Resource :: atom(), UserId :: binary() | undefined,
                      Bindings :: [{atom(), any()}]) -> boolean().
%% ====================================================================
delete_resource(user, UserId, _Bindings) ->
    user_logic:remove(UserId);
delete_resource(space, UserId, Bindings) ->
    SID = proplists:get_value(sid, Bindings),
    space_logic:remove_user(SID, UserId);
delete_resource(group, UserId, Bindings) ->
    GID = proplists:get_value(gid, Bindings),
    group_logic:remove_user(GID, UserId).
