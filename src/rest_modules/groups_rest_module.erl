%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module handling logic behind /groups REST resources.
%% ===================================================================
-module(groups_rest_module).
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
%% ====================================================================
-spec routes() ->
    [{PathMatch :: binary(), rest_handler, State :: rstate()}].
%% ====================================================================
routes() ->
    S = #rstate{module = ?MODULE},
    M = rest_handler,
    [
        {<<"/groups">>,                             M, S#rstate{resource = groups,  methods = [post]        }},
        {<<"/groups/:id">>,                         M, S#rstate{resource = group,   methods = [get, patch, delete]}},
        {<<"/groups/:id/users">>,                   M, S#rstate{resource = users,   methods = [get]         }},
        {<<"/groups/:id/users/token">>,             M, S#rstate{resource = uinvite, methods = [get]         }},
        {<<"/groups/:id/users/:uid">>,              M, S#rstate{resource = user,    methods = [get, delete] }},
        {<<"/groups/:id/users/:uid/privileges">>,   M, S#rstate{resource = upriv,   methods = [get, put]    }},
        {<<"/groups/:id/spaces">>,                  M, S#rstate{resource = spaces,  methods = [get, post]   }},
        {<<"/groups/:id/spaces/join">>,             M, S#rstate{resource = sjoin,   methods = [post]        }},
        {<<"/groups/:id/spaces/token">>,            M, S#rstate{resource = screate, methods = [get]         }},
        {<<"/groups/:id/spaces/:sid">>,             M, S#rstate{resource = space,   methods = [get, delete] }}
    ].


%% is_authorized/4
%% ====================================================================
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec is_authorized(Resource :: atom(), Method :: method(),
                    GroupId :: binary() | undefined, Client :: client()) ->
    boolean().
%% ====================================================================
is_authorized(_, _, _, #client{type = ClientType}) when ClientType =/= user ->
    false;
is_authorized(groups, post, _GroupId, _Client) ->
    true;
is_authorized(group, patch, GroupId, #client{id = UserId}) ->
    group_logic:has_privilege(GroupId, UserId, group_change_data);
is_authorized(group, delete, GroupId, #client{id = UserId}) ->
    group_logic:has_privilege(GroupId, UserId, group_remove);
is_authorized(uinvite, get, GroupId, #client{id = UserId}) ->
    group_logic:has_privilege(GroupId, UserId, group_invite_user);
is_authorized(user, delete, GroupId, #client{id = UserId}) ->
    group_logic:has_privilege(GroupId, UserId, group_remove_user);
is_authorized(upriv, put, GroupId, #client{id = UserId}) ->
    group_logic:has_privilege(GroupId, UserId, group_set_privileges);
is_authorized(spaces, post, GroupId, #client{id = UserId}) ->
    group_logic:has_privilege(GroupId, UserId, group_create_space);
is_authorized(sjoin, post, GroupId, #client{id = UserId}) ->
    group_logic:has_privilege(GroupId, UserId, group_join_space);
is_authorized(screate, get, GroupId, #client{id = UserId}) ->
    group_logic:has_privilege(GroupId, UserId, group_create_space_token);
is_authorized(space, delete, GroupId, #client{id = UserId}) ->
    group_logic:has_privilege(GroupId, UserId, group_leave_space);
is_authorized(_, get, GroupId, #client{id = UserId}) ->
    group_logic:has_privilege(GroupId, UserId, group_view_data);
is_authorized(_, _, _, _) ->
    false.


%% resource_exists/3
%% ====================================================================
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec resource_exists(Resource :: atom(), GroupId :: binary() | undefined,
                      Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
%% ====================================================================
resource_exists(groups, _GroupId, Req) ->
    {true, Req};
resource_exists(UserBound, GroupId, Req) when UserBound =:= user; UserBound =:= upriv ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    UID = proplists:get_value(uid, Bindings),
    {group_logic:has_user(GroupId, UID), Req2};
resource_exists(space, GroupId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    SID = proplists:get_value(sid, Bindings),
    {space_logic:has_group(SID, GroupId), Req2};
resource_exists(_, GroupId, Req) ->
    {group_logic:exists(GroupId), Req}.


%% accept_resource/6
%% ====================================================================
%% @doc Processes data submitted by a client through POST, PATCH on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec accept_resource(Resource :: atom(), Method :: method(),
                      GroupId :: binary() | undefined,
                      Data :: [proplists:property()], Client :: client(),
                      Req :: cowboy_req:req()) ->
    {{true, {url, URL :: binary()} | {data, Data :: [proplists:property()]}} |
        boolean(), cowboy_req:req()}.
%% ====================================================================
accept_resource(groups, post, _GroupId, Data, #client{id = UserId}, Req) ->
    Name = proplists:get_value(<<"name">>, Data),
    Result = if
        Name =:= undefined -> false;
        true ->
            {ok, GroupId} = group_logic:create(UserId, Name),
            {true, {url, <<"/groups/", GroupId/binary>>}}
    end,
    {Result, Req};
accept_resource(group, patch, GroupId, Data, _Client, Req) ->
    Name = proplists:get_value(<<"name">>, Data),
    Result = if
        Name =:= undefined -> false;
        true ->
            ok = group_logic:modify(GroupId, Name),
            true
    end,
    {Result, Req};
accept_resource(upriv, put, GroupId, Data, _Client, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    UID = proplists:get_value(uid, Bindings),
    BinPrivileges = proplists:get_value(<<"privileges">>, Data),
    Result = if
        BinPrivileges =:= undefined -> false;
        true ->
            Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
            ok = group_logic:set_privileges(GroupId, UID, Privileges),
            true
    end,
    {Result, Req2};
accept_resource(spaces, post, GroupId, Data, _Client, Req) ->
    Name = proplists:get_value(<<"name">>, Data),
    Result = if
        Name =:= undefined -> false;
        true ->
            {ok, SpaceId} = space_logic:create({group, GroupId}, Name),
            {true, {url, <<"/spaces/", SpaceId/binary>>}}
    end,
    {Result, Req};
accept_resource(sjoin, post, GroupId, Data, _Client, Req) ->
    Token = proplists:get_value(<<"token">>, Data),
    Result = case token_logic:is_valid(Token, space_invite_group_token) of
        false -> false;
        true ->
            {ok, SpaceId} = space_logic:join({group, GroupId}, Token),
            {true, {url, <<"/groups/", GroupId/binary, "/spaces/", SpaceId/binary>>}}
    end,
    {Result, Req}.


%% provide_resource/4
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(Resource :: atom(), GroupId :: binary() | undefined,
                       Client :: client(), Req :: cowboy_req:req()) ->
    {Data :: [proplists:property()], cowboy_req:req()}.
%% ====================================================================
provide_resource(group, GroupId, _Client, Req) ->
    {ok, Data} = group_logic:get_data(GroupId),
    {Data, Req};
provide_resource(users, GroupId, _Client, Req) ->
    {ok, Users} = group_logic:get_users(GroupId),
    {Users, Req};
provide_resource(uinvite, GroupId, _Client, Req) ->
    {ok, Token} = token_logic:create(group_invite_token, {group, GroupId}),
    {[{token, Token}], Req};
provide_resource(user, GroupId, _Client, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    UID = proplists:get_value(uid, Bindings),
    {ok, User} = group_logic:get_user(GroupId, UID),
    {User, Req2};
provide_resource(upriv, GroupId, _Client, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    UID = proplists:get_value(uid, Bindings),
    {ok, Privileges} = group_logic:get_privileges(GroupId, UID),
    {[{privileges, Privileges}], Req2};
provide_resource(spaces, GroupId, _Client, Req) ->
    {ok, Spaces} = group_logic:get_spaces(GroupId),
    {Spaces, Req};
provide_resource(screate, GroupId, _Client, Req) ->
    {ok, Token} = token_logic:create(space_create_token, {group, GroupId}),
    {[{token, Token}], Req};
provide_resource(space, _GroupId, _Client, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    SID = proplists:get_value(sid, Bindings),
    {ok, Space} = space_logic:get_data(SID, user),
    {Space, Req2}.


%% delete_resource/3
%% ====================================================================
%% @doc Deletes the resource identified by the SpaceId parameter.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(Resource :: atom(), GroupId :: binary() | undefined,
                      Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
%% ====================================================================
delete_resource(group, GroupId, Req) ->
    {group_logic:remove(GroupId), Req};
delete_resource(user, GroupId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    UID = proplists:get_value(uid, Bindings),
    {group_logic:remove_user(GroupId, UID), Req2};
delete_resource(space, GroupId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    SID = proplists:get_value(sid, Bindings),
    {space_logic:remove_group(SID, GroupId), Req2}.
