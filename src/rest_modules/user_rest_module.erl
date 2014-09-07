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

-include("dao/dao_users.hrl").
-include("handlers/rest_handler.hrl").

-include_lib("ctool/include/logging.hrl").

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
        {<<"/user/spaces/default">>,M, S#rstate{resource = defspace,methods = [get, put]    }},
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
                      Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
%% ====================================================================
resource_exists(space, UserId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {sid, SID} = lists:keyfind(sid, 1, Bindings),
    {space_logic:has_user(SID, UserId), Req2};
resource_exists(group, UserId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {gid, GID} = lists:keyfind(gid, 1, Bindings),
    {group_logic:has_user(GID, UserId), Req2};
resource_exists(_, _, Req) ->
    {true, Req}.


%% accept_resource/6
%% ====================================================================
%% @doc Processes data submitted by a client through POST, PATCH, PUT on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec accept_resource(Resource :: atom(), Method :: method(),
                      UserId :: binary() | undefined,
                      Data :: [proplists:property()], Client :: client(),
                      Req :: cowboy_req:req()) ->
    {{true, {url, URL :: binary()} | {data, Data :: [proplists:property()]}} |
        boolean(), cowboy_req:req()} | no_return().
%% ====================================================================
accept_resource(user, post, _UserId, Data, _Client, Req) ->
    Name = rest_module_helper:assert_key(<<"name">>, Data),
    {ok, _} = user_logic:create(#user{name = Name}),
    {true, Req};
accept_resource(user, patch, UserId, Data, _Client, Req) ->
    Name = rest_module_helper:assert_key(<<"name">>, Data),
    ok = user_logic:modify(UserId, [{name, Name}]),
    {true, Req};
accept_resource(spaces, post, _UserId, Data, Client, Req) ->
    spaces_rest_module:accept_resource(spaces, post, undefined, Data, Client, Req);
accept_resource(defspace, put, UserId, Data, _Client, Req) ->
    SpaceId = rest_module_helper:assert_key(<<"spaceId">>, Data),
    Result = user_logic:set_default_space(UserId, SpaceId),
    {Result, Req};
accept_resource(sjoin, post, UserId, Data, _Client, Req) ->
    Token = rest_module_helper:assert_key(<<"token">>, Data),
    case token_logic:is_valid(Token, space_invite_user_token) of
        false -> rest_module_helper:report_invalid_value(<<"token">>, Token);
        true ->
            {ok, SpaceId} = space_logic:join({user, UserId}, Token),
            {{true, {url, <<"/user/spaces/", SpaceId/binary>>}}, Req}
    end;
accept_resource(groups, post, _UserId, Data, Client, Req) ->
    groups_rest_module:accept_resource(groups, post, undefined, Data, Client, Req);
accept_resource(gjoin, post, UserId, Data, _Client, Req) ->
    Token = rest_module_helper:assert_key(<<"token">>, Data),
    case token_logic:is_valid(Token, group_invite_token) of
        false -> rest_module_helper:report_invalid_value(<<"token">>, Token);
        true ->
            {ok, GroupId} = group_logic:join(UserId, Token),
            {{true, {url, <<"/user/groups/", GroupId/binary>>}}, Req}
    end;
accept_resource(merge, post, UserId, Data, _Client, Req) ->
    Token = rest_module_helper:assert_key(<<"token">>, Data),
    case token_logic:is_valid(Token, accounts_merge_token) of
        false -> rest_module_helper:report_invalid_value(<<"token">>, Token);
        true ->
            ok = user_logic:merge(UserId, Token),
            {true, Req}
    end.


%% provide_resource/4
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(Resource :: atom(), UserId :: binary() | undefined,
                       Client :: client(), Req :: cowboy_req:req()) ->
    {Data :: [proplists:property()], cowboy_req:req()}.
%% ====================================================================
provide_resource(user, UserId, _Client, Req) ->
    {ok, User} = user_logic:get_data(UserId),
    {User, Req};
provide_resource(spaces, UserId, _Client, Req) ->
    {ok, Spaces} = user_logic:get_spaces(UserId),
    {Spaces, Req};
provide_resource(defspace, UserId, _Client, Req) ->
    {ok, DefaultSpaceId} = user_logic:get_default_space(UserId),
    {[{spaceId, DefaultSpaceId}], Req};
provide_resource(screate, UserId, _Client, Req) ->
    {ok, Token} = token_logic:create(space_create_token, {user, UserId}),
    {[{token, Token}], Req};
provide_resource(space, _UserId, _Client, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {sid, SID} = lists:keyfind(sid, 1, Bindings),
    {ok, Space} = space_logic:get_data(SID, user),
    {Space, Req2};
provide_resource(groups, UserId, _Client, Req) ->
    {ok, Groups} = user_logic:get_groups(UserId),
    {Groups, Req};
provide_resource(group, _UserId, _Client, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {sid, SID} = lists:keyfind(sid, 1, Bindings),
    {ok, Group} = group_logic:get_data(SID),
    {Group, Req2};
provide_resource(mtoken, UserId, _Client, Req) ->
    {ok, Token} = token_logic:create(accounts_merge_token, {user, UserId}),
    {[{token, Token}], Req}.


%% delete_resource/3
%% ====================================================================
%% @doc Deletes the resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(Resource :: atom(), UserId :: binary() | undefined,
                      Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
%% ====================================================================
delete_resource(user, UserId, Req) ->
    {user_logic:remove(UserId), Req};
delete_resource(space, UserId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {sid, SID} = lists:keyfind(sid, 1, Bindings),
    {space_logic:remove_user(SID, UserId), Req2};
delete_resource(group, UserId, Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {gid, GID} = lists:keyfind(gid, 1, Bindings),
    {group_logic:remove_user(GID, UserId), Req2}.
