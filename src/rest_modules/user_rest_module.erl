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
is_authorized(_Method, #reqstate{resource = join_space, client = #reqclient{type = user, id = UserId}, data = Data}) ->
    Token = proplists:get_value(<<"token">>, Data),
    user_logic:can_join_space(UserId, Token);
is_authorized(_Method, #reqstate{resource = join_group, client = #reqclient{type = user, id = UserId}, data = Data}) ->
    Token = proplists:get_value(<<"token">>, Data),
    user_logic:can_join_group(UserId, Token);
is_authorized(_Method, #reqstate{resource = merge_accounts, client = #reqclient{type = user, id = UserId}, data = Data}) ->
    Token = proplists:get_value(<<"token">>, Data),
    user_logic:can_merge_accounts(UserId, Token);
is_authorized(_Method, #reqstate{resource = Res, client = #reqclient{type = user}}) when Res =/= create ->
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
    Name = proplists:get_value(<<"name">>, Data),
    {ok, UserId} = user_logic:register(Name),
    {ok, [{userId, UserId}]};
accept_resource(#reqstate{resource = join_space, client = #reqclient{id = UserId}, data = Data}) ->
    Token = proplists:get_value(<<"token">>, Data),
    {ok, SpaceId} = user_logic:join_space(UserId, Token),
    {ok, [{spaceId, SpaceId}]};
accept_resource(#reqstate{resource = merge_accounts, client = #reqclient{id = UserId}, data = Data}) ->
    Token = proplists:get_value(<<"token">>, Data),
    user_logic:merge_accounts(UserId, Token);
accept_resource(#reqstate{resource = join_group, client = #reqclient{id = UserId}, data = Data}) ->
    Token = proplists:get_value(<<"token">>, Data),
    {ok, GroupId} = user_logic:join_group(UserId, Token),
    {ok, [{groupId, GroupId}]};
accept_resource(#reqstate{resource = account_merge_token, client = #reqclient{id = UserId}}) ->
    {ok, Token} = user_logic:new_accounts_merge_token(UserId),
    {ok, [{token, Token}]};
accept_resource(#reqstate{resource = space_create_token, client = #reqclient{id = UserId}}) ->
    {ok, Token} = user_logic:new_space_create_token(UserId),
    {ok, [{token, Token}]};
accept_resource(#reqstate{resource = main, client = #reqclient{id = UserId}, data = Data}) ->
    user_logic:modify_data(UserId, Data).


%% provide_resource/1
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(State :: #reqstate{}) ->
    {ok, Data :: reqdata()} | {error, Reason :: term()}.
%% ====================================================================
provide_resource(#reqstate{resource = main, client = #reqclient{id = UserId}}) ->
    user_logic:get_data(UserId).


%% delete_resource/1
%% ====================================================================
%% @doc Deletes the resource identified by the Id parameter.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(State :: #reqstate{}) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
delete_resource(#reqstate{resource = main, client = #reqclient{id = UserId}}) ->
    user_logic:unregister(UserId).
