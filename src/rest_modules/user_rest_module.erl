%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: The module handling logic behind /user REST resources.
%% @end
%% ===================================================================
-module(user_rest_module).
-author("Konrad Zemek").

-include("handlers/rest_handler.hrl").

-behavior(rest_module_behavior).


%% API
-export([routes/0, is_authorized/2, accept_resource/3, provide_resource/2,
    delete_resource/2]).


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
        {"/user", rest_handler, S#reqstate{resource = main}},
        {"/user/create", rest_handler, S#reqstate{resource = create}},
        {"/user/joinGroup", rest_handler, S#reqstate{resource = join_group}},
        {"/user/joinSpace", rest_handler, S#reqstate{resource = join_space}},
        {"/user/mergeAccounts", rest_handler, S#reqstate{resource = merge_accounts}},
        {"/user/tokens/accountMerge/create", rest_handler, S#reqstate{resource = account_merge_token}},
        {"/user/tokens/spaceCreate/create", rest_handler, S#reqstate{resource = space_create_token}}
    ].


%% is_authorized/2
%% ====================================================================
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec is_authorized(Id :: binary(), State :: #reqstate{}) -> boolean().
%% ====================================================================
is_authorized(_Id, #reqstate{client = {user, _}} = _State) ->
    true.


%% accept_resource/3
%% ====================================================================
%% @doc Processes data submitted by a client through POST on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec accept_resource(Id :: binary(), Data :: [proplists:property()],
                      State :: #reqstate{}) ->
    {ok, Response :: [proplists:property()]} | ok | {error, Reason :: term()}.
%% ====================================================================
accept_resource(_Id, Data, #reqstate{resource = create, client = undefined}) ->
    [{<<"name">>, Name}] = Data,
    {ok, UserId} = user_logic:register(Name),
    {ok, [{userId, UserId}]};
accept_resource(_Id, Data, #reqstate{resource = join_space, client = {user, UserId}}) ->
    [{<<"token">>, Token}] = Data,
    {ok, SpaceId} = user_logic:join_space(UserId, Token),
    {ok, [{spaceId, SpaceId}]};
accept_resource(_Id, Data, #reqstate{resource = merge_accounts, client = {user, UserId}}) ->
    [{<<"token">>, Token}] = Data,
    user_logic:merge_accounts(UserId, Token);
accept_resource(_Id, Data, #reqstate{resource = join_group, client = {user, UserId}}) ->
    [{<<"token">>, Token}] = Data,
    {ok, GroupId} = user_logic:join_group(UserId, Token),
    {ok, [{groupId, GroupId}]};
accept_resource(_Id, _Data, #reqstate{resource = account_merge_token, client = {user, UserId}}) ->
    {ok, Token} = user_logic:new_accounts_merge_token(UserId),
    {ok, [{token, Token}]};
accept_resource(_Id, _Data, #reqstate{resource = space_create_token, client = {user, UserId}}) ->
    {ok, Token} = user_logic:new_space_create_token(UserId),
    {ok, [{token, Token}]};
accept_resource(_Id, Data, #reqstate{resource = main, client = {user, UserId}}) ->
    user_logic:modify_data(UserId, Data).


%% provide_resource/2
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(Id :: binary(), State :: #reqstate{}) ->
    {ok, Data :: [proplists:property()]} | {error, Reason :: term()}.
%% ====================================================================
provide_resource(_Id, #reqstate{resource = main, client = {user, UserId}} = _State) ->
    user_logic:get_data(UserId).


%% delete_resource/2
%% ====================================================================
%% @doc Deletes the resource identified by the Id parameter.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(Id :: binary(), State :: #reqstate{}) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
delete_resource(_Id, #reqstate{resource = main, client = {user, UserId}} = _State) ->
    user_logic:unregister(UserId).
