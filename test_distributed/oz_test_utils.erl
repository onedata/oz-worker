%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Common functions for ct tests.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_test_utils).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([create_space/3, create_user/2, create_group/3]).
-export([join_group/3, join_space/3, leave_space/3, support_space/4]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates space in Global Registry.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Config :: term(), Member :: {user | group, Id :: binary()}, Name :: binary()) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
create_space(Config, Member, Name) ->
    try
        [Node | _] = ?config(oz_worker_nodes, Config),
        rpc:call(Node, space_logic, create, [Member, Name])
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Creates user in Global Registry.
%% @end
%%--------------------------------------------------------------------
-spec create_user(Config :: term(), User :: #onedata_user{}) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
create_user(Config, User) ->
    try
        [Node | _] = ?config(oz_worker_nodes, Config),
        rpc:call(Node, user_logic, create, [User])
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Creates group in Global Registry.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Config :: term(), UserId :: binary(), Name :: binary()) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
create_group(Config, UserId, Name) ->
    try
        [Node | _] = ?config(oz_worker_nodes, Config),
        rpc:call(Node, group_logic, create, [UserId, Name])
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Adds user to group in Global Registry.
%% @end
%%--------------------------------------------------------------------
-spec join_group(Config :: term(), UserId :: binary(), GroupId :: binary()) ->
    ok | {error, Reason :: term()}.
join_group(Config, UserId, GroupId) ->
    try
        [Node | _] = ?config(oz_worker_nodes, Config),
        rpc:call(Node, erlang, apply, [fun() ->
            {ok, Token} = token_logic:create(#client{type = user, id = UserId}, group_invite_token, {group, GroupId}),
            {ok, Macaroon} = macaroon:deserialize(Token),
            {ok, GroupId} = group_logic:join(UserId, Macaroon)
        end, []]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Joins space as a user or a group.
%% @end
%%--------------------------------------------------------------------
-spec join_space(Config :: term(), {user | group, Id :: binary()}, SpaceId :: binary()) ->
    ok | {error, Reason :: term()}.
join_space(Config, {user, UserId}, SpaceId) ->
    try
        [Node | _] = ?config(oz_worker_nodes, Config),
        {ok, SpaceId} = rpc:call(Node, erlang, apply, [fun() ->
            {ok, Token} = token_logic:create(#client{type = user, id = UserId}, space_invite_user_token, {space, SpaceId}),
            {ok, Macaroon} = macaroon:deserialize(Token),
            space_logic:join({user, UserId}, Macaroon)
        end, []]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end;

join_space(Config, {group, GroupId}, SpaceId) ->
    try
        [Node | _] = ?config(oz_worker_nodes, Config),
        {ok, SpaceId} = rpc:call(Node, erlang, apply, [fun() ->
            {ok, Token} = token_logic:create(#client{type = provider}, space_invite_group_token, {space, SpaceId}),
            {ok, Macaroon} = macaroon:deserialize(Token),
            space_logic:join({group, GroupId}, Macaroon)
        end, []]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Leaves space as a user or a group.
%% @end
%%--------------------------------------------------------------------
-spec leave_space(Config :: term(), {user | group, Id :: binary()}, SpaceId :: binary()) ->
    ok | {error, Reason :: term()}.
leave_space(Config, {user, UserId}, SpaceId) ->
    try
        [Node | _] = ?config(oz_worker_nodes, Config),
        rpc:call(Node, space_logic, remove_user, [SpaceId, UserId])
    catch
        _:Reason ->
            {error, Reason}
    end;

leave_space(Config, {group, GroupId}, SpaceId) ->
    try
        [Node | _] = ?config(oz_worker_nodes, Config),
        rpc:call(Node, space_logic, remove_group, [SpaceId, GroupId])
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Supports space by provider.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Config :: term(), ProviderId :: binary(), SpaceId :: binary(), Size :: non_neg_integer()) ->
    ok | {error, Reason :: term()}.
support_space(Config, ProviderId, SpaceId, Size) ->
    try
        [Node | _] = ?config(oz_worker_nodes, Config),
        {ok, SpaceId} = rpc:call(Node, erlang, apply, [fun() ->
            {ok, Token} = token_logic:create(#client{type = provider}, space_support_token, {space, SpaceId}),
            {ok, Macaroon} = macaroon:deserialize(Token),
            space_logic:support(ProviderId, Macaroon, Size)
        end, []]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.