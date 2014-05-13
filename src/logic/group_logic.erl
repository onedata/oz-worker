%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: The module implementing the business logic for groups of users.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(group_logic).
-author("Konrad Zemek").

%% API
-export([can_modify/3, can_view/2, can_delete/2, can_invite_users/2,
    can_create_spaces/2]).
-export([create/2, modify/3, join_space/3, new_user_invite_token/2,
    new_space_create_token/2, view/2, delete/2]).


-spec can_modify(GroupId :: binary(), UserId :: binary(), Modifications :: binary()) -> boolean().
can_modify(GroupId, UserId, Modifications) ->
    true.


-spec can_view(GroupId :: binary(), UserId :: binary()) -> boolean().
can_view(GroupId, UserId) ->
    true.


-spec can_delete(GroupId :: binary(), UserId :: binary()) -> boolean().
can_delete(GroupId, UserId) ->
    true.


-spec can_invite_users(GroupId :: binary(), UserId :: binary()) -> boolean().
can_invite_users(GroupId, UserId) ->
    true.


-spec can_create_spaces(GroupId :: binary(), UserId :: binary()) -> boolean().
can_create_spaces(GroupId, UserId) ->
    true.


-spec create(Name :: binary(), UserId :: binary()) -> {ok, GroupId :: binary()} | {error, Reason :: term()}.
create(Name, UserId) ->
    {ok, <<"groupid">>}.


-spec modify(GroupId :: binary(), UserId :: binary(), Modifications :: binary()) -> ok | {error, Reason :: term()}.
modify(GroupId, UserId, Modifications) ->
    ok.


-spec join_space(GroupId :: binary(), UserId :: binary(), Token :: binary()) -> {ok, SpaceId :: binary()} | {error, Reason :: term()}.
join_space(GroupId, UserId, Token) ->
    {ok, <<"spaceid">>}.


-spec new_user_invite_token(GroupId :: binary(), UserId :: binary()) -> {ok, Token :: binary()} | {error, Reason :: term()}.
new_user_invite_token(GroupId, UserId) ->
    {ok, <<"invitetoken">>}.


-spec new_space_create_token(GroupId :: binary(), UserId :: binary()) -> {ok, Token :: binary()} | {error, Reason :: term()}.
new_space_create_token(GroupId, UserId) ->
    {ok, <<"spacecreatetoken">>}.


-spec view(GroupId :: binary(), UserId :: binary()) -> {ok, [proplists:property()]} | {error, Reason :: term()}.
view(GroupId, UserId) ->
    {ok, [{data, dad}]}.


-spec delete(GroupId :: binary(), UserId :: binary()) -> ok | {error, Reason :: term()}.
delete(GroupId, UserId) ->
    ok.
