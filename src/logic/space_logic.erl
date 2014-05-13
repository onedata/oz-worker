%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: The module implementing the business logic for spaces in the registry.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(space_logic).
-author("Konrad Zemek").

%% API

-export([can_provider_create/2, can_modify/3, can_user_view/2,
    can_provider_view/2, can_invite/2, can_add_providers/2, can_delete/2]).

-export([user_create/2, provider_create/3, modify/3, new_user_invite_token/2,
    new_group_invite_token/3, new_support_token/2, user_view/2, provider_view/2,
    delete/2]).


-spec can_provider_create(ProviderId :: binary(), Token :: binary()) -> boolean().
can_provider_create(ProviderId, Token) ->
    true.


-spec can_modify(SpaceId :: binary(), UserId :: binary(), Modifications :: [proplists:property()]) -> boolean().
can_modify(SpaceId, UserId, Modifications) ->
    true.


-spec can_user_view(SpaceId :: binary(), UserId :: binary()) -> boolean().
can_user_view(SpaceId, UserId) ->
    true.


-spec can_provider_view(SpaceId :: binary(), ProviderId :: binary()) -> boolean().
can_provider_view(SpaceId, ProviderId) ->
    true.


-spec can_invite(SpaceId :: binary(), UserId :: binary()) -> boolean().
can_invite(SpaceId, UserId) ->
    true.


-spec can_add_providers(SpaceId :: binary(), UserId :: binary()) -> boolean().
can_add_providers(SpaceId, UserId) ->
    true.


-spec can_delete(SpaceId :: binary(), UserId :: binary()) -> boolean().
can_delete(SpaceId, UserId) ->
    true.


-spec user_create(Name :: binary(), UserId :: binary()) -> {ok, SpaceId :: binary()} | {error, Reason :: term()}.
user_create(Name, UserId) ->
    {ok, <<"spaceid">>}.


-spec provider_create(Name :: binary(), ProviderId :: binary(), Token :: binary()) -> {ok, SpaceId :: binary()} | {error, Reason :: term()}.
provider_create(Name, ProviderId, Token) ->
    {ok, <<"spaceid">>}.


-spec modify(SpaceId :: binary(), UserId :: binary(), Modifications :: [proplists:property()]) -> ok | {error, Reason :: term()}.
modify(SpaceId, UserId, Modifications) ->
    ok.


-spec new_user_invite_token(SpaceId :: binary(), UserId :: binary()) -> {ok, Token :: binary()} | {error, Reason :: term()}.
new_user_invite_token(SpaceId, UserId) ->
    {ok, <<"spaceuserinvitetoken">>}.


-spec new_group_invite_token(SpaceId :: binary(), GroupId :: binary(), UserId :: binary()) -> {ok, Token :: binary()} | {error, Reason :: term()}.
new_group_invite_token(SpaceId, GroupId, UserId) ->
    {ok, <<"spacegroupinvitetoken">>}.


-spec new_support_token(SpaceId :: binary(), UserId :: binary()) -> {ok, Token :: binary()} | {error, Reason :: term()}.
new_support_token(SpaceId, UserId) ->
    {ok, <<"spacesupporttoken">>}.


-spec user_view(SpaceId :: binary(), UserId :: binary()) -> {ok, [proplists:property()]} | {error, Reason :: term()}.
user_view(SpaceId, UserId) ->
    {ok, [{<<"data">>, 1}]}.


-spec provider_view(SpaceId :: binary(), UserId :: binary()) -> {ok, [proplists:property()]} | {error, Reason :: term()}.
provider_view(SpaceId, UserId) ->
    {ok, [{<<"data">>, 1}]}.


-spec delete(SpaceId :: binary(), UserId :: binary()) -> ok | {error, Reason :: term()}.
delete(SpaceId, UserId) ->
    ok.
