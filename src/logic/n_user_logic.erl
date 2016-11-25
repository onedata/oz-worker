%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc

%%% @end
%%%-------------------------------------------------------------------
-module(n_user_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create/1
]).
-export([
    get/2,
    get_users/2,
    get_invite_user_token/2
]).
-export([
    add_user/3,
    add_group/3
%%    join_as_user/2,
%%    join_as_group/3
]).
-export([
    update/3
]).
-export([
    delete/2
]).


create(UserInfo) ->
    od_user:create(#document{value = UserInfo}).


get(Issuer, SpaceId) ->
    n_entity_logic:get(Issuer, n_space_logic_plugin, SpaceId, entity).


get_users(Issuer, SpaceId) ->
    n_entity_logic:get(Issuer, n_space_logic_plugin, SpaceId, users).


get_invite_user_token({user, UserId}, SpaceId) ->
    n_entity_logic:get({user, UserId}, n_space_logic_plugin, SpaceId, space_invite_user_token).


add_user(Issuer, SpaceId, UserId) when is_binary(UserId) ->
    add_user(Issuer, SpaceId, #{<<"userId">> => UserId});
add_user(Issuer, SpaceId, Data) ->
    n_entity_logic:create(
        Issuer, n_space_logic_plugin, SpaceId, users, Data
    ).


add_group(Issuer, SpaceId, GroupId) when is_binary(GroupId) ->
    add_group(Issuer, SpaceId, #{<<"groupId">> => GroupId});
add_group(Issuer, SpaceId, Data) ->
    n_entity_logic:create(
        Issuer, n_space_logic_plugin, SpaceId, groups, Data
    ).


%%join_as_user({user, UserId}, Macaroon) ->
%%    n_entity_logic:consume_token(
%%        {user, UserId}, n_space_logic_plugin, undefined, {od_user, UserId}, Macaroon
%%    ).
%%
%%
%%join_as_group({user, UserId}, GroupId, Macaroon) ->
%%    n_entity_logic:consume_token(
%%        {user, UserId}, n_space_logic_plugin, undefined, {od_group, GroupId}, Macaroon
%%    ).


update(Issuer, SpaceId, NewName) when is_binary(NewName) ->
    update(Issuer, SpaceId, #{<<"name">> => NewName});
update(Issuer, SpaceId, Data) ->
    n_entity_logic:update(Issuer, n_space_logic_plugin, SpaceId, entity, Data).


delete(Issuer, SpaceId) ->
    n_entity_logic:delete(Issuer, n_space_logic_plugin, SpaceId, entity).

