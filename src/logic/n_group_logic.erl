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
-module(n_group_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_group_logic_plugin).

-export([create/2]).

-export([
    add_user/3,
    add_group/3
]).

-export([get/2]).

-export([update/3]).

-export([delete/2]).

create(Issuer, Name) when is_binary(Name) ->
    create(Issuer, #{<<"name">> => Name});
create(Issuer, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, undefined, entity, Data).


add_user(Issuer, GroupId, UserId) when is_binary(UserId) ->
    add_user(Issuer, GroupId, #{<<"userId">> => UserId});
add_user(Issuer, GroupId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, GroupId, users, Data).


add_group(Issuer, GroupId, ChildGroupId) when is_binary(ChildGroupId) ->
    add_group(Issuer, GroupId, #{<<"groupId">> => ChildGroupId});
add_group(Issuer, GroupId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, GroupId, groups, Data).


get(Issuer, GroupId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, entity, GroupId).

%%add_relation(Issuer, {GroupId, users}, od_user, UserId) ->
%%    n_entity_logic:add_relation(
%%        Issuer, ?PLUGIN, {GroupId, users}, od_user, UserId
%%    ).




update(Issuer, GroupId, Data) ->
    n_entity_logic:update(Issuer, ?PLUGIN, GroupId, entity, Data).

delete(Issuer, GroupId) ->
    n_entity_logic:delete(Issuer, ?PLUGIN, GroupId, entity).
