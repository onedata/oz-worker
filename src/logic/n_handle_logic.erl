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
-module(n_handle_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_handle_logic_plugin).

-export([create/5, create/2]).

-export([
    add_user/3,
    add_group/3
]).

-export([get/2]).

-export([update/3]).

-export([delete/2]).



create(Issuer, HandleServiceId, ResourceType, ResourceId, Metadata) ->
    create(Issuer, #{
        <<"handleServiceId">> => HandleServiceId,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => Metadata
    }).
create(Issuer, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, undefined, entity, Data).


add_user(Issuer, HandleId, UserId) when is_binary(UserId) ->
    add_user(Issuer, HandleId, #{<<"userId">> => UserId});
add_user(Issuer, HandleId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, HandleId, users, Data).


add_group(Issuer, HandleId, GroupId) when is_binary(GroupId) ->
    add_group(Issuer, HandleId, #{<<"groupId">> => GroupId});
add_group(Issuer, HandleId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, HandleId, groups, Data).


get(Issuer, HandleId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, entity, HandleId).

%%add_relation(Issuer, {HandleId, users}, od_user, UserId) ->
%%    n_entity_logic:add_relation(
%%        Issuer, ?PLUGIN, {HandleId, users}, od_user, UserId
%%    ).




update(Issuer, HandleId, Data) ->
    n_entity_logic:update(Issuer, ?PLUGIN, HandleId, entity, Data).

delete(Issuer, HandleId) ->
    n_entity_logic:delete(Issuer, ?PLUGIN, HandleId, entity).
