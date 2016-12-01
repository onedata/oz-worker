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
-module(n_handle_service_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_handle_service_logic_plugin).

-export([create/5, create/2]).

-export([
    add_user/3,
    add_group/3
]).

-export([get/2]).

-export([update/3]).

-export([delete/2]).

create(Issuer, Name, ProxyEndpoint, ServiceProperties, Type) ->
    create(Issuer, #{
        <<"name">> => Name,
        <<"proxyEndpoint">> => ProxyEndpoint,
        <<"serviceProperties">> => ServiceProperties,
        <<"type">> => Type
    }).
create(Issuer, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, undefined, entity, Data).


add_user(Issuer, HandleServiceId, UserId) when is_binary(UserId) ->
    add_user(Issuer, HandleServiceId, #{<<"userId">> => UserId});
add_user(Issuer, HandleServiceId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, HandleServiceId, users, Data).


add_group(Issuer, HandleServiceId, GroupId) when is_binary(GroupId) ->
    add_group(Issuer, HandleServiceId, #{<<"groupId">> => GroupId});
add_group(Issuer, HandleServiceId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, HandleServiceId, groups, Data).


get(Issuer, HandleServiceId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, entity, HandleServiceId).

%%add_relation(Issuer, {HandleServiceId, users}, od_user, UserId) ->
%%    n_entity_logic:add_relation(
%%        Issuer, ?PLUGIN, {HandleServiceId, users}, od_user, UserId
%%    ).




update(Issuer, HandleServiceId, Data) ->
    n_entity_logic:update(Issuer, ?PLUGIN, HandleServiceId, entity, Data).

delete(Issuer, HandleServiceId) ->
    n_entity_logic:delete(Issuer, ?PLUGIN, HandleServiceId, entity).
