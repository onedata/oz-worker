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

-export([create/3, get/2, add_relation/4, update/3, delete/2]).

create(Issuer, Resource, Data) ->
    n_entity_logic:create(Issuer, n_group_logic_plugin, Resource, Data).

get(Issuer, Resource) ->
    n_entity_logic:get(Issuer, n_group_logic_plugin, Resource).

add_relation(Issuer, {GroupId, users}, od_user, UserId) ->
    n_entity_logic:add_relation(
        Issuer, n_group_logic_plugin, {GroupId, users}, od_user, UserId
    ).

update(Issuer, Resource, Data) ->
    n_entity_logic:update(Issuer, n_group_logic_plugin, Resource, Data).

delete(Issuer, Resource) ->
    n_entity_logic:delete(Issuer, n_group_logic_plugin, Resource).
