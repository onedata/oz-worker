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

-export([create/2, get/2, update/3, delete/2]).

create(Issuer, Name) when is_binary(Name) ->
    create(Issuer, #{<<"name">> => Name});
create(Issuer, Data) ->
    n_entity_logic:create(Issuer, n_group_logic_plugin, undefined, entity, Data).

get(Issuer, GroupId) ->
    n_entity_logic:get(Issuer, n_group_logic_plugin, entity, GroupId).

%%add_relation(Issuer, {GroupId, users}, od_user, UserId) ->
%%    n_entity_logic:add_relation(
%%        Issuer, n_group_logic_plugin, {GroupId, users}, od_user, UserId
%%    ).

update(Issuer, GroupId, Data) ->
    n_entity_logic:update(Issuer, n_group_logic_plugin, GroupId, entity, Data).

delete(Issuer, GroupId) ->
    n_entity_logic:delete(Issuer, n_group_logic_plugin, GroupId, entity).
