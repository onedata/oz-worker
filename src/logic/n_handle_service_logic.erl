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

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_handle_service_logic_plugin).

-export([create/5, create/2]).

-export([
    add_user/3,
    add_group/3
]).

-export([
    get/2,
    list/1
]).

-export([update/3]).

-export([delete/2]).

-export([
    exists/1,
    has_eff_privilege/3
]).


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


list(Issuer) ->
    n_entity_logic:get(Issuer, ?PLUGIN, undefined, list).


%%add_relation(Issuer, {HandleServiceId, users}, od_user, UserId) ->
%%    n_entity_logic:add_relation(
%%        Issuer, ?PLUGIN, {HandleServiceId, users}, od_user, UserId
%%    ).




update(Issuer, HandleServiceId, Data) ->
    n_entity_logic:update(Issuer, ?PLUGIN, HandleServiceId, entity, Data).

delete(Issuer, HandleServiceId) ->
    n_entity_logic:delete(Issuer, ?PLUGIN, HandleServiceId, entity).


has_eff_privilege(HServiceId, UserId, Privilege) when is_binary(HServiceId) ->
    {ok, #document{value = HService}} = od_handle_service:get(HServiceId),
    has_eff_privilege(HService, UserId, Privilege);
has_eff_privilege(#od_handle_service{eff_users = UsersPrivileges}, UserId, Privilege) ->
    {UserPrivileges, _} = maps:get(UserId, UsersPrivileges, {[], []}),
    lists:member(Privilege, UserPrivileges).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a handle service exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(HandleServiceId :: od_handle_service:id()) -> boolean().
exists(HandleServiceId) ->
    od_handle_service:exists(HandleServiceId).


