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

-export([create/4, create/2]).

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


create(Client, Name, ProxyEndpoint, ServiceProperties) ->
    create(Client, #{
        <<"name">> => Name,
        <<"proxyEndpoint">> => ProxyEndpoint,
        <<"serviceProperties">> => ServiceProperties
    }).
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).


add_user(Client, HandleServiceId, UserId) when is_binary(UserId) ->
    add_user(Client, HandleServiceId, #{<<"userId">> => UserId});
add_user(Client, HandleServiceId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, HandleServiceId, users, Data).


add_group(Client, HandleServiceId, GroupId) when is_binary(GroupId) ->
    add_group(Client, HandleServiceId, #{<<"groupId">> => GroupId});
add_group(Client, HandleServiceId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, HandleServiceId, groups, Data).


get(Client, HandleServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, entity, HandleServiceId).


list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


%%add_relation(Client, {HandleServiceId, users}, od_user, UserId) ->
%%    n_entity_logic:add_relation(
%%        Client, ?PLUGIN, {HandleServiceId, users}, od_user, UserId
%%    ).




update(Client, HandleServiceId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, HandleServiceId, entity, Data).

delete(Client, HandleServiceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, HandleServiceId, entity).


has_eff_privilege(HServiceId, UserId, Privilege) when is_binary(HServiceId) ->
    case od_handle_service:get(HServiceId) of
        {ok, #document{value = HService}} ->
            has_eff_privilege(HService, UserId, Privilege);
        _ ->
            false
    end;
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


