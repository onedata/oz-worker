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

-export([
    create/4, create/2
]).
-export([
    get/2,
    get_data/2,
    list/1
]).
-export([
    update/3
]).
-export([
    delete/2
]).
-export([
    add_user/4, add_user/3,
    add_group/4, add_group/3,

    get_users/2, get_eff_users/2,
    get_user/3, get_eff_user/3,
    get_user_privileges/3, get_eff_user_privileges/3,

    get_groups/2, get_eff_groups/2,
    get_group/3, get_eff_group/3,
    get_group_privileges/3, get_eff_group_privileges/3,

    get_handles/2, get_handle/3,

    remove_user/3,
    remove_group/3
]).
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


get(Client, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, entity, HServiceId).


get_data(Client, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, data).


list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


update(Client, HServiceId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, HServiceId, entity, Data).


delete(Client, HServiceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, HServiceId, entity).


add_user(Client, HServiceId, UserId, Privileges) when is_binary(UserId) ->
    add_user(Client, HServiceId, #{
        <<"userId">> => UserId,
        <<"privileges">> => Privileges
    }).
add_user(Client, HServiceId, UserId) when is_binary(UserId) ->
    add_user(Client, HServiceId, #{<<"userId">> => UserId});
add_user(Client, HServiceId, Data) ->
    n_entity_logic:create(
        Client, ?PLUGIN, HServiceId, users, Data
    ).


add_group(Client, HServiceId, GroupId, Privileges) when is_binary(GroupId) ->
    add_group(Client, HServiceId, #{
        <<"groupId">> => GroupId,
        <<"privileges">> => Privileges
    }).
add_group(Client, HServiceId, GroupId) when is_binary(GroupId) ->
    add_group(Client, HServiceId, #{<<"groupId">> => GroupId});
add_group(Client, HServiceId, Data) ->
    n_entity_logic:create(
        Client, ?PLUGIN, HServiceId, groups, Data
    ).


get_users(Client, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, users).


get_eff_users(Client, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, eff_users).


get_user(Client, HServiceId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, {user, UserId}).


get_eff_user(Client, HServiceId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, {eff_user, UserId}).


get_user_privileges(Client, HServiceId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, {user_privileges, UserId}).


get_eff_user_privileges(Client, HServiceId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, {eff_user_privileges, UserId}).


get_groups(Client, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, groups).


get_eff_groups(Client, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, eff_groups).


get_group(Client, HServiceId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, {group, GroupId}).


get_eff_group(Client, HServiceId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, {eff_group, GroupId}).


get_group_privileges(Client, HServiceId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, {group_privileges, GroupId}).


get_eff_group_privileges(Client, HServiceId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, {eff_group_privileges, GroupId}).


get_handles(Client, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, handles).


get_handle(Client, HServiceId, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, {handle, HandleId}).


remove_user(Client, HServiceId, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, HServiceId, {user, UserId}).


remove_group(Client, HServiceId, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, HServiceId, {group, GroupId}).


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
-spec exists(HServiceId :: od_handle_service:id()) -> boolean().
exists(HServiceId) ->
    od_handle_service:exists(HServiceId).


