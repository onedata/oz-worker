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

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_handle_logic_plugin).

-export([
    create/5, create/2
]).
-export([
    get/2,
    get_data/2,
    get_public_data/2,
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

    update_user_privileges/5, update_user_privileges/4,
    update_group_privileges/5, update_group_privileges/4,

    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    user_has_eff_privilege/3,
    group_has_eff_privilege/3
]).



create(Client, HServiceId, ResourceType, ResourceId, Metadata) ->
    create(Client, #{
        <<"handleServiceId">> => HServiceId,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => Metadata
    }).
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).


get(Client, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, entity, HandleId).


get_data(Client, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, data).


get_public_data(Client, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, public_data).


list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


update(Client, HandleId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, HandleId, entity, Data).


delete(Client, HandleId) ->
    n_entity_logic:delete(Client, ?PLUGIN, HandleId, entity).


add_user(Client, HandleId, UserId, Privileges) when is_binary(UserId) ->
    add_user(Client, HandleId, #{
        <<"userId">> => UserId,
        <<"privileges">> => Privileges
    }).
add_user(Client, HandleId, UserId) when is_binary(UserId) ->
    add_user(Client, HandleId, #{<<"userId">> => UserId});
add_user(Client, HandleId, Data) ->
    n_entity_logic:create(
        Client, ?PLUGIN, HandleId, users, Data
    ).


add_group(Client, HandleId, GroupId, Privileges) when is_binary(GroupId) ->
    add_group(Client, HandleId, #{
        <<"groupId">> => GroupId,
        <<"privileges">> => Privileges
    }).
add_group(Client, HandleId, GroupId) when is_binary(GroupId) ->
    add_group(Client, HandleId, #{<<"groupId">> => GroupId});
add_group(Client, HandleId, Data) ->
    n_entity_logic:create(
        Client, ?PLUGIN, HandleId, groups, Data
    ).


get_users(Client, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, users).


get_eff_users(Client, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, eff_users).


get_user(Client, HandleId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {user, UserId}).


get_eff_user(Client, HandleId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {eff_user, UserId}).


get_user_privileges(Client, HandleId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {user_privileges, UserId}).


get_eff_user_privileges(Client, HandleId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {eff_user_privileges, UserId}).


get_groups(Client, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, groups).


get_eff_groups(Client, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, eff_groups).


get_group(Client, HandleId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {group, GroupId}).


get_eff_group(Client, HandleId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {eff_group, GroupId}).


get_group_privileges(Client, HandleId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {group_privileges, GroupId}).


get_eff_group_privileges(Client, HandleId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {eff_group_privileges, GroupId}).


update_user_privileges(Client, HandleId, UserId, Operation, Privs) when is_list(Privs) ->
    update_user_privileges(Client, HandleId, UserId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_user_privileges(Client, HandleId, UserId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, HandleId, {user_privileges, UserId}, Data).


update_group_privileges(Client, HandleId, GroupId, Operation, Privs) when is_list(Privs) ->
    update_group_privileges(Client, HandleId, GroupId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_group_privileges(Client, HandleId, GroupId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, HandleId, {group_privileges, GroupId}, Data).


remove_user(Client, HandleId, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, HandleId, {user, UserId}).


remove_group(Client, HandleId, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, HandleId, {group, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a handle exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(HandleId :: od_handle:id()) -> boolean().
exists(HandleId) ->
    od_handle:exists(HandleId).


user_has_eff_privilege(HandleId, UserId, Privilege) when is_binary(HandleId) ->
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle}} ->
            user_has_eff_privilege(Handle, UserId, Privilege);
        _ ->
            false
    end;
user_has_eff_privilege(#od_handle{eff_users = UsersPrivileges}, UserId, Privilege) ->
    {UserPrivileges, _} = maps:get(UserId, UsersPrivileges, {[], []}),
    lists:member(Privilege, UserPrivileges).


group_has_eff_privilege(HandleId, GroupId, Privilege) when is_binary(HandleId) ->
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle}} ->
            group_has_eff_privilege(Handle, GroupId, Privilege);
        _ ->
            false
    end;
group_has_eff_privilege(#od_handle{eff_groups = GroupsPrivileges}, GroupId, Privilege) ->
    {GroupPrivileges, _} = maps:get(GroupId, GroupsPrivileges, {[], []}),
    lists:member(Privilege, GroupPrivileges).

