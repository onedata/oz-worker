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

    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    has_eff_privilege/3
]).



create(Client, HandleServiceId, ResourceType, ResourceId, Metadata) ->
    create(Client, #{
        <<"handleServiceId">> => HandleServiceId,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => Metadata
    }).
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).


get(Client, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, entity, HandleId).

get_data(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, data).


get_public_data(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, public_data).


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


get_users(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, users).


get_eff_users(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, eff_users).


get_user(Client, UserId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {user, GroupId}).


get_eff_user(Client, UserId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {eff_user, GroupId}).


get_user_privileges(Client, UserId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {user_privileges, GroupId}).


get_eff_user_privileges(Client, UserId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {eff_user_privileges, GroupId}).


get_groups(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, groups).


get_eff_groups(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, eff_groups).


get_group(Client, UserId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {group, GroupId}).


get_eff_group(Client, UserId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {eff_group, GroupId}).


get_group_privileges(Client, UserId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {group_privileges, GroupId}).


get_eff_group_privileges(Client, UserId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {eff_group_privileges, GroupId}).


remove_user(Client, SpaceId, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, {user, UserId}).


remove_group(Client, SpaceId, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, {group, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a handle exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(HandleId :: od_handle:id()) -> boolean().
exists(HandleId) ->
    od_handle:exists(HandleId).


has_eff_privilege(HandleId, UserId, Privilege) when is_binary(HandleId) ->
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle}} ->
            has_eff_privilege(Handle, UserId, Privilege);
        _ ->
            false
    end;
has_eff_privilege(#od_handle{eff_users = UsersPrivileges}, UserId, Privilege) ->
    {UserPrivileges, _} = maps:get(UserId, UsersPrivileges, {[], []}),
    lists:member(Privilege, UserPrivileges).

