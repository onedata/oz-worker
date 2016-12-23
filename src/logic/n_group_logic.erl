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

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_group_logic_plugin).

-export([
    create/2
]).
-export([
    get/2,
    list/1
]).
-export([
    update/3,
    modify_oz_privileges/4, modify_oz_privileges/3
]).
-export([
    delete/2
]).
-export([
    create_space_create_token/3,
    join_group/3,
    join_space/3,
    add_user/3,
    add_group/3,
    update_user_privileges/5, update_user_privileges/4,
    update_group_privileges/5, update_group_privileges/4,
    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    has_eff_privilege/3
]).

create(Client, Name) when is_binary(Name) ->
    create(Client, #{<<"name">> => Name});
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).


get(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, entity, GroupId).


list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


update(Client, GroupId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, GroupId, entity, Data).


modify_oz_privileges(Client, GroupId, Operation, Privs) when is_list(Privs) ->
    modify_oz_privileges(Client, GroupId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
modify_oz_privileges(Client, GroupId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, GroupId, oz_privileges, Data).


delete(Client, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, GroupId, entity).


add_user(Client, GroupId, UserId) when is_binary(UserId) ->
    add_user(Client, GroupId, #{<<"userId">> => UserId});
add_user(Client, GroupId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, users, Data).


update_user_privileges(Client, GroupId, UserId, Operation, Privs) when is_list(Privs) ->
    update_user_privileges(Client, GroupId, UserId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_user_privileges(Client, GroupId, UserId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, GroupId, {user, UserId}, Data).


remove_user(Client, GroupId, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, GroupId, {user, UserId}).


add_group(Client, GroupId, ChildGroupId) when is_binary(ChildGroupId) ->
    add_group(Client, GroupId, #{<<"groupId">> => ChildGroupId});
add_group(Client, GroupId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, groups, Data).


update_group_privileges(Client, ParentId, ChildId, Operation, Privs) when is_list(Privs) ->
    update_group_privileges(Client, ParentId, ChildId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_group_privileges(Client, ParentId, ChildId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, ParentId, {group, ChildId}, Data).


remove_group(Client, ParentId, ChildId) ->
    n_entity_logic:delete(Client, ?PLUGIN, ParentId, {group, ChildId}).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a group exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(GroupId :: od_group:id()) -> boolean().
exists(GroupId) ->
    od_group:exists(GroupId).


has_eff_privilege(GroupId, UserId, Privilege) when is_binary(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            has_eff_privilege(Group, UserId, Privilege);
        _ ->
            false
    end;
has_eff_privilege(#od_group{eff_users = UsersPrivileges}, UserId, Privilege) ->
    {UserPrivileges, _} = maps:get(UserId, UsersPrivileges, {[], []}),
    lists:member(Privilege, UserPrivileges).
