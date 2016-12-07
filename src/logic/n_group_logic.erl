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

-export([create/2]).

-export([
    add_user/3,
    add_group/3
]).

-export([get/2]).

-export([
    update/3,
    modify_oz_privileges/4, modify_oz_privileges/3
]).

-export([delete/2]).

-export([
    exists/1,
    has_eff_privilege/3
]).

create(Issuer, Name) when is_binary(Name) ->
    create(Issuer, #{<<"name">> => Name});
create(Issuer, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, undefined, entity, Data).


add_user(Issuer, GroupId, UserId) when is_binary(UserId) ->
    add_user(Issuer, GroupId, #{<<"userId">> => UserId});
add_user(Issuer, GroupId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, GroupId, users, Data).


add_group(Issuer, GroupId, ChildGroupId) when is_binary(ChildGroupId) ->
    add_group(Issuer, GroupId, #{<<"groupId">> => ChildGroupId});
add_group(Issuer, GroupId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, GroupId, groups, Data).


get(Issuer, GroupId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, entity, GroupId).



update(Issuer, GroupId, Data) ->
    n_entity_logic:update(Issuer, ?PLUGIN, GroupId, entity, Data).


modify_oz_privileges(Issuer, GroupId, Operation, Privs) when is_list(Privs) ->
    modify_oz_privileges(Issuer, GroupId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
modify_oz_privileges(Issuer, GroupId, Data) ->
    n_entity_logic:update(Issuer, ?PLUGIN, GroupId, oz_privileges, Data).



delete(Issuer, GroupId) ->
    n_entity_logic:delete(Issuer, ?PLUGIN, GroupId, entity).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a group exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(GroupId :: od_group:id()) -> boolean().
exists(GroupId) ->
    od_group:exists(GroupId).




has_eff_privilege(GroupId, UserId, Privilege) when is_binary(GroupId) ->
    {ok, #document{value = Group}} = od_group:get(GroupId),
    has_eff_privilege(Group, UserId, Privilege);
has_eff_privilege(#od_group{eff_users = UsersPrivileges}, UserId, Privilege) ->
    % TODO eff_users
    {UserPrivileges, _} = maps:get(UserId, UsersPrivileges, []),
    lists:member(Privilege, UserPrivileges).
