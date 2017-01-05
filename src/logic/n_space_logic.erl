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
-module(n_space_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_space_logic_plugin).

-export([
    create/2
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
    create_user_invite_token/2,
    create_group_invite_token/2,
    create_provider_invite_token/2,
    add_user/4, add_user/3,
    add_group/4, add_group/3,
    get_users/2,
    update_user_privileges/5, update_user_privileges/4,
    update_group_privileges/5, update_group_privileges/4,
    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    has_eff_privilege/3,
    has_eff_user/2
]).


create(Client, Name) when is_binary(Name) ->
    create(Client, #{<<"name">> => Name});
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).


create_provider_invite_token(Client, SpaceId) ->
    n_entity_logic:create(Client, ?PLUGIN, SpaceId, invite_provider_token, #{}).


create_user_invite_token(Client, SpaceId) ->
    n_entity_logic:create(Client, ?PLUGIN, SpaceId, invite_user_token, #{}).


create_group_invite_token(Client, SpaceId) ->
    n_entity_logic:create(Client, ?PLUGIN, SpaceId, invite_group_token, #{}).


get(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, entity).

get_data(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, data).


list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


get_users(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, users).


add_user(Client, SpaceId, UserId, Privileges) when is_binary(UserId) ->
    add_user(Client, SpaceId, #{
        <<"userId">> => UserId,
        <<"privileges">> => Privileges
    }).
add_user(Client, SpaceId, UserId) when is_binary(UserId) ->
    add_user(Client, SpaceId, #{<<"userId">> => UserId});
add_user(Client, SpaceId, Data) ->
    n_entity_logic:create(
        Client, ?PLUGIN, SpaceId, users, Data
    ).


update_user_privileges(Client, SpaceId, UserId, Operation, Privs) when is_list(Privs) ->
    update_user_privileges(Client, SpaceId, UserId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_user_privileges(Client, SpaceId, UserId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, SpaceId, {user, UserId}, Data).


remove_user(Client, SpaceId, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, {user, UserId}).


add_group(Client, SpaceId, GroupId, Privileges) when is_binary(GroupId) ->
    add_group(Client, SpaceId, #{
        <<"groupId">> => GroupId,
        <<"privileges">> => Privileges
    }).
add_group(Client, SpaceId, GroupId) when is_binary(GroupId) ->
    add_group(Client, SpaceId, #{<<"groupId">> => GroupId});
add_group(Client, SpaceId, Data) ->
    n_entity_logic:create(
        Client, ?PLUGIN, SpaceId, groups, Data
    ).


update_group_privileges(Client, SpaceId, GroupId, Operation, Privs) when is_list(Privs) ->
    update_group_privileges(Client, SpaceId, GroupId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_group_privileges(Client, SpaceId, GroupId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, SpaceId, {group, GroupId}, Data).


remove_group(Client, SpaceId, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, {group, GroupId}).


update(Client, SpaceId, NewName) when is_binary(NewName) ->
    update(Client, SpaceId, #{<<"name">> => NewName});
update(Client, SpaceId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, SpaceId, entity, Data).


delete(Client, SpaceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, entity).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a space exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(SpaceId :: od_space:id()) -> boolean().
exists(SpaceId) ->
    od_space:exists(SpaceId).


has_eff_privilege(SpaceId, UserId, Privilege) when is_binary(SpaceId) ->
    case od_space:get(SpaceId) of
        {ok, #document{value = Space}} ->
            has_eff_privilege(Space, UserId, Privilege);
        _ ->
            false
    end;
has_eff_privilege(#od_space{eff_users = UsersPrivileges}, UserId, Privilege) ->
    {UserPrivileges, _} = maps:get(UserId, UsersPrivileges, {[], []}),
    lists:member(Privilege, UserPrivileges).


has_eff_user(SpaceId, UserId) when is_binary(SpaceId) ->
    {ok, #document{value = Space}} = od_space:get(SpaceId),
    has_eff_user(Space, UserId);
has_eff_user(#od_space{eff_users = EffUsers}, UserId) ->
    maps:is_key(UserId, EffUsers).

