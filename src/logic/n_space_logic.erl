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

    get_users/2, get_eff_users/2,
    get_user/3, get_eff_user/3,
    get_user_privileges/3, get_eff_user_privileges/3,

    get_groups/2, get_eff_groups/2,
    get_group/3, get_eff_group/3,
    get_group_privileges/3, get_eff_group_privileges/3,

    get_shares/2, get_share/3,

    get_providers/2, get_provider/3,

    update_user_privileges/5, update_user_privileges/4,
    update_group_privileges/5, update_group_privileges/4,

    leave_provider/3,

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


get(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, entity).

get_data(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, data).


list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


update(Client, SpaceId, NewName) when is_binary(NewName) ->
    update(Client, SpaceId, #{<<"name">> => NewName});
update(Client, SpaceId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, SpaceId, entity, Data).


delete(Client, SpaceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, entity).


create_user_invite_token(Client, SpaceId) ->
    n_entity_logic:create(Client, ?PLUGIN, SpaceId, invite_user_token, #{}).


create_group_invite_token(Client, SpaceId) ->
    n_entity_logic:create(Client, ?PLUGIN, SpaceId, invite_group_token, #{}).


create_provider_invite_token(Client, SpaceId) ->
    n_entity_logic:create(Client, ?PLUGIN, SpaceId, invite_provider_token, #{}).


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


get_shares(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, shares).


get_share(Client, UserId, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {share, SpaceId}).


get_providers(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, providers).


get_provider(Client, UserId, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {provider, SpaceId}).




update_user_privileges(Client, SpaceId, UserId, Operation, Privs) when is_list(Privs) ->
    update_user_privileges(Client, SpaceId, UserId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_user_privileges(Client, SpaceId, UserId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, SpaceId, {user, UserId}, Data).


update_group_privileges(Client, SpaceId, GroupId, Operation, Privs) when is_list(Privs) ->
    update_group_privileges(Client, SpaceId, GroupId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_group_privileges(Client, SpaceId, GroupId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, SpaceId, {group, GroupId}, Data).


leave_provider(Client, SpaceId, ProviderId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, {provider, ProviderId}).


remove_user(Client, SpaceId, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, {user, UserId}).


remove_group(Client, SpaceId, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, {group, GroupId}).


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

