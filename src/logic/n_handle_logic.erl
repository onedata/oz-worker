%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all handle service logic functionalities.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(n_handle_logic).
-author("Lukasz Opiola").

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

    add_user/3, add_user/4,
    add_group/3, add_group/4,

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

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle document in database based on HandleServiceId,
%% ResourceType, ResourceId and Metadata.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: n_entity_logic:client(), HandleId :: od_handle_service:id(),
    ResourceType :: od_handle:resource_type(), ResourceId :: od_handle:resource_id(),
    Metadata :: od_handle:metadata()) -> {ok, od_handle:id()} | {error, term()}.
create(Client, HServiceId, ResourceType, ResourceId, Metadata) ->
    create(Client, #{
        <<"handleServiceId">> => HServiceId,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => Metadata
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle document in database. HandleServiceId, ResourceType,
%% ResourceId and Metadata are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: n_entity_logic:client(), Data :: #{}) ->
    {ok, od_handle:id()} | {error, term()}.
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a handle record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: n_entity_logic:client(), HandleId :: od_handle:id()) ->
    {ok, #od_handle{}} | {error, term()}.
get(Client, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, entity).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves information about a handle record from database.
%% @end
%%--------------------------------------------------------------------
-spec get_data(Client :: n_entity_logic:client(), HandleId :: od_handle:id()) ->
    {ok, #{}} | {error, term()}.
get_data(Client, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, data).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves public information (available without any authorization)
%% about a handle record from database.
%% @end
%%--------------------------------------------------------------------
-spec get_public_data(Client :: n_entity_logic:client(), HandleId :: od_handle:id()) ->
    {ok, #{}} | {error, term()}.
get_public_data(Client, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, HServiceId, public_data).


%%--------------------------------------------------------------------
%% @doc
%% Lists all handles (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Client :: n_entity_logic:client()) ->
    {ok, [od_handle:id()]} | {error, term()}.
list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given handle (currently only Metadata is supported).
%% Has two variants:
%% 1) Metadata is given explicitly
%% 2) Metadata is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    MetadataOrData :: od_handle:metadata() | #{}) -> ok | {error, term()}.
update(Client, HandleId, Metadata) when is_binary(Metadata) ->
    update(Client, HandleId, #{<<"metadata">> => Metadata});
update(Client, HandleId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, HandleId, entity, Data).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given handle from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Client :: n_entity_logic:client(), HandleId :: od_handle:id()) ->
    ok | {error, term()}.
delete(Client, HandleId) ->
    n_entity_logic:delete(Client, ?PLUGIN, HandleId, entity).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given handle.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Client :: n_entity_logic:client(),
    HandleId :: od_handle:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Client, HandleId, UserId) ->
    add_user(Client, HandleId, UserId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given handle.
%% Allows to specify the privileges of the newly added user. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Client :: n_entity_logic:client(),
    HandleId :: od_handle:id(), UserId :: od_user:id(),
    PrivilegesPrivilegesOrData :: [privileges:handle_privileges()] | #{}) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Client, HandleId, UserId, Privileges) when is_list(Privileges) ->
    add_user(Client, HandleId, UserId, #{
        <<"privileges">> => Privileges
    });
add_user(Client, HandleId, UserId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, HandleId, {user, UserId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given handle.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Client :: n_entity_logic:client(),
    HandleId :: od_handle:id(), GroupId :: od_group:id()) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Client, HandleId, GroupId) ->
    add_group(Client, HandleId, GroupId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given handle.
%% Allows to specify the privileges of the newly added group. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Client :: n_entity_logic:client(),
    HandleId :: od_handle:id(), GroupId :: od_group:id(),
    PrivilegesOrData :: [privileges:handle_privileges()] | #{}) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Client, HandleId, GroupId, Privileges) when is_list(Privileges) ->
    add_group(Client, HandleId, GroupId, #{
        <<"privileges">> => Privileges
    });
add_group(Client, HandleId, GroupId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, HandleId, {group, GroupId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_users(Client, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, users).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_eff_users(Client, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, eff_users).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific user among users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_user(Client, HandleId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {user, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_eff_user(Client, HandleId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {eff_user, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific user among users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, [privileges:handle_privileges()]} | {error, term()}.
get_user_privileges(Client, HandleId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {user_privileges, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective user
%% among effective users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_privileges(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, [privileges:handle_privileges()]} | {error, term()}.
get_eff_user_privileges(Client, HandleId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {eff_user_privileges, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Client :: n_entity_logic:client(), HandleId :: od_handle:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_groups(Client, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, groups).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_groups(Client :: n_entity_logic:client(), HandleId :: od_handle:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_groups(Client, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, eff_groups).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific group among groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_group(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_group(Client, HandleId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {group, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective group among
%% effective groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_group(Client, HandleId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {eff_group, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific group among groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:handle_privileges()]} | {error, term()}.
get_group_privileges(Client, HandleId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {group_privileges, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective group
%% among effective groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_privileges(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:handle_privileges()]} | {error, term()}.
get_eff_group_privileges(Client, HandleId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, HandleId, {eff_group_privileges, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given handle.
%% Allows to specify operation (set | grant | revoke) and the privileges.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id(), Operation :: entity_graph:privileges_operation(),
    Privs :: [privileges:handle_privilege()]) -> ok | {error, term()}.
update_user_privileges(Client, HandleId, UserId, Operation, Privs) when is_list(Privs) ->
    update_user_privileges(Client, HandleId, UserId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given handle.
%% Privileges must be included in proper Data object, operation is optional.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_user_privileges(Client, HandleId, UserId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, HandleId, {user_privileges, UserId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given handle.
%% Allows to specify operation (set | grant | revoke) and the privileges.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_group:id(), Operation :: entity_graph:privileges_operation(),
    Privs :: [privileges:handle_privilege()]) -> ok | {error, term()}.
update_group_privileges(Client, HandleId, GroupId, Operation, Privs) when is_list(Privs) ->
    update_group_privileges(Client, HandleId, GroupId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given handle.
%% Privileges must be included in proper Data object, operation is optional.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_group_privileges(Client, HandleId, GroupId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, HandleId, {group_privileges, GroupId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified user from given handle.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> ok | {error, term()}.
remove_user(Client, HandleId, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, HandleId, {user, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified group from given handle.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Client :: n_entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> ok | {error, term()}.
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


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified effective user has specified
%% effective privilege in given handle.
%% @end
%%--------------------------------------------------------------------
-spec user_has_eff_privilege(HandleOrId :: od_handle:id() | #od_handle{},
    UserId :: od_user:id(), Privilege :: privileges:handle_privileges()) ->
    boolean().
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


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified effective group has specified
%% effective privilege in given handle.
%% @end
%%--------------------------------------------------------------------
-spec group_has_eff_privilege(HandleOrId :: od_handle:id() | #od_handle{},
    GroupId :: od_group:id(), Privilege :: privileges:handle_privileges()) ->
    boolean().
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

