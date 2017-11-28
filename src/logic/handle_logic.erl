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
-module(handle_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, handle_logic_plugin).

-export([
    create/5, create/2
]).
-export([
    get/2,
    get_protected_data/2,
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
    has_eff_privilege/3,
    has_eff_user/2,
    has_eff_group/2,
    has_handle_service/2
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
-spec create(Client :: entity_logic:client(), HandleId :: od_handle_service:id(),
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
-spec create(Client :: entity_logic:client(), Data :: #{}) ->
    {ok, od_handle:id()} | {error, term()}.
create(Client, Data) ->
    AuthHint = case Client of
        ?USER(UserId) -> ?AS_USER(UserId);
        _ -> undefined
    end,
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_handle, id = undefined, aspect = instance},
        data = Data,
        auth_hint = AuthHint
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a handle record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: entity_logic:client(), HandleId :: od_handle:id()) ->
    {ok, #od_handle{}} | {error, term()}.
get(Client, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves protected handle data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_protected_data(Client :: entity_logic:client(), HandleId :: od_handle:id()) ->
    {ok, maps:map()} | {error, term()}.
get_protected_data(Client, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance, scope = protected}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Lists all handles (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Client :: entity_logic:client()) ->
    {ok, [od_handle:id()]} | {error, term()}.
list(Client) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_handle, id = undefined, aspect = list}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given handle (currently only Metadata is supported).
%% Has two variants:
%% 1) Metadata is given explicitly
%% 2) Metadata is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    MetadataOrData :: od_handle:metadata() | #{}) -> ok | {error, term()}.
update(Client, HandleId, Metadata) when is_binary(Metadata) ->
    update(Client, HandleId, #{<<"metadata">> => Metadata});
update(Client, HandleId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given handle from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Client :: entity_logic:client(), HandleId :: od_handle:id()) ->
    ok | {error, term()}.
delete(Client, HandleId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given handle.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Client :: entity_logic:client(),
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
-spec add_user(Client :: entity_logic:client(),
    HandleId :: od_handle:id(), UserId :: od_user:id(),
    PrivilegesPrivilegesOrData :: [privileges:handle_privileges()] | #{}) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Client, HandleId, UserId, Privileges) when is_list(Privileges) ->
    add_user(Client, HandleId, UserId, #{
        <<"privileges">> => Privileges
    });
add_user(Client, HandleId, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = {user, UserId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given handle.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Client :: entity_logic:client(),
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
-spec add_group(Client :: entity_logic:client(),
    HandleId :: od_handle:id(), GroupId :: od_group:id(),
    PrivilegesOrData :: [privileges:handle_privileges()] | #{}) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Client, HandleId, GroupId, Privileges) when is_list(Privileges) ->
    add_group(Client, HandleId, GroupId, #{
        <<"privileges">> => Privileges
    });
add_group(Client, HandleId, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = {group, GroupId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Client :: entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_users(Client, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Client :: entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_eff_users(Client, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = eff_users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific user among users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_user(Client, HandleId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HANDLE(HandleId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_eff_user(Client, HandleId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HANDLE(HandleId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific user among users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, [privileges:handle_privileges()]} | {error, term()}.
get_user_privileges(Client, HandleId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = {user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective user
%% among effective users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_privileges(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, [privileges:handle_privileges()]} | {error, term()}.
get_eff_user_privileges(Client, HandleId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = {eff_user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Client :: entity_logic:client(), HandleId :: od_handle:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_groups(Client, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_groups(Client :: entity_logic:client(), HandleId :: od_handle:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_groups(Client, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = eff_groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific group among groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_group(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_group(Client, HandleId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HANDLE(HandleId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective group among
%% effective groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_group(Client, HandleId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HANDLE(HandleId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific group among groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:handle_privileges()]} | {error, term()}.
get_group_privileges(Client, HandleId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = {group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective group
%% among effective groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_privileges(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:handle_privileges()]} | {error, term()}.
get_eff_group_privileges(Client, HandleId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = {eff_group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given handle.
%% Allows to specify operation (set | grant | revoke) and the privileges.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Client :: entity_logic:client(), HandleId :: od_handle:id(),
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
-spec update_user_privileges(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_user_privileges(Client, HandleId, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect =  {user_privileges, UserId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given handle.
%% Allows to specify operation (set | grant | revoke) and the privileges.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Client :: entity_logic:client(), HandleId :: od_handle:id(),
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
-spec update_group_privileges(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_group_privileges(Client, HandleId, GroupId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = {group_privileges, GroupId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified user from given handle.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> ok | {error, term()}.
remove_user(Client, HandleId, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = {user, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified group from given handle.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Client :: entity_logic:client(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> ok | {error, term()}.
remove_group(Client, HandleId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_handle, id = HandleId, aspect = {group, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a handle exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(HandleId :: od_handle:id()) -> boolean().
exists(HandleId) ->
    {ok, Exists} = od_handle:exists(HandleId),
    Exists.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified effective user has specified
%% effective privilege in given handle.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_privilege(HandleOrId :: od_handle:id() | #od_handle{},
    UserId :: od_user:id(), Privilege :: privileges:handle_privileges()) ->
    boolean().
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


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an effective user in given handle.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_user(HandleOrId :: od_handle:id() | #od_handle{},
    UserId :: od_user:id()) -> boolean().
has_eff_user(HandleId, UserId) when is_binary(HandleId) ->
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle}} ->
            has_eff_user(Handle, UserId);
        _ ->
            false
    end;
has_eff_user(#od_handle{eff_users = EffUsers}, UserId) ->
    maps:is_key(UserId, EffUsers).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group in given
%% handle.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_group(HandleOrId :: od_handle:id() | #od_handle{},
    GroupId :: od_group:id()) -> boolean().
has_eff_group(HandleId, GroupId) when is_binary(HandleId) ->
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle}} ->
            has_eff_group(Handle, GroupId);
        _ ->
            false
    end;
has_eff_group(#od_handle{eff_groups = EffGroups}, GroupId) ->
    maps:is_key(GroupId, EffGroups).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether handle belongs to specified handle service.
%% @end
%%--------------------------------------------------------------------
-spec has_handle_service(HandleOrId :: od_handle:id() | #od_handle{},
    HServiceId :: od_handle_service:id()) -> boolean().
has_handle_service(HandleId, HServiceId) when is_binary(HandleId) ->
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle}} ->
            has_handle_service(Handle, HServiceId);
        _ ->
            false
    end;
has_handle_service(#od_handle{handle_service = HService}, HServiceId) ->
    HServiceId =:= HService.
