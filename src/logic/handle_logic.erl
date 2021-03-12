%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all handle service logic functionality.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create/5, create/2
]).
-export([
    get/2,
    get_protected_data/2,
    get_public_data/2,
    list/1,
    list_privileges/0
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
-spec create(Auth :: aai:auth(), HandleId :: od_handle_service:id(),
    ResourceType :: od_handle:resource_type(), ResourceId :: od_handle:resource_id(),
    Metadata :: od_handle:metadata()) -> {ok, od_handle:id()} | errors:error().
create(Auth, HServiceId, ResourceType, ResourceId, Metadata) ->
    create(Auth, #{
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
-spec create(Auth :: aai:auth(), Data :: #{}) ->
    {ok, od_handle:id()} | errors:error().
create(Auth, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_handle, id = undefined, aspect = instance},
        data = Data,
        auth_hint = undefined
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a handle record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Auth :: aai:auth(), HandleId :: od_handle:id()) ->
    {ok, #od_handle{}} | errors:error().
get(Auth, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves protected handle data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_protected_data(Auth :: aai:auth(), HandleId :: od_handle:id()) ->
    {ok, map()} | errors:error().
get_protected_data(Auth, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance, scope = protected}
    }).


-spec get_public_data(aai:auth(), od_handle:id()) ->
    {ok, map()} | errors:error().
get_public_data(Auth, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance, scope = public}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Lists all handles (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Auth :: aai:auth()) ->
    {ok, [od_handle:id()]} | errors:error().
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = undefined, aspect = list}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Get all possible handle privileges.
%% @end
%%--------------------------------------------------------------------
-spec list_privileges() -> {ok, map()} | errors:error().
list_privileges() ->
    entity_logic:handle(#el_req{
        operation = get,
        gri = #gri{type = od_handle, id = undefined, aspect = privileges}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given handle (currently only Metadata is supported).
%% Has two variants:
%% 1) Metadata is given explicitly
%% 2) Metadata is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update(Auth :: aai:auth(), HandleId :: od_handle:id(),
    MetadataOrData :: od_handle:metadata() | #{}) -> ok | errors:error().
update(Auth, HandleId, Metadata) when is_binary(Metadata) ->
    update(Auth, HandleId, #{<<"metadata">> => Metadata});
update(Auth, HandleId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given handle from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Auth :: aai:auth(), HandleId :: od_handle:id()) ->
    ok | errors:error().
delete(Auth, HandleId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given handle.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Auth :: aai:auth(),
    HandleId :: od_handle:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()} | errors:error().
add_user(Auth, HandleId, UserId) ->
    add_user(Auth, HandleId, UserId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given handle.
%% Allows to specify the privileges of the newly added user. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Auth :: aai:auth(),
    HandleId :: od_handle:id(), UserId :: od_user:id(),
    PrivilegesPrivilegesOrData :: [privileges:handle_privilege()] | #{}) ->
    {ok, od_user:id()} | errors:error().
add_user(Auth, HandleId, UserId, Privileges) when is_list(Privileges) ->
    add_user(Auth, HandleId, UserId, #{
        <<"privileges">> => Privileges
    });
add_user(Auth, HandleId, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = {user, UserId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given handle.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Auth :: aai:auth(),
    HandleId :: od_handle:id(), GroupId :: od_group:id()) ->
    {ok, od_group:id()} | errors:error().
add_group(Auth, HandleId, GroupId) ->
    add_group(Auth, HandleId, GroupId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given handle.
%% Allows to specify the privileges of the newly added group. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Auth :: aai:auth(),
    HandleId :: od_handle:id(), GroupId :: od_group:id(),
    PrivilegesOrData :: [privileges:handle_privilege()] | #{}) ->
    {ok, od_group:id()} | errors:error().
add_group(Auth, HandleId, GroupId, Privileges) when is_list(Privileges) ->
    add_group(Auth, HandleId, GroupId, #{
        <<"privileges">> => Privileges
    });
add_group(Auth, HandleId, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = {group, GroupId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, [od_user:id()]} | errors:error().
get_users(Auth, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, [od_user:id()]} | errors:error().
get_eff_users(Auth, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = eff_users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific user among users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Auth :: aai:auth(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, #{}} | errors:error().
get_user(Auth, HandleId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HANDLE(HandleId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Auth :: aai:auth(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, #{}} | errors:error().
get_eff_user(Auth, HandleId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HANDLE(HandleId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific user among users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Auth :: aai:auth(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, [privileges:handle_privilege()]} | errors:error().
get_user_privileges(Auth, HandleId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = {user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective user
%% among effective users of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_privileges(Auth :: aai:auth(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, [privileges:handle_privilege()]} | errors:error().
get_eff_user_privileges(Auth, HandleId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = {eff_user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Auth :: aai:auth(), HandleId :: od_handle:id()) ->
    {ok, [od_group:id()]} | errors:error().
get_groups(Auth, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_groups(Auth :: aai:auth(), HandleId :: od_handle:id()) ->
    {ok, [od_group:id()]} | errors:error().
get_eff_groups(Auth, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = eff_groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific group among groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_group(Auth :: aai:auth(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | errors:error().
get_group(Auth, HandleId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HANDLE(HandleId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective group among
%% effective groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group(Auth :: aai:auth(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | errors:error().
get_eff_group(Auth, HandleId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HANDLE(HandleId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific group among groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(Auth :: aai:auth(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:handle_privilege()]} | errors:error().
get_group_privileges(Auth, HandleId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = {group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective group
%% among effective groups of given handle.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_privileges(Auth :: aai:auth(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:handle_privilege()]} | errors:error().
get_eff_group_privileges(Auth, HandleId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = {eff_group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given handle.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Auth :: aai:auth(), HandleId :: od_handle:id(),
    UserId :: od_user:id(), PrivsToGrant :: [privileges:handle_privilege()],
    PrivsToRevoke :: [privileges:handle_privilege()]) -> ok | errors:error().
update_user_privileges(Auth, HandleId, UserId, PrivsToGrant, PrivsToRevoke) ->
    update_user_privileges(Auth, HandleId, UserId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given handle.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Auth :: aai:auth(), HandleId :: od_handle:id(),
    UserId :: od_user:id(), Data :: #{}) -> ok | errors:error().
update_user_privileges(Auth, HandleId, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = {user_privileges, UserId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given handle.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Auth :: aai:auth(), HandleId :: od_handle:id(),
    GroupId :: od_group:id(), PrivsToGrant :: [privileges:handle_privilege()],
    PrivsToRevoke :: [privileges:handle_privilege()]) -> ok | errors:error().
update_group_privileges(Auth, HandleId, GroupId, PrivsToGrant, PrivsToRevoke) ->
    update_group_privileges(Auth, HandleId, GroupId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given handle.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Auth :: aai:auth(), HandleId :: od_handle:id(),
    GroupId :: od_user:id(), Data :: #{}) -> ok | errors:error().
update_group_privileges(Auth, HandleId, GroupId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = {group_privileges, GroupId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified user from given handle.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Auth :: aai:auth(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> ok | errors:error().
remove_user(Auth, HandleId, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = {user, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified group from given handle.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Auth :: aai:auth(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> ok | errors:error().
remove_group(Auth, HandleId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
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
    UserId :: od_user:id(), Privilege :: privileges:handle_privilege()) ->
    boolean().
has_eff_privilege(HandleId, UserId, Privilege) when is_binary(HandleId) ->
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, od_handle, HandleId);
has_eff_privilege(Handle, UserId, Privilege) ->
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, Handle).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an effective user of given handle.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_user(HandleOrId :: od_handle:id() | #od_handle{},
    UserId :: od_user:id()) -> boolean().
has_eff_user(HandleId, UserId) when is_binary(HandleId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, od_handle, HandleId);
has_eff_user(Handle, UserId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Handle).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group of given handle.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_group(HandleOrId :: od_handle:id() | #od_handle{},
    GroupId :: od_group:id()) -> boolean().
has_eff_group(HandleId, GroupId) when is_binary(HandleId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, od_handle, HandleId);
has_eff_group(Handle, GroupId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Handle).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether handle belongs to specified handle service.
%% @end
%%--------------------------------------------------------------------
-spec has_handle_service(HandleOrId :: od_space:id() | #od_space{},
    HServiceId :: od_handle_service:id()) -> boolean().
has_handle_service(HandleId, HServiceId) when is_binary(HandleId) ->
    entity_graph:has_relation(direct, top_down, od_handle_service, HServiceId, od_handle, HandleId);
has_handle_service(Handle, HServiceId) ->
    entity_graph:has_relation(direct, top_down, od_handle_service, HServiceId, Handle).
