%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all Automation Inventory logic functionality.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_inventory_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create/2
]).
-export([
    get/2,
    get_protected_data/2,
    get_name/2,
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
    create_group/3, create_group/4,

    get_users/2, get_eff_users/2,
    get_user/3, get_eff_user/3,
    get_user_privileges/3, get_eff_user_privileges/3,
    get_eff_user_membership_intermediaries/3,

    get_groups/2, get_eff_groups/2,
    get_group/3, get_eff_group/3,
    get_group_privileges/3, get_eff_group_privileges/3,
    get_eff_group_membership_intermediaries/3,

    update_user_privileges/5, update_user_privileges/4,
    update_group_privileges/5, update_group_privileges/4,

    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    has_eff_privilege/3,
    has_eff_user/2,
    has_direct_user/2,
    has_eff_group/2
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(aai:auth(), od_atm_inventory:name() | entity_logic:data()) ->
    {ok, od_atm_inventory:id()} | errors:error().
create(Auth, Name) when is_binary(Name) ->
    create(Auth, #{<<"name">> => Name});
create(Auth, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = undefined, aspect = instance},
        data = Data
    })).


-spec get(aai:auth(), od_atm_inventory:id()) ->
    {ok, od_atm_inventory:record()} | errors:error().
get(Auth, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = instance}
    }).


-spec get_protected_data(aai:auth(), od_atm_inventory:id()) ->
    {ok, entity_logic:data()} | errors:error().
get_protected_data(Auth, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = instance, scope = protected}
    }).


-spec get_name(aai:auth(), od_atm_inventory:id()) ->
    {ok, od_atm_inventory:name()} | {error, term()}.
get_name(Auth, GroupId) ->
    case get(Auth, GroupId) of
        {ok, #od_atm_inventory{name = Name}} -> {ok, Name};
        {error, _} = Error -> Error
    end.


-spec list(aai:auth()) ->
    {ok, [od_atm_inventory:id()]} | errors:error().
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = undefined, aspect = list}
    }).


-spec list_privileges() -> {ok, entity_logic:data()} | errors:error().
list_privileges() ->
    entity_logic:handle(#el_req{
        operation = get,
        gri = #gri{type = od_atm_inventory, id = undefined, aspect = privileges}
    }).


-spec update(aai:auth(), od_atm_inventory:id(), entity_logic:data()) ->
    ok | errors:error().
update(Auth, AtmInventoryId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = instance},
        data = Data
    }).


-spec delete(aai:auth(), od_atm_inventory:id()) ->
    ok | errors:error().
delete(Auth, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = instance}
    }).


-spec add_user(aai:auth(), od_atm_inventory:id(), od_user:id()) ->
    {ok, od_user:id()} | errors:error().
add_user(Auth, AtmInventoryId, UserId) ->
    add_user(Auth, AtmInventoryId, UserId, #{}).


-spec add_user(aai:auth(), od_atm_inventory:id(), od_user:id(), [privileges:atm_inventory_privilege()] | entity_logic:data()) ->
    {ok, od_user:id()} | errors:error().
add_user(Auth, AtmInventoryId, UserId, Privileges) when is_list(Privileges) ->
    add_user(Auth, AtmInventoryId, UserId, #{
        <<"privileges">> => Privileges
    });
add_user(Auth, AtmInventoryId, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {user, UserId}},
        data = Data
    })).


-spec add_group(aai:auth(), od_atm_inventory:id(), od_group:id()) ->
    {ok, od_group:id()} | errors:error().
add_group(Auth, AtmInventoryId, GroupId) ->
    add_group(Auth, AtmInventoryId, GroupId, #{}).


-spec add_group(aai:auth(), od_atm_inventory:id(), od_group:id(), [privileges:atm_inventory_privilege()] | entity_logic:data()) ->
    {ok, od_group:id()} | errors:error().
add_group(Auth, AtmInventoryId, GroupId, Privileges) when is_list(Privileges) ->
    add_group(Auth, AtmInventoryId, GroupId, #{
        <<"privileges">> => Privileges
    });
add_group(Auth, AtmInventoryId, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {group, GroupId}},
        data = Data
    })).


-spec create_group(aai:auth(), od_atm_inventory:id(), od_group:name(),
    od_group:type()) -> {ok, od_group:id()} | errors:error().
create_group(Auth, AtmInventoryId, Name, Type) ->
    create_group(Auth, AtmInventoryId, #{<<"name">> => Name, <<"type">> => Type}).


-spec create_group(aai:auth(), od_atm_inventory:id(), od_group:name() | entity_logic:data()) ->
    {ok, od_group:id()} | errors:error().
create_group(Auth, AtmInventoryId, Name) when is_binary(Name) ->
    create_group(Auth, AtmInventoryId, #{<<"name">> => Name});
create_group(Auth, AtmInventoryId, Data) ->
    AuthHint = case Auth of
        ?USER(UserId) -> ?AS_USER(UserId);
        _ -> undefined
    end,
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = group},
        data = Data,
        auth_hint = AuthHint
    })).


-spec get_users(aai:auth(), od_atm_inventory:id()) ->
    {ok, [od_user:id()]} | errors:error().
get_users(Auth, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = users}
    }).


-spec get_eff_users(aai:auth(), od_atm_inventory:id()) ->
    {ok, [od_user:id()]} | errors:error().
get_eff_users(Auth, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = eff_users}
    }).


-spec get_user(aai:auth(), od_atm_inventory:id(),
    od_user:id()) -> {ok, entity_logic:data()} | errors:error().
get_user(Auth, AtmInventoryId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_ATM_INVENTORY(AtmInventoryId)
    }).


-spec get_eff_user(aai:auth(), od_atm_inventory:id(), od_user:id()) ->
    {ok, entity_logic:data()} | errors:error().
get_eff_user(Auth, AtmInventoryId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_ATM_INVENTORY(AtmInventoryId)
    }).


-spec get_user_privileges(aai:auth(), od_atm_inventory:id(), od_user:id()) ->
    {ok, [privileges:atm_inventory_privilege()]} | errors:error().
get_user_privileges(Auth, AtmInventoryId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {user_privileges, UserId}}
    }).


-spec get_eff_user_privileges(aai:auth(), od_atm_inventory:id(), od_user:id()) ->
    {ok, [privileges:atm_inventory_privilege()]} | errors:error().
get_eff_user_privileges(Auth, AtmInventoryId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {eff_user_privileges, UserId}}
    }).


-spec get_eff_user_membership_intermediaries(aai:auth(), od_atm_inventory:id(), od_user:id()) ->
    {ok, entity_graph:intermediaries()} | errors:error().
get_eff_user_membership_intermediaries(Auth, AtmInventoryId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {eff_user_membership, UserId}}
    }).


-spec get_groups(aai:auth(), od_atm_inventory:id()) ->
    {ok, [od_group:id()]} | errors:error().
get_groups(Auth, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = groups}
    }).


-spec get_eff_groups(aai:auth(), od_atm_inventory:id()) ->
    {ok, [od_group:id()]} | errors:error().
get_eff_groups(Auth, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = eff_groups}
    }).


-spec get_group(aai:auth(), od_atm_inventory:id(), od_group:id()) ->
    {ok, entity_logic:data()} | errors:error().
get_group(Auth, AtmInventoryId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_ATM_INVENTORY(AtmInventoryId)
    }).


-spec get_eff_group(aai:auth(), od_atm_inventory:id(), od_group:id()) ->
    {ok, entity_logic:data()} | errors:error().
get_eff_group(Auth, AtmInventoryId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_ATM_INVENTORY(AtmInventoryId)
    }).


-spec get_group_privileges(aai:auth(), od_atm_inventory:id(), od_group:id()) ->
    {ok, [privileges:atm_inventory_privilege()]} | errors:error().
get_group_privileges(Auth, AtmInventoryId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {group_privileges, GroupId}}
    }).


-spec get_eff_group_privileges(aai:auth(), od_atm_inventory:id(), od_group:id()) ->
    {ok, [privileges:atm_inventory_privilege()]} | errors:error().
get_eff_group_privileges(Auth, AtmInventoryId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {eff_group_privileges, GroupId}}
    }).


-spec get_eff_group_membership_intermediaries(aai:auth(), od_atm_inventory:id(), od_group:id()) ->
    {ok, entity_graph:intermediaries()} | errors:error().
get_eff_group_membership_intermediaries(Auth, AtmInventoryId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {eff_group_membership, GroupId}}
    }).


-spec update_user_privileges(
    aai:auth(), od_atm_inventory:id(), od_user:id(),
    [privileges:atm_inventory_privilege()], [privileges:atm_inventory_privilege()]
) ->
    ok | errors:error().
update_user_privileges(Auth, AtmInventoryId, UserId, PrivsToGrant, PrivsToRevoke) ->
    update_user_privileges(Auth, AtmInventoryId, UserId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


-spec update_user_privileges(aai:auth(), od_atm_inventory:id(), od_user:id(), entity_logic:data()) ->
    ok | errors:error().
update_user_privileges(Auth, AtmInventoryId, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {user_privileges, UserId}},
        data = Data
    }).


-spec update_group_privileges(
    aai:auth(), od_atm_inventory:id(), od_group:id(),
    [privileges:atm_inventory_privilege()], [privileges:atm_inventory_privilege()]
) ->
    ok | errors:error().
update_group_privileges(Auth, AtmInventoryId, GroupId, PrivsToGrant, PrivsToRevoke) ->
    update_group_privileges(Auth, AtmInventoryId, GroupId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


-spec update_group_privileges(aai:auth(), od_atm_inventory:id(), od_group:id(), entity_logic:data()) ->
    ok | errors:error().
update_group_privileges(Auth, AtmInventoryId, GroupId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {group_privileges, GroupId}},
        data = Data
    }).


-spec remove_user(aai:auth(), od_atm_inventory:id(), od_user:id()) ->
    ok | errors:error().
remove_user(Auth, AtmInventoryId, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {user, UserId}}
    }).


-spec remove_group(aai:auth(), od_atm_inventory:id(), od_group:id()) ->
    ok | errors:error().
remove_group(Auth, AtmInventoryId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = {group, GroupId}}
    }).


-spec exists(od_atm_inventory:id()) -> boolean().
exists(AtmInventoryId) ->
    {ok, Exists} = od_atm_inventory:exists(AtmInventoryId),
    Exists.


-spec has_eff_privilege(od_atm_inventory:id() | od_atm_inventory:record(), od_user:id(), privileges:atm_inventory_privilege()) ->
    boolean().
has_eff_privilege(AtmInventoryId, UserId, Privilege) when is_binary(AtmInventoryId) ->
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, od_atm_inventory, AtmInventoryId);
has_eff_privilege(AtmInventory, UserId, Privilege) ->
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, AtmInventory).


-spec has_eff_user(od_atm_inventory:id() | od_atm_inventory:record(), od_user:id()) ->
    boolean().
has_eff_user(AtmInventoryId, UserId) when is_binary(AtmInventoryId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, od_atm_inventory, AtmInventoryId);
has_eff_user(AtmInventory, UserId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, AtmInventory).


-spec has_direct_user(od_atm_inventory:id() | od_atm_inventory:record(), od_user:id()) ->
    boolean().
has_direct_user(AtmInventoryId, UserId) when is_binary(AtmInventoryId) ->
    case od_atm_inventory:get(AtmInventoryId) of
        {ok, #document{value = AtmInventory}} ->
            has_direct_user(AtmInventory, UserId);
        _ ->
            false
    end;
has_direct_user(#od_atm_inventory{users = Users}, UserId) ->
    maps:is_key(UserId, Users).


-spec has_eff_group(od_atm_inventory:id() | od_atm_inventory:record(), od_group:id()) ->
    boolean().
has_eff_group(AtmInventoryId, GroupId) when is_binary(AtmInventoryId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, od_atm_inventory, AtmInventoryId);
has_eff_group(AtmInventory, GroupId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, AtmInventory).
