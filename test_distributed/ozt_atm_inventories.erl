%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating automation inventories of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_atm_inventories).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([list/0]).
-export([create/0, create/1]).
-export([get/1]).
-export([add_user/2, add_user/3]).
-export([add_group/2, add_group/3]).
-export([get_users/1, get_groups/1]).
-export([get_user_privileges/2, get_group_privileges/2]).
-export([set_user_privileges/3, set_group_privileges/3]).
-export([remove_user/2, remove_group/2]).
-export([create_user_invite_token/2, create_group_invite_token/2]).
-export([delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec list() -> [od_atm_inventory:id()].
list() ->
    {ok, AtmInventories} = ?assertMatch({ok, _}, ozt:rpc(atm_inventory_logic, list, [?ROOT])),
    AtmInventories.

-spec create() -> od_atm_inventory:id().
create() ->
    create(<<"atm_inventory-", (?UNIQUE_STRING)/binary>>).


-spec create(od_atm_inventory:name()) -> od_atm_inventory:id().
create(Name) ->
    {ok, AtmInventoryId} = ?assertMatch({ok, _}, ozt:rpc(atm_inventory_logic, create, [?ROOT, #{<<"name">> => Name}])),
    AtmInventoryId.


-spec get(od_atm_inventory:id()) -> od_atm_inventory:record().
get(AtmInventoryId) ->
    {ok, AtmInventory} = ?assertMatch({ok, _}, ozt:rpc(atm_inventory_logic, get, [?ROOT, AtmInventoryId])),
    AtmInventory.


-spec add_user(od_atm_inventory:id(), od_user:id()) -> ok.
add_user(AtmInventoryId, UserId) ->
    add_user(AtmInventoryId, UserId, privileges:atm_inventory_member()).

-spec add_user(od_atm_inventory:id(), od_user:id(), [privileges:atm_inventory_privilege()]) -> ok.
add_user(AtmInventoryId, UserId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(atm_inventory_logic, add_user, [?ROOT, AtmInventoryId, UserId, Privileges])),
    ok.


-spec add_group(od_atm_inventory:id(), od_group:id()) -> ok.
add_group(AtmInventoryId, GroupId) ->
    add_group(AtmInventoryId, GroupId, privileges:atm_inventory_member()).

-spec add_group(od_atm_inventory:id(), od_group:id(), [privileges:atm_inventory_privilege()]) -> ok.
add_group(AtmInventoryId, GroupId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(atm_inventory_logic, add_group, [?ROOT, AtmInventoryId, GroupId, Privileges])),
    ok.


-spec get_users(od_atm_inventory:id()) -> [od_user:id()].
get_users(AtmInventoryId) ->
    {ok, Users} = ?assertMatch({ok, _}, ozt:rpc(atm_inventory_logic, get_users, [?ROOT, AtmInventoryId])),
    Users.


-spec get_groups(od_atm_inventory:id()) -> [od_group:id()].
get_groups(AtmInventoryId) ->
    {ok, Groups} = ?assertMatch({ok, _}, ozt:rpc(atm_inventory_logic, get_groups, [?ROOT, AtmInventoryId])),
    Groups.


-spec get_user_privileges(od_atm_inventory:id(), od_user:id()) -> [privileges:atm_inventory_privilege()].
get_user_privileges(AtmInventoryId, UserId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(atm_inventory_logic, get_user_privileges, [?ROOT, AtmInventoryId, UserId])),
    Privs.


-spec get_group_privileges(od_atm_inventory:id(), od_group:id()) -> [privileges:atm_inventory_privilege()].
get_group_privileges(AtmInventoryId, GroupId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(atm_inventory_logic, get_group_privileges, [?ROOT, AtmInventoryId, GroupId])),
    Privs.


-spec set_user_privileges(od_atm_inventory:id(), od_user:id(), [privileges:atm_inventory_privilege()]) -> ok.
set_user_privileges(AtmInventoryId, UserId, Privileges) ->
    ?assertMatch(ok, ozt:rpc(atm_inventory_logic, update_user_privileges, [?ROOT, AtmInventoryId, UserId, #{
        <<"grant">> => Privileges,
        <<"revoke">> => lists_utils:subtract(privileges:atm_inventory_admin(), Privileges)
    }])).


-spec set_group_privileges(od_atm_inventory:id(), od_group:id(), [privileges:atm_inventory_privilege()]) -> ok.
set_group_privileges(AtmInventoryId, GroupId, Privileges) ->
    ?assertMatch(ok, ozt:rpc(atm_inventory_logic, update_group_privileges, [?ROOT, AtmInventoryId, GroupId, #{
        <<"grant">> => Privileges,
        <<"revoke">> => lists_utils:subtract(privileges:atm_inventory_admin(), Privileges)
    }])).


-spec remove_user(od_atm_inventory:id(), od_user:id()) -> ok.
remove_user(AtmInventoryId, UserId) ->
    ?assertMatch(ok, ozt:rpc(atm_inventory_logic, remove_user, [?ROOT, AtmInventoryId, UserId])).


-spec remove_group(od_atm_inventory:id(), od_group:id()) -> ok.
remove_group(AtmInventoryId, GroupId) ->
    ?assertMatch(ok, ozt:rpc(atm_inventory_logic, remove_group, [?ROOT, AtmInventoryId, GroupId])).


-spec create_user_invite_token(od_atm_inventory:id(), od_user:id()) -> tokens:token().
create_user_invite_token(AtmInventoryId, UserId) ->
    ozt_tokens:create(temporary, ?SUB(user, UserId), ?INVITE_TOKEN(?USER_JOIN_ATM_INVENTORY, AtmInventoryId)).


-spec create_group_invite_token(od_atm_inventory:id(), od_user:id()) -> tokens:token().
create_group_invite_token(AtmInventoryId, UserId) ->
    ozt_tokens:create(temporary, ?SUB(user, UserId), ?INVITE_TOKEN(?GROUP_JOIN_ATM_INVENTORY, AtmInventoryId)).


-spec delete(od_atm_inventory:id()) -> ok.
delete(AtmInventoryId) ->
    ?assertMatch(ok, ozt:rpc(atm_inventory_logic, delete, [?ROOT, AtmInventoryId])).
