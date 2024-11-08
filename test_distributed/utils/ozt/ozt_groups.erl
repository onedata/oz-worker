%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating groups of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_groups).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([create/0, create/1]).
-export([get/1]).
-export([create_space_for/1]).
-export([create_atm_inventory_for/1, create_atm_inventory_for/2]).
-export([add_user/2, add_user/3]).
-export([remove_user/2]).
-export([add_child/2, add_child/3]).
-export([set_user_privileges/3, get_user_privileges/2, get_child_privileges/2]).
-export([get_atm_inventories/1]).
-export([grant_oz_privileges/2, revoke_oz_privileges/2]).
-export([join_space/3]).
-export([delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create() -> od_group:id().
create() ->
    create(#{<<"name">> => ?UNIQUE_STRING}).

-spec create(entity_logic:data()) -> od_group:id().
create(Data) ->
    {ok, GroupId} = ?assertMatch({ok, _}, ozt:rpc(group_logic, create, [?ROOT, Data])),
    GroupId.


-spec get(od_group:id()) -> od_group:record().
get(GroupId) ->
    {ok, Group} = ?assertMatch({ok, _}, ozt:rpc(group_logic, get, [?ROOT, GroupId])),
    Group.


-spec create_space_for(od_group:id()) -> od_group:id().
create_space_for(GroupId) ->
    {ok, SpaceId} = ?assertMatch({ok, _}, ozt:rpc(group_logic, create_space, [
        ?ROOT, GroupId, #{<<"name">> => <<"of-group-", GroupId/binary>>}
    ])),
    SpaceId.


-spec create_atm_inventory_for(od_group:id()) -> od_atm_inventory:id().
create_atm_inventory_for(GroupId) ->
    create_atm_inventory_for(GroupId, #{<<"name">> => <<"of-group-", GroupId/binary>>}).


-spec create_atm_inventory_for(od_group:id(), entity_logic:data()) -> od_atm_inventory:id().
create_atm_inventory_for(GroupId, Data) ->
    {ok, AtmInventoryId} = ?assertMatch({ok, _}, ozt:rpc(group_logic, create_atm_inventory, [
        ?ROOT, GroupId, Data
    ])),
    AtmInventoryId.


-spec add_user(od_group:id(), od_user:id()) -> ok.
add_user(GroupId, UserId) ->
    add_user(GroupId, UserId, privileges:group_member()).

-spec add_user(od_group:id(), od_user:id(), [privileges:group_privilege()]) -> ok.
add_user(GroupId, UserId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(group_logic, add_user, [?ROOT, GroupId, UserId, Privileges])),
    ok.


-spec remove_user(od_group:id(), od_user:id()) -> ok.
remove_user(GroupId, UserId) ->
    ?assertMatch(ok, ozt:rpc(group_logic, remove_user, [?ROOT, GroupId, UserId])).


-spec add_child(od_group:id(), od_group:id()) -> ok.
add_child(ParentId, ChildId) ->
    add_child(ParentId, ChildId, privileges:group_member()).

-spec add_child(od_group:id(), od_group:id(), [privileges:group_privilege()]) -> ok.
add_child(ParentId, ChildId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(group_logic, add_group, [?ROOT, ParentId, ChildId, Privileges])),
    ok.


-spec set_user_privileges(od_group:id(), od_user:id(), [privileges:group_privilege()]) -> ok.
set_user_privileges(GroupId, UserId, Privileges) ->
    ?assertMatch(ok, ozt:rpc(group_logic, update_user_privileges, [?ROOT, GroupId, UserId, #{
        <<"grant">> => Privileges,
        <<"revoke">> => lists_utils:subtract(privileges:group_admin(), Privileges)
    }])).


-spec get_user_privileges(od_group:id(), od_user:id()) -> [privileges:group_privilege()].
get_user_privileges(GroupId, UserId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(group_logic, get_user_privileges, [?ROOT, GroupId, UserId])),
    Privs.


-spec get_child_privileges(Parent :: od_group:id(), Child :: od_group:id()) -> [privileges:group_privilege()].
get_child_privileges(ParentId, ChildId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(group_logic, get_child_privileges, [?ROOT, ParentId, ChildId])),
    Privs.


-spec get_atm_inventories(od_group:id()) -> [od_atm_inventory:id()].
get_atm_inventories(GroupId) ->
    {ok, Inventories} = ?assertMatch({ok, _}, ozt:rpc(group_logic, get_atm_inventories, [?ROOT, GroupId])),
    Inventories.


-spec grant_oz_privileges(od_group:id(), [privileges:oz_privilege()]) -> ok.
grant_oz_privileges(GroupId, OzPrivileges) ->
    ?assertMatch(ok, ozt:rpc(group_logic, update_oz_privileges, [?ROOT, GroupId, OzPrivileges, []])).


-spec revoke_oz_privileges(od_group:id(), [privileges:oz_privilege()]) -> ok.
revoke_oz_privileges(GroupId, OzPrivileges) ->
    ?assertMatch(ok, ozt:rpc(group_logic, update_oz_privileges, [?ROOT, GroupId, [], OzPrivileges])).


-spec join_space(od_group:id(), od_user:id(), tokens:token()) -> od_space:id().
join_space(GroupId, ConsumerUserId, Token) ->
    {ok, SpaceId} = ?assertMatch({ok, _}, ozt:rpc(group_logic, join_space, [
        ?USER(ConsumerUserId), GroupId, #{<<"token">> => Token}
    ])),
    SpaceId.


-spec delete(od_group:id()) -> ok.
delete(GroupId) ->
    ?assertMatch(ok, ozt:rpc(group_logic, delete, [?ROOT, GroupId])).
