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
-export([create/0, create/1, create_protected/0]).
-export([list/0, get/1]).
-export([create_space_for/1]).
-export([create_atm_inventory_for/1, create_atm_inventory_for/2]).
-export([add_user/2, add_user/3]).
-export([remove_user/2]).
-export([create_one_shot_group_invite_token/2]).
-export([add_child/2, add_child/3]).
-export([get_children/1]).
-export([remove_child/2]).
-export([set_user_privileges/3, get_user_privileges/2]).
-export([set_child_privileges/3, update_child_privileges/4, get_child_privileges/2]).
-export([get_atm_inventories/1]).
-export([grant_oz_privileges/2, revoke_oz_privileges/2]).
-export([join_space/3]).
-export([delete/1]).
-export([mark_protected/2, is_protected/1]).
-export([run_without_protection/2]).

-compile({no_auto_import, [get/1]}).

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


-spec create_protected() -> od_group:id().
create_protected() ->
    GroupId = create(),
    mark_protected(GroupId, true),
    GroupId.


-spec list() -> [od_group:id()].
list() ->
    {ok, GroupIds} = ?assertMatch({ok, _}, ozt:rpc(group_logic, list, [?ROOT])),
    GroupIds.


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


-spec create_one_shot_group_invite_token(od_group:id(), od_user:id()) -> tokens:token().
create_one_shot_group_invite_token(GroupId, CreatorUserId) ->
    ozt_tokens:create(named, ?SUB(user, CreatorUserId), #{
        <<"type">> => ?INVITE_TOKEN(?GROUP_JOIN_GROUP, GroupId),
        <<"usageLimit">> => 1
    }).


-spec add_child(od_group:id(), od_group:id()) -> ok.
add_child(ParentId, ChildId) ->
    add_child(ParentId, ChildId, privileges:group_member()).

-spec add_child(od_group:id(), od_group:id(), [privileges:group_privilege()]) -> ok.
add_child(ParentId, ChildId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(group_logic, add_group, [?ROOT, ParentId, ChildId, Privileges])),
    ok.


-spec get_children(od_group:id()) -> [od_group:id()].
get_children(GroupId) ->
    {ok, ChildIds} = ?assertMatch({ok, _}, ozt:rpc(group_logic, get_children, [?ROOT, GroupId])),
    ChildIds.


-spec remove_child(od_group:id(), od_group:id()) -> ok.
remove_child(ParentId, ChildId) ->
    ?assertMatch(ok, ozt:rpc(group_logic, remove_group, [?ROOT, ParentId, ChildId])).


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


-spec set_child_privileges(od_group:id(), od_group:id(), [privileges:group_privilege()]) -> ok.
set_child_privileges(ParentId, ChildId, Privileges) ->
    update_child_privileges(ParentId, ChildId, Privileges, lists_utils:subtract(privileges:group_admin(), Privileges)).


-spec update_child_privileges(
    od_group:id(),
    od_group:id(),
    [privileges:group_privilege()],
    [privileges:group_privilege()]
) -> ok.
update_child_privileges(ParentId, ChildId, ToGrant, ToRevoke) ->
    ?assertMatch(ok, ozt:rpc(group_logic, update_child_privileges, [?ROOT, ParentId, ChildId, #{
        <<"grant">> => ToGrant,
        <<"revoke">> => ToRevoke
    }])).


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


-spec mark_protected(od_group:id(), boolean()) -> ok.
mark_protected(GroupId, Flag) ->
    ?assertMatch({ok, _}, ozt:rpc(od_group, update, [GroupId, fun(Group) ->
        {ok, Group#od_group{protected = Flag}}
    end])),
    ok.


-spec is_protected(od_group:id()) -> boolean().
is_protected(GroupId) ->
    #od_group{protected = Protected} = get(GroupId),
    Protected.


-spec run_without_protection(od_group:id(), fun(() -> ok)) -> ok.
run_without_protection(GroupId, Procedure) ->
    WasProtected = ozt_groups:is_protected(GroupId),
    WasProtected andalso ozt_groups:mark_protected(GroupId, false),
    Procedure(),
    WasProtected andalso ozt_groups:mark_protected(GroupId, true),
    ok.
