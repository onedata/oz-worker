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
-export([create_space_for/1]).
-export([get_user_privileges/2, get_child_privileges/2]).
-export([grant_oz_privileges/2, revoke_oz_privileges/2]).
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


-spec create_space_for(od_group:id()) -> od_group:id().
create_space_for(GroupId) ->
    {ok, SpaceId} = ?assertMatch({ok, _}, ozt:rpc(group_logic, create_space, [
        ?ROOT, GroupId, #{<<"name">> => <<"of-group-", GroupId/binary>>}
    ])),
    SpaceId.


-spec get_user_privileges(od_group:id(), od_user:id()) -> [privileges:group_privilege()].
get_user_privileges(GroupId, UserId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(group_logic, get_user_privileges, [?ROOT, GroupId, UserId])),
    Privs.


-spec get_child_privileges(Parent :: od_group:id(), Child :: od_group:id()) -> [privileges:group_privilege()].
get_child_privileges(ParentId, ChildId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(group_logic, get_child_privileges, [?ROOT, ParentId, ChildId])),
    Privs.


-spec grant_oz_privileges(od_group:id(), [privileges:oz_privilege()]) -> ok.
grant_oz_privileges(GroupId, OzPrivileges) ->
    ?assertMatch(ok, ozt:rpc(group_logic, update_oz_privileges, [?ROOT, GroupId, OzPrivileges, []])).


-spec revoke_oz_privileges(od_group:id(), [privileges:oz_privilege()]) -> ok.
revoke_oz_privileges(GroupId, OzPrivileges) ->
    ?assertMatch(ok, ozt:rpc(group_logic, update_oz_privileges, [?ROOT, GroupId, [], OzPrivileges])).


-spec delete(od_group:id()) -> ok.
delete(GroupId) ->
    ?assertMatch(ok, ozt:rpc(group_logic, delete, [?ROOT, GroupId])).
