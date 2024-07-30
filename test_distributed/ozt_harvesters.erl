%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating harvesters of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_harvesters).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([create/0, create/1]).
-export([add_user/2, add_user/3]).
-export([add_group/2, add_group/3]).
-export([add_space/2]).
-export([get_user_privileges/2, get_group_privileges/2]).
-export([delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create() -> od_harvester:id().
create() ->
    create(<<"harvester-", (?UNIQUE_STRING)/binary>>).


-spec create(od_harvester:name()) -> od_harvester:id().
create(Name) ->
    {ok, HarvesterId} = ?assertMatch({ok, _}, ozt:rpc(harvester_logic, create, [?ROOT, ?HARVESTER_CREATE_DATA(Name)])),
    HarvesterId.


-spec add_user(od_harvester:id(), od_user:id()) -> ok.
add_user(HarvesterId, UserId) ->
    add_user(HarvesterId, UserId, privileges:harvester_member()).

-spec add_user(od_harvester:id(), od_user:id(), [privileges:harvester_privilege()]) -> ok.
add_user(HarvesterId, UserId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(harvester_logic, add_user, [?ROOT, HarvesterId, UserId, Privileges])),
    ok.


-spec add_group(od_harvester:id(), od_group:id()) -> ok.
add_group(HarvesterId, GroupId) ->
    add_group(HarvesterId, GroupId, privileges:harvester_member()).

-spec add_group(od_harvester:id(), od_group:id(), [privileges:harvester_privilege()]) -> ok.
add_group(HarvesterId, GroupId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(harvester_logic, add_group, [?ROOT, HarvesterId, GroupId, Privileges])),
    ok.


add_space(HarvesterId, SpaceId) ->
    ?assertMatch({ok, _}, ozt:rpc(harvester_logic, add_space, [?ROOT, HarvesterId, SpaceId])),
    ok.


-spec get_user_privileges(od_harvester:id(), od_user:id()) -> [privileges:harvester_privilege()].
get_user_privileges(HarvesterId, UserId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(harvester_logic, get_user_privileges, [?ROOT, HarvesterId, UserId])),
    Privs.


-spec get_group_privileges(od_harvester:id(), od_group:id()) -> [privileges:harvester_privilege()].
get_group_privileges(HarvesterId, GroupId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(harvester_logic, get_group_privileges, [?ROOT, HarvesterId, GroupId])),
    Privs.


-spec delete(od_harvester:id()) -> ok.
delete(HarvesterId) ->
    ?assertMatch(ok, ozt:rpc(harvester_logic, delete, [?ROOT, HarvesterId])).
