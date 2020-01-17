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
-export([get_user_privileges/2, get_group_privileges/2]).
-export([delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

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
