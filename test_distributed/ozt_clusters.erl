%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating clusters of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_clusters).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([add_user/2, add_user/3, ensure_member/2]).
-export([get_user_privileges/2, get_group_privileges/2]).
-export([remove_user/2]).
-export([ensure_not_a_member/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec add_user(od_cluster:id(), od_user:id()) -> ok.
add_user(ClusterId, UserId) ->
    add_user(ClusterId, UserId, privileges:cluster_member()).


-spec add_user(od_cluster:id(), od_user:id(), [privileges:cluster_privilege()]) -> ok.
add_user(ClusterId, UserId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(cluster_logic, add_user, [?ROOT, ClusterId, UserId, Privileges])),
    ok.


-spec ensure_member(od_cluster:id(), od_user:id()) -> ok.
ensure_member(ClusterId, UserId) ->
    ?assertSuccessOrAlreadyExists(ozt:rpc(cluster_logic, add_user, [?ROOT, ClusterId, UserId])).


-spec get_user_privileges(od_cluster:id(), od_user:id()) -> [privileges:cluster_privilege()].
get_user_privileges(ClusterId, UserId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(cluster_logic, get_user_privileges, [?ROOT, ClusterId, UserId])),
    Privs.


-spec get_group_privileges(od_cluster:id(), od_group:id()) -> [privileges:cluster_privilege()].
get_group_privileges(ClusterId, GroupId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(cluster_logic, get_group_privileges, [?ROOT, ClusterId, GroupId])),
    Privs.


-spec remove_user(od_cluster:id(), od_user:id()) -> ok.
remove_user(ClusterId, UserId) ->
    ?assertMatch(ok, ozt:rpc(cluster_logic, remove_user, [?ROOT, ClusterId, UserId])).


-spec ensure_not_a_member(od_cluster:id(), od_user:id()) -> ok.
ensure_not_a_member(ClusterId, UserId) ->
    ?assertSuccessOrDoesNotExist(ozt:rpc(cluster_logic, remove_user, [?ROOT, ClusterId, UserId])).
