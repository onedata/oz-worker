%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating spaces of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_spaces).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([create/0]).
-export([add_user/2, add_user/3]).
-export([create_support_token/2]).
-export([get_user_privileges/2, get_group_privileges/2]).
-export([delete/1]).
-export([minimum_support_size/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create() -> od_space:id().
create() ->
    {ok, SpaceId} = ?assertMatch({ok, _}, ozt:rpc(space_logic, create, [
        ?ROOT, #{<<"name">> => ?UNIQUE_STRING}
    ])),
    SpaceId.


-spec add_user(od_space:id(), od_user:id()) -> ok.
add_user(SpaceId, UserId) ->
    add_user(SpaceId, UserId, privileges:space_member()).

-spec add_user(od_space:id(), od_user:id(), [privileges:space_privilege()]) -> ok.
add_user(SpaceId, UserId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(space_logic, add_user, [?ROOT, SpaceId, UserId, Privileges])),
    ok.


-spec create_support_token(od_space:id(), od_user:id()) -> tokens:token().
create_support_token(SpaceId, UserId) ->
    ozt_tokens:create(temporary, ?SUB(user, UserId), ?INVITE_TOKEN(?SUPPORT_SPACE, SpaceId)).


-spec get_user_privileges(od_space:id(), od_user:id()) -> [privileges:space_privilege()].
get_user_privileges(SpaceId, UserId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_user_privileges, [?ROOT, SpaceId, UserId])),
    Privs.


-spec get_group_privileges(od_space:id(), od_group:id()) -> [privileges:space_privilege()].
get_group_privileges(SpaceId, GroupId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_group_privileges, [?ROOT, SpaceId, GroupId])),
    Privs.


-spec delete(od_space:id()) -> ok.
delete(SpaceId) ->
    ?assertMatch(ok, ozt:rpc(space_logic, delete, [?ROOT, SpaceId])).


-spec minimum_support_size() -> od_space:support_size().
minimum_support_size() ->
    ozt:get_env(minimum_space_support_size).