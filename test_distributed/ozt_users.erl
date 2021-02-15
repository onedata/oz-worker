%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating users of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_users).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([create/0, create/1]).
-export([create_admin/0, create_admin/1]).
-export([get/1]).
-export([toggle_access_block/2]).
-export([create_group_for/1]).
-export([create_space_for/1]).
-export([join_space/2, leave_space/2]).
-export([create_handle_service_for/1]).
-export([create_harvester_for/1]).
-export([get_eff_providers/1]).
-export([grant_oz_privileges/2, revoke_oz_privileges/2]).
-export([delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create() -> od_user:id().
create() ->
    create(#{}).

-spec create(entity_logic:data()) -> od_user:id().
create(Data) ->
    {ok, UserId} = ?assertMatch({ok, _}, ozt:rpc(user_logic, create, [?ROOT, Data])),
    UserId.


-spec create_admin() -> od_user:id().
create_admin() ->
    create_admin(privileges:oz_admin()).

-spec create_admin([privileges:oz_privilege()]) -> od_user:id().
create_admin(OzPrivileges) ->
    UserId = create(),
    grant_oz_privileges(UserId, OzPrivileges),
    UserId.


-spec get(od_user:id()) -> od_user:record().
get(UserId) ->
    {ok, User} = ?assertMatch({ok, _}, ozt:rpc(user_logic, get, [?ROOT, UserId])),
    User.


-spec toggle_access_block(od_user:id(), boolean()) -> ok.
toggle_access_block(UserId, Blocked) ->
    ?assertMatch(ok, ozt:rpc(user_logic, toggle_access_block, [?ROOT, UserId, Blocked])).


-spec create_group_for(od_user:id()) -> od_group:id().
create_group_for(UserId) ->
    {ok, GroupId} = ?assertMatch({ok, _}, ozt:rpc(user_logic, create_group, [
        ?USER(UserId), UserId, #{<<"name">> => <<"of-user-", UserId/binary>>}
    ])),
    GroupId.


-spec create_space_for(od_user:id()) -> od_space:id().
create_space_for(UserId) ->
    {ok, SpaceId} = ?assertMatch({ok, _}, ozt:rpc(user_logic, create_space, [
        ?USER(UserId), UserId, #{<<"name">> => <<"of-user-", UserId/binary>>}
    ])),
    SpaceId.


-spec join_space(od_user:id(), tokens:token()) -> od_space:id().
join_space(UserId, Token) ->
    {ok, SpaceId} = ?assertMatch({ok, _}, ozt:rpc(user_logic, join_space, [
        ?USER(UserId), UserId, #{<<"token">> => Token}
    ])),
    SpaceId.


-spec leave_space(od_user:id(), od_space:id()) -> ok.
leave_space(UserId, SpaceId) ->
    ?assertMatch(ok, ozt:rpc(user_logic, leave_space, [?USER(UserId), UserId, SpaceId])).


-spec create_handle_service_for(od_user:id()) -> od_handle_service:id().
create_handle_service_for(UserId) ->
    grant_oz_privileges(UserId, [?OZ_HANDLE_SERVICES_CREATE]),
    {ok, HServiceId} = ?assertMatch({ok, _}, ozt:rpc(user_logic, create_handle_service, [
        ?USER(UserId), UserId, ?DOI_SERVICE
    ])),
    HServiceId.


-spec create_harvester_for(od_user:id()) -> od_harvester:id().
create_harvester_for(UserId) ->
    grant_oz_privileges(UserId, [?OZ_HARVESTERS_CREATE]),
    {ok, HarvesterId} = ?assertMatch({ok, _}, ozt:rpc(user_logic, create_harvester, [
        ?USER(UserId), UserId, ?HARVESTER_CREATE_DATA
    ])),
    HarvesterId.


-spec get_eff_providers(od_user:id()) -> [od_provider:id()].
get_eff_providers(UserId) ->
    {ok, Providers} = ?assertMatch({ok, _}, ozt:rpc(user_logic, get_eff_providers, [?ROOT, UserId])),
    Providers.


-spec grant_oz_privileges(od_user:id(), [privileges:oz_privilege()]) -> ok.
grant_oz_privileges(UserId, OzPrivileges) ->
    ?assertMatch(ok, ozt:rpc(user_logic, update_oz_privileges, [?ROOT, UserId, OzPrivileges, []])).


-spec revoke_oz_privileges(od_user:id(), [privileges:oz_privilege()]) -> ok.
revoke_oz_privileges(UserId, OzPrivileges) ->
    ?assertMatch(ok, ozt:rpc(user_logic, update_oz_privileges, [?ROOT, UserId, [], OzPrivileges])).


-spec delete(od_user:id()) -> ok.
delete(UserId) ->
    ?assertMatch(ok, ozt:rpc(user_logic, delete, [?ROOT, UserId])).