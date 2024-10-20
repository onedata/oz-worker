%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating handle services of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_handle_services).
-author("Lukasz Opiola").

-include("ozt.hrl").
-include("plugins/onezone_plugins.hrl").

%% API
-export([create/0, create/1, get/1, list/0]).
-export([add_user/2, add_user/3]).
-export([add_group/2, add_group/3]).
-export([set_user_privileges/3]).

%%%===================================================================
%%% API
%%%===================================================================


-spec create() -> od_handle_service:id().
create() ->
    {ok, HServiceId} = ?assertMatch({ok, _}, ozt:rpc(handle_service_logic, create, [
        ?ROOT, ?DOI_SERVICE
    ])),
    HServiceId.


-spec create(od_handle_service:name()) -> od_handle_service:id().
create(Name) ->
    Data = ?DOI_SERVICE,
    {ok, HServiceId} = ?assertMatch({ok, _}, ozt:rpc(handle_service_logic, create, [
        ?ROOT, Data#{<<"name">> => Name}
    ])),
    HServiceId.


-spec get(od_handle_service:id()) -> od_handle_service:record().
get(HServiceId) ->
    {ok, HServiceRecord} = ?assertMatch({ok, _}, ozt:rpc(handle_service_logic, get, [?ROOT, HServiceId])),
    HServiceRecord.


-spec list() -> [od_handle_service:id()].
list() ->
    {ok, HServiceIds} = ?assertMatch({ok, _}, ozt:rpc(handle_service_logic, list, [?ROOT])),
    HServiceIds.


-spec add_user(od_handle_service:id(), od_user:id()) -> ok.
add_user(HServiceId, UserId) ->
    add_user(HServiceId, UserId, privileges:handle_service_member()).

-spec add_user(od_handle_service:id(), od_user:id(), [privileges:handle_service_privilege()]) -> ok.
add_user(HServiceId, UserId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(handle_service_logic, add_user, [?ROOT, HServiceId, UserId, Privileges])),
    ok.


-spec add_group(od_handle_service:id(), od_group:id()) -> ok.
add_group(HServiceId, GroupId) ->
    add_group(HServiceId, GroupId, privileges:handle_service_member()).

-spec add_group(od_handle_service:id(), od_group:id(), [privileges:handle_service_privilege()]) -> ok.
add_group(HServiceId, GroupId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(handle_service_logic, add_group, [?ROOT, HServiceId, GroupId, Privileges])),
    ok.


-spec set_user_privileges(od_handle_service:id(), od_user:id(), [privileges:handle_service_privilege()]) -> ok.
set_user_privileges(HServiceId, UserId, Privileges) ->
    ?assertMatch(ok, ozt:rpc(handle_service_logic, update_user_privileges, [?ROOT, HServiceId, UserId, #{
        <<"grant">> => Privileges,
        <<"revoke">> => lists_utils:subtract(privileges:handle_service_admin(), Privileges)
    }])).
