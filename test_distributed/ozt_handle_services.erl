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

%% API
-export([create/0, create/1]).
-export([add_user/2, add_user/3]).
-export([create_handle/2]).

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


-spec add_user(od_handle_service:id(), od_user:id()) -> ok.
add_user(HServiceId, UserId) ->
    add_user(HServiceId, UserId, privileges:handle_service_member()).

-spec add_user(od_handle_service:id(), od_user:id(), [privileges:handle_service_privilege()]) -> ok.
add_user(HServiceId, UserId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(handle_service_logic, add_user, [?ROOT, HServiceId, UserId, Privileges])),
    ok.


create_handle(HServiceId, ShareId) ->
    {ok, HandleId} = ?assertMatch({ok, _}, ozt:rpc(handle_logic, create, [?ROOT, ?HANDLE(HServiceId, ShareId)])),
    HandleId.
