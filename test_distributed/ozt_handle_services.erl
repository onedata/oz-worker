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


create_handle(HServiceId, ShareId) ->
    {ok, HandleId} = ?assertMatch({ok, _}, ozt:rpc(handle_logic, create, [?ROOT, ?HANDLE(HServiceId, ShareId)])),
    HandleId.
