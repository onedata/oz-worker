%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Implementation of od_error_ctx_provider_behaviour for oz-worker service.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_ctx_oz_provider).
-author("Bartosz Walkowicz").

-behaviour(od_error_ctx_provider_behaviour).

-include_lib("ctool/include/onedata.hrl").

%% od_error_ctx_provider_behaviour callbacks
-export([
    service/0,
    service_id/0,
    service_domain/0,
    service_release_version/0,
    service_build_version/0
]).


%%%===================================================================
%%% od_error_ctx_provider_behaviour callbacks
%%%===================================================================


-spec service() -> ?OZ_WORKER.
service() -> ?OZ_WORKER.


-spec service_id() -> onedata:service_id().
service_id() -> ?ONEZONE_CLUSTER_ID.


-spec service_domain() -> undefined | binary().
service_domain() -> try oz_worker:get_domain() catch _:_ -> undefined end.


-spec service_release_version() -> onedata:release_version().
service_release_version() -> oz_worker:get_release_version().


-spec service_build_version() -> binary().
service_build_version() -> oz_worker:get_build_version().
