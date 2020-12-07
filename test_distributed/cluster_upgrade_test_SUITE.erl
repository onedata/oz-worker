%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This test verifies if cluster upgrade procedures (employed during software
%%% upgrades) work as expected.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_upgrade_test_SUITE).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include("api_test_utils.hrl").
-include_lib("ctool/include/space_support/support_stage.hrl").
-include_lib("ctool/include/space_support/provider_sync_progress.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-define(SUPPORT_SIZE, 9876543210).

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    upgrade_from_20_02_1_space_support_info/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    upgrade_from_20_02_1_space_support_info
]).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    ozt:init_per_suite(Config).

end_per_suite(_Config) ->
    ssl:stop().

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time(),
    ozt:delete_all_entities(),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

upgrade_from_20_02_1_space_support_info(_Config) ->
    SpaceAlpha = ozt_spaces:create(),
    SpaceBeta = ozt_spaces:create(),
    SpaceGamma = ozt_spaces:create(),
    SpaceDelta = ozt_spaces:create(),
    SpaceOmega = ozt_spaces:create(),

    P1 = simulate_existing_provider_with_spaces_in_old_model([SpaceAlpha, SpaceBeta, SpaceGamma]),
    P2 = simulate_existing_provider_with_spaces_in_old_model([SpaceAlpha, SpaceGamma, SpaceDelta]),
    P3 = simulate_existing_provider_with_spaces_in_old_model([SpaceGamma, SpaceDelta, SpaceOmega]),

    ?assertEqual({ok, 3}, ozt:rpc(node_manager_plugin, upgrade_cluster, [2])),
    ozt:reconcile_entity_graph(),

    DefaultParameters = support_parameters:build(global, eager),

    AlphaRecord = ozt_spaces:get(SpaceAlpha),
    BetaRecord = ozt_spaces:get(SpaceBeta),
    GammaRecord = ozt_spaces:get(SpaceGamma),
    DeltaRecord = ozt_spaces:get(SpaceDelta),
    OmegaRecord = ozt_spaces:get(SpaceOmega),

    ?assertEqual(AlphaRecord#od_space.support_parameters_registry, #{
        P1 => DefaultParameters, P2 => DefaultParameters
    }),
    ?assertEqual(BetaRecord#od_space.support_parameters_registry, #{
        P1 => DefaultParameters
    }),
    ?assertEqual(GammaRecord#od_space.support_parameters_registry, #{
        P1 => DefaultParameters, P2 => DefaultParameters, P3 => DefaultParameters
    }),
    ?assertEqual(DeltaRecord#od_space.support_parameters_registry, #{
        P2 => DefaultParameters, P3 => DefaultParameters
    }),
    ?assertEqual(OmegaRecord#od_space.support_parameters_registry, #{
        P3 => DefaultParameters
    }),

    ?assertEqual(AlphaRecord#od_space.support_stage_registry, #{
        P1 => ?LEGACY_SUPPORT, P2 => ?LEGACY_SUPPORT
    }),
    ?assertEqual(BetaRecord#od_space.support_stage_registry, #{
        P1 => ?LEGACY_SUPPORT
    }),
    ?assertEqual(GammaRecord#od_space.support_stage_registry, #{
        P1 => ?LEGACY_SUPPORT, P2 => ?LEGACY_SUPPORT, P3 => ?LEGACY_SUPPORT
    }),
    ?assertEqual(DeltaRecord#od_space.support_stage_registry, #{
        P2 => ?LEGACY_SUPPORT, P3 => ?LEGACY_SUPPORT
    }),
    ?assertEqual(OmegaRecord#od_space.support_stage_registry, #{
        P3 => ?LEGACY_SUPPORT
    }),

    Now = ozt_mocks:get_frozen_time_seconds(),

    ?assertEqual(ozt_spaces:extract_sync_progress_registry_matrix(SpaceAlpha), #{
        P1 => #{P1 => {1, Now}, P2 => {1, Now}},
        P2 => #{P1 => {1, Now}, P2 => {1, Now}}
    }),
    ?assertEqual(ozt_spaces:extract_sync_progress_registry_matrix(SpaceBeta), #{
        P1 => #{P1 => {1, Now}}
    }),
    ?assertEqual(ozt_spaces:extract_sync_progress_registry_matrix(SpaceGamma), #{
        P1 => #{P1 => {1, Now}, P2 => {1, Now}, P3 => {1, Now}},
        P2 => #{P1 => {1, Now}, P2 => {1, Now}, P3 => {1, Now}},
        P3 => #{P1 => {1, Now}, P2 => {1, Now}, P3 => {1, Now}}
    }),
    ?assertEqual(ozt_spaces:extract_sync_progress_registry_matrix(SpaceDelta), #{
        P2 => #{P2 => {1, Now}, P3 => {1, Now}},
        P3 => #{P2 => {1, Now}, P3 => {1, Now}}
    }),
    ?assertEqual(ozt_spaces:extract_sync_progress_registry_matrix(SpaceOmega), #{
        P3 => #{P3 => {1, Now}}
    }).

%%%===================================================================
%%% Helper functions
%%%===================================================================

% creates space supports using the procedures from 19.02 and 20.02 line
simulate_existing_provider_with_spaces_in_old_model(SpaceIds) ->
    ProviderId = ozt_providers:create(),
    ozt_providers:simulate_version(ProviderId, ?LINE_20_02(<<"1">>)),
    ModernStorageId = ozt_providers:create_storage(ProviderId),
    % providers may still have legacy storages if they did not upgrade from 19.02
    LegacyStorageId = ProviderId,
    ozt_providers:ensure_storage(ProviderId, LegacyStorageId),
    lists:foreach(fun(SpaceId) ->
        StorageId = lists_utils:random_element([ModernStorageId, LegacyStorageId]),
        ozt:rpc(entity_graph, add_relation, [
            od_space, SpaceId,
            od_storage, StorageId,
            ?SUPPORT_SIZE
        ])
    end, SpaceIds),
    ProviderId.
