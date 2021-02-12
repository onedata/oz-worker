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
    upgrade_from_21_02_1_space_support_info/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    upgrade_from_21_02_1_space_support_info
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
    ozt:delete_all_entities().

%%%===================================================================
%%% Tests
%%%===================================================================

upgrade_from_21_02_1_space_support_info(_Config) ->
    SpaceCreationTime = ozt_mocks:get_frozen_time_seconds(),

    SpaceAlpha = ozt_spaces:create(),
    SpaceGamma = ozt_spaces:create(),
    SpaceDelta = ozt_spaces:create(),
    SpaceTheta = ozt_spaces:create(),
    SpaceOmega = ozt_spaces:create(),

    {P1, St1} = simulate_existing_provider_with_spaces_in_old_model([SpaceAlpha, SpaceGamma, SpaceTheta]),
    {P2, St2} = simulate_existing_provider_with_spaces_in_old_model([SpaceAlpha, SpaceGamma, SpaceDelta]),
    {P3, St3} = simulate_existing_provider_with_spaces_in_old_model([SpaceGamma, SpaceDelta, SpaceOmega]),

    ?assertEqual({ok, 3}, ozt:rpc(node_manager_plugin, upgrade_cluster, [2])),
    ozt:reconcile_entity_graph(),

    ?assert(ozt_spaces:has_default_support_parameters(SpaceAlpha, [P1, P2])),
    ?assert(ozt_spaces:has_default_support_parameters(SpaceGamma, [P1, P2, P3])),
    ?assert(ozt_spaces:has_default_support_parameters(SpaceDelta, [P2, P3])),
    ?assert(ozt_spaces:has_default_support_parameters(SpaceTheta, [P1])),
    ?assert(ozt_spaces:has_default_support_parameters(SpaceOmega, [P3])),

    ?assert(ozt_spaces:has_legacy_support_stages(SpaceAlpha, #{P1 => [St1], P2 => [St2]})),
    ?assert(ozt_spaces:has_legacy_support_stages(SpaceGamma, #{P1 => [St1], P2 => [St2], P3 => [St3]})),
    ?assert(ozt_spaces:has_legacy_support_stages(SpaceDelta, #{P2 => [St2], P3 => [St3]})),
    ?assert(ozt_spaces:has_legacy_support_stages(SpaceTheta, #{P1 => [St1]})),
    ?assert(ozt_spaces:has_legacy_support_stages(SpaceOmega, #{P3 => [St3]})),

    ?assert(ozt_spaces:has_initial_capacity_usage(SpaceAlpha, ?SUPPORT_SIZE, #{P1 => [St1], P2 => [St2]})),
    ?assert(ozt_spaces:has_initial_capacity_usage(SpaceGamma, ?SUPPORT_SIZE, #{P1 => [St1], P2 => [St2], P3 => [St3]})),
    ?assert(ozt_spaces:has_initial_capacity_usage(SpaceDelta, ?SUPPORT_SIZE, #{P2 => [St2], P3 => [St3]})),
    ?assert(ozt_spaces:has_initial_capacity_usage(SpaceTheta, ?SUPPORT_SIZE, #{P1 => [St1]})),
    ?assert(ozt_spaces:has_initial_capacity_usage(SpaceOmega, ?SUPPORT_SIZE, #{P3 => [St3]})),

    ?assert(ozt_spaces:has_initial_sync_progress(SpaceAlpha, SpaceCreationTime, [P1, P2])),
    ?assert(ozt_spaces:has_initial_sync_progress(SpaceGamma, SpaceCreationTime, [P1, P2, P3])),
    ?assert(ozt_spaces:has_initial_sync_progress(SpaceDelta, SpaceCreationTime, [P2, P3])),
    ?assert(ozt_spaces:has_initial_sync_progress(SpaceTheta, SpaceCreationTime, [P1])),
    ?assert(ozt_spaces:has_initial_sync_progress(SpaceOmega, SpaceCreationTime, [P3])).

%%%===================================================================
%%% Helper functions
%%%===================================================================

% creates space supports using the procedures from 21.02 line
simulate_existing_provider_with_spaces_in_old_model(SpaceIds) ->
    ProviderId = ozt_providers:create(),
    ozt_providers:simulate_version(ProviderId, ?LINE_21_02(<<"1">>)),
    StorageId = ozt_providers:create_storage(ProviderId),
    lists:foreach(fun(SpaceId) ->
        ozt:rpc(entity_graph, add_relation, [
            od_space, SpaceId,
            od_storage, StorageId,
            ?SUPPORT_SIZE
        ])
    end, SpaceIds),
    {ProviderId, StorageId}.
