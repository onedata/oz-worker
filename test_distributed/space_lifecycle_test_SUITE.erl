%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Empty test SUITE to be implemented in the future.
%%% @end
%%%-------------------------------------------------------------------
-module(space_lifecycle_test_SUITE).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include("api_test_utils.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/space_support/support_stage.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    upgrade_from_20_02_with_providers_of_different_versions/1
    %% @TODO VFS-6981 comprehensive tests related to the logic in space_support
    %% and space_stats modules with different combinations of different provider versions
]).

-define(SUPPORT_SIZE, 1029384756).

-define(waitUntilEqual(Expected, Value), ?assertEqual(Expected, Value, 60)).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    upgrade_from_20_02_with_providers_of_different_versions
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

%% @TODO VFS-6981 adjust name to other tests
upgrade_from_20_02_with_providers_of_different_versions(_Config) ->
    SpaceCreationTime = ozt_mocks:get_frozen_time_seconds(),
    SpaceAlpha = ozt_spaces:create(),
    SpaceBeta = ozt_spaces:create(),
    {P1, P1Storage} = simulate_preexisting_provider_with_spaces(?LINE_20_02(<<"1">>), [SpaceAlpha, SpaceBeta]),
    {P2, _P2Storage} = simulate_preexisting_provider_with_spaces(?LINE_20_02(<<"2">>), []),
    {P3, P3Storage} = simulate_preexisting_provider_with_spaces(?LINE_20_02(<<"3">>), [SpaceAlpha]),

    ?assert(ozt_spaces:has_legacy_support_stages(SpaceAlpha, #{
        P1 => [P1Storage], P3 => [P3Storage]
    })),
    ?assert(ozt_spaces:has_legacy_support_stages(SpaceBeta, #{
        P1 => [P1Storage]
    })),

    % at this point, all providers are in version 19.02.* and cannot connect to
    % Onezone (must be upgraded first)

    % P2 is upgraded to 21.02 and supports SpaceBeta using the legacy support procedure
    NewP2Storage = ozt_providers:create_storage(P2),
    ozt_providers:simulate_version(P2, ?LINE_21_02(<<"rc2">>)),
    ozt_providers:support_space(P2, NewP2Storage, SpaceBeta, ?SUPPORT_SIZE),
    ?assert(ozt_spaces:has_legacy_support_stages(SpaceBeta, #{
        P1 => [P1Storage], P2 => [NewP2Storage]
    })),
    ?assert(ozt_spaces:has_initial_capacity_usage(SpaceBeta, ?SUPPORT_SIZE, #{
        P1 => [P1Storage], P2 => [NewP2Storage]
    })),
    ?assert(ozt_spaces:has_initial_sync_progress(SpaceBeta, SpaceCreationTime, [P1, P2])),

    % P2 is upgraded to the newest version
    upgrade_support_to_22_02(P2, NewP2Storage, SpaceBeta),
    ozt_providers:simulate_version(P2, ?LINE_22_02(<<"1">>)),
    ?waitUntilEqual(ozt_spaces:get_support_stage_registry(SpaceBeta), #{
        P1 => legacy_support_entry(P1Storage),
        P2 => #support_stage_details{provider_stage = active, per_storage = #{NewP2Storage => active}}
    }),
    ?assert(ozt_spaces:has_initial_capacity_usage(SpaceBeta, ?SUPPORT_SIZE, #{
        P1 => [P1Storage], P2 => [NewP2Storage]
    })),
    ?assert(ozt_spaces:has_initial_sync_progress(SpaceBeta, SpaceCreationTime, [P1, P2])),

    % P3 is upgraded to the newest version
    NewP3Storage = ozt_providers:create_storage(P3),
    ozt_providers:simulate_version(P3, ?LINE_21_02(<<"2">>)),
    ozt_providers:support_space(P3, NewP3Storage, SpaceAlpha, ?SUPPORT_SIZE),
    upgrade_support_to_22_02(P3, P3Storage, SpaceAlpha),
    upgrade_support_to_22_02(P3, NewP3Storage, SpaceAlpha),
    ozt_providers:simulate_version(P3, ?LINE_22_02(<<"2">>)),
    ?waitUntilEqual(ozt_spaces:get_support_stage_registry(SpaceAlpha), #{
        P1 => legacy_support_entry(P1Storage),
        P3 => #support_stage_details{provider_stage = active, per_storage = #{
            P3Storage => active,
            NewP3Storage => active
        }}
    }),
    ?assert(ozt_spaces:has_initial_capacity_usage(SpaceAlpha, ?SUPPORT_SIZE, #{
        P1 => [P1Storage], P3 => [P3Storage, NewP3Storage]
    })),
    ?assert(ozt_spaces:has_initial_sync_progress(SpaceAlpha, SpaceCreationTime, [P1, P3])),

    % SpaceBeta is supported by P3 using the modern procedure
    ozt_providers:support_space(P3, NewP3Storage, SpaceBeta, ?SUPPORT_SIZE),
    ?waitUntilEqual(ozt_spaces:get_support_stage_registry(SpaceBeta), #{
        P1 => legacy_support_entry(P1Storage),
        P2 => #support_stage_details{provider_stage = active, per_storage = #{NewP2Storage => active}},
        P3 => #support_stage_details{provider_stage = active, per_storage = #{NewP3Storage => active}}
    }),
    ?assert(ozt_spaces:has_initial_capacity_usage(SpaceBeta, ?SUPPORT_SIZE, #{
        P1 => [P1Storage], P2 => [NewP2Storage], P3 => [NewP3Storage]
    })),
    ?assert(ozt_spaces:has_initial_sync_progress(SpaceBeta, SpaceCreationTime, [P1, P2, P3])).

%%%===================================================================
%%% Helper functions
%%%===================================================================

simulate_preexisting_provider_with_spaces(Version, SpaceIds) ->
    ProviderId = ozt_providers:create(),
    ozt_providers:simulate_version(ProviderId, Version),
    StorageId = ozt_providers:create_storage(ProviderId),
    lists:foreach(fun(SpaceId) ->
        ozt_providers:support_space(ProviderId, StorageId, SpaceId, ?SUPPORT_SIZE)
    end, SpaceIds),
    {ProviderId, StorageId}.


upgrade_support_to_22_02(ProviderId, StorageId, SpaceId) ->
    ozt:rpc(storage_logic, upgrade_support_to_22_02, [?PROVIDER(ProviderId), StorageId, SpaceId]).


legacy_support_entry(StorageId) ->
    #support_stage_details{provider_stage = legacy, per_storage = #{StorageId => legacy}}.
