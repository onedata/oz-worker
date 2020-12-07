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
-include_lib("ctool/include/space_support/support_stage.hrl").
-include_lib("ctool/include/space_support/provider_sync_progress.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-define(waitUntilEqual(Expected, Value), ?assertEqual(Expected, Value, 60)).

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    upgrade_from_19_02_with_providers_of_different_versions/1
    %% @TODO VFS-6981 comprehensive tests related to the logic in space_support
    %% and space_stats modules with different combinations of different provider versions
]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    upgrade_from_19_02_with_providers_of_different_versions
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
upgrade_from_19_02_with_providers_of_different_versions(_Config) ->
    SpaceAlpha = ozt_spaces:create(),
    SpaceBeta = ozt_spaces:create(),
    P1 = simulate_preexisting_provider_with_spaces(?LINE_19_02(<<"1">>), [SpaceAlpha, SpaceBeta]),
    P2 = simulate_preexisting_provider_with_spaces(?LINE_19_02(<<"2">>), []),
    P3 = simulate_preexisting_provider_with_spaces(?LINE_19_02(<<"3">>), [SpaceAlpha]),

    ?assertEqual(ozt_spaces:get_support_stage_registry(SpaceAlpha), #{
        P1 => ?LEGACY_SUPPORT,
        P3 => ?LEGACY_SUPPORT
    }),
    ?assertEqual(ozt_spaces:get_support_stage_registry(SpaceBeta), #{
        P1 => ?LEGACY_SUPPORT
    }),

    % at this point, all providers are in version 19.02.* and cannot connect to
    % Onezone (must be upgraded first)
    Now = ozt_mocks:get_frozen_time_seconds(),

    % P2 is upgraded to 20.02 and supports SpaceBeta using the legacy support procedure
    P2Storage = ozt_providers:create_storage(P2),
    ozt_providers:simulate_version(P2, ?LINE_20_02(<<"rc2">>)),
    ozt_providers:support_space(P2, P2Storage, SpaceBeta),
    ?assertEqual(ozt_spaces:get_support_stage_registry(SpaceBeta), #{
        P1 => ?LEGACY_SUPPORT,
        P2 => ?LEGACY_SUPPORT
    }),
    ?assertEqual(ozt_spaces:extract_sync_progress_registry_matrix(SpaceBeta), #{
        P1 => #{P1 => {1, Now}, P2 => {1, Now}},
        P2 => #{P1 => {1, Now}, P2 => {1, Now}}
    }),

    % P2 is upgraded to the newest version
    upgrade_support_to_21_02(P2, P2Storage, SpaceBeta),
    ozt_providers:simulate_version(P2, ?LINE_21_02(<<"1">>)),
    ?waitUntilEqual(ozt_spaces:get_support_stage_registry(SpaceBeta), #{
        P1 => ?LEGACY_SUPPORT,
        P2 => #support_stage_details{provider_stage = active, per_storage = #{P2Storage => active}}
    }),
    ?assertEqual(ozt_spaces:extract_sync_progress_registry_matrix(SpaceBeta), #{
        P1 => #{P1 => {1, Now}, P2 => {1, Now}},
        P2 => #{P1 => {1, Now}, P2 => {1, Now}}
    }),

    % P3 is upgraded to the newest version
    P3Storage = ozt_providers:create_storage(P3),
    upgrade_support_to_20_02(P3, P3Storage, SpaceAlpha),
    ozt_providers:simulate_version(P3, ?LINE_20_02(<<"2">>)),
    upgrade_support_to_21_02(P3, P3Storage, SpaceAlpha),
    ozt_providers:simulate_version(P3, ?LINE_21_02(<<"2">>)),
    ?waitUntilEqual(ozt_spaces:get_support_stage_registry(SpaceAlpha), #{
        P1 => ?LEGACY_SUPPORT,
        P3 => #support_stage_details{provider_stage = active, per_storage = #{P3Storage => active}}
    }),
    ?assertEqual(ozt_spaces:extract_sync_progress_registry_matrix(SpaceAlpha), #{
        P1 => #{P1 => {1, Now}, P3 => {1, Now}},
        P3 => #{P1 => {1, Now}, P3 => {1, Now}}
    }),

    % SpaceBeta is supported by P3 using the modern procedure
    ozt_providers:support_space(P3, P3Storage, SpaceBeta),
    ?waitUntilEqual(ozt_spaces:get_support_stage_registry(SpaceBeta), #{
        P1 => ?LEGACY_SUPPORT,
        P2 => #support_stage_details{provider_stage = active, per_storage = #{P2Storage => active}},
        P3 => #support_stage_details{provider_stage = active, per_storage = #{P3Storage => active}}
    }),
    ?assertEqual(ozt_spaces:extract_sync_progress_registry_matrix(SpaceBeta), #{
        P1 => #{P1 => {1, Now}, P2 => {1, Now}, P3 => {1, Now}},
        P2 => #{P1 => {1, Now}, P2 => {1, Now}, P3 => {1, Now}},
        P3 => #{P1 => {1, Now}, P2 => {1, Now}, P3 => {1, Now}}
    }).

%%%===================================================================
%%% Helper functions
%%%===================================================================

simulate_preexisting_provider_with_spaces(Version, SpaceIds) ->
    ProviderId = ozt_providers:create(),
    ozt_providers:simulate_version(ProviderId, Version),
    lists:foreach(fun(SpaceId) ->
        case Version of
            ?LINE_19_02(_) ->
                ozt_providers:simulate_preexisting_19_02_space_support(ProviderId, SpaceId);
            ?LINE_20_02(_) ->
                StorageId = ozt_providers:create_storage(ProviderId),
                ozt_providers:support_space(ProviderId, StorageId, SpaceId)
        end
    end, SpaceIds),
    ProviderId.


upgrade_support_to_20_02(ProviderId, StorageId, SpaceId) ->
    ozt:rpc(storage_logic, upgrade_support_to_20_02, [?PROVIDER(ProviderId), StorageId, SpaceId]).


upgrade_support_to_21_02(ProviderId, StorageId, SpaceId) ->
    ozt:rpc(storage_logic, upgrade_support_to_21_02, [?PROVIDER(ProviderId), StorageId, SpaceId]).