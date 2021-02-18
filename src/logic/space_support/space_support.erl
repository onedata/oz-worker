%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all logic related to space support, space lifecycle
%%% and support compatibility/upgradability across different Onezone versions.
%%% It is used by the entity_logic layer and assumes that validity or
%%% authorization checks have already been applied.
%%%
%%% Compatiblity with given Oneprovider versions:
%%%
%%% 19.02 - Oneproviders in this version can be no longer upgraded, as it is
%%%         possible to upgrade only to 20.02, which is too old to communicate
%%%         with Onezone. After Onezone is upgraded to 22.02, such providers
%%%         are effectively lost.
%%%
%%% 20.02 - Oneproviders in this version won't connect to Onezone as the compa-
%%%         tibility reference prevents that, but they can be upgraded to 21.02.
%%%         It is required that such Oneprovider had successfully upgraded to
%%%         20.02 in the past and it uses the modern concept of storages held
%%%         in Onezone.
%%%
%%% 21.02 - Oneproviders in this version are supported, but treated as legacy.
%%%         Their supports do not conform to the new mechanisms introduced in
%%%         22.02 - space parameters, support stages or provider sync progress.
%%%         Such providers will remain operational and marked as having the
%%%         default support parameters, legacy support stage and initial
%%%         sync progress (seq = 1). They will be able to operate with other
%%%         providers in 21.02, but not with the modern ones. The next upgrade
%%%         is needed to move on:
%%%         * Onezone has already initialized the new support info for all
%%%           spaces during its upgrade to 22.02, by calling the
%%%           'migrate_all_supports_to_22_02_model' procedure - all providers
%%%           have default parameters, legacy support and initial sync progress.
%%%         * Oneprovider will upgrade its cluster and switch to the new support
%%%           stage handling by calling the 'upgrade_support_to_22_02'
%%%           operation, which will set its stage to joining.
%%%         * Oneprovider will update its version info and mark itself as 22.02.
%%%         * Oneprovider will start reporting its sync progress, which will
%%%           populate the space_stats record and cause support stage transition
%%%           to active when it has caught up with other providers.
%%%         * Peer Oneproviders within the supported space that do not make the
%%%           upgrade will shortly be marked as desync and will not be able to
%%%           communicate with the 22.02 Oneproviders, nevertheless they will be
%%%           able to communicate with other un-upgraded Oneproviders. Until all
%%%           supporting Oneproviders unify their versions, there will be a
%%%           growing desync state of the their databases
%%%           (stored data, transfers, QoS etc).
%%%
%%% 22.02 - Oneproviders in this version are treated as up-to-date and use all
%%%         mechanisms related to space support.
%%%
%%%
%%% Adding/updating/deleting support depending on version:
%%%
%%% 19.02 - Oneproviders in this version are not allowed to connect to Onezone
%%%         nor change any of their support state. The 19.02 provider API
%%%         related to space support has been completely removed. @TODO VFS-5856
%%%
%%% 20.02 - Oneproviders in this version are not allowed to connect to Onezone
%%%         nor change any of their support state.
%%%
%%% 21.02 - Oneproviders in this version still use the legacy support API, made
%%%         up of 3 operations: create/update/delete support. These API calls
%%%         are emulated using the new support mechanisms - the legacy logic is
%%%         reflected in the new space support model, leading to the same state
%%%         as after upgrading Onezone to 22.02.
%%%
%%% 22.02 - Oneproviders in this version use the API that corresponds to the
%%%         support stage transitions, e.g. init_support, init_unsupport,
%%%         complete_unsupport_resize etc.
%%%
%%% @TODO VFS-6311 update these docs when the lifecycle integration is done
%%% @end
%%%-------------------------------------------------------------------
-module(space_support).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/space_support/support_stage.hrl").

-type registry_transition_fun() :: fun((support_stage:registry()) -> {ok, support_stage:registry()} | errors:error()).
-type space_update_fun() :: fun((od_space:record()) -> od_space:record() | no_return()).

-export([init_support/5]).
-export([finalize_support/2]).
-export([resize/3]).
-export([init_unsupport/3]).
-export([complete_unsupport_resize/3]).
-export([complete_unsupport_purge/3]).
-export([finalize_unsupport/3]).

-export([force_unsupport/3]).

-export([upgrade_support_to_20_02/3]).
-export([upgrade_support_to_22_02/3]).
-export([migrate_all_supports_to_22_02_model/0]).

-export([lock_on_storage/2, lock_on_space_support/3]).

-define(CHECK_TRANSITION(Term), case Term of
    {ok, __NewRegistry} -> __NewRegistry;
    {error, _} = __Error -> throw(__Error)
end).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes support of a space with given provider's storage. Common
%% procedure for legacy and modern providers - internally, it checks the
%% provider version and applies legacy or modern logic.
%% @end
%%--------------------------------------------------------------------
-spec init_support(od_provider:id(), od_storage:id(), od_space:id(),
    od_space:support_size(), support_parameters:parameters()) -> entity_logic:create_result() | no_return().
init_support(ProviderId, StorageId, SpaceId, SupportSize, ProposedParameters) ->
    lock_on_space_support(StorageId, SpaceId, fun() ->
        init_support_insecure(ProviderId, StorageId, SpaceId, SupportSize, ProposedParameters)
    end).

%% @private
-spec init_support_insecure(od_provider:id(), od_storage:id(), od_space:id(),
    od_space:support_size(), support_parameters:parameters()) -> entity_logic:create_result() | no_return().
init_support_insecure(ProviderId, StorageId, SpaceId, SupportSize, ProposedParameters) ->
    Storage = get_storage(StorageId),
    case is_imported_storage(Storage) of
        true ->
            ensure_storage_not_supporting_any_space(Storage),
            ensure_space_not_supported_by_imported_storage(SpaceId);
        _ ->
            ok
    end,

    entity_graph:add_relation(
        od_space, SpaceId,
        od_storage, StorageId,
        SupportSize
    ),

    SupportModel = od_provider:check_support_model(ProviderId),

    RegistryTransitionFun = fun(Registry) ->
        % if the provider has already supported the space and it is in a stage later
        % than 'joining', the new support should be immediately marked as 'active'
        % (the provider is already synced as it has finished joining at least once).
        ShouldTransitionToActive = case support_stage:lookup_details(Registry, ProviderId) of
            {ok, #support_stage_details{provider_stage = legacy}} -> false;
            {ok, #support_stage_details{provider_stage = joining}} -> false;
            {ok, #support_stage_details{provider_stage = retired}} -> false;
            {ok, #support_stage_details{provider_stage = _}} -> true;
            error -> false
        end,
        case ShouldTransitionToActive of
            false ->
                support_stage:init_support(Registry, SupportModel, ProviderId, StorageId);
            true ->
                NewRegistry = ?CHECK_TRANSITION(support_stage:init_support(Registry, SupportModel, ProviderId, StorageId)),
                support_stage:finalize_support(NewRegistry, ProviderId, StorageId)
        end
    end,
    SpaceDiff = fun(#od_space{support_parameters_registry = ParametersRegistry} = Space) ->
        SpaceSupportParameters = case support_parameters:lookup_by_provider(ParametersRegistry, ProviderId) of
            % If the space is already supported by the provider, the parameters are inherited
            % from previous supports (and token parameters are ignored in such case)
            {ok, Existing} -> Existing;
            error -> ProposedParameters
        end,
        Space#od_space{
            support_parameters_registry = support_parameters:update_for_provider(
                ParametersRegistry, ProviderId, SpaceSupportParameters
            )
        }
    end,
    update_space_with_transition(
        SpaceId, ProviderId, SpaceDiff, RegistryTransitionFun
    ),

    space_stats:register_support(SupportModel, SpaceId, StorageId, ProviderId, SupportSize),

    NewGRI = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
    {true, {Space, Rev}} = space_logic_plugin:fetch_entity(NewGRI),

    IsFirstSupportOfProvider = length(od_space:get_supporting_storages_of_provider(Space, ProviderId)) =:= 1,
    IsFirstSupportOfProvider andalso lists:foreach(fun(HarvesterId) ->
        harvester_indices:update_stats(HarvesterId, all, fun(ExistingStats) ->
            harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, ProviderId, false)
        end)
    end, Space#od_space.harvesters),

    {ok, SpaceData} = space_logic_plugin:get(#el_req{gri = NewGRI}, Space),
    {ok, resource, {NewGRI, {SpaceData, Rev}}}.


%%--------------------------------------------------------------------
%% @doc
%% Finalizes the support process, marking the provider as active.
%% Must not be called for legacy providers.
%% @end
%%--------------------------------------------------------------------
-spec finalize_support(od_provider:id(), od_space:id()) -> od_space:record() | no_return().
finalize_support(ProviderId, SpaceId) ->
    update_space(SpaceId, fun(Space = #od_space{support_stage_registry = SupportStageRegistry}) ->
        {ok, SupportStageDetails} = support_stage:lookup_details(SupportStageRegistry, ProviderId),
        lists:foldl(fun(StorageId, AccSpace) ->
            apply_transition_within_space(SpaceId, AccSpace, ProviderId, fun(Registry) ->
                support_stage:finalize_support(Registry, ProviderId, StorageId)
            end)
        end, Space, maps:keys(SupportStageDetails#support_stage_details.per_storage))
    end).


-spec resize(od_storage:id(), od_space:id(), od_space:support_size()) -> ok | no_return().
resize(StorageId, SpaceId, NewSupportSize) ->
    lock_on_space_support(StorageId, SpaceId, fun() ->
        entity_graph:update_relation(
            od_space, SpaceId,
            od_storage, StorageId,
            NewSupportSize
        )
    end).


-spec init_unsupport(od_provider:id(), od_storage:id(), od_space:id()) -> ok | no_return().
init_unsupport(ProviderId, StorageId, SpaceId) ->
    update_space_with_transition(SpaceId, ProviderId, fun(Registry) ->
        support_stage:init_unsupport(Registry, ProviderId, StorageId)
    end),
    ok.


-spec complete_unsupport_resize(od_provider:id(), od_storage:id(), od_space:id()) -> ok | no_return().
complete_unsupport_resize(ProviderId, StorageId, SpaceId) ->
    update_space_with_transition(SpaceId, ProviderId, fun(Registry) ->
        support_stage:complete_unsupport_resize(Registry, ProviderId, StorageId)
    end),
    ok.


-spec complete_unsupport_purge(od_provider:id(), od_storage:id(), od_space:id()) -> ok | no_return().
complete_unsupport_purge(ProviderId, StorageId, SpaceId) ->
    update_space_with_transition(SpaceId, ProviderId, fun(Registry) ->
        support_stage:complete_unsupport_purge(Registry, ProviderId, StorageId)
    end),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Finalizes unsupport of a space with given provider's storage. Common
%% procedure for legacy and modern providers - internally, it checks the
%% provider version and applies legacy or modern logic.
%% @end
%%--------------------------------------------------------------------
-spec finalize_unsupport(od_provider:id(), od_storage:id(), od_space:id()) -> ok | no_return().
finalize_unsupport(ProviderId, StorageId, SpaceId) ->
    lock_on_space_support(StorageId, SpaceId, fun() ->
        finalize_unsupport_insecure(ProviderId, StorageId, SpaceId)
    end).

%% @private
-spec finalize_unsupport_insecure(od_provider:id(), od_storage:id(), od_space:id()) -> ok | no_return().
finalize_unsupport_insecure(ProviderId, StorageId, SpaceId) ->
    entity_graph:remove_relation(
        od_space, SpaceId,
        od_storage, StorageId
    ),

    SupportModel = od_provider:check_support_model(ProviderId),

    space_stats:register_unsupport(SupportModel, SpaceId, StorageId, ProviderId),

    RegistryTransitionFun = fun(Registry) ->
        support_stage:finalize_unsupport(Registry, ProviderId, StorageId)
    end,
    SpaceDiff = fun(#od_space{support_parameters_registry = ParametersRegistry} = Space) ->
        Space#od_space{
            % support parameters are removed in case of the last support
            support_parameters_registry = case space_logic:is_supported_by_provider(Space, ProviderId) of
                true -> ParametersRegistry;
                false -> support_parameters:remove_for_provider(ParametersRegistry, ProviderId)
            end
        }
    end,
    UpdatedSpace = update_space_with_transition(
        SpaceId, ProviderId, SpaceDiff, RegistryTransitionFun
    ),

    WasLastSupportSupportOfProvider = not space_logic:is_supported_by_provider(UpdatedSpace, ProviderId),
    case WasLastSupportSupportOfProvider of
        false ->
            ok;
        true ->
            lists:foreach(fun(HarvesterId) ->
                harvester_indices:update_stats(HarvesterId, all, fun(ExistingStats) ->
                    harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, ProviderId, true)
                end)
            end, UpdatedSpace#od_space.harvesters)
    end.


%% @TODO VFS-6304 currently, this pretends to be a step-by-step unsupport, however it should be
%% handled in different way to allow providers to clean up afterwards (mark them as force-retired)
-spec force_unsupport(od_provider:id(), od_storage:id(), od_space:id()) -> ok | no_return().
force_unsupport(ProviderId, StorageId, SpaceId) ->
    % run step-by-step unsupport only for modern providers as legacy
    % providers cannot change their support stage
    case od_provider:check_support_model(ProviderId) of
        legacy ->
            ok;
        modern ->
            init_unsupport(ProviderId, StorageId, SpaceId),
            complete_unsupport_resize(ProviderId, StorageId, SpaceId),
            complete_unsupport_purge(ProviderId, StorageId, SpaceId)
    end,
    finalize_unsupport(ProviderId, StorageId, SpaceId).


%%--------------------------------------------------------------------
%% @doc
%% Providers that are upgraded from version 19.02 to 20.02 call this operation
%% during the cluster upgrade procedure to migrate their supports from the
%% virtual storage (id == providerId) to the storages migrated shortly beforehand.
%% The procedure is idempotent in case a provider calls it twice due to a crash
%% or restart during the cluster upgrade.
%% @end
%%--------------------------------------------------------------------
%% @TODO VFS-5856 remove deprecated space support functionalities
-spec upgrade_support_to_20_02(od_provider:id(), od_storage:id(), od_space:id()) -> ok.
upgrade_support_to_20_02(ProviderId, StorageId, SpaceId) ->
    VirtualStorageId = ProviderId,
    % run the procedure only if the space is supported with the virtual storage
    % otherwise just return success (the upgrade should be idempotent)
    storage_logic:supports_space(VirtualStorageId, SpaceId) andalso lock_on_space_support(StorageId, SpaceId, fun() ->
        ?info("Processing request to upgrade legacy support of space '~s' by provider '~s' to ~s model...", [
            SpaceId, ProviderId, ?LINE_20_02
        ]),
        {ok, #document{value = VirtualStorage}} = od_storage:get(VirtualStorageId),
        SupportSize = maps:get(SpaceId, VirtualStorage#od_storage.spaces),
        try
            init_support(ProviderId, StorageId, SpaceId, SupportSize, support_parameters:build(global, eager))
        catch
            _:(?ERROR_RELATION_ALREADY_EXISTS(_, _, _, _)) -> ok
        end,
        try
            entity_graph:remove_relation(od_space, SpaceId, od_storage, VirtualStorageId)
        catch
            _:(?ERROR_RELATION_DOES_NOT_EXIST(_, _, _, _)) -> ok
        end,
        ?notice("Successfully upgraded legacy support of space '~s' by provider '~s' to ~s model", [
            SpaceId, ProviderId, ?LINE_20_02
        ])
    end),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Providers that are upgraded from version 21.02 to 22.02 call this operation
%% during the cluster upgrade procedure to migrate their supports to the new model.
%% The procedure is idempotent in case a provider calls it twice due to a crash
%% or restart during the cluster upgrade.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_support_to_22_02(od_provider:id(), od_storage:id(), od_space:id()) -> ok.
upgrade_support_to_22_02(ProviderId, StorageId, SpaceId) ->
    ?info("Processing request to upgrade legacy support of space '~s' by provider '~s' to ~s model...", [
        SpaceId, ProviderId, ?LINE_22_02
    ]),
    UpdatedSpace = update_space_with_transition(SpaceId, ProviderId, fun(Registry) ->
        case support_stage:lookup_details(Registry, ProviderId) of
            {ok, #support_stage_details{per_storage = #{StorageId := legacy}}} ->
                support_stage:upgrade_support(Registry, ProviderId, StorageId);
            {ok, _} ->
                % the upgrade has already been done
                {ok, Registry};
            error ->
                % the provider does not support the space
                ?ERROR_FORBIDDEN
        end
    end),
    try
        case support_stage:lookup_details(UpdatedSpace#od_space.support_stage_registry, ProviderId) of
            {ok, #support_stage_details{provider_stage = joining}} ->
                % the support is considered upgraded when all storages have been upgraded
                % (which causes provider stage transition to joining)
                space_stats:register_upgrade_of_legacy_support(SpaceId, ProviderId);
            _ ->
                ok
        end
    catch error:{not_a_legacy_provider, ProviderId} ->
        ok
    end,
    ?notice("Successfully upgraded legacy support of space '~s' by provider '~s' to ~s model", [
        SpaceId, ProviderId, ?LINE_22_02
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Initializes new models related to space support with default values.
%% Dedicated for upgrading Onezone from 21.02.* to 22.02.*.
%% The procedure is idempotent in case Onezone crashes during execution.
%% @end
%%--------------------------------------------------------------------
-spec migrate_all_supports_to_22_02_model() -> ok.
migrate_all_supports_to_22_02_model() ->
    entity_graph:ensure_up_to_date(),
    ?info("Migrating space supports to the new model (~s)...", [?LINE_22_02]),
    {ok, Spaces} = od_space:list(),
    lists:foreach(fun(#document{key = SpaceId, value = SpaceRecord}) ->
        % @TODO VFS-6780 rework calculation of effective supports to eliminate risk of non-recalculated graph
        EffProviders = maps:keys(SpaceRecord#od_space.eff_providers),
        SupportingStorages = maps:keys(SpaceRecord#od_space.storages),
        UpdatedSpace = update_space(SpaceId, fun(Space) ->
            lists:foldl(fun(ProvId, SpaceAcc) ->
                SpaceAccWithParameters = SpaceAcc#od_space{
                    support_parameters_registry = support_parameters:update_for_provider(
                        SpaceAcc#od_space.support_parameters_registry, ProvId, support_parameters:build(global, eager)
                    )
                },
                {ok, #od_provider{storages = ProviderStorages}} = provider_logic:get(?ROOT, ProvId),
                SupportingProviderStorages = lists_utils:intersect(ProviderStorages, SupportingStorages),
                lists:foldl(fun(StorageId, SpaceAccWithNewSupport) ->
                    apply_transition_within_space(SpaceId, SpaceAccWithNewSupport, ProvId, fun(Registry) ->
                        support_stage:init_support(Registry, legacy, ProvId, StorageId)
                    end)
                end, SpaceAccWithParameters, SupportingProviderStorages)
            end, Space, EffProviders)
        end),
        #od_space{
            name = Name,
            creation_time = CreationTime,
            storages = SpaceStorages
        } = UpdatedSpace,
        space_stats:init_for_space(SpaceId, CreationTime),
        maps:map(fun(StorageId, SupportSize) ->
            {ok, #od_storage{provider = ProviderId}} = storage_logic:get(?ROOT, StorageId),
            try
                space_stats:register_support(legacy, SpaceId, StorageId, ProviderId, SupportSize)
            catch error:{support_already_registered, ProviderId} ->
                ok
            end
        end, SpaceStorages),
        ?info("  * space '~ts' (~ts) OK, ~B providers", [Name, SpaceId, length(EffProviders)])
    end, Spaces),
    ?notice("Successfully migrated space supports").


%%--------------------------------------------------------------------
%% @doc
%% Used to avoid race conditions between added/removed supported spaces
%% and storage modification.
%% @end
%%--------------------------------------------------------------------
-spec lock_on_storage(od_storage:id(), fun(() -> Term)) -> Term.
lock_on_storage(StorageId, Fun) ->
    critical_section:run({storage_lock, StorageId}, Fun).


%%--------------------------------------------------------------------
%% @doc
%% Used to avoid race conditions when adding/removing relations between spaces/storages
%% and to avoid simultaneous supports by 2 providers with imported storage.
%% @end
%%--------------------------------------------------------------------
-spec lock_on_space_support(od_storage:id(), od_space:id(), fun(() -> Term)) -> Term.
lock_on_space_support(StorageId, SpaceId, Fun) ->
    lock_on_storage(StorageId, fun() ->
        critical_section:run({space_support, SpaceId}, Fun)
    end).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec get_storage(od_storage:id()) -> od_storage:record().
get_storage(StorageId) ->
    {ok, #document{value = Storage}} = od_storage:get(StorageId),
    Storage.


%% @private
-spec update_space(od_space:id(), space_update_fun()) -> od_space:record() | no_return().
update_space(SpaceId, Diff) ->
    ErrorHandlingWrapper = fun(Space) ->
        try
            {ok, Diff(Space)}
        catch throw:{error, _} = Error ->
            {error, {thrown, Error}}
        end
    end,
    case od_space:update(SpaceId, ErrorHandlingWrapper) of
        {ok, #document{value = SpaceRecord}} -> SpaceRecord;
        % re-throw only explicitly thrown (expected) errors, otherwise just crash
        {error, {thrown, Error}} -> throw(Error)
    end.


%% @private
-spec update_space_with_transition(od_space:id(), od_provider:id(), registry_transition_fun()) ->
    od_space:record() | no_return().
update_space_with_transition(SpaceId, ProviderId, RegistryTransitionFun) ->
    update_space_with_transition(SpaceId, ProviderId, fun(Space) -> Space end, RegistryTransitionFun).


%% @private
-spec update_space_with_transition(od_space:id(), od_provider:id(), space_update_fun(), registry_transition_fun()) ->
    od_space:record() | no_return().
update_space_with_transition(SpaceId, ProviderId, SpaceDiff, RegistryTransitionFun) ->
    update_space(SpaceId, fun(Space) ->
        NewSpace = SpaceDiff(Space),
        apply_transition_within_space(SpaceId, NewSpace, ProviderId, RegistryTransitionFun)
    end).


%% @private
-spec apply_transition_within_space(od_space:id(), od_space:record(), od_provider:id(), registry_transition_fun()) ->
    od_space:record() | no_return().
apply_transition_within_space(SpaceId, Space, ProviderId, RegistryTransitionFun) ->
    NewRegistry = ?CHECK_TRANSITION(RegistryTransitionFun(Space#od_space.support_stage_registry)),
    {ok, ProviderName} = od_provider:get_name(ProviderId),
    case support_stage:lookup_details(NewRegistry, ProviderId) of
        error ->
            % possible when a legacy provider's support has been revoked
            ?info(
                "Support changed for space '~ts' (~s):~n"
                "  * Legacy provider '~ts' has ceased support for the space (~s)", [
                    Space#od_space.name, SpaceId,
                    ProviderName, ProviderId
                ]
            );
        {ok, Details = #support_stage_details{provider_stage = ProviderStage}} ->
            StorageStagesStr = maps:fold(fun(StorageId, StorageStage, Acc) ->
                {ok, StorageName} = od_storage:get_name(StorageId),
                StorageStageStr = str_utils:format_bin(
                    "  * Storage '~ts' is now in stage '~w' (~s)~n",
                    [StorageName, StorageStage, StorageId]
                ),
                <<Acc/binary, StorageStageStr/binary>>
            end, <<"">>, Details#support_stage_details.per_storage),
            ?info(
                "Support changed for space '~ts' (~s):~n"
                "~ts"
                "  * Provider '~ts' is now in stage '~s' (~s)", [
                    Space#od_space.name, SpaceId,
                    StorageStagesStr,
                    ProviderName, ProviderStage, ProviderId
                ]
            )
    end,
    Space#od_space{support_stage_registry = NewRegistry}.


%% @private
-spec is_imported_storage(od_storage:id() | od_storage:record()) -> boolean() | no_return().
is_imported_storage(#od_storage{imported = ImportedStorage}) ->
    ImportedStorage == true;
is_imported_storage(StorageId) ->
    is_imported_storage(get_storage(StorageId)).


%% @private
-spec ensure_space_not_supported_by_imported_storage(od_space:id()) -> ok | no_return().
ensure_space_not_supported_by_imported_storage(SpaceId) ->
    {ok, #document{value = #od_space{storages = Storages}}} = od_space:get(SpaceId),
    lists:foreach(fun(StorageId) ->
        is_imported_storage(StorageId) andalso throw(
            ?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(SpaceId, StorageId)
        )
    end, maps:keys(Storages)).


%% @private
-spec ensure_storage_not_supporting_any_space(od_storage:record()) -> ok | no_return().
ensure_storage_not_supporting_any_space(#od_storage{spaces = Spaces}) when map_size(Spaces) =:= 0 ->
    ok;
ensure_storage_not_supporting_any_space(_) ->
    throw(?ERROR_STORAGE_IN_USE).