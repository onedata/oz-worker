%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for records that store space statistics per SpaceId.
%%%
%%% Stats are retained for all providers that ever supported the space for
%%% archival purposes.
%%%
%%% Apart from above, this module implements entity logic plugin behaviour and
%%% handles entity logic operations corresponding to space_stats - fetching
%%% and subscribing for changes.
%%% @end
%%%-------------------------------------------------------------------
-module(space_stats).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/space_support/provider_sync_progress.hrl").
-include_lib("ctool/include/space_support/support_stage.hrl").

-define(CTX, #{
    model => ?MODULE,
    sync_enabled => true,
    memory_copies => all
}).

-type record() :: #space_stats{}.
-export_type([record/0]).

%% API
-export([init_for_space/1]).
-export([register_support/3]).
-export([register_unsupport/2]).
-export([register_legacy_support/3]).
-export([register_upgrade_of_legacy_support/2]).
-export([register_legacy_unsupport/2]).
-export([clear_for_space/1]).

%% datastore_model callbacks
-export([get_record_struct/1]).
-export([encode_provider_sync_progress_registry/1, decode_provider_sync_progress_registry/1]).

%% entity_logic_plugin_behaviour
-export([entity_logic_plugin/0]).

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init_for_space(od_space:id()) -> ok.
init_for_space(SpaceId) ->
    case datastore_model:create(?CTX, #document{key = SpaceId, value = #space_stats{}}) of
        {ok, _} -> ok;
        {error, already_exists} -> ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% Initializes space statistics for a newly supporting provider.
%% Should be called only when the first support from the provider is granted.
%% NOTE: must not be used for legacy providers.
%% @end
%%--------------------------------------------------------------------
-spec register_support(od_space:id(), od_provider:id(), time:seconds()) ->
    ok | no_return().
register_support(SpaceId, NewProviderId, SpaceCreationTimestamp) ->
    update_sync_progress_registry(SpaceId, fun(SyncProgressRegistry) ->
        provider_sync_progress:register_support(SyncProgressRegistry, NewProviderId, SpaceCreationTimestamp)
    end).


%%--------------------------------------------------------------------
%% @doc
%% Marks a provider as no longer supporting the space in all space statistics.
%% Should be called only when the last support of the provider is ceased.
%% NOTE: must not be used for legacy providers.
%% @end
%%--------------------------------------------------------------------
-spec register_unsupport(od_space:id(), od_provider:id()) -> ok | no_return().
register_unsupport(SpaceId, ProviderId) ->
    update_sync_progress_registry(SpaceId, fun(SyncProgressRegistry) ->
        provider_sync_progress:register_unsupport(SyncProgressRegistry, ProviderId)
    end).


%%--------------------------------------------------------------------
%% @doc
%% Equivalent of register_support/3 for legacy providers.
%% The statistics are merely the starting values to ensure backward compatibility,
%% the provider will start collecting them after it is upgraded.
%% @end
%%--------------------------------------------------------------------
-spec register_legacy_support(od_space:id(), od_provider:id(), time:seconds()) ->
    ok | no_return().
register_legacy_support(SpaceId, NewProviderId, SpaceCreationTimestamp) ->
    update_sync_progress_registry(SpaceId, fun(SyncProgressRegistry) ->
        provider_sync_progress:register_legacy_support(SyncProgressRegistry, NewProviderId, SpaceCreationTimestamp)
    end).


%%--------------------------------------------------------------------
%% @doc
%% Should be called when a legacy support is upgraded to the new model. Transforms
%% the statistics accordingly so that the provider can start collecting them.
%% @end
%%--------------------------------------------------------------------
-spec register_upgrade_of_legacy_support(od_space:id(), od_provider:id()) -> ok | no_return().
register_upgrade_of_legacy_support(SpaceId, ProviderId) ->
    update_sync_progress_registry(SpaceId, fun(SyncProgressRegistry) ->
        provider_sync_progress:register_upgrade_of_legacy_support(SyncProgressRegistry, ProviderId)
    end).


%%--------------------------------------------------------------------
%% @doc
%% Equivalent of register_unsupport/2 for legacy providers.
%% @end
%%--------------------------------------------------------------------
-spec register_legacy_unsupport(od_space:id(), od_provider:id()) -> ok | no_return().
register_legacy_unsupport(SpaceId, ProviderId) ->
    update_sync_progress_registry(SpaceId, fun(SyncProgressRegistry) ->
        provider_sync_progress:register_legacy_unsupport(SyncProgressRegistry, ProviderId)
    end).


-spec clear_for_space(od_space:id()) -> ok.
clear_for_space(SpaceId) ->
    ok = datastore_model:delete(?CTX, SpaceId).

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {sync_progress_registry, {custom, json, {
            ?MODULE, encode_provider_sync_progress_registry, decode_provider_sync_progress_registry
        }}},
        {transitioned_from_joining, [string]}
    ]}.


%% @private
-spec encode_provider_sync_progress_registry(provider_sync_progress:registry()) -> binary().
encode_provider_sync_progress_registry(Value) ->
    json_utils:encode(provider_sync_progress:registry_to_json(Value)).


%% @private
-spec decode_provider_sync_progress_registry(binary()) -> provider_sync_progress:registry().
decode_provider_sync_progress_registry(Value) ->
    provider_sync_progress:registry_from_json(json_utils:decode(Value)).

%%%===================================================================
%%% entity logic plugin behaviour
%%%===================================================================

-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    ?MODULE.


-spec operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().
operation_supported(get, instance, private) -> true;
operation_supported(get, {latest_emitted_seq, _ProviderId}, private) -> true;
operation_supported(update, {provider_sync_progress, _ProviderId}, private) -> true;
operation_supported(_, _, _) -> false.


-spec is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().
is_subscribable(instance, private) -> true;
is_subscribable(_, _) -> false.


-spec fetch_entity(gri:gri()) ->
    {true, entity_logic:versioned_entity()} | false | errors:error().
fetch_entity(#gri{id = SpaceId}) ->
    case datastore_model:get(?CTX, SpaceId) of
        {ok, #document{value = SpaceStats, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {SpaceStats, Revision}};
        _ ->
            ?ERROR_NOT_FOUND
    end.


-spec create(entity_logic:req()) -> errors:error().
create(_) ->
    ?ERROR_NOT_SUPPORTED.


-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = instance, scope = private}}, SpaceStats) ->
    {ok, SpaceStats};
get(#el_req{gri = #gri{aspect = {latest_emitted_seq, ProviderId}, scope = private}}, SpaceStats) ->
    case provider_sync_progress:lookup(SpaceStats#space_stats.sync_progress_registry, ProviderId) of
        error ->
            ?ERROR_NOT_FOUND;
        {ok, #provider_summary{per_peer = #{ProviderId := #peer_summary{seen_seq = LatestEmittedSeq}}}} ->
            {ok, LatestEmittedSeq}
    end.


-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = SpaceId, aspect = {provider_sync_progress, ProviderId}}, data = Data}) ->
    % @TODO VFS-6329 allow only for non-deleted providers and spaces
    CollectiveReportJson = maps:get(<<"providerSyncProgressReport">>, Data),
    CollectiveReport = provider_sync_progress:collective_report_from_json(CollectiveReportJson),
    update_sync_progress_registry(SpaceId, fun(SyncProgressRegistry) ->
        provider_sync_progress:consume_collective_report(SyncProgressRegistry, ProviderId, CollectiveReport)
    end).


-spec delete(entity_logic:req()) -> errors:error().
delete(_) ->
    ?ERROR_NOT_SUPPORTED.


-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(_, _) ->
    true. % Checked in fetch_entity


-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
% all aspects can be fetched by supporting providers and users with view privilege
authorize(#el_req{auth = ?USER(UserId), operation = get, gri = #gri{id = SpaceId, aspect = _}}, _) ->
    space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_VIEW);
authorize(#el_req{auth = ?PROVIDER(ProviderId), operation = get, gri = #gri{id = SpaceId, aspect = _}}, _) ->
    space_logic:is_supported_by_provider(SpaceId, ProviderId);
authorize(#el_req{auth = ?PROVIDER(PrId), operation = update, gri = #gri{id = SpaceId, aspect = {provider_sync_progress, PrId}}}, _) ->
    space_logic:is_supported_by_provider(SpaceId, PrId).


-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = private}}) ->
    [?OZ_SPACES_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {latest_emitted_seq, _}, scope = private}}) ->
    [?OZ_SPACES_VIEW];
required_admin_privileges(_) ->
    forbidden.


-spec validate(entity_logic:req()) -> entity_logic:validity_verificator().
validate(#el_req{operation = update, gri = #gri{aspect = {provider_sync_progress, _}}}) ->
    #{
        required => #{
            <<"providerSyncProgressReport">> => {json, fun(Json) ->
                try
                    provider_sync_progress:collective_report_from_json(Json),
                    true
                catch _:_ ->
                    false
                end
            end}
        }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec update_sync_progress_registry(
    od_space:id(),
    fun((provider_sync_progress:registry()) -> provider_sync_progress:registry_update_result())
) ->
    ok | no_return().
update_sync_progress_registry(SpaceId, RegistryUpdateFun) ->
    Diff = fun(SpaceStats) ->
        try
            #registry_update_result{
                new_registry = NewRegistry,
                transitioned_from_joining = NewTransitionedFromJoining
            } = RegistryUpdateFun(SpaceStats#space_stats.sync_progress_registry),
            {ok, SpaceStats#space_stats{
                sync_progress_registry = NewRegistry,
                transitioned_from_joining = lists:append(
                    SpaceStats#space_stats.transitioned_from_joining,
                    NewTransitionedFromJoining
                )
            }}
        catch error:ErrorException ->
            {error, {error_exception, ErrorException}}
        end
    end,
    case datastore_model:update(?CTX, SpaceId, Diff) of
        {ok, _} -> ok;
        {error, {error_exception, ErrorException}} -> error(ErrorException)
    end,
    % asynchronously mark all providers that caught up with sync progress as active
    spawn(fun() ->
        critical_section:run(transition_from_joining, fun() ->
            handle_transitioned_from_joining_insecure(SpaceId)
        end)
    end),
    ok.


%% @private
%% @doc must NOT be run in parallel
-spec handle_transitioned_from_joining_insecure(od_space:id()) -> ok.
handle_transitioned_from_joining_insecure(SpaceId) ->
    {ok, #document{value = #space_stats{transitioned_from_joining = All}}} = datastore_model:get(?CTX, SpaceId),
    lists:foreach(fun(ProviderId) ->
        try
            space_support:finalize_support(ProviderId, SpaceId)
        catch Class:Reason ->
            % this is rather unexpected, but can happen if this procedure is run
            % just after a provider revokes support for a space
            ?warning_stacktrace("Failed to mark provider ~s as active in space ~s~nError was ~w:~p", [
                ProviderId, SpaceId, Class, Reason
            ])
        end,
        {ok, _} = datastore_model:update(?CTX, SpaceId, fun(SpaceStats) ->
            {ok, SpaceStats#space_stats{
                transitioned_from_joining = lists:delete(ProviderId, SpaceStats#space_stats.transitioned_from_joining)
            }}
        end)
    end, All).

