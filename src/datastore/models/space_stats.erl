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

-define(CTX, #{
    model => ?MODULE,
    sync_enabled => true,
    memory_copies => all
}).

-type record() :: #space_stats{}.
-export_type([record/0]).

%% API
-export([init_for_space/1, coalesce_providers/2, clear_for_space/1]).

%% datastore_model callbacks
-export([get_record_struct/1]).
-export([encode_sync_progress_per_provider/1, decode_sync_progress_per_provider/1]).

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
%% Populates the space stats document with empty entries for all supporting
%% providers where required. Assumes that the space_stats doc already exists.
%% @end
%%--------------------------------------------------------------------
-spec coalesce_providers(od_space:id(), [od_provider:id()]) ->
    ok.
coalesce_providers(SpaceId, SupportingProviders) ->
    {ok, _} = datastore_model:update(?CTX, SpaceId, fun(SpaceStats) ->
        #space_stats{
            all_providers = PreviousProviders,
            sync_progress_per_provider = SyncProgressPerProvider
        } = SpaceStats,
        % stats are retained for all providers that ever supported the space for
        % archival purposes
        AllProviders = lists:usort(PreviousProviders ++ SupportingProviders),
        {ok, SpaceStats#space_stats{
            all_providers = AllProviders,
            sync_progress_per_provider = provider_sync_progress:coalesce_all(
                SyncProgressPerProvider, AllProviders
            )
        }}
    end),
    ok.


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
        {all_providers, [string]},
        {sync_progress_per_provider, {custom, json, {
            ?MODULE, encode_sync_progress_per_provider, decode_sync_progress_per_provider
        }}}
    ]}.


%% @private
-spec encode_sync_progress_per_provider(provider_sync_progress:per_provider()) -> binary().
encode_sync_progress_per_provider(Value) ->
    json_utils:encode(provider_sync_progress:per_provider_to_json(Value)).

%% @private
-spec decode_sync_progress_per_provider(binary()) -> provider_sync_progress:per_provider().
decode_sync_progress_per_provider(Value) ->
    provider_sync_progress:per_provider_from_json(json_utils:decode(Value)).

%%%===================================================================
%%% entity logic plugin behaviour
%%%===================================================================

-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    ?MODULE.


-spec operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().
operation_supported(get, instance, private) -> true;
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
    {ok, SpaceStats}.


-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = SpaceId, aspect = {provider_sync_progress, ProviderId}}, data = Data}) ->
    % @TODO VFS-6329 allow only for non-deleted providers and spaces
    ProviderSyncProgress = provider_sync_progress:from_json(maps:get(<<"providerSyncProgress">>, Data)),
    {ok, _} = datastore_model:update(?CTX, SpaceId, fun(SpaceStats) ->
        #space_stats{
            all_providers = AllProviders,
            sync_progress_per_provider = PerProvider
        } = SpaceStats,
        {ok, SpaceStats#space_stats{
            sync_progress_per_provider = provider_sync_progress:update_for_provider(
                PerProvider, AllProviders, ProviderId, ProviderSyncProgress
            )
        }}
    end),
    ok.


-spec delete(entity_logic:req()) -> errors:error().
delete(_) ->
    ?ERROR_NOT_SUPPORTED.


-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(_, _) ->
    true. % Checked in fetch_entity


-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(#el_req{auth = ?USER(UserId), operation = get, gri = #gri{id = SpaceId, aspect = instance}}, _) ->
    space_logic:has_eff_user(SpaceId, UserId);
authorize(#el_req{auth = ?PROVIDER(PrId), operation = update, gri = #gri{id = SpaceId, aspect = {provider_sync_progress, PrId}}}, _) ->
    space_logic:is_supported_by_provider(SpaceId, PrId).


-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = private}}) ->
    [?OZ_SPACES_VIEW];
required_admin_privileges(_) ->
    forbidden.


-spec validate(entity_logic:req()) -> entity_logic:validity_verificator().
validate(#el_req{operation = update, gri = #gri{aspect = {provider_sync_progress, _}}}) ->
    #{
        required => #{
            <<"providerSyncProgress">> => {json, fun(Json) ->
                try
                    provider_sync_progress:from_json(Json),
                    true
                catch _:_ ->
                    false
                end
            end}
        }
    }.
