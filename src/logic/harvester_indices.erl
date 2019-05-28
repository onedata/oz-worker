%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all harvester indices functionalities.
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_indices).
-author("Michal Stanisz").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    update_stats/3
]).

-export([
    coalesce_index_stats/3, coalesce_index_stats/4,
    update_seqs/6
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Updates given indices stats in harvester record using given UpdateFun.
%% IndicesToUpdate can be given as list of indices or as list of tuples {IndexId, Mod}
%% in which case given value will be passed to UpdateFun for given index.
%% @end
%%--------------------------------------------------------------------
-spec update_stats
    (od_harvester:id(), od_harvester:indices() | all, UpdateFun) -> ok
    when UpdateFun :: fun((od_harvester:indices_stats()) -> od_harvester:indices_stats());
    (od_harvester:id(), [{od_harvester:index_id(), Mod}], UpdateFun) -> ok
    when UpdateFun :: fun((od_harvester:indices_stats(), Mod) -> od_harvester:indices_stats()),
    Mod :: term().
update_stats(HarvesterId, IndicesToUpdate, UpdateFun) ->
    {ok, _} = od_harvester:update(HarvesterId, fun(#od_harvester{indices = Indices} = Harvester) ->
        NewIndices = case IndicesToUpdate of
            all -> update__stats_internal(maps:keys(Indices), Indices, UpdateFun);
            _ -> update__stats_internal(IndicesToUpdate, Indices, UpdateFun)
        end,
        {ok, Harvester#od_harvester{indices = NewIndices}}
    end),
    ok.


update__stats_internal(IndicesToUpdate, ExistingIndices, UpdateFun) ->
    lists:foldl(fun
        ({IndexId, Mod}, AccIndices) ->
            update_index_stats(AccIndices, IndexId,
                fun(S) -> UpdateFun(S, Mod) end);
        (IndexId, AccIndices) ->
            update_index_stats(AccIndices, IndexId, UpdateFun)
    end, ExistingIndices, IndicesToUpdate).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates given index stats using UpdateFun and returns updated indices stats.
%% @end
%%--------------------------------------------------------------------
-spec update_index_stats(od_harvester:indices(), od_harvester:index_id(), UpdateFun) ->
    od_harvester:indices() when UpdateFun :: fun((od_harvester:indices_stats()) -> od_harvester:indices_stats()).
update_index_stats(Indices, IndexId, UpdateFun) ->
    case maps:find(IndexId, Indices) of
        {ok, #harvester_index{stats = Stats} = IndexData} ->
            Indices#{IndexId => IndexData#harvester_index{stats = UpdateFun(Stats)}};
        _ ->
            % Index have been deleted in meantime
            Indices
    end.


%%--------------------------------------------------------------------
%% @doc
%% Sets archival flag in existing stats to given value
%% and creates missing stats for all providers that support given space.
%% @end
%%--------------------------------------------------------------------
-spec coalesce_index_stats(od_harvester:indices_stats(), od_space:id(), boolean()) ->
    od_harvester:indices_stats().
coalesce_index_stats(ExistingStats, SpaceId, ArchivalFlagValue) ->
    {ok, #od_space{providers = Providers}} = space_logic_plugin:fetch_entity(SpaceId),
    coalesce_index_stats(ExistingStats, SpaceId, maps:keys(Providers), ArchivalFlagValue).


%%--------------------------------------------------------------------
%% @doc
%% Sets archival flag in existing stats to given value
%% and creates missing stats for all given providers in given space.
%% @end
%%--------------------------------------------------------------------
-spec coalesce_index_stats(od_harvester:indices_stats(), od_space:id(),
    od_provider:id() | [od_provider:id()], boolean()) -> od_harvester:indices_stats().
coalesce_index_stats(ExistingStats, SpaceId, Provider, ArchivalFlagValue) when is_binary(Provider) ->
    coalesce_index_stats(ExistingStats, SpaceId, [Provider], ArchivalFlagValue);

coalesce_index_stats(ExistingStats, SpaceId, Providers, ArchivalFlagValue) ->
    StatsPerProvider = maps:get(SpaceId, ExistingStats, #{}),
    ExistingStats#{
        SpaceId => lists:foldl(fun(ProviderId, NewStatsPerProvider) ->
            Stats = maps:get(ProviderId, StatsPerProvider, #index_stats{archival = ArchivalFlagValue}),
            NewStatsPerProvider#{ProviderId => Stats#index_stats{archival = ArchivalFlagValue}}
        end, StatsPerProvider, Providers)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates current sequence number and max sequence number for given index stats.
%% When NewSeq is undefined current sequence number will not be updated.
%% Sequence numbers are stored per space per provider.
%% @end
%%--------------------------------------------------------------------
-spec update_seqs(od_harvester:indices_stats(), od_space:id(), od_provider:id(),
    NewSeq :: non_neg_integer() | undefined, NewMaxSeq :: non_neg_integer(),
    ErrorMsg :: binary() | undefined) -> od_harvester:indices_stats().
update_seqs(Stats, SpaceId, ProviderId, NewSeq, NewMaxSeq, ErrorMsg) ->
    SeqsPerProvider = maps:get(SpaceId, Stats, #{}),
    #index_stats{
        current_seq = CurrentSeq,
        max_seq = CurrentMaxSeq
    } = maps:get(ProviderId, SeqsPerProvider, #index_stats{}),

    NewCurrentSeq = case NewSeq of
        undefined -> CurrentSeq;
        _-> max(NewSeq, CurrentSeq)
    end,

    Stats#{
        SpaceId => SeqsPerProvider#{
            ProviderId => #index_stats{
                current_seq = NewCurrentSeq,
                max_seq = max(CurrentMaxSeq, NewMaxSeq),
                last_update = time_utils:cluster_time_seconds(),
                error = ErrorMsg
            }
        }
    }.
