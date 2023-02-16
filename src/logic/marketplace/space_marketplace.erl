%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles storing spaces advertised in marketplace
%%% @end
%%%-------------------------------------------------------------------
-module(space_marketplace).
-author("Bartosz Walkowicz").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").

-export([index/2]).
-export([add/3, delete/3]).
-export([list/2]).

% index() consists of 2 parts:
%  1) space name - so that links would be sorted by name.
%  2) space id - this part allows to distinguish links associated with spaces
%                using the same name.
-type index() :: binary().
-type offset() :: integer().
-type limit() :: pos_integer().

-type listing_opts() :: #{
    limit := limit(),
    start_index => index(),
    offset => offset()
}.
-type entries() :: [{index(), od_space:id()}].

-export_type([index/0, offset/0, limit/0, listing_opts/0, entries/0]).


-define(CTX, (od_provider:get_ctx())).

-define(FOREST, <<"space-marketplace-forest">>).

-define(ALL_TREE_ID, <<"space-marketplace-all-tree">>).
-define(TAG_TREE_ID(__TAG), <<"space-marketplace-tree-for-", __TAG/binary>>).


%%%===================================================================
%%% API
%%%===================================================================


-spec index(od_space:name(), od_space:id()) -> index().
index(SpaceName, SpaceId) ->
    <<SpaceName/binary, "@", SpaceId/binary>>.


-spec add(od_space:name(), od_space:id(), [od_space:tag()]) -> ok.
add(SpaceName, SpaceId, Tags) ->
    Link = {index(SpaceName, SpaceId), SpaceId},

    lists:foreach(fun(TreeId) ->
        case datastore_model:add_links(?CTX, ?FOREST, TreeId, Link) of
            {ok, _} -> ok;
            {error, already_exists} -> ok
        end
    end, [?ALL_TREE_ID | [?TAG_TREE_ID(Tag) || Tag <- Tags]]).


-spec delete(od_space:name(), od_space:id(), [od_space:tag()]) -> ok.
delete(SpaceName, SpaceId, Tags) ->
    Index = index(SpaceName, SpaceId),

    lists:foreach(fun(TreeId) ->
        case datastore_model:delete_links(?CTX, ?FOREST, TreeId, Index) of
            ok -> ok;
            {error, not_found} -> ok
        end
    end, [?ALL_TREE_ID | [?TAG_TREE_ID(Tag) || Tag <- Tags]]).


-spec list(all | [od_space:tag()], listing_opts()) -> entries().
list(all, ListingOpts) ->
    FoldOpts = kv_utils:copy_found([
        {offset, offset},
        {limit, size},
        {start_index, prev_link_name}
    ], ListingOpts),

    FoldFun = fun(#link{name = Index, target = SpaceId}, Acc) ->
        {ok, [{Index, SpaceId} | Acc]}
    end,
    {ok, Entries} = datastore_model:fold_links(
        ?CTX, ?FOREST, ?ALL_TREE_ID, FoldFun, [], FoldOpts
    ),
    lists:reverse(Entries);

list(Tags, #{limit := Limit} = ListingOpts) ->
    Offset = maps:get(offset, ListingOpts, 0),
    StartIndex = maps:get(start_index, ListingOpts, <<>>),

    TreeIds = [?TAG_TREE_ID(Tag) || Tag <- Tags],

    FoldFun = fun
        (#link{name = Index}, Acc = {take, _N, [{PrevIndex, _} | _]}) when Index == PrevIndex ->
            {ok, Acc};
        (#link{name = Index, target = SpaceId}, {take, N, EntriesAcc0}) ->
            EntriesAcc1 = [{Index, SpaceId} | EntriesAcc0],

            case N of
                1 -> {stop, {take, 0, EntriesAcc1}};
                _ -> {ok, {take, N - 1, EntriesAcc1}}
            end;

        (#link{name = Index}, Acc = {skip, _N, LastSkippedIndex}) when Index == LastSkippedIndex ->
            {ok, Acc};
        (_Link, {skip, 1, _LastSkippedIndex}) ->
            {ok, {take, Limit, []}};
        (#link{name = Index}, {skip, N, _LastSkippedIndex}) ->
            {ok, {skip, N - 1, Index}};

        (#link{name = Index, target = SpaceId}, {take_until_start_index, EntriesAcc0}) when
            Index >= StartIndex
        ->
            EntriesAcc1 = lists:sublist(EntriesAcc0, abs(Offset)),
            RemainingLimit = Limit - length(EntriesAcc1),

            if
                RemainingLimit =< 0 ->
                    {stop, {take, 0, lists:nthtail(abs(RemainingLimit), EntriesAcc1)}};
                RemainingLimit == 1 ->
                    {stop, {take, 0, [{Index, SpaceId} | EntriesAcc1]}};
                true ->
                    {ok, {take, RemainingLimit - 1, [{Index, SpaceId} | EntriesAcc1]}}
            end;
        (#link{name = Index}, Acc = {take_until_start_index, [{PrevIndex, _} | _]}) when
            Index == PrevIndex
        ->
            {ok, Acc};
        (#link{name = Index, target = SpaceId}, {take_until_start_index, EntriesAcc}) ->
            {ok, {take_until_start_index, [{Index, SpaceId} | EntriesAcc]}}
    end,

    BasicFoldOpts = #{prev_link_name => StartIndex},

    {InitialAcc, FoldOpts} = if
        Offset < 0 ->
            {{take_until_start_index, []}, BasicFoldOpts#{offset => Offset * length(Tags)}};
        Offset == 0 ->
            {{take, Limit, []}, BasicFoldOpts};
        Offset > 0 ->
            {{skip, Offset, <<>>}, BasicFoldOpts}
    end,

    case datastore_model:fold_links(?CTX, ?FOREST, TreeIds, FoldFun, InitialAcc, FoldOpts) of
        {ok, {take, _, Entries}} ->
            lists:reverse(Entries);
        {ok, {skip, _, _}} ->
            [];
        {ok, {take_until_start_index, Entries}} ->
            lists:sublist(lists:reverse(lists:sublist(Entries, 1, abs(Offset))), 1, Limit)
    end.
