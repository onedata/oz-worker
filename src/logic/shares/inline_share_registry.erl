%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2026 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Map-based registry of shares, in the scope of a specific space.
%%% Stores the same information about shares as the share_registry,
%%% but serves as an inline structure of the od_space document to
%%% hold shares with better performance than links. Used only
%%% when the share count does not exceed ?MAX_INLINE_REGISTRY_SIZE,
%%% BUT is ALWAYS used to track the total share count, even if they
%%% are held in datastore links.
%%% @end
%%%-------------------------------------------------------------------
-module(inline_share_registry).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").


-export([empty/0, post_upgrade_from_25_0/0]).
-export([can_fit/1, is_active/1, requires_reorganization/1]).
-export([get_share_count/1, adjust_share_count/2]).
-export([initialize_with/2, mark_migrated_to_db_links/2]).
-export([add_link/3, delete_link/2]).
-export([list_links/2]).

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type link_key() :: share_registry:link_key().
-type link_value() :: share_registry:link_value().


-record(inline_share_registry, {
    % indicates if the registry has been properly initialized:
    %   * empty registry created for a new space is considered initialized
    %   * a registry that has been added to an existing space (during upgrade) requires reorganization
    state = requires_reorganization :: initialized | requires_reorganization,
    % always tracks the share count for the space, even if entries are held in datastore links
    count = 0 :: non_neg_integer(),
    % empty if the share count exceeds ?MAX_INLINE_REGISTRY_SIZE
    entries = #{} :: #{link_key() => link_value()}
}).
-type record() :: #inline_share_registry{}.
-export_type([record/0]).


-define(CTX, (od_share:get_ctx())).

-define(MAX_INLINE_REGISTRY_SIZE, oz_worker:get_env(max_inline_share_registry_size, 1000)).


%%%===================================================================
%%% API
%%%===================================================================


-spec empty() -> record().
empty() ->
    #inline_share_registry{
        state = initialized
    }.


%
-spec post_upgrade_from_25_0() -> record().
post_upgrade_from_25_0() ->
    #inline_share_registry{
        state = requires_reorganization
    }.


-spec can_fit(non_neg_integer()) -> boolean().
can_fit(Count) ->
    Count =< ?MAX_INLINE_REGISTRY_SIZE.


%% @doc Says if this inline registry is used to store shares; if the
%% share count exceeds the limit, all the links are migrated to DB link docs.
-spec is_active(record()) -> boolean().
is_active(#inline_share_registry{state = requires_reorganization}) ->
    false;
is_active(#inline_share_registry{count = 0}) ->
    true;
is_active(#inline_share_registry{entries = Entries}) ->
    maps:size(Entries) > 0.


-spec requires_reorganization(record()) -> boolean().
requires_reorganization(#inline_share_registry{state = requires_reorganization}) ->
    true;
requires_reorganization(_) ->
    false.


% NOTE: always holds the actual value, even if the space is using DB links
-spec get_share_count(record()) -> non_neg_integer().
get_share_count(#inline_share_registry{count = Count}) ->
    Count.


% NOTE: should be used ONLY to track changes in shares stored outside the
%       inline registry (DB links);
%       the count is updated automatically if the inline registry is active
-spec adjust_share_count(record(), integer()) -> record().
adjust_share_count(Registry = #inline_share_registry{count = Count}, Adjustment) ->
    Registry#inline_share_registry{
        count = Count + Adjustment
    }.


-spec initialize_with(record(), [{link_key(), link_value()}]) -> record().
initialize_with(Registry, Links) ->
    InitializedEmptyRegistry = Registry#inline_share_registry{
        state = initialized,
        entries = #{},
        count = 0
    },
    lists:foldl(fun({LinkKey, LinkValue}, AccReg) ->
        add_link(AccReg, LinkKey, LinkValue)
    end, InitializedEmptyRegistry, Links).


-spec mark_migrated_to_db_links(record(), non_neg_integer()) -> record().
mark_migrated_to_db_links(Registry, CurrentCount) ->
    Registry#inline_share_registry{
        entries = #{},
        count = CurrentCount
    }.


-spec add_link(record(), link_key(), link_value()) -> record().
add_link(#inline_share_registry{state = requires_reorganization}, _LinkKey, _LinkValue) ->
    error(requires_reorganization);

add_link(#inline_share_registry{entries = Entries} = Registry, LinkKey, LinkValue) ->
    maps:is_key(LinkKey, Entries) andalso throw(?ERROR_ALREADY_EXISTS),
    NewEntries = maps:put(LinkKey, LinkValue, Entries),
    Registry#inline_share_registry{
        entries = NewEntries,
        count = maps:size(NewEntries)
    }.


-spec delete_link(record(), link_key()) -> record().
delete_link(#inline_share_registry{state = requires_reorganization}, _LinkKey) ->
    error(requires_reorganization);

delete_link(#inline_share_registry{entries = Entries} = Registry, LinkKey) ->
    NewEntries = maps:remove(LinkKey, Entries),
    Registry#inline_share_registry{
        entries = NewEntries,
        count = maps:size(NewEntries)
    }.


% mimics the DB #link{}s in the output
-spec list_links(record(), share_registry:listing_opts()) -> [datastore:link()].
list_links(#inline_share_registry{entries = Entries}, ListingOpts) ->
    Limit = maps:get(limit, ListingOpts),
    Offset = maps:get(offset, ListingOpts, 0),
    StartIndex = maps:get(start_index, ListingOpts, <<>>),
    Inclusive = maps:get(inclusive, ListingOpts, true),

    LinkKeys = lists:sort(maps:keys(Entries)),
    LinkCount = length(LinkKeys),

    AnchorIndex = lists_utils:foldl_while(fun(LinkKey, CurrentIndex) ->
        if
            Inclusive, LinkKey >= StartIndex ->
                {halt, CurrentIndex};
            not Inclusive, LinkKey > StartIndex ->
                {halt, CurrentIndex};
            true ->
                {cont, CurrentIndex + 1}
        end
    end, 1, LinkKeys),

    StartPos = max(1, AnchorIndex + Offset),

    SliceLength = min(Limit, max(0, LinkCount - StartPos + 1)),
    SlicedKeys = lists:sublist(LinkKeys, StartPos, SliceLength),

    lists:map(fun(LinkKey) ->
        #link{
            tree_id = <<"inline">>,
            name = LinkKey,
            target = maps:get(LinkKey, Entries)
        }
    end, SlicedKeys).


%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================


-spec to_json(record()) -> json_utils:json_term().
to_json(#inline_share_registry{
    entries = Entries,
    count = Count,
    state = State
}) ->
    json_utils:encode(#{
        <<"entries">> => Entries,
        <<"count">> => Count,
        <<"state">> => atom_to_binary(State)
    }).


-spec from_json(json_utils:json_term()) -> record().
from_json(EncodedJson) ->
    #{
        <<"entries">> := Entries,
        <<"count">> := Count,
        <<"state">> := State
    } = json_utils:decode(EncodedJson),
    #inline_share_registry{
        entries = Entries,
        count = Count,
        state = binary_to_existing_atom(State)
    }.


%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================


-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, _NestedRecordEncoder) ->
    to_json(Record).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    from_json(RecordJson).
