%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Link-tree-based registry of shares, in the scope of a specific space.
%%% Shares are ordered by {HasHandle, ShareName, ShareId} (shares with a
%%% public data handle come first).
%%% The link value encodes some share details, which is redundant regarding
%%% the od_share record, but allows to list shares with required details
%%% without fetching any od_share record.
%%%
%%% This module handles the decisions where to store the shares:
%%%   * For spaces with low share count, @see inline_share_registry is used
%%%     (stored directly in the space doc).
%%%   * When the inline share registry limit is reached, all the links are
%%%     migrated to DB link documents, and new ones are added there.
%%%   * Similarly, when the share count decreases so they can fit into the
%%%     inline registry, the links are migrated back there.
%%% @end
%%%-------------------------------------------------------------------
-module(share_registry).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").


-export([get_share_count/1]).
-export([report_created/2, report_name_updated/3, report_deleted/2]).
-export([report_handle_created_for/4, report_handle_deleted_for/2]).
-export([index_of/2, find_entry/2]).
-export([list_ids/2, list_ids/3]).
-export([list_entries/2, list_entries/3]).
-export([foreach/3]).
-export([ensure_reorganized/1]).


% A JSON object with the information about a share, @see list_entries/2 for details:
-type share_entry() :: json_utils:json_map().

% link_key() consists of 3 parts:
%  1) has handle - if true "0", otherwise "1"
%                  (to achieve desired sorting, where public data shares come first)
%  2) share name
%  3) share id - to deterministically order shares with the same name
-type link_key() :: binary().

% link_value() encodes the following information:
%  1) root file type - binary() :: atom_to_binary(od_share:file_type())
%  2) root file uuid - binary()
%  3) handle id - binary() | undefined
%  4) public handle (URL) - binary() | undefined
-type link_value() :: binary().

-type offset() :: integer().
-type limit() :: infinity | pos_integer().

%% @formatter:off
-type listing_opts() :: #{
    limit := limit(),
    start_index => link_key(),
    % relevant if start_index provided, decides if the start index
    % should be included as the first entry (if exists); defaults to true
    inclusive => boolean(),
    offset => offset()
}.
%% @formatter:on

-export_type([share_entry/0, link_key/0, link_value/0, offset/0, limit/0, listing_opts/0]).


-define(CTX, (od_share:get_ctx())).

-define(FOREST, <<"share-forest">>).
-define(TREE_FOR_SPACE(SpaceId), <<"shares-of-", SpaceId/binary>>).

-define(FOREACH_BATCH_SIZE, oz_worker:get_env(share_registry_foreach_batch_size, 1000)).

-define(critical_section(SpaceId, Fun), critical_section:run(SpaceId, Fun)).

% Uses NULL char for separator to ensure alphabetical sorting
-define(SEP, 0).


%%%===================================================================
%%% API
%%%===================================================================


-spec get_share_count(od_space:id() | od_space:record()) -> non_neg_integer().
get_share_count(SpaceId) when is_binary(SpaceId) ->
    get_share_count(get_space_record(SpaceId));
get_share_count(#od_space{inline_share_registry = InlineRegistry}) ->
    inline_share_registry:get_share_count(InlineRegistry).


-spec report_created(od_share:id(), od_share:record()) -> ok.
report_created(ShareId, ShareRecord = #od_share{handle = undefined}) ->
    add_entry(ShareId, ShareRecord, undefined).


%% @doc NOTE: non-thread-safe, must not be run in parallel with itself or other non-thread-safe functions!
-spec report_name_updated(od_share:id(), od_share:record(), od_share:name()) -> ok.
report_name_updated(ShareId, PrevShareRecord = #od_share{space = SpaceId}, NewShareName) ->
    EntryIndex = pack_link_key(ShareId, PrevShareRecord),

    [PrevPublicHandle] = list_internal(
        SpaceId,
        get_space_record(SpaceId),
        #{start_index => EntryIndex, limit => 1},
        fun(#link{target = LinkValue}) ->
            {_ShareName, _RootFileType, _RootFileUuid, _HandleId, PublicHandle} = unpack_link_value(LinkValue),
            PublicHandle
        end
    ),

    replace_entry(ShareId, PrevShareRecord, PrevShareRecord#od_share{name = NewShareName}, PrevPublicHandle).


%% @doc NOTE: non-thread-safe, must not be run in parallel with itself or other non-thread-safe functions!
-spec report_deleted(od_share:id(), od_share:record()) -> ok.
report_deleted(ShareId, ShareRecord = #od_share{handle = undefined}) ->
    delete_entry(ShareId, ShareRecord).


%% @doc NOTE: non-thread-safe, must not be run in parallel with itself or other non-thread-safe functions!
-spec report_handle_created_for(od_share:id(), od_share:record(), od_handle:id(), od_handle:public_handle()) -> ok.
report_handle_created_for(
    ShareId,
    PrevShareRecord = #od_share{handle = undefined},
    HandleId,
    PublicHandle
) ->
    replace_entry(ShareId, PrevShareRecord, PrevShareRecord#od_share{handle = HandleId}, PublicHandle).


%% @doc NOTE: non-thread-safe, must not be run in parallel with itself or other non-thread-safe functions!
-spec report_handle_deleted_for(od_share:id(), od_share:record()) -> ok.
report_handle_deleted_for(ShareId, PrevShareRecord) when PrevShareRecord#od_share.handle /= undefined ->
    replace_entry(ShareId, PrevShareRecord, PrevShareRecord#od_share{handle = undefined}, undefined).


-spec index_of(od_share:id(), od_share:record()) -> link_key().
index_of(ShareId, ShareRecord) ->
    pack_link_key(ShareId, ShareRecord).


-spec find_entry(od_share:id(), od_share:record()) -> error | {ok, share_entry()}.
find_entry(ShareId, ShareRecord = #od_share{space = SpaceId}) ->
    Index = index_of(ShareId, ShareRecord) ,
    case list_entries(SpaceId, #{start_index => Index, limit => 1}) of
        [#{<<"index">> := Index} = ShareEntry] ->
            {ok, ShareEntry};
        _ ->
            error
    end.


-spec list_ids(od_space:id(), listing_opts()) -> [od_share:id()].
list_ids(SpaceId, ListingOpts) ->
    list_ids(SpaceId, get_space_record(SpaceId), ListingOpts).

-spec list_ids(od_space:id(), od_space:record(), listing_opts()) -> [od_share:id()].
list_ids(SpaceId, SpaceRecord, ListingOpts) ->
    list_internal(SpaceId, SpaceRecord, ListingOpts, fun(#link{name = LinkKey}) ->
        link_key_to_share_id(LinkKey)
    end).


-spec list_entries(od_space:id(), listing_opts()) -> [share_entry()].
list_entries(SpaceId, ListingOpts) ->
    list_entries(SpaceId, get_space_record(SpaceId), ListingOpts).

-spec list_entries(od_space:id(), od_space:record(), listing_opts()) -> [share_entry()].
list_entries(SpaceId, SpaceRecord, ListingOpts) ->
    list_internal(SpaceId, SpaceRecord, ListingOpts, fun(#link{name = LinkKey, target = LinkValue}) ->
        ShareId = link_key_to_share_id(LinkKey),
        {ShareName, RootFileType, RootFileUuid, HandleId, PublicHandle} = unpack_link_value(LinkValue),
        #{
            <<"index">> => LinkKey,
            <<"shareId">> => ShareId,
            <<"name">> => ShareName,
            <<"rootFileType">> => RootFileType,
            <<"rootFilePrivateId">> => file_id:pack_guid(RootFileUuid, SpaceId),
            <<"rootFilePublicId">> => file_id:pack_share_guid(RootFileUuid, SpaceId, ShareId),
            <<"sharePublicUrl">> => od_share:build_public_url(ShareId),
            <<"handleId">> => utils:undefined_to_null(HandleId),
            <<"handlePublicUrl">> => utils:undefined_to_null(PublicHandle)
        }
    end).


-spec foreach(od_space:id(), od_space:record(), fun((od_share:id()) -> ok)) -> ok.
foreach(SpaceId, SpaceRecord, ForeachFun) ->
    foreach_internal(SpaceId, SpaceRecord, ForeachFun, <<"">>).


-spec ensure_reorganized(od_space:id()) -> inline_share_registry:record().
ensure_reorganized(SpaceId) ->
    ?critical_section(SpaceId, fun() ->
        SpaceRecord = #od_space{inline_share_registry = InlineRegistry} = get_space_record(SpaceId),
        AllLinks = list_all_links(SpaceId, SpaceRecord),
        ShareCount = length(AllLinks),
        case {
            inline_share_registry:requires_reorganization(InlineRegistry),
            inline_share_registry:can_fit(ShareCount)
        } of
            {false, _} ->
                InlineRegistry;

            {true, true} ->
                UpdatedRegistry = update_inline_registry(SpaceId, fun(IR) ->
                    inline_share_registry:initialize_with(IR, AllLinks)
                end),
                delete_datastore_links(SpaceId, AllLinks),
                UpdatedRegistry;

            {true, false} ->
                add_datastore_links(SpaceId, AllLinks, tolerate_existing),
                update_inline_registry(SpaceId, fun(IR) ->
                    UpdatedRegistry = inline_share_registry:initialize_with(IR, []),
                    inline_share_registry:adjust_share_count(UpdatedRegistry, ShareCount)
                end)
        end
    end).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec foreach_internal(od_space:id(), od_space:record(), fun((od_share:id()) -> ok), link_key()) -> ok.
foreach_internal(SpaceId, SpaceRecord, ForeachFun, StartAfterIndex) ->
    Entries = list_entries(SpaceId, SpaceRecord, #{
        limit => ?FOREACH_BATCH_SIZE,
        start_index => StartAfterIndex,
        inclusive => false
    }),
    ShareIds = [ShareId || #{<<"shareId">> := ShareId} <- Entries],

    lists:foreach(ForeachFun, ShareIds),

    case length(ShareIds) < ?FOREACH_BATCH_SIZE of
        true ->
            ok;
        false ->
            NewStartAfterIndex = maps:get(<<"index">>, lists:last(Entries)),
            foreach_internal(SpaceId, SpaceRecord, ForeachFun, NewStartAfterIndex)
    end.


%% @private
-spec list_internal(
    od_space:id(),
    od_space:record(),
    listing_opts(),
    fun((datastore_links:link()) -> Element)
) ->
    [Element].
list_internal(SpaceId, SpaceRecord, ListingOpts, MapLinkFun) ->
    InlineRegistry = SpaceRecord#od_space.inline_share_registry,

    case inline_share_registry:is_active(InlineRegistry) of
        true ->
            Links = inline_share_registry:list_links(InlineRegistry, ListingOpts),
            lists:map(MapLinkFun, Links);
        false ->
            BaseFoldOpts = #{
                size => limit_to_fold_size(maps:get(limit, ListingOpts)),
                offset => maps:get(offset, ListingOpts, 0),
                prev_link_name => maps:get(start_index, ListingOpts, <<>>),
                inclusive => maps:get(inclusive, ListingOpts, true)
            },
            FoldOpts = case BaseFoldOpts of
                #{inclusive := true} ->
                    BaseFoldOpts;
                #{inclusive := false} ->
                    BaseFoldOpts#{prev_tree_id => ?TREE_FOR_SPACE(SpaceId)}  % necessary for inclusive => false to work
            end,

            FoldFun = fun(Link, Acc) ->
                {ok, [MapLinkFun(Link) | Acc]}
            end,

            {ok, ReversedItems} = datastore_model:fold_links(
                ?CTX, ?FOREST, ?TREE_FOR_SPACE(SpaceId), FoldFun, [], FoldOpts
            ),
            lists:reverse(ReversedItems)
    end.


%% @private
-spec list_all_links(od_space:id(), od_space:record()) -> [{link_key(), link_value()}].
list_all_links(SpaceId, SpaceRecord) ->
    list_internal(
        SpaceId,
        SpaceRecord,
        #{limit => infinity},
        fun(#link{name = Key, target = Value}) -> {Key, Value} end
    ).


%% @private
-spec add_entry(od_share:id(), od_share:record(), od_handle:public_handle() | undefined) -> ok.
add_entry(ShareId, #od_share{space = SpaceId} = ShareRecord, PublicHandle) ->
    LinkKey = pack_link_key(ShareId, ShareRecord),
    LinkValue = pack_link_value(ShareRecord, PublicHandle),
    ?critical_section(SpaceId, fun() ->
        #od_space{inline_share_registry = InlineRegistry} = SpaceRecord = get_space_record(SpaceId),
        ShareCount = inline_share_registry:get_share_count(InlineRegistry),

        IsActive = inline_share_registry:is_active(InlineRegistry),
        CanFitBefore = inline_share_registry:can_fit(ShareCount),
        CanFitAfter = inline_share_registry:can_fit(ShareCount + 1),

        case {IsActive, CanFitBefore, CanFitAfter} of
            {true, true, true} ->
                update_inline_registry(SpaceId, fun(IR) ->
                    inline_share_registry:add_link(IR, LinkKey, LinkValue)
                end);

            {false, _, false} ->
                add_datastore_links(SpaceId, [{LinkKey, LinkValue}], ensure_unique),
                update_inline_registry(SpaceId, fun(IR) ->
                    inline_share_registry:adjust_share_count(IR, 1)
                end);

            {false, true, true} ->
                ?info(
                    "The max inline share registry size appears to have been increased, "
                    "migrating shares for space ~ts into the inline registry",
                    [SpaceId]
                ),
                migrate_to_inline_registry_and_apply(SpaceId, SpaceRecord, fun(IR) ->
                    inline_share_registry:add_link(IR, LinkKey, LinkValue)
                end);

            {true, CanFitBefore, false} ->
                CanFitBefore orelse ?info(
                    "The max inline share registry size appears to have been decreased, "
                    "migrating shares for space ~ts into DB links",
                    [SpaceId]
                ),
                AllLinks = list_all_links(SpaceId, SpaceRecord),
                proplists:is_defined(LinkKey, AllLinks) andalso throw(?ERROR_ALREADY_EXISTS),
                migrate_to_db_links(SpaceId, [{LinkKey, LinkValue} | AllLinks]);

            {_, _, _} ->
                throw(?report_internal_server_error(
                    ?autoformat_with_msg("Unexpected share registry state", [
                        SpaceId,
                        IsActive,
                        CanFitAfter,
                        CanFitBefore,
                        InlineRegistry
                    ])
                ))
        end
    end),
    ok.


%% @private
-spec delete_entry(od_share:id(), od_share:record()) -> ok.
delete_entry(ShareId, #od_share{space = SpaceId} = ShareRecord) ->
    LinkKey = pack_link_key(ShareId, ShareRecord),
    ?critical_section(SpaceId, fun() ->
        #od_space{inline_share_registry = InlineRegistry} = SpaceRecord = get_space_record(SpaceId),
        ShareCount = inline_share_registry:get_share_count(InlineRegistry),
        IsActive = inline_share_registry:is_active(InlineRegistry),
        CanFitBefore = inline_share_registry:can_fit(ShareCount),
        CanFitAfter = inline_share_registry:can_fit(ShareCount - 1),

        case {IsActive, CanFitBefore, CanFitAfter} of
            {true, true, true} ->
                update_inline_registry(SpaceId, fun(IR) ->
                    inline_share_registry:delete_link(IR, LinkKey)
                end);

            {false, false, false} ->
                1 = delete_datastore_links(SpaceId, [LinkKey]),
                update_inline_registry(SpaceId, fun(IR) ->
                    inline_share_registry:adjust_share_count(IR, -1)
                end);

            {true, _, false} ->
                ?info(
                    "The max inline share registry size appears to have been decreased, "
                    "reorganizing shares for space ~ts into DB links",
                    [SpaceId]
                ),
                AllLinks = list_all_links(SpaceId, SpaceRecord),
                proplists:is_defined(LinkKey, AllLinks) orelse throw(?ERROR_NOT_FOUND),
                migrate_to_db_links(SpaceId, proplists:delete(LinkKey, AllLinks));

            {false, CanFitBefore, true} ->
                CanFitBefore andalso ?info(
                    "The max inline share registry size appears to have been increased, "
                    "reorganizing shares for space ~ts into the inline registry",
                    [SpaceId]
                ),
                migrate_to_inline_registry_and_apply(SpaceId, SpaceRecord, fun(IR) ->
                    inline_share_registry:delete_link(IR, LinkKey)
                end);

            {_, _, _} ->
                throw(?report_internal_server_error(
                    ?autoformat_with_msg("Unexpected share registry state", [
                        SpaceId,
                        IsActive,
                        CanFitBefore,
                        CanFitAfter,
                        InlineRegistry
                    ])
                ))
        end
    end),
    ok.


%% @private
-spec replace_entry(od_share:id(), od_share:record(), od_share:record(), od_handle:public_handle() | undefined) -> ok.
replace_entry(ShareId, #od_share{space = SpaceId} = PrevShareRecord, NewShareRecord, PublicHandle) ->
    PrevLinkKey = pack_link_key(ShareId, PrevShareRecord),
    NewLinkKey = pack_link_key(ShareId, NewShareRecord),
    NewLinkValue = pack_link_value(NewShareRecord, PublicHandle),
    ?critical_section(SpaceId, fun() ->
        #od_space{inline_share_registry = InlineRegistry} = get_space_record(SpaceId),

        case inline_share_registry:is_active(InlineRegistry) of
            true ->
                update_inline_registry(SpaceId, fun(IR0) ->
                    IR1 = inline_share_registry:delete_link(IR0, PrevLinkKey),
                    inline_share_registry:add_link(IR1, NewLinkKey, NewLinkValue)
                end);

            false ->
                1 = delete_datastore_links(SpaceId, [PrevLinkKey]),
                add_datastore_links(SpaceId, [{NewLinkKey, NewLinkValue}], ensure_unique)
        end
    end),
    ok.


%% @private
-spec update_inline_registry(
    od_space:id(),
    fun((inline_share_registry:record()) -> inline_share_registry:record())
) ->
    inline_share_registry:record().
update_inline_registry(SpaceId, UpdateFun) ->
    Diff = fun(SpaceRecord = #od_space{inline_share_registry = InlineRegistry}) ->
        case ?catch_exceptions(UpdateFun(InlineRegistry)) of
            {error, _} = Error ->
                Error;
            UpdatedRegistry ->
                {ok, SpaceRecord#od_space{inline_share_registry = UpdatedRegistry}}
        end
    end,
    case od_space:update(SpaceId, Diff) of
        {ok, #document{value = #od_space{inline_share_registry = UpdatedRegistry}}} ->
            UpdatedRegistry;
        {error, _} = Error ->
            ?throw_as_od_error(Error)
    end.


%% @private
-spec pack_link_key(od_share:id(), od_share:record()) -> link_key().
pack_link_key(ShareId, #od_share{name = ShareName, handle = HandleId}) ->
    pack_link_key(ShareId, onedata_file:filename_to_sorting_key(ShareName), HandleId).

%% @private
-spec pack_link_key(od_share:id(), binary(), od_handle:id() | undefined) -> link_key().
pack_link_key(ShareId, SortingKey, HandleId) ->
    HandlePart = case HandleId of
        undefined -> <<"1">>;
        _ -> <<"0">>
    end,
    <<HandlePart/binary, ?SEP, SortingKey/binary, ?SEP, ShareId/binary>>.


%% @private
-spec link_key_to_share_id(link_key()) -> od_share:id().
link_key_to_share_id(LinkKey) ->
    [_HandlePart, _SortingKey, ShareId] = binary:split(LinkKey, <<?SEP>>, [global]),
    ShareId.


%% @private
-spec pack_link_value(od_share:record(), od_handle:public_handle() | undefined) ->
    link_value().
pack_link_value(#od_share{
    name = ShareName,
    file_type = FileType,
    root_file_uuid = RootFileUuid,
    handle = HandleId
}, PublicHandle) ->
    pack_link_value(ShareName, atom_to_binary(FileType), RootFileUuid, HandleId, PublicHandle).

%% @private
-spec pack_link_value(
    od_share:name(),
    binary(),  % atom_to_binary(od_share:file_type()),
    file_id:file_uuid(),
    od_handle:id() | undefined,
    od_handle:public_handle() | undefined
) ->
    link_value().
pack_link_value(
    ShareName,
    RootFileTypeBin,
    RootFileUuid,
    HandleId,
    PublicHandle
) ->
    <<
        ShareName/binary, ?SEP,
        RootFileTypeBin/binary, ?SEP,
        RootFileUuid/binary, ?SEP,
        (encode_nullable(HandleId))/binary, ?SEP,
        (encode_nullable(PublicHandle))/binary
    >>.


%% @private
-spec unpack_link_value(link_value()) -> {
    od_share:name(),
    binary(),  % atom_to_binary(od_share:file_type()),
    file_id:file_uuid(),
    od_handle:id() | undefined,
    od_handle:public_handle() | undefined
}.
unpack_link_value(LinkValue) ->
    [
        ShareName,
        RootFileTypeBin,
        RootFileUuid,
        EncHandleId,
        EncPublicHandle
    ] = binary:split(LinkValue, <<?SEP>>, [global]),
    {
        ShareName,
        RootFileTypeBin,
        RootFileUuid,
        decode_nullable(EncHandleId),
        decode_nullable(EncPublicHandle)
    }.


%% @private
-spec encode_nullable(binary() | undefined) -> binary().
encode_nullable(undefined) -> <<"null">>;
encode_nullable(Binary) -> Binary.


%% @private
-spec decode_nullable(binary() | undefined) -> binary().
decode_nullable(<<"null">>) -> undefined;
decode_nullable(Binary) -> Binary.


%% @private
-spec limit_to_fold_size(limit()) -> undefined | non_neg_integer().
limit_to_fold_size(infinity) -> undefined;
limit_to_fold_size(Int) when is_integer(Int) andalso Int > 0 -> Int.


%% @private
-spec get_space_record(od_space:id()) -> od_space:record().
get_space_record(SpaceId) ->
    #document{value = SpaceRecord} = ?check(od_space:get(SpaceId)),
    SpaceRecord.


%% @private
-spec migrate_to_inline_registry_and_apply(
    od_space:id(),
    od_space:record(),
    fun((inline_share_registry:record()) -> inline_share_registry:record())
) ->
    ok.
migrate_to_inline_registry_and_apply(SpaceId, SpaceRecord, WhatToApplyOnIR) ->
    AllLinks = list_all_links(SpaceId, SpaceRecord),
    update_inline_registry(SpaceId, fun(IR) ->
        WhatToApplyOnIR(inline_share_registry:initialize_with(IR, AllLinks))
    end),
    delete_datastore_links(SpaceId, proplists:get_keys(AllLinks)),
    ok.


%% @private
-spec migrate_to_db_links(od_space:id(), [{link_key(), link_value()}]) -> inline_share_registry:record().
migrate_to_db_links(SpaceId, AllLinks) ->
    case catch add_datastore_links(SpaceId, AllLinks, ensure_unique) of
        ok ->
            ok;
        ?ERROR_ALREADY_EXISTS ->
            throw(?report_internal_server_error(
                "Unexpected link conflicts during migration of shares to DB links, space ~ts",
                [SpaceId]
            ))
    end,
    update_inline_registry(SpaceId, fun(IR1) ->
        inline_share_registry:mark_migrated_to_db_links(IR1, length(AllLinks))
    end).


%% @private
-spec add_datastore_links(od_space:id(), [{link_key(), link_value()}], ensure_unique | tolerate_existing) ->
    ok.
add_datastore_links(SpaceId, Links, Strategy) ->
    Results = datastore_model:add_links(?CTX, ?FOREST, ?TREE_FOR_SPACE(SpaceId), Links),
    lists:foreach(fun
        ({ok, _}) ->
            ok;
        ({error, already_exists}) when Strategy == ensure_unique ->
            throw(?ERROR_ALREADY_EXISTS);
        ({error, already_exists}) when Strategy == tolerate_existing ->
            ok;
        (Error) ->
            throw(?report_internal_server_error(?autoformat_with_msg(
                "Failed to add at least one datastore link",
                Error
            )))
    end, Results).


%% @private
-spec delete_datastore_links(od_space:id(), [link_key()]) -> non_neg_integer().
delete_datastore_links(SpaceId, Links) ->
    Results = datastore_model:delete_links(?CTX, ?FOREST, ?TREE_FOR_SPACE(SpaceId), Links),
    lists:foldl(fun
        (ok, Acc) -> Acc + 1;
        ({error, not_found}, Acc) -> Acc
    end, 0, Results).
