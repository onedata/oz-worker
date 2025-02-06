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
%%% @end
%%%-------------------------------------------------------------------
-module(share_registry).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([report_created/2, report_name_updated/3, report_deleted/2]).
-export([report_handle_created_for/4, report_handle_deleted_for/2]).
-export([index_of/2]).
-export([list_ids/2]).
-export([list_entries/2]).
-export([foreach/2]).

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
    offset => offset()
}.
%% @formatter:on


-export_type([share_entry/0, link_key/0, offset/0, limit/0, listing_opts/0]).


-define(CTX, (od_share:get_ctx())).

-define(FOREST, <<"share-forest">>).
-define(TREE_FOR_SPACE(SpaceId), <<"shares-of-", SpaceId/binary>>).

-define(FOREACH_BATCH_SIZE, 1000).

% Uses NULL char for separator to ensure alphabetical sorting
-define(SEP, 0).

%%%===================================================================
%%% API
%%%===================================================================

-spec report_created(od_share:id(), od_share:record()) -> ok.
report_created(ShareId, ShareRecord = #od_share{handle = undefined}) ->
    add_entry(ShareId, ShareRecord, undefined).


%% @doc NOTE: non-thread-safe, must not be run in parallel with itself or other non-thread-safe functions!
-spec report_name_updated(od_share:id(), od_share:record(), od_share:name()) -> ok.
report_name_updated(ShareId, PreviousShareRecord, NewShareName) ->
    EntryIndex = pack_link_key(ShareId, PreviousShareRecord),

    [PreviousPublicHandle] = list_internal(
        PreviousShareRecord#od_share.space,
        #{start_index => EntryIndex, limit => 1},
        fun(#link{target = LinkValue}) ->
            {_ShareName, _RootFileType, _RootFileUuid, _HandleId, PublicHandle} = unpack_link_value(LinkValue),
            PublicHandle
        end
    ),

    delete_entry(ShareId, PreviousShareRecord),
    add_entry(ShareId, PreviousShareRecord#od_share{name = NewShareName}, PreviousPublicHandle).


%% @doc NOTE: non-thread-safe, must not be run in parallel with itself or other non-thread-safe functions!
-spec report_deleted(od_share:id(), od_share:record()) -> ok.
report_deleted(ShareId, ShareRecord = #od_share{handle = undefined}) ->
    delete_entry(ShareId, ShareRecord).


%% @doc NOTE: non-thread-safe, must not be run in parallel with itself or other non-thread-safe functions!
-spec report_handle_created_for(od_share:id(), od_share:record(), od_handle:id(), od_handle:public_handle()) -> ok.
report_handle_created_for(
    ShareId,
    PreviousShareRecord = #od_share{handle = undefined},
    HandleId,
    PublicHandle
) ->
    delete_entry(ShareId, PreviousShareRecord),
    add_entry(ShareId, PreviousShareRecord#od_share{handle = HandleId}, PublicHandle).


%% @doc NOTE: non-thread-safe, must not be run in parallel with itself or other non-thread-safe functions!
-spec report_handle_deleted_for(od_share:id(), od_share:record()) -> ok.
report_handle_deleted_for(ShareId, PreviousShareRecord) when PreviousShareRecord#od_share.handle /= undefined ->
    delete_entry(ShareId, PreviousShareRecord),
    add_entry(ShareId, PreviousShareRecord#od_share{handle = undefined}, undefined).


-spec index_of(od_share:id(), od_share:record()) -> link_key().
index_of(ShareId, ShareRecord) ->
    pack_link_key(ShareId, ShareRecord).


-spec list_ids(od_space:id(), listing_opts()) -> [od_share:id()].
list_ids(SpaceId, ListingOpts) ->
    list_internal(SpaceId, ListingOpts, fun(#link{name = LinkKey}) ->
        link_key_to_share_id(LinkKey)
    end).


-spec list_entries(od_space:id(), listing_opts()) -> [share_entry()].
list_entries(SpaceId, ListingOpts) ->
    list_internal(SpaceId, ListingOpts, fun(#link{name = LinkKey, target = LinkValue}) ->
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


-spec foreach(od_space:id(), fun((od_share:id()) -> ok)) -> ok.
foreach(SpaceId, ForeachFun) ->
    foreach_internal(SpaceId, ForeachFun, <<"">>).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec foreach_internal(od_space:id(), fun((od_share:id()) -> ok), link_key()) -> ok.
foreach_internal(SpaceId, ForeachFun, StartAfterIndex) ->
    FoldOpts = #{
        size => ?FOREACH_BATCH_SIZE,
        prev_link_name => StartAfterIndex,
        prev_tree_id => ?TREE_FOR_SPACE(SpaceId),  % necessary for inclusive => false to work
        inclusive => false
    },

    FoldFun = fun(Link, Acc) -> {ok, [Link#link.name | Acc]} end,
    {ok, ReversedLinkKeys} = datastore_model:fold_links(
        ?CTX, ?FOREST, ?TREE_FOR_SPACE(SpaceId), FoldFun, [], FoldOpts
    ),

    ShareIds = lists:map(fun link_key_to_share_id/1, ReversedLinkKeys),
    lists:foreach(ForeachFun, ShareIds),

    case length(ReversedLinkKeys) < ?FOREACH_BATCH_SIZE of
        true ->
            ok;
        false ->
            foreach_internal(SpaceId, ForeachFun, hd(ReversedLinkKeys))
    end.


%% @private
-spec list_internal(
    od_space:id(),
    listing_opts(),
    fun((datastore_links:link()) -> Element)
) ->
    [Element] when Element :: od_share:id() | share_entry().
list_internal(SpaceId, ListingOpts, MapLinkFun) ->
    FoldOpts = #{
        size => limit_to_fold_size(maps:get(limit, ListingOpts)),
        offset => maps:get(offset, ListingOpts, 0),
        prev_link_name => maps:get(start_index, ListingOpts, <<>>)
    },

    FoldFun = fun(Link, Acc) ->
        {ok, [MapLinkFun(Link) | Acc]}
    end,

    {ok, InternalEntries} = datastore_model:fold_links(
        ?CTX, ?FOREST, ?TREE_FOR_SPACE(SpaceId), FoldFun, [], FoldOpts
    ),
    lists:reverse(InternalEntries).


%% @private
-spec add_entry(od_share:id(), od_share:record(), od_handle:public_handle() | undefined) -> ok.
add_entry(ShareId, #od_share{space = SpaceId} = ShareRecord, PublicHandle) ->
    Link = {
        pack_link_key(ShareId, ShareRecord),
        pack_link_value(ShareRecord, PublicHandle)
    },
    case datastore_model:add_links(?CTX, ?FOREST, ?TREE_FOR_SPACE(SpaceId), Link) of
        {ok, _} -> ok;
        {error, already_exists} -> throw(?ERROR_ALREADY_EXISTS)
    end.


%% @private
-spec delete_entry(od_share:id(), od_share:record()) -> ok.
delete_entry(ShareId, #od_share{space = SpaceId} = ShareRecord) ->
    LinkKey = pack_link_key(ShareId, ShareRecord),
    case datastore_model:delete_links(?CTX, ?FOREST, ?TREE_FOR_SPACE(SpaceId), LinkKey) of
        ok -> ok;
        {error, not_found} -> ok
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
