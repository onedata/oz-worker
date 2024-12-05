%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating shares of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_shares).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([create/1, create/2, create/3]).
-export([get/1, update/2, delete/1]).
-export([expected_share_entry_index/3]).
-export([gen_shares_for_space/2]).

-compile({no_auto_import, [get/1]}).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(od_space:id()) -> od_share:id().
create(SpaceId) ->
    create(SpaceId, <<"of-space-", SpaceId/binary>>).

-spec create(od_space:id(), od_share:name() | entity_logic:data()) -> od_share:id().
create(SpaceId, Name) when is_binary(Name) ->
    create(SpaceId, #{
        <<"name">> => Name
    });
create(SpaceId, Data) when not is_map_key(<<"shareId">>, Data) ->
    create(SpaceId, Data#{
        <<"shareId">> => datastore_key:new()
    });
create(SpaceId, Data) when not is_map_key(<<"rootFileId">>, Data) ->
    create(SpaceId, Data#{
        <<"rootFileId">> => ?GEN_ROOT_FILE_GUID(SpaceId, maps:get(<<"shareId">>, Data))
    });
create(SpaceId, Data) ->
    create(?ROOT, SpaceId, Data).

-spec create(aai:auth(), od_space:id(), entity_logic:data()) -> od_share:id().
create(Auth, SpaceId, Data) ->
    {ok, ShareId} = ?assertMatch({ok, _}, ozt:rpc(share_logic, create, [Auth, Data#{
        <<"spaceId">> => SpaceId
    }])),
    ShareId.


-spec get(od_share:id()) -> od_share:record().
get(ShareId) ->
    {ok, ShareRecord} = ?assertMatch({ok, _}, ozt:rpc(share_logic, get, [?ROOT, ShareId])),
    ShareRecord.


-spec update(od_share:id(), entity_logic:data()) -> ok.
update(ShareId, Data) ->
    ?assertMatch(ok, ozt:rpc(share_logic, update, [?ROOT, ShareId, Data])).


-spec delete(od_share:id()) -> ok.
delete(ShareId) ->
    ?assertMatch(ok, ozt:rpc(share_logic, delete, [?ROOT, ShareId])).


-spec expected_share_entry_index(od_share:id(), od_share:name(), od_handle:id() | null) -> binary().
expected_share_entry_index(ShareId, ShareName, null) ->
    <<"1", 0, (onedata_file:filename_to_sorting_key(ShareName))/binary, 0, ShareId/binary>>;
expected_share_entry_index(ShareId, ShareName, _) ->
    <<"0", 0, (onedata_file:filename_to_sorting_key(ShareName))/binary, 0, ShareId/binary>>.


-spec gen_shares_for_space(od_share:id(), pos_integer()) -> [share_registry:share_entry()].
gen_shares_for_space(SpaceId, Count) ->
    gen_shares_for_space(SpaceId, Count, ordsets:new(), #{}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc applies random operations (create / modify / delete) on space shares
gen_shares_for_space(_SpaceId, 0, SortedIndices, EntryByIndex) ->
    lists:map(fun(Index) -> maps:get(Index, EntryByIndex) end, SortedIndices);

gen_shares_for_space(SpaceId, OperationsLeft, SortedIndices, EntryByIndex) ->
    RandomOperationIndex = ?RAND_INT(1, 6),

    {NewSortedIndices, NewEntryByIndex} = if

        % with 33.3% change, modify an existing share (if there is any)
        RandomOperationIndex < 3 andalso SortedIndices /= [] ->
            RandomIndex = ?RAND_ELEMENT(SortedIndices),
            PreviousEntry = maps:get(RandomIndex, EntryByIndex),
            NewEntry = #{<<"index">> := NewIndex} = case ?RAND_BOOL() of
                true ->
                    gen_shares_update_name(PreviousEntry);
                false ->
                    gen_shares_toggle_handle_presence(PreviousEntry)
            end,
            {
                ordsets:add_element(NewIndex, ordsets:del_element(RandomIndex, SortedIndices)),
                maps:put(NewIndex, NewEntry, maps:remove(RandomIndex, EntryByIndex))
            };

        % with 16.6% change, delete an existing share (if there is any)
        RandomOperationIndex == 5 andalso SortedIndices /= [] ->
            RandomIndex = ?RAND_ELEMENT(SortedIndices),
            delete(maps:get(<<"shareId">>, maps:get(RandomIndex, EntryByIndex))),
            {ordsets:del_element(RandomIndex, SortedIndices), maps:remove(RandomIndex, EntryByIndex)};

        % with ~50% change, create a new share
        true ->
            #{<<"index">> := Index} = ShareEntry = gen_shares_create_new(SpaceId),
            {ordsets:add_element(Index, SortedIndices), EntryByIndex#{Index => ShareEntry}}

    end,

    gen_shares_for_space(SpaceId, OperationsLeft - 1, NewSortedIndices, NewEntryByIndex).


%% @private
-spec gen_shares_create_new(od_space:id()) -> share_registry:share_entry().
gen_shares_create_new(SpaceId) ->
    Name = ?RAND_SHARE_NAME(),
    FileType = ?RAND_CHOICE(?REGULAR_FILE_TYPE, ?DIRECTORY_TYPE),
    ShareId = datastore_key:new(),
    RootFileUuid = datastore_key:new(),
    RootFileGuid = ?GEN_ROOT_FILE_GUID(RootFileUuid, SpaceId, ShareId),
    create(SpaceId, #{
        <<"shareId">> => ShareId,
        <<"name">> => Name,
        <<"fileType">> => FileType,
        <<"rootFileId">> => RootFileGuid
    }),
    #{
        <<"index">> => expected_share_entry_index(ShareId, Name, null),
        <<"shareId">> => ShareId,
        <<"name">> => Name,
        <<"rootFileType">> => atom_to_binary(FileType),
        <<"rootFilePrivateId">> => file_id:pack_guid(RootFileUuid, SpaceId),
        <<"rootFilePublicId">> => file_id:pack_share_guid(RootFileUuid, SpaceId, ShareId),
        <<"sharePublicUrl">> => str_utils:format_bin("https://~ts/share/~ts", [ozt:get_domain(), ShareId]),
        <<"handleId">> => null,
        <<"handlePublicUrl">> => null
    }.


%% @private
-spec gen_shares_update_name(share_registry:share_entry()) -> share_registry:share_entry().
gen_shares_update_name(PreviousEntry = #{<<"shareId">> := ShareId, <<"handleId">> := HandleId}) ->
    NewName = ?RAND_SHARE_NAME(),
    update(ShareId, #{<<"name">> => NewName}),
    PreviousEntry#{
        <<"index">> => expected_share_entry_index(ShareId, NewName, HandleId),
        <<"name">> => NewName
    }.


%% @private
-spec gen_shares_toggle_handle_presence(share_registry:share_entry()) -> share_registry:share_entry().
gen_shares_toggle_handle_presence(PreviousEntry = #{<<"shareId">> := ShareId, <<"handleId">> := null}) ->
    HandleId = ozt_handles:create(ozt_handle_services:create(), ShareId),
    #od_handle{public_handle = PublicHandle} = ozt_handles:get(HandleId),
    PreviousEntry#{
        <<"index">> => expected_share_entry_index(ShareId, maps:get(<<"name">>, PreviousEntry), HandleId),
        <<"handleId">> => HandleId,
        <<"handlePublicUrl">> => PublicHandle
    };

gen_shares_toggle_handle_presence(PreviousEntry = #{<<"shareId">> := ShareId, <<"handleId">> := HandleId}) ->
    ozt_handles:delete(HandleId),
    PreviousEntry#{
        <<"index">> => expected_share_entry_index(ShareId, maps:get(<<"name">>, PreviousEntry), null),
        <<"handleId">> => null,
        <<"handlePublicUrl">> => null
    }.

