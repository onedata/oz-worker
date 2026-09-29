%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%%-------------------------------------------------------------------
-module(share_registry_test_SUITE).

-include_lib("ctool/include/test/test_utils.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("api_test_utils.hrl").


-export([
    groups/0, all/0,
    init_per_suite/1, end_per_suite/1
]).


-export([
    % parallel
    basic_create_and_list_test/1,
    ordering_test/1,
    get_share_count_test/1,
    index_of_test/1,
    rename_test/1,
    handle_attach_detach_test/1,
    delete_test/1,
    switch_between_inline_and_db_links_test/1,
    duplicate_add_should_fail_test/1,
    change_max_registry_size_during_lifecycle_test/1,
    foreach_test/1,
    list_test/1
]).


%%%===================================================================
%%% Groups
%%%===================================================================

groups() -> [
    {parallel, [parallel], [
        basic_create_and_list_test,
        ordering_test,
        get_share_count_test,
        index_of_test,
        rename_test,
        handle_attach_detach_test,
        delete_test,
        switch_between_inline_and_db_links_test,
        duplicate_add_should_fail_test,
        change_max_registry_size_during_lifecycle_test,
        foreach_test,
        list_test
    ]}
].


all() -> [
    {group, parallel}
].


-define(MAX_INLINE_SHARE_REGISTRY_SIZE, 50).
-define(FOREACH_BATCH_SIZE, 1000).


-record(share_info, {
    id :: binary(),
    record :: od_share:record()
}).

-define(ids(ShareInfos), [S#share_info.id || S <- ShareInfos]).


%%%===================================================================
%%% Tests
%%%===================================================================


basic_create_and_list_test(_) ->
    SpaceId = ozt_spaces:create(),

    S1 = create_share(SpaceId, <<"a">>, false),
    S2 = create_share(SpaceId, <<"b">>, false),

    ?assertEqual(?ids([S1, S2]), list_all_ids(SpaceId)).


ordering_test(_) ->
    SpaceId = ozt_spaces:create(),

    S1 = create_share(SpaceId, <<"b">>, false),
    S2 = create_share(SpaceId, <<"a">>, false),
    S3 = create_share(SpaceId, <<"c">>, true),  % should come first as it has a handle

    ?assertEqual(?ids([S3, S2, S1]), list_all_ids(SpaceId)).


get_share_count_test(_) ->
    SpaceId = ozt_spaces:create(),

    CheckShareCount = fun(Expected) ->
        SpaceRecord = ozt_spaces:get(SpaceId),
        CountAsExpected =
            true
                andalso Expected == ?rpc(share_registry:get_share_count(SpaceId))
                andalso Expected == ?rpc(share_registry:get_share_count(SpaceRecord))
                andalso Expected == length(list_all_ids(SpaceId))
                andalso Expected == length(list_all_entries(SpaceId)),
        case CountAsExpected of
            true -> true;
            false -> {false, length(list_all_ids(SpaceId))}
        end
    end,

    ?assert(CheckShareCount(0)),

    Shares1 = create_shares(SpaceId, ?MAX_INLINE_SHARE_REGISTRY_SIZE - 1),
        ?assert(CheckShareCount(?MAX_INLINE_SHARE_REGISTRY_SIZE - 1)),

    Shares2 = Shares1 ++ create_shares(SpaceId, 1),
    ?assert(CheckShareCount(?MAX_INLINE_SHARE_REGISTRY_SIZE)),

    Shares3 = Shares2 -- delete_n_random_shares(Shares2, 1),
    ?assert(CheckShareCount(?MAX_INLINE_SHARE_REGISTRY_SIZE - 1)),

    Shares4 = Shares3 ++ create_shares(SpaceId, 7),
    ?assert(CheckShareCount(?MAX_INLINE_SHARE_REGISTRY_SIZE + 6)),

    Shares5 = Shares4 -- delete_n_random_shares(Shares4, ?MAX_INLINE_SHARE_REGISTRY_SIZE),
    ?assert(CheckShareCount(6)),

    Shares6 = Shares5 ++ create_shares(SpaceId, 7 * ?MAX_INLINE_SHARE_REGISTRY_SIZE),
    ?assert(CheckShareCount(7 * ?MAX_INLINE_SHARE_REGISTRY_SIZE + 6)),

    delete_n_random_shares(Shares6, 7 * ?MAX_INLINE_SHARE_REGISTRY_SIZE + 6),
    ?assert(CheckShareCount(0)).


index_of_test(_) ->
    SpaceId = ozt_spaces:create(),
    index_of_test_base(SpaceId),

    create_shares(SpaceId, 1),
    index_of_test_base(SpaceId),

    create_shares(SpaceId, ?MAX_INLINE_SHARE_REGISTRY_SIZE div 2),
    index_of_test_base(SpaceId),

    create_shares(SpaceId, ?MAX_INLINE_SHARE_REGISTRY_SIZE),
    index_of_test_base(SpaceId),

    create_shares(SpaceId, ?MAX_INLINE_SHARE_REGISTRY_SIZE * 2),
    index_of_test_base(SpaceId).

index_of_test_base(SpaceId) ->
    #share_info{id = ShareId, record = ShareRecord} = create_share(SpaceId, ?RAND_SHARE_NAME(), ?RAND_BOOL()),

    Index = ?rpc(share_registry:index_of(ShareId, ShareRecord)),

    ?assertEqual([ShareId], ?rpc(share_registry:list_ids(SpaceId, #{
        start_index => Index,
        limit => 1
    }))).


rename_test(_) ->
    rename_test_base(true),
    rename_test_base(false).

rename_test_base(HasHandle) ->
    SpaceId = ozt_spaces:create(),
    create_shares(SpaceId, ?RAND_INT(?MAX_INLINE_SHARE_REGISTRY_SIZE div 2, ?MAX_INLINE_SHARE_REGISTRY_SIZE * 2)),

    #share_info{id = ShareId, record = ShareRecord} = create_share(SpaceId, <<"old">>, HasHandle),
    PreviousShareData = ?check(?rpc(share_registry:find_entry(ShareId, ShareRecord))),

    ok = ?rpc(share_registry:report_name_updated(ShareId, ShareRecord, <<"new">>)),

    ?assertMatch(
        #{<<"name">> := <<"new">>},
        ?check(?rpc(share_registry:find_entry(ShareId, ShareRecord#od_share{name = <<"new">>})))
    ),
    ?assertEqual(
        maps:without([<<"name">>, <<"index">>], PreviousShareData),
        maps:without([<<"name">>, <<"index">>], ?check(?rpc(share_registry:find_entry(ShareId, ShareRecord#od_share{name = <<"new">>}))))
    ).


handle_attach_detach_test(_) ->
    SpaceId = ozt_spaces:create(),
    create_shares(SpaceId, ?RAND_INT(?MAX_INLINE_SHARE_REGISTRY_SIZE div 2, ?MAX_INLINE_SHARE_REGISTRY_SIZE * 2)),

    #share_info{id = ShareId, record = ShareRecord} = create_share(SpaceId, <<"x">>, false),
    PreviousShareData = ?check(?rpc(share_registry:find_entry(ShareId, ShareRecord))),

    HandleId = datastore_key:new(),
    ok = ?rpc(share_registry:report_handle_created_for(ShareId, ShareRecord, HandleId, <<"url">>)),
    ?assertMatch(
        #{<<"handleId">> := HandleId},
        ?check(?rpc(share_registry:find_entry(ShareId, ShareRecord#od_share{handle = HandleId})))
    ),
    ?assertEqual(
        maps:without([<<"handleId">>, <<"handlePublicUrl">>, <<"index">>],
            PreviousShareData
        ),
        maps:without([<<"handleId">>, <<"handlePublicUrl">>, <<"index">>],
            ?check(?rpc(share_registry:find_entry(ShareId, ShareRecord#od_share{handle = HandleId})))
        )
    ),

    ok = ?rpc(share_registry:report_handle_deleted_for(ShareId, ShareRecord#od_share{handle = HandleId})),
    ?assertMatch(
        #{<<"handleId">> := null},
        ?check(?rpc(share_registry:find_entry(ShareId, ShareRecord#od_share{handle = undefined})))
    ),
    ?assertEqual(
        PreviousShareData,
        ?check(?rpc(share_registry:find_entry(ShareId, ShareRecord#od_share{handle = undefined})))
    ).


delete_test(_) ->
    SpaceId = ozt_spaces:create(),
    create_shares(SpaceId, ?RAND_INT(?MAX_INLINE_SHARE_REGISTRY_SIZE div 2, ?MAX_INLINE_SHARE_REGISTRY_SIZE * 2)),

    #share_info{id = ShareId, record = ShareRecord} = create_share(SpaceId, <<"a">>, false),

    ?assertEqual(ok, ?rpc(share_registry:report_deleted(ShareId, ShareRecord))),

    ?assertNot(lists:member(ShareId, list_all_ids(SpaceId))).


switch_between_inline_and_db_links_test(_) ->
    SpaceId = ozt_spaces:create(),
    InitialShares = create_shares(SpaceId, ?MAX_INLINE_SHARE_REGISTRY_SIZE),

    lists:foldl(fun(Diff, CurrentShares) ->
        UpdatedShares = case Diff > 0 of
            true ->
                CurrentShares ++ create_shares(SpaceId, Diff);
            false ->
                CurrentShares -- delete_n_random_shares(CurrentShares, -Diff)
        end,
        ?assertEqual(length(UpdatedShares), ?rpc(share_registry:get_share_count(SpaceId))),
        ?assertEqual(lists:sort(?ids(UpdatedShares)), lists:sort(list_all_ids(SpaceId))),
        UpdatedShares
    end, InitialShares, [
        + 1,
        -1,
        + 9,
        -9,
        + ?MAX_INLINE_SHARE_REGISTRY_SIZE,
        -?MAX_INLINE_SHARE_REGISTRY_SIZE,
        + ?MAX_INLINE_SHARE_REGISTRY_SIZE * 2,
        -?MAX_INLINE_SHARE_REGISTRY_SIZE div 2,
        -?MAX_INLINE_SHARE_REGISTRY_SIZE div 2,
        -?MAX_INLINE_SHARE_REGISTRY_SIZE div 2,
        -?MAX_INLINE_SHARE_REGISTRY_SIZE div 2
    ]).


duplicate_add_should_fail_test(_) ->
    SpaceId = ozt_spaces:create(),
    duplicate_add_should_fail_test_base(SpaceId),

    create_shares(SpaceId, ?RAND_INT(0, ?MAX_INLINE_SHARE_REGISTRY_SIZE * 2)),
    duplicate_add_should_fail_test_base(SpaceId),

    create_shares(SpaceId, ?MAX_INLINE_SHARE_REGISTRY_SIZE),
    duplicate_add_should_fail_test_base(SpaceId).

duplicate_add_should_fail_test_base(SpaceId) ->
    #share_info{id = ShareId, record = ShareRecord} = create_share(SpaceId, ?RAND_SHARE_NAME(), false),

    % check on the inline registry
    ?assertThrow(?ERROR_ALREADY_EXISTS, ?rpc(share_registry:report_created(ShareId, ShareRecord))),

    % force migration to DB links and check there
    create_shares(SpaceId, ?MAX_INLINE_SHARE_REGISTRY_SIZE * 3),
    ?assertThrow(?ERROR_ALREADY_EXISTS, ?rpc(share_registry:report_created(ShareId, ShareRecord))).


change_max_registry_size_during_lifecycle_test(_) ->
    SpaceId = ozt_spaces:create(),
    ozt:set_env(max_inline_share_registry_size, 100),
    Shares1 = create_shares(SpaceId, 50),
    ?assertEqual(50, length(list_all_ids(SpaceId))),
    ?assertEqual(inline_registry, check_share_storage_type(SpaceId)),

    ozt:set_env(max_inline_share_registry_size, 30),
    Shares2 = Shares1 ++ create_shares(SpaceId, 3),
    ?assertEqual(53, length(list_all_ids(SpaceId))),
    ?assertEqual(db_links, check_share_storage_type(SpaceId)),

    ozt:set_env(max_inline_share_registry_size, 100),
    Shares3 = Shares2 ++ create_shares(SpaceId, 3),
    ?assertEqual(56, length(list_all_ids(SpaceId))),
    ?assertEqual(inline_registry, check_share_storage_type(SpaceId)),

    Shares4 = Shares3 ++ create_shares(SpaceId, 45),
    ?assertEqual(101, length(list_all_ids(SpaceId))),
    ?assertEqual(db_links, check_share_storage_type(SpaceId)),

    ozt:set_env(max_inline_share_registry_size, 200),
    Shares5 = Shares4 ++ create_shares(SpaceId, 2),
    ?assertEqual(103, length(list_all_ids(SpaceId))),
    ?assertEqual(inline_registry, check_share_storage_type(SpaceId)),

    ozt:set_env(max_inline_share_registry_size, 50),
    Shares6 = Shares5 -- delete_n_random_shares(Shares5, 13),
    ?assertEqual(90, length(list_all_ids(SpaceId))),
    ?assertEqual(db_links, check_share_storage_type(SpaceId)),

    ozt:set_env(max_inline_share_registry_size, 100),
    delete_n_random_shares(Shares6, 1),
    ?assertEqual(89, length(list_all_ids(SpaceId))),
    ?assertEqual(inline_registry, check_share_storage_type(SpaceId)).


foreach_test(_) ->
    foreach_test_base(0),
    foreach_test_base(1),
    foreach_test_base(?MAX_INLINE_SHARE_REGISTRY_SIZE div 2),
    foreach_test_base(?MAX_INLINE_SHARE_REGISTRY_SIZE),
    foreach_test_base(?FOREACH_BATCH_SIZE),
    foreach_test_base(?FOREACH_BATCH_SIZE * 3).

foreach_test_base(ShareCount) ->
    SpaceId = ozt_spaces:create(),

    Shares = create_shares(SpaceId, ShareCount),
    SpaceRecord = ozt_spaces:get(SpaceId),

    % make sure the file is always there and erased, even if the ShareCount is 0
    utils:rpc_multicall(?rpc(consistent_hashing:get_all_nodes()), file, write_file, [
        "/tmp/shares.txt", <<"">>
    ]),

    ?rpc(share_registry:foreach(SpaceId, SpaceRecord, fun(ShareId) ->
        utils:rpc_multicall(consistent_hashing:get_all_nodes(), file, write_file, [
            "/tmp/shares.txt", <<ShareId/binary, "\n">>, [append]
        ])
    end)),

    {ok, FileContent} = ?rpc(file:read_file("/tmp/shares.txt")),
    ?assertEqual(lists:sort(?ids(Shares)), lists:sort(binary:split(FileContent, <<"\n">>, [global, trim_all]))).


list_test(_) ->
    SpaceId = ozt_spaces:create(),
    Count = 2222,
    create_shares(SpaceId, Count),

    utils:repeat(20, fun() ->
        ozt:set_env(max_inline_share_registry_size, Count div 5),
        adjust_for_current_max_inline_registry_size(SpaceId),

        RandListingOpts = randomize_listing_opts(Count),
        IdListingResult = ?rpc(share_registry:list_ids(SpaceId, RandListingOpts)),
        EntryListingResult = ?rpc(share_registry:list_entries(SpaceId, RandListingOpts)),

        ozt:set_env(max_inline_share_registry_size, Count + ?RAND_INT(0, 1)),
        adjust_for_current_max_inline_registry_size(SpaceId),

        ?assertEqual(IdListingResult, ?rpc(share_registry:list_ids(SpaceId, RandListingOpts))),
        ?assertEqual(EntryListingResult, ?rpc(share_registry:list_entries(SpaceId, RandListingOpts)))
    end).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ozt:init_per_suite(Config, fun() ->
        ozt:set_env(max_inline_share_registry_size, ?MAX_INLINE_SHARE_REGISTRY_SIZE),
        ozt:set_env(share_registry_foreach_batch_size, ?FOREACH_BATCH_SIZE)
    end).


end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================


create_share(SpaceId, Name, HasHandle) ->
    ShareId = datastore_key:new(),

    BaseShareRecord = #od_share{
        name = Name,
        space = SpaceId,
        root_file_uuid = datastore_key:new(),
        file_type = ?REGULAR_FILE_TYPE
    },

    ?rpc(share_registry:report_created(ShareId, BaseShareRecord)),

    #share_info{
        id = ShareId,
        record = case HasHandle of
            false ->
                BaseShareRecord;
            true ->
                HandleId = datastore_key:new(),
                ?rpc(share_registry:report_handle_created_for(
                    ShareId, BaseShareRecord, HandleId, <<"url">>
                )),
                BaseShareRecord#od_share{handle = HandleId}
        end
    }.


create_shares(SpaceId, Count) ->
    lists_utils:generate(fun() ->
        create_share(SpaceId, ?RAND_SHARE_NAME(), ?RAND_BOOL())
    end, Count).


delete_share(#share_info{id = ShareId, record = #od_share{handle = undefined} = ShareRecord} = ShareInfo) ->
    ok = ?rpc(share_registry:report_deleted(ShareId, ShareRecord)),
    ShareInfo;
delete_share(#share_info{id = ShareId, record = ShareRecord} = ShareInfo) ->
    ok = ?rpc(share_registry:report_handle_deleted_for(ShareId, ShareRecord)),
    delete_share(ShareInfo#share_info{record = ShareRecord#od_share{handle = undefined}}),
    ShareInfo.


delete_n_random_shares(ShareInfos, Count) ->
    ToDelete = ?RAND_SUBLIST(ShareInfos, Count),
    lists:map(fun delete_share/1, ToDelete).


list_all_ids(SpaceId) ->
    ?rpc(share_registry:list_ids(SpaceId, #{limit => infinity})).


list_all_entries(SpaceId) ->
    ?rpc(share_registry:list_entries(SpaceId, #{limit => infinity})).


check_share_storage_type(SpaceId) ->
    #od_space{inline_share_registry = ISR} = ozt_spaces:get(SpaceId),
    case inline_share_registry:is_active(ISR) of
        true -> inline_registry;
        false -> db_links
    end.


adjust_for_current_max_inline_registry_size(SpaceId) ->
    delete_share(create_share(SpaceId, <<"create+delete will trigger proper mechanisms">>, false)).


randomize_listing_opts(Count) ->
    Opts0 = #{
        limit => case ?RAND_INT(1, 5) of
            1 -> infinity;
            2 -> Count * ?RAND_INT(2, 5);
            _ -> ?RAND_INT(0, Count)
        end
    },
    Opts1 = maps_utils:put_if_defined(Opts0, start_index, case ?RAND_INT(1, 3) of
        1 -> undefined;
        _ -> <<<<(?RAND_INT(0, 255))>> || _ <- lists:seq(1, ?RAND_INT(0, 5))>>
    end),
    Opts2 = maps_utils:put_if_defined(Opts1, inclusive, case ?RAND_INT(1, 3) of
        1 -> undefined;
        _ -> ?RAND_BOOL()
    end),
    maps_utils:put_if_defined(Opts2, offset, case ?RAND_INT(1, 3) of
        1 -> undefined;
        _ -> ?RAND_INT(-Count, Count)
    end).
