%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Tests of the handles module.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_registry_test_SUITE).
-author("Katarzyna Such").

-include_lib("ctool/include/test/test_utils.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("http/public_data/oai.hrl").

-export([
    groups/0, all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    % parallel_tests
    resumption_token_test/1,
    list_in_one_batch_test/1,
    list_all_handle_test/1,
    list_handles_with_metadata_schema_test/1,
    list_size_elements_test/1,
    list_from_until_test/1,
    list_from_until_with_resumption_token_test/1,
    add_element_that_already_exist_test/1,
    purge_deleted_entry_test/1,
    purge_deleted_entries_for_service_test/1,

    % sequential_tests
    list_handles_from_services_test/1,
    add_handle_to_service_test/1,
    add_handle_with_earlier_timestamp_test/1,
    get_handle_with_earliest_timestamp_test/1,
    list_from_until_inclusive_test/1,
    update_handle_timestamp_test/1,
    delete_handle_from_service_test/1,
    delete_every_second_handle_test/1
]).

all() -> [
    {group, parallel_tests},
    {group, sequential_tests}
].

groups() -> [
    {parallel_tests, [parallel], [
        resumption_token_test,
        list_in_one_batch_test,
        list_all_handle_test,
        list_handles_with_metadata_schema_test,
        list_size_elements_test,
        list_from_until_test,
        list_from_until_with_resumption_token_test,
        add_element_that_already_exist_test,
        purge_deleted_entry_test,
        purge_deleted_entries_for_service_test
    ]},

    {sequential_tests, [sequential], [
        list_handles_from_services_test,
        add_handle_to_service_test,
        add_handle_with_earlier_timestamp_test,
        get_handle_with_earliest_timestamp_test,
        list_from_until_inclusive_test,
        update_handle_timestamp_test,
        delete_handle_from_service_test,
        delete_every_second_handle_test
    ]}
].

-define(INITIAL_TOTAL_HANDLE_COUNT, 3300).
-define(INITIAL_HANDLE_COUNT_IN_SMALL_HSERVICE, 600).
-define(RAND_ID(), str_utils:rand_hex(16)).
-define(HANDLE_LIST_LIMIT_FOR_TESTS, 1000).

-define(RAND_METADATA_SCHEMA(), ?RAND_ELEMENT([?OAI_DC_METADATA_PREFIX, ?EDM_METADATA_PREFIX])).

-define(RAND_TIMESTAMP(), ?RAND_INT(1200000000, 1700000000)).

-define(OZ_RPC_FIRST_NODE(), hd(lists:sort(ozt:get_nodes()))).

-define(FIRST_HSERVICE, <<"first">>).
-define(ANOTHER_HSERVICE, <<"another">>).
% the list of handles for this service is not modified during the tests for it to remain small (less than 1000 handles)
-define(SMALL_HSERVICE, <<"small">>).
-define(RAND_SERVICE(), case ?RAND_BOOL() of % see above
    true -> ?FIRST_HSERVICE;
    false -> ?ANOTHER_HSERVICE
end).

-define(checkListing(Opts), ?assertEqual(load_all_expected_entries(Opts), list_completely(Opts))).


%%%===================================================================
%%% Tests
%%%===================================================================


resumption_token_test(_Config) ->
    %% For every of 3300 handle metadata_schema is drawn using ?RAND_BOOL() from
    %% ?OAI_DC_METADATA_PREFIX and ?EDM_METADATA_PREFIX. Statistically it is improbable
    %% for one schema to come up more than about a half of 3300. Because of that each
    %% group of handles is represented by less than 2000 and more of 1000 elements.
    %% First listing will return 1000 elements and resumption token, second rest of the
    %% elements (less than 1000) and no resumption token.

    %% first listing, no resumption_token
    {List1, Token1} = list_portion(#{metadata_schema => ?OAI_DC_METADATA_PREFIX}),
    ?assertEqual(?HANDLE_LIST_LIMIT_FOR_TESTS, length(List1)),

    %% second listing, resumption token from first listing
    {List2, undefined} = list_portion(#{resumption_token => Token1}),
    ?assertEqual(load_all_expected_entries(#{metadata_schema => ?OAI_DC_METADATA_PREFIX}), List1 ++ List2),
    %% third listing, other schema
    {List3, Token3} = list_portion(#{metadata_schema => ?EDM_METADATA_PREFIX}),
    ?assertEqual(?HANDLE_LIST_LIMIT_FOR_TESTS, length(List3)),

    {List4, undefined} = list_portion(#{resumption_token => Token3}),
    ?assertEqual(load_all_expected_entries(#{metadata_schema => ?EDM_METADATA_PREFIX}), List3 ++ List4),
    ?assertEqual(?INITIAL_TOTAL_HANDLE_COUNT, length(List1) + length(List2) + length(List3) + length(List4)).


list_in_one_batch_test(_Config) ->
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    ListOpts = #{service_id => ?SMALL_HSERVICE, metadata_schema => MetadataSchema},
    {List, undefined} = list_portion(ListOpts),
    ?assertEqual(load_all_expected_entries(ListOpts), List).


list_all_handle_test(_Config) ->
    AllEntries = gather_by_all_schemas(),
    ?assertEqual(?INITIAL_TOTAL_HANDLE_COUNT, length(AllEntries)),
    ?assertEqual(load_all_expected_entries(), AllEntries).


list_handles_with_metadata_schema_test(_Config) ->
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    ListOpts = #{metadata_schema => MetadataSchema},
    ActualList = list_completely(ListOpts),
    ?assertEqual(load_all_expected_entries(ListOpts), ActualList),

    ListOptsHService = #{metadata_schema => MetadataSchema, service_id => ?SMALL_HSERVICE},
    ActualListMetadataAndHService = list_completely(ListOptsHService),
    ?assertEqual(load_all_expected_entries(ListOptsHService),
        ActualListMetadataAndHService).


list_size_elements_test(_Config) ->
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    {List, ResumptionToken} = list_portion(#{limit => 5, metadata_schema => MetadataSchema}),
    ?assert(is_binary(ResumptionToken)),
    ?assertEqual(lists:sublist(load_all_expected_entries(#{metadata_schema => MetadataSchema}), 5), List),
    ?assertEqual(5, length(List)).


list_from_until_test(_Config) ->
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    % list from
    OptsFrom = #{from => ?RAND_TIMESTAMP(), metadata_schema => MetadataSchema},
    ?checkListing(OptsFrom),

    % list until
    OptsUntil = #{until => ?RAND_TIMESTAMP(), metadata_schema => MetadataSchema},
    ?checkListing(OptsUntil),

    % list from to until
    [From2, Until2] = lists:sort([?RAND_TIMESTAMP(), ?RAND_TIMESTAMP()]),
    ?checkListing(#{from => From2, until => Until2, metadata_schema => MetadataSchema}),

    % what if until is equal to from and it is the moment of creating first
    #handle_listing_entry{
        timestamp = TimestampSeconds, handle_id = HandleId
    } = hd(load_expected_entries(MetadataSchema)),
    [#handle_listing_entry{handle_id = HandleIdFromList}] = FullListFromUntilEqual = list_completely(#{
        from => TimestampSeconds, until => TimestampSeconds, metadata_schema => MetadataSchema
    }),
    ?assert(1 =< length(FullListFromUntilEqual)),
    ?assertEqual(HandleId, HandleIdFromList),

    % from greater than until
    FromGreater = Until2,
    UntilGreater = From2,
    FullListFromGreaterThanUntil = list_completely(
        #{from => FromGreater, until => UntilGreater, metadata_schema => MetadataSchema}
    ),
    ?assertEqual([], FullListFromGreaterThanUntil),

    % from and until outside the range of available dates
    % rand timestamp +/- 10 years
    FromOutside = ?RAND_TIMESTAMP() - 3600 * 24 * 365 * 10,
    UntilOutside = ?RAND_TIMESTAMP() + 3600 * 24 * 365 * 10,
    ?checkListing(#{from => FromOutside, until => UntilOutside, metadata_schema => MetadataSchema}).


list_from_until_with_resumption_token_test(_Config) ->
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    AllHandles = load_expected_entries(MetadataSchema),
    NumOfElements = ceil(length(AllHandles) / 3),
    {_Handles1, Handles23} = lists:split(NumOfElements, AllHandles),
    {Handles2, _Handles3} = lists:split(NumOfElements, Handles23),

    #handle_listing_entry{timestamp = From} = hd(Handles2),
    #handle_listing_entry{timestamp = Until} = lists:last(Handles2),
    Limit = ceil(length(AllHandles) / 10),

    ?checkListing(#{from => From, metadata_schema => MetadataSchema, limit => Limit}),
    ?checkListing(#{until => Until, metadata_schema => MetadataSchema, limit => Limit}),
    ?checkListing(#{from => From, until => Until, metadata_schema => MetadataSchema, limit => Limit}).


add_element_that_already_exist_test(_Config) ->
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    [#handle_listing_entry{timestamp = FirstTimestamp, handle_id = FirstHandleId} | _] = InitialList =
        list_completely(
            #{service_id => ?FIRST_HSERVICE, metadata_schema => MetadataSchema}
        ),
    try
        create_entry(?FIRST_HSERVICE, MetadataSchema, FirstTimestamp, FirstHandleId)
    catch
        error:ErrorReason:_Stacktrace ->
            ?assertEqual(ErrorReason, {badrpc, ?ERROR_ALREADY_EXISTS})
    end,
    ?assertEqual(InitialList, list_completely(
        #{service_id => ?FIRST_HSERVICE, metadata_schema => MetadataSchema})
    ).


purge_deleted_entry_test(_Config) ->
    HServiceId = str_utils:format_bin("hs-~ts", [?FUNCTION_NAME]),
    MetadataSchema = str_utils:format_bin("mp-~ts", [?FUNCTION_NAME]),
    HandleId = ?RAND_ID(),
    Timestamp = ?RAND_TIMESTAMP(),

    ozt:rpc(handle_registry, report_created, [MetadataSchema, HServiceId, HandleId, Timestamp]),

    DelTimestamp = Timestamp + 100,
    ?assertEqual(ok, ozt:rpc(handle_registry, report_deleted, [MetadataSchema, HServiceId, HandleId, Timestamp, DelTimestamp])),
    ?assertMatch({ok, _}, ozt:rpc(handle_registry, lookup_deleted, [HandleId])),

    ?assertEqual(ok, ozt:rpc(handle_registry, purge_deleted_entry, [HandleId])),
    ?assertEqual(error, ozt:rpc(handle_registry, lookup_deleted, [HandleId])),

    ?assertMatch(?ERROR_NOT_FOUND, ozt:rpc(handle_registry, purge_deleted_entry, [HandleId])).


purge_deleted_entries_for_service_test(_Config) ->
    HServiceId = str_utils:format_bin("hs-~ts", [?FUNCTION_NAME]),
    MetadataSchema = str_utils:format_bin("mp-~ts", [?FUNCTION_NAME]),

    utils:repeat(2222, fun() -> create_entry(HServiceId, MetadataSchema) end),
    AllEntries = list_completely(#{metadata_schema => MetadataSchema, service_id => HServiceId}),

    lists:foreach(fun(HandleEntry) ->
        delete_entry(MetadataSchema, HandleEntry#handle_listing_entry.timestamp, HandleEntry)
    end, AllEntries),
    ExpAllDeletedEntries = [E#handle_listing_entry{status = deleted} || E <- AllEntries],
    ?assertEqual(ExpAllDeletedEntries, list_completely(#{
        metadata_schema => MetadataSchema, service_id => HServiceId, include_deleted => true
    })),
    ?assertEqual([], list_completely(#{
        metadata_schema => MetadataSchema, service_id => HServiceId, include_deleted => false
    })),

    ?assertEqual(ok, ozt:rpc(handle_registry, purge_deleted_entries_for_service, [HServiceId])),
    ?assertEqual([], list_completely(#{
        metadata_schema => MetadataSchema, service_id => HServiceId, include_deleted => true
    })),
    ?assertEqual([], list_completely(#{
        metadata_schema => MetadataSchema, service_id => HServiceId, include_deleted => false
    })).


list_handles_from_services_test(_Config) ->
    MkListOpts = fun(MetadataSchema, HServiceId) ->
        %% listing limit lower than the number of handles is used to check continuous listing with resumption tokens
        #{metadata_schema => MetadataSchema, service_id => HServiceId, limit => 500}
    end,
    ?checkListing(MkListOpts(?OAI_DC_METADATA_PREFIX, ?FIRST_HSERVICE)),
    ?checkListing(MkListOpts(?EDM_METADATA_PREFIX, ?FIRST_HSERVICE)),

    ?checkListing(MkListOpts(?OAI_DC_METADATA_PREFIX, ?ANOTHER_HSERVICE)),
    ?checkListing(MkListOpts(?EDM_METADATA_PREFIX, ?ANOTHER_HSERVICE)),

    ?checkListing(MkListOpts(?OAI_DC_METADATA_PREFIX, ?SMALL_HSERVICE)),
    ?checkListing(MkListOpts(?EDM_METADATA_PREFIX, ?SMALL_HSERVICE)),

    ?assertEqual(
        load_all_expected_entries(#{metadata_schema => ?OAI_DC_METADATA_PREFIX}),
        lists:umerge([
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?FIRST_HSERVICE)),
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?ANOTHER_HSERVICE)),
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?SMALL_HSERVICE))
        ])
    ),
    ?assertEqual(
        load_all_expected_entries(#{metadata_schema => ?EDM_METADATA_PREFIX}),
        lists:umerge([
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?FIRST_HSERVICE)),
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?ANOTHER_HSERVICE)),
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?SMALL_HSERVICE))
        ])
    ),
    ?assertEqual(
        ?INITIAL_TOTAL_HANDLE_COUNT,
        lists:flatlength([
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?FIRST_HSERVICE)),
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?ANOTHER_HSERVICE)),
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?SMALL_HSERVICE)),
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?FIRST_HSERVICE)),
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?ANOTHER_HSERVICE)),
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?SMALL_HSERVICE))
        ])
    ).


add_handle_to_service_test(_Config) ->
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    BeforeAddingHService1 = length(list_completely(
        #{service_id => ?FIRST_HSERVICE, metadata_schema => MetadataSchema}
    )),
    BeforeAddingHService2 = length(list_completely(
        #{service_id => ?ANOTHER_HSERVICE, metadata_schema => MetadataSchema}
    )),
    BeforeAddingAll = length(list_completely(#{metadata_schema => MetadataSchema})),
    create_entry(?FIRST_HSERVICE, MetadataSchema),

    ?assertEqual(BeforeAddingAll + 1, length(list_completely(#{metadata_schema => MetadataSchema}))),

    utils:repeat(10, fun() -> create_entry(?ANOTHER_HSERVICE, MetadataSchema) end),

    ?assertEqual(BeforeAddingHService1 + 1,
        length(list_completely(#{service_id => ?FIRST_HSERVICE, metadata_schema => MetadataSchema}))),
    ?assertEqual(BeforeAddingHService2 + 10,
        length(list_completely(#{service_id => ?ANOTHER_HSERVICE, metadata_schema => MetadataSchema}))),
    ?assertEqual(BeforeAddingAll + 11, length(list_completely(#{metadata_schema => MetadataSchema}))).


%% checks if handles are added sorted by date
add_handle_with_earlier_timestamp_test(_Config) ->
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    HServiceId = ?RAND_SERVICE(),
    #handle_listing_entry{timestamp = FirstTimestamp} = hd(list_completely(#{metadata_schema => MetadataSchema})),
    Timestamp = FirstTimestamp - ?RAND_INT(1, 10000),
    [Handle3, Handle2, Handle1] = lists:map(fun(Number) ->
        create_entry(HServiceId, MetadataSchema, Timestamp - Number)
    end, lists:seq(1, 3)),

    {List, _Token} = list_portion(#{metadata_schema => MetadataSchema}),
    [H1, H2, H3] = lists:sublist(List, 3),
    ?assertEqual(Handle1, H1),
    ?assertEqual(Handle2, H2),
    ?assertEqual(Handle3, H3).


get_handle_with_earliest_timestamp_test(_Config) ->
    #handle_listing_entry{timestamp = FirstTimestamp} = hd(gather_by_all_schemas()),
    Timestamp = FirstTimestamp - ?RAND_INT(1, 10000),

    lists:map(fun(Number) ->
        create_entry(?RAND_SERVICE(), ?RAND_METADATA_SCHEMA(), Timestamp - Number)
    end, lists:seq(1, 4)),
    ExpectedEarliestTimestamp = Timestamp - 4,
    EarliestTimestamp = ozt:rpc(handle_registry, get_earliest_timestamp, []),
    ?assertEqual(ExpectedEarliestTimestamp, EarliestTimestamp).


list_from_until_inclusive_test(_Config) ->
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    AllList = gather_by_all_schemas(),
    #handle_listing_entry{timestamp = TimestampFirstOld} = hd(AllList),
    #handle_listing_entry{timestamp = TimestampLastOld} = lists:last(AllList),
    TimestampFirst = TimestampFirstOld - ?RAND_INT(1, 10000),
    TimestampLast = TimestampLastOld + ?RAND_INT(1, 10000),

    utils:repeat(4, fun() -> create_entry(?RAND_SERVICE(), MetadataSchema, TimestampFirst) end),
    utils:repeat(4, fun() -> create_entry(?RAND_SERVICE(), MetadataSchema, TimestampLast) end),

    ?checkListing(#{until => TimestampFirst, metadata_schema => MetadataSchema}),
    ?checkListing(#{from => TimestampFirst, until => TimestampFirst, metadata_schema => MetadataSchema}),
    ?checkListing(#{from => TimestampLast, until => TimestampLast, metadata_schema => MetadataSchema}),
    ?checkListing(#{from => TimestampLast, metadata_schema => MetadataSchema}),

    ?checkListing(#{from => TimestampFirst, until => TimestampLast, metadata_schema => MetadataSchema}),
    ?checkListing(#{from => TimestampFirst + 1, until => TimestampLast + 1, metadata_schema => MetadataSchema}),
    ?checkListing(#{from => TimestampFirst, metadata_schema => MetadataSchema}),
    ?checkListing(#{until => TimestampLast, metadata_schema => MetadataSchema}).


update_handle_timestamp_test(_Config) ->
    %% listing limit lower than the number of handles is used to check continuous listing with resumption tokens
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    ListOpts = #{service_id => ?FIRST_HSERVICE, metadata_schema => MetadataSchema, limit => 500},

    #handle_listing_entry{timestamp = FirstTimestamp} = hd(gather_by_all_schemas()),
    #handle_listing_entry{handle_id = HandleId} = lists:last(list_completely(ListOpts)),
    NewTimestamp = FirstTimestamp - ?RAND_INT(1, 10000),

    %% after updating timestamp to FirstTimestamp - RandNumber this handle will be at the beginning
    update_entry(MetadataSchema, HandleId, NewTimestamp),
    AllAfterUpdate = list_completely(ListOpts),

    ?assertEqual(#handle_listing_entry{
        timestamp = NewTimestamp,
        handle_id = HandleId,
        service_id = ?FIRST_HSERVICE,
        status = present
    }, hd(AllAfterUpdate)),
    ?assertEqual(NewTimestamp, ozt:rpc(handle_registry, get_earliest_timestamp, [])),
    ?assertEqual(load_all_expected_entries(ListOpts), AllAfterUpdate).


delete_handle_from_service_test(_Config) ->
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    OptsMetadataSchema = #{metadata_schema => MetadataSchema},
    OptsFirstHService = #{service_id => ?FIRST_HSERVICE, metadata_schema => MetadataSchema},
    OptsAnotherHService = #{service_id => ?ANOTHER_HSERVICE, metadata_schema => MetadataSchema},
    ListAll = list_completely(OptsMetadataSchema),
    ListHService1 = list_completely(OptsFirstHService),
    ListHService2 = list_completely(OptsAnotherHService),

    DeletedFromHService1 = ?RAND_SUBLIST(ListHService1),
    lists:foreach(fun(HandleEntry) -> delete_entry(MetadataSchema, HandleEntry) end, DeletedFromHService1),

    ?assertEqual(lists:subtract(ListAll, DeletedFromHService1), list_completely(OptsMetadataSchema)),
    ?checkListing(OptsMetadataSchema),

    DeletedFromHService2 = ?RAND_SUBLIST(ListHService2),
    lists:foreach(fun(HandleEntry) -> delete_entry(MetadataSchema, HandleEntry) end, DeletedFromHService2),

    ?assertEqual(lists:subtract(ListHService1, DeletedFromHService1), list_completely(OptsFirstHService)),
    ?checkListing(OptsFirstHService),
    ?checkListing(OptsFirstHService#{include_deleted => true}),

    ?assertEqual(lists:subtract(ListHService2, DeletedFromHService2), list_completely(OptsAnotherHService)),
    ?checkListing(OptsAnotherHService),
    ?checkListing(OptsAnotherHService#{include_deleted => true}),

    ?assertEqual(
        lists:subtract(ListAll, DeletedFromHService1 ++ DeletedFromHService2),
        list_completely(OptsMetadataSchema)
    ),
    ?checkListing(OptsMetadataSchema),
    ?checkListing(OptsMetadataSchema#{include_deleted => true}).


delete_every_second_handle_test(_Config) ->
    MetadataSchema = ?RAND_METADATA_SCHEMA(),
    NumberToCreate = 200,
    HServiceId = datastore_key:new(),
    utils:repeat(NumberToCreate, fun() -> create_entry(HServiceId, MetadataSchema) end),
    ListingOpts = #{service_id => HServiceId, metadata_schema => MetadataSchema},
    All = list_completely(ListingOpts),
    ?assertEqual(NumberToCreate, length(All)),

    ElementsToDelete = [HandleEntry || {Number, HandleEntry} <- lists:enumerate(All), Number rem 2 =:= 0],
    lists:foreach(fun(HandleEntry) -> delete_entry(MetadataSchema, HandleEntry) end, ElementsToDelete),
    ListingOptsLimit = maps:put(limit, NumberToCreate div 2, ListingOpts),
    AfterDeleting = list_completely(ListingOptsLimit),
    ?assertEqual(NumberToCreate div 2, length(AfterDeleting)),

    IncludeDeleted = list_completely(maps:put(include_deleted, true, ListingOptsLimit)),
    ?assertEqual(NumberToCreate, length(IncludeDeleted)),

    ?assertEqual(error, ozt:rpc(handle_registry, lookup_deleted, [<<"IdThatNotExists">>])),

    #handle_listing_entry{handle_id = HandleId} = hd(ElementsToDelete),
    ?assertEqual(
        {ok, {MetadataSchema, lookup_expected_entry(HandleId)}},
        ozt:rpc(handle_registry, lookup_deleted, [HandleId])
    ).


%%%===================================================================
%%% Helpers
%%%===================================================================

create_entry(HServiceId) ->
    create_entry(HServiceId, ?RAND_METADATA_SCHEMA()).

create_entry(HServiceId, MetadataSchema) ->
    create_entry(HServiceId, MetadataSchema, ?RAND_TIMESTAMP()).

create_entry(HServiceId, MetadataSchema, Timestamp) ->
    create_entry(HServiceId, MetadataSchema, Timestamp, ?RAND_ID()).

create_entry(HServiceId, MetadataSchema, Timestamp, HandleId) ->
    ozt:rpc(handle_registry, report_created, [MetadataSchema, HServiceId, HandleId, Timestamp]),
    HandleEntry = #handle_listing_entry{
        timestamp = Timestamp,
        handle_id = HandleId,
        service_id = HServiceId,
        status = present
    },
    update_expected_entries(MetadataSchema, HServiceId, fun(OldExpected) ->
        lists:umerge([HandleEntry], OldExpected)
    end),
    HandleEntry.

update_entry(MetadataSchema, HandleId, NewTimestamp) ->
    #handle_listing_entry{
        timestamp = OldTimestamp,
        service_id = HServiceId
    } = HandleEntry = lookup_expected_entry(HandleId),
    ozt:rpc(handle_registry, report_updated, [MetadataSchema, HServiceId, HandleId, OldTimestamp, NewTimestamp]),
    update_expected_entries(MetadataSchema, HServiceId, fun(OldExpected) ->
        lists:umerge(
            [HandleEntry#handle_listing_entry{timestamp = NewTimestamp}],
            lists:delete(HandleEntry, OldExpected)
        )
    end).

delete_entry(MetadataSchema, HandleEntry) ->
    delete_entry(MetadataSchema, ?RAND_TIMESTAMP(), HandleEntry).

delete_entry(MetadataSchema, DeletionTimestamp, #handle_listing_entry{
    timestamp = Timestamp, handle_id = HandleId, service_id = HServiceId
} = HandleEntry) ->
    ozt:rpc(handle_registry, report_deleted, [MetadataSchema, HServiceId, HandleId, Timestamp, DeletionTimestamp]),
    update_expected_entries(MetadataSchema, HServiceId, fun(OldExpected) ->
        lists:umerge(
            [HandleEntry#handle_listing_entry{timestamp = DeletionTimestamp, status = deleted}],
            lists:delete(HandleEntry, OldExpected))
    end).

lookup_expected_entry(HandleId) ->
    Expected = load_all_expected_entries(),
    ?check(lists_utils:find(fun(H) -> H#handle_listing_entry.handle_id == HandleId end, Expected)).

load_all_expected_entries(Opts) when is_map(Opts) ->
    MetadataSchema = maps:get(metadata_schema, Opts),
    From = maps:get(from, Opts, 0),
    Until = maps:get(until, Opts, 99999999999),
    HService = maps:get(service_id, Opts, undefined),
    AllHandles = load_expected_entries(MetadataSchema, HService),

    IncludeDeleted = maps:get(include_deleted, Opts, false),

    AllHandlesWithMatchingStatus = lists:filter(fun(#handle_listing_entry{status = Status}) ->
        case {IncludeDeleted, Status} of
            {false, deleted} -> false;
            _ -> true
        end
    end, AllHandles),
    ListFrom = lists:dropwhile(fun(H) -> H#handle_listing_entry.timestamp < From end, AllHandlesWithMatchingStatus),
    lists:takewhile(fun(H) -> H#handle_listing_entry.timestamp =< Until end, ListFrom).

load_all_expected_entries() ->
    lists:umerge(
        load_expected_entries(?OAI_DC_METADATA_PREFIX),
        load_expected_entries(?EDM_METADATA_PREFIX)
    ).

update_expected_entries(MetadataSchema, HServiceId, UpdateFun) ->
    %% update expectation for all handles and for specific HService
    save_expected_entries(MetadataSchema, undefined, UpdateFun(load_expected_entries(MetadataSchema, undefined))),
    save_expected_entries(MetadataSchema, HServiceId, UpdateFun(load_expected_entries(MetadataSchema, HServiceId))).

%% expected handles are stored on the first oz-worker node to allow
%% rerunning tests with --no-clean option
load_expected_entries(MetadataSchema) ->
    load_expected_entries(MetadataSchema, undefined).
load_expected_entries(MetadataSchema, HServiceId) ->
    ozt:rpc(?OZ_RPC_FIRST_NODE(), node_cache, get,
        [<<MetadataSchema/binary, (str_utils:to_binary(HServiceId))/binary>>, []]
    ).


save_expected_entries(MetadataSchema, HServiceId, HandlesList) ->
    ozt:rpc(?OZ_RPC_FIRST_NODE(), node_cache, put,
        [<<MetadataSchema/binary, (str_utils:to_binary(HServiceId))/binary>>, HandlesList]).

clear_expected_entries(MetadataSchema) ->
    clear_expected_entries(MetadataSchema, undefined).
clear_expected_entries(MetadataSchema, HServiceId) ->
    ozt:rpc(?OZ_RPC_FIRST_NODE(), node_cache, clear, [
        <<MetadataSchema/binary, (str_utils:to_binary(HServiceId))/binary>>]).

gather_by_all_schemas() ->
    ozt:rpc(handle_registry, gather_by_all_schemas, []).

list_completely(ListingOpts) ->
    ozt:rpc(handle_registry, list_completely, [ListingOpts]).

list_portion(ListingOpts) ->
    ozt:rpc(handle_registry, list_portion, [ListingOpts]).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ozt:init_per_suite(Config, fun() ->
        ozt:set_env(default_handle_list_limit, ?HANDLE_LIST_LIMIT_FOR_TESTS),
        lists:foreach(fun(MetadataSchema) ->
            lists:foreach(fun(HandleEntry) ->
                delete_entry(MetadataSchema, HandleEntry)
            end, load_all_expected_entries(#{metadata_schema => MetadataSchema})),
            clear_expected_entries(MetadataSchema)
        end, [?OAI_DC_METADATA_PREFIX, ?EDM_METADATA_PREFIX]),
        lists:foreach(fun(HServiceId) ->
            ok = ozt:rpc(handle_registry, purge_deleted_entries_for_service, [HServiceId]),
            lists:foreach(fun(MetadataSchema) ->
                clear_expected_entries(MetadataSchema, HServiceId)
            end, [?OAI_DC_METADATA_PREFIX, ?EDM_METADATA_PREFIX])
        end, [?FIRST_HSERVICE, ?ANOTHER_HSERVICE, ?SMALL_HSERVICE]),
        utils:repeat(?INITIAL_HANDLE_COUNT_IN_SMALL_HSERVICE, fun() -> create_entry(?SMALL_HSERVICE) end),
        utils:repeat(?INITIAL_TOTAL_HANDLE_COUNT - ?INITIAL_HANDLE_COUNT_IN_SMALL_HSERVICE,
            fun() -> create_entry(?RAND_SERVICE()) end)
    end).

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.


