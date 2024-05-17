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
-module(handles_test_SUITE).
-author("Katarzyna Such").

-include_lib("ctool/include/test/test_utils.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("http/handlers/oai.hrl").

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
    list_handles_with_metadata_format_test/1,
    list_size_elements_test/1,
    list_from_until_test/1,
    list_from_until_with_resumption_token_test/1,
    add_element_that_already_exist_test/1,

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
        list_handles_with_metadata_format_test,
        list_size_elements_test,
        list_from_until_test,
        list_from_until_with_resumption_token_test,
        add_element_that_already_exist_test
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
-define(RAND_NAME(), ?RAND_UNICODE_STR(200)).
-define(RAND_ID(), str_utils:rand_hex(16)).
-define(DEFAULT_LIST_LIMIT, oz_worker:get_env(default_handle_list_limit, 1000)).

-define(RAND_METADATA_PREFIX(), case ?RAND_BOOL() of
    true -> ?OAI_DC_METADATA_PREFIX;
    false -> ?EDM_METADATA_PREFIX
end).

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

-define(checkListing(Opts), ?assertEqual(infer_expected_handle_entries(Opts), list_completely(Opts))).


%%%===================================================================
%%% Tests
%%%===================================================================


resumption_token_test(_Config) ->
    %% For every of 3300 handle metadata_prefix is drawn using ?RAND_BOOL() from
    %% ?OAI_DC_METADATA_PREFIX and ?EDM_METADATA_PREFIX. Statistically it is improbable
    %% for one prefix to come up more than about a half of 3300. Because of that each
    %% group of handles is represented by less than 2000 and more of 1000 elements.
    %% First listing will return 1000 elements and resumption token, second rest of the
    %% elements (less than 1000) and no resumption token.

    %% first listing, no resumption_token
    {List1, Token1} = list_portion(#{metadata_prefix => ?OAI_DC_METADATA_PREFIX}),
    ?assertEqual(?DEFAULT_LIST_LIMIT, length(List1)),

    %% second listing, resumption token from first listing
    {List2, undefined} = list_portion(#{resumption_token => Token1}),
    ?assertEqual(infer_expected_handle_entries(#{metadata_prefix => ?OAI_DC_METADATA_PREFIX}), List1 ++ List2),
    %% third listing, other prefix
    {List3, Token3} = list_portion(#{metadata_prefix => ?EDM_METADATA_PREFIX}),
    ?assertEqual(?DEFAULT_LIST_LIMIT, length(List3)),

    {List4, undefined} = list_portion(#{resumption_token => Token3}),
    ?assertEqual(infer_expected_handle_entries(#{metadata_prefix => ?EDM_METADATA_PREFIX}), List3 ++ List4),
    ?assertEqual(?INITIAL_TOTAL_HANDLE_COUNT, length(List1) + length(List2) + length(List3) + length(List4)).


list_in_one_batch_test(_Config) ->
    MetadataPrefix =  ?RAND_METADATA_PREFIX(),
    ListOpts = #{service_id => ?SMALL_HSERVICE, metadata_prefix => MetadataPrefix},
    {List, undefined} = list_portion(ListOpts),
    ?assertEqual(infer_expected_handle_entries(ListOpts), List).


list_all_handle_test(_Config) ->
    ListAll = gather_by_all_prefixes(),
    ?assertEqual(?INITIAL_TOTAL_HANDLE_COUNT, length(ListAll)),
    ?assertEqual(lists:sort(infer_expected_handle_entries()), lists:sort(ListAll)).


list_handles_with_metadata_format_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ListOpts = #{metadata_prefix => MetadataPrefix},
    ActualList = list_completely(ListOpts),
    ?assertEqual(infer_expected_handle_entries(ListOpts), ActualList),

    ListOptsHService = #{metadata_prefix => MetadataPrefix, service_id => ?SMALL_HSERVICE},
    ActualListMetadataAndHService = list_completely(ListOptsHService),
    ?assertEqual(infer_expected_handle_entries(ListOptsHService),
        ActualListMetadataAndHService).


list_size_elements_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    {List, ResumptionToken} = list_portion(#{limit => 5, metadata_prefix => MetadataPrefix}),
    ?assert(is_binary(ResumptionToken)),
    ?assertEqual(lists:sublist(infer_expected_handle_entries(#{metadata_prefix => MetadataPrefix}), 5), List),
    ?assertEqual(5, length(List)).


list_from_until_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    % list from
    OptsFrom = #{from => ?RAND_TIMESTAMP(), metadata_prefix => MetadataPrefix},
    ?checkListing(OptsFrom),

    % list until
    OptsUntil = #{until => ?RAND_TIMESTAMP(), metadata_prefix => MetadataPrefix},
    ?checkListing(OptsUntil),

    % list from to until
    [From2, Until2] = lists:sort([?RAND_TIMESTAMP(), ?RAND_TIMESTAMP()]),
    ?checkListing(#{from => From2, until => Until2, metadata_prefix => MetadataPrefix}),

    % what if until is equal to from and it is the moment of creating first
    #handle_listing_entry{timestamp = TimeStampSeconds, handle_id = HandleId} =
        hd(load_expected_handles(MetadataPrefix)),
    [#handle_listing_entry{handle_id = HandleIdFromList}] = FullListFromUntilEqual = list_completely(#{
        from => TimeStampSeconds, until => TimeStampSeconds, metadata_prefix => MetadataPrefix
    }),
    ?assert(1 =< length(FullListFromUntilEqual)),
    ?assertEqual(HandleId, HandleIdFromList),

    % from greater than until
    FromGreater = Until2,
    UntilGreater = From2,
    FullListFromGreaterThanUntil = list_completely(
        #{from => FromGreater, until => UntilGreater, metadata_prefix => MetadataPrefix}
    ),
    ?assertEqual([], FullListFromGreaterThanUntil),

    % from and until outside the range of available dates
    % rand timestamp +/- 10 years
    FromOutside = ?RAND_TIMESTAMP() - 3600 * 24 * 365 * 10,
    UntilOutside = ?RAND_TIMESTAMP() + 3600 * 24 * 365 * 10,
    ?checkListing(#{from => FromOutside, until => UntilOutside, metadata_prefix => MetadataPrefix}).


list_from_until_with_resumption_token_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    AllHandles = load_expected_handles(MetadataPrefix),
    NumOfElements = ceil(length(AllHandles)/3),
    {_Handles1, Handles23} = lists:split(NumOfElements, AllHandles),
    {Handles2, _Handles3} = lists:split(NumOfElements, Handles23),

    #handle_listing_entry{timestamp = From} = hd(Handles2),
    #handle_listing_entry{timestamp = Until} = lists:last(Handles2),
    Limit = ceil(length(AllHandles)/10),

    ?checkListing(#{from => From, metadata_prefix => MetadataPrefix, limit => Limit}),
    ?checkListing(#{until => Until, metadata_prefix => MetadataPrefix, limit => Limit}),
    ?checkListing(#{from => From, until => Until, metadata_prefix => MetadataPrefix, limit => Limit}).


add_element_that_already_exist_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    [#handle_listing_entry{timestamp = FirstTimestamp, handle_id = FirstHandleId} | _] = InitialList =
        list_completely(
            #{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}
        ),
    try
        create_handle(?FIRST_HSERVICE, MetadataPrefix, FirstTimestamp, FirstHandleId)
    catch
        error:ErrorReason:_Stacktrace ->
            ?assertEqual(ErrorReason, {badrpc, ?ERROR_ALREADY_EXISTS})
    end,
    ?assertEqual(InitialList, list_completely(
        #{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix})
    ).


list_handles_from_services_test(_Config) ->
    MkListOpts = fun(MetadataPrefix, HServiceId) ->
        %% listing limit lower than the number of handles is used to check continuous listing with resumption tokens
        #{metadata_prefix => MetadataPrefix, service_id => HServiceId, limit => 500}
    end,
    ?checkListing(MkListOpts(?OAI_DC_METADATA_PREFIX, ?FIRST_HSERVICE)),
    ?checkListing(MkListOpts(?EDM_METADATA_PREFIX, ?FIRST_HSERVICE)),

    ?checkListing(MkListOpts(?OAI_DC_METADATA_PREFIX, ?ANOTHER_HSERVICE)),
    ?checkListing(MkListOpts(?EDM_METADATA_PREFIX, ?ANOTHER_HSERVICE)),

    ?checkListing(MkListOpts(?OAI_DC_METADATA_PREFIX, ?SMALL_HSERVICE)),
    ?checkListing(MkListOpts(?EDM_METADATA_PREFIX, ?SMALL_HSERVICE)),

    ?assertEqual(infer_expected_handle_entries(#{metadata_prefix => ?OAI_DC_METADATA_PREFIX}),
        list_sort(
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?FIRST_HSERVICE)) ++
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?ANOTHER_HSERVICE)) ++
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?SMALL_HSERVICE))
        )),
    ?assertEqual(infer_expected_handle_entries(#{metadata_prefix => ?EDM_METADATA_PREFIX}),
        list_sort(
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?FIRST_HSERVICE)) ++
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?ANOTHER_HSERVICE)) ++
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?SMALL_HSERVICE))
        )),
    ?assertEqual(?INITIAL_TOTAL_HANDLE_COUNT,
        length(
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?FIRST_HSERVICE)) ++
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?ANOTHER_HSERVICE)) ++
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?SMALL_HSERVICE)) ++
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?FIRST_HSERVICE)) ++
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?ANOTHER_HSERVICE)) ++
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?SMALL_HSERVICE))
        )).


add_handle_to_service_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    BeforeAddingHService1 = length(list_completely(
        #{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}
    )),
    BeforeAddingHService2 = length(list_completely(
        #{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}
    )),
    BeforeAddingAll = length(list_completely(#{metadata_prefix => MetadataPrefix})),
    create_handle(?FIRST_HSERVICE, MetadataPrefix),

    ?assertEqual(BeforeAddingAll + 1, length(list_completely(#{metadata_prefix => MetadataPrefix}))),

    utils:repeat(10, fun() -> create_handle(?ANOTHER_HSERVICE, MetadataPrefix) end),

    ?assertEqual(BeforeAddingHService1 + 1,
        length(list_completely(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(BeforeAddingHService2 + 10,
        length(list_completely(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(BeforeAddingAll + 11, length(list_completely(#{metadata_prefix => MetadataPrefix}))).


%% checks if handles are added sorted by date
add_handle_with_earlier_timestamp_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    HServiceId = ?RAND_SERVICE(),
    #handle_listing_entry{timestamp = FirstTimestamp} = hd(list_completely(#{metadata_prefix => MetadataPrefix})),
    RandNumber = ?RAND_INT(1, 10000),
    TimeStamp = FirstTimestamp - RandNumber,
    [Handle3, Handle2, Handle1] = lists:map(fun(Number) ->
        create_handle(HServiceId, MetadataPrefix, TimeStamp - Number)
    end, lists:seq(1, 3)),

    {List, _Token} = list_portion(#{metadata_prefix => MetadataPrefix}),
    [H1, H2, H3] = lists:sublist(List, 3),
    ?assertEqual(Handle1, H1),
    ?assertEqual(Handle2, H2),
    ?assertEqual(Handle3, H3).


get_handle_with_earliest_timestamp_test(_Config) ->
    #handle_listing_entry{timestamp = FirstTimeStamp} =  hd(gather_by_all_prefixes()),
    TimeStamp = FirstTimeStamp - ?RAND_INT(1, 10000),

    lists:map(fun(Number) ->
        create_handle(?RAND_SERVICE(), ?RAND_METADATA_PREFIX(), TimeStamp - Number)
    end, lists:seq(1, 4)),
    ExpectedEarliestTimestamp = TimeStamp - 4,
    EarliestTimestamp = ozt:rpc(handles, get_earliest_timestamp, []),
    ?assertEqual(ExpectedEarliestTimestamp, EarliestTimestamp).


list_from_until_inclusive_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    AllList =  gather_by_all_prefixes(),
    #handle_listing_entry{timestamp = TimeStampFirstOld} = hd(AllList),
    #handle_listing_entry{timestamp = TimeStampLastOld} = lists:last(AllList),
    TimeStampFirst = TimeStampFirstOld - ?RAND_INT(1, 10000),
    TimeStampLast = TimeStampLastOld + ?RAND_INT(1, 10000),

    utils:repeat(4, fun() -> create_handle(?RAND_SERVICE(), MetadataPrefix, TimeStampFirst) end),
    utils:repeat(4, fun() -> create_handle(?RAND_SERVICE(), MetadataPrefix, TimeStampLast) end),

    ?checkListing(#{until => TimeStampFirst, metadata_prefix => MetadataPrefix}),
    ?checkListing(#{from => TimeStampFirst, until => TimeStampFirst, metadata_prefix => MetadataPrefix}),
    ?checkListing(#{from => TimeStampLast, until => TimeStampLast, metadata_prefix => MetadataPrefix}),
    ?checkListing(#{from => TimeStampLast, metadata_prefix => MetadataPrefix}),

    ?checkListing(#{from => TimeStampFirst, until => TimeStampLast, metadata_prefix => MetadataPrefix}),
    ?checkListing(#{from => TimeStampFirst + 1, until => TimeStampLast + 1, metadata_prefix => MetadataPrefix}),
    ?checkListing(#{from => TimeStampFirst, metadata_prefix => MetadataPrefix}),
    ?checkListing(#{until => TimeStampLast, metadata_prefix => MetadataPrefix}).


update_handle_timestamp_test(_Config) ->
    %% listing limit lower than the number of handles is used to check continuous listing with resumption tokens
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ListOpts = #{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix, limit => 500},
    RandNumber = ?RAND_INT(1, 10000),

    #handle_listing_entry{timestamp = FirstTimestamp} = hd(gather_by_all_prefixes()),
    #handle_listing_entry{handle_id = HandleId} = lists:last(list_completely(ListOpts)),
    NewTimeStamp = FirstTimestamp - RandNumber,

    %% after updating timestamp to FirstTimestamp - RandNumber this handle will be at the beginning
    update_handle(HandleId, NewTimeStamp, MetadataPrefix),
    AllAfterUpdate = list_completely(ListOpts),

    ?assertEqual(#handle_listing_entry{
        timestamp = NewTimeStamp,
        service_id = ?FIRST_HSERVICE,
        handle_id = HandleId,
        status = present
    }, hd(AllAfterUpdate)),
    ?assertEqual(NewTimeStamp, ozt:rpc(handles, get_earliest_timestamp, [])),
    ?assertEqual(infer_expected_handle_entries(ListOpts), AllAfterUpdate).


delete_handle_from_service_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    OptsMetadataPrefix = #{metadata_prefix => MetadataPrefix},
    OptsFirstHService = #{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix},
    OptsAnotherHService = #{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix},
    ListAll = list_completely(OptsMetadataPrefix),
    ListHService1 = list_completely(OptsFirstHService),
    ListHService2 = list_completely(OptsAnotherHService),

    DeletedHService1 = ?RAND_SUBLIST(ListHService1),
    lists:foreach(fun(Handle1) -> delete_handle(Handle1, MetadataPrefix) end, DeletedHService1),

    ?assertEqual(lists:subtract(ListAll, DeletedHService1), list_completely(OptsMetadataPrefix)),
    ?checkListing(OptsMetadataPrefix),

    DeletedHService2 = ?RAND_SUBLIST(ListHService2),
    lists:foreach(fun(Handle2) -> delete_handle(Handle2, MetadataPrefix) end, DeletedHService2),

    ?assertEqual(lists:subtract(ListHService1, DeletedHService1), list_completely(OptsFirstHService)),
    ?checkListing(OptsFirstHService),
    ?checkListing(OptsFirstHService#{include_deleted => true}),

    ?assertEqual(lists:subtract(ListHService2, DeletedHService2), list_completely(OptsAnotherHService)),
    ?checkListing(OptsAnotherHService),
    ?checkListing(OptsAnotherHService#{include_deleted => true}),

    ?assertEqual(
        lists:subtract(ListAll, DeletedHService1 ++ DeletedHService2),
        list_completely(OptsMetadataPrefix)
    ),
    ?checkListing(OptsMetadataPrefix),
    ?checkListing(OptsMetadataPrefix#{include_deleted => true}).


delete_every_second_handle_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    NumberToCreate = 200,
    HServiceId = datastore_key:new(),
    utils:repeat(NumberToCreate, fun() -> create_handle(HServiceId, MetadataPrefix) end),
    ListingOpts = #{service_id => HServiceId, metadata_prefix => MetadataPrefix},
    All = list_completely(ListingOpts),
    ?assertEqual(NumberToCreate, length(All)),

    ElementsToDelete = [Handle || {Number, Handle} <- lists:enumerate(All), Number rem 2 =:= 0],
    lists:foreach(fun(Handle) -> delete_handle(Handle, MetadataPrefix) end, ElementsToDelete),
    ListingOptsLimit = maps:put(limit, NumberToCreate div 2, ListingOpts),
    AfterDeleting = list_completely(ListingOptsLimit),
    ?assertEqual(NumberToCreate div 2, length(AfterDeleting)),

    IncludeDeleted = list_completely(maps:put(include_deleted, true, ListingOptsLimit)),
    ?assertEqual(NumberToCreate, length(IncludeDeleted)),

    ?assertEqual(error, ozt:rpc(handles, lookup_deleted, [<<"IdThatNotExists">>])),

    #handle_listing_entry{handle_id = HandleId} = hd(ElementsToDelete),
    ?assertEqual({ok, lookup_expected_handle(HandleId)}, ozt:rpc(handles, lookup_deleted, [HandleId])).



%%%===================================================================
%%% Helpers
%%%===================================================================

create_handle(HServiceId) ->
    create_handle(HServiceId, ?RAND_METADATA_PREFIX()).

create_handle(HServiceId, MetadataPrefix) ->
    create_handle(HServiceId, MetadataPrefix, ?RAND_TIMESTAMP()).

create_handle(HServiceId, MetadataPrefix, TimeSeconds) ->
    create_handle(HServiceId, MetadataPrefix, TimeSeconds, ?RAND_ID()).

create_handle(HServiceId, MetadataPrefix, TimeSeconds, HandleId) ->
    ozt:rpc(handles, report_created, [MetadataPrefix, HServiceId, HandleId, TimeSeconds]),
    Handle = #handle_listing_entry{
        timestamp = TimeSeconds,
        service_id = HServiceId,
        handle_id = HandleId,
        status = present
    },
    UpdateHandlesFun = fun(OldExpected) -> list_sort([Handle | OldExpected]) end,
    update_expected_handles(UpdateHandlesFun, MetadataPrefix, HServiceId),
    Handle.

update_handle(HandleId, NewTimeStamp, MetadataPrefix) ->
    #handle_listing_entry{
        timestamp = OldTimeStamp,
        service_id = HServiceId
    } = Handle = lookup_expected_handle(HandleId),

    ozt:rpc(handles, update_timestamp, [
        MetadataPrefix, HServiceId, HandleId, OldTimeStamp, NewTimeStamp
    ]),
    UpdatedHandle = Handle#handle_listing_entry{timestamp = NewTimeStamp},
    UpdateHandlesFun = fun(OldExpected) -> list_sort([UpdatedHandle | lists:delete(Handle, OldExpected)]) end,
    update_expected_handles(UpdateHandlesFun, MetadataPrefix, HServiceId).

delete_handle(#handle_listing_entry{timestamp = TimeStamp, service_id = HServiceId, handle_id = HandleId} = Handle,
    MetadataPrefix) ->
    DeletionTimestamp = ?RAND_TIMESTAMP(),
    ozt:rpc(handles, report_deleted, [MetadataPrefix, HServiceId, HandleId, TimeStamp, DeletionTimestamp]),
    DeletedHandle = Handle#handle_listing_entry{timestamp = DeletionTimestamp, status = deleted},
    UpdateHandlesFun = fun(OldExpected) -> list_sort([DeletedHandle | lists:delete(Handle, OldExpected)]) end,
    update_expected_handles(UpdateHandlesFun, MetadataPrefix, HServiceId).

lookup_expected_handle(HandleId) ->
    Expected = load_all_expected_handles(),
    {ok, Handle} = lists_utils:find(
        fun(#handle_listing_entry{handle_id = HId}) ->
            HId == HandleId end, Expected
    ),
    Handle.

infer_expected_handle_entries(Opts) when is_map(Opts)->
    MetadataPrefix = maps:get(metadata_prefix, Opts),
    From = maps:get(from, Opts, 0),
    Until = maps:get(until, Opts, 99999999999),
    HService = maps:get(service_id, Opts, undefined),
    AllHandles = load_expected_handles(MetadataPrefix, HService),

    IncludeDeleted = maps:get(include_deleted, Opts, false),

    AllHandlesWithMatchingStatus = lists:filter(fun(#handle_listing_entry{status = Status}) ->
        case {IncludeDeleted, Status} of
            {false, deleted} -> false;
            _ -> true
        end
    end, AllHandles),
    ListFrom = lists:dropwhile(
        fun(#handle_listing_entry{timestamp = TimeStamp}) -> TimeStamp < From end,
    AllHandlesWithMatchingStatus),
    lists:takewhile(fun(#handle_listing_entry{timestamp = TimeStamp}) -> TimeStamp =< Until end, ListFrom).

infer_expected_handle_entries() ->
    HandlesList = load_all_expected_handles(),
    [#handle_listing_entry{
        timestamp = Timestamp,
        service_id = HServiceId,
        handle_id = HandleId,
        status = Status
    } || #handle_listing_entry{
        timestamp = Timestamp,
        service_id = HServiceId,
        handle_id = HandleId,
        status = Status
    } <- HandlesList].

load_all_expected_handles() ->
    load_expected_handles(?OAI_DC_METADATA_PREFIX) ++ load_expected_handles(?EDM_METADATA_PREFIX).

update_expected_handles(UpdateFun, MetadataPrefix, HServiceId) ->
    %% update expectation for all handles and for specific HService
    save_expected_handles(MetadataPrefix, undefined, UpdateFun(load_expected_handles(MetadataPrefix, undefined))),
    save_expected_handles(MetadataPrefix, HServiceId, UpdateFun(load_expected_handles(MetadataPrefix, HServiceId))).

%% expected handles are stored on the first oz-worker node to allow
%% rerunning tests with --no-clean option
load_expected_handles(MetadataPrefix) ->
    load_expected_handles(MetadataPrefix, undefined).
load_expected_handles(MetadataPrefix, HServiceId) ->
   ozt:rpc(?OZ_RPC_FIRST_NODE(), node_cache, get,
       [<<MetadataPrefix/binary, (str_utils:to_binary(HServiceId))/binary>>, []]
   ).


save_expected_handles(MetadataPrefix, HServiceId, HandlesList) ->
    ozt:rpc(?OZ_RPC_FIRST_NODE(), node_cache, put,
        [<<MetadataPrefix/binary, (str_utils:to_binary(HServiceId))/binary>>, HandlesList]).

clear_expected_handles(MetadataPrefix) ->
    clear_expected_handles(MetadataPrefix, undefined).
clear_expected_handles(MetadataPrefix, HServiceId) ->
    ozt:rpc(?OZ_RPC_FIRST_NODE(), node_cache, clear, [
        <<MetadataPrefix/binary, (str_utils:to_binary(HServiceId))/binary>>]).

gather_by_all_prefixes() ->
    ozt:rpc(handles, gather_by_all_prefixes, []).

list_completely(ListingOpts) ->
    ozt:rpc(handles, list_completely, [ListingOpts]).

list_portion(ListingOpts) ->
    ozt:rpc(handles, list_portion, [ListingOpts]).

list_sort(Handles) ->
    lists:sort(fun(
        #handle_listing_entry{timestamp = TSt1, handle_id = HId1},
        #handle_listing_entry{timestamp = TSt2, handle_id = HId2}
    ) ->
        FormattedTSt1 = str_utils:format_bin("~11..0B", [TSt1]),
        FormattedTSt2 = str_utils:format_bin("~11..0B", [TSt2]),
        <<(FormattedTSt1)/binary, 0, HId1/binary>> =< <<(FormattedTSt2)/binary, 0, HId2/binary>>
    end, Handles).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ozt:init_per_suite(Config, fun() ->
        lists:foreach(fun(MetadataPrefix) ->
            lists:foreach(fun(Handle) ->
                delete_handle(Handle, MetadataPrefix)
            end, infer_expected_handle_entries(#{metadata_prefix => MetadataPrefix})),
            clear_expected_handles(MetadataPrefix)
        end, [?OAI_DC_METADATA_PREFIX, ?EDM_METADATA_PREFIX]),
        ok = ozt:rpc(handles, purge_all_deleted_entries, []),
        lists:foreach(fun(MetadataPrefix) ->
            lists:foreach(fun(HServiceId) ->
                clear_expected_handles(MetadataPrefix, HServiceId)
            end, [?FIRST_HSERVICE, ?ANOTHER_HSERVICE, ?SMALL_HSERVICE])
        end, [?OAI_DC_METADATA_PREFIX, ?EDM_METADATA_PREFIX]),
        utils:repeat(?INITIAL_HANDLE_COUNT_IN_SMALL_HSERVICE, fun() -> create_handle(?SMALL_HSERVICE) end),
        utils:repeat(?INITIAL_TOTAL_HANDLE_COUNT - ?INITIAL_HANDLE_COUNT_IN_SMALL_HSERVICE,
            fun() -> create_handle(?RAND_SERVICE()) end)
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
