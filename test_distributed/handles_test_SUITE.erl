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
    delete_handle_from_service_test/1
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
        delete_handle_from_service_test
    ]}
].

-define(TOTAL_HANDLE_COUNT, 3300).
-define(HANDLE_COUNT_IN_SMALL_HSERVICE, 600).
-define(EARLIEST_TIMESTAMP, 1200000000).
-define(LATEST_TIMESTAMP, 1700000000).
-define(RAND_NAME(), ?RAND_UNICODE_STR(200)).
-define(RAND_ID(), str_utils:rand_hex(16)).
-define(DEFAULT_LIST_LIMIT, oz_worker:get_env(default_handle_list_limit, 1000)).

-define(RAND_METADATA_PREFIX(), case ?RAND_BOOL() of
    true -> ?OAI_DC_METADATA_PREFIX;
    false -> ?EDM_METADATA_PREFIX
end).

-define(RAND_TIMESTAMP(), ?RAND_INT(?EARLIEST_TIMESTAMP, ?LATEST_TIMESTAMP)).

-define(OZ_RPC_FIRST_NODE(), hd(lists:sort(ozt:get_nodes()))).

-define(FIRST_HSERVICE, <<"first">>).
-define(ANOTHER_HSERVICE, <<"another">>).
% the list of handles for this service is not modified during the tests for it to remain small (less than 1000 handles)
-define(SMALL_HSERVICE, <<"small">>).
-define(RAND_SERVICE(), case ?RAND_BOOL() of % see above
    true -> ?FIRST_HSERVICE;
    false -> ?ANOTHER_HSERVICE
end).

-define(checkListing(Opts), ?assertEqual(infer_expected_handle_ids(Opts), list_completely(Opts))).

-record(handle_entry, {
    %% timestamp field must be the first in the handle_entry because we sort records based on it
    timestamp :: od_handle:timestamp_seconds(),
    metadata_prefix :: binary(),
    handle_id :: od_handle:id(),
    handle_service_id :: undefined | binary()
}).


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
    {List1, Token1} = list_once(#{metadata_prefix => ?OAI_DC_METADATA_PREFIX}),
    ?assertEqual(?DEFAULT_LIST_LIMIT, length(List1)),

    %% second listing, resumption token from first listing
    {List2, undefined} = list_once(#{resumption_token => Token1}),
    ?assertEqual(infer_expected_handle_ids(#{metadata_prefix => ?OAI_DC_METADATA_PREFIX}), List1 ++ List2),
    %% third listing, other prefix
    {List3, Token3} = list_once(#{metadata_prefix => ?EDM_METADATA_PREFIX}),
    ?assertEqual(?DEFAULT_LIST_LIMIT, length(List3)),

    {List4, undefined} = list_once(#{resumption_token => Token3}),
    ?assertEqual(infer_expected_handle_ids(#{metadata_prefix => ?EDM_METADATA_PREFIX}), List3 ++ List4),
    ?assertEqual(?TOTAL_HANDLE_COUNT, length(List1) + length(List2) + length(List3) + length(List4)).


list_in_one_batch_test(_Config) ->
    MetadataPrefix =  ?RAND_METADATA_PREFIX(),
    ListOpts = #{service_id => ?SMALL_HSERVICE, metadata_prefix => MetadataPrefix},
    {List, undefined} = list_once(ListOpts),
    ?assertEqual(infer_expected_handle_ids(ListOpts), List).


list_all_handle_test(_Config) ->
    ListAll = list_by_all_prefixes(),
    ?assertEqual(?TOTAL_HANDLE_COUNT, length(ListAll)),
    ?assertEqual(lists:sort(infer_all_expected_handles_ids()), lists:sort(ListAll)).


list_handles_with_metadata_format_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ListOpts = #{metadata_prefix => MetadataPrefix},
    ActualList = list_completely(ListOpts),
    ?assertEqual(infer_expected_handle_ids(ListOpts), ActualList),

    ListOptsHService = #{metadata_prefix => MetadataPrefix, service_id => ?SMALL_HSERVICE},
    ActualListMetadataAndHService = list_completely(ListOptsHService),
    ?assertEqual(infer_expected_handle_ids(ListOptsHService),
    ActualListMetadataAndHService).


list_size_elements_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    {List, ResumptionToken} = list_once(#{limit => 5, metadata_prefix => MetadataPrefix}),
    ?assert(is_binary(ResumptionToken)),
    ?assertEqual(lists:sublist(infer_expected_handle_ids(#{metadata_prefix => MetadataPrefix}), 5), List),
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
    #handle_entry{timestamp = TimeStampSeconds, handle_id = HandleId} =
        hd(load_expected_handles(MetadataPrefix)),
    [HandleIdFromList] = FullListFromUntilEqual = list_completely(#{
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

    #handle_entry{timestamp = From} = hd(Handles2),
    #handle_entry{timestamp = Until} = hd(lists:reverse(Handles2)),
    Limit = ceil(length(AllHandles)/10),

    ?checkListing(#{from => From, metadata_prefix => MetadataPrefix, limit => Limit}),
    ?checkListing(#{until => Until, metadata_prefix => MetadataPrefix, limit => Limit}),
    ?checkListing(#{from => From, until => Until, metadata_prefix => MetadataPrefix, limit => Limit}).


add_element_that_already_exist_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    [FirstHandleId | _] = InitialList = list_completely(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}),
    try
        create_handle(?FIRST_HSERVICE, MetadataPrefix, lookup_timestamp(FirstHandleId), FirstHandleId)
    catch
        error:ErrorReason:_Stacktrace ->
            ?assertEqual(ErrorReason, {badrpc, ?ERROR_ALREADY_EXISTS})
    end,
    ?assertEqual(InitialList, list_completely(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix})).


list_handles_from_services_test(_Config) ->
    MkListOpts = fun(MetadataPrefix, HServiceId) ->
        #{metadata_prefix => MetadataPrefix, service_id => HServiceId, limit => 500}
    end,
    ?checkListing(MkListOpts(?OAI_DC_METADATA_PREFIX, ?FIRST_HSERVICE)),
    ?checkListing(MkListOpts(?EDM_METADATA_PREFIX, ?FIRST_HSERVICE)),

    ?checkListing(MkListOpts(?OAI_DC_METADATA_PREFIX, ?ANOTHER_HSERVICE)),
    ?checkListing(MkListOpts(?EDM_METADATA_PREFIX, ?ANOTHER_HSERVICE)),

    ?checkListing(MkListOpts(?OAI_DC_METADATA_PREFIX, ?SMALL_HSERVICE)),
    ?checkListing(MkListOpts(?EDM_METADATA_PREFIX, ?SMALL_HSERVICE)),


    ?assertEqual(lists:sort(infer_expected_handle_ids(#{metadata_prefix => ?OAI_DC_METADATA_PREFIX})),
        lists:sort(lists:flatten([
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?FIRST_HSERVICE)),
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?ANOTHER_HSERVICE)),
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?SMALL_HSERVICE))
    ]))),
    ?assertEqual(lists:sort(infer_expected_handle_ids(#{metadata_prefix => ?EDM_METADATA_PREFIX})),
        lists:sort(lists:flatten([
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?FIRST_HSERVICE)),
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?ANOTHER_HSERVICE)),
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?SMALL_HSERVICE))
    ]))),
    ?assertEqual(?TOTAL_HANDLE_COUNT,
        length(lists:flatten([
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?FIRST_HSERVICE)),
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?ANOTHER_HSERVICE)),
            list_completely(MkListOpts(?OAI_DC_METADATA_PREFIX, ?SMALL_HSERVICE)),
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?FIRST_HSERVICE)),
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?ANOTHER_HSERVICE)),
            list_completely(MkListOpts(?EDM_METADATA_PREFIX, ?SMALL_HSERVICE))
    ]))).


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
    FirstHandleId = hd(list_completely(#{metadata_prefix => MetadataPrefix})),
    RandNumber = ?RAND_INT(1, 10000),
    TimeStamp = lookup_timestamp(FirstHandleId) - RandNumber,
    [#handle_entry{handle_id = HandleId3}, #handle_entry{handle_id = HandleId2},
        #handle_entry{handle_id = HandleId1}] =
        lists:map(fun(Number) ->
            create_handle(HServiceId, MetadataPrefix, TimeStamp - Number)
        end, lists:seq(1, 3)),

    {List, _Token} = list_once(#{metadata_prefix => MetadataPrefix}),
    [HId1, HId2, HId3] = lists:sublist(List, 3),
    ?assertEqual(HandleId1, HId1),
    ?assertEqual(HandleId2, HId2),
    ?assertEqual(HandleId3, HId3).


get_handle_with_earliest_timestamp_test(_Config) ->
    FirstHandleId =  hd(list_by_all_prefixes()),
    RandNumber = ?RAND_INT(1, 10000),
    TimeStamp = lookup_timestamp(FirstHandleId) - RandNumber,

    lists:map(fun(Number) ->
        create_handle(?RAND_SERVICE(), ?RAND_METADATA_PREFIX(), TimeStamp - Number)
    end, lists:seq(1, 4)),
    ExpectedEarliestTimestamp = TimeStamp - 4,
    EarliestTimestamp = ozt:rpc(handles, get_earliest_timestamp, []),
    ?assertEqual(ExpectedEarliestTimestamp, EarliestTimestamp).


list_from_until_inclusive_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    AllList =  list_by_all_prefixes(),
    FirstHandleId = hd(AllList),
    LastHandleId = hd(lists:reverse(AllList)),
    TimeStampFirst = lookup_timestamp(FirstHandleId) - ?RAND_INT(1, 10000),
    TimeStampLast = lookup_timestamp(LastHandleId) - ?RAND_INT(1, 10000),

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
    HandleId = lists:last(list_completely(ListOpts)),
    NewTimeStamp = ?EARLIEST_TIMESTAMP - ?RAND_INT(1, 10000),

    %% after updating timestamp to ?EARLIEST_TIMESTAMP - ?RAND_INT(1, 10000) this handle will be at the beginning
    update_handle(HandleId, NewTimeStamp),
    AllAfterUpdate = list_completely(ListOpts),
    ?assertEqual(HandleId, hd(AllAfterUpdate)),
    ?assertEqual(NewTimeStamp, ozt:rpc(handles, get_earliest_timestamp, [])),

    %% list all after update from ?FIRST_HSERVICE
    ?assertEqual(infer_expected_handle_ids(ListOpts), AllAfterUpdate).


delete_handle_from_service_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ListAll = list_completely(#{metadata_prefix => MetadataPrefix}),
    ListHService1 = list_completely(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}),
    ListHService2 = list_completely(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}),

    lists:foreach(fun(HandleId1) ->
        delete_handle(HandleId1)
    end, lists:sublist(ListHService1, 10)),

    ?assertEqual(length(ListAll) - 10, length(list_completely(#{metadata_prefix => MetadataPrefix}))),

    [HandleId2 | _RestList] = ListHService2,
    delete_handle(HandleId2),

    ?assertEqual(length(ListHService1) - 10,
        length(list_completely(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(length(ListHService2) - 1,
        length(list_completely(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(length(ListAll) - 11, length(list_completely(#{metadata_prefix => MetadataPrefix}))).


%%%===================================================================
%%% Helpers
%%%===================================================================

list_completely(ListingOpts) ->
    case list_once(ListingOpts) of
        {List, undefined} -> List;
        {List, ResumptionToken} -> List ++ list_completely(#{resumption_token => ResumptionToken})
    end.

list_by_all_prefixes() ->
    List = lists:sort(lists:flatmap(fun(MetadataPrefix) ->
        list_by_all_prefixes(#{metadata_prefix => MetadataPrefix})
    end, ozt_handles:supported_metadata_prefixes())),
    [HandleId || {_, _, HandleId} <- List].

list_by_all_prefixes(ListingOpts) ->
    case ozt:rpc(handles, list, [ListingOpts]) of
        {List, undefined} -> List;
        {List, ResumptionToken} -> List ++ list_by_all_prefixes(#{resumption_token => ResumptionToken})
    end.

list_once(ListingOpts) ->
    {Handles, Token} =  ozt:rpc(handles, list, [ListingOpts]),
    {[HandleId || {_, _, HandleId} <- Handles], Token}.

create_handle(HServiceId) ->
    create_handle(HServiceId, ?RAND_METADATA_PREFIX()).

create_handle(HServiceId, MetadataPrefix) ->
    create_handle(HServiceId, MetadataPrefix, ?RAND_TIMESTAMP()).

create_handle(HServiceId, MetadataPrefix, TimeSeconds) ->
    create_handle(HServiceId, MetadataPrefix, TimeSeconds, ?RAND_ID()).

create_handle(HServiceId, MetadataPrefix, TimeSeconds, HandleId) ->
    ozt:rpc(handles, add, [MetadataPrefix, HServiceId, HandleId, TimeSeconds]),
    Handle = #handle_entry{
        timestamp = TimeSeconds,
        metadata_prefix = MetadataPrefix,
        handle_id = HandleId,
        handle_service_id = HServiceId
    },
    update_expected_handles(Handle),
    Handle.

update_handle(HandleId, NewTimeStamp) ->
    #handle_entry{
        timestamp = OldTimeStamp,
        metadata_prefix = MetadataPrefix,
        handle_service_id = HServiceId
    } = Handle = lookup_expected_handle(HandleId),

    ozt:rpc(handles, update_timestamp, [
        MetadataPrefix, HServiceId, HandleId, OldTimeStamp, NewTimeStamp
    ]),
    UpdatedHandle = Handle#handle_entry{timestamp = NewTimeStamp},
    update_expected_handles_after_update(UpdatedHandle).

delete_handle(HandleId) ->
    %% there is no need to update expectations because the test
    %% calling these functions is the final parallel test in this suite
    #handle_entry{
        timestamp = TimeStamp,
        metadata_prefix = MetadataPrefix,
        handle_service_id = HServiceId
    } = lookup_expected_handle(HandleId),
    ozt:rpc(handles, delete, [MetadataPrefix, HServiceId, HandleId, TimeStamp]).

lookup_expected_handle(HandleId) ->
    Expected = load_all_expected_handles(),
    {ok, Handle} = lists_utils:find(
        fun(#handle_entry{handle_id = HId}) ->
            HId == HandleId end, Expected
    ),
    Handle.

lookup_timestamp(HandleId) ->
    Handle = lookup_expected_handle(HandleId),
    Handle#handle_entry.timestamp.


infer_expected_handle_ids(Opts) when is_map(Opts)->
    MetadataPrefix = maps:get(metadata_prefix, Opts),
    From = maps:get(from, Opts, ?EARLIEST_TIMESTAMP),
    Until = maps:get(until, Opts, ?LATEST_TIMESTAMP),
    HService = maps:get(service_id, Opts, undefined),
    AllHandles = load_expected_handles(MetadataPrefix, HService),
    ListFrom = lists:dropwhile(fun(#handle_entry{timestamp = TimeStamp}) -> TimeStamp < From end, AllHandles),
    ListFromUntil = lists:takewhile(fun(#handle_entry{timestamp = TimeStamp}) -> TimeStamp =< Until end, ListFrom),
    [HandleId || #handle_entry{handle_id = HandleId} <- ListFromUntil].

infer_all_expected_handles_ids() ->
    HandlesList = load_all_expected_handles(),
    [HandleId || #handle_entry{handle_id = HandleId} <- HandlesList].


update_expected_handles(NewHandle) ->
    #handle_entry{metadata_prefix = MetadataPrefix, handle_service_id = HServiceId} = NewHandle,
    UpdateHandlesFun = fun(OldExpected) -> lists:sort([NewHandle | OldExpected]) end,
    save_expected_handles(MetadataPrefix, HServiceId, UpdateHandlesFun(load_expected_handles(MetadataPrefix, HServiceId))),
    save_expected_handles(MetadataPrefix, UpdateHandlesFun(load_expected_handles(MetadataPrefix))).


update_expected_handles_after_update(UpdatedHandle) ->
    Entry = lookup_expected_handle(UpdatedHandle#handle_entry.handle_id),
    #handle_entry{metadata_prefix = MetadataPrefix, handle_service_id = HServiceId} = UpdatedHandle,
    UpdateHandlesFun = fun(OldExpected) -> [UpdatedHandle | lists:delete(Entry, OldExpected)] end,
    save_expected_handles(MetadataPrefix, HServiceId, UpdateHandlesFun(load_expected_handles(MetadataPrefix, HServiceId))),
    save_expected_handles(MetadataPrefix, UpdateHandlesFun(load_expected_handles(MetadataPrefix))).


load_all_expected_handles() ->
    load_expected_handles(?OAI_DC_METADATA_PREFIX) ++ load_expected_handles(?EDM_METADATA_PREFIX).


%% expected handles are stored on the first oz-worker node to allow
%% rerunning tests with --no-clean option
load_expected_handles(MetadataPrefix) ->
    load_expected_handles(MetadataPrefix, undefined).
load_expected_handles(MetadataPrefix, HServiceId) ->
    ozt:rpc(?OZ_RPC_FIRST_NODE(), node_cache, get,
        [<<MetadataPrefix/binary, (str_utils:to_binary(HServiceId))/binary>>, []]).


save_expected_handles(MetadataPrefix, HandlesList) ->
    save_expected_handles(MetadataPrefix, undefined, HandlesList).
save_expected_handles(MetadataPrefix, HServiceId, HandlesList) ->
    ozt:rpc(?OZ_RPC_FIRST_NODE(), node_cache, put,
        [<<MetadataPrefix/binary, (str_utils:to_binary(HServiceId))/binary>>, HandlesList]).


clear_expected_handles(MetadataPrefix) ->
    clear_expected_handles(MetadataPrefix, undefined).
clear_expected_handles(MetadataPrefix, HServiceId) ->
    ozt:rpc(?OZ_RPC_FIRST_NODE(), node_cache, clear, [
        <<MetadataPrefix/binary, (str_utils:to_binary(HServiceId))/binary>>]).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ozt:init_per_suite(Config, fun() ->
        lists:foreach(fun(#handle_entry{handle_id = HandleID}) ->
            delete_handle(HandleID)
        end, load_all_expected_handles()),
        lists:foreach(fun(MetadataPrefix) -> clear_expected_handles(MetadataPrefix) end,
            [?OAI_DC_METADATA_PREFIX, ?EDM_METADATA_PREFIX]),
        lists:foreach(fun(MetadataPrefix) ->
            lists:foreach(fun(HServiceId) ->
                clear_expected_handles(MetadataPrefix, HServiceId)
            end, [?FIRST_HSERVICE, ?ANOTHER_HSERVICE, ?SMALL_HSERVICE])
        end, [?OAI_DC_METADATA_PREFIX, ?EDM_METADATA_PREFIX]),
        utils:repeat(?HANDLE_COUNT_IN_SMALL_HSERVICE, fun() -> create_handle(?SMALL_HSERVICE) end),
        utils:repeat(?TOTAL_HANDLE_COUNT - ?HANDLE_COUNT_IN_SMALL_HSERVICE,
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
