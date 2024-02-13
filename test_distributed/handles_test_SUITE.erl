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

-export([
    groups/0, all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    resumption_token_test/1,
    list_all_handle_test/1,
    list_handles_from_services_test/1,
    list_handles_with_metadata_format_test/1,
    list_size_elements_test/1,
    add_element_that_already_exist_test/1,
    list_from_until_test/1,
    list_no_resumption_token_test/1,
    add_handle_to_service_test/1,
    add_handle_with_earlier_timestamp_test/1,
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
        list_no_resumption_token_test,
        list_all_handle_test,
        list_handles_from_services_test,
        list_handles_with_metadata_format_test,
        list_size_elements_test,
        list_from_until_test,
        add_element_that_already_exist_test
    ]},

    {sequential_tests, [sequential], [
        add_handle_to_service_test,
        add_handle_with_earlier_timestamp_test,
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

-define(OAI_DC_METADATA_PREFIX, <<"oai_dc">>).
-define(EDM_METADATA_PREFIX, <<"edm">>).
-define(RAND_METADATA_PREFIX(), case ?RAND_BOOL() of
    true -> ?OAI_DC_METADATA_PREFIX;
    false -> ?EDM_METADATA_PREFIX
end).

-define(FIRST_HSERVICE, <<"first">>).
-define(ANOTHER_HSERVICE, <<"another">>).
% the list of handles for this service is not modified during the tests for it to remain small (less than 1000 handles)
-define(SMALL_HSERVICE, <<"small">>).
-define(RAND_SERVICE(), case ?RAND_BOOL() of % see above
    true -> ?FIRST_HSERVICE;
    false -> ?ANOTHER_HSERVICE
end).

-define(checkListing(Opts), ?assertEqual(length(expected_handles(Opts)), length(list_all(Opts)))).

-record(handle_entry, {
    timestamp :: od_handle:timestamp_seconds(),
    metadata_prefix :: binary(),
    handle_id :: od_handle:id(),
    service :: undefined | binary()
}).


%%%===================================================================
%%% Tests
%%%===================================================================


resumption_token_test(_Config) ->
    DefaultListLimit = 1000,

    %% first listing, no resumption_token
    {List1, Token1} = list_once(#{metadata_prefix => ?OAI_DC_METADATA_PREFIX}),
    ?assertEqual(DefaultListLimit, length(List1)),

    %% second listing, resumption token from first listing
    {List2, Token2} = list_once(#{resumption_token => Token1, metadata_prefix => ?OAI_DC_METADATA_PREFIX}),
    ?assertEqual(undefined, Token2),
    ?assertEqual(expected_handles(?OAI_DC_METADATA_PREFIX), List1 ++ List2),
    %% third listing, other prefix
    {List3, Token3} = list_once(#{metadata_prefix => ?EDM_METADATA_PREFIX}),
    ?assertEqual(DefaultListLimit, length(List3)),

    {List4, Token4} = list_once(#{resumption_token => Token3, metadata_prefix => ?EDM_METADATA_PREFIX}),
    ?assertEqual(undefined, Token4),
    ?assertEqual(expected_handles(?EDM_METADATA_PREFIX), List3 ++ List4),
    ?assertEqual(3300, length(List1) + length(List2) + length(List3) + length(List4)).


list_no_resumption_token_test(_Config) ->
    MetadataPrefix =  ?RAND_METADATA_PREFIX(),
    {List, undefined} = list_once(#{service_id => ?SMALL_HSERVICE, metadata_prefix => MetadataPrefix}),
    ?assertEqual(expected_handles(MetadataPrefix, ?SMALL_HSERVICE), List).


list_all_handle_test(_Config) ->
    ListAll = list_all(),
    ?assertEqual(?TOTAL_HANDLE_COUNT, length(ListAll)),
    ?assertEqual(lists:sort(expected_handles(all)), lists:sort(ListAll)).


list_handles_from_services_test(_Config) ->
    ListFirstHServiceDC = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => ?OAI_DC_METADATA_PREFIX}),
    ListFirstHServiceEDM = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => ?EDM_METADATA_PREFIX}),

    ListAnotherHServiceDC = list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => ?OAI_DC_METADATA_PREFIX}),
    ListAnotherHServiceEDM = list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => ?EDM_METADATA_PREFIX}),
    ListSmallHServiceDC = list_all(#{service_id => ?SMALL_HSERVICE, metadata_prefix => ?OAI_DC_METADATA_PREFIX}),
    ListSmallHServiceEDM = list_all(#{service_id => ?SMALL_HSERVICE, metadata_prefix => ?EDM_METADATA_PREFIX}),

    ?assertEqual(expected_handles(?OAI_DC_METADATA_PREFIX, ?SMALL_HSERVICE), ListSmallHServiceDC),
    ?assertEqual(expected_handles(?EDM_METADATA_PREFIX, ?SMALL_HSERVICE), ListSmallHServiceEDM),
    ?assertEqual(lists:sort(expected_handles(?OAI_DC_METADATA_PREFIX)),
        lists:sort(ListFirstHServiceDC ++ ListAnotherHServiceDC ++ ListSmallHServiceDC)),
    ?assertEqual(lists:sort(expected_handles(?EDM_METADATA_PREFIX)),
        lists:sort(ListFirstHServiceEDM ++ ListAnotherHServiceEDM ++ ListSmallHServiceEDM)),
    ?assertEqual(
        ?TOTAL_HANDLE_COUNT,
        length(ListFirstHServiceDC) + length(ListFirstHServiceEDM)
            + length(ListAnotherHServiceDC) + length(ListAnotherHServiceEDM)
            + length(ListSmallHServiceDC) + length(ListSmallHServiceEDM)
    ).


list_handles_with_metadata_format_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ActualList = list_all(#{metadata_prefix => MetadataPrefix}),
    ?assertEqual(expected_handles(MetadataPrefix), ActualList),

    ActualListMetadataAndService = list_all(#{
        metadata_prefix => MetadataPrefix,
        service_id => ?SMALL_HSERVICE
    }),
    ?assertEqual(expected_handles(MetadataPrefix, ?SMALL_HSERVICE), ActualListMetadataAndService).


list_size_elements_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    {List, undefined} = list_once(#{size => 5, metadata_prefix => MetadataPrefix}),
    ?assertEqual(lists:sublist(expected_handles(MetadataPrefix), 5), List),
    ?assertEqual(5, length(List)).


add_element_that_already_exist_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ListService = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}),
    BeforeAdding = length(ListService),
    HandleId = hd(ListService),
    TimeStamp = lookup_timestamp(HandleId),
    try
        create_handle(?FIRST_HSERVICE, MetadataPrefix, TimeStamp, HandleId)
    catch
        error:ErrorReason:_Stacktrace ->
            ?assertEqual(ErrorReason, {badrpc, ?ERROR_ALREADY_EXISTS})
    end,
    AfterAdding = length(list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix})),
    ?assertEqual(BeforeAdding, AfterAdding).


list_from_until_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    % list from
    OptsFrom = #{from => generate_rand_timestamp(), metadata_prefix => MetadataPrefix},
    ?checkListing(OptsFrom),

    % list until
    OptsUntil = #{until => generate_rand_timestamp(), metadata_prefix => MetadataPrefix},
    ?checkListing(OptsUntil),

    % list from to until
    [From2, Until2] = lists:sort([generate_rand_timestamp(), generate_rand_timestamp()]),
    ?checkListing(#{from => From2, until => Until2, metadata_prefix => MetadataPrefix}),

    % what if until is equal to from and it is the moment of creating first
    #handle_entry{timestamp = TimeStampSeconds, handle_id = HandleId} =
        hd(get_expected_handles(MetadataPrefix)),
    [HandleIdFromList] = FullListFromUntilEqual = list_all(#{from => TimeStampSeconds,
        until => TimeStampSeconds, metadata_prefix => MetadataPrefix}),
    ?assert(1 =< length(FullListFromUntilEqual)),
    ?assertEqual(HandleId, HandleIdFromList),

    % from greater than until
    FromGreater = Until2,
    UntilGreater = From2,
    FullListFromGreaterThanUntil = list_all(#{from => FromGreater,
        until => UntilGreater, metadata_prefix => MetadataPrefix}),
    ?assertEqual([], FullListFromGreaterThanUntil),

    % from and until outside the range of available dates
    % rand timestamp +/- 10 years
    FromOutside = generate_rand_timestamp() - 3600 * 24 * 365 * 10,
    UntilOutside = generate_rand_timestamp() + 3600 * 24 * 365 * 10,
    ?checkListing(#{from => FromOutside, until => UntilOutside, metadata_prefix => MetadataPrefix}).


add_handle_to_service_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    BeforeAddingService1 = length(list_all(
        #{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}
    )),
    BeforeAddingService2 = length(list_all(
        #{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}
    )),
    BeforeAddingAll = length(list_all(#{metadata_prefix => MetadataPrefix})),
    create_handle(?FIRST_HSERVICE, MetadataPrefix),

    ?assertEqual(BeforeAddingAll + 1, length(list_all(#{metadata_prefix => MetadataPrefix}))),

    lists:foreach(fun(_) -> create_handle(?ANOTHER_HSERVICE, MetadataPrefix) end,
        lists:seq(1, 10)),

    ?assertEqual(BeforeAddingService1 + 1,
        length(list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(BeforeAddingService2 + 10,
        length(list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(BeforeAddingAll + 11, length(list_all(#{metadata_prefix => MetadataPrefix}))).


%% checks if handles are added sorted by date
add_handle_with_earlier_timestamp_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Service = ?RAND_SERVICE(),
    FirstHandle = hd(list_all(#{metadata_prefix => MetadataPrefix})),
    RandNumber = ?RAND_INT(1, 10000),
    TimeStamp = lookup_timestamp(FirstHandle) - RandNumber,
    [#handle_entry{handle_id = HandleId3}, #handle_entry{handle_id = HandleId2},
        #handle_entry{handle_id = HandleId1}] =
        lists:map(fun(Number) ->
            create_handle(Service, MetadataPrefix, TimeStamp - Number)
        end, lists:seq(1, 3)),

    {List, _Token} = list_once(#{metadata_prefix => MetadataPrefix}),
    [HId1, HId2, HId3] = lists:sublist(List, 3),
    ?assertEqual(HandleId1, HId1),
    ?assertEqual(HandleId2, HId2),
    ?assertEqual(HandleId3, HId3).


update_handle_timestamp_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    HandleId = lists:last(list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix})),
    NewTimeStamp = ?EARLIEST_TIMESTAMP - ?RAND_INT(1, 10000),

    % after updating timestamp to ?EARLIEST_TIMESTAMP - ?RAND_INT(1, 10000) this handle will be at the beginning
    update_handle(HandleId, NewTimeStamp),

    EarliestHID = hd(list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix})),
    ?assertEqual(EarliestHID, HandleId).


delete_handle_from_service_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ListAll = list_all(#{metadata_prefix => MetadataPrefix}),
    ListService1 = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}),
    ListService2 = list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}),

    lists:foreach(fun(HandleId1) ->
        delete_handle(HandleId1)
    end, lists:sublist(ListService1, 10)),

    ?assertEqual(length(ListAll) - 10, length(list_all(#{metadata_prefix => MetadataPrefix}))),

    [HandleId2 | _RestList] = ListService2,
    delete_handle(HandleId2),

    ?assertEqual(length(ListService1) - 10,
        length(list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(length(ListService2) - 1,
        length(list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(length(ListAll) - 11, length(list_all(#{metadata_prefix => MetadataPrefix}))).


%%%===================================================================
%%% Helpers
%%%===================================================================

list_all() ->
    lists:flatmap(fun(MetadataPrefix) ->
        list_all(#{metadata_prefix => MetadataPrefix})
    end, metadata_formats:supported_formats()).
list_all(ListingOpts) ->
    Until = maps:get(until, ListingOpts, <<>>),
    {List, Token} =
        list_once(ListingOpts),
    case Token == <<>> orelse Token == undefined of
        true -> List;
        false -> list_all(ListingOpts, Token, List, Until)
    end.
list_all(ListingOpts, Token, ListAll, Until) ->
    {List, NewToken} =
        list_once(ListingOpts, Token, Until),
    NewList = ListAll ++ List,
    case NewToken == <<>> orelse NewToken == undefined of
        true -> NewList;
        false -> list_all(ListingOpts, NewToken, NewList, Until)
    end.

list_once(ListingOpts) ->
    ozt:rpc(handles, list, [ListingOpts]).
list_once(ListingOpts, Token, Until) ->
    NewListingOpts = maps:merge(ListingOpts,
        #{resumption_token => Token, until => Until}),
    list_once(NewListingOpts).

generate_rand_timestamp() ->
    ?RAND_INT(?EARLIEST_TIMESTAMP, ?LATEST_TIMESTAMP).

create_handle(Service) ->
    create_handle(Service, ?RAND_METADATA_PREFIX()).

create_handle(Service, MetadataPrefix) ->
    create_handle(Service, MetadataPrefix, generate_rand_timestamp()).

create_handle(Service, MetadataPrefix, TimeSeconds) ->
    create_handle(Service, MetadataPrefix, TimeSeconds, ?RAND_ID()).

create_handle(Service, MetadataPrefix, TimeSeconds, HandleId) ->
    ozt:rpc(handles, add, [MetadataPrefix, Service, HandleId, TimeSeconds]),
    Handle = #handle_entry{
        timestamp = TimeSeconds,
        metadata_prefix = MetadataPrefix,
        handle_id = HandleId,
        service = Service
    },
    update_expected_handles([Handle]),
    Handle.

update_handle(HandleId, NewTimeStamp) ->
    #handle_entry{
        timestamp = OldTimeStamp,
        metadata_prefix = MetadataPrefix,
        service = Service
    } = Handle = lookup_handle(HandleId),

    ozt:rpc(handles, update_timestamp, [
        MetadataPrefix, Service, HandleId, OldTimeStamp, NewTimeStamp
    ]),
    UpdatedHandle = Handle#handle_entry{timestamp = NewTimeStamp},
    update_expected_handles_after_update(UpdatedHandle).

delete_handle(HandleId) ->
    #handle_entry{
        timestamp = TimeStamp,
        metadata_prefix = MetadataPrefix,
        service = Service
    } = lookup_handle(HandleId),
    ozt:rpc(handles, delete, [MetadataPrefix, Service, HandleId, TimeStamp]).

lookup_handle(HandleId) ->
    Expected = get_expected_handles(all),
    [Handle] = lists:filter(
        fun(#handle_entry{handle_id = HId}) ->
            HId == HandleId end, Expected
    ),
    Handle.

lookup_timestamp(HandleId) ->
    Handle = lookup_handle(HandleId),
    Handle#handle_entry.timestamp.

expected_handles(Opts) when is_map(Opts)->
    MetadataPrefix = maps:get(metadata_prefix, Opts),
    From = maps:get(from, Opts, ?EARLIEST_TIMESTAMP),
    Until = maps:get(until, Opts, ?LATEST_TIMESTAMP),
    AllHandles = get_expected_handles(MetadataPrefix),
    ListFrom = lists:dropwhile(fun(#handle_entry{timestamp = TimeStamp}) -> TimeStamp < From end, AllHandles),
    ListFromUntil = lists:takewhile(fun(#handle_entry{timestamp = TimeStamp}) -> TimeStamp =< Until end, ListFrom),
    [HandleId || #handle_entry{handle_id = HandleId} <- ListFromUntil];

expected_handles(all) ->
    HandlesList = get_expected_handles(all),
    [HandleId || #handle_entry{handle_id = HandleId} <- HandlesList];
expected_handles(MetadataPrefix) ->
    HandlesList = get_expected_handles(MetadataPrefix),
    [HandleId || #handle_entry{handle_id = HandleId} <- HandlesList].
expected_handles(MetadataPrefix, ?SMALL_HSERVICE) ->
    HandlesList = get_expected_handles(MetadataPrefix, ?SMALL_HSERVICE),
    [HandleId || #handle_entry{handle_id = HandleId} <- HandlesList].

update_expected_handles(NewHandles) ->
    #handle_entry{metadata_prefix = MetadataPrefix, service = Service} = hd(NewHandles),
    case Service of
        ?SMALL_HSERVICE -> update_expected_handles(NewHandles, ?SMALL_HSERVICE);
        _ -> ok
    end,
    ToUpdate = [all, MetadataPrefix],
    lists:foreach(fun(ListToUpdate) ->
        OldHandles = get_expected_handles(ListToUpdate),
        UpdatedHandles = sort_expected_handles(OldHandles, NewHandles),
        clear_expected_handles(ListToUpdate),
        save_expected_handles(ListToUpdate, UpdatedHandles)
    end, ToUpdate).

update_expected_handles(NewHandles, ?SMALL_HSERVICE) ->
    #handle_entry{metadata_prefix = MetadataPrefix, service = Service} = hd(NewHandles),
    OldHandles = get_expected_handles(MetadataPrefix, Service),
    UpdatedHandles = sort_expected_handles(OldHandles, NewHandles),
    clear_expected_handles(MetadataPrefix, Service),
    save_expected_handles(MetadataPrefix, Service, UpdatedHandles).

sort_expected_handles(OldHandles, NewHandles) ->
    lists:sort(fun(
        #handle_entry{timestamp = TimeSeconds1},
        #handle_entry{timestamp = TimeSeconds2}) ->
        TimeSeconds1 < TimeSeconds2
    end, OldHandles ++ NewHandles).

get_list_without_element_with_handle_id(List, HandleId) ->
    [Handle || Handle <- List, Handle#handle_entry.handle_id =/= HandleId].

update_expected_handles_after_update(UpdatedHandle) ->
    MetadataPrefix = UpdatedHandle#handle_entry.metadata_prefix,
    ToUpdate = [all, MetadataPrefix],
    lists:foreach(fun(ElementToUpdate) ->
        NewHandles = [UpdatedHandle | get_list_without_element_with_handle_id(
            get_expected_handles(ElementToUpdate), UpdatedHandle#handle_entry.handle_id)],
        clear_expected_handles(ElementToUpdate),
        save_expected_handles(ElementToUpdate, NewHandles)
        end, ToUpdate).


get_expected_handles(all) ->
    ozt:rpc(hd(lists:sort(ozt:get_nodes())), node_cache, get, [all, []]);
get_expected_handles(MetadataPrefix) ->
    ozt:rpc(hd(lists:sort(ozt:get_nodes())), node_cache, get, [MetadataPrefix, []]).
get_expected_handles(MetadataPrefix, ?SMALL_HSERVICE) ->
    ozt:rpc(hd(lists:sort(ozt:get_nodes())), node_cache, get,
        [<<MetadataPrefix/binary, <<"_hservice">>/binary>>, []]).


save_expected_handles(all, HandlesList) ->
    ozt:rpc(hd(lists:sort(ozt:get_nodes())), node_cache, put, [all, HandlesList]);
save_expected_handles(MetadataPrefix, HandlesList) ->
    ozt:rpc(hd(lists:sort(ozt:get_nodes())), node_cache, put, [MetadataPrefix, HandlesList]).
save_expected_handles(MetadataPrefix, ?SMALL_HSERVICE, HandlesList) ->
    ozt:rpc(hd(lists:sort(ozt:get_nodes())), node_cache, put,
        [<<MetadataPrefix/binary, <<"_hservice">>/binary>>, HandlesList]).


clear_expected_handles(all) ->
    ozt:rpc(hd(lists:sort(ozt:get_nodes())), node_cache, clear, [all]);
clear_expected_handles(MetadataPrefix) ->
    ozt:rpc(hd(lists:sort(ozt:get_nodes())), node_cache, clear, [MetadataPrefix]).
clear_expected_handles(MetadataPrefix, ?SMALL_HSERVICE) ->
    ozt:rpc(hd(lists:sort(ozt:get_nodes())), node_cache, clear,
        [<<MetadataPrefix/binary, <<"_hservice">>/binary>>]).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ozt:init_per_suite(Config, fun() ->
        lists:foreach(fun(#handle_entry{handle_id = HandleID}) ->
            delete_handle(HandleID)
        end, get_expected_handles(all)),
        ListToClear = [all, ?OAI_DC_METADATA_PREFIX, ?EDM_METADATA_PREFIX,
            <<"small_hservice">>, <<"oai_dc_hservice">>, <<"edm_hservice">>],
        lists:foreach(fun(List) -> clear_expected_handles(List) end, ListToClear),

        lists:foreach(fun(_) -> create_handle(?SMALL_HSERVICE) end,
            lists:seq(1, max(0, ?HANDLE_COUNT_IN_SMALL_HSERVICE))),
        lists:foreach(fun(_) -> create_handle(?RAND_SERVICE()) end,
            lists:seq(1, max(0, ?TOTAL_HANDLE_COUNT - ?HANDLE_COUNT_IN_SMALL_HSERVICE)))
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
