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
    delete_handle_from_service_test/1
]).

all() -> [
    {group, parallel_tests},
    {group, sequential_tests}
].

groups() -> [
    {parallel_tests, [parallel], [
        resumption_token_test,
        list_all_handle_test,
        list_handles_from_services_test,
        list_handles_with_metadata_format_test,
        list_size_elements_test,
        add_element_that_already_exist_test
    ]},
    {sequential_tests, [sequential], [
        list_from_until_test,
        list_no_resumption_token_test,
        add_handle_to_service_test,
        add_handle_with_earlier_timestamp_test,
        delete_handle_from_service_test
    ]}
].

-define(HANDLE_COUNT, 3300).
-define(HANDLE_COUNT_IN_SMALL_HSERVICE, 600).
-define(EARLIEST_TIMESTAMP, 1200000000).
-define(LATEST_TIMESTAMP, 1700000000).
-define(FIRST_HSERVICE, <<"first">>).
-define(ANOTHER_HSERVICE, <<"another">>).
-define(SMALL_HSERVICE, <<"small">>).

-define(RAND_NAME(), ?RAND_UNICODE_STR(200)).
-define(RAND_ID(), str_utils:rand_hex(16)).
-define(RAND_SERVICE(), case ?RAND_BOOL() of
    true -> ?FIRST_HSERVICE;
    false -> ?ANOTHER_HSERVICE
end).
-define(RAND_METADATA_PREFIX(), case ?RAND_BOOL() of
    true -> <<"dc">>;
    false -> <<"edm">>
end).
-define(INDEX_SEP, 0).


%%%===================================================================
%%% Tests
%%%===================================================================


resumption_token_test(_Config) ->
    DefaultListLimit = 1000,

    %% first listing, no resumption_token
    {List1, Token1} = ozt:rpc(handles, list, [#{}]),
    ?assertEqual(DefaultListLimit, length(List1)),

    %% second listing, resumption token from 1 listing
    {List2, Token2} = ozt:rpc(
        handles, list, [#{resumption_token => Token1}]
    ),
    ?assertEqual(DefaultListLimit, length(List2)),

    %% third listing, resumption token from 2 listing
    {List3, Token3} = ozt:rpc(
        handles, list, [#{resumption_token => Token2}]
    ),
    ?assertEqual(DefaultListLimit, length(List3)),

    %% fourth listing is the last one, so token is <<>>, and list length is rest so 300
    {List4, Token4} = ozt:rpc(
        handles, list, [#{resumption_token => Token3}]
    ),
    ?assertEqual(<<>>, Token4),
    ?assertEqual(300, length(List4)),

    ?assertEqual(3300, length(List1) + length(List2) + length(List3) + length(List4)).


list_all_handle_test(_Config) ->
    ListAll = list_all(#{}),
    ?assertEqual(?HANDLE_COUNT, length(ListAll)).


list_handles_from_services_test(_Config) ->
    ListFirstHService = list_all(#{service_id => ?FIRST_HSERVICE}),
    ListAnotherHService = list_all(#{service_id => ?ANOTHER_HSERVICE}),
    ListSmallHService = list_all(#{service_id => ?SMALL_HSERVICE}),
    ?assertEqual(
        ?HANDLE_COUNT,
        length(ListFirstHService) + length(ListAnotherHService) + length(ListSmallHService)
    ).


list_handles_with_metadata_format_test(_Config) ->
    MetadataFormat = ?RAND_METADATA_PREFIX(),
    ActualList = list_all(#{metadata_prefix => MetadataFormat}),
    ExpectedList = lists:filtermap(
        fun({_, HandleId, _, MetadataPrefix}) ->
            case MetadataPrefix == MetadataFormat of
                true -> {true, HandleId};
                false -> false
            end
        end, node_cache:get(all_handles)
    ),
    ?assertEqual(ExpectedList, ActualList),

    ActualListMetadataAndService = list_all(#{
        metadata_prefix => MetadataFormat,
        service_id => ?SMALL_HSERVICE
    }),
    ExpectedListMetadataAndService = lists:filtermap(
        fun({_, HandleId, _, MetadataPrefix}) ->
            case MetadataPrefix == MetadataFormat of
                true -> {true, HandleId};
                false -> false
            end
        end, node_cache:get(small_hservice)
    ),
    ?assertEqual(ActualListMetadataAndService, ExpectedListMetadataAndService).


list_size_elements_test(_Config) ->
    ListingOpts = #{size => 5},
    {List, undefined} = ozt:rpc(handles, list, [ListingOpts]),
    SubList = lists:sublist(node_cache:get(all_handles), 5),
    ExpectedList = lists:map(
        fun({_, HandleId, _, _}) -> HandleId end, SubList
    ),
    ?assertEqual(ExpectedList, List),
    ?assertEqual(5, length(List)).


add_element_that_already_exist_test(_Config) ->
    ListService = list_all(#{service_id => ?FIRST_HSERVICE}),
    BeforeAdding = length(ListService),
    HandleId = hd(ListService),
    TimeStamp = get_elem_from_handle_id(HandleId, timestamp),
    MetadataPrefix = get_elem_from_handle_id(HandleId, metadata_prefix),
    try
        ozt:rpc(handles, add, [TimeStamp, HandleId, ?FIRST_HSERVICE, MetadataPrefix])
    catch
        error:ErrorReason:_Stacktrace ->
            ?assertEqual(ErrorReason, {badrpc, ?ERROR_ALREADY_EXISTS})
    end,
    AfterAdding = length(list_all(#{service_id => ?FIRST_HSERVICE})),
    ?assertEqual(BeforeAdding, AfterAdding).


list_from_until_test(_Config) ->
    % list from
    From1 = generate_rand_timestamp(),
    ListingOptsFrom = #{from => From1},
    ListFrom = ozt:rpc(handles, list, [ListingOptsFrom]),
    FullListFrom = get_full_list(ListFrom),
    ExpectedListFrom = expected_handles(From1, ?LATEST_TIMESTAMP),
    ?assertEqual(ExpectedListFrom, FullListFrom),

    % list until
    Until1 = generate_rand_timestamp(),
    ListingOptsUntil = #{until => Until1},
    ListUntil = ozt:rpc(handles, list, [ListingOptsUntil]),
    FullListUntil = get_full_list(ListUntil, Until1),
    ExpectedListUntil = expected_handles(?EARLIEST_TIMESTAMP, Until1),
    ?assertEqual(ExpectedListUntil, FullListUntil),

    % list from to until
    Rand1 = generate_rand_timestamp(),
    Rand2 = generate_rand_timestamp(),
    {From2, Until2} = case Rand1 < Rand2 of
        true -> {Rand1, Rand2};
        false -> {Rand2, Rand1}
    end,
    ListingOptsFromUntil = #{from => From2, until => Until2},
    ListFromUntil = ozt:rpc(handles, list, [ListingOptsFromUntil]),
    FullListFromUntil = get_full_list(ListFromUntil, Until2),
    ExpectedListFromUntil = expected_handles(From2, Until2),
    ?assertEqual(ExpectedListFromUntil, FullListFromUntil),

    % what if until is equal to from and it is the moment of creating first
    [{TimeStampSeconds, HandleId, _, _} | _Rest] = node_cache:get(all_handles),
    ListingOptsFromUntilEqual = #{from => TimeStampSeconds, until => TimeStampSeconds},
    ListFromUntilEqual = ozt:rpc(handles, list, [ListingOptsFromUntilEqual]),
    [HandleIdFromList] = FullListFromUntilEqual = get_full_list(ListFromUntilEqual, TimeStampSeconds),
    ?assertEqual(1, length(FullListFromUntilEqual)),
    ?assertEqual(HandleId, HandleIdFromList),

    % from greater than until
    FromGreater = Until2,
    UntilGreater = From2,
    ListingOptsFromGreaterThanUntil = #{from => FromGreater, until => UntilGreater},
    ListFromGreaterThanUntil = ozt:rpc(handles, list, [ListingOptsFromGreaterThanUntil]),
    FullListFromGreaterThanUntil = get_full_list(ListFromGreaterThanUntil, UntilGreater),
    ?assertEqual(0, length(FullListFromGreaterThanUntil)),
    ?assertEqual([], FullListFromGreaterThanUntil),

    % from and until outside the range of available dates
    % rand timestamp +/- 10 years
    FromOutside = generate_rand_timestamp() - 3600 * 24 * 365 * 10,
    UntilOutside = generate_rand_timestamp() + 3600 * 24 * 365 * 10,
    ListingOptsOutside = #{from => FromOutside, until => UntilOutside},
    ListOutside = ozt:rpc(handles, list, [ListingOptsOutside]),
    FullListOutside = get_full_list(ListOutside, UntilOutside),
    ExpectedListOutside = expected_handles(FromOutside, UntilOutside),
    ?assertEqual(ExpectedListOutside, FullListOutside).


list_no_resumption_token_test(_Config) ->
    {List, undefined} = ozt:rpc(handles, list, [#{service_id => ?SMALL_HSERVICE}]),
    ?assertEqual(?HANDLE_COUNT_IN_SMALL_HSERVICE, length(List)).


add_handle_to_service_test(_Config) ->
    BeforeAddingService1 = length(list_all(#{service_id => ?FIRST_HSERVICE})),
    BeforeAddingService2 = length(list_all(#{service_id => ?ANOTHER_HSERVICE})),
    BeforeAddingAll = length(list_all()),

    {TimeStamp, HandleId, Service, MetadataPrefix} = create_handle_for_service(?FIRST_HSERVICE),

    ?assertEqual(BeforeAddingAll + 1, length(list_all())),

    HandleList = lists_utils:pmap(fun(_) ->
        create_handle_for_service(?ANOTHER_HSERVICE)
    end, lists:seq(1, 10)),
    update_node_cache(HandleList ++ [{TimeStamp, HandleId, Service, MetadataPrefix}]),

    ?assertEqual(BeforeAddingService1 + 1,
        length(list_all(#{service_id => ?FIRST_HSERVICE}))),
    ?assertEqual(BeforeAddingService2 + 10,
        length(list_all(#{service_id => ?ANOTHER_HSERVICE}))),
    ?assertEqual(BeforeAddingAll + 11, length(list_all())).


%% checks if handles are added sorted by data
add_handle_with_earlier_timestamp_test(_Config) ->
    % generate_rand_timestamp generates dates from 2008-2023
    % so after generating date and subtracting 16 years, we are sure we have
    % date earlier than rest on the list
    TimeStamp = generate_rand_timestamp(),
    [{_, HandleId3, _, _}, {_, HandleId2, _, _}, {_, HandleId1, _, _}] = NewHandles =
        lists:map(fun(Number) ->
            OldTimestamp = TimeStamp - (18 + Number) * 3600 * 24 * 365,
            HandleId = ?RAND_ID(),
            Service = ?RAND_SERVICE(),
            MetadataPrefix = ?RAND_METADATA_PREFIX(),
            ozt:rpc(handles, add, [OldTimestamp, HandleId, Service, MetadataPrefix]),
            {OldTimestamp, HandleId, Service, MetadataPrefix}
        end, lists:seq(1, 3)
        ),

    update_node_cache(NewHandles),

    {List, _Token} = ozt:rpc(handles, list, [#{}]),
    [HId1, HId2, HId3] = lists:sublist(List, 3),
    ?assertEqual(HandleId1, HId1),
    ?assertEqual(HandleId2, HId2),
    ?assertEqual(HandleId3, HId3).


delete_handle_from_service_test(_Config) ->
    ListAll = list_all(),
    ListService1 = list_all(#{service_id => ?FIRST_HSERVICE}),
    ListService2 = list_all(#{service_id => ?ANOTHER_HSERVICE}),

    lists:foreach(fun(HandleId1) ->
        ozt:rpc(handles, delete, [
            get_elem_from_handle_id(HandleId1, timestamp), HandleId1,
            ?FIRST_HSERVICE, get_elem_from_handle_id(HandleId1, metadata_prefix)
        ])
    end, lists:sublist(ListService1, 10)),

    ?assertEqual(length(ListAll) - 10, length(list_all())),

    [HandleId2 | _RestList] = ListService2,
    ozt:rpc(handles, delete, [
        get_elem_from_handle_id(HandleId2, timestamp), HandleId2,
        ?ANOTHER_HSERVICE, get_elem_from_handle_id(HandleId2, metadata_prefix)
    ]),

    ?assertEqual(length(ListService1) - 10,
        length(list_all(#{service_id => ?FIRST_HSERVICE}))),
    ?assertEqual(length(ListService2) - 1,
        length(list_all(#{service_id => ?ANOTHER_HSERVICE}))),
    ?assertEqual(length(ListAll) - 11, length(list_all())).


%%%===================================================================
%%% Helpers
%%%===================================================================

get_full_list(List) ->
    case List of
        {NotFullList, undefined} -> NotFullList;
        {NotFullList, Token} -> list_all(#{}, Token, NotFullList, <<>>)
    end.

get_full_list(List, Until) ->
    case List of
        {NotFullList, undefined} -> NotFullList;
        {NotFullList, Token} -> list_all(#{}, Token, NotFullList, Until)
    end.

list_all() ->
    list_all(#{}).
list_all(ListingOpts) ->
    {List, Token} =
        list_with_opts(ListingOpts),
    case Token == <<>> orelse Token == undefined of
        true -> List;
        false -> list_all(ListingOpts, Token, List, <<>>)
    end.
list_all(ListingOpts, Token, ListAll, Until) ->
    {List, NewToken} =
        list_with_opts(ListingOpts, Token, Until),
    NewList = ListAll ++ List,
    case NewToken == <<>> orelse NewToken == undefined of
        true -> NewList;
        false -> list_all(ListingOpts, NewToken, NewList, Until)
    end.

list_with_opts(ListingOpts) ->
    ozt:rpc(handles, list, [ListingOpts]).
list_with_opts(ListingOpts, Token, Until) ->
    NewListingOpts = maps:merge(ListingOpts,
        #{resumption_token => Token, until => Until}),
    ozt:rpc(handles, list, [NewListingOpts]).

generate_rand_timestamp() ->
    rand:uniform(
        ?LATEST_TIMESTAMP - ?EARLIEST_TIMESTAMP + 1
    ) + ?EARLIEST_TIMESTAMP - 1.

create_handle() ->
    TimeSeconds = generate_rand_timestamp(),
    HandleId = ?RAND_ID(),
    HandleService = ?RAND_SERVICE(),
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ozt:rpc(handles, add, [TimeSeconds, HandleId, HandleService, MetadataPrefix]),
    {TimeSeconds, HandleId, HandleService, MetadataPrefix}.

create_handle_for_service(Service) ->
    TimeSeconds = generate_rand_timestamp(),
    HandleId = ?RAND_ID(),
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ozt:rpc(handles, add, [TimeSeconds, HandleId, Service, MetadataPrefix]),
    {TimeSeconds, HandleId, Service, MetadataPrefix}.

get_elem_from_handle_id(HandleId, Atom) ->
    Expected = node_cache:get(all_handles),
    [{TimeStamp, _HId, _HSId, MetadataPrefix}] = lists:filter(
        fun({_TimeStamp, HId, _HSId, _MetadataPrefix}) -> HId == HandleId end, Expected
    ),
    Elem = case Atom of
        timestamp -> TimeStamp;
        metadata_prefix -> MetadataPrefix
    end,
    Elem.


update_node_cache(NewHandles) ->
    OldHandles = node_cache:get(all_handles),
    UpdatedHandles = OldHandles ++ NewHandles,
    node_cache:clear(all_handles),
    node_cache:put(all_handles, lists:sort(
        fun({TimeSeconds1, _, _, _}, {TimeSeconds2, _, _, _}) ->
            TimeSeconds1 < TimeSeconds2
        end, UpdatedHandles)).

expected_handles(From, Until) ->
    AllHandles = node_cache:get(all_handles),
    ListFrom = lists:dropwhile(fun({TimeStamp, _, _, _}) -> TimeStamp < From end, AllHandles),
    ListFromUntil = lists:takewhile(fun({TimeStamp, _, _, _}) -> TimeStamp =< Until end, ListFrom),
    lists:map(fun({_, HandleId, _, _}) -> HandleId end, ListFromUntil).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ozt:init_per_suite(Config, fun() ->
        SmallHServiceList = lists_utils:pmap(fun(_) ->
            create_handle_for_service(?SMALL_HSERVICE)
        end, lists:seq(1, max(0, ?HANDLE_COUNT_IN_SMALL_HSERVICE))),
        node_cache:put(small_hservice, lists:sort(
            fun({TimeSeconds1, _, _, _}, {TimeSeconds2, _, _, _}) ->
                TimeSeconds1 < TimeSeconds2
            end, SmallHServiceList)),

        HandleList = lists_utils:pmap(fun(_) ->
            create_handle()
        end, lists:seq(1, max(0, ?HANDLE_COUNT - ?HANDLE_COUNT_IN_SMALL_HSERVICE))),
        node_cache:put(all_handles, lists:sort(
            fun({TimeSeconds1, _, _, _}, {TimeSeconds2, _, _, _}) ->
                TimeSeconds1 < TimeSeconds2
            end, HandleList ++ SmallHServiceList))
    end).

end_per_suite(_Config) ->
    lists:foreach(fun({TimeSeconds, HandleID, HandleServiceId, MetadataPrefix}) ->
        ozt:rpc(handles, delete, [
            TimeSeconds, HandleID, HandleServiceId, MetadataPrefix
        ])
    end, node_cache:get(all_handles)),
    node_cache:clear(all_handles),
    lists:foreach(fun({TimeSeconds, HandleID, HandleServiceId, MetadataPrefix}) ->
        ozt:rpc(handles, delete, [
            TimeSeconds, HandleID, HandleServiceId, MetadataPrefix
        ])
    end, node_cache:get(small_hservice)),
    node_cache:clear(small_hservice),
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.
