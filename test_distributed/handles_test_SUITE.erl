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
    update_handle_timestamp_test/1,
    list_from_until_test/1,
    list_no_resumption_token_test/1,
    add_handle_to_service_test/1,
    add_handle_with_earlier_timestamp_test/1,
    delete_handle_from_service_test/1
]).

all() -> [
    {group, parallel_tests}
%%    {group, sequential_tests}
].

groups() -> [
    {parallel_tests, [parallel], [
%%        resumption_token_test,
%%        list_all_handle_test,
%%        list_handles_from_services_test,
%%        list_handles_with_metadata_format_test,
%%        list_size_elements_test,
%%        add_element_that_already_exist_test
        update_handle_timestamp_test
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
    true -> <<"oai_dc">>;
    false -> <<"edm">>
end).
-define(INDEX_SEP, 0).


%%%===================================================================
%%% Tests
%%%===================================================================


resumption_token_test(_Config) ->
    DefaultListLimit = 1000,

    %% first listing, no resumption_token
    {List1, Token1} = ozt:rpc(handles, list, [#{metadata_prefix => <<"oai_dc">>}]),
    ?assertEqual(DefaultListLimit, length(List1)),

    %% second listing, resumption token from first listing
    {List2, Token2} = ozt:rpc(
        handles, list, [#{resumption_token => Token1, metadata_prefix => <<"oai_dc">>}]
    ),
    ?assertEqual(undefined, Token2),

    %% third listing, other prefix
    {List3, Token3} = ozt:rpc(
        handles, list, [#{metadata_prefix => <<"edm">>}]
    ),
    ?assertEqual(DefaultListLimit, length(List3)),

    {List4, Token4} = ozt:rpc(
        handles, list, [#{resumption_token => Token3, metadata_prefix => <<"edm">>}]
    ),
    ?assertEqual(undefined, Token4),

    ?assertEqual(3300, length(List1) + length(List2) + length(List3) + length(List4)).


list_all_handle_test(_Config) ->
    ListAll = list_all(),
    ?assertEqual(?HANDLE_COUNT, length(ListAll)).


list_handles_from_services_test(_Config) ->
    ListFirstHServiceDC = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => <<"oai_dc">>}),
    ListFirstHServiceEDM = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => <<"edm">>}),
    ListAnotherHServiceDC = list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => <<"oai_dc">>}),
    ListAnotherHServiceEDM = list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => <<"edm">>}),
    ListSmallHServiceDC = list_all(#{service_id => ?SMALL_HSERVICE, metadata_prefix => <<"oai_dc">>}),
    ListSmallHServiceEDM = list_all(#{service_id => ?SMALL_HSERVICE, metadata_prefix => <<"edm">>}),
    ?assertEqual(
        ?HANDLE_COUNT,
        length(ListFirstHServiceDC) + length(ListFirstHServiceEDM)
            + length(ListAnotherHServiceDC) + length(ListAnotherHServiceEDM)
            + length(ListSmallHServiceDC) + length(ListSmallHServiceEDM)
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
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ListingOpts = #{size => 5, metadata_prefix => MetadataPrefix},
    {List, undefined} = ozt:rpc(handles, list, [ListingOpts]),
    SubList = lists:sublist(node_cache:get(binary_to_atom(MetadataPrefix)), 5),
    ExpectedList = lists:map(
        fun({_, HandleId, _, _}) -> HandleId end, SubList
    ),
    ?assertEqual(ExpectedList, List),
    ?assertEqual(5, length(List)).


add_element_that_already_exist_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ListService = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}),
    BeforeAdding = length(ListService),
    HandleId = hd(ListService),
    TimeStamp = get_elem_from_handle_id(HandleId, timestamp),
    try
        ozt:rpc(handles, add, [MetadataPrefix, ?FIRST_HSERVICE, HandleId, TimeStamp])
    catch
        error:ErrorReason:_Stacktrace ->
            ?assertEqual(ErrorReason, {badrpc, ?ERROR_ALREADY_EXISTS})
    end,
    AfterAdding = length(list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix})),
    ?assertEqual(BeforeAdding, AfterAdding).

update_handle_timestamp_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ListService = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}),
    ct:pal("~p ~n", [length(ListService)]),
    HandleId = hd(ListService),
    ct:pal("~p ~n", [HandleId]),
    NewTimeStamp = ?EARLIEST_TIMESTAMP - 1,
    AllHandles = node_cache:get(binary_to_atom(MetadataPrefix)),
    [{OldTimeStamp, _HId, _HSId, MetadataPrefix}] = lists:filter(
        fun({_TimeStamp, HId, _HSId, _MetadataPrefix}) ->
            HId == HandleId end, AllHandles
    ),
    ct:pal("~p~n", [OldTimeStamp]),
    ozt:rpc(handles, update, [
        MetadataPrefix, ?FIRST_HSERVICE, HandleId, OldTimeStamp, NewTimeStamp
    ]),
    node_cache:clear(binary_to_atom(MetadataPrefix)),
    node_cache:clear(all_handles),
    node_cache:put(binary_to_atom(MetadataPrefix),
        list_all(#{metadata_prefix => MetadataPrefix})),
    node_cache:put(all_handles, list_all()),
    EarliestTimestamp = ozt:rpc(handles, get_earliest_timestamp),
    ct:pal("New ~p ~n Earliest ~p~n~n", [NewTimeStamp, EarliestTimestamp]).


list_from_until_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    % list from
    From1 = generate_rand_timestamp(),
    ListingOptsFrom = #{from => From1, metadata_prefix => MetadataPrefix},
    ListFrom = ozt:rpc(handles, list, [ListingOptsFrom]),
    FullListFrom = get_full_list(ListFrom, MetadataPrefix),
    ExpectedListFrom = expected_handles(From1, MetadataPrefix, ?LATEST_TIMESTAMP),
    ?assertEqual(ExpectedListFrom, FullListFrom),

    % list until
    Until1 = generate_rand_timestamp(),
    ListingOptsUntil = #{until => Until1, metadata_prefix => MetadataPrefix},
    ListUntil = ozt:rpc(handles, list, [ListingOptsUntil]),
    FullListUntil = get_full_list(ListUntil, Until1, MetadataPrefix),
    ExpectedListUntil = expected_handles(?EARLIEST_TIMESTAMP, MetadataPrefix, Until1),
    ?assertEqual(ExpectedListUntil, FullListUntil),

    % list from to until
    Rand1 = generate_rand_timestamp(),
    Rand2 = generate_rand_timestamp(),
    {From2, Until2} = case Rand1 < Rand2 of
        true -> {Rand1, Rand2};
        false -> {Rand2, Rand1}
    end,
    ListingOptsFromUntil = #{from => From2, until => Until2, metadata_prefix => MetadataPrefix},
    ListFromUntil = ozt:rpc(handles, list, [ListingOptsFromUntil]),
    FullListFromUntil = get_full_list(ListFromUntil, Until2, MetadataPrefix),
    ExpectedListFromUntil = expected_handles(From2, MetadataPrefix, Until2),
    ?assertEqual(ExpectedListFromUntil, FullListFromUntil),

    % what if until is equal to from and it is the moment of creating first
    [{TimeStampSeconds, HandleId, _, _} | _Rest] = node_cache:get(binary_to_atom(MetadataPrefix)),
    ListingOptsFromUntilEqual = #{from => TimeStampSeconds, until => TimeStampSeconds,
        metadata_prefix => MetadataPrefix},
    ListFromUntilEqual = ozt:rpc(handles, list, [ListingOptsFromUntilEqual]),
    [HandleIdFromList] = FullListFromUntilEqual = get_full_list(ListFromUntilEqual,
        TimeStampSeconds, MetadataPrefix),
    ?assertEqual(1, length(FullListFromUntilEqual)),
    ?assertEqual(HandleId, HandleIdFromList),

    % from greater than until
    FromGreater = Until2,
    UntilGreater = From2,
    ListingOptsFromGreaterThanUntil = #{from => FromGreater, until => UntilGreater,
        metadata_prefix => MetadataPrefix},
    ListFromGreaterThanUntil = ozt:rpc(handles, list, [ListingOptsFromGreaterThanUntil]),
    FullListFromGreaterThanUntil = get_full_list(ListFromGreaterThanUntil,
        UntilGreater, MetadataPrefix),
    ?assertEqual(0, length(FullListFromGreaterThanUntil)),
    ?assertEqual([], FullListFromGreaterThanUntil),

    % from and until outside the range of available dates
    % rand timestamp +/- 10 years
    FromOutside = generate_rand_timestamp() - 3600 * 24 * 365 * 10,
    UntilOutside = generate_rand_timestamp() + 3600 * 24 * 365 * 10,
    ListingOptsOutside = #{from => FromOutside, until => UntilOutside, metadata_prefix => MetadataPrefix},
    ListOutside = ozt:rpc(handles, list, [ListingOptsOutside]),
    FullListOutside = get_full_list(ListOutside, UntilOutside, MetadataPrefix),
    ExpectedListOutside = expected_handles(FromOutside, MetadataPrefix, UntilOutside),
    ?assertEqual(ExpectedListOutside, FullListOutside).


list_no_resumption_token_test(_Config) ->
    MetadataPrefix =  ?RAND_METADATA_PREFIX(),
    {List, undefined} = ozt:rpc(handles, list, [#{service_id => ?SMALL_HSERVICE,
        metadata_prefix => MetadataPrefix}]),
    ExpectedList = lists:map(
        fun({_, HandleId, _, _}) -> HandleId end,
        node_cache:get(binary_to_atom(<<MetadataPrefix/binary, <<"_hservice">>/binary>>))
    ),
    ?assertEqual(ExpectedList, List).


add_handle_to_service_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    BeforeAddingService1 = length(list_all(
        #{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}
    )),
    BeforeAddingService2 = length(list_all(
        #{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}
    )),
    BeforeAddingAll = length(list_all(#{metadata_prefix => MetadataPrefix})),

    {TimeStamp, HandleId, Service, _} = create_handle_for_service(?FIRST_HSERVICE, MetadataPrefix),

    ?assertEqual(BeforeAddingAll + 1, length(list_all(#{metadata_prefix => MetadataPrefix}))),

    HandleList = lists_utils:pmap(fun(_) ->
        create_handle_for_service(?ANOTHER_HSERVICE, MetadataPrefix)
    end, lists:seq(1, 10)),
    update_node_cache(HandleList ++ [{TimeStamp, HandleId, Service, MetadataPrefix}]),

    ?assertEqual(BeforeAddingService1 + 1,
        length(list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(BeforeAddingService2 + 10,
        length(list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(BeforeAddingAll + 11, length(list_all(#{metadata_prefix => MetadataPrefix}))).


%% checks if handles are added sorted by data
add_handle_with_earlier_timestamp_test(_Config) ->
    % generate_rand_timestamp generates dates from 2008-2023
    % so after generating date and subtracting 16 years, we are sure we have
    % date earlier than rest on the list
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    TimeStamp = generate_rand_timestamp(),
    [{_, HandleId3, _, _}, {_, HandleId2, _, _}, {_, HandleId1, _, _}] = NewHandles =
        lists:map(fun(Number) ->
            OldTimestamp = TimeStamp - (18 + Number) * 3600 * 24 * 365,
            HandleId = ?RAND_ID(),
            Service = ?RAND_SERVICE(),
            ozt:rpc(handles, add, [MetadataPrefix, Service, HandleId, OldTimestamp]),
            {OldTimestamp, HandleId, Service, MetadataPrefix}
        end, lists:seq(1, 3)
        ),

    update_node_cache(NewHandles),

    {List, _Token} = ozt:rpc(handles, list, [#{metadata_prefix => MetadataPrefix}]),
    [HId1, HId2, HId3] = lists:sublist(List, 3),
    ?assertEqual(HandleId1, HId1),
    ?assertEqual(HandleId2, HId2),
    ?assertEqual(HandleId3, HId3).


delete_handle_from_service_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ListAll = list_all(#{metadata_prefix => MetadataPrefix}),
    ListService1 = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}),
    ListService2 = list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}),

    lists:foreach(fun(HandleId1) ->
        ozt:rpc(handles, delete, [
            get_elem_from_handle_id(HandleId1, timestamp), HandleId1,
            ?FIRST_HSERVICE, MetadataPrefix
        ])
    end, lists:sublist(ListService1, 10)),

    ?assertEqual(length(ListAll) - 10, length(list_all(#{metadata_prefix => MetadataPrefix}))),

    [HandleId2 | _RestList] = ListService2,
    ozt:rpc(handles, delete, [
        get_elem_from_handle_id(HandleId2, timestamp), HandleId2,
        ?ANOTHER_HSERVICE, MetadataPrefix
    ]),

    ?assertEqual(length(ListService1) - 10,
        length(list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(length(ListService2) - 1,
        length(list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(length(ListAll) - 11, length(list_all(#{metadata_prefix => MetadataPrefix}))).


%%%===================================================================
%%% Helpers
%%%===================================================================

get_full_list(List, MetadataPrefix) ->
    case List of
        {NotFullList, undefined} -> NotFullList;
        {NotFullList, Token} -> list_all(#{metadata_prefix => MetadataPrefix},
            Token, NotFullList, <<>>)
    end.

get_full_list(List, Until, MetadataPrefix) ->
    case List of
        {NotFullList, undefined} -> NotFullList;
        {NotFullList, Token} -> list_all(#{metadata_prefix => MetadataPrefix},
            Token, NotFullList, Until)
    end.

list_all() ->
    lists:flatmap(fun(MetadataFormat) ->
        list_all(#{metadata_prefix => MetadataFormat})
    end, metadata_formats:supported_formats()).
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
    ozt:rpc(handles, add, [MetadataPrefix, HandleService, HandleId, TimeSeconds]),
    {TimeSeconds, HandleId, HandleService, MetadataPrefix}.

create_handle_for_service(Service) ->
    TimeSeconds = generate_rand_timestamp(),
    HandleId = ?RAND_ID(),
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ozt:rpc(handles, add, [MetadataPrefix, Service, HandleId, TimeSeconds]),
    {TimeSeconds, HandleId, Service, MetadataPrefix}.

create_handle_for_service(Service, MetadataPrefix) ->
    TimeSeconds = generate_rand_timestamp(),
    HandleId = ?RAND_ID(),
    ozt:rpc(handles, add, [MetadataPrefix, Service, HandleId, TimeSeconds]),
    {TimeSeconds, HandleId, Service, MetadataPrefix}.

get_elem_from_handle_id(HandleId, Atom) ->
    Expected = node_cache:get(all_handles),
    [{TimeStamp, _HId, _HSId, MetadataPrefix}] = lists:filter(
        fun({_TimeStamp, HId, _HSId, _MetadataPrefix}) ->
            HId == HandleId end, Expected
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

expected_handles(From, MetadataPrefix, Until) ->
    AllHandles = node_cache:get(binary_to_atom(MetadataPrefix)),
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
        SortedSmallHServiceList = lists:sort(
            fun({TimeSeconds1, _, _, _}, {TimeSeconds2, _, _, _}) ->
                TimeSeconds1 < TimeSeconds2
            end, SmallHServiceList),
        node_cache:put(small_hservice, SortedSmallHServiceList),
        {OAI_DC_Service, EDM_Service} = lists:partition(fun({_, _, _, MetadataPrefix}) ->
            case MetadataPrefix of
                <<"oai_dc">> -> true;
                <<"edm">> -> false
            end
        end, SortedSmallHServiceList),
        node_cache:put(oai_dc_hservice, OAI_DC_Service),
        node_cache:put(edm_hservice, EDM_Service),

        HandleList = lists_utils:pmap(fun(_) ->
            create_handle()
        end, lists:seq(1, max(0, ?HANDLE_COUNT - ?HANDLE_COUNT_IN_SMALL_HSERVICE))),
        SortedHandles = lists:sort(
            fun({TimeSeconds1, _, _, _}, {TimeSeconds2, _, _, _}) ->
                TimeSeconds1 < TimeSeconds2
            end, HandleList ++ SmallHServiceList),
        node_cache:put(all_handles, SortedHandles),
        {OAI_DC, EDM} = lists:partition(fun({_, _, _, MetadataPrefix}) ->
            case MetadataPrefix of
                <<"oai_dc">> -> true;
                <<"edm">> -> false
            end
        end, SortedHandles),
        node_cache:put(oai_dc, OAI_DC),
        node_cache:put(edm, EDM)
    end).

end_per_suite(_Config) ->
    lists:foreach(fun({TimeSeconds, HandleID, HandleServiceId, MetadataPrefix}) ->
        ozt:rpc(handles, delete, [
            TimeSeconds, HandleID, HandleServiceId, MetadataPrefix
        ])
    end, node_cache:get(all_handles)),
    node_cache:clear(all_handles),
    node_cache:clear(oai_dc),
    node_cache:clear(edm),
    lists:foreach(fun({TimeSeconds, HandleID, HandleServiceId, MetadataPrefix}) ->
        ozt:rpc(handles, delete, [
            TimeSeconds, HandleID, HandleServiceId, MetadataPrefix
        ])
    end, node_cache:get(small_hservice)),
    node_cache:clear(small_hservice),
    node_cache:clear(oai_dc_hservice),
    node_cache:clear(edm_hservice),
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.
