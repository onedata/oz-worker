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
-define(INDEX_SEP, 0).


%%%===================================================================
%%% Tests
%%%===================================================================


resumption_token_test(_Config) ->
    DefaultListLimit = 1000,

    %% first listing, no resumption_token
    {List1, Token1} = ozt:rpc(handles, list, [all, #{}]),
    ?assertEqual(DefaultListLimit, length(List1)),

    %% second listing, resumption token from 1 listing
    {List2, Token2} = ozt:rpc(
        handles, list, [all, #{resumption_token => Token1}]
    ),
    ?assertEqual(DefaultListLimit, length(List2)),

    %% third listing, resumption token from 2 listing
    {List3, Token3} = ozt:rpc(
        handles, list, [all, #{resumption_token => Token2}]
    ),
    ?assertEqual(DefaultListLimit, length(List3)),

    %% fourth listing is the last one, so token is <<>>, and list length is rest so 300
    {List4, Token4} = ozt:rpc(
        handles, list, [all, #{resumption_token => Token3}]
    ),
    ?assertEqual(<<>>, Token4),
    ?assertEqual(300, length(List4)),

    ?assertEqual(3300, length(List1) + length(List2) + length(List3) + length(List4)).


list_all_handle_test(_Config) ->
    ListAll = list_all(all),
    ?assertEqual(?HANDLE_COUNT, length(ListAll)).


list_handles_from_services_test(_Config) ->
    ListFirstHService = list_all(?FIRST_HSERVICE),
    ListAnotherHService = list_all(?ANOTHER_HSERVICE),
    ListSmallHService = list_all(?SMALL_HSERVICE),
    ?assertEqual(
        ?HANDLE_COUNT,
        length(ListFirstHService) + length(ListAnotherHService) + length(ListSmallHService)
    ).


list_size_elements_test(_Config) ->
    ListingOpts = #{size => 5},
    {List, undefined} = ozt:rpc(handles, list, [all, ListingOpts]),
    ?assertEqual(5, length(List)).


add_element_that_already_exist_test(_Config) ->
    ListService = list_all(?FIRST_HSERVICE),
    BeforeAdding = length(ListService),
    HandleId = hd(ListService),
    TimeStamp = get_timestamp_from_handle_id(HandleId),
    try
        ozt:rpc(handles, add, [TimeStamp, HandleId, ?FIRST_HSERVICE])
    catch
        error:ErrorReason:_Stacktrace ->
            ?assertEqual(ErrorReason, {badrpc, ?ERROR_ALREADY_EXISTS})
    end,
    AfterAdding = length(list_all(?FIRST_HSERVICE)),
    ?assertEqual(BeforeAdding, AfterAdding).


list_from_until_test(_Config) ->
    % list from
    From1 = generate_rand_timestamp(),
    ListingOptsFrom = #{from => From1},
    ListFrom = ozt:rpc(handles, list, [all, ListingOptsFrom]),
    FullListFrom = get_full_list(ListFrom),
    ExpectedListFrom = expected_handles(From1, ?LATEST_TIMESTAMP),
    ?assertEqual(ExpectedListFrom, FullListFrom),

    % list until
    Until1 = generate_rand_timestamp(),
    ListingOptsUntil = #{until => Until1},
    ListUntil = ozt:rpc(handles, list, [all, ListingOptsUntil]),
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
    ListFromUntil = ozt:rpc(handles, list, [all, ListingOptsFromUntil]),
    FullListFromUntil = get_full_list(ListFromUntil, Until2),
    ExpectedListFromUntil = expected_handles(From2, Until2),
    ?assertEqual(ExpectedListFromUntil, FullListFromUntil),

    % what if until is equal to from and it is the moment of creating first
    [{TimeStampSeconds, HandleId, _} | _Rest] = node_cache:get(all_handles),
    ListingOptsFromUntilEqual = #{from => TimeStampSeconds, until => TimeStampSeconds},
    ListFromUntilEqual = ozt:rpc(handles, list, [all, ListingOptsFromUntilEqual]),
    [HandleIdFromList] = FullListFromUntilEqual = get_full_list(ListFromUntilEqual, TimeStampSeconds),
    ?assertEqual(1, length(FullListFromUntilEqual)),
    ?assertEqual(HandleId, HandleIdFromList),

    % from greater than until
    FromGreater = Until2,
    UntilGreater = From2,
    ListingOptsFromGreaterThanUntil = #{from => FromGreater, until => UntilGreater},
    ListFromGreaterThanUntil = ozt:rpc(handles, list, [all, ListingOptsFromGreaterThanUntil]),
    FullListFromGreaterThanUntil = get_full_list(ListFromGreaterThanUntil, UntilGreater),
    ?assertEqual(0, length(FullListFromGreaterThanUntil)),
    ?assertEqual([], FullListFromGreaterThanUntil),

    % from and until outside the range of available dates
    % rand timestamp +/- 10 years
    FromOutside = generate_rand_timestamp() - 3600 * 24 * 365 * 10,
    UntilOutside = generate_rand_timestamp() + 3600 * 24 * 365 * 10,
    ListingOptsOutside = #{from => FromOutside, until => UntilOutside},
    ListOutside = ozt:rpc(handles, list, [all, ListingOptsOutside]),
    FullListOutside = get_full_list(ListOutside, UntilOutside),
    ExpectedListOutside = expected_handles(FromOutside, UntilOutside),
    ?assertEqual(ExpectedListOutside, FullListOutside).


list_no_resumption_token_test(_Config) ->
    {List, undefined} = ozt:rpc(handles, list, [?SMALL_HSERVICE, #{}]),
    ?assertEqual(?HANDLE_COUNT_IN_SMALL_HSERVICE, length(List)).


add_handle_to_service_test(_Config) ->
    BeforeAddingService1 = length(list_all(?FIRST_HSERVICE)),
    BeforeAddingService2 = length(list_all(?ANOTHER_HSERVICE)),
    BeforeAddingAll = length(list_all(all)),

    {TimeStamp, HandleId, Service} = create_handle_for_service(?FIRST_HSERVICE),

    ?assertEqual(BeforeAddingAll + 1, length(list_all(all))),

    HandleList = lists_utils:pmap(fun(_) ->
        create_handle_for_service(?ANOTHER_HSERVICE)
    end, lists:seq(1, 10)),
    update_node_cache(HandleList ++ [{TimeStamp, HandleId, Service}]),

    ?assertEqual(BeforeAddingService1 + 1, length(list_all(?FIRST_HSERVICE))),
    ?assertEqual(BeforeAddingService2 + 10, length(list_all(?ANOTHER_HSERVICE))),
    ?assertEqual(BeforeAddingAll + 11, length(list_all(all))).


%% checks if handles are added sorted by data
add_handle_with_earlier_timestamp_test(_Config) ->
    % generate_rand_timestamp generates dates from 2008-2023
    % so after generating date and subtracting 16 years, we are sure we have
    % date earlier than rest on the list
    TimeStamp = generate_rand_timestamp(),
    OldTimeStamp1 = TimeStamp - 22 * 3600 * 24 * 365,
    HandleId1 = ?RAND_ID(),
    Service1 = ?RAND_SERVICE(),
    OldTimeStamp2 = TimeStamp - 20 * 3600 * 24 * 365,
    HandleId2 = ?RAND_ID(),
    Service2 = ?RAND_SERVICE(),
    OldTimeStamp3 = TimeStamp - 18 * 3600 * 24 * 365,
    HandleId3 = ?RAND_ID(),
    Service3 = ?RAND_SERVICE(),
    ozt:rpc(handles, add, [OldTimeStamp1, HandleId1, Service1]),
    ozt:rpc(handles, add, [OldTimeStamp2, HandleId2, Service2]),
    ozt:rpc(handles, add, [OldTimeStamp3, HandleId3, Service3]),
    NewHandles = [{OldTimeStamp1, HandleId1, Service1},
        {OldTimeStamp2, HandleId2, Service2}, {OldTimeStamp3, HandleId3, Service3}],
    update_node_cache(NewHandles),

    {List, _Token} = ozt:rpc(handles, list, [all, #{}]),
    [HId1, HId2, HId3] = lists:sublist(List, 3),
    ?assertEqual(HandleId1, HId1),
    ?assertEqual(HandleId2, HId2),
    ?assertEqual(HandleId3, HId3).


delete_handle_from_service_test(_Config) ->
    ListAll = list_all(all),
    ListService1 = list_all(?FIRST_HSERVICE),
    ListService2 = list_all(?ANOTHER_HSERVICE),

    lists:foreach(fun(HandleId1) ->
        ozt:rpc(handles, delete, [
            get_timestamp_from_handle_id(HandleId1), HandleId1, ?FIRST_HSERVICE
        ])
    end, lists:sublist(ListService1, 10)),

    ?assertEqual(length(ListAll) - 10, length(list_all(all))),

    [HandleId2 | _RestList] = ListService2,
    ozt:rpc(handles, delete, [
        get_timestamp_from_handle_id(HandleId2), HandleId2, ?ANOTHER_HSERVICE
    ]),

    ?assertEqual(length(ListService1) - 10, length(list_all(?FIRST_HSERVICE))),
    ?assertEqual(length(ListService2) - 1, length(list_all(?ANOTHER_HSERVICE))),
    ?assertEqual(length(ListAll) - 11, length(list_all(all))).


%%%===================================================================
%%% Helpers
%%%===================================================================

get_full_list(List) ->
    case List of
        {NotFullList, undefined} -> NotFullList;
        {NotFullList, Token} -> list_all(all, Token, NotFullList, <<>>)
    end.

get_full_list(List, Until) ->
    case List of
        {NotFullList, undefined} -> NotFullList;
        {NotFullList, Token} -> list_all(all, Token, NotFullList, Until)
    end.

list_all(WhatToList) ->
    {List, Token} =
        list_handle_services(WhatToList),
    case Token == <<>> orelse Token == undefined of
        true -> List;
        false -> list_all(WhatToList, Token, List, <<>>)
    end.
list_all(WhatToList, Token, ListAll, Until) ->
    {List, NewToken} =
        list_handle_services(WhatToList, Token, Until),
    NewList = ListAll ++ List,
    case NewToken == <<>> orelse NewToken == undefined of
        true -> NewList;
        false -> list_all(WhatToList, NewToken, NewList, Until)
    end.

list_handle_services(WhatToList) ->
    ListingOpts = #{},
    ozt:rpc(handles, list, [WhatToList, ListingOpts]).
list_handle_services(WhatToList, Token, Until) ->
    ListingOpts = #{resumption_token => Token, until => Until},
    ozt:rpc(handles, list, [WhatToList, ListingOpts]).

generate_rand_timestamp() ->
    rand:uniform(
        ?LATEST_TIMESTAMP - ?EARLIEST_TIMESTAMP + 1
    ) + ?EARLIEST_TIMESTAMP - 1.

create_handle() ->
    TimeSeconds = generate_rand_timestamp(),
    HandleId = ?RAND_ID(),
    HandleService = ?RAND_SERVICE(),
    ozt:rpc(handles, add, [TimeSeconds, HandleId, HandleService]),
    {TimeSeconds, HandleId, HandleService}.

create_handle_for_service(Service) ->
    TimeSeconds = generate_rand_timestamp(),
    HandleId = ?RAND_ID(),
    ozt:rpc(handles, add, [TimeSeconds, HandleId, Service]),
    {TimeSeconds, HandleId, Service}.

get_timestamp_from_handle_id(HandleId) ->
    Expected = node_cache:get(all_handles),
    [{TimeStamp, _HId, _HSId}] = lists:filter(
        fun({_TimeStamp, HId, _HSId}) -> HId == HandleId end, Expected
    ),
    TimeStamp.

update_node_cache(NewHandles) ->
    OldHandles = node_cache:get(all_handles),
    UpdatedHandles = OldHandles ++ NewHandles,
    node_cache:clear(all_handles),
    node_cache:put(all_handles, lists:sort(
        fun({TimeSeconds1, _, _}, {TimeSeconds2, _, _}) ->
            TimeSeconds1 < TimeSeconds2
        end, UpdatedHandles)).

expected_handles(From, Until) ->
    AllHandles = node_cache:get(all_handles),
    ListFrom = lists:dropwhile(fun({TimeStamp, _, _}) -> TimeStamp < From end, AllHandles),
    ListFromUntil = lists:takewhile(fun({TimeStamp, _, _}) -> TimeStamp =< Until end, ListFrom),
    lists:map(fun({_, HandleId, _}) -> HandleId end, ListFromUntil).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ozt:init_per_suite(Config, fun() ->
        SmallHServiceList = lists_utils:pmap(fun(_) ->
            create_handle_for_service(?SMALL_HSERVICE)
        end, lists:seq(1, max(0, ?HANDLE_COUNT_IN_SMALL_HSERVICE))),
        node_cache:put(small_hservice, lists:sort(
            fun({TimeSeconds1, _, _}, {TimeSeconds2, _, _}) ->
                TimeSeconds1 < TimeSeconds2
            end, SmallHServiceList)),

        HandleList = lists_utils:pmap(fun(_) ->
            create_handle()
        end, lists:seq(1, max(0, ?HANDLE_COUNT - ?HANDLE_COUNT_IN_SMALL_HSERVICE))),
        node_cache:put(all_handles, lists:sort(
            fun({TimeSeconds1, _, _}, {TimeSeconds2, _, _}) ->
                TimeSeconds1 < TimeSeconds2
            end, HandleList ++ SmallHServiceList))
    end).

end_per_suite(_Config) ->
    lists:foreach(fun({TimeSeconds, HandleID, HandleServiceId}) ->
        ozt:rpc(handles, delete, [
            TimeSeconds, HandleID, HandleServiceId
        ])
    end, node_cache:get(all_handles)),
    node_cache:clear(all_handles),
    lists:foreach(fun({TimeSeconds, HandleID, HandleServiceId}) ->
        ozt:rpc(handles, delete, [
            TimeSeconds, HandleID, HandleServiceId
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
