%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Tests related to storing handles in handle services.
%%% @end
%%%-------------------------------------------------------------------
-module(store_handles_test_SUITE).
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
%%    {group, parallel_tests}
    {group, sequential_tests}
].

groups() -> [
    {parallel_tests, [parallel], [
        resumption_token_test,
        list_all_handle_test
%%        list_handles_from_services_test,
%%        list_size_elements_test,
%%        add_element_that_already_exist_test
    ]},
    {sequential_tests, [sequential], [
        list_from_until_test
%%        list_no_resumption_token_test,
%%        add_handle_to_service_test,
%%        add_handle_with_earlier_timestamp_test,
%%        delete_handle_from_service_test
    ]}
].

-define(HANDLE_STORE_SIZE, 3300).
-define(SMALL_SERVICE_SIZE, 600).
-define(SERVICE1, <<"Service1">>).
-define(SERVICE2, <<"Service2">>).
-define(SERVICE3, <<"Service3">>).

-define(RAND_NAME(), ?RAND_UNICODE_STR(200)).
-define(RAND_ID(), str_utils:rand_hex(16)).
-define(RAND_SERVICE(), case ?RAND_BOOL() of
    true -> ?SERVICE1;
    false -> ?SERVICE2
end).
-define(INDEX_SEP, 0).
-define(DEFAULT_LIST_LIMIT, 1000).


%%%===================================================================
%%% Tests
%%%===================================================================


resumption_token_test(_Config) ->
    %% first listing, no resumption_token
    {List1, {{resumption_token, Token1}, {until, _}}} = ozt:rpc(handles, list, [all, #{}]),
    ?assertEqual(?DEFAULT_LIST_LIMIT, length(List1)),

    %% second listing, resumption token from 1 listing
    %% number of listed elements is ?DEFAULT_LIST_LIMIT-1 because datastore_model:fold_links
    %% return list with resumptionToken which is already included in List1

    %% po co tu jest get_token_from_handle_id ????
    {List2, {{resumption_token, Token2}, {until, _}}} = ozt:rpc(
        handles, list, [all, #{resumption_token => Token1}]
    ),
    ?assertEqual(?DEFAULT_LIST_LIMIT - 1, length(List2)),

    %% third listing, resumption token from 2 listing
    %% after this listing, 1000+999+999=2998 elements will be listed
    {List3, {{resumption_token, Token3}, {until, _}}} = ozt:rpc(
        handles, list, [all, #{resumption_token => Token2}]
    ),
    ?assertEqual(?DEFAULT_LIST_LIMIT - 1, length(List3)),

    %% fourth listing is the last one, so token is <<>>, and size is rest so 302
    {List4, {{resumption_token, Token4}, {until, _}}} = ozt:rpc(
        handles, list, [all, #{resumption_token => Token3}]
    ),
    ?assertEqual(<<>>, Token4),
    ct:pal("~p", [List4]),
    ?assertEqual(302, length(List4)),

    ?assertEqual(3300, length(List1) + length(List2) + length(List3) + length(List4)).


list_all_handle_test(_Config) ->
    ListAll = list_all(all),
    ?assertEqual(?HANDLE_STORE_SIZE, length(ListAll)).


list_handles_from_services_test(_Config) ->
    ListService1 = list_all(?SERVICE1),
    ListService2 = list_all(?SERVICE2),
    ?assertEqual(?HANDLE_STORE_SIZE, length(ListService1) + length(ListService2)).


list_size_elements_test(_Config) ->
    ListingOpts = #{size => 5},
    List = ozt:rpc(handles, list, [all, ListingOpts]),
    ?assertEqual(5, length(List)).


add_element_that_already_exist_test(_Config) ->
    ListService = list_all(?SERVICE1),
    BeforeAdding = length(ListService),
    HandleId = hd(ListService),
    TimeStamp = get_timestamp_from_handle_id(HandleId),
    ozt:rpc(handles, add, [TimeStamp, HandleId, ?SERVICE1]),
    AfterAdding = length(list_all(?SERVICE1)),
    ?assertEqual(BeforeAdding, AfterAdding).


list_from_until_test(_Config) ->
    % list from
    DataFrom1 = generate_rand_timestamp(),
    ListingOptsFrom = #{from => DataFrom1},
    ListFrom = ozt:rpc(handles, list, [all, ListingOptsFrom]),
    FullListFrom = get_full_list(ListFrom),

    lists:foreach(fun(HandleId) ->
        ?assert(DataFrom1 =< get_timestamp_from_handle_id(HandleId))
    end, FullListFrom),

    % list until
    DataUntil1 = generate_rand_timestamp(),
    ListingOptsUntil = #{until => DataUntil1},
    ListUntil = ozt:rpc(handles, list, [all, ListingOptsUntil]),
    FullListUntil = get_full_list(ListUntil),

    lists:foreach(fun(HandleId) ->
        ?assert(DataUntil1 >= get_timestamp_from_handle_id(HandleId))
    end, FullListUntil),

    % list from to until
    Rand1 = generate_rand_timestamp(),
    Rand2 = generate_rand_timestamp(),
    {DataFrom2, DataUntil2} = case Rand1 < Rand2 of
        true -> {Rand1, Rand2};
        false -> {Rand2, Rand1}
    end,
    ListingOptsFromUntil = #{from => DataFrom2, until => DataUntil2},
    ListFromUntil = ozt:rpc(handles, list, [all, ListingOptsFromUntil]),
    FullListFromUntil = get_full_list(ListFromUntil),
    lists:foreach(fun(HandleId) ->
        TimeSeconds = get_timestamp_from_handle_id(HandleId),
        ?assert(
            DataUntil2 >= TimeSeconds andalso DataFrom2 =< TimeSeconds
        )
    end, FullListFromUntil),

    % what if until is equal to from and is now
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    TimeStampSeconds = MegaSecs * 1000000 + Secs,
    HandleId = ?RAND_ID(),
    Service = ?RAND_SERVICE(),
    ozt:rpc(handles, add, [TimeStampSeconds, HandleId, Service]),
    ListingOptsFromUntilEqual = #{from => TimeStampSeconds, until => TimeStampSeconds},
    ListFromUntilEqual = ozt:rpc(handles, list, [all, ListingOptsFromUntilEqual]),
    [HandleIdFromList] = FullListFromUntilEqual = get_full_list(ListFromUntilEqual),
    ?assertEqual(1, length(FullListFromUntilEqual)),
    ?assertEqual(HandleId, HandleIdFromList),
    ozt:rpc(handles, delete, [TimeStampSeconds, HandleId, Service]),

    % until greater than from
    DataFromGreater = DataUntil2,
    DataUntilGreater = DataFrom2,
    ListingOptsFromGreaterThanUntil = #{from => DataFromGreater, until => DataUntilGreater},
    ListFromGreaterThanUntil = ozt:rpc(handles, list, [all, ListingOptsFromGreaterThanUntil]),
    FullListFromGreaterThanUntil = get_full_list(ListFromGreaterThanUntil),
    ?assertEqual(0, length(FullListFromGreaterThanUntil)),
    ?assertEqual([], FullListFromGreaterThanUntil),

    % from and until outside the range of available dates
    % rand timestamp +/- 10 years
    DataFromOutside = generate_rand_timestamp() - 3600 * 24 * 365 * 10,
    DataUntilOutside = generate_rand_timestamp() + 3600 * 24 * 365 * 10,
    ListingOptsOutside = #{from => DataFromOutside, until => DataUntilOutside},
    ListOutside = ozt:rpc(handles, list, [all, ListingOptsOutside]),
    FullListOutside = get_full_list(ListOutside),
    ?assertEqual(3300, length(FullListOutside)).


list_no_resumption_token_test(_Config) ->
    List = ozt:rpc(handles, list, [?SERVICE3, #{}]),
    ?assertEqual(?SMALL_SERVICE_SIZE, length(List)).


add_handle_to_service_test(_Config) ->
    BeforeAddingService1 = length(list_all(?SERVICE1)),
    BeforeAddingService2 = length(list_all(?SERVICE2)),
    BeforeAddingAll = length(list_all(all)),

    {TimeStamp, HandleId, Service} = create_handle_for_service(?SERVICE1),

    ?assertEqual(BeforeAddingAll + 1, length(list_all(all))),

    HandleList = lists_utils:pmap(fun(_) ->
        create_handle_for_service(?SERVICE2)
    end, lists:seq(1, 10)),
    update_node_cache(HandleList ++ [{TimeStamp, HandleId, Service}]),

    ?assertEqual(BeforeAddingService1 + 1, length(list_all(?SERVICE1))),
    ?assertEqual(BeforeAddingService2 + 10, length(list_all(?SERVICE2))),
    ?assertEqual(BeforeAddingAll + 11, length(list_all(all))).


%% checks if handles are added sorted by data
add_handle_with_earlier_timestamp_test(_Config) ->
    % generate_rand_timestamp generates dates from 2020-2023
    % so after generating date and subtracting 4 years, we are sure we have
    % date earlier than rest on the list
    TimeStamp = generate_rand_timestamp(),
    OldTimeStamp1 = TimeStamp - 8 * 3600 * 24 * 365,
    HandleId1 = ?RAND_ID(),
    Service1 = ?RAND_SERVICE(),
    OldTimeStamp2 = TimeStamp - 6 * 3600 * 24 * 365,
    HandleId2 = ?RAND_ID(),
    Service2 = ?RAND_SERVICE(),
    OldTimeStamp3 = TimeStamp - 4 * 3600 * 24 * 365,
    HandleId3 = ?RAND_ID(),
    Service3 = ?RAND_SERVICE(),
    ozt:rpc(handles, add, [OldTimeStamp1, HandleId1, Service1]),
    ozt:rpc(handles, add, [OldTimeStamp2, HandleId2, Service2]),
    ozt:rpc(handles, add, [OldTimeStamp3, HandleId3, Service3]),
    NewHandles = [{OldTimeStamp1, HandleId1, Service1},
        {OldTimeStamp2, HandleId2, Service2}, {OldTimeStamp3, HandleId3, Service3}],
    update_node_cache(NewHandles),

    {List, {{resumption_token, _}, {until, _}}} = ozt:rpc(handles, list, [all, #{}]),
    [HId1, HId2, HId3] = lists:sublist(List, 3),
    ?assertEqual(HandleId1, HId1),
    ?assertEqual(HandleId2, HId2),
    ?assertEqual(HandleId3, HId3).


delete_handle_from_service_test(_Config) ->
    ListAll = list_all(all),
    ListService1 = list_all(?SERVICE1),
    ListService2 = list_all(?SERVICE2),

    lists:foreach(fun(HandleId1) ->
        ozt:rpc(handles, delete, [
            get_timestamp_from_handle_id(HandleId1), HandleId1, ?SERVICE1
        ])
    end, lists:sublist(ListService1, 10)),

    ?assertEqual(length(ListAll) - 10, length(list_all(all))),

    [HandleId2 | _RestList] = ListService2,
    ozt:rpc(handles, delete, [
        get_timestamp_from_handle_id(HandleId2), HandleId2, ?SERVICE2
    ]),

    ?assertEqual(length(ListService1) - 10, length(list_all(?SERVICE1))),
    ?assertEqual(length(ListService2) - 1, length(list_all(?SERVICE2))),
    ?assertEqual(length(ListAll) - 11, length(list_all(all))).


%%%===================================================================
%%% Helpers
%%%===================================================================

get_full_list(List) ->
    case List of
        {NotFullList, {{resumption_token, Token}, {until, Until}}} ->
            list_all(all, Token, NotFullList, Until);
        _ ->
            List
    end.

list_all(WhatToList) ->
    {List, {{resumption_token, Token}, {until, Until}}} =
        list_handle_services(WhatToList),
    case Token of
        <<>> ->
            List;
        _ ->
            list_all(WhatToList, Token, List, Until)
    end.
list_all(WhatToList, Token, ListAll, Until) ->
    {List, {{resumption_token, NewToken}, {until, _Until}}} =
        list_handle_services(WhatToList, Token, Until),
    NewList = ListAll ++ List,
    case NewToken of
        <<>> ->
            NewList;
        _ ->
            list_all(WhatToList, NewToken, NewList, Until)
    end.

list_handle_services(WhatToList) ->
    ListingOpts = #{},
    ozt:rpc(handles, list, [WhatToList, ListingOpts]).
list_handle_services(WhatToList, Token, Until) ->
    ListingOpts = #{resumption_token => Token, until => Until},
    ozt:rpc(handles, list, [WhatToList, ListingOpts]).

generate_rand_timestamp() ->
    Hour = rand:uniform(24) - 1,     % Hour (0-23)
    Minute = rand:uniform(60) - 1,   % Minute (0-59)
    Second = rand:uniform(60) - 1,   % Second (0-59)
    Year = rand:uniform(4) + 2019, % Year (from 2020 to 2023)
    Month = rand:uniform(12),      % Month (1-12)
    Day = rand:uniform(
        calendar:last_day_of_the_month(Year, Month)
    ),                             % Day (1-31)
    RandData = {{Year, Month, Day}, {Hour, Minute, Second}},
    time:datetime_to_seconds(RandData).

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

get_token_from_handle_id(HandleId) ->
    TimeStamp = get_timestamp_from_handle_id(HandleId),
    FormattedTimeSeconds = str_utils:format_bin("~11..0B", [TimeStamp]),
    <<((FormattedTimeSeconds))/binary, ?INDEX_SEP, HandleId/binary>>.

update_node_cache(NewHandles) ->
    OldHandles = node_cache:get(all_handles),
    UpdatedHandles = OldHandles ++ NewHandles,
    node_cache:clear(all_handles),
    node_cache:put(all_handles, lists:sort(
        fun({TimeSeconds1, _, _}, {TimeSeconds2, _, _}) ->
            TimeSeconds1 < TimeSeconds2
        end, UpdatedHandles)).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ozt:init_per_suite(Config, fun() ->
        HandleList = lists_utils:pmap(fun(_) ->
            create_handle()
        end, lists:seq(1, max(0, ?HANDLE_STORE_SIZE))),

        node_cache:put(all_handles, lists:sort(
            fun({TimeSeconds1, _, _}, {TimeSeconds2, _, _}) ->
                TimeSeconds1 < TimeSeconds2
            end, HandleList))
    end).

end_per_suite(_Config) ->
    lists:foreach(fun({TimeSeconds, HandleID, HandleServiceId}) ->
        ozt:rpc(handles, delete, [
            TimeSeconds, HandleID, HandleServiceId
        ])
    end, node_cache:get(all_handles)),
    node_cache:clear(all_handles),
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(list_no_resumption_token_test, Config) ->
    HandleList = lists_utils:pmap(fun(_) ->
        create_handle_for_service(?SERVICE3)
    end, lists:seq(1, max(0, ?SMALL_SERVICE_SIZE))),
    node_cache:put(handles_service3, lists:sort(
        fun({TimeSeconds1, _, _}, {TimeSeconds2, _, _}) ->
            TimeSeconds1 < TimeSeconds2
        end, HandleList)),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(list_no_resumption_token_test, Config) ->
    lists:foreach(fun({TimeSeconds, HandleID, HandleServiceId}) ->
        ozt:rpc(handles, delete, [
            TimeSeconds, HandleID, HandleServiceId
        ])
    end, node_cache:get(handles_service3)),
    node_cache:clear(handles_service3),
    Config;
end_per_testcase(_, Config) ->
    Config.
