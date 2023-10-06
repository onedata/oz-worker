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
    add_element_that_already_exist_test/1,
    list_from_until_test/1,
    list_no_resumption_token_test/1,
    add_handle_to_service_test/1,
    add_handle_withe_earlier_datastamp_test/1,
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
        add_element_that_already_exist_test
    ]},
    {sequential_tests, [sequential], [
        list_from_until_test,
        list_no_resumption_token_test,
        add_handle_to_service_test,
        add_handle_withe_earlier_datastamp_test,
        delete_handle_from_service_test
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
-define(SIZE, 1000).


%%%===================================================================
%%% Tests
%%%===================================================================


resumption_token_test(_Config) ->
    %% first listing, no resumption_token
    {List1, {{resumption_token, Token1}, {until, _}}} = ozt:rpc(handle, list, [all, #{}]),
    ?assertEqual(lists:last(List1), Token1),
    ?assertEqual(?SIZE, length(List1)),

    %% second listing, resumption token from 1 listing
    %% number of listed elements is ?SIZE-1 because datastore_model:fold_links
    %% return list with resumptionToken which is already included in List1
    {List2, {{resumption_token, Token2}, {until, _}}} = ozt:rpc(
        handle, list, [all, #{resumption_token => get_token_bin(Token1)}]
    ),
    ?assertEqual(lists:last(List2), Token2),
    ?assertEqual(?SIZE -1 , length(List2)),

    %% third listing, resumption token from 2 listing
    %% after this listing, 1000+999+999=2998 elements will be listed
    {List3, {{resumption_token, Token3}, {until, _}}} = ozt:rpc(
        handle, list, [all, #{resumption_token => get_token_bin(Token2)}]
    ),
    ?assertEqual(lists:last(List3), Token3),
    ?assertEqual(?SIZE -1 , length(List3)),

    %% fourth listing is the last one, so token is <<>>, and size is rest so 302
    {List4, {{resumption_token, Token4}, {until, _}}} = ozt:rpc(
        handle, list, [all, #{resumption_token => get_token_bin(Token3)}]
    ),
    ?assertEqual(<<>>, Token4),
    ?assertEqual(302 , length(List4)),

    ?assertEqual(3300, length(List1) + length(List2) + length(List3) + length(List4)).


list_all_handle_test(_Config) ->
    ListAll = list_all(all),

    %% this checks that Data after conversion is valid data
    lists:foreach(fun({TimeSecondsBin, _HandleID}) ->
        {Date, _Time} = time:seconds_to_datetime(binary_to_integer(TimeSecondsBin)),
        case calendar:valid_date(Date) of
            true -> ok;
            false -> throw({error, invalidDateFormat})
        end
                  end,  ListAll),

    ?assertEqual(?HANDLE_STORE_SIZE, length(ListAll)).


list_handles_from_services_test(_Config) ->
    ListService1 = list_all(?SERVICE1),
    ListService2 = list_all(?SERVICE2),
    ?assertEqual(?HANDLE_STORE_SIZE, length(ListService1) + length(ListService2)).


add_element_that_already_exist_test(_Config) ->
    ListService = list_all(?SERVICE1),
    BeforeAdding = length(ListService),
    {TimeSecondsBin, HandleId} = hd(ListService),
    ozt:rpc(handle, add, [binary_to_integer(TimeSecondsBin), HandleId, ?SERVICE1]),
    AfterAdding = length(list_all(?SERVICE1)),
    ?assertEqual(BeforeAdding, AfterAdding).


list_from_until_test(_Config) ->
    % list from
    DataFrom1 = time:datetime_to_seconds({{2023, 6, 10}, {0, 0, 0}}),
    ListingOptsFrom = #{from => DataFrom1},
    ListFrom = ozt:rpc(handle, list, [all, ListingOptsFrom]),
    FullListFrom = get_full_list(ListFrom),

    lists:foreach(fun({TimeSeconds, _}) ->
        ?assert(DataFrom1 =< binary_to_integer(TimeSeconds))
    end, FullListFrom),

    % list until
    DataUntil1 = time:datetime_to_seconds({{2021, 6, 10}, {10, 0, 0}}),
    ListingOptsUntil = #{until => DataUntil1},
    ListUntil = ozt:rpc(handle, list, [all, ListingOptsUntil]),
    FullListUntil = get_full_list(ListUntil),

    lists:foreach(fun({TimeSeconds, _}) ->
        ?assert(DataUntil1 >= binary_to_integer(TimeSeconds))
    end, FullListUntil),

    % list from to until
    DataFrom2 = time:datetime_to_seconds({{2020, 7, 10}, {11, 11, 11}}),
    DataUntil2 = time:datetime_to_seconds({{2021, 6, 10}, {10, 0, 0}}),
    ListingOptsFromUntil = #{from => DataFrom2, until => DataUntil2},
    ListFromUntil = ozt:rpc(handle, list, [all, ListingOptsFromUntil]),
    FullListFromUntil = get_full_list(ListFromUntil),

    lists:foreach(fun({TimeSeconds, _}) ->
        ?assert(
            DataUntil2 >= binary_to_integer(TimeSeconds) andalso DataFrom2 =< binary_to_integer(TimeSeconds)
        )
    end, FullListFromUntil),

    % what if until is equal to from and is now
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    TimeStampSeconds = MegaSecs * 1000000 + Secs,
    HandleId = ?RAND_ID(),
    Service = ?RAND_SERVICE(),
    ozt:rpc(handle, add, [TimeStampSeconds, HandleId, Service]),
    ListingOptsFromUntilEqual = #{from => TimeStampSeconds, until => TimeStampSeconds},
    ListFromUntilEqual = ozt:rpc(handle, list, [all, ListingOptsFromUntilEqual]),
    [{TimeStampBin, HandleIdFromList}] = FullListFromUntilEqual = get_full_list(ListFromUntilEqual),
    ?assertEqual(1, length(FullListFromUntilEqual)),
    ?assertEqual({TimeStampSeconds, HandleId}, {binary_to_integer(TimeStampBin), HandleIdFromList}),
    ozt:rpc(handle, delete, [TimeStampBin, HandleId, Service]),

    % until greater than from
    DataFromGreater = time:datetime_to_seconds({{2021, 6, 10}, {10, 0, 0}}),
    DataUntilGreater = time:datetime_to_seconds({{2020, 7, 10}, {11, 11, 11}}),
    ListingOptsFromGreaterThanUntil = #{from => DataFromGreater, until => DataUntilGreater},
    ListFromGreaterThanUntil = ozt:rpc(handle, list, [all, ListingOptsFromGreaterThanUntil]),
    FullListFromGreaterThanUntil= get_full_list(ListFromGreaterThanUntil),
    ?assertEqual(0, length(FullListFromGreaterThanUntil)),
    ?assertEqual([], FullListFromGreaterThanUntil),

    % from and until outside the range of available dates
    DataFromOutside = time:datetime_to_seconds({{2001, 9, 10}, {10, 10, 0}}),
    DataUntilOutside = time:datetime_to_seconds({{2024, 9, 10}, {10, 10, 0}}),
    ListingOptsOutside = #{from => DataFromOutside, until => DataUntilOutside},
    ListOutside = ozt:rpc(handle, list, [all, ListingOptsOutside]),
    FullListOutside= get_full_list(ListOutside),
    ?assertEqual(3300, length(FullListOutside)).


list_no_resumption_token_test(_Config) ->
    List = ozt:rpc(handle, list, [?SERVICE3, #{}]),
    lists:foreach(fun({TimeSecondsBin, _HandleID}) ->
        {Date, _Time} = time:seconds_to_datetime(binary_to_integer(TimeSecondsBin)),
        case calendar:valid_date(Date) of
            true  -> ok;
            false -> throw({error, invalidDateFormat})
        end
                  end, List),
    ?assertEqual(?SMALL_SERVICE_SIZE, length(List)).


add_handle_to_service_test(_Config) ->
    BeforeAddingService1 = length(list_all(?SERVICE1)),
    BeforeAddingService2 = length(list_all(?SERVICE2)),
    BeforeAddingAll = length(list_all(all)),

    create_handle_for_service(?SERVICE1),

    ?assertEqual(BeforeAddingAll + 1, length(list_all(all))),

    lists:foreach(fun(_) ->
        create_handle_for_service(?SERVICE2)
                  end, lists:seq(1, 10)),

    ?assertEqual(BeforeAddingService1 + 1, length(list_all(?SERVICE1))),
    ?assertEqual(BeforeAddingService2 + 10, length(list_all(?SERVICE2))),
    ?assertEqual(BeforeAddingAll + 11, length(list_all(all))).


%% checks if handles are added sorted by data
add_handle_withe_earlier_datastamp_test(_Config) ->
    OldDataStamp1 = {{2001,9,29}, {6,59,58}},
    OldDataStamp2 = {{2003,9,29}, {6,59,58}},
    OldDataStamp3 = {{2014,9,29}, {6,59,58}},
    ozt:rpc(handle, add, [time:datetime_to_seconds(OldDataStamp1), ?RAND_ID(), ?RAND_SERVICE()]),
    ozt:rpc(handle, add, [time:datetime_to_seconds(OldDataStamp2), ?RAND_ID(), ?RAND_SERVICE()]),
    ozt:rpc(handle, add, [time:datetime_to_seconds(OldDataStamp3), ?RAND_ID(), ?RAND_SERVICE()]),

    {List, {{resumption_token, _}, {until, _}}}= ozt:rpc(handle, list, [all, #{}]),
    [{TimeSeconds1, _}, {TimeSeconds2, _}, {TimeSeconds3, _}] = lists:sublist(List, 3),
    ?assertEqual(OldDataStamp1, time:seconds_to_datetime(binary_to_integer(TimeSeconds1))),
    ?assertEqual(OldDataStamp2, time:seconds_to_datetime(binary_to_integer(TimeSeconds2))),
    ?assertEqual(OldDataStamp3, time:seconds_to_datetime(binary_to_integer(TimeSeconds3))).


delete_handle_from_service_test(_Config) ->
    ListAll = list_all(all),
    ListService1 = list_all(?SERVICE1),
    ListService2 = list_all(?SERVICE2),

    lists:foreach(fun({TimeStamp1, HandleID1} ) ->
        ozt:rpc(handle, delete, [TimeStamp1, HandleID1, ?SERVICE1])
                  end,  lists:sublist(ListService1, 10)),

    ?assertEqual(length(ListAll)-10, length(list_all(all))),

    [{TimeStamp2, HandleID2} | _RestList] = ListService2,
    ozt:rpc(handle, delete, [TimeStamp2, HandleID2, ?SERVICE2]),

    ?assertEqual(length(ListService1)-10, length(list_all(?SERVICE1))),
    ?assertEqual(length(ListService2)-1, length(list_all(?SERVICE2))),
    ?assertEqual(length(ListAll)-11, length(list_all(all))).


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
    {List, {{resumption_token, Token}, {until, Until}}} = list_handle_services(WhatToList),
    case Token of
        <<>> ->
            List;
        _ ->
            list_all(WhatToList, Token, List, Until)
    end.
list_all(WhatToList, Token, ListAll, Until) ->
    {List, {{resumption_token, NewToken}, {until, _Until}}} = list_handle_services(WhatToList, Token, Until),
    NewList = ListAll ++ List,
    case NewToken of
        <<>> ->
            NewList;
        _ ->
            list_all(WhatToList, NewToken, NewList, Until)
    end.

list_handle_services(WhatToList) ->
    ListingOpts = #{},
    ozt:rpc(handle, list, [WhatToList, ListingOpts]).
list_handle_services(WhatToList, Token, Until) ->
    ListingOpts = #{resumption_token => get_token_bin(Token), until => Until},
    ozt:rpc(handle, list, [WhatToList, ListingOpts]).

generate_rand_timestamp() ->
    Hour = rand:uniform(24)-1,     % Hour (0-23)
    Minute = rand:uniform(60)-1,   % Minute (0-59)
    Second = rand:uniform(60)-1,   % Second (0-59)
    Year = rand:uniform(4) + 2019, % Year (from 2020 to 2023)
    Month = rand:uniform(12),      % Month (1-12)
    Day = rand:uniform(
        calendar:last_day_of_the_month(Year, Month)
    ),                             % Day (1-31)
    RandData = {{Year, Month, Day},{Hour, Minute, Second}},
    time:datetime_to_seconds(RandData).

create_handle() ->
    TimeSeconds = generate_rand_timestamp(),
    ozt:rpc(handle, add, [TimeSeconds, ?RAND_ID(), ?RAND_SERVICE()]).

create_handle_for_service(Service) ->
    TimeSeconds = generate_rand_timestamp(),
    ozt:rpc(handle, add, [TimeSeconds, ?RAND_ID(), Service]).

get_token_bin(Token) ->
    {TimeSeconds, HandleId} = Token,
    <<TimeSeconds/binary, ?INDEX_SEP, HandleId/binary>>.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ozt:init_per_suite(Config, fun() ->
        lists_utils:pforeach(fun(_) ->
            create_handle()
        end, lists:seq(1, max(0, ?HANDLE_STORE_SIZE)))
   end).

end_per_suite(_Config) ->
    ListService1 = list_all(?SERVICE1),
    ListService2 = list_all(?SERVICE2),
    lists:foreach(fun({TimeSeconds, HandleID}) ->
        ozt:rpc(handle, delete, [TimeSeconds, HandleID, ?SERVICE1])
    end, ListService1),
    lists:foreach(fun({TimeSeconds, HandleID}) ->
        ozt:rpc(handle, delete, [TimeSeconds, HandleID, ?SERVICE2])
    end, ListService2),
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(list_no_resumption_token_test, Config) ->
    lists_utils:pforeach(fun(_) ->
        create_handle_for_service(?SERVICE3)
    end, lists:seq(1, max(0, ?SMALL_SERVICE_SIZE))),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(list_no_resumption_token_test, Config) ->
    ListService3 = ozt:rpc(handle, list, [?SERVICE3, #{}]),
    lists:foreach(fun({TimeSeconds, HandleID}) ->
        ozt:rpc(handle, delete, [TimeSeconds, HandleID, ?SERVICE3])
    end, ListService3),
    Config;
end_per_testcase(_, Config) ->
    Config.
