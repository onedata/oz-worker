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
    list_from_until_test/1,
    delete_handle_from_service_test/1,
    add_handle_to_service_test/1,
    add_handle_withe_earlier_datastamp_test/1,
    list_no_resumption_token_test/1
]).

all() -> [
%%    {group, parallel_tests}
    {group, sequential_tests}
].

groups() -> [
    {parallel_tests, [parallel], [
%%        resumption_token_test,
%%        list_all_handle_test,
%%        list_handles_from_services_test,
        list_from_until_test
    ]},
    {sequential_tests, [sequential], [
%%        delete_handle_from_service_test,
%%        add_handle_to_service_test,
        add_handle_withe_earlier_datastamp_test
%%        list_no_resumption_token_test
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
    {List1, {resumption_token, Token1}} = ozt:rpc(handle, list, [all, #{}]),
    ?assertEqual(lists:last(List1), Token1),
    ?assertEqual(?SIZE, length(List1)),

    %% second listing, resumption token from 1 listing
    %% number of listed elements is ?SIZE-1 because datastore_model:fold_links
    %% return list with resumptionToken which is already included in List1
    {List2, {resumption_token, Token2}} = ozt:rpc(
        handle, list, [all, #{resumption_token => get_token_bin(Token1)}]
    ),
    ?assertEqual(lists:last(List2), Token2),
    ?assertEqual(?SIZE -1 , length(List2)),

    %% third listing, resumption token from 2 listing
    %% after this listing, 1000+999+999=2998 elements will be listed
    {List3, {resumption_token, Token3}} = ozt:rpc(
        handle, list, [all, #{resumption_token => get_token_bin(Token2)}]
    ),
    ?assertEqual(lists:last(List3), Token3),
    ?assertEqual(?SIZE -1 , length(List3)),

    %% fourth listing is the last one, so token is <<>>, and size is rest so 302
    {List4, {resumption_token, Token4}} = ozt:rpc(
        handle, list, [all, #{resumption_token => get_token_bin(Token3)}]
    ),
    ?assertEqual(<<>>, Token4),
    ?assertEqual(302 , length(List4)),

    ?assertEqual(3300, length(List1) + length(List2) + length(List3) + length(List4)).


list_no_resumption_token_test(_Config) ->
    List = ozt:rpc(handle, list, [?SERVICE3, #{}]),
    lists:foreach(fun({DataStampBin, _HandleID}) ->
        case binary_to_term(DataStampBin) of
            {{_Hour, _Minute, _Seconds}, {_Year, _Month, _Day}}  -> ok;
            _ -> throw({error, recordsNotMatch})
        end
    end, List),
    ?assertEqual(?SMALL_SERVICE_SIZE, length(List)).


list_all_handle_test(_Config) ->
    ListAll = list_all(all),

    %% this checks that DataStamp after conversion is in {Time, Data} format
    lists:foreach(fun({DataStampBin, _HandleID}) ->
        case binary_to_term(DataStampBin) of
            {{_Hour, _Minute, _Seconds}, {_Year, _Month, _Day}} -> ok;
            _ -> {error, recordsNotMatch}
        end
    end,  ListAll),

    ?assertEqual(?HANDLE_STORE_SIZE, length(ListAll)).


list_handles_from_services_test(_Config) ->
    ListService1 = list_all(?SERVICE1),
    ListService2 = list_all(?SERVICE2),
    ?assertEqual(?HANDLE_STORE_SIZE, length(ListService1) + length(ListService2)).

list_from_until_test(_Config) ->
    ListingOpts = #{from => {{0, 0, 0}, {2023, 6, 10}}},
    List = ozt:rpc(handle, list, [all, ListingOpts]),
%%    lists:foreach(fun({DataStamp, _}) ->
%%            ct:pal("~p~n", [binary_to_term(DataStamp)])
%%    end,
%%        case List of
%%            {List1, {_, _}} -> List1;
%%            _ -> List
%%        end),

%%    No nie działa
    case List of
        {NotFullList, {resumption_token, _Token}} ->
            {DataStamp, _} = hd(NotFullList),
            ct:pal("Resumption ~p ~n", [binary_to_term(DataStamp)]);
        _ ->
            {DataStamp, _} = hd(List),
            ct:pal("No resumption token ~p ~n", [binary_to_term(DataStamp)])
    end.


delete_handle_from_service_test(_Config) ->
    ListAll = list_all(all),
    ListService1 = list_all(?SERVICE1),
    ListService2 = list_all(?SERVICE2),

    lists:foreach(fun({DataStamp1, HandleID1} ) ->
        ozt:rpc(handle, delete, [DataStamp1, HandleID1, ?SERVICE1])
    end,  lists:sublist(ListService1, 10)),

    ?assertEqual(length(ListAll)-10, length(list_all(all))),

    [{DataStamp2, HandleID2} | _RestList] = ListService2,
    ozt:rpc(handle, delete, [DataStamp2, HandleID2, ?SERVICE2]),

    ?assertEqual(length(ListService1)-10, length(list_all(?SERVICE1))),
    ?assertEqual(length(ListService2)-1, length(list_all(?SERVICE2))),
    ?assertEqual(length(ListAll)-11, length(list_all(all))).


%% checks if Handles are added sorted by data
add_handle_withe_earlier_datastamp_test(_Config) ->
    OldDataStamp1 = {{6,59,58}, {1999,9,29}},
    OldDataStamp2 = {{6,59,58}, {2012,9,29}},
    OldDataStamp3 = {{6,59,58}, {2018,9,29}},
    ozt:rpc(handle, add, [OldDataStamp3, ?RAND_ID(), ?RAND_SERVICE()]),
    ozt:rpc(handle, add, [OldDataStamp1, ?RAND_ID(), ?RAND_SERVICE()]),
    ozt:rpc(handle, add, [OldDataStamp2, ?RAND_ID(), ?RAND_SERVICE()]),
    {List, {resumption_token, _}}= ozt:rpc(handle, list, [all, #{}]),
    [{DataStamp1, _}, {DataStamp2, _}, {DataStamp3, _}] = lists:sublist(List, 3),
    ?assertEqual(OldDataStamp1, binary_to_term(DataStamp1)),
    ?assertEqual(OldDataStamp2, binary_to_term(DataStamp2)),
    ?assertEqual(OldDataStamp3, binary_to_term(DataStamp3)).


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


%%%===================================================================
%%% Helpers
%%%===================================================================

list_all(WhatToList) ->
    {List, {resumption_token, Token}} = list_handle_services(WhatToList),
    case Token of
        <<>> ->
            List;
        _ ->
            list_all(WhatToList, Token, List)
    end.

list_all(WhatToList, Token, ListAll) ->
    {List, {resumption_token, NewToken}} = list_handle_services(WhatToList, Token),
    NewList = ListAll ++ List,
    case NewToken of
        <<>> ->
            NewList;
        _ ->
            list_all(WhatToList, NewToken, NewList)
    end.


list_handle_services(WhatToList) ->
    ListingOpts = #{},
    ozt:rpc(handle, list, [WhatToList, ListingOpts]).

list_handle_services(WhatToList, Token) ->
    ListingOpts = #{resumption_token => get_token_bin(Token)},
    ozt:rpc(handle, list, [WhatToList, ListingOpts]).

generate_random_date() ->
    random:seed(os:timestamp()),
    Hour = random:uniform(24)-1,     % Hour (0-23)
    Minute = random:uniform(60)-1,   % Minute (0-59)
    Second = random:uniform(60)-1,   % Second (0-59)
    Year = random:uniform(4) + 2019,  % Year (from 2020 to 2023)
    Month = random:uniform(12),      % Month (1-12)
    Day = random:uniform(31),        % Dzień (1-31)
    {{Hour, Minute, Second}, {Year, Month, Day}}.

create_handle() ->
    ozt:rpc(handle, add, [generate_random_date(), ?RAND_ID(), ?RAND_SERVICE()]).

create_handle_for_service(Service) ->
    ozt:rpc(handle, add, [generate_random_date(), ?RAND_ID(), Service]).


get_token_bin(Token) ->
    {DataStamp, HandleId} = Token,
    <<DataStamp/binary, ?INDEX_SEP, HandleId/binary>>.

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
    lists:foreach(fun({DataStamp, HandleID}) ->
        ozt:rpc(handle, delete, [DataStamp, HandleID, ?SERVICE1])
    end, ListService1),
    lists:foreach(fun({DataStamp, HandleID}) ->
        ozt:rpc(handle, delete, [DataStamp, HandleID, ?SERVICE2])
    end, ListService2),
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(list_no_resumption_token, Config) ->
    lists_utils:pforeach(fun(_) ->
        create_handle_for_service(?SERVICE3)
    end, lists:seq(1, max(0, ?SMALL_SERVICE_SIZE))),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(list_no_resumption_token, Config) ->
    ListService3 = ozt:rpc(handle, list, [?SERVICE3, #{}]),
    lists:foreach(fun({DataStamp, HandleID}) ->
        ozt:rpc(handle, delete, [DataStamp, HandleID, ?SERVICE3])
    end, ListService3),
    Config;
end_per_testcase(_, Config) ->
    Config.
