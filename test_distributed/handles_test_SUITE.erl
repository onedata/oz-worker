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
%%    update_handle_timestamp_test/1,
    list_from_until_test/1,
    list_no_resumption_token_test/1,
    add_handle_to_service_test/1,
    add_handle_with_earlier_timestamp_test/1,
    delete_handle_from_service_test/1
]).

all() -> [
%%    {group, parallel_tests},
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
%%        update_handle_timestamp_test
    ]},
    {sequential_tests, [sequential], [
%%        list_from_until_test,
%%        list_no_resumption_token_test,
%%        add_handle_to_service_test,
        add_handle_with_earlier_timestamp_test
%%        delete_handle_from_service_test
    ]}
].

-define(TOTAL_HANDLE_COUNT, 3300).
-define(HANDLE_COUNT_IN_SMALL_HSERVICE, 600).
-define(EARLIEST_TIMESTAMP, 1200000000).
-define(LATEST_TIMESTAMP, 1700000000).
-define(FIRST_HSERVICE, <<"first">>).
-define(ANOTHER_HSERVICE, <<"another">>).
-define(SMALL_HSERVICE, <<"small">>).
-define(OAI_DC_METADATA_PREFIX, <<"oai_dc">>).
-define(EDM_METADATA_PREFIX, <<"edm">>).

-define(RAND_NAME(), ?RAND_UNICODE_STR(200)).
-define(RAND_ID(), str_utils:rand_hex(16)).
-define(RAND_SERVICE(), case ?RAND_BOOL() of
    true -> ?FIRST_HSERVICE;
    false -> ?ANOTHER_HSERVICE
end).
-define(RAND_METADATA_PREFIX(), case ?RAND_BOOL() of
    true -> ?OAI_DC_METADATA_PREFIX;
    false -> ?EDM_METADATA_PREFIX
end).
-define(checkListing(Opts), ?assertEqual( length(expected_handles(Opts)), length(list_all(Opts)))).


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
    ?assertEqual(expected_handles(oai_dc), List1 ++ List2),
    %% third listing, other prefix
    {List3, Token3} = list_once(#{metadata_prefix => ?EDM_METADATA_PREFIX}),
    ?assertEqual(DefaultListLimit, length(List3)),

    {List4, Token4} = list_once(#{resumption_token => Token3, metadata_prefix => ?EDM_METADATA_PREFIX}),
    ?assertEqual(undefined, Token4),
    ?assertEqual(expected_handles(edm), List3 ++ List4),
    ?assertEqual(3300, length(List1) + length(List2) + length(List3) + length(List4)).


list_no_resumption_token_test(_Config) ->
    MetadataPrefix =  ?RAND_METADATA_PREFIX(),
    {List, undefined} = list_once(#{service_id => ?SMALL_HSERVICE, metadata_prefix => MetadataPrefix}),
    ?assertEqual(expected_handles(binary_to_atom(<<MetadataPrefix/binary, <<"_hservice">>/binary>>)), List).


list_all_handle_test(_Config) ->
    ListAll = list_all(),
    ?assertEqual(?TOTAL_HANDLE_COUNT, length(ListAll)),
%%    # te length tutaj trzeba zamienic na porownywanie list (przegladnac wszystko)
    ?assertEqual(
        length(lists:sort(expected_handles(all_handles))),
        length(lists:sort(ListAll))
    ).


list_handles_from_services_test(_Config) ->
    ListFirstHServiceDC = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => ?OAI_DC_METADATA_PREFIX}),
    ListFirstHServiceEDM = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => ?EDM_METADATA_PREFIX}),

    ListAnotherHServiceDC = list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => ?OAI_DC_METADATA_PREFIX}),
    ListAnotherHServiceEDM = list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => ?EDM_METADATA_PREFIX}),
    ListSmallHServiceDC = list_all(#{service_id => ?SMALL_HSERVICE, metadata_prefix => ?OAI_DC_METADATA_PREFIX}),
    ListSmallHServiceEDM = list_all(#{service_id => ?SMALL_HSERVICE, metadata_prefix => ?EDM_METADATA_PREFIX}),

    ?assertEqual(length(expected_handles(oai_dc_hservice)), length(ListSmallHServiceDC)),
    ?assertEqual(length(expected_handles(edm_hservice)), length(ListSmallHServiceEDM)),
    ?assertEqual(length(lists:sort(expected_handles(oai_dc))),
        length(lists:sort(ListFirstHServiceDC ++ ListAnotherHServiceDC ++ ListSmallHServiceDC))).
%%    ?assertEqual(lists:sort(expected_handles(edm)),
%%        lists:sort(ListFirstHServiceEDM ++ ListAnotherHServiceEDM ++ ListSmallHServiceEDM)),
%%    ?assertEqual(
%%        ?TOTAL_HANDLE_COUNT,
%%        length(ListFirstHServiceDC) + length(ListFirstHServiceEDM)
%%            + length(ListAnotherHServiceDC) + length(ListAnotherHServiceEDM)
%%            + length(ListSmallHServiceDC) + length(ListSmallHServiceEDM)
%%    ).


list_handles_with_metadata_format_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ActualList = list_all(#{metadata_prefix => MetadataPrefix}),
    ?assertEqual(length(expected_handles(binary_to_atom(MetadataPrefix))), length(ActualList)),

    ActualListMetadataAndService = list_all(#{
        metadata_prefix => MetadataPrefix,
        service_id => ?SMALL_HSERVICE
    }),
    ?assertEqual(length(expected_handles(
        binary_to_atom(<<MetadataPrefix/binary, <<"_hservice">>/binary>>))), length(ActualListMetadataAndService)).


list_size_elements_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    {List, undefined} = list_once(#{size => 5, metadata_prefix => MetadataPrefix}),
    ?assertEqual(lists:sublist(expected_handles(binary_to_atom(MetadataPrefix)), 5), List),
    ?assertEqual(5, length(List)).


add_element_that_already_exist_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    ListService = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}),
    BeforeAdding = length(ListService),
    HandleId = hd(ListService),
    TimeStamp = lookup_timestamp(HandleId),
    try
        ozt:rpc(handles, add, [MetadataPrefix, ?FIRST_HSERVICE, HandleId, TimeStamp])
    catch
        error:ErrorReason:_Stacktrace ->
            ?assertEqual(ErrorReason, {badrpc, ?ERROR_ALREADY_EXISTS})
    end,
    AfterAdding = length(list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix})),
    ?assertEqual(BeforeAdding, AfterAdding).

%%update_handle_timestamp_test(_Config) ->
%%    MetadataPrefix = ?RAND_METADATA_PREFIX(),
%%    ListService = list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}),
%%    HandleId = lists:last(ListService),
%%    NewTimeStamp = ?EARLIEST_TIMESTAMP - 1,
%%
%%    OldTimeStamp = lookup_timestamp(HandleId),
%%
%%    ozt:rpc(handles, update, [
%%        MetadataPrefix, ?FIRST_HSERVICE, HandleId, OldTimeStamp, NewTimeStamp
%%    ]),
%% trzeba będzie się pozbyć tych node:cache
%%    node_cache:clear(binary_to_atom(MetadataPrefix)),
%%    node_cache:clear(all_handles),
%%    node_cache:put(binary_to_atom(MetadataPrefix),
%%        list_all(#{metadata_prefix => MetadataPrefix})),
%%    node_cache:put(all_handles, list_all()),
%%    EarliestHID = hd(list_all()),
%%    ct:pal("New ~p ~n Earliest ~p~n~n", [HandleId, EarliestHID]).


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
    {TimeStampSeconds, HandleId, _, _} = hd(get_expected_handles(
        binary_to_atom(MetadataPrefix))),
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

    {TimeStamp, HandleId, ?FIRST_HSERVICE, MetadataPrefix} = create_handle_for_service(?FIRST_HSERVICE, MetadataPrefix),
    update_expected_handles([{TimeStamp, HandleId, ?FIRST_HSERVICE, MetadataPrefix}], MetadataPrefix, ?FIRST_HSERVICE),
    ?assertEqual(BeforeAddingAll + 1, length(list_all(#{metadata_prefix => MetadataPrefix}))),

    List = lists_utils:pmap(fun(_) ->
        create_handle_for_service(?ANOTHER_HSERVICE, MetadataPrefix)
    end, lists:seq(1, 10)),
%%    ct:pal("~n BEFORE ALL HANDLES ~p~n", [length(get_expected_handles(all_handles))]),
%%    ct:pal("~n BEFORE MetadataPrefix ~p~n", [length(get_expected_handles(binary_to_atom(MetadataPrefix)))]),
    update_expected_handles(List, MetadataPrefix, ?ANOTHER_HSERVICE),
%%    ct:pal("~nALL HANDLES ~p~n", [length(get_expected_handles(all_handles))]),
%%    ct:pal("~n MetadataPrefix ~p~n", [length(get_expected_handles(binary_to_atom(MetadataPrefix)))]),
    ?assertEqual(BeforeAddingService1 + 1,
        length(list_all(#{service_id => ?FIRST_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(BeforeAddingService2 + 10,
        length(list_all(#{service_id => ?ANOTHER_HSERVICE, metadata_prefix => MetadataPrefix}))),
    ?assertEqual(BeforeAddingAll + 11, length(list_all(#{metadata_prefix => MetadataPrefix}))).


%% checks if handles are added sorted by date
add_handle_with_earlier_timestamp_test(_Config) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Service = ?RAND_SERVICE(),
    [FirstHandle] = list_all(#{size => 1, metadata_prefix => MetadataPrefix}),
    TimeStamp = lookup_timestamp(FirstHandle) - ?RAND_INT(1, 10000),
    [{_, HandleId3, _, _}, {_, HandleId2, _, _}, {_, HandleId1, _, _}] = NewHandles =
        lists:map(fun(Number) ->
            OldTimestamp = TimeStamp - Number,
            HandleId = ?RAND_ID(),
            ozt:rpc(handles, add, [MetadataPrefix, Service, HandleId, OldTimestamp]),
            {OldTimestamp, HandleId, Service, MetadataPrefix}
        end, lists:seq(1, 3)
        ),

    update_expected_handles(NewHandles, MetadataPrefix, Service),

    {List, _Token} = list_once(#{metadata_prefix => MetadataPrefix}),
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
            MetadataPrefix, ?FIRST_HSERVICE, HandleId1,
            lookup_timestamp(HandleId1)

        ])
    end, lists:sublist(ListService1, 10)),

    ?assertEqual(length(ListAll) - 10, length(list_all(#{metadata_prefix => MetadataPrefix}))),

    [HandleId2 | _RestList] = ListService2,
    ozt:rpc(handles, delete, [
        MetadataPrefix, ?ANOTHER_HSERVICE, HandleId2,
        lookup_timestamp(HandleId2)
    ]),

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
    rand:uniform(
        ?LATEST_TIMESTAMP - ?EARLIEST_TIMESTAMP + 1
    ) + ?EARLIEST_TIMESTAMP - 1.

create_handle() ->
    create_handle_for_service(?RAND_SERVICE()).

create_handle_for_service(Service) ->
    create_handle_for_service(Service, ?RAND_METADATA_PREFIX()).

create_handle_for_service(Service, MetadataPrefix) ->
    TimeSeconds = generate_rand_timestamp(),
    HandleId = ?RAND_ID(),
    ozt:rpc(handles, add, [MetadataPrefix, Service, HandleId, TimeSeconds]),
    NewHandle = {TimeSeconds, HandleId, Service, MetadataPrefix},
%%    update_expected_handles([NewHandle], MetadataPrefix, Service),
    NewHandle.

%%create_handle_for_service_update_expected(Service, MetadataPrefix) ->
%%    TimeSeconds = generate_rand_timestamp(),
%%    HandleId = ?RAND_ID(),
%%    ozt:rpc(handles, add, [MetadataPrefix, Service, HandleId, TimeSeconds]),
%%    NewHandle = {TimeSeconds, HandleId, Service, MetadataPrefix},
%%    update_expected_handles([NewHandle], MetadataPrefix, Service),
%%    NewHandle.

lookup_handle_attributes(HandleId) ->
    Expected = get_expected_handles(all_handles),
    [{TimeStamp, _HId, HSId, MetadataPrefix}] = lists:filter(
        fun({_TimeStamp, HId, _HSId, _MetadataPrefix}) ->
            HId == HandleId end, Expected
    ),
    {TimeStamp, HSId, MetadataPrefix}.

lookup_timestamp(HandleId) ->
    {TimeStamp, _, _} = lookup_handle_attributes(HandleId),
    TimeStamp.

expected_handles(TargetedHandles) when is_atom(TargetedHandles)  ->
    HandlesList = get_expected_handles(TargetedHandles),
    [HandleId || {_, HandleId, _, _} <- HandlesList];

expected_handles(Opts) when is_map(Opts)->
    MetadataPrefix = maps:get(metadata_prefix, Opts),
    From = maps:get(from, Opts, ?EARLIEST_TIMESTAMP),
    Until = maps:get(until, Opts, ?LATEST_TIMESTAMP),
    AllHandles = get_expected_handles(binary_to_atom(MetadataPrefix)),
    ListFrom = lists:dropwhile(fun({TimeStamp, _, _, _}) -> TimeStamp < From end, AllHandles),
    ListFromUntil = lists:takewhile(fun({TimeStamp, _, _, _}) -> TimeStamp =< Until end, ListFrom),
    [HandleId || {_, HandleId, _, _} <- ListFromUntil].

update_expected_handles(NewHandles, MetadataPrefix, Service) ->
    update_expected_handles(NewHandles, all_handles),
    update_expected_handles(NewHandles, binary_to_atom(MetadataPrefix)),
    case Service of
        ?SMALL_HSERVICE -> update_expected_handles(NewHandles,
            binary_to_atom(<<MetadataPrefix/binary, <<"_hservice">>/binary>>)),
            update_expected_handles(NewHandles, small_hservice);
        _ -> ok
    end.

update_expected_handles(NewHandles, NodeAtom) ->
    OldHandles = get_expected_handles(NodeAtom),
    UpdatedHandles = lists:sort(fun({TimeSeconds1, _, _, _}, {TimeSeconds2, _, _, _}) ->
        TimeSeconds1 < TimeSeconds2
    end, OldHandles ++ NewHandles),
    clear_expected_handles(NodeAtom),
    save_expected_handles(NodeAtom, UpdatedHandles).

get_expected_handles(TargetedHandles) ->
    node_cache:get(TargetedHandles, []).

save_expected_handles(TargetedHandles, HandlesList) ->
    node_cache:put(TargetedHandles, HandlesList).

clear_expected_handles(TargetedHandles) ->
    node_cache:clear(TargetedHandles).

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
        save_expected_handles(small_hservice, SortedSmallHServiceList),
        {OAI_DC_Service, EDM_Service} = lists:partition(fun({_, _, _, MetadataPrefix}) ->
            case MetadataPrefix of
                ?OAI_DC_METADATA_PREFIX -> true;
                ?EDM_METADATA_PREFIX -> false
            end
        end, SortedSmallHServiceList),
        save_expected_handles(oai_dc_hservice, OAI_DC_Service),
        save_expected_handles(edm_hservice, EDM_Service),

        HandleList = lists_utils:pmap(fun(_) ->
            create_handle()
        end, lists:seq(1, max(0, ?TOTAL_HANDLE_COUNT - ?HANDLE_COUNT_IN_SMALL_HSERVICE))),
        SortedHandles = lists:sort(
            fun({TimeSeconds1, _, _, _}, {TimeSeconds2, _, _, _}) ->
                TimeSeconds1 < TimeSeconds2
            end, HandleList ++ SmallHServiceList),
        save_expected_handles(all_handles, SortedHandles),
        {OAI_DC, EDM} = lists:partition(fun({_, _, _, MetadataPrefix}) ->
            case MetadataPrefix of
                ?OAI_DC_METADATA_PREFIX -> true;
                ?EDM_METADATA_PREFIX -> false
            end
        end, SortedHandles),
        save_expected_handles(oai_dc, OAI_DC),
        save_expected_handles(edm, EDM)
    end).

end_per_suite(_Config) ->
%%    AllHandles = list_all(),
    lists:foreach(fun({TimeSeconds, HandleID, HandleServiceId, MetadataPrefix}) ->
%%    lists:foreach(fun(HandleID) ->
%%        {ok, #document{value = #od_handle{
%%            handle_service = HandleServiceId,
%%            timestamp = TimeSeconds,
%%            metadata_prefix = MetadataPrefix
%%        }}} = ozt:rpc(od_handle, get, [HandleID]),
        ozt:rpc(handles, delete, [
            MetadataPrefix, HandleServiceId, HandleID, TimeSeconds
        ])
%%    end, AllHandles),
    end, get_expected_handles(all_handles)),
    clear_expected_handles(all_handles),
    clear_expected_handles(oai_dc),
    clear_expected_handles(edm),
    lists:foreach(fun({TimeSeconds, HandleID, HandleServiceId, MetadataPrefix}) ->
        ozt:rpc(handles, delete, [
            MetadataPrefix, HandleServiceId, HandleID, TimeSeconds
        ])
    end, get_expected_handles(small_hservice)),
    clear_expected_handles(small_hservice),
    clear_expected_handles(oai_dc_hservice),
    clear_expected_handles(edm_hservice),
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.
