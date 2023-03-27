%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of space_membership_requests module.
%%% Other tests of the module are placed in CT suite space_marketplace_test_SUITE.
%%% @end
%%%-------------------------------------------------------------------
-module(space_membership_requests_tests).
-author("Lukasz Opiola").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include("entity_logic.hrl").


encode_decode_test() ->
    ?assertEqual(
        #{<<"pending">> => #{}, <<"rejected">> => #{}, <<"lastPendingRequestPruningTime">> => 0},
        jsonable_record:to_json(space_membership_requests:empty())
    ),

    Records = lists_utils:generate(fun() ->
        jsonable_record:from_json(#{
            <<"pending">> => maps_utils:generate(fun() -> {?RAND_STR(), random_request_json()} end, 50),
            <<"rejected">> => maps_utils:generate(fun() -> {?RAND_STR(), random_request_json()} end, 50),
            <<"lastPendingRequestPruningTime">> => ?RAND_INT(0, 12716914191)
        }, space_membership_requests)
    end, 10),
    lists:foreach(fun(Record) ->
        ?assert(eunit_utils:is_equal_after_json_encode_and_decode(Record)),
        ?assert(eunit_utils:is_equal_after_db_encode_and_decode(Record))
    end, Records).


random_request_json() ->
    #{
        <<"requestId">> => ?RAND_STR(0),
        <<"contactEmail">> => ?RAND_STR(0),
        <<"lastActivity">> => ?RAND_INT(0, 17784321954)
    }.


-endif.

