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


%%%===================================================================
%%% Tests
%%%===================================================================

encode_decode_test() ->
    ?assertEqual(
        #{<<"pending">> => #{}, <<"rejected">> => #{}},
        jsonable_record:to_json(space_membership_requests:empty())
    ),

    lists_utils:generate(fun() ->
        ?assert(eunit_utils:is_equal_after_db_encode_and_decode(persistent_record:from_string(
            random_record_db_encoded(), space_membership_requests
        ))),
        ?assert(eunit_utils:is_equal_after_json_encode_and_decode(jsonable_record:from_json(
            random_record_json(), space_membership_requests
        )))
    end, 20).

%%%===================================================================
%%% Helpers
%%%===================================================================

random_record_json() ->
    #{
        <<"pending">> => maps_utils:generate(fun() -> {?RAND_STR(), random_request_json()} end, 50),
        <<"rejected">> => maps_utils:generate(fun() -> {?RAND_STR(), random_request_json()} end, 50)
    }.


random_record_db_encoded() ->
    json_utils:encode(#{
        <<"_version">> => 1,
        <<"_data">> => maps:merge(random_record_json(), #{
            <<"lastPendingRequestPruningTime">> => ?RAND_INT(0, 12716914191)
        })
    }).


random_request_json() ->
    #{
        <<"requestId">> => ?RAND_STR(0),
        <<"contactEmail">> => ?RAND_STR(0),
        <<"lastActivity">> => ?RAND_INT(0, 17784321954)
    }.


-endif.

