%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This is an example test module. It contains unit tests that base on eunit.
%%% @end
%%%-------------------------------------------------------------------
-module(example_tests).
-author("Tomasz Lichon").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests description
%%%===================================================================

example_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"example test", fun mock_example/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(http_client).

teardown(_) ->
    meck:unload(http_client).

%%%===================================================================
%%% Tests functions
%%%===================================================================

mock_example() ->
    ExpectedAns = "<html></html>",
    meck:expect(http_client, get, fun(_, _) -> ExpectedAns end),
    ?assertEqual(ExpectedAns, http_client:get("url", [])).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-endif.