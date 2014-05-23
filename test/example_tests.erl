%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc This module tests is the example eunit test
%%% @end
%%% Created : 15. May 2014 12:11
%%%-------------------------------------------------------------------
-module(example_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).

%% ===================================================================
%% Tests description
%% ===================================================================

example_test_() ->
  {foreach,
    fun setup/0,
    fun teardown/1,
    [
      {"example test", fun mock_example/0}
    ]
  }.

%% ===================================================================
%% Setup/teardown functions
%% ===================================================================

setup() ->
  ok.

teardown(_) ->
  ok.

%% ===================================================================
%% Tests functions
%% ===================================================================

mock_example() ->
  ExpectedAns="<html></html>",
  meck:new(ibrowse),
  meck:expect(ibrowse,send_req,fun(_,_,_) -> ExpectedAns end),
  ?assertEqual(ExpectedAns, ibrowse:send_req("url",[],get)),
  meck:unload(ibrowse).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-endif.