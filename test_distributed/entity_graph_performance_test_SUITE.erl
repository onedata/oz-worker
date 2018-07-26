%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains dummy performance test suite, to be implemented.
%%% @end
%%%-------------------------------------------------------------------
-module(entity_graph_performance_test_SUITE).
-author("Lukasz Opiola").

-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-define(PERF_PARAM(Name, Value, Unit, Description), [
    {name, Name},
    {value, Value},
    {description, Description},
    {unit, Unit}
]).
-define(PERF_CFG(Name, Params), {config, [
    {name, Name},
    {description, atom_to_list(Name)},
    {parameters, Params}
]}).


%% API
-export([all/0]).

-export([
    dummy_performance_test/1, dummy_performance_test_base/1
]).

-define(TEST_CASES, []).

-define(PERFORMANCE_TEST_CASES, [
    dummy_performance_test
]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() ->
    ?ALL(?TEST_CASES, ?PERFORMANCE_TEST_CASES).

%%%===================================================================
%%% Performance tests
%%%===================================================================

dummy_performance_test(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 1},
        {success_rate, 100},
        {description, "Dummy test."},
        {parameters, []},
        ?PERF_CFG(dummy, [])
    ]).
dummy_performance_test_base(_Config) ->
    ok.