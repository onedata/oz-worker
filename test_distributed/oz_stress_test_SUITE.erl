%%%--------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This test checks subscriptions.
%%% @end
%%%--------------------------------------------------------------------
-module(oz_stress_test_SUITE).
-author("Michal Zmuda").

-include("subscriptions_test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%%tests
-export([stress_test/1, subscriptions_generate_spaces_stress_test/1,
    subscriptions_modify_space_stress_test/1]).
%%test_bases
-export([stress_test_base/1, subscriptions_generate_spaces_stress_test_base/1,
    subscriptions_modify_space_stress_test_base/1]).

-define(STRESS_CASES, [
]).

-define(STRESS_NO_CLEARING_CASES, [
    subscriptions_generate_spaces_stress_test,
    subscriptions_modify_space_stress_test_base
]).

all() ->
    ?STRESS_ALL(?STRESS_CASES, ?STRESS_NO_CLEARING_CASES).

%%%===================================================================
%%% Test functions
%%%===================================================================

stress_test(Config) ->
    ?STRESS(Config, [
        {description, "Main stress test function. Links together all cases to be done multiple times as one continous test."},
        {success_rate, 95},
        {config, [{name, stress}, {description, "Basic config for stress test"}]}
    ]).
stress_test_base(Config) ->
    ?STRESS_TEST_BASE(Config).

%%%===================================================================


subscriptions_generate_spaces_stress_test(Config) ->
    ?PERFORMANCE(Config, [
        {parameters, [
            ?PROVIDERS_NUM(10),
            ?DOCS_NUM(10)
        ]},
        {description, "Performs document saves and gathers subscription updated for many providers"}
    ]).
subscriptions_generate_spaces_stress_test_base(Config) ->
    subscriptions_performance_test_SUITE:generate_spaces_test(Config).


%%%===================================================================


subscriptions_modify_space_stress_test(Config) ->
    ?PERFORMANCE(Config, [
        {parameters, [
            ?DOCUMENT_MODIFICATIONS_NUM(100),
            ?USERS_NUM(25),
            ?GROUPS_NUM(5)
        ]},
        {description, "Performs document saves and gathers subscription updated for many providers"}
    ]).
subscriptions_modify_space_stress_test_base(Config) ->
    subscriptions_performance_test_SUITE:generate_spaces_test(Config).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
