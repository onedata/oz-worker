%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This module contains performance tests of subscriptions mechanism.
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions_performance_test_SUITE).
-author("Jakub Kudzia").

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([generate_spaces_test/1, generate_spaces_test_base/1]).

all() -> ?ALL([], [generate_spaces_test]).

-define(MESSAGES_WAIT_TIMEOUT, timer:seconds(2)).

-define(PROVIDERS_COUNT(Value), [
    {name, providers_count},
    {value, Value},
    {description, "Number of providers (threads) used during the test."}
]).

-define(DOCS_COUNT(Value), [
    {name, docs_count},
    {value, 3},
    {description, "Number of documents used by a single thread/provider."}
]).

-define(CFG(CfgName, Descr, ProvCount, DocsCount), {config,
    [
        {name, CfgName},
        {description, Descr},
        {parameters, [?PROVIDERS_COUNT(ProvCount), ?DOCS_COUNT(DocsCount)]}
    ]
}).


%%%===================================================================
%%% Test functions
%%%===================================================================
generate_spaces_test(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 10},
        {success_rate, 95},
        {parameters, [
            [{name, providers_count}, {value, 10}, {description, "Number of providers (threads) used during the test."}],
            [{name, docs_count}, {value, 3}, {description, "Number of documents used by a single thread/provider."}]
        ]},
        {description, "Performs document saves and gathers subscription updated for many providers"},
        ?CFG(one_provider,
            "One provider saving small number of documents", 1, 100),
        ?CFG(two_providers,
            "One provider saving medium number of documents", 2, 100),
        ?CFG(three_providers,
            "One provider saving big number of documents", 3, 100),
        ?CFG(four_providers,
            "One provider saving small number of documents", 4, 100),
        ?CFG(five_providers,
            "One provider saving medium number of documents", 5, 100),
        ?CFG(six_providers,
            "One provider saving big number of documents", 6, 100),
        ?CFG(seven_providers,
            "One provider saving small number of documents", 7, 100),
        ?CFG(eight_providers,
            "One provider saving small number of documents", 8, 100),
        ?CFG(nine_providers,
            "One provider saving small number of documents", 9, 100),
        ?CFG(ten_providers,
            "One provider saving small number of documents", 10, 100),
        ?CFG(fifteen_providers,
            "One provider saving small number of documents", 15, 100),
        ?CFG(twenty_providers,
            "One provider saving small number of documents", 20, 100),
        ?CFG(twenty_five_providers,
            "One provider saving small number of documents", 25, 100),
        ?CFG(twenty_five_providers,
            "One provider saving small number of documents", 50, 100),
        ?CFG(twenty_five_providers,
            "One provider saving small number of documents", 100, 100)

    ]).
%%
generate_spaces_test_base(Config) ->
    subscriptions_test_SUITE:stress_test(Config).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).

init_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, group_graph),
    test_utils:mock_expect(Nodes, group_graph, refresh_effective_caches, fun() -> ok end),
    Config.

end_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes),
    flush(),
    ok.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).



flush() ->
    receive _ -> flush() after ?MESSAGES_WAIT_TIMEOUT -> ok end.