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

-include("datastore/oz_datastore_models_def.hrl").
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
    {value, Value},
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
        %%        todo update descriptions
        ?CFG(one_provider,
            "One provider saving 100 documents", 1, 100),
        ?CFG(two_providers,
            "Two providers saving 100 documents", 2, 100),
        ?CFG(three_providers,
            "Three providers saving 100 documents", 3, 100),
        ?CFG(four_providers,
            "Four providers saving 100 documents", 4, 100),
        ?CFG(five_providers,
            "Five providers saving 100 documents", 5, 100),
        ?CFG(six_providers,
            "Six providers saving 100 documents", 6, 100),
        ?CFG(seven_providers,
            "Seven providers saving 100 documents", 7, 100),
        ?CFG(eight_providers,
            "Eight providers saving 100 documents", 8, 100),
        ?CFG(nine_providers,
            "Nine providers saving 100 documents", 9, 100),
        ?CFG(ten_providers,
            "Ten providers saving 100 documents", 10, 100),
        ?CFG(fifteen_providers,
            "Fifteen providers saving 100 documents", 15, 100),
        ?CFG(twenty_providers,
            "Twenty providers saving 100 documents", 20, 100),
        ?CFG(twenty_five_providers,
            "Twenty five providers saving 100 documents", 25, 100),
        ?CFG(fifty_providers,
            "Fifty providers saving 100 documents", 50, 100),
        ?CFG(hundred_providers,
            "Hundred providers saving 100 documents", 100, 100)

    ]).

generate_spaces_test_base(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),

    ProvidersCount = ?config(providers_count, Config),
    DocsCount = ?config(docs_count, Config),

    Results = utils:pmap(fun(ID) ->
        %% given
        PNameList = "provider_" ++ integer_to_list(ID),
        PName = list_to_binary(PNameList),
        SIDs = lists:map(fun(ID1) ->
            list_to_binary("space_" ++ integer_to_list(ID1) ++ "@" ++ PNameList)
                         end, lists:seq(1, DocsCount)),

        %% when
        PID = subscriptions_test_utils:create_provider(Node, PName, SIDs),
        Space = #space{name = <<"name">>, providers_supports = [{PID, 0}]},
        Context = init_messages(Node, PID, []),
        lists:map(fun(SID) ->
            subscriptions_test_utils:save(Node, SID, Space)
                  end, SIDs),

        %% then
        Start = erlang:system_time(milli_seconds),
        subscriptions_test_utils:verify_messages_present(Context,
            lists:map(fun(SID) -> subscriptions_test_utils:expectation(SID, Space) end, SIDs)
        ),
        {ok, erlang:system_time(milli_seconds) - Start}
                         end, lists:seq(1, ProvidersCount)),

    lists:map(fun(Res) ->
        ?assertMatch({ok, _}, Res)
              end, Results),

    UpdatesMeanTime = lists:sum(lists:map(fun
                                              ({ok, Time}) -> Time
                                          end, Results)) / length(Results),

    [
        #parameter{name = updates_await, value = UpdatesMeanTime, unit = "ms",
            description = "Time until every update arrived (providers mean)"}
    ].

%%modify_space_test(Config) ->
%%    ?PERFORMANCE(Config, [
%%        {repeats, 10},
%%        {success_rate, 95},
%%        {parameters, [
%%            [{name, providers_count}, {value, 10}, {description, "Number of providers (threads) used during the test."}],
%%            [{name, docs_count}, {value, 3}, {description, "Number of documents used by a single thread/provider."}]
%%        ]},
%%        {description, "Performs document saves and gathers subscription updated for many providers"}
%%    ]).
%%
%%modify_space_test_base(Config) ->
%%
%%    {ok, UserId} = ?assertMatch({ok, _},
%%        oz_test_utils:create_user(Config, #onedata_user{})),
%%
%%    SpaceName1 = <<"space_name">>,
%%    {ok, SpaceId1} = ?assertMatch({ok, _},
%%        oz_test_utils:create_space(Config, {user, UserId}, SpaceName1)),
%%
%%    ProvidersCount = ?config(providers_count, Config),
%%    DocsCount = ?config(docs_count, Config),
%%
%%    Results = utils:pmap(fun(ID) ->
%%        %% given
%%        PNameList = "provider_" ++ integer_to_list(ID),
%%        PName = list_to_binary(PNameList),
%%        SIDs = lists:map(fun(ID1) ->
%%            list_to_binary("space_" ++ integer_to_list(ID1) ++ "@" ++ PNameList)
%%                         end, lists:seq(1, DocsCount)),
%%
%%        %% when
%%        PID = create_provider(Node, PName, SIDs),
%%        Space = #space{name = <<"name">>, providers_supports = [{PID, 0}]},
%%        Context = init_messages(Node, PID, []),
%%        lists:map(fun(SID) ->
%%            save(Node, SID, Space)
%%                  end, SIDs),
%%
%%        %% then
%%        Start = erlang:system_time(milli_seconds),
%%        verify_messages_present(Context,
%%            lists:map(fun(SID) -> expectation(SID, Space) end, SIDs)
%%        ),
%%        {ok, erlang:system_time(milli_seconds) - Start}
%%                         end, lists:seq(1, ProvidersCount)),
%%
%%    lists:map(fun(Res) ->
%%        ?assertMatch({ok, _}, Res)
%%              end, Results),
%%
%%    UpdatesMeanTime = lists:sum(lists:map(fun
%%                                              ({ok, Time}) -> Time
%%                                          end, Results)) / length(Results),
%%
%%    [
%%        #parameter{name = updates_await, value = UpdatesMeanTime, unit = "ms",
%%            description = "Time until every update arrived (providers mean)"}
%%    ].



%%todo similiar but modifying space_name

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).

init_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, group_graph),
    test_utils:mock_expect(Nodes, group_graph, refresh_effective_caches,
        fun() -> ok end),
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