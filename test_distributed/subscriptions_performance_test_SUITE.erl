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

-include("subscriptions_test_utils.hrl").
-include("subscriptions/subscriptions.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([generate_spaces_test/1, space_update_test/1]).
-export([generate_spaces_test_base/1, space_update_test_base/1]).

all() -> ?ALL([], [
    generate_spaces_test,
    space_update_test
]).
%%        ,
%%    modify_space_record_test


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

-define(DOCS_MODIFICATIONS_COUNT(Value), [
    {name, docs_modifications_count},
    {value, Value},
    {description, "Number of modifications on document, performed by a single thread/provider."}
]).

-define(USERS_NUM(Value), [
    {name, users_num},
    {value, Value},
    {description, "Number of users."}
]).

-define(GROUPS_NUM(Value), [
    {name, groups_num},
    {value, Value},
    {description, "Number of groups."}
]).


-define(GEN_CFG(CfgName, Descr, ProvCount, DocsCount), {config,
    [
        {name, CfgName},
        {description, Descr},
        {parameters, [?PROVIDERS_COUNT(ProvCount), ?DOCS_COUNT(DocsCount)]}
    ]
}).

-define(MOD_CFG(CfgName, Descr, ProvCount, ModsCount), {config,
    [
        {name, CfgName},
        {description, Descr},
        {parameters,
            [
                ?PROVIDERS_COUNT(ProvCount),
                ?DOCS_MODIFICATIONS_COUNT(ModsCount)
            ]
        }
    ]
}).

-define(UPDATE_CFG(CfgName, Descr, ModsCount, UsersNum, GroupsNum), {config,
    [
        {name, CfgName},
        {description, Descr},
        {parameters,
            [
                ?DOCS_MODIFICATIONS_COUNT(ModsCount),
                ?USERS_NUM(UsersNum),
                ?GROUPS_NUM(GroupsNum)
            ]
        }
    ]
}).

-define(DOCS_NUM, 10).

%%%===================================================================
%%% Test functions
%%%===================================================================
generate_spaces_test(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 10},
        {success_rate, 95},
        {description, "Performs document saves and gathers subscription updated for many providers"},
        ?GEN_CFG(one_provider, "One provider saving 100 documents", 1,
            ?DOCS_NUM),
        ?GEN_CFG(two_providers, "Two providers saving 100 documents", 2,
            ?DOCS_NUM),
        ?GEN_CFG(three_providers, "Three providers saving 100 documents", 3,
            ?DOCS_NUM),
        ?GEN_CFG(four_providers, "Four providers saving 100 documents", 4,
            ?DOCS_NUM),
        ?GEN_CFG(five_providers, "Five providers saving 100 documents", 5,
            ?DOCS_NUM),
        ?GEN_CFG(six_providers, "Six providers saving 100 documents", 6,
            ?DOCS_NUM),
        ?GEN_CFG(seven_providers, "Seven providers saving 100 documents", 7,
            ?DOCS_NUM),
        ?GEN_CFG(eight_providers, "Eight providers saving 100 documents", 8,
            ?DOCS_NUM),
        ?GEN_CFG(nine_providers, "Nine providers saving 100 documents", 9,
            ?DOCS_NUM),
        ?GEN_CFG(ten_providers, "Ten providers saving 100 documents", 10,
            ?DOCS_NUM),
        ?GEN_CFG(fifteen_providers, "Fifteen providers saving 100 documents",
            15, ?DOCS_NUM),
        ?GEN_CFG(twenty_providers, "Twenty providers saving 100 documents", 20,
            ?DOCS_NUM),
        ?GEN_CFG(twenty_five_providers,
            "Twenty five providers saving 100 documents", 25, ?DOCS_NUM),
        ?GEN_CFG(fifty_providers, "Fifty providers saving 100 documents", 50,
            ?DOCS_NUM),
        ?GEN_CFG(hundred_providers, "Hundred providers saving 100 documents",
            100, ?DOCS_NUM)
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
        Context = subscriptions_test_utils:init_messages(Node, PID, []),

        lists:map(fun(SID) ->
            subscriptions_test_utils:save(Node, SID, Space)
        end, SIDs),

        %% then
        Start = erlang:system_time(milli_seconds),
        subscriptions_test_utils:verify_messages_present(Context,
            lists:map(fun(SID) ->
                subscriptions_test_utils:expectation(SID, Space)
            end, SIDs)
        ),
        {ok, erlang:system_time(milli_seconds) - Start}
    end, lists:seq(1, ProvidersCount)),

    lists:map(fun(Res) ->
        ?assertMatch({ok, _}, Res)
    end, Results),

    UpdatesMeanTime = lists:sum(lists:map(fun({ok, Time}) -> Time end, Results)) / length(Results),

    [
        #parameter{name = updates_await, value = UpdatesMeanTime, unit = "ms",
            description = "Time until every update arrived (providers mean)"}
    ].

%%modify_space_record_test(Config) ->
%%    ?PERFORMANCE(Config, [
%%        {repeats, 1},
%%        {success_rate, 95},
%%        {description, "Performs document saves and gathers subscription updated for many providers"},
%%        ?MOD_CFG(one_provider, "One provider modifying document 10 times", 1, 10)
%%
%%    ]).
%%
%%modify_space_record_test_base(Config) ->
%%    % given
%%    [Node | _] = ?config(oz_worker_nodes, Config),
%%
%%    ProvidersCount = ?config(providers_count, Config),
%%    DocModificationsCount = ?config(docs_modifications_count, Config),
%%
%%    {ok, UserId} = ?assertMatch({ok, _},
%%        oz_test_utils:create_user(Config, #onedata_user{})),
%%
%%    InitialSpaceName = <<"space_name">>,
%%    {ok, SpaceId} = ?assertMatch({ok, _},
%%        oz_test_utils:create_space(Config, {user, UserId}, InitialSpaceName)),
%%
%%    Results = utils:pmap(fun(ID) ->
%%        %% given
%%        PNameList = "provider_" ++ integer_to_list(ID),
%%        PName = list_to_binary(PNameList),
%%
%%        SpaceNames = lists:map(fun(ID1) ->
%%            list_to_binary("space_" ++ integer_to_list(ID1) ++ "@" ++ PNameList)
%%        end, lists:seq(1, DocModificationsCount)),
%%
%%        %% when
%%        PID = subscriptions_test_utils:create_provider(Node, PName, [SpaceId]),
%%        Context = subscriptions_test_utils:init_messages(Node, PID, []),
%%
%%        Spaces = lists:map(fun(SpaceName) ->
%%            Space = #space{name = SpaceName, providers_supports = [{PID, 0}]},
%%            subscriptions_test_utils:save(Node, SpaceId, Space),
%%            Space
%%        end, SpaceNames),
%%
%%        %% then
%%        Start = erlang:system_time(milli_seconds),
%%        subscriptions_test_utils:verify_messages_present(Context,
%%            lists:map(fun(Space) ->
%%                subscriptions_test_utils:expectation(SpaceId, Space)
%%            end, Spaces)
%%        ),
%%        {ok, erlang:system_time(milli_seconds) - Start}
%%    end, lists:seq(1, ProvidersCount)),
%%
%%    lists:map(fun(Res) ->
%%        ?assertMatch({ok, _}, Res)
%%    end, Results),
%%
%%    UpdatesMeanTime = lists:sum(lists:map(fun({ok, Time}) -> Time end, Results)) / length(Results),
%%
%%    [
%%        #parameter{name = updates_await, value = UpdatesMeanTime, unit = "ms",
%%            description = "Time until every update arrived (providers mean)"}
%%    ].

space_update_test(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 10},
        {parameters, [?DOCS_MODIFICATIONS_COUNT(10), ?USERS_NUM(2), ?GROUPS_NUM(4)]},
        {success_rate, 95},
        {description, "Performs document updates and gathers subscription updated for provider"},
        ?UPDATE_CFG(one_provider, "One provider modifying document 10 times", 10, 2, 4)
    ]).

space_update_test_base(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),

    UsersNum = ?config(users_num, Config),
    GroupsNum = ?config(groups_num, Config),
    UpdatesNum = ?config(docs_modifications_count, Config),

    PID = subscriptions_test_utils:create_provider(Node, subscriptions_test_utils:id("p1"), []),
    GIDs = subscriptions_test_utils:generate_group_ids(GroupsNum),
    UIDs = subscriptions_test_utils:generate_user_ids(UsersNum),
    SIDs = subscriptions_test_utils:generate_space_ids(1),

    [{SID1, S1} | _] = subscriptions_test_utils:create_spaces(SIDs, UIDs, GIDs, Node),
    Users = subscriptions_test_utils:create_users(UIDs, GIDs, Node),
    _Groups = subscriptions_test_utils:create_groups(GIDs, UIDs, SIDs, Node),

    % when
    {UID1, _} = hd(Users),
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [UID1]),
    Context = subscriptions_test_utils:flush_messages(
        Context1, subscriptions_test_utils:expectation(SID1, S1)),

    %% ensure sequence number won't be repeated when more entities are created
    SeqStart = UsersNum + GroupsNum + 100,

    ModifiedSpaces = lists:map(fun(N) ->
        {N + SeqStart, S1#space{name=list_to_binary("modified" ++ integer_to_list(N))}}
    end, lists:seq(1, UpdatesNum)),

    Start = erlang:system_time(milli_seconds),

    utils:pforeach(fun({Seq, Space}) ->
        rpc:cast(Node, worker_proxy, cast, [
            ?SUBSCRIPTIONS_WORKER_NAME, {
                handle_change, Seq,
                #document{key= SID1, value=Space},
                space
            }])
    end, ModifiedSpaces),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
            subscriptions_test_utils:expectation(SID1, Space) || {_Seq, Space} <- ModifiedSpaces
    ]),
    Time = erlang:system_time(milli_seconds) - Start,

    [
        #parameter{name = updates_await, value = Time, unit = "ms",
            description = "Time until every update arrived (providers mean)"}
    ].





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
    subscriptions_test_utils:flush(),
    ok.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================

