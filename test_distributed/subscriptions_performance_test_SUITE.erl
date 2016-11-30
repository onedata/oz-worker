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

-define(NUMBER_OF_DOCS, 10).

-define(MODELS_TO_CLEAN, [od_provider, od_space, od_group, od_user]).

%%%===================================================================
%%% Test functions
%%%===================================================================
generate_spaces_test(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 10},
        {success_rate, 95},
        {parameters, [
            ?PROVIDERS_NUM(1),
            ?DOCS_NUM(10)
        ]},
        {description, "Generates spaces documents, saves them and gathers subscription updates for many providers"},
        ?GENERATE_TEST_CFG(1, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(2, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(3, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(4, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(5, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(6, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(7, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(8, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(9, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(10, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(15, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(20, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(25, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(50, ?NUMBER_OF_DOCS),
        ?GENERATE_TEST_CFG(100, ?NUMBER_OF_DOCS)
    ]).

generate_spaces_test_base(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),

    ProvidersNum = ?config(providers_num, Config),
    DocsCount = ?config(docs_num, Config),

    maybe_clear(Config),

    Results = utils:pmap(fun(ID) ->
        %% given
        PNameList = "provider_" ++ integer_to_list(ID),
        PName = list_to_binary(PNameList),
        SIDs = lists:map(fun(ID1) ->
            list_to_binary("space_" ++ integer_to_list(ID1) ++ "@" ++ PNameList)
        end, lists:seq(1, DocsCount)),

        %% when
        PID = subscriptions_test_utils:create_provider(Node, PName, SIDs),
        Space = #od_space{name = <<"name">>, providers = [{PID, 0}]},
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
    end, lists:seq(1, ProvidersNum)),

    lists:map(fun(Res) ->
        ?assertMatch({ok, _}, Res)
    end, Results),

    UpdatesMeanTime = lists:sum(lists:map(fun({ok, Time}) -> Time end, Results)) / length(Results),

    [
        #parameter{name = updates_await, value = UpdatesMeanTime, unit = "ms",
            description = "Time until every update arrived (providers mean)"}
    ].


space_update_test(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 10},
        {success_rate, 95},
        {description, "Performs document updates and gathers subscription updates for provider"},
        ?UPDATE_TEST_CFG(10, 2, 1),
        ?UPDATE_TEST_CFG(10, 10, 5),
        ?UPDATE_TEST_CFG(10, 20, 10),
        ?UPDATE_TEST_CFG(10, 50, 25),
        ?UPDATE_TEST_CFG(100, 2, 1),
        ?UPDATE_TEST_CFG(100, 10, 5),
        ?UPDATE_TEST_CFG(100, 20, 10),
        ?UPDATE_TEST_CFG(100, 50, 25)
    ]).

space_update_test_base(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),

    maybe_clear(Config),

    UsersNum = ?config(users_num, Config),
    GroupsNum = ?config(groups_num, Config),
    UpdatesNum = ?config(docs_modifications_num, Config),

    PID = subscriptions_test_utils:create_provider(Node, ?ID(p1), []),
    GIDs = subscriptions_test_utils:generate_group_ids(GroupsNum),
    UIDs = subscriptions_test_utils:generate_user_ids(UsersNum),
    SIDs = subscriptions_test_utils:generate_space_ids(1),

    Context1 = subscriptions_test_utils:init_messages(Node, PID, [hd(UIDs)]),
    Start = subscriptions_test_utils:get_last_sequence_number(Node),
    NewContext = Context1#subs_ctx{resume_at=binary_to_integer(Start)},

    [{SID1, S1} | _] = subscriptions_test_utils:create_spaces(SIDs, UIDs, GIDs, Node),
    subscriptions_test_utils:create_users(UIDs, GIDs, Node),
    subscriptions_test_utils:create_groups(GIDs, UIDs, SIDs, Node),

    % when
    Context = subscriptions_test_utils:flush_messages(
        NewContext, subscriptions_test_utils:expectation(SID1, S1)),

    Start1 = Context#subs_ctx.resume_at,

    %% ensure sequence number won't be repeated when more entities are created
    SeqStart = Start1 + 100,

    ModifiedSpaces = lists:map(fun(N) ->
        {N + SeqStart, S1#od_space{name=list_to_binary("modified" ++ integer_to_list(N))}}
    end, lists:seq(1, UpdatesNum)),

    StartTime = erlang:system_time(milli_seconds),

    utils:pforeach(fun({Seq, Space}) ->
        rpc:cast(Node, worker_proxy, cast, [
            ?SUBSCRIPTIONS_WORKER_NAME, {
                handle_change, Seq,
                #document{key= SID1, value=Space},
                od_space
            }])
    end, ModifiedSpaces),

    % then
    subscriptions_test_utils:verify_messages_present(Context, [
            subscriptions_test_utils:expectation(SID1, Space) || {_Seq, Space} <- ModifiedSpaces
    ]),

    Time = erlang:system_time(milli_seconds) - StartTime,
    subscriptions_test_utils:empty_cache(Node),

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
    throw(not_implemented),
%%    Nodes = ?config(oz_worker_nodes, Config),
%%    test_utils:mock_new(Nodes, group_graph),
%%    test_utils:mock_expect(Nodes, group_graph, refresh_effective_caches, fun() ->
%%        ok end),
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



maybe_clear(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    case performance:should_clear(Config) of
        true -> clear_db_all(Nodes);
        _ -> ok
    end.

clear_db_all(Nodes) ->
    lists:foreach(fun(Node) -> clear_db(Node) end, Nodes).

clear_db(Node) ->
    Docs = lists:flatmap(fun(Model) ->
        subscriptions_test_utils:list(Node, Model)
    end, ?MODELS_TO_CLEAN),

    subscriptions_test_utils:delete_all(Node, Docs).
