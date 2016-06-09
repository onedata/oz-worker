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
-export([generate_spaces_test/1, modify_space_record_test/1, space_update_test/1]).
-export([generate_spaces_test_base/1, modify_space_record_test_base/1, space_update_test_base/1, id/1]).

all() -> ?ALL([space_update_test], [
%%    generate_spaces_test
%%        ,
%%    modify_space_record_test
    space_update_test]).


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

modify_space_record_test(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 1},
        {success_rate, 95},
        {description, "Performs document saves and gathers subscription updated for many providers"},
        ?MOD_CFG(one_provider, "One provider modifying document 10 times", 1, 10)

    ]).

modify_space_record_test_base(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),

    ProvidersCount = ?config(providers_count, Config),
    DocModificationsCount = ?config(docs_modifications_count, Config),

    {ok, UserId} = ?assertMatch({ok, _},
        oz_test_utils:create_user(Config, #onedata_user{})),

    InitialSpaceName = <<"space_name">>,
    {ok, SpaceId} = ?assertMatch({ok, _},
        oz_test_utils:create_space(Config, {user, UserId}, InitialSpaceName)),

    Results = utils:pmap(fun(ID) ->
        %% given
        PNameList = "provider_" ++ integer_to_list(ID),
        PName = list_to_binary(PNameList),

        SpaceNames = lists:map(fun(ID1) ->
            list_to_binary("space_" ++ integer_to_list(ID1) ++ "@" ++ PNameList)
        end, lists:seq(1, DocModificationsCount)),

        %% when
        PID = subscriptions_test_utils:create_provider(Node, PName, [SpaceId]),
        Context = subscriptions_test_utils:init_messages(Node, PID, []),

        Spaces = lists:map(fun(SpaceName) ->
            Space = #space{name = SpaceName, providers_supports = [{PID, 0}]},
            subscriptions_test_utils:save(Node, SpaceId, Space),
            Space
        end, SpaceNames),

        %% then
        Start = erlang:system_time(milli_seconds),
        subscriptions_test_utils:verify_messages_present(Context,
            lists:map(fun(Space) ->
                subscriptions_test_utils:expectation(SpaceId, Space)
            end, Spaces)
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

space_update_test(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 1},
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

    %TODO cleanup and generate groups and users

    PID = subscriptions_test_utils:create_provider(Node, id("p1"), []),
    S1 = #space{
        name = <<"initial">>,
        groups = [
            {id(g1), []},
            {id(g2), []},
            {id(g3), []}
        ]
    },

    Users = lists:map(fun(N) ->
        Name = "u" ++ integer_to_list(N),
        User = #onedata_user{
            name=list_to_binary(Name),
            groups = [id(g1), id(g2), id(g3)]},
        UserId = id(Name),
        subscriptions_test_utils:save(Node, UserId, User),
        {UserId, User}
    end, lists:seq(1, UsersNum)),

    G1 = #user_group{
        name = <<"g1">>,
        users = [{UserId, []} || {UserId, _} <- Users],
        spaces = [id(s1)]
    },
    G2 = #user_group{
        name = <<"g2">>,
        users = [{UserId, []} || {UserId, _} <- Users],
        spaces = [id(s1)]
    },
    G3 = #user_group{
        name = <<"g3">>,
        users = [{UserId, []} || {UserId, _} <- Users],
        spaces = [id(s1)]
    },

%%    TODO generate full groups
    EmptyGroups = lists:map(fun(N) ->
        Name = "g" ++ integer_to_list(N),
        Group = #user_group{
            name = list_to_binary("g" ++ integer_to_list(N)),
            users = [],
            spaces = []
        },
        GID = id(Name),
        {GID, Group}
    end, lists:seq(4,GroupsNum)),

    Groups = [{id(g1), G1}, {id(g2), G2}, {id(g3), G3}] ++ EmptyGroups,
    subscriptions_test_utils:save(Node, id(s1), S1),

    lists:foreach(fun({GID, Group}) ->
        subscriptions_test_utils:save(Node, GID, Group)
    end, Groups),
    % when
    Context1 = subscriptions_test_utils:init_messages(Node, PID, [id(u1)]),

    Context = subscriptions_test_utils:flush_messages(Context1,
        subscriptions_test_utils:expectation(id(s1), S1)),

    ModifiedSpaces = lists:map(fun(N) ->
        {N + 100, S1#space{name=list_to_binary("modified" ++ integer_to_list(N))}}
    end, lists:seq(1, UpdatesNum)),


    Start = erlang:system_time(milli_seconds),

    utils:pforeach(fun({Seq, Space}) ->
        rpc:cast(Node, worker_proxy, cast, [
            ?SUBSCRIPTIONS_WORKER_NAME, {
                handle_change, Seq,
                #document{key= id(s1), value=Space},
                space
            }])
    end, ModifiedSpaces),

    % then
%%    lists:foreach(fun({_Seq, Space}) ->
    subscriptions_test_utils:verify_messages_present(Context, [
            subscriptions_test_utils:expectation(id(s1), Space) || {_Seq, Space} <- ModifiedSpaces
    ]),
%%    end, ModifiedSpaces),
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

id(Id) when is_atom(Id) ->
    ?ID(Id);
id(Id) ->
    ?ID(list_to_atom(Id)).