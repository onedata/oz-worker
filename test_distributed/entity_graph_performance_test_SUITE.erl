%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is used for performance tests of Entity Graph in Onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(entity_graph_performance_test_SUITE).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("cluster_worker/test_distributed/performance_test_utils.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).

-export([
    create_group_performance/1, create_group_performance_base/1,
    create_space_performance/1, create_space_performance_base/1,
    group_chain_performance/1, group_chain_performance_base/1,
    group_chain_append_performance/1, group_chain_append_performance_base/1,
    big_group_performance/1, big_group_performance_base/1,
    update_privileges_performance/1, update_privileges_performance_base/1,
    reconcile_privileges_in_group_chain_performance/1, reconcile_privileges_in_group_chain_performance_base/1
]).

-define(CT_TEST_CASES, []).

-define(PERFORMANCE_TEST_CASES, [
    create_group_performance,
    create_space_performance,
    group_chain_performance,
    group_chain_append_performance,
    big_group_performance,
    update_privileges_performance,
    reconcile_privileges_in_group_chain_performance
]).


% Performance tests parameters
-define(USER_NUM(Value), ?PERF_PARAM(
    user_num, Value, "", "Number of users."
)).
-define(USER_NUM, ?config(user_num, Config)).

-define(GROUP_NUM(Value), ?PERF_PARAM(
    group_num, Value, "", "Number of groups."
)).
-define(GROUP_NUM, ?config(group_num, Config)).

-define(GROUP_CHAIN_LENGTH(Value), ?PERF_PARAM(
    group_chain_length, Value, "", "Length of the group chain."
)).
-define(GROUP_CHAIN_LENGTH, ?config(group_chain_length, Config)).

-define(SPACE_NUM(Value), ?PERF_PARAM(
    space_num, Value, "", "Number of spaces."
)).
-define(SPACE_NUM, ?config(space_num, Config)).

-define(STARTING_GROUP_NUM(Value), ?PERF_PARAM(
    starting_group_num, Value, "", "Number of groups at the beginning of the test."
)).
-define(STARTING_GROUP_NUM, ?config(starting_group_num, Config)).


-define(ENDING_GROUP_NUM(Value), ?PERF_PARAM(
    ending_group_num, Value, "", "Number of groups at the end of the test."
)).
-define(ENDING_GROUP_NUM, ?config(ending_group_num, Config)).


-define(STARTING_USER_NUM(Value), ?PERF_PARAM(
    starting_user_num, Value, "", "Number of users at the beginning of the test."
)).
-define(STARTING_USER_NUM, ?config(starting_user_num, Config)).


-define(ENDING_USER_NUM(Value), ?PERF_PARAM(
    ending_user_num, Value, "", "Number of users at the end of the test."
)).
-define(ENDING_USER_NUM, ?config(ending_user_num, Config)).

%%%===================================================================
%%% API functions
%%%===================================================================

all() ->
    ?ALL(?CT_TEST_CASES, ?PERFORMANCE_TEST_CASES).

%%%===================================================================
%%% Performance tests
%%%===================================================================

create_group_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of creating a lot of independent groups."},
        {parameters, [?GROUP_NUM(100)]},
        ?PERF_CFG(small, [?GROUP_NUM(100)]),
        ?PERF_CFG(medium, [?GROUP_NUM(750)]),
        ?PERF_CFG(large, [?GROUP_NUM(1500)])
    ]).
create_group_performance_base(Config) ->
    GroupNum = ?GROUP_NUM,
    GroupOwners = create_n_users(Config, GroupNum),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    %% ----------------------
    %% Start time measurement
    %% ----------------------
    StartTimestamp = os:timestamp(),

    lists:foreach(fun(User) ->
        oz_test_utils:create_group(Config, ?USER(User), <<"group">>)
    end, GroupOwners),

    TimestampAfterCreation = os:timestamp(),
    CreationTime = timer:now_diff(TimestampAfterCreation, StartTimestamp) / 1000,

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ReconciliationTime = timer:now_diff(os:timestamp(), TimestampAfterCreation) / 1000,
    %% ----------------------
    %% End time measurement
    %% ----------------------
    AvgPerGroup = CreationTime / GroupNum,
    AvgReconciliationTime = ReconciliationTime / GroupNum,

    [
        #parameter{name = group_creation_time, value = CreationTime, unit = "ms",
            description = "Time taken to create the groups."},
        #parameter{name = avg_time_per_group, value = AvgPerGroup, unit = "ms",
            description = "Average time taken to create one group."},
        #parameter{name = entity_graph_reconciliation_time, value = ReconciliationTime, unit = "ms",
            description = "Time taken to reconcile the entity graph after the last group was created."},
        #parameter{name = avg_reconciliation_time_per_group, value = AvgReconciliationTime, unit = "ms",
            description = "Average time taken to reconcile the entity graph per group created."}
    ].


create_space_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of creating a lot of independent spaces."},
        {parameters, [?SPACE_NUM(100)]},
        ?PERF_CFG(small, [?SPACE_NUM(100)]),
        ?PERF_CFG(medium, [?SPACE_NUM(750)]),
        ?PERF_CFG(large, [?SPACE_NUM(1500)])
    ]).
create_space_performance_base(Config) ->
    SpaceNum = ?SPACE_NUM,
    GroupOwners = create_n_users(Config, SpaceNum),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    %% ----------------------
    %% Start time measurement
    %% ----------------------
    StartTimestamp = os:timestamp(),

    lists:foreach(fun(User) ->
        oz_test_utils:create_space(Config, ?USER(User), <<"space">>)
    end, GroupOwners),

    TimestampAfterCreation = os:timestamp(),
    CreationTime = timer:now_diff(TimestampAfterCreation, StartTimestamp) / 1000,

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ReconciliationTime = timer:now_diff(os:timestamp(), TimestampAfterCreation) / 1000,
    %% ----------------------
    %% End time measurement
    %% ----------------------
    AvgPerSpace = CreationTime / SpaceNum,
    AvgReconciliationTime = ReconciliationTime / SpaceNum,

    [
        #parameter{name = space_creation_time, value = CreationTime, unit = "ms",
            description = "Time taken to create the spaces."},
        #parameter{name = avg_time_per_space, value = AvgPerSpace, unit = "ms",
            description = "Average time taken to create one space."},
        #parameter{name = entity_graph_reconciliation_time, value = ReconciliationTime, unit = "ms",
            description = "Time taken to reconcile the entity graph after the last space was created."},
        #parameter{name = avg_reconciliation_time_per_space, value = AvgReconciliationTime, unit = "ms",
            description = "Average time taken to reconcile the entity graph per space created."}
    ].


group_chain_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of creating a long chain of groups."},
        {parameters, [?GROUP_CHAIN_LENGTH(20)]},
        ?PERF_CFG(small, [?GROUP_CHAIN_LENGTH(20)]),
        ?PERF_CFG(medium, [?GROUP_CHAIN_LENGTH(100)]),
        ?PERF_CFG(large, [?GROUP_CHAIN_LENGTH(200)])
    ]).
group_chain_performance_base(Config) ->
    GroupChainLength = ?GROUP_CHAIN_LENGTH,
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    %% ----------------------
    %% Start time measurement
    %% ----------------------
    StartTimestamp = os:timestamp(),

    create_group_chain(Config, ?USER(User), GroupChainLength),

    TimestampAfterCreation = os:timestamp(),
    CreationTime = timer:now_diff(TimestampAfterCreation, StartTimestamp) / 1000,

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ReconciliationTime = timer:now_diff(os:timestamp(), TimestampAfterCreation) / 1000,
    %% ----------------------
    %% End time measurement
    %% ----------------------
    AvgPerGroup = CreationTime / GroupChainLength,
    AvgReconciliationTime = ReconciliationTime / GroupChainLength,

    [
        #parameter{name = group_creation_time, value = CreationTime, unit = "ms",
            description = "Time taken to create the group chain."},
        #parameter{name = avg_time_per_group, value = AvgPerGroup, unit = "ms",
            description = "Average time taken to create one group."},
        #parameter{name = entity_graph_reconciliation_time, value = ReconciliationTime, unit = "ms",
            description = "Time taken to reconcile the entity graph after the last group was created."},
        #parameter{name = avg_reconciliation_time_per_group, value = AvgReconciliationTime, unit = "ms",
            description = "Average time taken to reconcile the entity graph per group added."}
    ].


group_chain_append_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of appending to a long chain of groups."},
        {parameters, [?STARTING_GROUP_NUM(50), ?ENDING_GROUP_NUM(60)]},
        ?PERF_CFG(small, [?STARTING_GROUP_NUM(50), ?ENDING_GROUP_NUM(60)]),
        ?PERF_CFG(medium, [?STARTING_GROUP_NUM(100), ?ENDING_GROUP_NUM(120)]),
        ?PERF_CFG(large, [?STARTING_GROUP_NUM(200), ?ENDING_GROUP_NUM(300)])
    ]).
group_chain_append_performance_base(Config) ->
    StartingGroupNum = ?STARTING_GROUP_NUM,
    EndingGroupNum = ?ENDING_GROUP_NUM,
    ToAppendGroupNum = EndingGroupNum - StartingGroupNum,
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    {_BottomGroup, TopGroup} = create_group_chain(Config, ?USER(User), StartingGroupNum),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    %% ----------------------
    %% Start time measurement
    %% ----------------------
    StartTimestamp = os:timestamp(),

    create_group_chain(
        Config, ?USER(User), ToAppendGroupNum, TopGroup
    ),

    TimestampAfterCreation = os:timestamp(),
    CreationTime = timer:now_diff(TimestampAfterCreation, StartTimestamp) / 1000,

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ReconciliationTime = timer:now_diff(os:timestamp(), TimestampAfterCreation) / 1000,
    %% ----------------------
    %% End time measurement
    %% ----------------------
    AvgPerGroup = CreationTime / ToAppendGroupNum,
    AvgReconciliationTime = ReconciliationTime / ToAppendGroupNum,

    [
        #parameter{name = group_appending_time, value = CreationTime, unit = "ms",
            description = "Time taken to append to the group chain."},
        #parameter{name = avg_time_per_group, value = AvgPerGroup, unit = "ms",
            description = "Average time taken to append one group."},
        #parameter{name = entity_graph_reconciliation_time, value = ReconciliationTime, unit = "ms",
            description = "Time taken to reconcile the entity graph after the last group was appended."},
        #parameter{name = avg_reconciliation_time_per_group, value = AvgReconciliationTime, unit = "ms",
            description = "Average time taken to reconcile the entity graph per group added."}
    ].


big_group_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of adding users to a large group."},
        {parameters, [?STARTING_USER_NUM(80), ?ENDING_USER_NUM(100)]},
        ?PERF_CFG(small, [?STARTING_USER_NUM(80), ?ENDING_USER_NUM(100)]),
        ?PERF_CFG(medium, [?STARTING_USER_NUM(650), ?ENDING_USER_NUM(750)]),
        ?PERF_CFG(large, [?STARTING_USER_NUM(1300), ?ENDING_USER_NUM(1500)])
    ]).
big_group_performance_base(Config) ->
    StartingUserNum = ?STARTING_USER_NUM,
    EndingUserNum = ?ENDING_USER_NUM,
    ToAddUserNum = EndingUserNum - StartingUserNum,
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(User), <<"group">>),

    lists:foreach(fun(_) ->
        {ok, NewUser} = oz_test_utils:create_user(Config, #od_user{}),
        oz_test_utils:group_add_user(Config, Group, NewUser)
    end, lists:seq(2, StartingUserNum)),

    UsersToAdd = lists:map(fun(_) ->
        {ok, NewUser} = oz_test_utils:create_user(Config, #od_user{}),
        NewUser
    end, lists:seq(1, ToAddUserNum)),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    %% ----------------------
    %% Start time measurement
    %% ----------------------
    StartTimestamp = os:timestamp(),

    lists:foreach(fun(NewUser) ->
        oz_test_utils:group_add_user(Config, Group, NewUser)
    end, UsersToAdd),

    TimestampAfterAdding = os:timestamp(),
    CreationTime = timer:now_diff(TimestampAfterAdding, StartTimestamp) / 1000,

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ReconciliationTime = timer:now_diff(os:timestamp(), TimestampAfterAdding) / 1000,
    %% ----------------------
    %% End time measurement
    %% ----------------------
    AvgPerUser = CreationTime / ToAddUserNum,
    AvgReconciliationTime = ReconciliationTime / ToAddUserNum,

    [
        #parameter{name = user_adding_time, value = CreationTime, unit = "ms",
            description = "Time taken to add users to the big group."},
        #parameter{name = avg_time_per_user, value = AvgPerUser, unit = "ms",
            description = "Average time taken to add one user."},
        #parameter{name = entity_graph_reconciliation_time, value = ReconciliationTime, unit = "ms",
            description = "Time taken to reconcile the entity graph after the last user was added."},
        #parameter{name = avg_reconciliation_time_per_user, value = AvgReconciliationTime, unit = "ms",
            description = "Average time taken to reconcile the entity graph per user added."}
    ].


update_privileges_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of updating user privileges in a group."},
        {parameters, [?USER_NUM(100)]},
        ?PERF_CFG(small, [?USER_NUM(100)]),
        ?PERF_CFG(medium, [?USER_NUM(500)]),
        ?PERF_CFG(large, [?USER_NUM(1000)])
    ]).
update_privileges_performance_base(Config) ->
    UserNum = ?USER_NUM,
    GroupPrivileges = oz_test_utils:all_group_privileges(Config),

    {ok, GroupCreator} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(GroupCreator), <<"group">>),

    Users = [GroupCreator] ++ lists:map(fun(_) ->
        {ok, NewUser} = oz_test_utils:create_user(Config, #od_user{}),
        oz_test_utils:group_add_user(Config, Group, NewUser),
        NewUser
    end, lists:seq(2, UserNum)),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),


    %% ----------------------
    %% Start time measurement
    %% ----------------------
    StartTimestamp = os:timestamp(),

    lists:foreach(fun(User) ->
        RandomPrivileges = lists:sublist(GroupPrivileges, rand:uniform(length(GroupPrivileges))),
        oz_test_utils:group_set_user_privileges(Config, Group, User, set, RandomPrivileges)
    end, Users),

    TimestampAfterUpdating = os:timestamp(),
    UpdateTime = timer:now_diff(TimestampAfterUpdating, StartTimestamp) / 1000,

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ReconciliationTime = timer:now_diff(os:timestamp(), TimestampAfterUpdating) / 1000,
    %% ----------------------
    %% End time measurement
    %% ----------------------
    AvgPerUser = UpdateTime / UserNum,
    AvgReconciliationTime = ReconciliationTime / UserNum,

    [
        #parameter{name = privileges_updating_time, value = UpdateTime, unit = "ms",
            description = "Time taken to update privileges of all users in the group."},
        #parameter{name = avg_time_per_user, value = AvgPerUser, unit = "ms",
            description = "Average time taken to update privileges of a user in the group."},
        #parameter{name = entity_graph_reconciliation_time, value = ReconciliationTime, unit = "ms",
            description = "Time taken to reconcile the entity graph after the last privileges update."},
        #parameter{name = avg_reconciliation_time_per_user, value = AvgReconciliationTime, unit = "ms",
            description = "Average time taken to reconcile the entity graph per user."}
    ].


reconcile_privileges_in_group_chain_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of updating effective user "
        "privileges towards the top group of long chain after the privileges "
        "of second to top group are updated."},
        {parameters, [?GROUP_CHAIN_LENGTH(30), ?USER_NUM(30)]},
        ?PERF_CFG(small_chain, [?GROUP_CHAIN_LENGTH(100), ?USER_NUM(250)]),
        ?PERF_CFG(medium_chain, [?GROUP_CHAIN_LENGTH(250), ?USER_NUM(250)]),
        ?PERF_CFG(large_chain, [?GROUP_CHAIN_LENGTH(500), ?USER_NUM(250)]),
        ?PERF_CFG(small_members_num, [?GROUP_CHAIN_LENGTH(250), ?USER_NUM(100)]),
        ?PERF_CFG(medium_members_num, [?GROUP_CHAIN_LENGTH(250), ?USER_NUM(250)]),
        ?PERF_CFG(large_members_num, [?GROUP_CHAIN_LENGTH(60), ?USER_NUM(500)])
    ]).
reconcile_privileges_in_group_chain_performance_base(Config) ->
    GroupChainLength = ?GROUP_CHAIN_LENGTH,
    UserNum = ?USER_NUM,
    GroupPrivileges = oz_test_utils:all_group_privileges(Config),

    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {BottomGroup, TopGroup} = create_group_chain(Config, ?USER(User), GroupChainLength),

    lists:foreach(fun(_) ->
        {ok, NewUser} = oz_test_utils:create_user(Config, #od_user{}),
        oz_test_utils:group_add_user(Config, BottomGroup, NewUser)
    end, lists:seq(2, UserNum)),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, [SecondFromTopGroup]} = oz_test_utils:group_get_children(Config, TopGroup),

    %% ----------------------
    %% Start time measurement
    %% ----------------------
    StartTimestamp = os:timestamp(),

    RandomPrivileges = lists:sublist(GroupPrivileges, rand:uniform(length(GroupPrivileges))),
    oz_test_utils:group_set_group_privileges(Config, TopGroup, SecondFromTopGroup, set, RandomPrivileges),

    TimestampAfterUpdate = os:timestamp(),
    UpdateTime = timer:now_diff(TimestampAfterUpdate, StartTimestamp) / 1000,

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ReconciliationTime = timer:now_diff(os:timestamp(), TimestampAfterUpdate) / 1000,
    %% ----------------------
    %% End time measurement
    %% ----------------------
    AvgReconciliationTimePerGroupInChain = ReconciliationTime / GroupChainLength,
    AvgReconciliationTimePerUser = ReconciliationTime / UserNum,

    [
        #parameter{name = privileges_update_time, value = UpdateTime, unit = "ms",
            description = "Time taken to update privileges of the second to top group towards top group."},
        #parameter{name = reconciliation_time, value = ReconciliationTime, unit = "ms",
            description = "Time taken to reconcile the entity graph after the privileges update."},
        #parameter{name = avg_reconciliation_time_per_group_in_chain, value = AvgReconciliationTimePerGroupInChain, unit = "ms",
            description = "Average time taken to reconcile the entity graph per group in chain."},
        #parameter{name = avg_reconciliation_time_per_user, value = AvgReconciliationTimePerUser, unit = "ms",
            description = "Average time taken to reconcile the entity graph per user in the bottom group."}
    ].


%%%===================================================================
%%% Internal functions
%%%===================================================================

create_group_chain(Config, Client, NumberOfGroups) ->
    {ok, BottomGroup} = oz_test_utils:create_group(Config, Client, <<"group">>),
    create_group_chain(Config, Client, NumberOfGroups, BottomGroup).

create_group_chain(Config, Client, NumberOfGroups, BottomGroup) ->
    TopGroup = lists:foldl(fun(_, PreviousGroupId) ->
        {ok, ParentId} = oz_test_utils:create_group(Config, Client, <<"group">>),
        {ok, PreviousGroupId} = oz_test_utils:group_add_group(
            Config, Client, ParentId, PreviousGroupId
        ),
        io:format(user, "x", []),
        ParentId
    end, BottomGroup, lists:seq(2, NumberOfGroups)),
    {BottomGroup, TopGroup}.


create_n_users(Config, Number) ->
    lists:map(fun(_) ->
        {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
        User
    end, lists:seq(1, Number)).

%%%===================================================================
%%% Setup / teardown functions
%%%===================================================================


init_per_suite(Config) ->
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    ok.