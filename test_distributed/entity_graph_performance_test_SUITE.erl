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
-include_lib("ctool/include/http/headers.hrl").
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


% Number of parallel processes making requests
-define(PARALLEL_PROCESSES, 300).

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


-define(API_TYPE(Value), ?PERF_PARAM(
    api_type, Value, "", "Type of API used during the test (REST or RPC)."
)).
-define(API_TYPE, ?config(api_type, Config)).

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
        {description, "Checks the performance of creating a lot of independent groups for different users."},
        {parameters, [?GROUP_NUM(100)]},
        ?PERF_CFG(small_rpc, [?GROUP_NUM(100), ?API_TYPE(rpc)]),
        ?PERF_CFG(small_rest, [?GROUP_NUM(100), ?API_TYPE(rest)]),
        ?PERF_CFG(medium_rpc, [?GROUP_NUM(1500), ?API_TYPE(rpc)]),
        ?PERF_CFG(medium_rest, [?GROUP_NUM(1500), ?API_TYPE(rest)]),
        ?PERF_CFG(large_rpc, [?GROUP_NUM(3000), ?API_TYPE(rpc)]),
        ?PERF_CFG(large_rest, [?GROUP_NUM(3000), ?API_TYPE(rest)])
    ]).
create_group_performance_base(Config) ->
    GroupNum = ?GROUP_NUM,
    ApiType = ?API_TYPE,
    UsersAndAuths = create_n_users(Config, GroupNum),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),


    ?begin_measurement(groups_creation_time),
    ok = parallel_foreach(fun(UserAndAuth) ->
        create_group(Config, ApiType, UserAndAuth, <<"group">>)
    end, UsersAndAuths),
    ?end_measurement(groups_creation_time),

    ?begin_measurement(reconciliation_time),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?end_measurement(reconciliation_time),

    ?derive_measurement(groups_creation_time, avg_time_per_group, fun(M) ->
        M / GroupNum
    end),
    ?derive_measurement(reconciliation_time, avg_reconciliation_time_per_group, fun(M) ->
        M / GroupNum
    end),

    [
        ?format_measurement(groups_creation_time, ms,
            "Time taken to create all groups."),
        ?format_measurement(avg_time_per_group, us,
            "Average time taken to create one group."),
        ?format_measurement(reconciliation_time, ms,
            "Time taken to reconcile the entity graph after the last group was created."),
        ?format_measurement(avg_reconciliation_time_per_group, us,
            "Average time taken to reconcile the entity graph per group created.")
    ].


create_space_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of creating a lot of independent spaces for different users."},
        {parameters, [?SPACE_NUM(100), ?API_TYPE(rpc), ?API_TYPE(rpc)]},
        ?PERF_CFG(small_rpc, [?SPACE_NUM(100), ?API_TYPE(rpc)]),
        ?PERF_CFG(small_rest, [?SPACE_NUM(100), ?API_TYPE(rest)]),
        ?PERF_CFG(medium_rpc, [?SPACE_NUM(1500), ?API_TYPE(rpc)]),
        ?PERF_CFG(medium_rest, [?SPACE_NUM(1500), ?API_TYPE(rest)]),
        ?PERF_CFG(large_rpc, [?SPACE_NUM(3000), ?API_TYPE(rpc)]),
        ?PERF_CFG(large_rest, [?SPACE_NUM(3000), ?API_TYPE(rest)])
    ]).
create_space_performance_base(Config) ->
    SpaceNum = ?SPACE_NUM,
    ApiType = ?API_TYPE,
    GroupOwners = create_n_users(Config, SpaceNum),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),


    ?begin_measurement(space_creation_time),
    ok = parallel_foreach(fun(User) ->
        create_space(Config, ApiType, ?USER(User), <<"space">>)
    end, GroupOwners),
    ?end_measurement(space_creation_time),

    ?begin_measurement(reconciliation_time),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?end_measurement(reconciliation_time),

    ?derive_measurement(space_creation_time, avg_time_per_space, fun(M) ->
        M / SpaceNum
    end),
    ?derive_measurement(reconciliation_time, avg_reconciliation_time_per_space, fun(M) ->
        M / SpaceNum
    end),

    [
        ?format_measurement(space_creation_time, ms,
            "Time taken to create the spaces."),
        ?format_measurement(avg_time_per_space, us,
            "Average time taken to create one space."),
        ?format_measurement(reconciliation_time, ms,
            "Time taken to reconcile the entity graph after the last space was created."),
        ?format_measurement(avg_reconciliation_time_per_space, us,
            "Average time taken to reconcile the entity graph per space created.")
    ].


group_chain_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of creating a long chain of groups."},
        {parameters, [?GROUP_CHAIN_LENGTH(20), ?API_TYPE(rpc)]},
        ?PERF_CFG(small_rpc, [?GROUP_CHAIN_LENGTH(20), ?API_TYPE(rpc)]),
        ?PERF_CFG(small_rest, [?GROUP_CHAIN_LENGTH(20), ?API_TYPE(rest)]),
        ?PERF_CFG(medium_rpc, [?GROUP_CHAIN_LENGTH(150), ?API_TYPE(rpc)]),
        ?PERF_CFG(medium_rest, [?GROUP_CHAIN_LENGTH(150), ?API_TYPE(rest)]),
        ?PERF_CFG(large_rpc, [?GROUP_CHAIN_LENGTH(300), ?API_TYPE(rpc)]),
        ?PERF_CFG(large_rest, [?GROUP_CHAIN_LENGTH(300), ?API_TYPE(rest)])
    ]).
group_chain_performance_base(Config) ->
    GroupChainLength = ?GROUP_CHAIN_LENGTH,
    ApiType = ?API_TYPE,
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, Token} = oz_test_utils:create_client_token(Config, User),


    ?begin_measurement(groups_creation_time),
    create_group_chain(Config, ApiType, {User, Token}, GroupChainLength),
    ?end_measurement(groups_creation_time),

    ?begin_measurement(reconciliation_time),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?end_measurement(reconciliation_time),

    ?derive_measurement(groups_creation_time, avg_time_per_group, fun(M) ->
        M / GroupChainLength
    end),
    ?derive_measurement(reconciliation_time, avg_reconciliation_time_per_group, fun(M) ->
        M / GroupChainLength
    end),

    [
        ?format_measurement(groups_creation_time, ms,
            "Time taken to create the group chain."),
        ?format_measurement(avg_time_per_group, ms,
            "Average time taken to create one group and add it to the chain (2 operations)."),
        ?format_measurement(reconciliation_time, ms,
            "Time taken to reconcile the entity graph after the last group was created."),
        ?format_measurement(avg_reconciliation_time_per_group, ms,
            "Average time taken to reconcile the entity graph per group added.")
    ].


group_chain_append_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of appending to a long chain of groups."},
        {parameters, [?STARTING_GROUP_NUM(50), ?ENDING_GROUP_NUM(60), ?API_TYPE(rpc)]},
        ?PERF_CFG(small_rpc, [?STARTING_GROUP_NUM(50), ?ENDING_GROUP_NUM(60), ?API_TYPE(rpc)]),
        ?PERF_CFG(small_rest, [?STARTING_GROUP_NUM(50), ?ENDING_GROUP_NUM(60), ?API_TYPE(rest)]),
        ?PERF_CFG(medium_rpc, [?STARTING_GROUP_NUM(100), ?ENDING_GROUP_NUM(120), ?API_TYPE(rpc)]),
        ?PERF_CFG(medium_rest, [?STARTING_GROUP_NUM(100), ?ENDING_GROUP_NUM(120), ?API_TYPE(rest)]),
        ?PERF_CFG(large_rpc, [?STARTING_GROUP_NUM(200), ?ENDING_GROUP_NUM(300), ?API_TYPE(rpc)]),
        ?PERF_CFG(large_rest, [?STARTING_GROUP_NUM(200), ?ENDING_GROUP_NUM(300), ?API_TYPE(rest)])
    ]).
group_chain_append_performance_base(Config) ->
    StartingGroupNum = ?STARTING_GROUP_NUM,
    EndingGroupNum = ?ENDING_GROUP_NUM,
    ApiType = ?API_TYPE,
    ToAppendGroupNum = EndingGroupNum - StartingGroupNum,
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, Token} = oz_test_utils:create_client_token(Config, User),

    {_BottomGroup, TopGroup} = create_group_chain(Config, ApiType, {User, Token}, StartingGroupNum),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),


    ?begin_measurement(group_appending_time),
    create_group_chain(Config, ApiType, {User, Token}, ToAppendGroupNum, TopGroup),
    ?end_measurement(group_appending_time),

    ?begin_measurement(reconciliation_time),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?end_measurement(reconciliation_time),

    ?derive_measurement(group_appending_time, avg_time_per_group, fun(M) ->
        M / ToAppendGroupNum
    end),
    ?derive_measurement(reconciliation_time, avg_reconciliation_time_per_group, fun(M) ->
        M / ToAppendGroupNum
    end),

    [
        ?format_measurement(group_appending_time, ms,
            "Time taken to append to the group chain."),
        ?format_measurement(avg_time_per_group, ms,
            "Average time taken to create one group and add it to the chain (2 operations)."),
        ?format_measurement(reconciliation_time, ms,
            "Time taken to reconcile the entity graph after the last group was appended."),
        ?format_measurement(avg_reconciliation_time_per_group, ms,
            "Average time taken to reconcile the entity graph per group added.")
    ].


big_group_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of adding users to a large group."},
        {parameters, [?STARTING_USER_NUM(80), ?ENDING_USER_NUM(100), ?API_TYPE(rpc)]},
        ?PERF_CFG(small_rpc, [?STARTING_USER_NUM(80), ?ENDING_USER_NUM(100), ?API_TYPE(rpc)]),
        ?PERF_CFG(small_rest, [?STARTING_USER_NUM(80), ?ENDING_USER_NUM(100), ?API_TYPE(rest)]),
        ?PERF_CFG(medium_rpc, [?STARTING_USER_NUM(850), ?ENDING_USER_NUM(1000), ?API_TYPE(rpc)]),
        ?PERF_CFG(medium_rest, [?STARTING_USER_NUM(850), ?ENDING_USER_NUM(1000), ?API_TYPE(rest)]),
        ?PERF_CFG(large_rpc, [?STARTING_USER_NUM(1700), ?ENDING_USER_NUM(2000), ?API_TYPE(rpc)]),
        ?PERF_CFG(large_rest, [?STARTING_USER_NUM(1700), ?ENDING_USER_NUM(2000), ?API_TYPE(rest)])
    ]).
big_group_performance_base(Config) ->
    StartingUserNum = ?STARTING_USER_NUM,
    EndingUserNum = ?ENDING_USER_NUM,
    ApiType = ?API_TYPE,
    ToAddUserNum = EndingUserNum - StartingUserNum,

    {ok, Admin} = oz_test_utils:create_user(Config),
    {ok, AdminToken} = oz_test_utils:create_client_token(Config, Admin),
    oz_test_utils:user_set_oz_privileges(Config, Admin, privileges:oz_admin(), []),

    {ok, User} = oz_test_utils:create_user(Config),
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(User), <<"group">>),

    lists:foreach(fun(_) ->
        {ok, NewUser} = oz_test_utils:create_user(Config),
        oz_test_utils:group_add_user(Config, Group, NewUser)
    end, lists:seq(2, StartingUserNum)),

    UsersToAdd = lists:map(fun(_) ->
        {ok, NewUser} = oz_test_utils:create_user(Config),
        NewUser
    end, lists:seq(1, ToAddUserNum)),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),


    ?begin_measurement(user_adding_time),
    ok = parallel_foreach(fun(UserToAdd) ->
        group_add_user(Config, ApiType, {Admin, AdminToken}, Group, UserToAdd)
    end, UsersToAdd),
    ?end_measurement(user_adding_time),

    ?begin_measurement(reconciliation_time),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?end_measurement(reconciliation_time),

    ?derive_measurement(user_adding_time, avg_time_per_user, fun(M) ->
        M / ToAddUserNum
    end),
    ?derive_measurement(reconciliation_time, avg_reconciliation_time_per_user, fun(M) ->
        M / ToAddUserNum
    end),

    [
        ?format_measurement(user_adding_time, ms,
            "Time taken to add users to the big group."),
        ?format_measurement(avg_time_per_user, us,
            "Average time taken to add one user."),
        ?format_measurement(reconciliation_time, ms,
            "Time taken to reconcile the entity graph after the last user was added."),
        ?format_measurement(avg_reconciliation_time_per_user, us,
            "Average time taken to reconcile the entity graph per user added.")
    ].


update_privileges_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of updating user privileges in a group."},
        {parameters, [?USER_NUM(100), ?API_TYPE(rpc)]},
        ?PERF_CFG(small_rpc, [?USER_NUM(100), ?API_TYPE(rpc)]),
        ?PERF_CFG(small_rest, [?USER_NUM(100), ?API_TYPE(rest)]),
        ?PERF_CFG(medium_rpc, [?USER_NUM(750), ?API_TYPE(rpc)]),
        ?PERF_CFG(medium_rest, [?USER_NUM(750), ?API_TYPE(rest)]),
        ?PERF_CFG(large_rpc, [?USER_NUM(1500), ?API_TYPE(rpc)]),
        ?PERF_CFG(large_rest, [?USER_NUM(1500), ?API_TYPE(rest)])
    ]).
update_privileges_performance_base(Config) ->
    UserNum = ?USER_NUM,
    ApiType = ?API_TYPE,
    GroupPrivileges = privileges:group_privileges(),

    {ok, Admin} = oz_test_utils:create_user(Config),
    {ok, AdminToken} = oz_test_utils:create_client_token(Config, Admin),
    oz_test_utils:user_set_oz_privileges(Config, Admin, privileges:oz_admin(), []),

    {ok, GroupCreator} = oz_test_utils:create_user(Config),
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(GroupCreator), <<"group">>),

    Users = [GroupCreator] ++ lists:map(fun(_) ->
        {ok, NewUser} = oz_test_utils:create_user(Config),
        oz_test_utils:group_add_user(Config, Group, NewUser),
        NewUser
    end, lists:seq(2, UserNum)),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),


    ?begin_measurement(privileges_updating_time),
    ok = parallel_foreach(fun(User) ->
        RandomPrivileges = lists:sublist(GroupPrivileges, rand:uniform(length(GroupPrivileges))),
        group_set_user_privileges(
            Config, ApiType, {Admin, AdminToken}, Group, User, RandomPrivileges, RandomPrivileges -- GroupPrivileges
        )
    end, Users),
    ?end_measurement(privileges_updating_time),

    ?begin_measurement(reconciliation_time),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?end_measurement(reconciliation_time),

    ?derive_measurement(privileges_updating_time, avg_time_per_user, fun(M) ->
        M / UserNum
    end),
    ?derive_measurement(reconciliation_time, avg_reconciliation_time_per_user, fun(M) ->
        M / UserNum
    end),

    [
        ?format_measurement(privileges_updating_time, ms,
            "Time taken to update privileges of all users in the group."),
        ?format_measurement(avg_time_per_user, us,
            "Average time taken to update privileges of a user in the group."),
        ?format_measurement(reconciliation_time, ms,
            "Time taken to reconcile the entity graph after the last privileges update."),
        ?format_measurement(avg_reconciliation_time_per_user, us,
            "Average time taken to reconcile the entity graph per user.")
    ].


reconcile_privileges_in_group_chain_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of updating effective user "
        "privileges towards the top group of long chain after the privileges "
        "of second to top group are updated."},
        {parameters, [?GROUP_CHAIN_LENGTH(20), ?USER_NUM(100)]},
        ?PERF_CFG(small_chain, [?GROUP_CHAIN_LENGTH(20), ?USER_NUM(500)]),
        ?PERF_CFG(medium_chain, [?GROUP_CHAIN_LENGTH(100), ?USER_NUM(500)]),
        ?PERF_CFG(large_chain, [?GROUP_CHAIN_LENGTH(200), ?USER_NUM(500)]),
        ?PERF_CFG(small_members_num, [?GROUP_CHAIN_LENGTH(100), ?USER_NUM(100)]),
        ?PERF_CFG(medium_members_num, [?GROUP_CHAIN_LENGTH(100), ?USER_NUM(500)]),
        ?PERF_CFG(large_members_num, [?GROUP_CHAIN_LENGTH(100), ?USER_NUM(1000)])
    ]).
reconcile_privileges_in_group_chain_performance_base(Config) ->
    GroupChainLength = ?GROUP_CHAIN_LENGTH,
    UserNum = ?USER_NUM,
    GroupPrivileges = privileges:group_privileges(),
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, Token} = oz_test_utils:create_client_token(Config, User),
    {BottomGroup, TopGroup} = create_group_chain(Config, rpc, {User, Token}, GroupChainLength),

    lists:foreach(fun(_) ->
        {ok, NewUser} = oz_test_utils:create_user(Config),
        oz_test_utils:group_add_user(Config, BottomGroup, NewUser)
    end, lists:seq(2, UserNum)),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, [SecondFromTopGroup]} = oz_test_utils:group_get_children(Config, TopGroup),
    RandomPrivileges = lists:sublist(GroupPrivileges, rand:uniform(length(GroupPrivileges))),


    ?begin_measurement(privileges_update_time),
    oz_test_utils:group_set_group_privileges(
        Config, TopGroup, SecondFromTopGroup, RandomPrivileges, GroupPrivileges -- RandomPrivileges
    ),
    ?end_measurement(privileges_update_time),

    ?begin_measurement(reconciliation_time),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?end_measurement(reconciliation_time),

    ?derive_measurement(reconciliation_time, avg_reconciliation_time_per_group_in_chain, fun(M) ->
        M / GroupChainLength
    end),
    ?derive_measurement(reconciliation_time, avg_reconciliation_time_per_user, fun(M) ->
        M / UserNum
    end),

    [
        ?format_measurement(privileges_update_time, ms,
            "Time taken to update privileges of the second to top group towards top group."),
        ?format_measurement(reconciliation_time, ms,
            "Time taken to reconcile the entity graph after the privileges update."),
        ?format_measurement(avg_reconciliation_time_per_group_in_chain, us,
            "Average time taken to reconcile the entity graph per group in chain."),
        ?format_measurement(avg_reconciliation_time_per_user, us,
            "Average time taken to reconcile the entity graph per user in the bottom group.")
    ].


%%%===================================================================
%%% Internal functions
%%%===================================================================

create_group_chain(Config, ApiType, Client, NumberOfGroups) ->
    {ok, BottomGroup} = create_group(Config, ApiType, Client, <<"group">>),
    create_group_chain(Config, ApiType, Client, NumberOfGroups, BottomGroup).

create_group_chain(Config, ApiType, Client, NumberOfGroups, BottomGroup) ->
    TopGroup = lists:foldl(fun(_, ChildId) ->
        {ok, ParentId} = create_group(Config, ApiType, Client, <<"group">>),
        {ok, ChildId} = group_add_group(Config, ApiType, Client, ParentId, ChildId),
        ParentId
    end, BottomGroup, lists:seq(2, NumberOfGroups)),
    {BottomGroup, TopGroup}.


create_n_users(Config, Number) ->
    lists:map(fun(_) ->
        {ok, User} = oz_test_utils:create_user(Config),
        {ok, Token} = oz_test_utils:create_client_token(Config, User),
        {User, Token}
    end, lists:seq(1, Number)).

%%%===================================================================
%%% Setup / teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils, rest_test_utils]} | Config].


end_per_suite(_Config) ->
    ssl:stop(),
    hackney:stop(),
    ok.


parallel_foreach(Fun, List) ->
    ForeachSublist = fun(Sublist) -> lists:foreach(Fun, Sublist) end,
    utils:pforeach(ForeachSublist, split_into_sublists(List)).


% Splits the list as evenly as possible, into (at most) ?PARALLEL_PROCESSES chunks
split_into_sublists(List) ->
    {_, Sublists} = lists:foldl(fun
        (_ChunksLeft, {[], OutputAcc}) ->
            {[], OutputAcc};
        (ChunksLeft, {InputAcc, OutputAcc}) ->
            ChunkSize = ceil(length(InputAcc) / ChunksLeft),
            NewInputAcc = lists:sublist(InputAcc, ChunkSize + 1, length(InputAcc) - ChunkSize),
            NewOutputAcc = [lists:sublist(InputAcc, 1, ChunkSize) | OutputAcc],
            {NewInputAcc, NewOutputAcc}
    end, {List, []}, lists:seq(?PARALLEL_PROCESSES, 1, -1)),
    Sublists.


create_group(Config, rpc, {User, _Token}, Name) ->
    oz_test_utils:create_group(Config, ?USER(User), Name);
create_group(Config, rest, {_User, Token}, Name) ->
    {ok, 201, #{<<"Location">> := Location}, _} = rest_req(
        Config, Token, post, <<"/user/groups/">>, #{<<"name">> => Name}
    ),
    {ok, lists:last(binary:split(Location, <<"/">>, [global, trim_all]))}.


group_add_user(Config, rpc, {User, _Token}, Group, NewUser) ->
    oz_test_utils:group_add_user(Config, ?USER(User), Group, NewUser);
group_add_user(Config, rest, {_User, Token}, Group, NewUser) ->
    {ok, 201, #{<<"Location">> := Location}, _} = rest_req(
        Config, Token, put, [<<"/groups/">>, Group, <<"/users/">>, NewUser]
    ),
    {ok, lists:last(binary:split(Location, <<"/">>, [global, trim_all]))}.


group_add_group(Config, rpc, {User, _Token}, Group, ChildGroup) ->
    oz_test_utils:group_add_group(Config, ?USER(User), Group, ChildGroup);
group_add_group(Config, rest, {_User, Token}, Group, ChildGroup) ->
    {ok, 201, #{<<"Location">> := Location}, _} = rest_req(
        Config, Token, put, [<<"/groups/">>, Group, <<"/children/">>, ChildGroup]
    ),
    {ok, lists:last(binary:split(Location, <<"/">>, [global, trim_all]))}.


group_set_user_privileges(Config, rpc, {User, _Token}, Group, User, PrivsToGrant, PrivsToRevoke) ->
    oz_test_utils:group_set_user_privileges(Config, ?USER(User), Group, User, PrivsToGrant, PrivsToRevoke);
group_set_user_privileges(Config, rest, {_User, Token}, Group, User, PrivsToGrant, PrivsToRevoke) ->
    {ok, 204, _, _} = rest_req(
        Config, Token, put, [<<"/groups/">>, Group, <<"/users/">>, User, <<"/privileges">>], #{
            <<"grant">> => [atom_to_binary(P, utf8) || P <- PrivsToGrant],
            <<"revoke">> => [atom_to_binary(P, utf8) || P <- PrivsToRevoke]
        }
    ),
    ok.


create_space(Config, rpc, {User, _Token}, Name) ->
    oz_test_utils:create_space(Config, ?USER(User), Name);
create_space(Config, rest, {_User, Token}, Name) ->
    {ok, 201, #{<<"Location">> := Location}, _} = rest_req(
        Config, Token, post, <<"/user/spaces/">>, #{<<"name">> => Name}
    ),
    {ok, lists:last(binary:split(Location, <<"/">>, [global, trim_all]))}.


rest_req(Config, Token, Method, Path) ->
    rest_req(Config, Token, Method, Path, #{}).

rest_req(Config, Token, Method, Path, Body) ->
    Opts = [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}],
    URL = oz_test_utils:oz_rest_url(Config, Path),
    Headers = #{
        ?HDR_CONTENT_TYPE => <<"application/json">>,
        ?HDR_X_AUTH_TOKEN => Token
    },
    http_client:request(Method, URL, Headers, json_utils:encode(Body), Opts).
