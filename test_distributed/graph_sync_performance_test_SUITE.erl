%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is used for performance tests of Graph Sync in Onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(graph_sync_performance_test_SUITE).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("gui/include/gui_session.hrl").
-include_lib("cluster_worker/include/global_definitions.hrl").
-include_lib("cluster_worker/test_distributed/performance_test_utils.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).

-export([
    update_propagation_performance/1, update_propagation_performance_base/1,
    privileges_in_a_big_space_performance/1, privileges_in_a_big_space_performance_base/1,
    concurrent_active_clients_spawning_performance/1, concurrent_active_clients_spawning_performance_base/1
]).

-define(PERFORMANCE_TEST_CASES, [
    update_propagation_performance,
    privileges_in_a_big_space_performance,
    concurrent_active_clients_spawning_performance
]).
-define(CT_TEST_CASES, []).


-define(NO_OP_FUN, fun(_) -> ok end).


% Performance tests parameters
-define(USER_NUM(Value), ?PERF_PARAM(
    user_num, Value, "", "Number of users."
)).
-define(USER_NUM, ?config(user_num, Config)).

-define(GROUP_NUM(Value), ?PERF_PARAM(
    group_num, Value, "", "Number of groups."
)).
-define(GROUP_NUM, ?config(group_num, Config)).

-define(PROVIDER_NUM(Value), ?PERF_PARAM(
    provider_num, Value, "", "Number of providers."
)).
-define(PROVIDER_NUM, ?config(provider_num, Config)).

-define(UPDATE_NUM(Value), ?PERF_PARAM(
    update_num, Value, "", "Number of updates of group names."
)).
-define(UPDATE_NUM, ?config(update_num, Config)).

-define(REQUEST_INTERVAL_SECONDS(Value), ?PERF_PARAM(
    request_interval, Value, "", "How often each client performs a GS request."
)).
-define(REQUEST_INTERVAL_SECONDS, ?config(request_interval, Config)).

%%%===================================================================
%%% API functions
%%%===================================================================

all() ->
    ?ALL(?CT_TEST_CASES, ?PERFORMANCE_TEST_CASES).

%%%===================================================================
%%% Performance tests
%%%===================================================================

update_propagation_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of update propagation via Graph Sync, "
        "where a number of users all belong to a number of groups, which names are modified."},
        {parameters, [?USER_NUM(10), ?GROUP_NUM(10), ?UPDATE_NUM(5)]},
        ?PERF_CFG(small, [?USER_NUM(10), ?GROUP_NUM(10), ?UPDATE_NUM(10)]),
        ?PERF_CFG(medium, [?USER_NUM(30), ?GROUP_NUM(30), ?UPDATE_NUM(10)]),
        ?PERF_CFG(large, [?USER_NUM(50), ?GROUP_NUM(50), ?UPDATE_NUM(10)])
    ]).
update_propagation_performance_base(Config) ->
    UserNum = ?USER_NUM,
    GroupNum = ?GROUP_NUM,
    UpdateNum = ?UPDATE_NUM,
    Users = create_n_users(Config, UserNum),
    Groups = lists:map(fun(_) ->
        [Creator | Rest] = Users,
        {ok, Group} = oz_test_utils:create_group(Config, ?USER(Creator), <<"group">>),
        [oz_test_utils:group_add_user(Config, Group, U) || U <- Rest],
        Group
    end, lists:seq(1, GroupNum)),

    FinalGroupName = <<"finalGroupName">>,

    Master = self(),

    GathererLoop = fun Loop({GroupsLeftPerClient, FinishedClients} = _State) ->
        {NewGroupsLeftPerClient, NewFinishedClients} = receive
            {finish, ClientRef, GroupId} ->
                GroupsLeft = maps:get(ClientRef, GroupsLeftPerClient, []),
                NewGroupsLeft = lists:delete(GroupId, GroupsLeft),
                GroupsLeftPerClient2 = maps:put(ClientRef, NewGroupsLeft, GroupsLeftPerClient),
                FinishedClients2 = case NewGroupsLeft of
                    [] -> [ClientRef | FinishedClients];
                    _ -> FinishedClients
                end,
                {GroupsLeftPerClient2, FinishedClients2}
        end,
        case length(NewFinishedClients) of
            UserNum -> Master ! finished;
            _ -> Loop({NewGroupsLeftPerClient, NewFinishedClients})
        end
    end,

    GathererPid = spawn(fun() ->
        StartingGroupsLeftPerClient = maps:from_list(
            [{User, Groups} || User <- Users]
        ),
        GathererLoop({StartingGroupsLeftPerClient, []})
    end),

    GatherUpdate = fun(Push) ->
        #gs_push_graph{gri = #gri{
            type = od_group, id = GroupId, aspect = instance
        }, change_type = updated, data = #{<<"name">> := NewName}} = Push,
        case NewName of
            FinalGroupName ->
                GathererPid ! {finish, self(), GroupId};
            _ ->
                ok
        end
    end,

    OnSuccessFun = fun(Client) ->
        lists:foreach(fun(Group) ->
            ?assertMatch(
                {ok, #gs_resp_graph{}},
                gs_client:graph_request(Client, #gri{
                    type = od_group, id = Group, aspect = instance
                }, get, #{}, true)
            )
        end, Groups)
    end,

    {ok, SupervisorPid, _} = spawn_clients(Config, gui, Users, true, GatherUpdate, OnSuccessFun),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),


    ?begin_measurement(groups_update_time),
    lists:foreach(fun(Seq) ->
        NewName = case Seq of
            UpdateNum -> FinalGroupName;
            _ -> <<"gr", (integer_to_binary(rand:uniform(999999)))/binary>>
        end,
        lists:foreach(fun(Group) ->
            oz_test_utils:update_group(Config, Group, NewName)
        end, Groups)
    end, lists:seq(1, UpdateNum)),
    ?end_measurement(groups_update_time),

    ?begin_measurement(updates_propagation_time),
    receive finished -> ok end,
    ?end_measurement(updates_propagation_time),

    ?derive_measurement(groups_update_time, avg_update_time_per_group, fun(M) ->
        M / GroupNum / UpdateNum
    end),
    ?derive_measurement(updates_propagation_time, avg_propagation_time_per_client, fun(M) ->
        M / UserNum
    end),
    ?derive_measurement(updates_propagation_time, avg_propagation_time_per_client_per_group, fun(M) ->
        M / GroupNum / UserNum
    end),


    terminate_clients(Config, SupervisorPid),

    [
        ?format_measurement(groups_update_time, ms,
            "Time taken to make all updates of group names."),
        ?format_measurement(avg_update_time_per_group, us,
            "Average time taken to make one update per group."),
        ?format_measurement(updates_propagation_time, ms,
            "Time taken to propagate all updates after they are done."),
        ?format_measurement(avg_propagation_time_per_client, ms,
            "Average time taken to propagate all updates after they are done per client."),
        ?format_measurement(avg_propagation_time_per_client_per_group, us,
            "Average time taken to propagate all updates after they are done per client per group.")
    ].


privileges_in_a_big_space_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of privileges update propagation to providers via Graph Sync, "
        "where a number of users all belong to a big space and their privileges are modified. "
        "Granted privileges are always a long list to generate bigger update sizes."},
        {parameters, [?PROVIDER_NUM(30), ?USER_NUM(30), ?UPDATE_NUM(3)]},
        ?PERF_CFG(small, [?PROVIDER_NUM(30), ?USER_NUM(30), ?UPDATE_NUM(3)]),
        ?PERF_CFG(medium, [?PROVIDER_NUM(60), ?USER_NUM(60), ?UPDATE_NUM(6)]),
        ?PERF_CFG(large, [?PROVIDER_NUM(100), ?USER_NUM(100), ?UPDATE_NUM(10)])
    ]).
privileges_in_a_big_space_performance_base(Config) ->
    UserNum = ?USER_NUM,
    UpdateNum = ?UPDATE_NUM,
    ProviderNum = ?PROVIDER_NUM,
    SpacePrivileges = privileges:space_privileges(),
    PrivsNum = length(SpacePrivileges),
    [Creator | Rest] = Users = create_n_users(Config, UserNum),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(Creator), <<"space">>),
    [oz_test_utils:space_add_user(Config, Space, U) || U <- Rest],
    ProvidersAndAuths = create_n_supporting_providers(Config, ProviderNum, Space),
    flush_db_changes(Config),

    FinalPrivileges = [atom_to_binary(?SPACE_VIEW, utf8)],

    Master = self(),

    GathererLoop = fun Loop(FinishedClients = _State) ->
        NewFinishedClients = receive
            {finish, ClientRef} ->
                [ClientRef | FinishedClients]
        end,
        case length(NewFinishedClients) of
            ProviderNum -> Master ! finished;
            _ -> Loop(NewFinishedClients)
        end
    end,

    GathererPid = spawn(fun() ->
        GathererLoop([])
    end),

    SpaceGri = #gri{type = od_space, id = Space, aspect = instance},
    GatherUpdate = fun(Push) ->
        #gs_push_graph{gri = SpaceGri, change_type = updated, data = #{
            <<"effectiveUsers">> := UserPrivileges}
        } = Push,
        Finished = lists:all(fun({_, Privileges}) ->
            Privileges =:= FinalPrivileges
        end, maps:to_list(UserPrivileges)),
        case Finished of
            true ->
                GathererPid ! {finish, self()};
            _ -> ok
        end
    end,

    OnSuccessFun = fun(Client) ->
        ?assertMatch(
            {ok, #gs_resp_graph{}},
            gs_client:graph_request(Client, SpaceGri, get, #{}, true)
        )
    end,

    {ok, SupervisorPid, _} = spawn_clients(Config, provider, ProvidersAndAuths, true, GatherUpdate, OnSuccessFun),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),


    ?begin_measurement(privileges_update_time),
    lists:foreach(fun(Seq) ->
        NewPrivileges = case Seq of
            UpdateNum -> FinalPrivileges;
            % Granted privileges are always a long list to generate bigger update sizes.
            _ -> lists:sublist(SpacePrivileges, PrivsNum - 2 + (Seq rem 3))
        end,
        utils:pforeach(fun(User) ->
            oz_test_utils:space_set_user_privileges(
                Config, Space, User, NewPrivileges, SpacePrivileges -- NewPrivileges
            )
        end, Users)
    end, lists:seq(1, UpdateNum)),
    ?end_measurement(privileges_update_time),


    ?begin_measurement(updates_propagation_time),
    receive finished -> ok end,
    ?end_measurement(updates_propagation_time),

    ?derive_measurement(privileges_update_time, avg_update_time_per_user, fun(M) ->
        M / UserNum / UpdateNum
    end),
    ?derive_measurement(updates_propagation_time, avg_propagation_time_per_client, fun(M) ->
        M / ProviderNum
    end),
    ?derive_measurement(updates_propagation_time, avg_propagation_time_per_client_per_user, fun(M) ->
        M / ProviderNum / UserNum
    end),


    terminate_clients(Config, SupervisorPid),

    [
        ?format_measurement(privileges_update_time, ms,
            "Time taken to update privileges of all users given number of times."),
        ?format_measurement(avg_update_time_per_user, us,
            "Average time taken to update privileges of one user once."),
        ?format_measurement(updates_propagation_time, ms,
            "Time taken to propagate all updates after they are done."),
        ?format_measurement(avg_propagation_time_per_client, us,
            "Average time taken to propagate all updates after they are done per client."),
        ?format_measurement(avg_propagation_time_per_client_per_user, us,
            "Average time taken to propagate all updates after they are done per client per user.")
    ].


concurrent_active_clients_spawning_performance(Config) ->
    ?PERFORMANCE(Config, [
        {repeats, 3},
        {success_rate, 100},
        {description, "Checks the performance of spawning multiple, parallel GS "
        "clients that regularly make a request."},
        {parameters, [?USER_NUM(100), ?REQUEST_INTERVAL_SECONDS(2)]},
        ?PERF_CFG(small, [?USER_NUM(100), ?REQUEST_INTERVAL_SECONDS(2)]),
        ?PERF_CFG(medium, [?USER_NUM(500), ?REQUEST_INTERVAL_SECONDS(2)]),
        ?PERF_CFG(large, [?USER_NUM(1000), ?REQUEST_INTERVAL_SECONDS(2)])
    ]).
concurrent_active_clients_spawning_performance_base(Config) ->
    UserNum = ?USER_NUM,
    RequestInterval = ?REQUEST_INTERVAL_SECONDS,
    Users = create_n_users(Config, UserNum),

    MakeRequestRegularly = fun(Client) ->
        Pid = spawn_link(fun Loop() ->
            receive
                perform_request ->
                    ?assertMatch(
                        {ok, #gs_resp_graph{}},
                        gs_client:graph_request(Client, #gri{
                            type = od_user, id = ?SELF, aspect = instance
                        }, get)
                    ),
                    erlang:send_after(round(timer:seconds(RequestInterval)), self(), perform_request),
                    Loop()
            end
        end),
        erlang:send_after(rand:uniform(round(timer:seconds(RequestInterval))), Pid, perform_request)
    end,


    ?begin_measurement(clients_spawning_time),
    {ok, SupervisorPid, _} = spawn_clients(
        Config, gui, Users, true, ?NO_OP_FUN, MakeRequestRegularly
    ),
    ?end_measurement(clients_spawning_time),

    ?derive_measurement(clients_spawning_time, avg_time_per_client, fun(M) ->
        M / UserNum
    end),


    terminate_clients(Config, SupervisorPid),

    [
        ?format_measurement(clients_spawning_time, s,
            "Time taken by clients spawning and making regular requests."),
        ?format_measurement(avg_time_per_client, ms,
            "Average time taken by one client to spawn and make regular requests.")
    ].


%%%===================================================================
%%% Internal functions
%%%===================================================================

spawn_clients(Config, Type, Clients, RetryFlag, CallbackFunction, OnSuccessFun) ->
    URL = oz_test_utils:graph_sync_url(Config, Type),
    AuthsAndIdentities = lists:map(fun(Client) ->
        case Type of
            gui ->
                {ok, {_SessionId, CookieValue}} = oz_test_utils:log_in(Config, Client),
                {ok, GuiToken} = oz_test_utils:request_gui_token(Config, CookieValue),
                Auth = {token, GuiToken},
                Identity = ?SUB(user, Client),
                {Auth, Identity};
            provider ->
                {ProviderId, ProviderToken} = Client,
                Auth = {token, ProviderToken},
                Identity = ?SUB(?ONEPROVIDER, ProviderId),
                {Auth, Identity}
        end
    end, Clients),

    graph_sync_test_utils:spawn_clients(
        URL, ssl_opts(Config), AuthsAndIdentities, RetryFlag, CallbackFunction, OnSuccessFun
    ).


terminate_clients(Config, SupervisorPid) ->
    KeepaliveInterval = rpc:call(random_node(Config), application, get_env, [
        ?CLUSTER_WORKER_APP_NAME, graph_sync_websocket_keepalive, timer:seconds(5)
    ]),
    graph_sync_test_utils:terminate_clients(SupervisorPid, KeepaliveInterval * 2).


create_n_users(Config, Number) ->
    lists:map(fun(_) ->
        {ok, User} = oz_test_utils:create_user(Config),
        User
    end, lists:seq(1, Number)).


create_n_supporting_providers(Config, Number, SpaceId) ->
    SupportSize = oz_test_utils:minimum_support_size(Config),
    lists:map(fun(_) ->
        {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(
            Config, <<"provider">>
        ),
        oz_test_utils:support_space(Config, Provider, SpaceId, SupportSize),
        {Provider, ProviderToken}
    end, lists:seq(1, Number)).


random_node(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    lists:nth(rand:uniform(length(Nodes)), Nodes).


% Waits until seq counter in graph sync state does not change within given
% timeout, which indicates that no more changes are arriving.
flush_db_changes(Config) ->
    flush_db_changes(Config, timer:seconds(5)).
flush_db_changes(Config, Timeout) ->
    flush_db_changes(Config, -1, Timeout).
flush_db_changes(Config, LastSeq, Timeout) ->
    case rpc:call(random_node(Config), gs_server_state, get_seq, []) of
        LastSeq ->
            ok;
        NewSeq ->
            timer:sleep(Timeout),
            flush_db_changes(Config, NewSeq, Timeout)
    end.


ssl_opts(Config) ->
    [{secure, only_verify_peercert}, {cacerts, oz_test_utils:gui_ca_certs(Config)}].


%%%===================================================================
%%% Setup / teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop(),
    ok.
