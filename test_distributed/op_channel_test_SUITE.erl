%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains tests for communication channel between
%% providers and Global Registry.
%% @end
%% ===================================================================
-module(op_channel_test_SUITE).
-author("Krzysztof Trzepla").

%% Includes
-include("dao/dao_users.hrl").
-include("registered_names.hrl").
-include("gr_messages_pb.hrl").
-include("gr_communication_protocol_pb.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("annotations/include/annotations.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([connection_test/1, space_support_test/1, modification_test/1, removal_test/1]).

-perf_test({perf_cases, []}).
all() -> [connection_test, space_support_test, modification_test, removal_test].

%% ====================================================================
%% Test functions
%% ====================================================================

%% This test checks whether connections to providers are properly added and removed.
%% Moreover it tests whether messages are pushed to providers.
connection_test(Config) ->
    [
        {ProviderId1, _, _},
        {ProviderId2, _, _},
        {ProviderId3, KeyFile3, CertFile3}
    ] = Providers = create_providers(Config, 3, []),

    [Socket1, Socket2, Socket3] = Sockets = connect_providers(Config, Providers),

    Msg = <<"test_message">>,

    send_to_providers(Config, [ProviderId1, ProviderId2, ProviderId3], Msg),
    assert_received([Socket1, Socket2, Socket3], Msg),

    send_to_providers(Config, [ProviderId1, ProviderId2], Msg),
    assert_received([Socket1, Socket2], Msg),
    assert_not_received([Socket3]),

    send_to_providers(Config, [ProviderId1], Msg),
    assert_received([Socket1], Msg),
    assert_not_received([Socket2, Socket3]),

    disconnect_providers([Socket3]),

    send_to_providers(Config, [ProviderId1, ProviderId2, ProviderId3], Msg),
    assert_received([Socket1, Socket2], Msg),
    assert_not_received([Socket3]),

    [NewSocket3] = connect_providers(Config, [{ProviderId3, KeyFile3, CertFile3}]),
    send_to_providers(Config, [ProviderId1, ProviderId2, ProviderId3], Msg),
    assert_received([Socket1, Socket2, NewSocket3], Msg),

    disconnect_providers(Sockets).


%% This test checks whether porivders supporting given space get notification
%% about new provider that starts to support this space. Moreover it checks
%% whether new provider gets to know about all users and groups belonging to
%% newly supported space.
space_support_test(Config) ->
    [
        {ProviderId1, _, _},
        {ProviderId2, _, _}
    ] = Providers = create_providers(Config, 2, []),

    [Socket1, Socket2] = Sockets = connect_providers(Config, Providers),

    [UserId1, UserId2, UserId3] = create_users(Config, 3, []),

    [{SpaceId1, SpaceName1}] = create_spaces(Config, {user, UserId1}, 1, []),

    ?assertEqual(ok, gr_test_utils:support_space(Config, ProviderId1, SpaceId1, 1)),

    assert_received(Config, [Socket1], #spacemodified{
        id = SpaceId1,
        name = SpaceName1,
        size = [#spacemodified_size{provider = ProviderId1, size = 1}],
        users = [UserId1],
        groups = [],
        providers = [ProviderId1]
    }),

    assert_received(Config, [Socket1], #usermodified{id = UserId1, spaces = [SpaceId1], groups = []}),

    assert_not_received([Socket1]),

    [{GroupId1, GroupName1}] = create_groups(Config, UserId2, 1, []),

    ?assertEqual(ok, gr_test_utils:join_group(Config, UserId3, GroupId1)),

    ?assertEqual(ok, gr_test_utils:join_space(Config, {group, GroupId1}, SpaceId1)),

    assert_received(Config, [Socket1], #spacemodified{
        id = SpaceId1,
        name = SpaceName1,
        size = [#spacemodified_size{provider = ProviderId1, size = 1}],
        users = [UserId1],
        groups = [GroupId1],
        providers = [ProviderId1]
    }),

    assert_received(Config, [Socket1], #groupmodified{id = GroupId1, name = GroupName1}),

    assert_received(Config, [Socket1], #usermodified{id = UserId3, spaces = [], groups = [GroupId1]}),
    assert_received(Config, [Socket1], #usermodified{id = UserId2, spaces = [], groups = [GroupId1]}),

    assert_not_received([Socket1]),

    ?assertEqual(ok, gr_test_utils:support_space(Config, ProviderId2, SpaceId1, 2)),

    assert_received(Config, [Socket1, Socket2], #spacemodified{
        id = SpaceId1,
        name = SpaceName1,
        size = [
            #spacemodified_size{provider = ProviderId2, size = 2},
            #spacemodified_size{provider = ProviderId1, size = 1}
        ],
        users = [UserId1],
        groups = [GroupId1],
        providers = [ProviderId2, ProviderId1]
    }),

    assert_received(Config, [Socket2], #usermodified{id = UserId1, spaces = [SpaceId1], groups = []}),

    assert_received(Config, [Socket2], #groupmodified{id = GroupId1, name = GroupName1}),

    assert_received(Config, [Socket2], #usermodified{id = UserId3, spaces = [], groups = [GroupId1]}),
    assert_received(Config, [Socket2], #usermodified{id = UserId2, spaces = [], groups = [GroupId1]}),

    assert_not_received([Socket1, Socket2]),

    unsupport_space(Config, [ProviderId1], SpaceId1),

    assert_received(Config, [Socket1], #spaceremoved{id = SpaceId1}),

    assert_received(Config, [Socket2], #spacemodified{
        id = SpaceId1,
        name = SpaceName1,
        size = [
            #spacemodified_size{provider = ProviderId2, size = 2}
        ],
        users = [UserId1],
        groups = [GroupId1],
        providers = [ProviderId2]
    }),

    assert_not_received([Socket1, Socket2]),

    remove_providers(Config, [ProviderId2]),

    assert_received(Config, [Socket2], #spaceremoved{id = SpaceId1}),

    assert_not_received([Socket1, Socket2]),

    disconnect_providers(Sockets).


%% This test checks whether providers receive notifications about modifications of groups and spaces.
modification_test(Config) ->
    [
        {ProviderId1, _, _},
        {ProviderId2, _, _},
        {ProviderId3, _, _}
    ] = Providers = create_providers(Config, 3, []),

    [UserId1, UserId2, UserId3] = create_users(Config, 3, []),

    [{GroupId1, GroupName1}] = create_groups(Config, UserId1, 1, []),
    [{GroupId2, GroupName2}] = create_groups(Config, UserId2, 1, []),
    [{GroupId3, GroupName3}] = create_groups(Config, UserId3, 1, []),

    [{SpaceId1, SpaceName1}] = create_spaces(Config, {group, GroupId1}, 1, []),
    [{SpaceId2, SpaceName2}] = create_spaces(Config, {group, GroupId2}, 1, []),
    [{SpaceId3, SpaceName3}] = create_spaces(Config, {group, GroupId3}, 1, []),

    ?assertEqual(ok, gr_test_utils:support_space(Config, ProviderId1, SpaceId1, 1)),
    ?assertEqual(ok, gr_test_utils:support_space(Config, ProviderId1, SpaceId2, 1)),
    ?assertEqual(ok, gr_test_utils:support_space(Config, ProviderId1, SpaceId3, 1)),

    ?assertEqual(ok, gr_test_utils:support_space(Config, ProviderId2, SpaceId1, 2)),
    ?assertEqual(ok, gr_test_utils:support_space(Config, ProviderId2, SpaceId2, 2)),

    ?assertEqual(ok, gr_test_utils:support_space(Config, ProviderId3, SpaceId1, 3)),

    [Socket1, Socket2, Socket3] = Sockets = connect_providers(Config, Providers),

    NewGroupName1 = <<GroupName1/binary, "_new">>,
    NewGroupName2 = <<GroupName2/binary, "_new">>,
    NewGroupName3 = <<GroupName3/binary, "_new">>,

    modify_groups(Config, [{GroupId1, NewGroupName1}, {GroupId2, NewGroupName2}, {GroupId3, NewGroupName3}]),

    assert_received(Config, [Socket1, Socket2, Socket3], #groupmodified{id = GroupId1, name = NewGroupName1}),
    assert_received(Config, [Socket1, Socket2], #groupmodified{id = GroupId2, name = NewGroupName2}),
    assert_received(Config, [Socket1], #groupmodified{id = GroupId3, name = NewGroupName3}),

    assert_not_received([Socket1, Socket2, Socket3]),

    NewSpaceName1 = <<SpaceName1/binary, "_new">>,
    NewSpaceName2 = <<SpaceName2/binary, "_new">>,
    NewSpaceName3 = <<SpaceName3/binary, "_new">>,

    modify_spaces(Config, [{SpaceId1, NewSpaceName1}, {SpaceId2, NewSpaceName2}, {SpaceId3, NewSpaceName3}]),

    assert_received(Config, [Socket1, Socket2, Socket3], #spacemodified{
        id = SpaceId1,
        name = NewSpaceName1,
        size = [
            #spacemodified_size{provider = ProviderId3, size = 3},
            #spacemodified_size{provider = ProviderId2, size = 2},
            #spacemodified_size{provider = ProviderId1, size = 1}
        ],
        users = [],
        groups = [GroupId1],
        providers = [ProviderId3, ProviderId2, ProviderId1]
    }),

    assert_received(Config, [Socket1, Socket2], #spacemodified{
        id = SpaceId2,
        name = NewSpaceName2,
        size = [
            #spacemodified_size{provider = ProviderId2, size = 2},
            #spacemodified_size{provider = ProviderId1, size = 1}
        ],
        users = [],
        groups = [GroupId2],
        providers = [ProviderId2, ProviderId1]
    }),

    assert_received(Config, [Socket1], #spacemodified{
        id = SpaceId3,
        name = NewSpaceName3,
        size = [
            #spacemodified_size{provider = ProviderId1, size = 1}
        ],
        users = [],
        groups = [GroupId3],
        providers = [ProviderId1]
    }),

    assert_not_received([Socket1, Socket2, Socket3]),

    disconnect_providers(Sockets).


%% This test checks whether providers receive notifications about removal of groups and spaces.
removal_test(Config) ->
    [
        {ProviderId1, _, _},
        {ProviderId2, _, _}
    ] = Providers = create_providers(Config, 2, []),

    [UserId1, UserId2, UserId3] = create_users(Config, 3, []),

    [{GroupId1, _}] = create_groups(Config, UserId1, 1, []),

    ?assertEqual(ok, gr_test_utils:join_group(Config, UserId2, GroupId1)),
    ?assertEqual(ok, gr_test_utils:join_group(Config, UserId3, GroupId1)),

    [{SpaceId1, _}] = create_spaces(Config, {group, GroupId1}, 1, []),

    ?assertEqual(ok, gr_test_utils:support_space(Config, ProviderId1, SpaceId1, 1)),
    ?assertEqual(ok, gr_test_utils:support_space(Config, ProviderId2, SpaceId1, 2)),

    [Socket1, Socket2] = Sockets = connect_providers(Config, Providers),

    remove_group_users(Config, [UserId1], GroupId1),

    assert_received(Config, [Socket1, Socket2], #usermodified{id = UserId1, spaces = [], groups = []}),

    assert_not_received(Sockets),

    remove_groups(Config, [GroupId1]),

    assert_received(Config, Sockets, #usermodified{id = UserId3, spaces = [], groups = []}),
    assert_received(Config, Sockets, #usermodified{id = UserId2, spaces = [], groups = []}),

    assert_received(Config, Sockets, #spaceremoved{id = SpaceId1}),

    assert_received(Config, Sockets, #groupremoved{id = GroupId1}),

    assert_not_received(Sockets),

    disconnect_providers(Sockets).


%% ====================================================================
%% SetUp and TearDown functions
%% ====================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    timer:sleep(60000), % TODO add nagios to GR and delete sleep
    NewConfig.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_providers(_, 0, Providers) ->
    Providers;

create_providers(Config, N, Providers) ->
    CreateProviderAns = gr_test_utils:create_provider(Config, <<"provider", (integer_to_binary(N))/binary>>,
        [<<"127.0.0.1">>], <<"https://127.0.0.1:443">>),
    ?assertMatch({ok, _, _, _}, CreateProviderAns),
    {ok, ProviderId, KeyFile, CertFile} = CreateProviderAns,
    create_providers(Config, N - 1, [{ProviderId, KeyFile, CertFile} | Providers]).


remove_providers(Config, Providers) ->
    [Node] = ?config(gr_nodes, Config),
    lists:foreach(fun(Provider) ->
        ?assert(rpc:call(Node, provider_logic, remove, [Provider]))
    end, Providers).


create_users(_, 0, Users) ->
    Users;

create_users(Config, N, Users) ->
    CreateUserAns = gr_test_utils:create_user(Config, #user{name = <<"user", (integer_to_binary(N))/binary>>}),
    ?assertMatch({ok, _}, CreateUserAns),
    {ok, UserId} = CreateUserAns,
    create_users(Config, N - 1, [UserId | Users]).


create_groups(_, _, 0, Groups) ->
    Groups;

create_groups(Config, UserId, N, Groups) ->
    Name = <<"group", (integer_to_binary(N))/binary>>,
    CreateGroupAns = gr_test_utils:create_group(Config, UserId, Name),
    ?assertMatch({ok, _}, CreateGroupAns),
    {ok, GroupId} = CreateGroupAns,
    create_groups(Config, UserId, N - 1, [{GroupId, Name} | Groups]).


modify_groups(Config, Groups) ->
    [Node] = ?config(gr_nodes, Config),
    lists:foreach(fun({GroupId, Name}) ->
        ?assertEqual(ok, rpc:call(Node, group_logic, modify, [GroupId, Name]))
    end, Groups).


remove_groups(Config, Groups) ->
    [Node] = ?config(gr_nodes, Config),
    lists:foreach(fun(GroupId) ->
        ?assert(rpc:call(Node, group_logic, remove, [GroupId]))
    end, Groups).


remove_group_users(Config, Users, GroupId) ->
    [Node] = ?config(gr_nodes, Config),
    lists:foreach(fun(UserId) ->
        ?assert(rpc:call(Node, group_logic, remove_user, [GroupId, UserId]))
    end, Users).


create_spaces(_, _, 0, Spaces) ->
    Spaces;

create_spaces(Config, Member, N, Spaces) ->
    Name = <<"space", (integer_to_binary(N))/binary>>,
    CreateSpaceAns = gr_test_utils:create_space(Config, Member, Name),
    ?assertMatch({ok, _}, CreateSpaceAns),
    {ok, SpaceId} = CreateSpaceAns,
    create_spaces(Config, Member, N - 1, [{SpaceId, Name} | Spaces]).


modify_spaces(Config, Spaces) ->
    [Node] = ?config(gr_nodes, Config),
    lists:foreach(fun({SpaceId, Name}) ->
        ?assertEqual(ok, rpc:call(Node, space_logic, modify, [SpaceId, Name]))
    end, Spaces).


unsupport_space(Config, Providers, Space) ->
    [Node] = ?config(gr_nodes, Config),
    lists:foreach(fun(Provider) ->
        ?assert(rpc:call(Node, space_logic, remove_provider, [Space, Provider]))
    end, Providers).


connect_providers(Config, Providers) ->
    lists:map(fun({_, KeyFile, CertFile}) ->
        [Node] = ?config(gr_nodes, Config),
        CACertFile = ?config(grpcacert_file, Config),
        {ok, Port} = rpc:call(Node, application, get_env, [?APP_Name, op_channel_port]),
        WSSConnectAns = wss:connect(utils:get_host(Node), Port, [{keyfile, KeyFile}, {certfile, CertFile}, {cacertfile, CACertFile}]),
        ?assertMatch({ok, _}, WSSConnectAns),
        {ok, Socket} = WSSConnectAns,
        Socket
    end, Providers).


send_to_providers(Config, ProviderIds, Msg) ->
    [Node] = ?config(gr_nodes, Config),
    rpc:call(Node, op_channel_logic, push, [ProviderIds, Msg]).


assert_received(Sockets, Msg) ->
    lists:foreach(fun(Socket) ->
        ?assertEqual({ok, Msg}, wss_handler:recv(Socket))
    end, Sockets).

assert_received(Config, Sockets, Msg) ->
    [Node] = ?config(gr_nodes, Config),
    lists:foreach(fun(Socket) ->
        WSSReceiveAns = wss_handler:recv(Socket),
        ?assertMatch({ok, _}, WSSReceiveAns),
        {ok, Data} = WSSReceiveAns,
        PbDecodeAns = rpc:call(Node, pb, decode, ["gr_communication_protocol", "message", Data]),
        ?assertMatch({ok, _}, PbDecodeAns),
        {ok, #message{message_decoder_name = Decoder, message_type = Type, input = Input}} = PbDecodeAns,
        ?assertEqual({ok, Msg}, rpc:call(Node, pb, decode, [Decoder, Type, Input]))
    end, Sockets).


assert_not_received(Sockets) ->
    lists:foreach(fun(Socket) ->
        ?assertEqual({error, timeout}, wss_handler:recv(Socket))
    end, Sockets).


disconnect_providers(Sockets) ->
    lists:foreach(fun(Socket) ->
        ?assertEqual(ok, wss_handler:disconnect(Socket))
    end, Sockets).
