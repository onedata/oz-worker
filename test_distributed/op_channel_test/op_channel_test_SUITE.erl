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
-include("test_utils.hrl").
-include("dao/dao_users.hrl").
-include("registered_names.hrl").
-include("gr_messages_pb.hrl").
-include("gr_communication_protocol_pb.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_node_starter.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([connection_test/1, space_support_test/1]).

all() -> [connection_test, space_support_test].

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

    ?assertEqual(ok, test_utils:support_space(Config, ProviderId1, SpaceId1, 1)),

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

    ?assertEqual(ok, test_utils:join_group(Config, UserId3, GroupId1)),

    ?assertEqual(ok, test_utils:join_space(Config, {group, GroupId1}, SpaceId1)),

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

    ?assertEqual(ok, test_utils:support_space(Config, ProviderId2, SpaceId1, 2)),

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

    disconnect_providers(Sockets).


%% ====================================================================
%% SetUp and TearDown functions
%% ====================================================================

init_per_suite(Config) ->
    ?INIT_CODE_PATH,
    test_utils:cleanup(),
    {Certs, CACertsDir, GRPCADir} = ?PREPARE_CERT_FILES(Config),

    DbNodesEnv = {db_nodes, [?DB_NODE]},
    Nodes = test_node_starter:start_test_nodes(1, true),
    test_node_starter:start_app_on_nodes(?APP_Name, ?GR_DEPS, Nodes,
        [[
            DbNodesEnv,
            ?cert_paths(Certs, CACertsDir, GRPCADir)
        ]]
    ),
    Config ++ [{nodes, Nodes}].


end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    test_node_starter:stop_app_on_nodes(?APP_Name, ?GR_DEPS, Nodes),
    test_node_starter:stop_test_nodes(Nodes).


%% ====================================================================
%% Internal functions
%% ====================================================================

create_providers(_, 0, Providers) ->
    Providers;

create_providers(Config, N, Providers) ->
    CreateProviderAns = test_utils:create_provider(Config, <<"provider", (integer_to_binary(N))/binary>>,
        [<<"127.0.0.1">>], <<"https://127.0.0.1:443">>),
    ?assertMatch({ok, _, _, _}, CreateProviderAns),
    {ok, ProviderId, KeyFile, CertFile} = CreateProviderAns,
    create_providers(Config, N - 1, [{ProviderId, KeyFile, CertFile} | Providers]).


create_users(_, 0, Users) ->
    Users;

create_users(Config, N, Users) ->
    CreateUserAns = test_utils:create_user(Config, #user{name = <<"user", (integer_to_binary(N))/binary>>}),
    ?assertMatch({ok, _}, CreateUserAns),
    {ok, UserId} = CreateUserAns,
    create_users(Config, N - 1, [UserId | Users]).


create_groups(_, _, 0, Groups) ->
    Groups;

create_groups(Config, UserId, N, Groups) ->
    Name = <<"group", (integer_to_binary(N))/binary>>,
    CreateGroupAns = test_utils:create_group(Config, UserId, Name),
    ?assertMatch({ok, _}, CreateGroupAns),
    {ok, GroupId} = CreateGroupAns,
    create_groups(Config, UserId, N - 1, [{GroupId, Name} | Groups]).


create_spaces(_, _, 0, Spaces) ->
    Spaces;

create_spaces(Config, Member, N, Spaces) ->
    Name = <<"space", (integer_to_binary(N))/binary>>,
    CreateSpaceAns = test_utils:create_space(Config, Member, Name),
    ?assertMatch({ok, _}, CreateSpaceAns),
    {ok, SpaceId} = CreateSpaceAns,
    create_spaces(Config, Member, N - 1, [{SpaceId, Name} | Spaces]).


connect_providers(Config, Providers) ->
    lists:map(fun({_, KeyFile, CertFile}) ->
        [Node] = ?config(nodes, Config),
        CACertFile = ?config(grpcacert_file, Config),
        {ok, Port} = rpc:call(Node, application, get_env, [?APP_Name, op_channel_port]),
        WSSConnectAns = wss:connect("127.0.0.1", Port, [{keyfile, KeyFile}, {certfile, CertFile}, {cacertfile, CACertFile}]),
        ?assertMatch({ok, _}, WSSConnectAns),
        {ok, Socket} = WSSConnectAns,
        Socket
    end, Providers).


send_to_providers(Config, ProviderIds, Msg) ->
    [Node] = ?config(nodes, Config),
    rpc:call(Node, op_channel_logic, push, [ProviderIds, Msg]).


assert_received(Sockets, Msg) ->
    lists:foreach(fun(Socket) ->
        ?assertEqual({ok, Msg}, wss:recv(Socket))
    end, Sockets).

assert_received(Config, Sockets, Msg) ->
    [Node] = ?config(nodes, Config),
    lists:foreach(fun(Socket) ->
        WSSReceiveAns = wss:recv(Socket),
        ?assertMatch({ok, _}, WSSReceiveAns),
        {ok, Data} = WSSReceiveAns,
        PbDecodeAns = rpc:call(Node, pb, decode, ["gr_communication_protocol", "message", Data]),
        ?assertMatch({ok, _}, PbDecodeAns),
        {ok, #message{message_decoder_name = Decoder, message_type = Type, input = Input}} = PbDecodeAns,
        ?assertEqual({ok, Msg}, rpc:call(Node, pb, decode, [Decoder, Type, Input]))
    end, Sockets).


assert_not_received(Sockets) ->
    lists:foreach(fun(Socket) ->
        ?assertEqual({error, timeout}, wss:recv(Socket))
    end, Sockets).


disconnect_providers(Sockets) ->
    lists:foreach(fun(Socket) ->
        ?assertEqual(ok, wss:disconnect(Socket))
    end, Sockets).
