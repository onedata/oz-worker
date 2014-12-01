%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains tests communication channel between providers
%% and Global Registry.
%% @end
%% ===================================================================
-module(op_channel_test_SUITE).
-author("Krzysztof Trzepla").

%% Includes
-include("test_utils.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_node_starter.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([connection_test/1]).

all() -> [connection_test].

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
    ] = Providers = register_providers(Config, 3, []),

    [Socket1, Socket2, Socket3] = Sockets = connect_providers(Config, Providers),

    Msg = <<"test_message">>,

    send_to_providers(Config, [ProviderId1, ProviderId2, ProviderId3], Msg),
    assert_received([Socket1, Socket2, Socket3], Msg),

    disconnect_providers([Socket3]),

    send_to_providers(Config, [ProviderId1, ProviderId2, ProviderId3], Msg),
    assert_received([Socket1, Socket2], Msg),
    assert_not_received([Socket3]),

    [NewSocket3] = connect_providers(Config, [{ProviderId3, KeyFile3, CertFile3}]),
    send_to_providers(Config, [ProviderId1, ProviderId2, ProviderId3], Msg),
    assert_received([Socket1, Socket2, NewSocket3], Msg),

    disconnect_providers(Sockets).


%% ====================================================================
%% SetUp and TearDown functions
%% ====================================================================

init_per_suite(Config) ->
    ?INIT_CODE_PATH,
    test_utils:cleanup(),
    {Certs, CACertsDir, GRPCADir} = ?PREPARE_CERT_FILES(Config),

    DbNodesEnv = {db_nodes, [?DB_NODE]},
    Nodes = test_node_starter:start_test_nodes(1),
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

register_providers(_, 0, Providers) ->
    Providers;

register_providers(Config, N, Providers) ->
    RegisterProviderAns = test_utils:register_provider(Config, <<"provider", (integer_to_binary(N))/binary>>,
        [<<"127.0.0.1">>], <<"https://127.0.0.1:443">>),
    ?assertMatch({ok, _, _, _}, RegisterProviderAns),
    {ok, ProviderId, KeyFile, CertFile} = RegisterProviderAns,
    register_providers(Config, N - 1, [{ProviderId, KeyFile, CertFile} | Providers]).


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


assert_not_received(Sockets) ->
    lists:foreach(fun(Socket) ->
        ?assertEqual({error, timeout}, wss:recv(Socket))
    end, Sockets).


disconnect_providers(Sockets) ->
    lists:foreach(fun(Socket) ->
        ?assertEqual(ok, wss:disconnect(Socket))
    end, Sockets).
