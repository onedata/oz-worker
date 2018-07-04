%%% @author Lukasz Opiola
%%% @copyright (C): 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning GUI session in Onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(session_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/global_definitions.hrl").
-include_lib("gui/include/gui_session.hrl").

-type config() :: [{atom(), term()}].

-define(STARTING_TIMESTAMP, 1500000000).

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    multiple_connections_per_session/1,
    multiple_sessions_per_user/1,
    session_cookie_refresh_and_grace_period/1,
    cleanup_on_session_expiry/1,
    cleanup_on_session_delete/1,
    cleanup_on_user_delete/1,
    cleanup_of_expired_sessions_upon_other_session_expiry/1,
    cleanup_of_expired_sessions_upon_other_session_delete/1
]).


all() -> ?ALL([
    multiple_connections_per_session,
    multiple_sessions_per_user,
    session_cookie_refresh_and_grace_period,
    cleanup_on_session_expiry,
    cleanup_on_session_delete,
    cleanup_on_user_delete,
    cleanup_of_expired_sessions_upon_other_session_expiry,
    cleanup_of_expired_sessions_upon_other_session_delete
]).


%%%===================================================================
%%% Tests
%%%===================================================================

multiple_connections_per_session(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #od_user{}),
    {SessionId, Cookie} = log_in(Config, UserId),

    ?assert(compare_user_sessions(Config, UserId, [SessionId])),

    {_ClientPid1, ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie),
    {ClientPid2, ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie),
    {_ClientPid3, ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie),
    ?assert(compare_connections_per_session(Config, SessionId, [ServerPid1, ServerPid2, ServerPid3])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2, ServerPid3])),

    exit(ClientPid2, kill),
    ?assert(compare_connections_per_session(Config, SessionId, [ServerPid1, ServerPid3])),

    {_ClientPid4, ServerPid4, {user, UserId}} = start_gs_connection(Config, Cookie),
    ?assert(compare_connections_per_session(Config, SessionId, [ServerPid1, ServerPid3, ServerPid4])),

    ok.


multiple_sessions_per_user(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #od_user{}),
    {Session1, Cookie1} = log_in(Config, UserId),

    {_ClientPid1, ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie1),
    {ClientPid2, ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie1),
    {_ClientPid3, ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie1),
    ?assert(compare_user_sessions(Config, UserId, [Session1])),

    {Session2, Cookie2} = log_in(Config, UserId),
    {_ClientPid4, ServerPid4, {user, UserId}} = start_gs_connection(Config, Cookie2),
    {_ClientPid5, ServerPid5, {user, UserId}} = start_gs_connection(Config, Cookie2),
    ?assert(compare_user_sessions(Config, UserId, [Session1, Session2])),

    ?assert(compare_connections_per_session(Config, Session1, [ServerPid1, ServerPid2, ServerPid3])),
    ?assert(compare_connections_per_session(Config, Session2, [ServerPid4, ServerPid5])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2, ServerPid3, ServerPid4, ServerPid5])),

    exit(ClientPid2, kill),

    ?assert(compare_connections_per_session(Config, Session1, [ServerPid1, ServerPid3])),
    ?assert(compare_connections_per_session(Config, Session2, [ServerPid4, ServerPid5])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid3, ServerPid4, ServerPid5])),

    ok.


session_cookie_refresh_and_grace_period(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #od_user{}),
    {SessionId, Cookie} = log_in(Config, UserId),
    {_ClientPid1, _ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie),
    {_ClientPid2, _ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie),
    {_ClientPid3, _ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie),

    {ok, #gui_session{
        nonce = OldNonce
    }} = rpc:call(random_node(Config), new_gui_session_plugin, get, [SessionId]),

    simulate_time_passing(Config, get_gui_config(Config, session_cookie_refresh_interval) + 1),
    {ok, _, NewCookie, _} = ?assertMatch({ok, _, NewCookie, _}, validate_session(Config, Cookie)),
    % Cookie should have been refreshed
    ?assertNotEqual(Cookie, NewCookie),

    {ok, #gui_session{
        nonce = NewNonce,
        previous_nonce = PrevNonce
    }} = rpc:call(random_node(Config), new_gui_session_plugin, get, [SessionId]),

    ?assertNotEqual(OldNonce, NewNonce),
    ?assertMatch(OldNonce, PrevNonce),

    % After the grace period, the old cookie should stop working
    simulate_time_passing(Config, get_gui_config(Config, session_cookie_grace_period) + 1),

    ?assertMatch({error, invalid}, validate_session(Config, Cookie)),
    {_ClientPid4, _ServerPid4, nobody} = start_gs_connection(Config, Cookie),

    ok.


cleanup_on_session_expiry(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #od_user{}),
    {SessionId, Cookie} = log_in(Config, UserId),
    {ClientPid1, _ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie),
    {ClientPid2, _ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie),
    {ClientPid3, _ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie),

    simulate_time_passing(Config, get_gui_config(Config, session_cookie_ttl) + 1),

    {_ClientPid4, _ServerPid4, nobody} = start_gs_connection(Config, Cookie),

    ?assert(compare_user_sessions(Config, UserId, [])),
    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_session(Config, SessionId, [])),
    ?assertNot(is_client_alive(ClientPid1)),
    ?assertNot(is_client_alive(ClientPid2)),
    ?assertNot(is_client_alive(ClientPid3)),

    ok.


cleanup_on_session_delete(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #od_user{}),
    {SessionId, Cookie} = log_in(Config, UserId),
    {ClientPid1, ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie),
    {ClientPid2, ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie),
    {ClientPid3, ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie),

    % Wait for the connections to initialize
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2, ServerPid3])),

    oz_test_utils:log_out(Config, Cookie),

    ?assert(compare_user_sessions(Config, UserId, [])),
    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_session(Config, SessionId, [])),
    ?assertNot(is_client_alive(ClientPid1)),
    ?assertNot(is_client_alive(ClientPid2)),
    ?assertNot(is_client_alive(ClientPid3)),

    ok.


cleanup_on_user_delete(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #od_user{}),
    {Session1, Cookie1} = log_in(Config, UserId),
    {Session2, Cookie2} = log_in(Config, UserId),
    {Session3, Cookie3} = log_in(Config, UserId),
    {ClientPid1, _ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie1),
    {ClientPid2, _ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie2),
    {ClientPid3, _ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie3),

    rpc:call(random_node(Config), od_user, force_delete, [UserId]),
    ?assert(compare_user_sessions(Config, UserId, [])),
    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_session(Config, Session1, [])),
    ?assert(compare_connections_per_session(Config, Session2, [])),
    ?assert(compare_connections_per_session(Config, Session3, [])),
    ?assertNot(is_client_alive(ClientPid1)),
    ?assertNot(is_client_alive(ClientPid2)),
    ?assertNot(is_client_alive(ClientPid3)),

    ok.


cleanup_of_expired_sessions_upon_other_session_expiry(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #od_user{}),
    {Session1, Cookie1} = log_in(Config, UserId),
    {Session2, Cookie2} = log_in(Config, UserId),
    {ClientPid1, ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie1),
    {ClientPid2, ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie2),

    simulate_time_passing(Config, get_gui_config(Config, session_cookie_ttl) + 1),

    {_Session3, Cookie3} = log_in(Config, UserId),

    ?assert(compare_user_sessions(Config, UserId, [Session1, Session2])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2])),
    ?assert(compare_connections_per_session(Config, Session1, [ServerPid1])),
    ?assert(compare_connections_per_session(Config, Session2, [ServerPid2])),
    ?assert(is_client_alive(ClientPid1)),
    ?assert(is_client_alive(ClientPid2)),

    simulate_time_passing(Config, get_gui_config(Config, session_cookie_ttl) + 1),
    {_ClientPid4, _ServerPid4, nobody} = start_gs_connection(Config, Cookie3),

    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_session(Config, Session1, [])),
    ?assert(compare_connections_per_session(Config, Session2, [])),
    ?assertNot(is_client_alive(ClientPid1)),
    ?assertNot(is_client_alive(ClientPid2)),

    ok.


cleanup_of_expired_sessions_upon_other_session_delete(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config, #od_user{}),
    {Session1, Cookie1} = log_in(Config, UserId),
    {Session2, Cookie2} = log_in(Config, UserId),
    {ClientPid1, ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie1),
    {ClientPid2, ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie2),

    simulate_time_passing(Config, get_gui_config(Config, session_cookie_ttl) + 1),

    {_Session3, Cookie3} = log_in(Config, UserId),

    ?assert(compare_user_sessions(Config, UserId, [Session1, Session2])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2])),
    ?assert(compare_connections_per_session(Config, Session1, [ServerPid1])),
    ?assert(compare_connections_per_session(Config, Session2, [ServerPid2])),
    ?assert(is_client_alive(ClientPid1)),
    ?assert(is_client_alive(ClientPid2)),

    oz_test_utils:log_out(Config, Cookie3),

    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_session(Config, Session1, [])),
    ?assert(compare_connections_per_session(Config, Session2, [])),
    ?assertNot(is_client_alive(ClientPid1)),
    ?assertNot(is_client_alive(ClientPid2)),

    ok.


%%%===================================================================
%%% Setup/Teardown functions
%%%===================================================================

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    ssl:start(),
    Posthook = fun(NewConfig) ->
        mock_time(NewConfig),
        mock_user_connected_callback(NewConfig),
        NewConfig
    end,
    [{env_up_posthook, Posthook}, {?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(Config) ->
    ssl:stop(),
    unmock_time(Config),
    unmock_user_connected_callback(Config),
    ok.


simulate_time_passing(Config, Seconds) ->
    Nodes = ?config(oz_worker_nodes, Config),
    CurrentTime = rpc:call(hd(Nodes), application, get_env, [?APP_NAME, mocked_time, ?STARTING_TIMESTAMP]),
    rpc:multicall(Nodes, application, set_env, [?APP_NAME, mocked_time, CurrentTime + Seconds]).


mock_time(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    simulate_time_passing(Config, 0),
    ok = test_utils:mock_new(Nodes, time_utils, [passthrough]),
    ok = test_utils:mock_expect(Nodes, time_utils, cluster_time_seconds, fun() ->
        application:get_env(?APP_NAME, mocked_time, ?STARTING_TIMESTAMP)
    end).


unmock_time(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, time_utils).


mock_user_connected_callback(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, gs_logic_plugin, [passthrough]),
    ok = test_utils:mock_expect(Nodes, gs_logic_plugin, client_connected,
        fun(Client, SessionId, ConnectionRef) ->
            {ok, Pid} = application:get_env(?APP_NAME, mocked_pid),
            Pid ! {client_connected, ConnectionRef},
            meck:passthrough([Client, SessionId, ConnectionRef])
        end).


unmock_user_connected_callback(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, gs_logic_plugin).


start_gs_connection(Config, SessionId) ->
    % Prevent exiting GS connections from killing the test master
    process_flag(trap_exit, true),
    Nodes = ?config(oz_worker_nodes, Config),
    rpc:multicall(Nodes, application, set_env, [?APP_NAME, mocked_pid, self()]),
    {ok, ClientPid, #gs_resp_handshake{identity = Identity}} = gs_client:start_link(
        oz_test_utils:get_gs_ws_url(Config),
        {cookie, {?SESSION_COOKIE_KEY, SessionId}},
        oz_test_utils:get_gs_supported_proto_versions(Config),
        fun(_) -> ok end,
        [{cacerts, oz_test_utils:gui_ca_certs(Config)}]
    ),
    ServerPid = receive
        {client_connected, ConnectionRef} ->
            ConnectionRef
    after 5000 ->
            exit(timeout)
    end,
    {ClientPid, ServerPid, Identity}.


log_in(Config, UserId) ->
    {ok, Cookie} = oz_test_utils:log_in(Config, UserId),
    SessionId = rpc:call(random_node(Config), new_gui_session, get_session_id, [Cookie]),
    {SessionId, Cookie}.


validate_session(Config, Cookie) ->
    MockedReq = #{
        resp_headers => #{},
        headers => #{<<"cookie">> => <<?SESSION_COOKIE_KEY/binary, "=", Cookie/binary>>}
    },
    rpc:call(random_node(Config), new_gui_session, validate, [MockedReq]).

is_client_alive(Pid) ->
    is_client_alive(Pid, 10).

is_client_alive(Pid, Retries) ->
    case is_process_alive(Pid) of
        false ->
            false;
        true ->
            Pid ! perform_ping,
            timer:sleep(1000),
            is_client_alive(Pid, Retries - 1)
    end.


compare_user_sessions(Config, UserId, Expected) ->
    ListFun = fun() ->
        {ok, Sessions} = rpc:call(random_node(Config), od_user, get_all_sessions, [UserId]),
        Sessions
    end,
    compare_lists(ListFun, Expected, 60).


compare_connections_per_session(Config, SessionId, Expected) ->
    ListFun = fun() ->
        {ok, Connections} = rpc:call(random_node(Config), user_connections, get_all, [SessionId]),
        Connections
    end,
    compare_lists(ListFun, Expected, 60).


compare_connections_per_user(Config, UserId, Expected) ->
    ListFun = fun() ->
        {ok, Sessions} = rpc:call(random_node(Config), od_user, get_all_sessions, [UserId]),
        lists:flatmap(fun(SessionId) ->
            {ok, Connections} = rpc:call(random_node(Config), user_connections, get_all, [SessionId]),
            Connections
        end, Sessions)
    end,
    compare_lists(ListFun, Expected, 60).

compare_lists(ListFun, Expected, Retries) ->
    Got = ListFun(),
    case lists:sort(Got) =:= lists:sort(Expected) of
        true ->
            true;
        false ->
            case Retries of
                0 ->
                    ct:pal("Compared lists are different: ~n"
                    "     got: ~p~n"
                    "expected: ~p", [
                        Got, Expected
                    ]),
                    false;
                _ ->
                    timer:sleep(1000),
                    compare_lists(ListFun, Expected, Retries - 1)
            end
    end.


random_node(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    lists:nth(rand:uniform(length(Nodes)), Nodes).


get_gui_config(Config, EnvName) ->
    {ok, Val} = rpc:call(random_node(Config), application, get_env, [gui, EnvName]),
    Val.
