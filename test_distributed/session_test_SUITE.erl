%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
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

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-include("api_test_utils.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    multiple_connections_per_session/1,
    multiple_sessions_per_user/1,
    default_user_session/1,
    session_cookie_refresh_and_grace_period/1,
    last_activity_tracking/1,
    cleanup_on_session_expiry/1,
    cleanup_on_session_delete/1,
    cleanup_on_user_delete/1,
    cleanup_on_user_access_block/1,
    cleanup_of_expired_sessions_upon_other_session_expiry/1,
    cleanup_of_expired_sessions_upon_other_session_delete/1
]).


all() -> ?ALL([
    multiple_connections_per_session,
    multiple_sessions_per_user,
    default_user_session,
    session_cookie_refresh_and_grace_period,
    last_activity_tracking,
    cleanup_on_session_expiry,
    cleanup_on_session_delete,
    cleanup_on_user_delete,
    cleanup_on_user_access_block,
    cleanup_of_expired_sessions_upon_other_session_expiry,
    cleanup_of_expired_sessions_upon_other_session_delete
]).


%%%===================================================================
%%% Tests
%%%===================================================================

multiple_connections_per_session(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {SessionId, Cookie}} = oz_test_utils:log_in(Config, UserId),

    ?assert(compare_user_sessions(Config, UserId, [SessionId])),

    {_ClientPid1, ServerPid1, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),
    {ClientPid2, ServerPid2, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),
    {_ClientPid3, ServerPid3, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),
    ?assert(compare_connections_per_session(Config, UserId, SessionId, [ServerPid1, ServerPid2, ServerPid3])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2, ServerPid3])),

    exit(ClientPid2, kill),
    ?assert(compare_connections_per_session(Config, UserId, SessionId, [ServerPid1, ServerPid3])),

    {_ClientPid4, ServerPid4, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),
    ?assert(compare_connections_per_session(Config, UserId, SessionId, [ServerPid1, ServerPid3, ServerPid4])).


multiple_sessions_per_user(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),

    {_ClientPid1, ServerPid1, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie1}),
    {ClientPid2, ServerPid2, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie1}),
    {_ClientPid3, ServerPid3, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie1}),
    ?assert(compare_user_sessions(Config, UserId, [Session1])),

    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),
    {_ClientPid4, ServerPid4, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie2}),
    {_ClientPid5, ServerPid5, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie2}),
    ?assert(compare_user_sessions(Config, UserId, [Session1, Session2])),

    ?assert(compare_connections_per_session(Config, UserId, Session1, [ServerPid1, ServerPid2, ServerPid3])),
    ?assert(compare_connections_per_session(Config, UserId, Session2, [ServerPid4, ServerPid5])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2, ServerPid3, ServerPid4, ServerPid5])),

    exit(ClientPid2, kill),

    ?assert(compare_connections_per_session(Config, UserId, Session1, [ServerPid1, ServerPid3])),
    ?assert(compare_connections_per_session(Config, UserId, Session2, [ServerPid4, ServerPid5])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid3, ServerPid4, ServerPid5])).


default_user_session(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    Token = oz_test_utils:acquire_temporary_token(Config, ?SUB(user, UserId)),
    DefaultUserSession = session:default_user_session(UserId),

    % default user session is not listed
    ?assert(compare_user_sessions(Config, UserId, [])),

    {ClientPid1, ServerPid1, ?SUB(user, UserId)} = start_gs_connection(Config, {token, Token}),
    {ClientPid2, ServerPid2, ?SUB(user, UserId)} = start_gs_connection(Config, {token, Token}),
    {ClientPid3, ServerPid3, ?SUB(user, UserId)} = start_gs_connection(Config, {token, Token}),
    ?assert(compare_connections_per_session(Config, UserId, DefaultUserSession, [ServerPid1, ServerPid2, ServerPid3])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2, ServerPid3])),

    exit(ClientPid2, kill),
    ?assert(compare_connections_per_session(Config, UserId, DefaultUserSession, [ServerPid1, ServerPid3])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid3])),

    {ClientPid4, ServerPid4, ?SUB(user, UserId)} = start_gs_connection(Config, {token, Token}),
    ?assert(compare_connections_per_session(Config, UserId, DefaultUserSession, [ServerPid1, ServerPid3, ServerPid4])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid3, ServerPid4])),

    exit(ClientPid1, kill),
    exit(ClientPid3, kill),
    exit(ClientPid4, kill),
    ?assert(compare_connections_per_session(Config, UserId, DefaultUserSession, [])),
    ?assert(compare_connections_per_user(Config, UserId, [])).



session_cookie_refresh_and_grace_period(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {SessionId, Cookie}} = oz_test_utils:log_in(Config, UserId),
    {_ClientPid1, _ServerPid1, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),
    {_ClientPid2, _ServerPid2, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),
    {_ClientPid3, _ServerPid3, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),

    {ok, #gui_session{
        nonce = OldNonce
    }} = oz_test_utils:call_oz(Config, gui_session_plugin, get, [SessionId]),

    oz_test_utils:simulate_seconds_passing(get_gui_config(Config, session_cookie_refresh_interval) + 1),
    {ok, _, _, Req} = ?assertMatch({ok, UserId, SessionId, _}, validate_session(Config, Cookie)),
    % Cookie should have been refreshed
    NewCookie = oz_test_utils:parse_resp_session_cookie(Req),
    ?assertNotEqual(Cookie, NewCookie),

    {ok, #gui_session{
        nonce = NewNonce,
        previous_nonce = PrevNonce
    }} = oz_test_utils:call_oz(Config, gui_session_plugin, get, [SessionId]),

    ?assertNotEqual(OldNonce, NewNonce),
    ?assertMatch(OldNonce, PrevNonce),

    % After the grace period, the old cookie should stop working
    oz_test_utils:simulate_seconds_passing(get_gui_config(Config, session_cookie_grace_period) + 1),

    ?assertMatch({error, invalid}, validate_session(Config, Cookie)),
    ?assertMatch(?ERROR_UNAUTHORIZED, start_gs_connection(Config, {cookie, Cookie})).


last_activity_tracking(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    Token = oz_test_utils:acquire_temporary_token(Config, ?SUB(user, UserId)),
    ?assertEqual(0, oz_test_utils:call_oz(Config, user_connections, get_last_activity, [UserId])),

    {ok, {_, Cookie}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid1, _, ?SUB(user, UserId)} = start_gs_connection(Config, {token, Token}),
    ?assertEqual(now, oz_test_utils:call_oz(Config, user_connections, get_last_activity, [UserId])),
    {ClientPid2, _, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),
    ?assertEqual(now, oz_test_utils:call_oz(Config, user_connections, get_last_activity, [UserId])),
    {ClientPid3, _, ?SUB(user, UserId)} = start_gs_connection(Config, {token, Token}),
    ?assertEqual(now, oz_test_utils:call_oz(Config, user_connections, get_last_activity, [UserId])),
    exit(ClientPid1, kill),
    exit(ClientPid2, kill),
    oz_test_utils:simulate_seconds_passing(54),
    exit(ClientPid3, kill),
    TimestampAlpha = oz_test_utils:get_frozen_time_seconds(),
    ?assertMatch(
        TimestampAlpha,
        oz_test_utils:call_oz(Config, user_connections, get_last_activity, [UserId]),
        60
    ),
    oz_test_utils:simulate_seconds_passing(22),
    {ClientPid4, _, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),
    ?assertEqual(now, oz_test_utils:call_oz(Config, user_connections, get_last_activity, [UserId])),
    exit(ClientPid4, kill),
    TimestampBeta = oz_test_utils:get_frozen_time_seconds(),
    ?assertMatch(
        TimestampBeta,
        oz_test_utils:call_oz(Config, user_connections, get_last_activity, [UserId]),
        60
    ).


cleanup_on_session_expiry(Config) ->
    session_cleanup_test_base(Config, fun(_Cookie) ->
        oz_test_utils:simulate_seconds_passing(get_gui_config(Config, session_cookie_ttl) + 1)
    end).


cleanup_on_session_delete(Config) ->
    session_cleanup_test_base(Config, fun(Cookie) ->
        oz_test_utils:log_out(Config, Cookie)
    end).


session_cleanup_test_base(Config, CauseCleanupFun) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {SessionId, Cookie}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid1, ServerPid1, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),
    {ClientPid2, ServerPid2, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),
    {ClientPid3, ServerPid3, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie}),

    % Wait for the connections to initialize
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2, ServerPid3])),

    CauseCleanupFun(Cookie),

    ?assertMatch(?ERROR_UNAUTHORIZED, start_gs_connection(Config, {cookie, Cookie})),

    ?assert(compare_user_sessions(Config, UserId, [])),
    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_session(Config, UserId, SessionId, [])),
    ?assertMatch(false, is_process_alive(ClientPid1), 10),
    ?assertMatch(false, is_process_alive(ClientPid2), 10),
    ?assertMatch(false, is_process_alive(ClientPid3), 10).


cleanup_on_user_delete(Config) ->
    user_cleanup_test_base(Config, fun(UserId) ->
        oz_test_utils:call_oz(Config, user_logic, delete, [?ROOT, UserId])
    end).


cleanup_on_user_access_block(Config) ->
    user_cleanup_test_base(Config, fun(UserId) ->
        oz_test_utils:toggle_user_access_block(Config, UserId, true)
    end).


user_cleanup_test_base(Config, CauseCleanupFun) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session3, Cookie3}} = oz_test_utils:log_in(Config, UserId),
    Token = oz_test_utils:acquire_temporary_token(Config, ?SUB(user, UserId)),
    DefaultUserSession = session:default_user_session(UserId),

    {ClientPid1, _ServerPid1, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie1}),
    {ClientPid2, _ServerPid2, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie2}),
    {ClientPid3, _ServerPid3, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie3}),
    {ClientPid4, _ServerPid4, ?SUB(user, UserId)} = start_gs_connection(Config, {token, Token}),

    CauseCleanupFun(UserId),

    ?assert(compare_user_sessions(Config, UserId, [])),
    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_session(Config, UserId, Session1, [])),
    ?assert(compare_connections_per_session(Config, UserId, Session2, [])),
    ?assert(compare_connections_per_session(Config, UserId, Session3, [])),
    ?assert(compare_connections_per_session(Config, UserId, DefaultUserSession, [])),
    ?assertMatch(false, is_process_alive(ClientPid1), 10),
    ?assertMatch(false, is_process_alive(ClientPid2), 10),
    ?assertMatch(false, is_process_alive(ClientPid3), 10),
    ?assertMatch(false, is_process_alive(ClientPid4), 10).


cleanup_of_expired_sessions_upon_other_session_expiry(Config) ->
    expired_sessions_cleanup_test_base(Config, fun() ->
        oz_test_utils:simulate_seconds_passing(get_gui_config(Config, session_cookie_ttl) + 1)
    end, fun(Cookie) ->
        % due to the adjusted time, this cookie is already expired and an attempt
        % to connect should cause a cleanup of expired sessions
        ?assertMatch(?ERROR_UNAUTHORIZED, start_gs_connection(Config, {cookie, Cookie}))
    end).


cleanup_of_expired_sessions_upon_other_session_delete(Config) ->
    expired_sessions_cleanup_test_base(Config, fun() ->
        ok
    end, fun(Cookie) ->
        oz_test_utils:log_out(Config, Cookie)
    end).


expired_sessions_cleanup_test_base(Config, AdjustTimeFun, CauseCleanupFun) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid1, ServerPid1, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie1}),
    {ClientPid2, ServerPid2, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie2}),

    oz_test_utils:simulate_seconds_passing(get_gui_config(Config, session_cookie_ttl) + 1),

    {ok, {Session3, Cookie3}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid3, ServerPid3, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie3}),

    ?assert(compare_user_sessions(Config, UserId, [Session1, Session2, Session3])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2, ServerPid3])),
    ?assert(compare_connections_per_session(Config, UserId, Session1, [ServerPid1])),
    ?assert(compare_connections_per_session(Config, UserId, Session2, [ServerPid2])),
    ?assert(compare_connections_per_session(Config, UserId, Session3, [ServerPid3])),
    ?assert(is_process_alive(ClientPid1)),
    ?assert(is_process_alive(ClientPid2)),
    ?assert(is_process_alive(ClientPid3)),

    AdjustTimeFun(),

    % The only session that is still valid, it should not be cleared
    {ok, {Session4, Cookie4}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid4, ServerPid4, ?SUB(user, UserId)} = start_gs_connection(Config, {cookie, Cookie4}),

    CauseCleanupFun(Cookie3),

    ?assert(compare_user_sessions(Config, UserId, [Session4])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid4])),
    ?assert(compare_connections_per_session(Config, UserId, Session1, [])),
    ?assert(compare_connections_per_session(Config, UserId, Session2, [])),
    ?assert(compare_connections_per_session(Config, UserId, Session3, [])),
    ?assert(compare_connections_per_session(Config, UserId, Session4, [ServerPid4])),
    ?assertMatch(false, is_process_alive(ClientPid1), 10),
    ?assertMatch(false, is_process_alive(ClientPid2), 10),
    ?assertMatch(false, is_process_alive(ClientPid3), 10),
    ?assert(is_process_alive(ClientPid4)).

%%%===================================================================
%%% Setup/Teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    Posthook = fun(NewConfig) ->
        oz_test_utils:freeze_time(NewConfig),
        mock_user_connected_callback(NewConfig),
        NewConfig
    end,
    [{env_up_posthook, Posthook}, {?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(Config) ->
    application:stop(hackney),
    ssl:stop(),
    oz_test_utils:unfreeze_time(Config),
    unmock_user_connected_callback(Config),
    ok.


mock_user_connected_callback(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, gs_logic_plugin, [passthrough]),
    ok = test_utils:mock_expect(Nodes, gs_logic_plugin, client_connected,
        fun(Auth, ConnectionRef) ->
            Pid = oz_worker:get_env(mocked_pid),
            Pid ! {client_connected, ConnectionRef},
            meck:passthrough([Auth, ConnectionRef])
        end).


unmock_user_connected_callback(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, gs_logic_plugin).


start_gs_connection(Config, {cookie, Cookie}) ->
    case oz_test_utils:request_gui_token(Config, Cookie) of
        {error, _} = Error ->
            Error;
        {ok, GuiToken} ->
            start_gs_connection(Config, GuiToken, Cookie)
    end;
start_gs_connection(Config, {token, Token}) ->
    start_gs_connection(Config, Token, undefined).

start_gs_connection(Config, Token, Cookie) ->
    % Prevent exiting GS connections from killing the test master
    process_flag(trap_exit, true),
    Nodes = ?config(oz_worker_nodes, Config),
    rpc:multicall(Nodes, oz_worker, set_env, [mocked_pid, self()]),
    {ok, ClientPid, #gs_resp_handshake{identity = Identity}} = gs_client:start_link(
        oz_test_utils:graph_sync_url(Config, gui),
        case Cookie of
            undefined -> {token, Token};
            _ -> {with_http_cookies, {token, Token}, [{?SESSION_COOKIE_KEY, Cookie}]}
        end,
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


validate_session(Config, Cookie) ->
    MockedReq = #{
        resp_headers => #{},
        headers => #{?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>}
    },
    oz_test_utils:call_oz(Config, gui_session, validate, [MockedReq]).


compare_user_sessions(Config, UserId, Expected) ->
    ListFun = fun() ->
        oz_test_utils:call_oz(Config, od_user, get_all_sessions, [UserId])
    end,
    compare_lists(ListFun, Expected, 60).


compare_connections_per_session(Config, UserId, SessionId, Expected) ->
    ListFun = fun() ->
        oz_test_utils:call_oz(Config, user_connections, get_all, [UserId, SessionId])
    end,
    compare_lists(ListFun, Expected, 60).


compare_connections_per_user(Config, UserId, Expected) ->
    ListFun = fun() ->
        Sessions = oz_test_utils:call_oz(Config, od_user, get_all_sessions, [UserId]),
        DefaultUserSession = session:default_user_session(UserId),
        lists:flatmap(fun(SessionId) ->
            oz_test_utils:call_oz(Config, user_connections, get_all, [UserId, SessionId])
        end, [DefaultUserSession | Sessions])
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


get_gui_config(Config, EnvName) ->
    {ok, Val} = oz_test_utils:call_oz(Config, application, get_env, [gui, EnvName]),
    Val.
