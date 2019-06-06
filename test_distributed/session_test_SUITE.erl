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
-include("graph_sync/oz_graph_sync.hrl").
-include("api_test_utils.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/global_definitions.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("gui/include/gui_session.hrl").

-type config() :: [{atom(), term()}].

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
    cleanup_of_expired_sessions_upon_other_session_delete/1,
    create_gui_macaroons/1,
    create_gui_macaroons_via_endpoint/1,
    expiration_of_gui_macaroons/1,
    reuse_or_refresh_gui_macaroons/1,
    cleanup_of_gui_macaroons/1
]).


all() -> ?ALL([
    multiple_connections_per_session,
    multiple_sessions_per_user,
    session_cookie_refresh_and_grace_period,
    cleanup_on_session_expiry,
    cleanup_on_session_delete,
    cleanup_on_user_delete,
    cleanup_of_expired_sessions_upon_other_session_expiry,
    cleanup_of_expired_sessions_upon_other_session_delete,
    create_gui_macaroons,
    create_gui_macaroons_via_endpoint,
    expiration_of_gui_macaroons,
    reuse_or_refresh_gui_macaroons,
    cleanup_of_gui_macaroons
]).


%%%===================================================================
%%% Tests
%%%===================================================================

multiple_connections_per_session(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {SessionId, Cookie}} = oz_test_utils:log_in(Config, UserId),

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
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),

    {_ClientPid1, ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie1),
    {ClientPid2, ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie1),
    {_ClientPid3, ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie1),
    ?assert(compare_user_sessions(Config, UserId, [Session1])),

    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),
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
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {SessionId, Cookie}} = oz_test_utils:log_in(Config, UserId),
    {_ClientPid1, _ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie),
    {_ClientPid2, _ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie),
    {_ClientPid3, _ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie),

    {ok, #gui_session{
        nonce = OldNonce
    }} = rpc:call(random_node(Config), gui_session_plugin, get, [SessionId]),

    oz_test_utils:simulate_time_passing(Config, get_gui_config(Config, session_cookie_refresh_interval) + 1),
    {ok, _, NewCookie, _} = ?assertMatch({ok, _, _, _}, validate_session(Config, Cookie)),
    % Cookie should have been refreshed
    ?assertNotEqual(Cookie, NewCookie),

    {ok, #gui_session{
        nonce = NewNonce,
        previous_nonce = PrevNonce
    }} = rpc:call(random_node(Config), gui_session_plugin, get, [SessionId]),

    ?assertNotEqual(OldNonce, NewNonce),
    ?assertMatch(OldNonce, PrevNonce),

    % After the grace period, the old cookie should stop working
    oz_test_utils:simulate_time_passing(Config, get_gui_config(Config, session_cookie_grace_period) + 1),

    ?assertMatch({error, invalid}, validate_session(Config, Cookie)),
    ?assertMatch(?ERROR_UNAUTHORIZED, start_gs_connection(Config, Cookie)),

    ok.


cleanup_on_session_expiry(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {SessionId, Cookie}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid1, _ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie),
    {ClientPid2, _ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie),
    {ClientPid3, _ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie),

    oz_test_utils:simulate_time_passing(Config, get_gui_config(Config, session_cookie_ttl) + 1),

    ?assertMatch(?ERROR_UNAUTHORIZED, start_gs_connection(Config, Cookie)),

    ?assert(compare_user_sessions(Config, UserId, [])),
    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_session(Config, SessionId, [])),
    ?assertMatch(false, is_process_alive(ClientPid1), 10),
    ?assertMatch(false, is_process_alive(ClientPid2), 10),
    ?assertMatch(false, is_process_alive(ClientPid3), 10),

    ok.


cleanup_on_session_delete(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {SessionId, Cookie}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid1, ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie),
    {ClientPid2, ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie),
    {ClientPid3, ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie),

    % Wait for the connections to initialize
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2, ServerPid3])),

    oz_test_utils:log_out(Config, Cookie),

    ?assert(compare_user_sessions(Config, UserId, [])),
    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_session(Config, SessionId, [])),
    ?assertMatch(false, is_process_alive(ClientPid1), 10),
    ?assertMatch(false, is_process_alive(ClientPid2), 10),
    ?assertMatch(false, is_process_alive(ClientPid3), 10),

    ok.


cleanup_on_user_delete(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session3, Cookie3}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid1, _ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie1),
    {ClientPid2, _ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie2),
    {ClientPid3, _ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie3),

    rpc:call(random_node(Config), od_user, force_delete, [UserId]),
    ?assert(compare_user_sessions(Config, UserId, [])),
    ?assert(compare_connections_per_user(Config, UserId, [])),
    ?assert(compare_connections_per_session(Config, Session1, [])),
    ?assert(compare_connections_per_session(Config, Session2, [])),
    ?assert(compare_connections_per_session(Config, Session3, [])),
    ?assertMatch(false, is_process_alive(ClientPid1), 10),
    ?assertMatch(false, is_process_alive(ClientPid2), 10),
    ?assertMatch(false, is_process_alive(ClientPid3), 10),

    ok.


cleanup_of_expired_sessions_upon_other_session_expiry(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid1, ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie1),
    {ClientPid2, ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie2),

    oz_test_utils:simulate_time_passing(Config, get_gui_config(Config, session_cookie_ttl) + 1),

    {ok, {Session3, Cookie3}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid3, ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie3),

    ?assert(compare_user_sessions(Config, UserId, [Session1, Session2, Session3])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2, ServerPid3])),
    ?assert(compare_connections_per_session(Config, Session1, [ServerPid1])),
    ?assert(compare_connections_per_session(Config, Session2, [ServerPid2])),
    ?assert(compare_connections_per_session(Config, Session3, [ServerPid3])),
    ?assert(is_process_alive(ClientPid1)),
    ?assert(is_process_alive(ClientPid2)),
    ?assert(is_process_alive(ClientPid3)),

    oz_test_utils:simulate_time_passing(Config, get_gui_config(Config, session_cookie_ttl) + 1),

    % The only session that is still valid, it should not be cleared
    {ok, {Session4, Cookie4}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid4, ServerPid4, {user, UserId}} = start_gs_connection(Config, Cookie4),

    ?assertMatch(?ERROR_UNAUTHORIZED, start_gs_connection(Config, Cookie3)),

    ?assert(compare_user_sessions(Config, UserId, [Session4])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid4])),
    ?assert(compare_connections_per_session(Config, Session1, [])),
    ?assert(compare_connections_per_session(Config, Session2, [])),
    ?assert(compare_connections_per_session(Config, Session3, [])),
    ?assert(compare_connections_per_session(Config, Session4, [ServerPid4])),
    ?assertMatch(false, is_process_alive(ClientPid1), 10),
    ?assertMatch(false, is_process_alive(ClientPid2), 10),
    ?assertMatch(false, is_process_alive(ClientPid3), 10),
    ?assert(is_process_alive(ClientPid4)),

    ok.


cleanup_of_expired_sessions_upon_other_session_delete(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid1, ServerPid1, {user, UserId}} = start_gs_connection(Config, Cookie1),
    {ClientPid2, ServerPid2, {user, UserId}} = start_gs_connection(Config, Cookie2),

    oz_test_utils:simulate_time_passing(Config, get_gui_config(Config, session_cookie_ttl) + 1),

    {ok, {Session3, Cookie3}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid3, ServerPid3, {user, UserId}} = start_gs_connection(Config, Cookie3),

    ?assert(compare_user_sessions(Config, UserId, [Session1, Session2, Session3])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid1, ServerPid2, ServerPid3])),
    ?assert(compare_connections_per_session(Config, Session1, [ServerPid1])),
    ?assert(compare_connections_per_session(Config, Session2, [ServerPid2])),
    ?assert(compare_connections_per_session(Config, Session3, [ServerPid3])),
    ?assert(is_process_alive(ClientPid1)),
    ?assert(is_process_alive(ClientPid2)),
    ?assert(is_process_alive(ClientPid3)),

    % The only session that is still valid, it should not be cleared
    {ok, {Session4, Cookie4}} = oz_test_utils:log_in(Config, UserId),
    {ClientPid4, ServerPid4, {user, UserId}} = start_gs_connection(Config, Cookie4),

    oz_test_utils:log_out(Config, Cookie3),

    ?assert(compare_user_sessions(Config, UserId, [Session4])),
    ?assert(compare_connections_per_user(Config, UserId, [ServerPid4])),
    ?assert(compare_connections_per_session(Config, Session1, [])),
    ?assert(compare_connections_per_session(Config, Session2, [])),
    ?assert(compare_connections_per_session(Config, Session3, [])),
    ?assert(compare_connections_per_session(Config, Session4, [ServerPid4])),
    ?assertMatch(false, is_process_alive(ClientPid1), 10),
    ?assertMatch(false, is_process_alive(ClientPid2), 10),
    ?assertMatch(false, is_process_alive(ClientPid3), 10),
    ?assert(is_process_alive(ClientPid4)),

    ok.


create_gui_macaroons(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),

    ?assertMatch({error, not_found}, oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [<<"bad-session">>, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    {ok, {Macaroon1, _}} = ?assertMatch({ok, {_, _}}, oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    {ok, {Macaroon2, _}} = ?assertMatch({ok, {_, _}}, oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session2, ?ONEPROVIDER, <<"1123123">>]
    )),

    ?assertMatch({ok, UserId, Session1}, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    ?assertMatch(?ERROR_MACAROON_INVALID, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon2, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    ?assertMatch(?ERROR_MACAROON_INVALID, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon1, ?ONEPROVIDER, ?ONEZONE_CLUSTER_ID]
    )),
    ?assertMatch(?ERROR_MACAROON_INVALID, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon1, ?ONEPROVIDER, <<"1123123">>]
    )),

    ?assertMatch({ok, UserId, Session2}, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon2, ?ONEPROVIDER, <<"1123123">>]
    )),
    ?assertMatch(?ERROR_MACAROON_INVALID, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon2, ?ONEPROVIDER, <<"zxvcsdgadg">>]
    )),

    oz_test_utils:log_out(Config, Cookie1),
    ?assertMatch(?ERROR_MACAROON_INVALID, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    ?assertMatch({error, not_found}, oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    ?assertMatch({ok, UserId, Session2}, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon2, ?ONEPROVIDER, <<"1123123">>]
    )),

    oz_test_utils:log_out(Config, Cookie2),
    ?assertMatch(?ERROR_MACAROON_INVALID, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon2, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    ?assertMatch({error, not_found}, oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session2, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    ok.


create_gui_macaroons_via_endpoint(Config) ->
    AcquireGuiToken = fun(Cookie, GuiType, ClusterId) ->
        oz_test_utils:acquire_gui_token(Config, Cookie, GuiType, ClusterId)
    end,

    VerifyToken = fun(Macaroon, ClusterType, ClusterId) ->
        oz_test_utils:call_oz(Config, session, verify_gui_macaroon, [Macaroon, ClusterType, ClusterId])
    end,

    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session, Cookie}} = oz_test_utils:log_in(Config, UserId),

    {ok, Token1} = ?assertMatch({ok, _}, AcquireGuiToken(Cookie, ?OZ_WORKER_GUI, ?ONEZONE_CLUSTER_ID)),
    {ok, Macaroon1} = onedata_macaroons:deserialize(Token1),
    ?assertMatch({ok, UserId, Session}, VerifyToken(Macaroon1, ?ONEZONE, ?ONEZONE_CLUSTER_ID)),

    % The user will belong to the cluster as the provider admin
    {ok, {ProviderId, ProviderMacaroon}} = oz_test_utils:create_provider(Config, UserId, <<"provider">>),
    ClusterId = ProviderId,
    % acquire_gui_token takes ClusterId
    % verify_gui_macaroon takes ProviderId

    % The user is a member of provider cluster, but is not supported by the provider,
    % so he can't generate a token for the provider GUI.
    ?assertMatch({ok, _}, AcquireGuiToken(Cookie, ?ONEPANEL_GUI, ClusterId)),
    ?assertMatch(?ERROR_FORBIDDEN, AcquireGuiToken(Cookie, ?OP_WORKER_GUI, ClusterId)),

    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space(Config, ProviderId, Space1),

    % Now it should be possible for the user to generate a token
    {ok, Token2} = ?assertMatch({ok, _}, AcquireGuiToken(Cookie, ?OP_WORKER_GUI, ClusterId)),
    {ok, Macaroon2} = onedata_macaroons:deserialize(Token2),

    ?assertMatch({ok, UserId, Session}, VerifyToken(Macaroon2, ?ONEPROVIDER, ProviderId)),
    ?assertMatch(?ERROR_MACAROON_INVALID, VerifyToken(Macaroon2, ?ONEZONE, ?ONEZONE_CLUSTER_ID)),
    ?assertMatch(?ERROR_MACAROON_INVALID, VerifyToken(Macaroon1, ?ONEPROVIDER, ProviderId)),

    % A user not belonging to the provider/cluster cannot generate GUI tokens for it
    {ok, User2} = oz_test_utils:create_user(Config),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, User2),
    ?assertMatch(?ERROR_FORBIDDEN, AcquireGuiToken(Cookie2, ?OP_WORKER_GUI, ClusterId)),

    % After becoming an effective member of the provider, he can
    {ok, Space2} = oz_test_utils:create_space(Config, ?USER(User2), <<"space">>),
    oz_test_utils:support_space(Config, ProviderId, Space2),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    {ok, Token3} = ?assertMatch({ok, _}, AcquireGuiToken(Cookie2, ?OP_WORKER_GUI, ClusterId)),
    % ... but not for the Onepanel GUI
    ?assertMatch(?ERROR_FORBIDDEN, AcquireGuiToken(Cookie2, ?ONEPANEL_GUI, ClusterId)),
    {ok, Macaroon3} = onedata_macaroons:deserialize(Token3),
    ?assertMatch({ok, User2, Session2}, VerifyToken(Macaroon3, ?ONEPROVIDER, ProviderId)),

    % Tokens can be generated only for existing clusters
    ?assertMatch(?ERROR_MALFORMED_DATA, AcquireGuiToken(Cookie2, ?OP_WORKER_GUI, <<"bad-cluster">>)),

    % Make sure provider gui tokens are properly accepted in REST
    {ok, _, _, UserData} = ?assertMatch({ok, 200, _, _}, http_client:get(
        ?URL(Config, [<<"/user">>]),
        #{
            <<"Subject-Token">> => Token3,
            <<"Audience-Token">> => ProviderMacaroon
        },
        <<"">>,
        [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}]
    )),
    ?assertMatch(#{<<"userId">> := User2}, json_utils:decode(UserData)),
    ok.


expiration_of_gui_macaroons(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, _Cookie2}} = oz_test_utils:log_in(Config, UserId),

    {ok, {Macaroon1, Expires1}} = ?assertMatch({ok, {_, _}}, oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    oz_test_utils:simulate_time_passing(Config, 10),
    {ok, {Macaroon2, _}} = ?assertMatch({ok, {_, _}}, oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session2, ?ONEPROVIDER, <<"1123123">>]
    )),

    ?assertMatch({ok, UserId, Session1}, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    ?assertMatch({ok, UserId, Session2}, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon2, ?ONEPROVIDER, <<"1123123">>]
    )),

    wait_for_expiration(Config, Expires1),
    ?assertMatch(?ERROR_MACAROON_EXPIRED, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    ?assertMatch({ok, UserId, Session2}, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon2, ?ONEPROVIDER, <<"1123123">>]
    )),

    oz_test_utils:simulate_time_passing(Config, 10),
    ?assertMatch(?ERROR_MACAROON_EXPIRED, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    ?assertMatch(?ERROR_MACAROON_EXPIRED, oz_test_utils:call_oz(
        Config, session, verify_gui_macaroon, [Macaroon2, ?ONEPROVIDER, <<"1123123">>]
    )).


reuse_or_refresh_gui_macaroons(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Macaroon, Expires}} = oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    ),
    % The macaroon for the same cluster should be reused if possible
    ?assertMatch({ok, {Macaroon, Expires}}, oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    )),
    % But after a certain point in time, it should be refreshed
    wait_for_refresh_threshold(Config, Expires),
    {ok, {NewMacaroon, _}} = oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    ),
    ?assertNotMatch(NewMacaroon, Macaroon),

    % Make sure that expiration threshold is not longer than the origin macaroon TTL
    {ok, {Macaroon2, Expires2}} = oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    ),

    wait_for_expiration(Config, Expires2),
    {ok, {NewMacaroon2, _}} = oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    ),
    ?assertNotMatch(NewMacaroon2, Macaroon2),

    ok.


cleanup_of_gui_macaroons(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),

    {ok, {Macaroon1, _}} = oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session1, ?ONEZONE, ?ONEZONE_CLUSTER_ID]
    ),
    {ok, {Macaroon2, _}} = oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session1, ?ONEPROVIDER, <<"1123123">>]
    ),
    {ok, {Macaroon3, _}} = oz_test_utils:call_oz(
        Config, session, acquire_gui_macaroon, [Session2, ?ONEPROVIDER, <<"sdafadsf">>]
    ),

    ?assertMatch({ok, _, _}, oz_test_utils:call_oz(
        Config, volatile_macaroon, get, [macaroon:identifier(Macaroon1)])
    ),
    ?assertMatch({ok, _, _}, oz_test_utils:call_oz(
        Config, volatile_macaroon, get, [macaroon:identifier(Macaroon2)])
    ),
    ?assertMatch({ok, _, _}, oz_test_utils:call_oz(
        Config, volatile_macaroon, get, [macaroon:identifier(Macaroon3)])
    ),

    oz_test_utils:log_out(Config, Cookie1),
    ?assertMatch({error, not_found}, oz_test_utils:call_oz(
        Config, volatile_macaroon, get, [macaroon:identifier(Macaroon1)])
    ),
    ?assertMatch({error, not_found}, oz_test_utils:call_oz(
        Config, volatile_macaroon, get, [macaroon:identifier(Macaroon2)])
    ),
    ?assertMatch({ok, _, _}, oz_test_utils:call_oz(
        Config, volatile_macaroon, get, [macaroon:identifier(Macaroon3)])
    ),

    oz_test_utils:log_out(Config, Cookie2),
    ?assertMatch({error, not_found}, oz_test_utils:call_oz(
        Config, volatile_macaroon, get, [macaroon:identifier(Macaroon1)])
    ),
    ?assertMatch({error, not_found}, oz_test_utils:call_oz(
        Config, volatile_macaroon, get, [macaroon:identifier(Macaroon2)])
    ),
    ?assertMatch({error, not_found}, oz_test_utils:call_oz(
        Config, volatile_macaroon, get, [macaroon:identifier(Macaroon3)])
    ),
    ok.


%%%===================================================================
%%% Setup/Teardown functions
%%%===================================================================

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) ->
        oz_test_utils:mock_time(NewConfig),
        mock_user_connected_callback(NewConfig),
        NewConfig
    end,
    [{env_up_posthook, Posthook}, {?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(Config) ->
    hackney:stop(),
    ssl:stop(),
    oz_test_utils:unmock_time(Config),
    unmock_user_connected_callback(Config),
    ok.


mock_user_connected_callback(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, gs_logic_plugin, [passthrough]),
    ok = test_utils:mock_expect(Nodes, gs_logic_plugin, client_connected,
        fun(Client, SessionId, ConnectionRef) ->
            Pid = oz_worker:get_env(mocked_pid),
            Pid ! {client_connected, ConnectionRef},
            meck:passthrough([Client, SessionId, ConnectionRef])
        end).


unmock_user_connected_callback(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, gs_logic_plugin).


start_gs_connection(Config, Cookie) ->
    case oz_test_utils:acquire_gui_token(Config, Cookie) of
        {error, _} = Error ->
            Error;
        {ok, GuiToken} ->
            % Prevent exiting GS connections from killing the test master
            process_flag(trap_exit, true),
            Nodes = ?config(oz_worker_nodes, Config),
            rpc:multicall(Nodes, oz_worker, set_env, [mocked_pid, self()]),
            {ok, ClientPid, #gs_resp_handshake{identity = Identity}} = gs_client:start_link(
                oz_test_utils:graph_sync_url(Config, gui),
                {macaroon, GuiToken, []},
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
            {ClientPid, ServerPid, Identity}
    end.


validate_session(Config, Cookie) ->
    MockedReq = #{
        resp_headers => #{},
        headers => #{<<"cookie">> => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>}
    },
    rpc:call(random_node(Config), gui_session, validate, [MockedReq]).


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
    {ok, Val} = oz_test_utils:call_oz(Config, application, get_env, [gui, EnvName]),
    Val.


wait_for_refresh_threshold(Config, Expires) ->
    oz_test_utils:simulate_time_passing(Config, 1),
    case oz_test_utils:call_oz(Config, macaroon_logic, should_refresh_gui_macaroon, [Expires]) of
        true -> ok;
        false -> wait_for_refresh_threshold(Config, Expires)
    end.


wait_for_expiration(Config, Expires) ->
    CurrentTime = oz_test_utils:get_mocked_time(Config),
    case CurrentTime > Expires of
        true -> ok;
        false -> oz_test_utils:simulate_time_passing(Config, Expires - CurrentTime + 1)
    end.
