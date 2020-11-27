%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains test concerning different types of tokens.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_tokens_test_SUITE).
-author("Lukasz Opiola").

-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").

-include("api_test_utils.hrl").

-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    gui_tokens_are_bound_to_specific_service/1,
    gui_tokens_for_onezone_work_only_with_session_cookie/1,
    gui_tokens_have_limited_api_power/1,
    gui_tokens_can_be_created_via_endpoint/1,
    gui_tokens_are_accepted_from_legacy_providers/1,
    gui_tokens_expire/1,
    gui_tokens_are_invalidated_upon_logout/1,
    gui_tokens_are_invalidated_when_member_leaves_a_service/1,
    gui_tokens_are_invalidated_upon_temporary_token_secret_change/1,
    gui_tokens_are_invalidated_when_user_is_deleted/1
]).

all() ->
    ?ALL([
        gui_tokens_are_bound_to_specific_service,
        gui_tokens_for_onezone_work_only_with_session_cookie,
        gui_tokens_have_limited_api_power,
        gui_tokens_can_be_created_via_endpoint,
        gui_tokens_are_accepted_from_legacy_providers,
        gui_tokens_expire,
        gui_tokens_are_invalidated_upon_logout,
        gui_tokens_are_invalidated_when_member_leaves_a_service,
        gui_tokens_are_invalidated_upon_temporary_token_secret_change,
        gui_tokens_are_invalidated_when_user_is_deleted
    ]).

-define(EXP_AUTH(UserId, SessionId), #auth{
    subject = ?SUB(user, UserId), session_id = SessionId
}).
-define(OZW_SRV(ServiceId), ?SERVICE(?OZ_WORKER, ServiceId)).
-define(OZP_SRV(ServiceId), ?SERVICE(?OZ_PANEL, ServiceId)).
-define(OPW_SRV(ServiceId), ?SERVICE(?OP_WORKER, ServiceId)).
-define(OPP_SRV(ServiceId), ?SERVICE(?OP_PANEL, ServiceId)).

-define(assertUnverifiedService(ExpService, Term), ?assertEqual(
    ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_service{whitelist = [ExpService]})),
    Term
)).

%%%===================================================================
%%% Test functions
%%%===================================================================

gui_tokens_are_bound_to_specific_service(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, _Cookie2}} = oz_test_utils:log_in(Config, UserId),

    % Tokens can be created only for existing sessions
    ?assertEqual(
        ?ERROR_TOKEN_SESSION_INVALID,
        create_access_token_for_gui(Config, UserId, <<"bad-session">>, ?OZW_SRV(?ONEZONE_CLUSTER_ID))
    ),

    % All users are allowed to create tokens for oz-worker
    {ok, {Token1, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZW_SRV(?ONEZONE_CLUSTER_ID)),
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session1)},
        verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))
    ),
    % undefined service defaults to oz-worker service
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session1)},
        verify_token(Config, Token1, undefined)
    ),

    ?assertUnverifiedService(?OZW_SRV(?ONEZONE_CLUSTER_ID), verify_token(Config, Token1, ?OZP_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertUnverifiedService(?OZW_SRV(?ONEZONE_CLUSTER_ID), verify_token(Config, Token1, ?OPW_SRV(<<"p1-a">>))),
    ?assertUnverifiedService(?OZW_SRV(?ONEZONE_CLUSTER_ID), verify_token(Config, Token1, ?OPP_SRV(<<"p1-a">>))),

    % Only users supported by a provider can create tokens for op-worker
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config),

    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPW_SRV(<<"non-existent">>)),
        create_access_token_for_gui(Config, UserId, Session2, ?OPW_SRV(<<"non-existent">>))
    ),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPW_SRV(ProviderId)),
        create_access_token_for_gui(Config, UserId, Session2, ?OPW_SRV(ProviderId))
    ),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space_by_provider(Config, ProviderId, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, {Token2, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OPW_SRV(ProviderId)),
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session2)},
        verify_token(Config, Token2, ?OPW_SRV(ProviderId))
    ),
    ?assertUnverifiedService(?OPW_SRV(ProviderId), verify_token(Config, Token2, ?OZW_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertUnverifiedService(?OPW_SRV(ProviderId), verify_token(Config, Token2, ?OZP_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertUnverifiedService(?OPW_SRV(ProviderId), verify_token(Config, Token2, ?OPP_SRV(ProviderId))),
    ?assertUnverifiedService(?OPW_SRV(ProviderId), verify_token(Config, Token2, undefined)),

    % Only members of given cluster can generate tokens for oz/op-panel
    OzClusterId = ?ONEZONE_CLUSTER_ID,
    OpClusterId = ProviderId,

    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OZP_SRV(OzClusterId)),
        create_access_token_for_gui(Config, UserId, Session2, ?OZP_SRV(OzClusterId))
    ),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPP_SRV(OpClusterId)),
        create_access_token_for_gui(Config, UserId, Session1, ?OPP_SRV(OpClusterId))
    ),
    oz_test_utils:cluster_add_user(Config, OzClusterId, UserId),
    {ok, {Token3, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OZP_SRV(OzClusterId)),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPP_SRV(OpClusterId)),
        create_access_token_for_gui(Config, UserId, Session1, ?OPP_SRV(OpClusterId))
    ),
    oz_test_utils:cluster_add_user(Config, OpClusterId, UserId),
    {ok, {Token4, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OPP_SRV(OpClusterId)),

    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session2)},
        verify_token(Config, Token3, ?OZP_SRV(OzClusterId))
    ),
    ?assertUnverifiedService(?OZP_SRV(OzClusterId), verify_token(Config, Token3, ?OZW_SRV(OzClusterId))),
    ?assertUnverifiedService(?OZP_SRV(OzClusterId), verify_token(Config, Token3, ?OPW_SRV(OpClusterId))),
    ?assertUnverifiedService(?OZP_SRV(OzClusterId), verify_token(Config, Token3, ?OPP_SRV(OpClusterId))),
    ?assertUnverifiedService(?OZP_SRV(OzClusterId), verify_token(Config, Token3, undefined)),

    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session1)},
        verify_token(Config, Token4, ?OPP_SRV(OpClusterId))
    ),
    ?assertUnverifiedService(?OPP_SRV(OpClusterId), verify_token(Config, Token4, ?OZW_SRV(OzClusterId))),
    ?assertUnverifiedService(?OPP_SRV(OpClusterId), verify_token(Config, Token4, ?OZP_SRV(OzClusterId))),
    ?assertUnverifiedService(?OPP_SRV(OpClusterId), verify_token(Config, Token4, ?OPW_SRV(OpClusterId))),
    ?assertUnverifiedService(?OPP_SRV(OpClusterId), verify_token(Config, Token4, undefined)).


gui_tokens_for_onezone_work_only_with_session_cookie(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {SessionId, Cookie}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Token, _}} = create_access_token_for_gui(Config, UserId, SessionId, ?OZW_SRV(?ONEZONE_CLUSTER_ID)),
    {ok, SerializedToken} = tokens:serialize(Token),

    % GS or REST requests with the token should work only if the cookie is provided
    ?assertEqual(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SESSION_INVALID), connect_via_graph_sync(
        Config, Token, []
    )),
    ?assertMatch({ok, _, #gs_resp_handshake{identity = ?SUB(user, UserId)}}, connect_via_graph_sync(
        Config, Token, [{?SESSION_COOKIE_KEY, Cookie}]
    )),

    ?assertMatch({ok, 401, _, _}, get_user_info_via_rest(Config, #{
        ?HDR_X_AUTH_TOKEN => SerializedToken
    })),
    ?assertMatch({ok, 200, _, _}, get_user_info_via_rest(Config, #{
        ?HDR_X_AUTH_TOKEN => SerializedToken,
        ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>
    })).


gui_tokens_have_limited_api_power(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {SessionId, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {OpClusterId, _}} = oz_test_utils:create_provider(Config),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId)),
    OzClusterId = ?ONEZONE_CLUSTER_ID,
    oz_test_utils:support_space_by_provider(Config, OpClusterId, SpaceId),
    oz_test_utils:cluster_add_user(Config, OpClusterId, UserId),
    oz_test_utils:cluster_add_user(Config, OzClusterId, UserId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    GetUser = fun(Auth) ->
        oz_test_utils:call_oz(Config, user_logic, get, [Auth, UserId])
    end,
    RenameUser = fun(Auth) ->
        oz_test_utils:call_oz(Config, user_logic, update_full_name, [Auth, UserId, <<"new name">>])
    end,
    DeleteUser = fun(Auth) ->
        oz_test_utils:call_oz(Config, user_logic, delete, [Auth, UserId])
    end,
    CallApiWithGuiToken = fun(Service, CallApiFun) ->
        {ok, {Token, _}} = create_access_token_for_gui(Config, UserId, SessionId, Service),
        {true, Auth} = verify_token(Config, Token, Service),
        CallApiFun(Auth)
    end,

    % Different services have different available API, imposed by the service caveat
    ?assertMatch({ok, _}, CallApiWithGuiToken(?OZW_SRV(OzClusterId), GetUser)),
    ?assertMatch({ok, _}, CallApiWithGuiToken(?OZP_SRV(OzClusterId), GetUser)),
    ?assertMatch({ok, _}, CallApiWithGuiToken(?OPW_SRV(OpClusterId), GetUser)),
    ?assertMatch({ok, _}, CallApiWithGuiToken(?OPP_SRV(OpClusterId), GetUser)),

    ?assertMatch(ok, CallApiWithGuiToken(?OZW_SRV(OzClusterId), RenameUser)),
    ?assertUnverifiedService(?OZP_SRV(OzClusterId), CallApiWithGuiToken(?OZP_SRV(OzClusterId), RenameUser)),
    ?assertUnverifiedService(?OPW_SRV(OpClusterId), CallApiWithGuiToken(?OPW_SRV(OpClusterId), RenameUser)),
    ?assertUnverifiedService(?OPP_SRV(OpClusterId), CallApiWithGuiToken(?OPP_SRV(OpClusterId), RenameUser)),


    ?assertUnverifiedService(?OZP_SRV(OzClusterId), CallApiWithGuiToken(?OZP_SRV(OzClusterId), DeleteUser)),
    ?assertUnverifiedService(?OPW_SRV(OpClusterId), CallApiWithGuiToken(?OPW_SRV(OpClusterId), DeleteUser)),
    ?assertUnverifiedService(?OPP_SRV(OpClusterId), CallApiWithGuiToken(?OPP_SRV(OpClusterId), DeleteUser)),
    % Check oz_worker service at the end as this operation deletes the user
    ?assertMatch(ok, CallApiWithGuiToken(?OZW_SRV(OzClusterId), DeleteUser)).


gui_tokens_can_be_created_via_endpoint(Config) ->
    AcquireGuiToken = fun(Cookie, GuiType, ClusterId) ->
        oz_test_utils:request_gui_token(Config, Cookie, GuiType, ClusterId)
    end,

    {ok, User1} = oz_test_utils:create_user(Config),
    {ok, {SessionU1, CookieU1}} = oz_test_utils:log_in(Config, User1),

    {ok, OzwTokenU1Serialized} = ?assertMatch({ok, _}, AcquireGuiToken(CookieU1, ?OZ_WORKER_GUI, ?ONEZONE_CLUSTER_ID)),
    {ok, OzwTokenU1} = tokens:deserialize(OzwTokenU1Serialized),
    ?assertMatch(
        {true, ?EXP_AUTH(User1, SessionU1)},
        verify_token(Config, OzwTokenU1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))
    ),

    % The user will belong to the cluster as the provider admin
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, User1, ?UNIQUE_STRING),

    % The user is a member of provider cluster, but is not supported by the provider,
    % so he can't generate a token for the provider GUI.
    ?assertMatch({ok, _}, AcquireGuiToken(CookieU1, ?ONEPANEL_GUI, ProviderId)),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_WORKER, ProviderId)),
        AcquireGuiToken(CookieU1, ?OP_WORKER_GUI, ProviderId)
    ),

    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(User1), ?UNIQUE_STRING),
    oz_test_utils:support_space_by_provider(Config, ProviderId, Space1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % Now it should be possible for the user to generate a token
    {ok, OpwTokenU1Serialized} = ?assertMatch({ok, _}, AcquireGuiToken(CookieU1, ?OP_WORKER_GUI, ProviderId)),
    {ok, OpwTokenU1} = tokens:deserialize(OpwTokenU1Serialized),

    ?assertMatch(
        {true, ?EXP_AUTH(User1, SessionU1)},
        verify_token(Config, OpwTokenU1, ?OPW_SRV(ProviderId))
    ),
    ?assertUnverifiedService(?OPW_SRV(ProviderId), verify_token(Config, OpwTokenU1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertUnverifiedService(?OZW_SRV(?ONEZONE_CLUSTER_ID), verify_token(Config, OzwTokenU1, ?OPW_SRV(ProviderId))),

    % A user not belonging to the provider/cluster cannot generate GUI tokens for it
    {ok, User2} = oz_test_utils:create_user(Config),
    {ok, {SessionU2, CookieU2}} = oz_test_utils:log_in(Config, User2),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_WORKER, ProviderId)),
        AcquireGuiToken(CookieU2, ?OP_WORKER_GUI, ProviderId)
    ),

    % After becoming an effective member of the provider, he can
    {ok, Space2} = oz_test_utils:create_space(Config, ?USER(User2), <<"space">>),
    oz_test_utils:support_space_by_provider(Config, ProviderId, Space2),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, OpwTokenU2Serialized} = ?assertMatch({ok, _}, AcquireGuiToken(CookieU2, ?OP_WORKER_GUI, ProviderId)),
    % ... but not for the Onepanel GUI
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId)),
        AcquireGuiToken(CookieU2, ?ONEPANEL_GUI, ProviderId)
    ),
    {ok, OpwTokenU2} = tokens:deserialize(OpwTokenU2Serialized),
    ?assertMatch(
        {true, ?EXP_AUTH(User2, SessionU2)},
        verify_token(Config, OpwTokenU2, ?OPW_SRV(ProviderId))
    ),

    % Tokens can be generated only for existing clusters
    ?assertMatch(?ERROR_NOT_FOUND, AcquireGuiToken(CookieU2, ?OP_WORKER_GUI, <<"bad-cluster">>)),

    % Make sure provider gui tokens are properly accepted in REST
    {ok, ProviderIdentityToken} = oz_test_utils:call_oz(Config, token_logic, create_provider_temporary_token, [
        ?ROOT, ProviderId, #{
            <<"type">> => ?IDENTITY_TOKEN,
            <<"caveats">> => [#cv_time{valid_until = oz_test_utils:timestamp_seconds(Config) + 36000}]
        }
    ]),
    {ok, SerializedProviderIdentityToken} = tokens:serialize(ProviderIdentityToken),
    {ok, _, _, UserData} = ?assertMatch({ok, 200, _, _}, get_user_info_via_rest(Config, #{
        ?HDR_X_AUTH_TOKEN => OpwTokenU1Serialized,
        ?HDR_X_ONEDATA_SERVICE_TOKEN => tokens:add_oneprovider_service_indication(?OP_WORKER, SerializedProviderIdentityToken),
        ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", CookieU1/binary>>
    })),
    ?assertMatch(#{<<"userId">> := User1}, json_utils:decode(UserData)).


gui_tokens_are_accepted_from_legacy_providers(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {SessionId, Cookie}} = oz_test_utils:log_in(Config, UserId),
    % The user will belong to the cluster as the provider admin
    {ok, {ProviderId, ProviderRootToken}} = oz_test_utils:create_provider(Config, UserId, ?UNIQUE_STRING),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space_by_provider(Config, ProviderId, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, {OpwToken, _}} = create_access_token_for_gui(Config, UserId, SessionId, ?OPW_SRV(ProviderId)),
    {ok, {OppToken, _}} = create_access_token_for_gui(Config, UserId, SessionId, ?OPP_SRV(ProviderId)),
    {ok, OpwTokenSerialized} = tokens:serialize(OpwToken),
    {ok, OppTokenSerialized} = tokens:serialize(OppToken),

    %% @todo VFS-6098 for backward compatibility - legacy provider access tokens should be
    %% accepted as identity tokens for backward compatibility with old providers
    LegacyProviderToken = oz_test_utils:create_legacy_access_token(Config, ?SUB(?ONEPROVIDER, ProviderId)),
    LegacyProviderTokenAuthNone = oz_test_utils:confine_token_with_legacy_auth_none_caveat(LegacyProviderToken),
    {ok, SerializedLegacyAuthNone} = tokens:serialize(LegacyProviderTokenAuthNone),
    {ok, _, _, UserData} = ?assertMatch({ok, 200, _, _}, get_user_info_via_rest(Config, #{
        ?HDR_X_AUTH_TOKEN => OpwTokenSerialized,
        ?HDR_X_ONEDATA_SERVICE_TOKEN => tokens:add_oneprovider_service_indication(?OP_WORKER, SerializedLegacyAuthNone),
        ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>
    })),
    ?assertMatch(#{<<"userId">> := UserId}, json_utils:decode(UserData)),

    ?assertMatch({ok, 200, _, UserData}, get_user_info_via_rest(Config, #{
        ?HDR_X_AUTH_TOKEN => OppTokenSerialized,
        ?HDR_X_ONEDATA_SERVICE_TOKEN => tokens:add_oneprovider_service_indication(?OP_PANEL, SerializedLegacyAuthNone),
        ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>
    })),

    %% @todo VFS-6098 for backward compatibility - when a legacy provider is
    %% registered in a modern zone, it gets a modern token but uses it in legacy
    %% way (presenting the access token as its identity proof) - this should
    %% also be supported
    ?assertMatch({ok, 200, _, UserData}, get_user_info_via_rest(Config, #{
        ?HDR_X_AUTH_TOKEN => OpwTokenSerialized,
        ?HDR_X_ONEDATA_SERVICE_TOKEN => tokens:add_oneprovider_service_indication(?OP_WORKER, ProviderRootToken),
        ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>
    })),
    ?assertMatch({ok, 200, _, UserData}, get_user_info_via_rest(Config, #{
        ?HDR_X_AUTH_TOKEN => OppTokenSerialized,
        ?HDR_X_ONEDATA_SERVICE_TOKEN => tokens:add_oneprovider_service_indication(?OP_PANEL, ProviderRootToken),
        ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>
    })).


gui_tokens_expire(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, _Cookie2}} = oz_test_utils:log_in(Config, UserId),
    ProviderId = create_provider_supporting_user(Config, UserId),

    {ok, {Token1, Ttl1}} = create_access_token_for_gui(Config, UserId, Session1, ?OZW_SRV(?ONEZONE_CLUSTER_ID)),
    ValidUntil1 = oz_test_utils:get_frozen_time_seconds() + Ttl1,
    oz_test_utils:simulate_seconds_passing(10),
    {ok, {Token2, Ttl2}} = create_access_token_for_gui(Config, UserId, Session2, ?OPW_SRV(ProviderId)),
    ValidUntil2 = oz_test_utils:get_frozen_time_seconds() + Ttl2,

    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session1)},
        verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))
    ),
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session2)},
        verify_token(Config, Token2, ?OPW_SRV(ProviderId))
    ),

    oz_test_utils:simulate_seconds_passing(Ttl1 - 10 + 1),

    ?assertEqual(
        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = ValidUntil1})),
        verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))
    ),
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session2)},
        verify_token(Config, Token2, ?OPW_SRV(ProviderId))
    ),

    oz_test_utils:simulate_seconds_passing(10),
    ?assertEqual(
        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = ValidUntil1})),
        verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))
    ),
    ?assertEqual(
        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = ValidUntil2})),
        verify_token(Config, Token2, ?OPW_SRV(ProviderId))
    ).


gui_tokens_are_invalidated_upon_logout(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),

    ProviderId = create_provider_supporting_user(Config, UserId),
    oz_test_utils:cluster_add_user(Config, ProviderId, UserId),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, UserId),

    {ok, {Token1, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZW_SRV(?ONEZONE_CLUSTER_ID)),
    {ok, {Token2, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZP_SRV(?ONEZONE_CLUSTER_ID)),
    {ok, {Token3, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OPW_SRV(ProviderId)),
    {ok, {Token4, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OPP_SRV(ProviderId)),

    oz_test_utils:log_out(Config, Cookie1),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SESSION_INVALID), verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SESSION_INVALID), verify_token(Config, Token2, ?OZP_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SESSION_INVALID), verify_token(Config, Token3, ?OPW_SRV(ProviderId))),
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session2)},
        verify_token(Config, Token4, ?OPP_SRV(ProviderId))
    ),

    oz_test_utils:log_out(Config, Cookie2),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SESSION_INVALID), verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SESSION_INVALID), verify_token(Config, Token2, ?OZP_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SESSION_INVALID), verify_token(Config, Token3, ?OPW_SRV(ProviderId))),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SESSION_INVALID), verify_token(Config, Token4, ?OPP_SRV(ProviderId))).


gui_tokens_are_invalidated_when_member_leaves_a_service(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session, _Cookie}} = oz_test_utils:log_in(Config, UserId),

    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config),
    {ok, StorageId} = oz_test_utils:create_storage(Config, ?PROVIDER(ProviderId), ?STORAGE_NAME1),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    {ok, SpaceId} = oz_test_utils:support_space(Config, ?PROVIDER(ProviderId), StorageId, SpaceId),
    oz_test_utils:cluster_add_user(Config, ProviderId, UserId),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, UserId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, {Token1, _}} = create_access_token_for_gui(Config, UserId, Session, ?OZW_SRV(?ONEZONE_CLUSTER_ID)),
    {ok, {Token2, _}} = create_access_token_for_gui(Config, UserId, Session, ?OZP_SRV(?ONEZONE_CLUSTER_ID)),
    {ok, {Token3, _}} = create_access_token_for_gui(Config, UserId, Session, ?OPW_SRV(ProviderId)),
    {ok, {Token4, _}} = create_access_token_for_gui(Config, UserId, Session, ?OPP_SRV(ProviderId)),

    oz_test_utils:unsupport_space(Config, StorageId, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertMatch({true, _}, verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch({true, _}, verify_token(Config, Token2, ?OZP_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(
        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPW_SRV(ProviderId))),
        verify_token(Config, Token3, ?OPW_SRV(ProviderId))
    ),
    ?assertMatch({true, _}, verify_token(Config, Token4, ?OPP_SRV(ProviderId))),

    oz_test_utils:cluster_remove_user(Config, ?ONEZONE_CLUSTER_ID, UserId),
    ?assertMatch({true, _}, verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(
        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SERVICE_FORBIDDEN(?OZP_SRV(?ONEZONE_CLUSTER_ID))),
        verify_token(Config, Token2, ?OZP_SRV(?ONEZONE_CLUSTER_ID))
    ),
    ?assertMatch(
        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPW_SRV(ProviderId))),
        verify_token(Config, Token3, ?OPW_SRV(ProviderId))
    ),
    ?assertMatch({true, _}, verify_token(Config, Token4, ?OPP_SRV(ProviderId))),

    oz_test_utils:cluster_remove_user(Config, ProviderId, UserId),
    ?assertMatch({true, _}, verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(
        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SERVICE_FORBIDDEN(?OZP_SRV(?ONEZONE_CLUSTER_ID))),
        verify_token(Config, Token2, ?OZP_SRV(?ONEZONE_CLUSTER_ID))
    ),
    ?assertMatch(
        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPW_SRV(ProviderId))),
        verify_token(Config, Token3, ?OPW_SRV(ProviderId))
    ),
    ?assertMatch(
        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPP_SRV(ProviderId))),
        verify_token(Config, Token4, ?OPP_SRV(ProviderId))
    ).


gui_tokens_are_invalidated_upon_temporary_token_secret_change(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, AnotherUser} = oz_test_utils:create_user(Config),
    {ok, {Session1, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, _Cookie2}} = oz_test_utils:log_in(Config, UserId),

    ProviderId = create_provider_supporting_user(Config, UserId),
    oz_test_utils:cluster_add_user(Config, ProviderId, UserId),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, UserId),

    {ok, {Token1, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZW_SRV(?ONEZONE_CLUSTER_ID)),
    {ok, {Token2, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OZP_SRV(?ONEZONE_CLUSTER_ID)),
    {ok, {Token3, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OPW_SRV(ProviderId)),
    {ok, {Token4, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OPP_SRV(ProviderId)),

    % Temporary token secret is shared per subject, so regenerating the secret of
    % AnotherUser should not affect the tested user
    oz_test_utils:call_oz(Config, temporary_token_secret, regenerate_for_subject, [?SUB(user, AnotherUser)]),
    ?assertMatch({true, _}, verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch({true, _}, verify_token(Config, Token2, ?OZP_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch({true, _}, verify_token(Config, Token3, ?OPW_SRV(ProviderId))),
    ?assertMatch({true, _}, verify_token(Config, Token4, ?OPP_SRV(ProviderId))),

    % Make sure that this works for the tested user
    oz_test_utils:call_oz(Config, temporary_token_secret, regenerate_for_subject, [?SUB(user, UserId)]),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_REVOKED), verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_REVOKED), verify_token(Config, Token2, ?OZP_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_REVOKED), verify_token(Config, Token3, ?OPW_SRV(ProviderId))),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_REVOKED), verify_token(Config, Token4, ?OPP_SRV(ProviderId))).


gui_tokens_are_invalidated_when_user_is_deleted(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, _Cookie2}} = oz_test_utils:log_in(Config, UserId),

    ProviderId = create_provider_supporting_user(Config, UserId),
    oz_test_utils:cluster_add_user(Config, ProviderId, UserId),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, UserId),

    {ok, {Token1, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZW_SRV(?ONEZONE_CLUSTER_ID)),
    {ok, {Token2, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZP_SRV(?ONEZONE_CLUSTER_ID)),
    {ok, {Token3, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OPW_SRV(ProviderId)),
    {ok, {Token4, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OPP_SRV(ProviderId)),

    oz_test_utils:delete_user(Config, UserId),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_INVALID), verify_token(Config, Token1, ?OZW_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_INVALID), verify_token(Config, Token2, ?OZP_SRV(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_INVALID), verify_token(Config, Token3, ?OPW_SRV(ProviderId))),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_INVALID), verify_token(Config, Token4, ?OPP_SRV(ProviderId))).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


init_per_testcase(_, Config) ->
    oz_test_utils:freeze_time(Config),
    Config.


end_per_testcase(_, Config) ->
    oz_test_utils:unfreeze_time(Config).

%%%===================================================================
%%% Helper functions
%%%===================================================================

create_access_token_for_gui(Config, UserId, SessionId, Service) ->
    oz_test_utils:call_oz(Config, token_logic, create_access_token_for_gui, [
        ?USER(UserId), UserId, SessionId, Service
    ]).


verify_token(Config, Token = #token{type = ?ACCESS_TOKEN(SessionId)}, Service) ->
    oz_test_utils:authenticate_by_token(Config, Token, #auth_ctx{service = Service, session_id = SessionId}).


create_provider_supporting_user(Config, UserId) ->
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space_by_provider(Config, ProviderId, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ProviderId.


connect_via_graph_sync(Config, Token, Cookies) ->
    Url = oz_test_utils:graph_sync_url(Config, gui),
    {ok, SerializedToken} = tokens:serialize(Token),
    gs_client:start_link(
        Url,
        {with_http_cookies, {token, SerializedToken}, Cookies},
        ?SUPPORTED_PROTO_VERSIONS,
        fun(_) -> ok end,
        [{secure, only_verify_peercert}, {cacerts, oz_test_utils:gui_ca_certs(Config)}]
    ).


get_user_info_via_rest(Config, Headers) ->
    http_client:get(
        ?URL(Config, [<<"/user">>]),
        Headers,
        <<"">>,
        [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}]
    ).
