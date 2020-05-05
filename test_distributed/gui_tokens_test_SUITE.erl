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

-include("api_test_utils.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    gui_tokens_are_bound_to_specific_service/1,
    gui_tokens_can_be_created_via_endpoint/1,
    gui_tokens_expire/1,
    gui_tokens_are_invalidated_upon_logout/1,
    gui_tokens_are_invalidated_when_member_leaves_a_service/1,
    gui_tokens_are_invalidated_upon_temporary_token_secret_change/1,
    gui_tokens_are_invalidated_when_user_is_deleted/1
]).

all() ->
    ?ALL([
        gui_tokens_are_bound_to_specific_service,
        gui_tokens_can_be_created_via_endpoint,
        gui_tokens_expire,
        gui_tokens_are_invalidated_upon_logout,
        gui_tokens_are_invalidated_when_member_leaves_a_service,
        gui_tokens_are_invalidated_upon_temporary_token_secret_change,
        gui_tokens_are_invalidated_when_user_is_deleted
    ]).

-define(EXP_AUTH(UserId, SessionId), #auth{
    subject = ?SUB(user, UserId), session_id = SessionId
}).
-define(OZW_AUD(ServiceId), ?SERVICE(?OZ_WORKER, ServiceId)).
-define(OZP_AUD(ServiceId), ?SERVICE(?OZ_PANEL, ServiceId)).
-define(OPW_AUD(ServiceId), ?SERVICE(?OP_WORKER, ServiceId)).
-define(OPP_AUD(ServiceId), ?SERVICE(?OP_PANEL, ServiceId)).
-define(USR_AUD(UserId), ?SERVICE(user, UserId)).

-define(assertUnverifiedService(ExpService, Term), ?assertEqual(
    ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_service{whitelist = [ExpService]}),
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
        create_access_token_for_gui(Config, UserId, <<"bad-session">>, ?OZW_AUD(?ONEZONE_CLUSTER_ID))
    ),

    % All users are allowed to create tokens for oz-worker
    {ok, {Token1, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session1)},
        verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))
    ),
    % undefined service defaults to oz-worker service
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session1)},
        verify_token(Config, Token1, undefined)
    ),

    ?assertUnverifiedService(?OZW_AUD(?ONEZONE_CLUSTER_ID), verify_token(Config, Token1, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertUnverifiedService(?OZW_AUD(?ONEZONE_CLUSTER_ID), verify_token(Config, Token1, ?OPW_AUD(<<"p1-a">>))),
    ?assertUnverifiedService(?OZW_AUD(?ONEZONE_CLUSTER_ID), verify_token(Config, Token1, ?OPP_AUD(<<"p1-a">>))),
    ?assertUnverifiedService(?OZW_AUD(?ONEZONE_CLUSTER_ID), verify_token(Config, Token1, ?USR_AUD(UserId))),

    % Only users supported by a provider can create tokens for op-worker
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config),

    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPW_AUD(<<"non-existent">>)),
        create_access_token_for_gui(Config, UserId, Session2, ?OPW_AUD(<<"non-existent">>))
    ),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPW_AUD(ProviderId)),
        create_access_token_for_gui(Config, UserId, Session2, ?OPW_AUD(ProviderId))
    ),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space_by_provider(Config, ProviderId, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, {Token2, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OPW_AUD(ProviderId)),
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session2)},
        verify_token(Config, Token2, ?OPW_AUD(ProviderId))
    ),
    ?assertUnverifiedService(?OPW_AUD(ProviderId), verify_token(Config, Token2, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertUnverifiedService(?OPW_AUD(ProviderId), verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertUnverifiedService(?OPW_AUD(ProviderId), verify_token(Config, Token2, ?OPP_AUD(ProviderId))),
    ?assertUnverifiedService(?OPW_AUD(ProviderId), verify_token(Config, Token2, ?USR_AUD(UserId))),
    ?assertUnverifiedService(?OPW_AUD(ProviderId), verify_token(Config, Token2, undefined)),

    % Only members of given cluster can generate tokens for oz/op-panel
    OzClusterId = ?ONEZONE_CLUSTER_ID,
    OpClusterId = ProviderId,

    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OZP_AUD(OzClusterId)),
        create_access_token_for_gui(Config, UserId, Session2, ?OZP_AUD(OzClusterId))
    ),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPP_AUD(OpClusterId)),
        create_access_token_for_gui(Config, UserId, Session1, ?OPP_AUD(OpClusterId))
    ),
    oz_test_utils:cluster_add_user(Config, OzClusterId, UserId),
    {ok, {Token3, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OZP_AUD(OzClusterId)),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPP_AUD(OpClusterId)),
        create_access_token_for_gui(Config, UserId, Session1, ?OPP_AUD(OpClusterId))
    ),
    oz_test_utils:cluster_add_user(Config, OpClusterId, UserId),
    {ok, {Token4, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OPP_AUD(OpClusterId)),

    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session2)},
        verify_token(Config, Token3, ?OZP_AUD(OzClusterId))
    ),
    ?assertUnverifiedService(?OZP_AUD(OzClusterId), verify_token(Config, Token3, ?OZW_AUD(OzClusterId))),
    ?assertUnverifiedService(?OZP_AUD(OzClusterId), verify_token(Config, Token3, ?OPW_AUD(OpClusterId))),
    ?assertUnverifiedService(?OZP_AUD(OzClusterId), verify_token(Config, Token3, ?OPP_AUD(OpClusterId))),
    ?assertUnverifiedService(?OZP_AUD(OzClusterId), verify_token(Config, Token3, ?USR_AUD(UserId))),
    ?assertUnverifiedService(?OZP_AUD(OzClusterId), verify_token(Config, Token3, undefined)),

    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session1)},
        verify_token(Config, Token4, ?OPP_AUD(OpClusterId))
    ),
    ?assertUnverifiedService(?OPP_AUD(OpClusterId), verify_token(Config, Token4, ?OZW_AUD(OzClusterId))),
    ?assertUnverifiedService(?OPP_AUD(OpClusterId), verify_token(Config, Token4, ?OZP_AUD(OzClusterId))),
    ?assertUnverifiedService(?OPP_AUD(OpClusterId), verify_token(Config, Token4, ?OPW_AUD(OpClusterId))),
    ?assertUnverifiedService(?OPP_AUD(OpClusterId), verify_token(Config, Token4, ?USR_AUD(UserId))),
    ?assertUnverifiedService(?OPP_AUD(OpClusterId), verify_token(Config, Token4, undefined)).


gui_tokens_can_be_created_via_endpoint(Config) ->
    AcquireGuiToken = fun(Cookie, GuiType, ClusterId) ->
        oz_test_utils:request_gui_token(Config, Cookie, GuiType, ClusterId)
    end,

    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session, Cookie}} = oz_test_utils:log_in(Config, UserId),

    {ok, SerializedToken1} = ?assertMatch({ok, _}, AcquireGuiToken(Cookie, ?OZ_WORKER_GUI, ?ONEZONE_CLUSTER_ID)),
    {ok, Token1} = tokens:deserialize(SerializedToken1),
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session)},
        verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))
    ),

    % The user will belong to the cluster as the provider admin
    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config, UserId, ?UNIQUE_STRING),

    % The user is a member of provider cluster, but is not supported by the provider,
    % so he can't generate a token for the provider GUI.
    ?assertMatch({ok, _}, AcquireGuiToken(Cookie, ?ONEPANEL_GUI, ProviderId)),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_WORKER, ProviderId)),
        AcquireGuiToken(Cookie, ?OP_WORKER_GUI, ProviderId)
    ),

    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space_by_provider(Config, ProviderId, Space1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % Now it should be possible for the user to generate a token
    {ok, SerializedToken2} = ?assertMatch({ok, _}, AcquireGuiToken(Cookie, ?OP_WORKER_GUI, ProviderId)),
    {ok, Token2} = tokens:deserialize(SerializedToken2),

    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session)},
        verify_token(Config, Token2, ?OPW_AUD(ProviderId))
    ),
    ?assertUnverifiedService(?OPW_AUD(ProviderId), verify_token(Config, Token2, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertUnverifiedService(?OZW_AUD(?ONEZONE_CLUSTER_ID), verify_token(Config, Token1, ?OPW_AUD(ProviderId))),

    % A user not belonging to the provider/cluster cannot generate GUI tokens for it
    {ok, User2} = oz_test_utils:create_user(Config),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, User2),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_WORKER, ProviderId)),
        AcquireGuiToken(Cookie2, ?OP_WORKER_GUI, ProviderId)
    ),

    % After becoming an effective member of the provider, he can
    {ok, Space2} = oz_test_utils:create_space(Config, ?USER(User2), <<"space">>),
    oz_test_utils:support_space_by_provider(Config, ProviderId, Space2),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, SerializedToken3} = ?assertMatch({ok, _}, AcquireGuiToken(Cookie2, ?OP_WORKER_GUI, ProviderId)),
    % ... but not for the Onepanel GUI
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId)),
        AcquireGuiToken(Cookie2, ?ONEPANEL_GUI, ProviderId)
    ),
    {ok, Token3} = tokens:deserialize(SerializedToken3),
    ?assertMatch(
        {true, ?EXP_AUTH(User2, Session2)},
        verify_token(Config, Token3, ?OPW_AUD(ProviderId))
    ),

    % Tokens can be generated only for existing clusters
    ?assertMatch(?ERROR_NOT_FOUND, AcquireGuiToken(Cookie2, ?OP_WORKER_GUI, <<"bad-cluster">>)),

    % Make sure provider gui tokens are properly accepted in REST
    {ok, ProviderIdentityToken} = oz_test_utils:call_oz(Config, token_logic, create_provider_temporary_token, [
        ?ROOT, ProviderId, #{
            <<"type">> => ?IDENTITY_TOKEN,
            <<"caveats">> => [#cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 36000}]
        }
    ]),
    {ok, SerializedProviderIdentityToken} = tokens:serialize(ProviderIdentityToken),
    {ok, _, _, UserData} = ?assertMatch({ok, 200, _, _}, http_client:get(
        ?URL(Config, [<<"/user">>]),
        #{
            ?HDR_X_AUTH_TOKEN => SerializedToken3,
            ?HDR_X_ONEDATA_SERVICE_TOKEN => tokens:add_oneprovider_service_indication(?OP_WORKER, SerializedProviderIdentityToken)
        },
        <<"">>,
        [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}]
    )),
    %% @todo VFS-6098 legacy provider access tokens should be accepted as
    %% identity tokens for backward compatibility with old providers
    LegacyProviderToken = oz_test_utils:create_legacy_access_token(Config, ?SUB(?ONEPROVIDER, ProviderId)),
    LegacyProviderTokenAuthNone = oz_test_utils:confine_token_with_legacy_auth_none_caveat(LegacyProviderToken),
    {ok, SerializedLegacyAuthNone} = tokens:serialize(LegacyProviderTokenAuthNone),
    {ok, _, _, UserData} = ?assertMatch({ok, 200, _, UserData}, http_client:get(
        ?URL(Config, [<<"/user">>]),
        #{
            ?HDR_X_AUTH_TOKEN => SerializedToken3,
            ?HDR_X_ONEDATA_SERVICE_TOKEN => tokens:add_oneprovider_service_indication(?OP_WORKER, SerializedLegacyAuthNone)
        },
        <<"">>,
        [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}]
    )),
    ?assertMatch(#{<<"userId">> := User2}, json_utils:decode(UserData)),
    ok.


gui_tokens_expire(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, _Cookie2}} = oz_test_utils:log_in(Config, UserId),
    ProviderId = create_provider_supporting_user(Config, UserId),

    {ok, {Token1, Ttl1}} = create_access_token_for_gui(Config, UserId, Session1, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    ValidUntil1 = oz_test_utils:get_mocked_time(Config) + Ttl1,
    oz_test_utils:simulate_time_passing(Config, 10),
    {ok, {Token2, Ttl2}} = create_access_token_for_gui(Config, UserId, Session2, ?OPW_AUD(ProviderId)),
    ValidUntil2 = oz_test_utils:get_mocked_time(Config) + Ttl2,

    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session1)},
        verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))
    ),
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session2)},
        verify_token(Config, Token2, ?OPW_AUD(ProviderId))
    ),

    oz_test_utils:simulate_time_passing(Config, Ttl1 - 10 + 1),

    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = ValidUntil1}),
        verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))
    ),
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session2)},
        verify_token(Config, Token2, ?OPW_AUD(ProviderId))
    ),

    oz_test_utils:simulate_time_passing(Config, 10),
    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = ValidUntil1}),
        verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))
    ),
    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = ValidUntil2}),
        verify_token(Config, Token2, ?OPW_AUD(ProviderId))
    ).


gui_tokens_are_invalidated_upon_logout(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),

    ProviderId = create_provider_supporting_user(Config, UserId),
    oz_test_utils:cluster_add_user(Config, ProviderId, UserId),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, UserId),

    {ok, {Token1, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, {Token2, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZP_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, {Token3, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OPW_AUD(ProviderId)),
    {ok, {Token4, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OPP_AUD(ProviderId)),

    oz_test_utils:log_out(Config, Cookie1),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token3, ?OPW_AUD(ProviderId))),
    ?assertMatch(
        {true, ?EXP_AUTH(UserId, Session2)},
        verify_token(Config, Token4, ?OPP_AUD(ProviderId))
    ),

    oz_test_utils:log_out(Config, Cookie2),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token3, ?OPW_AUD(ProviderId))),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token4, ?OPP_AUD(ProviderId))).


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

    {ok, {Token1, _}} = create_access_token_for_gui(Config, UserId, Session, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, {Token2, _}} = create_access_token_for_gui(Config, UserId, Session, ?OZP_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, {Token3, _}} = create_access_token_for_gui(Config, UserId, Session, ?OPW_AUD(ProviderId)),
    {ok, {Token4, _}} = create_access_token_for_gui(Config, UserId, Session, ?OPP_AUD(ProviderId)),

    oz_test_utils:unsupport_space(Config, StorageId, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertMatch({true, _}, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch({true, _}, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPW_AUD(ProviderId)),
        verify_token(Config, Token3, ?OPW_AUD(ProviderId))
    ),
    ?assertMatch({true, _}, verify_token(Config, Token4, ?OPP_AUD(ProviderId))),

    oz_test_utils:cluster_remove_user(Config, ?ONEZONE_CLUSTER_ID, UserId),
    ?assertMatch({true, _}, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OZP_AUD(?ONEZONE_CLUSTER_ID)),
        verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))
    ),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPW_AUD(ProviderId)),
        verify_token(Config, Token3, ?OPW_AUD(ProviderId))
    ),
    ?assertMatch({true, _}, verify_token(Config, Token4, ?OPP_AUD(ProviderId))),

    oz_test_utils:cluster_remove_user(Config, ProviderId, UserId),
    ?assertMatch({true, _}, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OZP_AUD(?ONEZONE_CLUSTER_ID)),
        verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))
    ),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPW_AUD(ProviderId)),
        verify_token(Config, Token3, ?OPW_AUD(ProviderId))
    ),
    ?assertMatch(
        ?ERROR_TOKEN_SERVICE_FORBIDDEN(?OPP_AUD(ProviderId)),
        verify_token(Config, Token4, ?OPP_AUD(ProviderId))
    ).


gui_tokens_are_invalidated_upon_temporary_token_secret_change(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, AnotherUser} = oz_test_utils:create_user(Config),
    {ok, {Session1, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, _Cookie2}} = oz_test_utils:log_in(Config, UserId),

    ProviderId = create_provider_supporting_user(Config, UserId),
    oz_test_utils:cluster_add_user(Config, ProviderId, UserId),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, UserId),

    {ok, {Token1, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, {Token2, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OZP_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, {Token3, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OPW_AUD(ProviderId)),
    {ok, {Token4, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OPP_AUD(ProviderId)),

    % Temporary token secret is shared per subject, so regenerating the secret of
    % AnotherUser should not affect the tested user
    oz_test_utils:call_oz(Config, temporary_token_secret, regenerate_for_subject, [?SUB(user, AnotherUser)]),
    ?assertMatch({true, _}, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch({true, _}, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch({true, _}, verify_token(Config, Token3, ?OPW_AUD(ProviderId))),
    ?assertMatch({true, _}, verify_token(Config, Token4, ?OPP_AUD(ProviderId))),

    % Make sure that this works for the tested user
    oz_test_utils:call_oz(Config, temporary_token_secret, regenerate_for_subject, [?SUB(user, UserId)]),
    ?assertMatch(?ERROR_TOKEN_REVOKED, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_REVOKED, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_REVOKED, verify_token(Config, Token3, ?OPW_AUD(ProviderId))),
    ?assertMatch(?ERROR_TOKEN_REVOKED, verify_token(Config, Token4, ?OPP_AUD(ProviderId))).


gui_tokens_are_invalidated_when_user_is_deleted(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, _Cookie2}} = oz_test_utils:log_in(Config, UserId),

    ProviderId = create_provider_supporting_user(Config, UserId),
    oz_test_utils:cluster_add_user(Config, ProviderId, UserId),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, UserId),

    {ok, {Token1, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, {Token2, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OZP_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, {Token3, _}} = create_access_token_for_gui(Config, UserId, Session1, ?OPW_AUD(ProviderId)),
    {ok, {Token4, _}} = create_access_token_for_gui(Config, UserId, Session2, ?OPP_AUD(ProviderId)),

    oz_test_utils:delete_user(Config, UserId),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token3, ?OPW_AUD(ProviderId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token4, ?OPP_AUD(ProviderId))).

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
    oz_test_utils:mock_time(Config),
    Config.


end_per_testcase(_, Config) ->
    oz_test_utils:unmock_time(Config),
    ok.


%%%===================================================================
%%% Helper functions
%%%===================================================================

create_access_token_for_gui(Config, UserId, SessionId, Service) ->
    oz_test_utils:call_oz(Config, token_logic, create_access_token_for_gui, [
        ?USER(UserId), UserId, SessionId, Service
    ]).


verify_token(Config, Token, Service) ->
    oz_test_utils:authenticate_by_token(Config, Token, #auth_ctx{service = Service}).


create_provider_supporting_user(Config, UserId) ->
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space_by_provider(Config, ProviderId, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ProviderId.
