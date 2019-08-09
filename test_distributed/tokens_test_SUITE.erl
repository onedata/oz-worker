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
-module(tokens_test_SUITE).
-author("Lukasz Opiola").

-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-include("api_test_utils.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    gui_tokens_are_bound_to_specific_audience/1,
    gui_tokens_can_be_created_via_endpoint/1,
    gui_tokens_expire/1,
    gui_tokens_are_invalidated_upon_logout/1,
    gui_tokens_are_invalidated_when_member_leaves_a_service/1,
    gui_tokens_are_invalidated_upon_shared_token_secret_change/1
]).

all() ->
    ?ALL([
        gui_tokens_are_bound_to_specific_audience,
        gui_tokens_can_be_created_via_endpoint,
        gui_tokens_expire,
        gui_tokens_are_invalidated_upon_logout,
        gui_tokens_are_invalidated_when_member_leaves_a_service,
        gui_tokens_are_invalidated_upon_shared_token_secret_change
    ]).

-define(EXP_AUTH(UserId, Audience, SessionId), #auth{
    subject = ?SUB(user, UserId), audience = Audience, session_id = SessionId
}).
-define(OZW_AUD(ServiceId), ?AUD(?OZ_WORKER, ServiceId)).
-define(OZP_AUD(ServiceId), ?AUD(?OZ_PANEL, ServiceId)).
-define(OPW_AUD(ServiceId), ?AUD(?OP_WORKER, ServiceId)).
-define(OPP_AUD(ServiceId), ?AUD(?OP_PANEL, ServiceId)).
-define(USR_AUD(UserId), ?AUD(user, UserId)).

%%%===================================================================
%%% Test functions
%%%===================================================================

gui_tokens_are_bound_to_specific_audience(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, _Cookie2}} = oz_test_utils:log_in(Config, UserId),

    % Tokens can be created for any session, but the session must exist at the
    % moment of verification
    {ok, TokenInvalidSess, _} = create_token(Config, <<"bad-session">>, UserId, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, TokenInvalidSess, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),

    % All users are allowed to create tokens for oz-worker
    {ok, Token1, _} = create_token(Config, UserId, Session1, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    ?assertMatch(
        {ok, ?EXP_AUTH(UserId, ?OZW_AUD(?ONEZONE_CLUSTER_ID), Session1)},
        verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))
    ),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token1, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token1, ?OPW_AUD(<<"p1-a">>))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token1, ?OPP_AUD(<<"p1-a">>))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token1, ?USR_AUD(UserId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token1, undefined)),

    % Only users supported by a provider can create tokens for op-worker
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ?UNIQUE_STRING),

    ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, create_token(Config, UserId, Session2, ?OPW_AUD(<<"non-existent">>))),
    ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, create_token(Config, UserId, Session2, ?OPW_AUD(ProviderId))),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space(Config, ProviderId, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, Token2, _} = create_token(Config, UserId, Session2, ?OPW_AUD(ProviderId)),
    ?assertMatch(
        {ok, ?EXP_AUTH(UserId, ?OPW_AUD(ProviderId), Session2)},
        verify_token(Config, Token2, ?OPW_AUD(ProviderId))
    ),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token2, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token2, ?OPP_AUD(ProviderId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token2, ?USR_AUD(UserId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token2, undefined)),

    % Only members of given cluster can generate tokens for oz/op-panel
    OzClusterId = ?ONEZONE_CLUSTER_ID,
    OpClusterId = ProviderId,

    ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, create_token(Config, UserId, Session2, ?OZP_AUD(OzClusterId))),
    ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, create_token(Config, UserId, Session1, ?OPP_AUD(OpClusterId))),
    oz_test_utils:cluster_add_user(Config, OzClusterId, UserId),
    {ok, Token3, _} = create_token(Config, UserId, Session2, ?OZP_AUD(OzClusterId)),
    ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, create_token(Config, UserId, Session1, ?OPP_AUD(OpClusterId))),
    oz_test_utils:cluster_add_user(Config, OpClusterId, UserId),
    {ok, Token4, _} = create_token(Config, UserId, Session1, ?OPP_AUD(OpClusterId)),

    ?assertMatch(
        {ok, ?EXP_AUTH(UserId, ?OZP_AUD(OzClusterId), Session2)},
        verify_token(Config, Token3, ?OZP_AUD(OzClusterId))
    ),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token3, ?OZW_AUD(OzClusterId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token3, ?OPW_AUD(OpClusterId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token3, ?OPP_AUD(OpClusterId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token3, ?USR_AUD(UserId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token3, undefined)),

    ?assertMatch(
        {ok, ?EXP_AUTH(UserId, ?OPP_AUD(OpClusterId), Session1)},
        verify_token(Config, Token4, ?OPP_AUD(OpClusterId))
    ),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token4, ?OZW_AUD(OzClusterId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token4, ?OZP_AUD(OzClusterId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token4, ?OPW_AUD(OpClusterId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token4, ?USR_AUD(UserId))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token4, undefined)).


gui_tokens_can_be_created_via_endpoint(Config) ->
    AcquireGuiToken = fun(Cookie, GuiType, ClusterId) ->
        oz_test_utils:request_gui_token(Config, Cookie, GuiType, ClusterId)
    end,

    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session, Cookie}} = oz_test_utils:log_in(Config, UserId),

    {ok, SerializedToken1} = ?assertMatch({ok, _}, AcquireGuiToken(Cookie, ?OZ_WORKER_GUI, ?ONEZONE_CLUSTER_ID)),
    {ok, Token1} = tokens:deserialize(SerializedToken1),
    ?assertMatch(
        {ok, ?EXP_AUTH(UserId, ?OZW_AUD(?ONEZONE_CLUSTER_ID), Session)},
        verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))
    ),

    % The user will belong to the cluster as the provider admin
    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config, UserId, ?UNIQUE_STRING),

    % The user is a member of provider cluster, but is not supported by the provider,
    % so he can't generate a token for the provider GUI.
    ?assertMatch({ok, _}, AcquireGuiToken(Cookie, ?ONEPANEL_GUI, ProviderId)),
    ?assertMatch(?ERROR_FORBIDDEN, AcquireGuiToken(Cookie, ?OP_WORKER_GUI, ProviderId)),

    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space(Config, ProviderId, Space1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % Now it should be possible for the user to generate a token
    {ok, SerializedToken2} = ?assertMatch({ok, _}, AcquireGuiToken(Cookie, ?OP_WORKER_GUI, ProviderId)),
    {ok, Token2} = tokens:deserialize(SerializedToken2),

    ?assertMatch(
        {ok, ?EXP_AUTH(UserId, ?OPW_AUD(ProviderId), Session)},
        verify_token(Config, Token2, ?OPW_AUD(ProviderId))
    ),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token2, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_INVALID, verify_token(Config, Token1, ?OPW_AUD(ProviderId))),

    % A user not belonging to the provider/cluster cannot generate GUI tokens for it
    {ok, User2} = oz_test_utils:create_user(Config),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, User2),
    ?assertMatch(?ERROR_FORBIDDEN, AcquireGuiToken(Cookie2, ?OP_WORKER_GUI, ProviderId)),

    % After becoming an effective member of the provider, he can
    {ok, Space2} = oz_test_utils:create_space(Config, ?USER(User2), <<"space">>),
    oz_test_utils:support_space(Config, ProviderId, Space2),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, SerializedToken3} = ?assertMatch({ok, _}, AcquireGuiToken(Cookie2, ?OP_WORKER_GUI, ProviderId)),
    % ... but not for the Onepanel GUI
    ?assertMatch(?ERROR_FORBIDDEN, AcquireGuiToken(Cookie2, ?ONEPANEL_GUI, ProviderId)),
    {ok, Token3} = tokens:deserialize(SerializedToken3),
    ?assertMatch(
        {ok, ?EXP_AUTH(User2, ?OPW_AUD(ProviderId), Session2)},
        verify_token(Config, Token3, ?OPW_AUD(ProviderId))
    ),

    % Tokens can be generated only for existing clusters
    ?assertMatch(?ERROR_NOT_FOUND, AcquireGuiToken(Cookie2, ?OP_WORKER_GUI, <<"bad-cluster">>)),

    % Make sure provider gui tokens are properly accepted in REST
    {ok, _, _, UserData} = ?assertMatch({ok, 200, _, _}, http_client:get(
        ?URL(Config, [<<"/user">>]),
        #{
            <<"X-Auth-Token">> => SerializedToken3,
            <<"X-Onedata-Audience-Token">> => tokens:serialize_audience_token(?OP_WORKER, ProviderToken)
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

    {ok, Token1, Expires1} = create_token(Config, UserId, Session1, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    oz_test_utils:simulate_time_passing(Config, 10),
    {ok, Token2, Expires2} = create_token(Config, UserId, Session2, ?OPW_AUD(ProviderId)),

    ?assertMatch(
        {ok, ?EXP_AUTH(UserId, ?OZW_AUD(?ONEZONE_CLUSTER_ID), Session1)},
        verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))
    ),
    ?assertMatch(
        {ok, ?EXP_AUTH(UserId, ?OPW_AUD(ProviderId), Session2)},
        verify_token(Config, Token2, ?OPW_AUD(ProviderId))
    ),

    wait_for_expiration(Config, Expires1),
    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:serialize(#cv_time{valid_until = Expires1})),
        verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))
    ),
    ?assertMatch(
        {ok, ?EXP_AUTH(UserId, ?OPW_AUD(ProviderId), Session2)},
        verify_token(Config, Token2, ?OPW_AUD(ProviderId))
    ),

    oz_test_utils:simulate_time_passing(Config, 10),
    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:serialize(#cv_time{valid_until = Expires1})),
        verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))
    ),
    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:serialize(#cv_time{valid_until = Expires2})),
        verify_token(Config, Token2, ?OPW_AUD(ProviderId))
    ).


gui_tokens_are_invalidated_upon_logout(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, Cookie2}} = oz_test_utils:log_in(Config, UserId),

    ProviderId = create_provider_supporting_user(Config, UserId),
    oz_test_utils:cluster_add_user(Config, ProviderId, UserId),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, UserId),

    {ok, Token1, _} = create_token(Config, UserId, Session1, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, Token2, _} = create_token(Config, UserId, Session1, ?OZP_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, Token3, _} = create_token(Config, UserId, Session1, ?OPW_AUD(ProviderId)),
    {ok, Token4, _} = create_token(Config, UserId, Session2, ?OPP_AUD(ProviderId)),

    oz_test_utils:log_out(Config, Cookie1),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token3, ?OPW_AUD(ProviderId))),
    ?assertMatch(
        {ok, ?EXP_AUTH(UserId, ?OPP_AUD(ProviderId), Session2)},
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

    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ?UNIQUE_STRING),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space(Config, ProviderId, SpaceId),
    oz_test_utils:cluster_add_user(Config, ProviderId, UserId),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, UserId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, Token1, _} = create_token(Config, UserId, Session, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, Token2, _} = create_token(Config, UserId, Session, ?OZP_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, Token3, _} = create_token(Config, UserId, Session, ?OPW_AUD(ProviderId)),
    {ok, Token4, _} = create_token(Config, UserId, Session, ?OPP_AUD(ProviderId)),

    oz_test_utils:unsupport_space(Config, ProviderId, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertMatch({ok, _}, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch({ok, _}, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, verify_token(Config, Token3, ?OPW_AUD(ProviderId))),
    ?assertMatch({ok, _}, verify_token(Config, Token4, ?OPP_AUD(ProviderId))),

    oz_test_utils:cluster_remove_user(Config, ?ONEZONE_CLUSTER_ID, UserId),
    ?assertMatch({ok, _}, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, verify_token(Config, Token3, ?OPW_AUD(ProviderId))),
    ?assertMatch({ok, _}, verify_token(Config, Token4, ?OPP_AUD(ProviderId))),

    oz_test_utils:cluster_remove_user(Config, ProviderId, UserId),
    ?assertMatch({ok, _}, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, verify_token(Config, Token3, ?OPW_AUD(ProviderId))),
    ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, verify_token(Config, Token4, ?OPP_AUD(ProviderId))),

    oz_test_utils:delete_user(Config, UserId),
    % User sessions should be deleted upon user deletion
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token1, ?OZW_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token2, ?OZP_AUD(?ONEZONE_CLUSTER_ID))),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token3, ?OPW_AUD(ProviderId))),
    ?assertMatch(?ERROR_TOKEN_SESSION_INVALID, verify_token(Config, Token4, ?OPP_AUD(ProviderId))).


gui_tokens_are_invalidated_upon_shared_token_secret_change(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, {Session1, _Cookie1}} = oz_test_utils:log_in(Config, UserId),
    {ok, {Session2, _Cookie2}} = oz_test_utils:log_in(Config, UserId),

    ProviderId = create_provider_supporting_user(Config, UserId),
    oz_test_utils:cluster_add_user(Config, ProviderId, UserId),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, UserId),

    {ok, Token1, _} = create_token(Config, UserId, Session1, ?OZW_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, Token2, _} = create_token(Config, UserId, Session2, ?OZP_AUD(?ONEZONE_CLUSTER_ID)),
    {ok, Token3, _} = create_token(Config, UserId, Session2, ?OPW_AUD(ProviderId)),
    {ok, Token4, _} = create_token(Config, UserId, Session2, ?OPP_AUD(ProviderId)),

    oz_test_utils:call_oz(Config, shared_token_secret, regenerate, []),

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

create_token(Config, UserId, SessionId, Audience) ->
    oz_test_utils:call_oz(Config, gui_tokens, create, [UserId, SessionId, Audience]).


verify_token(Config, Token, Audience) ->
    oz_test_utils:call_oz(Config, gui_tokens, verify, [Token, Audience]).


create_provider_supporting_user(Config, UserId) ->
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ?UNIQUE_STRING),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId), ?UNIQUE_STRING),
    oz_test_utils:support_space(Config, ProviderId, SpaceId),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ProviderId.


wait_for_expiration(Config, Expires) ->
    CurrentTime = oz_test_utils:get_mocked_time(Config),
    case CurrentTime > Expires of
        true -> ok;
        false -> oz_test_utils:simulate_time_passing(Config, Expires - CurrentTime + 1)
    end.