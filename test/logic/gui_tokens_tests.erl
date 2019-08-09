%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of gui_tokens module.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_tokens_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/onedata.hrl").
-include("datastore/oz_datastore_models.hrl").

-define(TIME_MOCK_STARTING_TIMESTAMP, 1500000000).

% Macros to mock users that are members of a cluster / provider
-define(USER_CLUSTER_MEMBER(ServiceId), <<"cl/", ServiceId/binary, "/", (datastore_utils:gen_key())/binary>>).
-define(USER_PROVIDER_MEMBER(ServiceId), <<"pr/", ServiceId/binary, "/", (datastore_utils:gen_key())/binary>>).

-define(EXISTING_SESSION, <<"existing-session-", (datastore_utils:gen_key())/binary>>).
-define(MATCH_EXISTING_SESSION, <<"existing-session-", _/binary>>).

%%%===================================================================
%%% Tests generator
%%%===================================================================

gui_tokens_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun create_gui_token/0,
            fun verify_gui_token/0,
            fun invalidate_tokens_when_session_ends/0,
            fun reject_forged_tokens/0
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(time_utils, []),
    meck:expect(time_utils, cluster_time_seconds, fun() ->
        get_mocked_time()
    end),

    meck:new(session, []),
    meck:expect(session, exists, fun mocked_session_exists/1),

    meck:new(shared_token_secret, []),
    meck:expect(shared_token_secret, get, fun() ->
        <<"mocked-shared-token-secret">>
    end),

    meck:new(cluster_logic, []),
    meck:expect(cluster_logic, has_eff_user, fun mocked_is_cluster_member/2),

    meck:new(provider_logic, []),
    meck:expect(provider_logic, has_eff_user, fun mocked_is_provider_member/2),

    oz_worker:set_env(http_domain, "dummy-location.org").

teardown(_) ->
    ?assert(meck:validate(time_utils)),
    ok = meck:unload(time_utils),
    ?assert(meck:validate(session)),
    ok = meck:unload(session),
    ?assert(meck:validate(shared_token_secret)),
    ok = meck:unload(shared_token_secret),
    ?assert(meck:validate(cluster_logic)),
    ok = meck:unload(cluster_logic),
    ?assert(meck:validate(provider_logic)),
    ok = meck:unload(provider_logic).

get_mocked_time() ->
    oz_worker:get_env(mocked_time, ?TIME_MOCK_STARTING_TIMESTAMP).

simulate_time_passing(Seconds) ->
    oz_worker:set_env(mocked_time, get_mocked_time() + Seconds),
    ok.

mock_session_existence(SessionId, Flag) ->
    put({session, SessionId}, Flag).

mocked_session_exists(SessionId) ->
    {ok, true =:= get({session, SessionId})}.

% Generates a mock user id that later evaluates to true if checked for
% membership in cluster / provider
mocked_service_member(?OZ_WORKER, ServiceId) ->
    <<"ozw/", ServiceId/binary, "/", (datastore_utils:gen_key())/binary>>;
mocked_service_member(?OP_WORKER, ServiceId) ->
    <<"opw/", ServiceId/binary, "/", (datastore_utils:gen_key())/binary>>;
mocked_service_member(?OZ_PANEL, ServiceId) ->
    <<"ozp/", ServiceId/binary, "/", (datastore_utils:gen_key())/binary>>;
mocked_service_member(?OP_PANEL, ServiceId) ->
    <<"opp/", ServiceId/binary, "/", (datastore_utils:gen_key())/binary>>.

mocked_is_cluster_member(ServiceId, UserId) ->
    case binary:split(UserId, <<"/">>, [global]) of
        [<<"ozp">>, ServiceId, _] -> true;
        [<<"opp">>, ServiceId, _] -> true;
        _ -> false
    end.

mocked_is_provider_member(ServiceId, UserId) ->
    case binary:split(UserId, <<"/">>, [global]) of
        [<<"opw">>, ServiceId, _] -> true;
        _ -> false
    end.


%%%===================================================================
%%% Tests
%%%===================================================================

create_gui_token() ->
    create_gui_token(?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)),
    create_gui_token(?AUD(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)),
    create_gui_token(?AUD(?OP_WORKER, <<"provider-id-1234">>)),
    create_gui_token(?AUD(?OP_PANEL, <<"provider-id-1234">>)).

create_gui_token(Audience = ?AUD(Service, ServiceId)) ->
    SessionId = <<(aai:serialize_audience(Audience))/binary, "-session">>,
    NotAMember = <<"user-not-a-member-of-service">>,
    ServiceMember = mocked_service_member(Service, ServiceId),
    case Service of
        ?OZ_WORKER ->
            % gui token for OZ worker can be generated for any user
            ?assertMatch({ok, _Token, _Expires}, gui_tokens:create(NotAMember, SessionId, Audience));
        _ ->
            % ... but for other services, only for members
            ?assertMatch(?ERROR_TOKEN_AUDIENCE_FORBIDDEN, gui_tokens:create(NotAMember, SessionId, Audience))
    end,
    ?assertMatch({ok, _Token, _Expires}, gui_tokens:create(ServiceMember, SessionId, Audience)).


verify_gui_token() ->
    verify_gui_token(?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)),
    verify_gui_token(?AUD(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)),
    verify_gui_token(?AUD(?OP_WORKER, <<"provider-id-abcd">>)),
    verify_gui_token(?AUD(?OP_PANEL, <<"provider-id-abcd">>)).

verify_gui_token(Audience = ?AUD(Service, ServiceId)) ->
    OtherServices = [?OZ_WORKER, ?OZ_PANEL, ?OP_WORKER, ?OP_PANEL] -- [Service],
    SessionId = <<(aai:serialize_audience(Audience))/binary, "-session">>,
    UserId = mocked_service_member(Service, ServiceId),
    {ok, Token, Expires} = gui_tokens:create(UserId, SessionId, Audience),

    mock_session_existence(SessionId, true),
    ?assertEqual(
        {ok, #auth{subject = ?SUB(user, UserId), audience = Audience, session_id = SessionId}},
        gui_tokens:verify(Token, Audience)
    ),
    lists:foreach(fun(OtherService) ->
        ?assertEqual(
            ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:serialize(#cv_audience{audience = Audience})),
            gui_tokens:verify(Token, ?AUD(OtherService, ServiceId))
        )
    end, OtherServices),
    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:serialize(#cv_audience{audience = Audience})),
        gui_tokens:verify(Token, ?AUD(Service, <<"asdad">>))
    ),

    simulate_time_passing(Expires - get_mocked_time() + 1),
    ?assertMatch(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(_),
        gui_tokens:verify(Token, ?AUD(Service, <<"asdad">>))
    ),
    ?ERROR_TOKEN_CAVEAT_UNVERIFIED(BadCaveat) = gui_tokens:verify(Token, ?AUD(Service, <<"asdad">>)),
    ?assert(lists:member(caveats:deserialize(BadCaveat), [
        #cv_time{valid_until = Expires},
        #cv_audience{audience = Audience}
    ])),

    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:serialize(#cv_time{valid_until = Expires})),
        gui_tokens:verify(Token, Audience)
    ).


invalidate_tokens_when_session_ends() ->
    invalidate_tokens_when_session_ends(?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)),
    invalidate_tokens_when_session_ends(?AUD(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)),
    invalidate_tokens_when_session_ends(?AUD(?OP_WORKER, <<"provider-id-9876">>)),
    invalidate_tokens_when_session_ends(?AUD(?OP_PANEL, <<"provider-id-9876">>)).

invalidate_tokens_when_session_ends(Audience = ?AUD(Service, ServiceId)) ->
    UserId = mocked_service_member(Service, ServiceId),
    SessionId = <<UserId/binary, "-session">>,
    {ok, Token, _Expires} = gui_tokens:create(
        UserId, SessionId, Audience
    ),

    mock_session_existence(SessionId, true),
    ?assertEqual(
        {ok, #auth{subject = ?SUB(user, UserId), audience = Audience, session_id = SessionId}},
        gui_tokens:verify(Token, Audience)
    ),

    mock_session_existence(SessionId, false),
    ?assertEqual(
        ?ERROR_TOKEN_SESSION_INVALID,
        gui_tokens:verify(Token, Audience)
    ).


reject_forged_tokens() ->
    UserId = <<"some-user-id">>,
    SessionId = <<"some-session-id">>,
    ForgedPrototype = #auth_token{
        onezone_domain = oz_worker:get_domain(),
        nonce = datastore_utils:gen_key(),
        persistent = false,
        subject = ?SUB(user, UserId),
        type = ?GUI_TOKEN(SessionId)
    },
    Token = tokens:construct(ForgedPrototype, <<"secret">>, [
        #cv_audience{audience = ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)},
        #cv_time{valid_until = get_mocked_time() + 500}
    ]),
    ?assertEqual(?ERROR_TOKEN_INVALID, access_tokens:verify_provider_auth(Token)),
    mock_session_existence(SessionId, true),
    ?assertEqual(?ERROR_TOKEN_INVALID, access_tokens:verify_provider_auth(Token)).

-endif.