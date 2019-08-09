%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of access_tokens module.
%%% @end
%%%-------------------------------------------------------------------
-module(access_tokens_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/api_errors.hrl").

-define(TIME_MOCK_STARTING_TIMESTAMP, 1500000000).
-define(NOW(), time_utils:cluster_time_seconds()).

%%%===================================================================
%%% Tests generator
%%%===================================================================

access_tokens_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun create_provider_root_token/0,
            fun verify_provider_auth/0,
            fun verify_provider_identity/0,
            fun reject_forged_tokens/0
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    % Use process memory for storing macaroons data
    meck:new(macaroon_auth, []),
    meck:expect(macaroon_auth, create, fun(Secret, Issuer) ->
        Id = datastore_utils:gen_key(),
        put(Id, {Secret, Issuer}),
        {ok, Id}
    end),
    meck:expect(macaroon_auth, get, fun(Id) ->
        case get(Id) of
            {Secret, Issuer} ->
                {ok, Secret, Issuer};
            _ ->
                {error, not_found}
        end
    end),
    meck:expect(macaroon_auth, delete, fun(Id) ->
        put(Id, undefined),
        ok
    end),

    meck:new(time_utils, []),
    meck:expect(time_utils, cluster_time_seconds, fun() ->
        get_mocked_time()
    end),

    oz_worker:set_env(http_domain, "dummy-location.org").

teardown(_) ->
    ?assert(meck:validate(macaroon_auth)),
    ok = meck:unload(macaroon_auth),
    ?assert(meck:validate(time_utils)),
    ok = meck:unload(time_utils).

get_mocked_time() ->
    oz_worker:get_env(mocked_time, ?TIME_MOCK_STARTING_TIMESTAMP).


%%%===================================================================
%%% Tests
%%%===================================================================

create_provider_root_token() ->
    ProviderId = <<"12345">>,
    {ok, _Token, Identifier} = access_tokens:create_provider_root_token(ProviderId),
    ?assertMatch({ok, _, _}, macaroon_auth:get(Identifier)).


verify_provider_auth() ->
    ProviderId = <<"12345">>,
    {ok, Token, _Identifier} = access_tokens:create_provider_root_token(ProviderId),
    {ok, OtherToken, _Identifier2} = access_tokens:create_provider_root_token(<<"other-provider">>),

    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_auth(Token)),
    ?assertNotEqual({ok, ProviderId}, access_tokens:verify_provider_auth(OtherToken)),

    TokenWithAuthNone = tokens:confine(Token, #cv_authorization_none{}),
    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:serialize(#cv_authorization_none{})),
        access_tokens:verify_provider_auth(TokenWithAuthNone)
    ),

    TokenWithTTL = tokens:confine(Token, #cv_time{valid_until = ?NOW() + 10}),
    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_auth(TokenWithTTL)),

    CvTimeExpired = #cv_time{valid_until = ?NOW() - 100},
    TokenWithExpiredTTL = tokens:confine(Token, CvTimeExpired),
    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:serialize(CvTimeExpired)),
        access_tokens:verify_provider_auth(TokenWithExpiredTTL)
    ),

    TokenWithTTLAndAuthNone = tokens:confine(TokenWithTTL, #cv_authorization_none{}),
    ?assertMatch(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(_),
        access_tokens:verify_provider_auth(TokenWithTTLAndAuthNone)
    ),
    ?ERROR_TOKEN_CAVEAT_UNVERIFIED(BadCaveat) = access_tokens:verify_provider_auth(TokenWithTTLAndAuthNone),
    ?assert(lists:member(caveats:deserialize(BadCaveat), [
        #cv_time{valid_until = ?NOW() - 100},
        #cv_authorization_none{}
    ])).


verify_provider_identity() ->
    ProviderId = <<"12345">>,
    {ok, Token, _Identifier} = access_tokens:create_provider_root_token(ProviderId),
    {ok, OtherToken, _Identifier2} = access_tokens:create_provider_root_token(<<"other-provider">>),

    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_identity(Token)),
    ?assertNotEqual({ok, ProviderId}, access_tokens:verify_provider_identity(OtherToken)),

    TokenWithAuthNone = tokens:confine(Token, #cv_authorization_none{}),
    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_identity(TokenWithAuthNone)),

    TokenWithTTL = tokens:confine(Token, #cv_time{valid_until = ?NOW() + 10}),
    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_identity(TokenWithTTL)),

    CvTimeExpired = #cv_time{valid_until = ?NOW() - 100},
    TokenWithExpiredTTL = tokens:confine(Token, CvTimeExpired),
    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:serialize(CvTimeExpired)),
        access_tokens:verify_provider_auth(TokenWithExpiredTTL)
    ),

    TokenWithTTLAndNoAuth = tokens:confine(TokenWithTTL, #cv_authorization_none{}),
    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_identity(TokenWithTTLAndNoAuth)).


reject_forged_tokens() ->
    ForgedPrototype = #auth_token{
        version = 1,
        onezone_domain = oz_worker:get_domain(),
        nonce = <<"123123123">>,
        persistent = true,
        type = ?ACCESS_TOKEN
    },
    Token = tokens:construct(ForgedPrototype, <<"secret">>, []),
    ?assertEqual(?ERROR_TOKEN_INVALID, access_tokens:verify_provider_auth(Token)).

-endif.