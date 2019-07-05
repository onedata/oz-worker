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

-include_lib("eunit/include/eunit.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/api_errors.hrl").

-define(MOCK_MAX_PROVIDER_MACAROON_TTL, 3600).
-define(TIME_MOCK_STARTING_TIMESTAMP, 1500000000).

%%%===================================================================
%%% Tests generator
%%%===================================================================

access_tokens_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun create_provider_root_macaroon/0,
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

    oz_worker:set_env(http_domain, "dummy-location.org"),
    oz_worker:set_env(max_provider_macaroon_ttl, ?MOCK_MAX_PROVIDER_MACAROON_TTL).

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

create_provider_root_macaroon() ->
    ProviderId = <<"12345">>,
    {ok, _Macaroon, Identifier} = access_tokens:create_provider_root_macaroon(ProviderId),
    ?assertMatch({ok, _, _}, macaroon_auth:get(Identifier)).


verify_provider_auth() ->
    ProviderId = <<"12345">>,
    {ok, Macaroon, _Identifier} = access_tokens:create_provider_root_macaroon(ProviderId),
    {ok, OtherMacaroon, _Identifier2} = access_tokens:create_provider_root_macaroon(<<"other-provider">>),

    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_auth(Macaroon)),
    ?assertNotEqual({ok, ProviderId}, access_tokens:verify_provider_auth(OtherMacaroon)),

    MacaroonWithAuthNone = tokens:add_caveat(Macaroon, ?AUTHORIZATION_NONE_CAVEAT),
    ?assertEqual(?ERROR_MACAROON_INVALID, access_tokens:verify_provider_auth(MacaroonWithAuthNone)),

    MacaroonWithTTL = tokens:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds(), 10)
    ),
    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_auth(MacaroonWithTTL)),

    MacaroonWithTooLongTTL = tokens:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds(), ?MOCK_MAX_PROVIDER_MACAROON_TTL + 10)
    ),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(?MOCK_MAX_PROVIDER_MACAROON_TTL),
        access_tokens:verify_provider_auth(MacaroonWithTooLongTTL)
    ),

    MacaroonWithExpiredTTL = tokens:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds() - 100, 10)
    ),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, access_tokens:verify_provider_auth(MacaroonWithExpiredTTL)),

    MacaroonWithTTLAndNoAuth = tokens:add_caveat(MacaroonWithTTL,
        ?AUTHORIZATION_NONE_CAVEAT
    ),
    ?assertEqual(?ERROR_MACAROON_INVALID, access_tokens:verify_provider_auth(MacaroonWithTTLAndNoAuth)).


verify_provider_identity() ->
    ProviderId = <<"12345">>,
    {ok, Macaroon, _Identifier} = access_tokens:create_provider_root_macaroon(ProviderId),
    {ok, OtherMacaroon, _Identifier2} = access_tokens:create_provider_root_macaroon(<<"other-provider">>),

    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_identity(Macaroon)),
    ?assertNotEqual({ok, ProviderId}, access_tokens:verify_provider_identity(OtherMacaroon)),

    MacaroonWithAuthNone = tokens:add_caveat(Macaroon, ?AUTHORIZATION_NONE_CAVEAT),
    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_identity(MacaroonWithAuthNone)),

    MacaroonWithTTL = tokens:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds(), 10)
    ),
    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_identity(MacaroonWithTTL)),

    MacaroonWithTooLongTTL = tokens:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds(), ?MOCK_MAX_PROVIDER_MACAROON_TTL + 10)
    ),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(?MOCK_MAX_PROVIDER_MACAROON_TTL),
        access_tokens:verify_provider_identity(MacaroonWithTooLongTTL)
    ),

    MacaroonWithExpiredTTL = tokens:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds() - 100, 10)
    ),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, access_tokens:verify_provider_identity(MacaroonWithExpiredTTL)),

    MacaroonWithTTLAndNoAuth = tokens:add_caveat(MacaroonWithTTL,
        ?AUTHORIZATION_NONE_CAVEAT
    ),
    ?assertEqual({ok, ProviderId}, access_tokens:verify_provider_identity(MacaroonWithTTLAndNoAuth)).


reject_forged_tokens() ->
    ForgedPrototype = #auth_token{
        version = 1,
        onezone_domain = oz_worker:get_domain(),
        nonce = <<"123123123">>,
        persistent = true,
        type = ?ACCESS_TOKEN
    },
    Token = tokens:construct(ForgedPrototype, <<"secret">>, []),
    ?assertEqual(?ERROR_MACAROON_INVALID, access_tokens:verify_provider_auth(Token)).
