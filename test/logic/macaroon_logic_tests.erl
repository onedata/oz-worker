%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of macaroon_logic module.
%%% @end
%%%-------------------------------------------------------------------
-module(macaroon_logic_tests).
-author("Lukasz Opiola").

-include_lib("eunit/include/eunit.hrl").
-include_lib("ctool/include/auth/onedata_macaroons.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").

-define(MOCK_MAX_PROVIDER_MACAROON_TTL, 3600).

%%%===================================================================
%%% Tests generator
%%%===================================================================

macaroon_logic_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun create_provider_auth/0,
            fun verify_provider_auth/0,
            fun verify_provider_identity/0
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    % Use process memory for storing macaroons data
    meck:new(macaroon_auth, []),
    meck:expect(macaroon_auth, create, fun(Secret, Type, Issuer) ->
        Id = datastore_utils:gen_key(),
        put(Id, {Secret, Type, Issuer}),
        {ok, Id}
    end),
    meck:expect(macaroon_auth, get, fun(Id) ->
        case get(Id) of
            {Secret, Type, Issuer} ->
                {ok, #macaroon_auth{secret = Secret, type = Type, issuer = Issuer}};
            _ ->
                {error, not_found}
        end
    end),
    meck:expect(macaroon_auth, delete, fun(Id) ->
        put(Id, undefined),
        ok
    end),

    meck:new(time_utils, [passthrough]),
    meck:expect(time_utils, cluster_time_seconds, fun() ->
        time_utils:system_time_seconds()
    end),

    application:set_env(?APP_NAME, http_domain, "dummy-location.org"),
    application:set_env(?APP_NAME, max_provider_macaroon_ttl, ?MOCK_MAX_PROVIDER_MACAROON_TTL).

teardown(_) ->
    ?assert(meck:validate(macaroon_auth)),
    ?assert(meck:validate(time_utils)),
    ok = meck:unload(macaroon_auth),
    ok = meck:unload(time_utils).



create_provider_auth() ->
    ProviderId = <<"12345">>,
    {ok, {_Macaroon, Identifier}} = macaroon_logic:create_provider_root_macaroon(ProviderId),
    {ok, _} = macaroon_auth:get(Identifier).


verify_provider_auth() ->
    ProviderId = <<"12345">>,
    {ok, {Macaroon, _Identifier}} = macaroon_logic:create_provider_root_macaroon(ProviderId),
    {ok, {OtherMacaroon, _Identifier2}} = macaroon_logic:create_provider_root_macaroon(<<"other-provider">>),

    ?assertEqual({ok, ProviderId}, macaroon_logic:verify_provider_auth(Macaroon)),
    ?assertNotEqual({ok, ProviderId}, macaroon_logic:verify_provider_auth(OtherMacaroon)),

    MacaroonWithAuthNone = onedata_macaroons:add_caveat(Macaroon, ?AUTHORIZATION_NONE_CAVEAT),
    ?assertEqual(?ERROR_MACAROON_INVALID, macaroon_logic:verify_provider_auth(MacaroonWithAuthNone)),

    MacaroonWithTTL = onedata_macaroons:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds(), 10)
    ),
    ?assertEqual({ok, ProviderId}, macaroon_logic:verify_provider_auth(MacaroonWithTTL)),

    MacaroonWithTooLongTTL = onedata_macaroons:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds(), ?MOCK_MAX_PROVIDER_MACAROON_TTL + 10)
    ),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(?MOCK_MAX_PROVIDER_MACAROON_TTL),
        macaroon_logic:verify_provider_auth(MacaroonWithTooLongTTL)
    ),

    MacaroonWithExpiredTTL = onedata_macaroons:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds() - 100, 10)
    ),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, macaroon_logic:verify_provider_auth(MacaroonWithExpiredTTL)),

    MacaroonWithTTLAndNoAuth = onedata_macaroons:add_caveat(MacaroonWithTTL,
        ?AUTHORIZATION_NONE_CAVEAT
    ),
    ?assertEqual(?ERROR_MACAROON_INVALID, macaroon_logic:verify_provider_auth(MacaroonWithTTLAndNoAuth)).


verify_provider_identity() ->
    ProviderId = <<"12345">>,
    {ok, {Macaroon, _Identifier}} = macaroon_logic:create_provider_root_macaroon(ProviderId),
    {ok, {OtherMacaroon, _Identifier2}} = macaroon_logic:create_provider_root_macaroon(<<"other-provider">>),

    ?assertEqual({ok, ProviderId}, macaroon_logic:verify_provider_identity(Macaroon)),
    ?assertNotEqual({ok, ProviderId}, macaroon_logic:verify_provider_identity(OtherMacaroon)),

    MacaroonWithAuthNone = onedata_macaroons:add_caveat(Macaroon, ?AUTHORIZATION_NONE_CAVEAT),
    ?assertEqual({ok, ProviderId}, macaroon_logic:verify_provider_identity(MacaroonWithAuthNone)),

    MacaroonWithTTL = onedata_macaroons:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds(), 10)
    ),
    ?assertEqual({ok, ProviderId}, macaroon_logic:verify_provider_identity(MacaroonWithTTL)),

    MacaroonWithTooLongTTL = onedata_macaroons:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds(), ?MOCK_MAX_PROVIDER_MACAROON_TTL + 10)
    ),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(?MOCK_MAX_PROVIDER_MACAROON_TTL),
        macaroon_logic:verify_provider_identity(MacaroonWithTooLongTTL)
    ),

    MacaroonWithExpiredTTL = onedata_macaroons:add_caveat(Macaroon,
        ?TIME_CAVEAT(time_utils:cluster_time_seconds() - 100, 10)
    ),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, macaroon_logic:verify_provider_identity(MacaroonWithExpiredTTL)),

    MacaroonWithTTLAndNoAuth = onedata_macaroons:add_caveat(MacaroonWithTTL,
        ?AUTHORIZATION_NONE_CAVEAT
    ),
    ?assertEqual({ok, ProviderId}, macaroon_logic:verify_provider_identity(MacaroonWithTTLAndNoAuth)).
