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
-include_lib("ctool/include/onedata.hrl").
-include("datastore/oz_datastore_models.hrl").

-define(MOCK_MAX_PROVIDER_MACAROON_TTL, 3600).
-define(TIME_MOCK_STARTING_TIMESTAMP, 1500000000).

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
            fun verify_provider_identity/0,
            fun create_gui_macaroon/0,
            fun verify_gui_macaroon/0,
            fun should_refresh_gui_macaroon/0,
            fun delete_gui_macaroon/0
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    % Use process memory for storing macaroons data
    MacaroonRecords = [macaroon_auth, volatile_macaroon],
    lists:foreach(fun(Module) ->
        meck:new(Module, []),
        meck:expect(Module, create, fun(Secret, Issuer) ->
            Id = datastore_utils:gen_key(),
            put(Id, {Secret, Issuer}),
            {ok, Id}
        end),
        meck:expect(Module, get, fun(Id) ->
            case get(Id) of
                {Secret, Issuer} ->
                    {ok, Secret, Issuer};
                _ ->
                    {error, not_found}
            end
        end),
        meck:expect(Module, delete, fun(Id) ->
            put(Id, undefined),
            ok
        end)
    end, MacaroonRecords),

    meck:new(time_utils, [passthrough]),
    meck:expect(time_utils, cluster_time_seconds, fun() ->
        get_mocked_time()
    end),

    oz_worker:set_env(http_domain, "dummy-location.org"),
    oz_worker:set_env(max_provider_macaroon_ttl, ?MOCK_MAX_PROVIDER_MACAROON_TTL).

teardown(_) ->
    MacaroonRecords = [macaroon_auth, volatile_macaroon],
    lists:foreach(fun(Module) ->
        ?assert(meck:validate(Module)),
        ok = meck:unload(Module)
    end, MacaroonRecords),
    ?assert(meck:validate(time_utils)),
    ok = meck:unload(time_utils).

get_mocked_time() ->
    oz_worker:get_env(mocked_time, ?TIME_MOCK_STARTING_TIMESTAMP).

simulate_time_passing(Seconds) ->
    oz_worker:set_env(mocked_time, get_mocked_time() + Seconds),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

create_provider_auth() ->
    ProviderId = <<"12345">>,
    {ok, {_Macaroon, Identifier}} = macaroon_logic:create_provider_root_macaroon(ProviderId),
    ?assertMatch({ok, _, _}, macaroon_auth:get(Identifier)).


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


create_gui_macaroon() ->
    create_gui_macaroon(?ONEPROVIDER, <<"12345">>),
    create_gui_macaroon(?ONEZONE, ?ONEZONE_CLUSTER_ID).

create_gui_macaroon(ClusterType, ClusterId) ->
    SessionId = <<ClusterId/binary, "-session">>,
    {ok, {Identifier, _Macaroon, _Expires}} = macaroon_logic:create_gui_macaroon(
        <<"user">>, SessionId, ClusterType, ClusterId
    ),
    ?assertMatch({ok, _, _}, volatile_macaroon:get(Identifier)).


verify_gui_macaroon() ->
    verify_gui_macaroon(?ONEPROVIDER, <<"abcds">>),
    verify_gui_macaroon(?ONEZONE, ?ONEZONE_CLUSTER_ID).

verify_gui_macaroon(ClusterType, ClusterId) ->
    [OtherClusterType] = [?ONEPROVIDER, ?ONEZONE] -- [ClusterType],
    SessionId = <<ClusterId/binary, "-session">>,
    UserId = <<"mockuserid789992">>,
    {ok, {Identifier, Macaroon, Expires}} = macaroon_logic:create_gui_macaroon(
        UserId, SessionId, ClusterType, ClusterId
    ),
    SessionVerifyFun = fun(_VerifySessionId, VerifyId) ->
        VerifyId == Identifier
    end,
    ?assertEqual(
        {ok, UserId, SessionId},
        macaroon_logic:verify_gui_macaroon(Macaroon, ClusterType, ClusterId, SessionVerifyFun)
    ),
    ?assertEqual(
        ?ERROR_MACAROON_INVALID,
        macaroon_logic:verify_gui_macaroon(Macaroon, OtherClusterType, ClusterId, SessionVerifyFun)
    ),
    ?assertEqual(
        ?ERROR_MACAROON_INVALID,
        macaroon_logic:verify_gui_macaroon(Macaroon, ClusterType, <<"asdad">>, SessionVerifyFun)
    ),

    simulate_time_passing(Expires - get_mocked_time() + 1),
    ?assertEqual(
        ?ERROR_MACAROON_INVALID,
        macaroon_logic:verify_gui_macaroon(Macaroon, ClusterType, <<"asdad">>, SessionVerifyFun)
    ).


should_refresh_gui_macaroon() ->
    should_refresh_gui_macaroon(?ONEPROVIDER, <<"dfvaerwfasdf">>),
    should_refresh_gui_macaroon(?ONEZONE, ?ONEZONE_CLUSTER_ID).

should_refresh_gui_macaroon(ClusterType, ClusterId) ->
    {ok, {_Identifier, _Macaroon, Expires}} = macaroon_logic:create_gui_macaroon(
        <<"user">>, <<"session">>, ClusterType, ClusterId
    ),
    ?assertEqual(false, macaroon_logic:should_refresh_gui_macaroon(Expires)),
    simulate_time_passing(Expires - get_mocked_time() + 1),
    ?assertEqual(true, macaroon_logic:should_refresh_gui_macaroon(Expires)).


delete_gui_macaroon() ->
    delete_gui_macaroon(?ONEPROVIDER, <<"12345abcd">>),
    delete_gui_macaroon(?ONEZONE, ?ONEZONE_CLUSTER_ID).

delete_gui_macaroon(ClusterType, ClusterId) ->
    UserId = <<"kosdhfsdf">>,
    SessionId = <<UserId/binary, "-session">>,
    SessionVerifyFun = fun(_, _) -> true end,
    {ok, {Identifier, Macaroon, _Expires}} = macaroon_logic:create_gui_macaroon(
        UserId, SessionId, ClusterType, ClusterId
    ),

    ?assertEqual(
        {ok, UserId, SessionId},
        macaroon_logic:verify_gui_macaroon(Macaroon, ClusterType, ClusterId, SessionVerifyFun)
    ),
    ?assertEqual(ok, macaroon_logic:delete_gui_macaroon(Macaroon)),
    ?assertEqual({error, not_found}, volatile_macaroon:get(Identifier)),
    ?assertEqual(
        ?ERROR_MACAROON_INVALID,
        macaroon_logic:verify_gui_macaroon(Macaroon, ClusterType, ClusterId, SessionVerifyFun)
    ).
