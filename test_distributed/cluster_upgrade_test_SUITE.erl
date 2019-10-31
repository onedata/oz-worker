%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This test verifies if cluster upgrade procedures (employed during software
%%% upgrades) work as expected.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_upgrade_test_SUITE).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include("api_test_utils.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").


%% API
-export([
    all/0, init_per_suite/1, end_per_suite/1
]).
-export([
    upgrade_from_19_02_x/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    upgrade_from_19_02_x
]).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Tests
%%%===================================================================

upgrade_from_19_02_x(Config) ->
    {ok, User1} = oz_test_utils:create_user(Config),
    {ok, User2} = oz_test_utils:create_user(Config),
    {ok, User3} = oz_test_utils:create_user(Config),
    {ok, User4} = oz_test_utils:create_user(Config),
    {ok, User5} = oz_test_utils:create_user(Config),

    User1Tokens = create_n_legacy_client_tokens(Config, User1, 0),
    User2Tokens = create_n_legacy_client_tokens(Config, User2, 2),
    User3Tokens = create_n_legacy_client_tokens(Config, User3, 7),
    User4Tokens = create_n_legacy_client_tokens(Config, User4, 1),
    User5Tokens = create_n_legacy_client_tokens(Config, User5, 13),

    {P1, _, _, _} = PToken1 = create_legacy_provider(Config),
    {P2, _, _, _} = PToken2 = create_legacy_provider(Config),
    {P3, _, _, _} = PToken3 = create_legacy_provider(Config),
    {P4, _, _, _} = PToken4 = create_legacy_provider(Config),

    % Force trigger a cluster upgrade
    ?assertEqual({ok, 2}, oz_test_utils:call_oz(Config, node_manager_plugin, upgrade_cluster, [1])),

    {ok, AllTokens} = oz_test_utils:call_oz(Config, token_logic, list, [?ROOT]),
    ?assertEqual(27, length(AllTokens)),

    ?assertEqual(0, length(element(2, {ok, _} = oz_test_utils:call_oz(
        Config, token_logic, list_user_named_tokens, [?ROOT, User1])))
    ),
    ?assertEqual(2, length(element(2, {ok, _} = oz_test_utils:call_oz(
        Config, token_logic, list_user_named_tokens, [?ROOT, User2])))
    ),
    ?assertEqual(7, length(element(2, {ok, _} = oz_test_utils:call_oz(
        Config, token_logic, list_user_named_tokens, [?ROOT, User3])))
    ),
    ?assertEqual(1, length(element(2, {ok, _} = oz_test_utils:call_oz(
        Config, token_logic, list_user_named_tokens, [?ROOT, User4])))
    ),
    ?assertEqual(13, length(element(2, {ok, _} = oz_test_utils:call_oz(
        Config, token_logic, list_user_named_tokens, [?ROOT, User5])))
    ),
    {ok, [_]} = oz_test_utils:call_oz(Config, token_logic, list_provider_named_tokens, [?ROOT, P1]),
    {ok, [_]} = oz_test_utils:call_oz(Config, token_logic, list_provider_named_tokens, [?ROOT, P2]),
    {ok, [_]} = oz_test_utils:call_oz(Config, token_logic, list_provider_named_tokens, [?ROOT, P3]),
    {ok, [_]} = oz_test_utils:call_oz(Config, token_logic, list_provider_named_tokens, [?ROOT, P4]),

    % Check all user tokens
    lists:foreach(fun({UserId, TokenId, Secret, Serialized}) ->
        {ok, Deserialized} = tokens:deserialize(Serialized),
        % Check if the migrated token has been saved in the new model
        {ok, #document{value = NamedToken}} = ?assertMatch({ok, #document{value = #od_token{
            name = <<"legacy token ", _/binary>>,
            version = 1,
            subject = ?SUB(user, UserId),
            type = ?ACCESS_TOKEN,
            caveats = [#cv_time{}],
            secret = Secret,
            metadata = #{<<"creationTime">> := _},
            revoked = false
        }}}, oz_test_utils:call_oz(Config, od_token, get, [TokenId])),
        ?assertMatch({ok, #{
            <<"name">> := <<"legacy token ", _/binary>>,
            <<"id">> := TokenId,
            <<"subject">> := ?SUB(user, UserId),
            <<"type">> := ?ACCESS_TOKEN,
            <<"caveats">> := [#cv_time{}],
            <<"metadata">> := #{<<"creationTime">> := _},
            <<"revoked">> := false,
            <<"token">> := Deserialized
        }}, oz_test_utils:call_oz(Config, token_logic, get_named_token, [?USER(UserId), TokenId])),
        % Make sure the migrated token has exactly the same serialized form
        % as the original one
        Token = oz_test_utils:call_oz(Config, od_token, named_token_to_token, [TokenId, NamedToken]),
        % Subject is not supported in tokens version 1
        ?assertEqual(Token#token{subject = ?SUB(nobody)}, Deserialized),
        % The legacy onedata_auth record should have been removed
        ?assertEqual({error, not_found}, oz_test_utils:call_oz(Config, onedata_auth, get, [TokenId])),
        % The migrated token should still work
        ?assertMatch({ok, ?USER(UserId)}, oz_test_utils:call_oz(
            Config, token_auth, verify_access_token, [Deserialized, undefined, undefined]
        ))
    end, User1Tokens ++ User2Tokens ++ User3Tokens ++ User4Tokens ++ User5Tokens),

    % Check all provider tokens
    lists:foreach(fun({ProviderId, TokenId, Secret, RootTokenSerialized}) ->
        {ok, RootTokenDeserialized} = tokens:deserialize(RootTokenSerialized),
        % Check if the migrated token has been saved in the new model
        {ok, #document{value = NamedToken}} = ?assertMatch({ok, #document{value = #od_token{
            name = <<"root token">>,
            version = 1,
            subject = ?SUB(?ONEPROVIDER, ProviderId),
            type = ?ACCESS_TOKEN,
            caveats = [],
            secret = Secret,
            metadata = #{<<"creationTime">> := _},
            revoked = false
        }}}, oz_test_utils:call_oz(Config, od_token, get, [TokenId])),
        ?assertMatch({ok, #{
            <<"name">> := <<"root token">>,
            <<"id">> := TokenId,
            <<"subject">> := ?SUB(?ONEPROVIDER, ProviderId),
            <<"type">> := ?ACCESS_TOKEN,
            <<"caveats">> := [],
            <<"metadata">> := #{<<"creationTime">> := _},
            <<"revoked">> := false,
            <<"token">> := RootTokenDeserialized
        }}, oz_test_utils:call_oz(Config, token_logic, get_named_token, [?PROVIDER(ProviderId), TokenId])),
        % Make sure the migrated token has exactly the same serialized form
        % as the original one
        Token = oz_test_utils:call_oz(Config, od_token, named_token_to_token, [TokenId, NamedToken]),
        % Subject is not supported in tokens version 1
        ?assertEqual(Token#token{subject = ?SUB(nobody)}, RootTokenDeserialized),
        % The legacy macaroon_auth record should have been removed
        ?assertEqual({error, not_found}, oz_test_utils:call_oz(Config, macaroon_auth, get, [TokenId])),
        % The migrated token should still work
        ?assertMatch({ok, ?PROVIDER(ProviderId)}, oz_test_utils:call_oz(
            Config, token_auth, verify_access_token, [RootTokenDeserialized, undefined, undefined]
        ))
    end, [PToken1, PToken2, PToken3, PToken4]).


create_n_legacy_client_tokens(Config, UserId, Count) ->
    lists:map(fun(_) ->
        Secret = tokens:generate_secret(),
        {ok, Doc} = oz_test_utils:call_oz(Config, onedata_auth, save, [#document{
            value = #onedata_auth{secret = Secret, user_id = UserId}
        }]),
        TokenId = Doc#document.key,
        ExpirationTime = oz_test_utils:cluster_time_seconds(Config) + 31536000, % 1 year
        Macaroon = macaroon:create(oz_test_utils:oz_domain(Config), Secret, TokenId),
        Confined = macaroon:add_first_party_caveat(Macaroon, ["time < ", integer_to_binary(ExpirationTime)]),
        Token = #token{
            version = 1,
            onezone_domain = oz_test_utils:oz_domain(Config),
            id = TokenId,
            persistent = true,
            type = ?ACCESS_TOKEN,
            macaroon = Confined
        },
        {ok, Serialized} = tokens:serialize(Token),
        oz_test_utils:call_oz(Config, od_user, update, [UserId, fun(User = #od_user{client_tokens = ClientTokens}) ->
            {ok, User#od_user{client_tokens = [Serialized | ClientTokens]}}
        end]),
        {UserId, TokenId, Secret, Serialized}
    end, lists:seq(1, Count)).


create_legacy_provider(Config) ->
    ProviderId = datastore_utils:gen_key(),
    Secret = tokens:generate_secret(),
    {ok, RootTokenId} = oz_test_utils:call_oz(Config, macaroon_auth, create, [
        Secret, ?PROVIDER(ProviderId)
    ]),
    Prototype = #token{
        version = 1,
        onezone_domain = oz_test_utils:oz_domain(Config),
        id = RootTokenId,
        persistent = true,
        type = ?ACCESS_TOKEN
    },
    RootToken = tokens:construct(Prototype, Secret, []),

    ProviderRecord = #od_provider{
        name = ?UNIQUE_STRING,
        root_token = RootTokenId,
        subdomain_delegation = false,
        domain = <<(?UNIQUE_STRING)/binary, ".example.com">>,
        subdomain = undefined,
        admin_email = <<(?UNIQUE_STRING)/binary, "@example.com">>
    },
    {ok, _} = oz_test_utils:call_oz(Config, od_provider, create, [
        #document{key = ProviderId, value = ProviderRecord}
    ]),
    oz_test_utils:call_oz(Config, cluster_logic, create_oneprovider_cluster, [
        undefined, ProviderId
    ]),
    {ok, SerializedRootToken} = tokens:serialize(RootToken),
    {ProviderId, RootTokenId, Secret, SerializedRootToken}.