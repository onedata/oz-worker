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
    upgrade_from_19_02_x_tokens/1,
    upgrade_from_19_02_x_storages/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    upgrade_from_19_02_x_tokens,
    upgrade_from_19_02_x_storages
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

upgrade_from_19_02_x_tokens(Config) ->
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

    {P1, _, _, _} = PToken1 = create_legacy_provider_tokens(Config),
    {P2, _, _, _} = PToken2 = create_legacy_provider_tokens(Config),
    {P3, _, _, _} = PToken3 = create_legacy_provider_tokens(Config),
    {P4, _, _, _} = PToken4 = create_legacy_provider_tokens(Config),

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
    {ok, [{<<"root token">>, _}]} = oz_test_utils:call_oz(Config, token_logic, list_provider_named_tokens, [?ROOT, P1]),
    {ok, [{<<"root token">>, _}]} = oz_test_utils:call_oz(Config, token_logic, list_provider_named_tokens, [?ROOT, P2]),
    {ok, [{<<"root token">>, _}]} = oz_test_utils:call_oz(Config, token_logic, list_provider_named_tokens, [?ROOT, P3]),
    {ok, [{<<"root token">>, _}]} = oz_test_utils:call_oz(Config, token_logic, list_provider_named_tokens, [?ROOT, P4]),

    % Check all user tokens
    lists:foreach(fun({UserId, TokenNonce, Secret, Serialized}) ->
        % Check if the migrated token has been saved in the new model
        {ok, NamedToken} = ?assertMatch({ok, #od_token{
            name = <<"legacy token ", _/binary>>,
            version = 1,
            subject = ?SUB(user, UserId),
            type = ?ACCESS_TOKEN,
            caveats = [<<"time < ", _/binary>>],
            secret = Secret,
            metadata = #{},
            revoked = false
        }}, oz_test_utils:call_oz(Config, token_logic, get_named_token_by_nonce, [?USER(UserId), TokenNonce])),
        % Make sure the migrated token has exactly the same serialized form
        % as the original one
        Token = oz_test_utils:call_oz(Config, token_logic, named_token_to_token, [TokenNonce, NamedToken]),
        % Subject is not supported in tokens version 1
        {ok, Deserialized} = tokens:deserialize(Serialized),
        ?assertEqual(Token#token{subject = ?SUB(nobody)}, Deserialized),
        % The legacy onedata_auth record should have been removed
        ?assertEqual({error, not_found}, oz_test_utils:call_oz(Config, onedata_auth, get, [TokenNonce])),
        % The migrated token should still work
        ?assertMatch({ok, ?USER(UserId)}, oz_test_utils:call_oz(
            Config, token_auth, verify_access_token, [Deserialized, undefined, undefined]
        ))
    end, User1Tokens ++ User2Tokens ++ User3Tokens ++ User4Tokens ++ User5Tokens),

    % Check all provider tokens
    lists:foreach(fun({ProviderId, TokenNonce, Secret, RootTokenSerialized}) ->
        % Check if the migrated token has been saved in the new model
        {ok, NamedToken} = ?assertMatch({ok, #od_token{
            name = <<"root token">>,
            version = 1,
            subject = ?SUB(?ONEPROVIDER, ProviderId),
            type = ?ACCESS_TOKEN,
            caveats = [],
            secret = Secret,
            metadata = #{},
            revoked = false
        }}, oz_test_utils:call_oz(Config, token_logic, get_named_token_by_nonce, [?PROVIDER(ProviderId), TokenNonce])),
        % Make sure the migrated token has exactly the same serialized form
        % as the original one
        Token = oz_test_utils:call_oz(Config, token_logic, named_token_to_token, [TokenNonce, NamedToken]),
        % Subject is not supported in tokens version 1
        {ok, RootTokenDeserialized} = tokens:deserialize(RootTokenSerialized),
        ?assertEqual(Token#token{subject = ?SUB(nobody)}, RootTokenDeserialized),
        % The legacy macaroon_auth record should have been removed
        ?assertEqual({error, not_found}, oz_test_utils:call_oz(Config, macaroon_auth, get, [TokenNonce])),
        % The migrated token should still work
        ?assertMatch({ok, ?PROVIDER(ProviderId)}, oz_test_utils:call_oz(
            Config, token_auth, verify_access_token, [RootTokenDeserialized, undefined, undefined]
        ))
    end, [PToken1, PToken2, PToken3, PToken4]).


upgrade_from_19_02_x_storages(Config) ->
    {P1, SpacesMap1} = create_provider_with_n_legacy_spaces(Config, 8),
    {P2, SpacesMap2} = create_provider_with_n_legacy_spaces(Config, 32),

    ?assertEqual({ok, 2}, oz_test_utils:call_oz(Config, node_manager_plugin, upgrade_cluster, [1])),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, P1Doc} = oz_test_utils:call_oz(Config, od_provider, get, [P1]),
    {ok, P2Doc} = oz_test_utils:call_oz(Config, od_provider, get, [P2]),

    % Check that there are no legacy spaces left
    ?assertEqual({ok, #{}}, oz_test_utils:call_oz(Config, provider_logic, get_legacy_spaces, [P1Doc])),
    ?assertEqual({ok, #{}}, oz_test_utils:call_oz(Config, provider_logic, get_legacy_spaces, [P2Doc])),

    % Check that virtual storage has been created
    ?assertEqual(true, oz_test_utils:call_oz(Config, provider_logic, has_storage, [P1, P1])),
    ?assertEqual(true, oz_test_utils:call_oz(Config, provider_logic, has_storage, [P2, P2])),

    % Check spaces are supported by correct storage
    lists:foreach(fun(SpaceId) ->
        ?assertEqual(true, oz_test_utils:call_oz(Config, space_logic, is_supported_by_storage, [SpaceId, P1])),
        ?assertEqual(true, oz_test_utils:call_oz(Config, space_logic, is_supported_by_provider, [SpaceId, P1])),
        ?assertEqual(true, oz_test_utils:call_oz(Config, storage_logic, supports_space, [P1, SpaceId])),
        ?assertEqual(true, oz_test_utils:call_oz(Config, provider_logic, supports_space, [P1, SpaceId])),

        ?assertEqual(false, oz_test_utils:call_oz(Config, space_logic, is_supported_by_storage, [SpaceId, P2])),
        ?assertEqual(false, oz_test_utils:call_oz(Config, space_logic, is_supported_by_provider, [SpaceId, P2])),
        ?assertEqual(false, oz_test_utils:call_oz(Config, storage_logic, supports_space, [P2, SpaceId])),
        ?assertEqual(false, oz_test_utils:call_oz(Config, provider_logic, supports_space, [P2, SpaceId]))
    end, maps:keys(SpacesMap1)),

    lists:foreach(fun(SpaceId) ->
        ?assertEqual(true, oz_test_utils:call_oz(Config, space_logic, is_supported_by_storage, [SpaceId, P2])),
        ?assertEqual(true, oz_test_utils:call_oz(Config, space_logic, is_supported_by_provider, [SpaceId, P2])),
        ?assertEqual(true, oz_test_utils:call_oz(Config, storage_logic, supports_space, [P2, SpaceId])),
        ?assertEqual(true, oz_test_utils:call_oz(Config, provider_logic, supports_space, [P2, SpaceId])),

        ?assertEqual(false, oz_test_utils:call_oz(Config, space_logic, is_supported_by_storage, [SpaceId, P1])),
        ?assertEqual(false, oz_test_utils:call_oz(Config, space_logic, is_supported_by_provider, [SpaceId, P1])),
        ?assertEqual(false, oz_test_utils:call_oz(Config, storage_logic, supports_space, [P1, SpaceId])),
        ?assertEqual(false, oz_test_utils:call_oz(Config, provider_logic, supports_space, [P1, SpaceId]))
    end, maps:keys(SpacesMap2)),


    AddIntermediaries = fun(Map, StId) ->
        maps:map(fun(_SpaceId, SupportSize) ->
            {SupportSize, [{od_storage, StId}]}
        end, Map)
    end,

   % Check support size and intermediaries
    P1EffSpaces = P1Doc#document.value#od_provider.eff_spaces,
    P2EffSpaces = P2Doc#document.value#od_provider.eff_spaces,
    ?assertEqual(P1EffSpaces, AddIntermediaries(SpacesMap1, P1)),
    ?assertEqual(P2EffSpaces, AddIntermediaries(SpacesMap2, P2)).



create_n_legacy_client_tokens(Config, UserId, Count) ->
    lists:map(fun(_) ->
        Secret = tokens:generate_secret(),
        {ok, Doc} = oz_test_utils:call_oz(Config, onedata_auth, save, [#document{
            value = #onedata_auth{secret = Secret, user_id = UserId}
        }]),
        Nonce = Doc#document.key,
        ExpirationTime = oz_test_utils:cluster_time_seconds(Config) + 31536000, % 1 year
        Macaroon = macaroon:create(oz_test_utils:oz_domain(Config), Secret, Nonce),
        Confined = macaroon:add_first_party_caveat(Macaroon, ["time < ", integer_to_binary(ExpirationTime)]),
        Token = #token{
            version = 1,
            onezone_domain = oz_test_utils:oz_domain(Config),
            nonce = Nonce,
            persistent = true,
            type = ?ACCESS_TOKEN,
            macaroon = Confined
        },
        {ok, Serialized} = tokens:serialize(Token),
        oz_test_utils:call_oz(Config, od_user, update, [UserId, fun(User = #od_user{client_tokens = ClientTokens}) ->
            {ok, User#od_user{client_tokens = [Serialized | ClientTokens]}}
        end]),
        {UserId, Nonce, Secret, Serialized}
    end, lists:seq(1, Count)).


create_legacy_provider_tokens(Config) ->
    ProviderId = datastore_utils:gen_key(),
    Secret = tokens:generate_secret(),
    {ok, RootTokenNonce} = oz_test_utils:call_oz(Config, macaroon_auth, create, [
        Secret, ?PROVIDER(ProviderId)
    ]),
    Prototype = #token{
        version = 1,
        onezone_domain = oz_test_utils:oz_domain(Config),
        nonce = RootTokenNonce,
        persistent = true,
        type = ?ACCESS_TOKEN
    },
    RootToken = tokens:construct(Prototype, Secret, []),

    ProviderRecord = #od_provider{
        name = ?UNIQUE_STRING,
        root_token = RootTokenNonce,
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
    {ProviderId, RootTokenNonce, Secret, SerializedRootToken}.


create_provider_with_n_legacy_spaces(Config, SpacesNum) ->
    ProviderId = datastore_utils:gen_key(),

    Spaces = lists:foldl(fun(Num, Acc) ->
        {ok, S} = oz_test_utils:create_space(Config, ?ROOT),
        Acc#{S => Num}
    end, #{}, lists:seq(1, SpacesNum)),

    ProviderRecord = #od_provider{
        name = ?UNIQUE_STRING,
        subdomain_delegation = false,
        domain = <<(?UNIQUE_STRING)/binary, ".example.com">>,
        admin_email = <<(?UNIQUE_STRING)/binary, "@example.com">>,
        legacy_spaces = Spaces
    },
    {ok, _} = oz_test_utils:call_oz(Config, od_provider, create, [
        #document{key = ProviderId, value = ProviderRecord}
    ]),
    {ProviderId, Spaces}.

