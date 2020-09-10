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
-include_lib("ctool/include/space_support/support_stage.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").


%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    upgrade_from_19_02_x_tokens/1,
    upgrade_from_19_02_x_storages/1,
    upgrade_from_20_02_0_beta3_space_support_info/1,
    upgrade_from_19_02_space_lifecycle_with_providers_of_different_versions/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    upgrade_from_19_02_x_tokens,
    upgrade_from_19_02_x_storages,
    upgrade_from_20_02_0_beta3_space_support_info,
    upgrade_from_19_02_space_lifecycle_with_providers_of_different_versions
]).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    ozt:init_per_suite(Config).

end_per_suite(_Config) ->
    ssl:stop().

init_per_testcase(_, Config) ->
    ozt_mocks:mock_time(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unmock_time(),
    ozt:delete_all_entities(),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

upgrade_from_19_02_x_tokens(_Config) ->
    User1 = ozt_users:create(),
    User2 = ozt_users:create(),
    User3 = ozt_users:create(),
    User4 = ozt_users:create(),
    User5 = ozt_users:create(),

    User1Tokens = create_n_legacy_client_tokens(User1, 0),
    User2Tokens = create_n_legacy_client_tokens(User2, 2),
    User3Tokens = create_n_legacy_client_tokens(User3, 7),
    User4Tokens = create_n_legacy_client_tokens(User4, 1),
    User5Tokens = create_n_legacy_client_tokens(User5, 13),

    {P1, _, _, _} = PToken1 = create_legacy_provider(),
    {P2, _, _, _} = PToken2 = create_legacy_provider(),
    {P3, _, _, _} = PToken3 = create_legacy_provider(),
    {P4, _, _, _} = PToken4 = create_legacy_provider(),

    % Force trigger a cluster upgrade
    ?assertEqual({ok, 2}, ozt:rpc(node_manager_plugin, upgrade_cluster, [1])),

    ?assertEqual(27, length(list_all_tokens())),

    ?assertEqual(0, length(list_user_named_tokens(User1))),
    ?assertEqual(2, length(list_user_named_tokens(User2))),
    ?assertEqual(7, length(list_user_named_tokens(User3))),
    ?assertEqual(1, length(list_user_named_tokens(User4))),
    ?assertEqual(13, length(list_user_named_tokens(User5))),

    ?assertMatch([_], list_provider_named_tokens(P1)),
    ?assertMatch([_], list_provider_named_tokens(P2)),
    ?assertMatch([_], list_provider_named_tokens(P3)),
    ?assertMatch([_], list_provider_named_tokens(P4)),

    % Check all user tokens
    lists:foreach(fun({UserId, TokenId, Secret, LegacyTokenSerialized}) ->
        {ok, LegacyToken} = tokens:deserialize(LegacyTokenSerialized),
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
        }}}, ozt:rpc(od_token, get, [TokenId])),

        % Subject is not supported in tokens version 1 when deserializing a
        % token (defaults to nobody), but it is filled in when a named token is retrieved.
        LegacyTokenWithSubject = LegacyToken#token{subject = ?SUB(user, UserId)},
        ?assertMatch({ok, #{
            <<"name">> := <<"legacy token ", _/binary>>,
            <<"id">> := TokenId,
            <<"subject">> := ?SUB(user, UserId),
            <<"type">> := ?ACCESS_TOKEN,
            <<"caveats">> := [#cv_time{}],
            <<"metadata">> := #{<<"creationTime">> := _},
            <<"revoked">> := false,
            % Legacy tokens do not include subject (defaults to nobody)
            <<"token">> := LegacyTokenWithSubject
        }}, ozt:rpc(token_logic, get_named_token, [?USER(UserId), TokenId])),

        % Make sure the migrated token has exactly the same serialized form as the original one
        Token = ozt:rpc(od_token, named_token_to_token, [TokenId, NamedToken]),
        ?assertEqual({ok, LegacyTokenSerialized}, tokens:serialize(Token)),
        % The legacy onedata_auth record should have been removed
        ?assertEqual({error, not_found}, ozt:rpc(onedata_auth, get, [TokenId])),
        % The migrated token should still work
        ?assertMatch({true, ?USER(UserId)}, ozt_tokens:authenticate(LegacyToken))
    end, User1Tokens ++ User2Tokens ++ User3Tokens ++ User4Tokens ++ User5Tokens),

    % Check all provider tokens
    lists:foreach(fun({ProviderId, TokenId, Secret, LegacyRootTokenSerialized}) ->
        {ok, LegacyRootToken} = tokens:deserialize(LegacyRootTokenSerialized),
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
        }}}, ozt:rpc(od_token, get, [TokenId])),

        % Subject is not supported in tokens version 1 when deserializing a
        % token (defaults to nobody), but it is filled in when a named token is retrieved.
        LegacyRootTokenWithSubject = LegacyRootToken#token{subject = ?SUB(?ONEPROVIDER, ProviderId)},
        ?assertMatch({ok, #{
            <<"name">> := <<"root token">>,
            <<"id">> := TokenId,
            <<"subject">> := ?SUB(?ONEPROVIDER, ProviderId),
            <<"type">> := ?ACCESS_TOKEN,
            <<"caveats">> := [],
            <<"metadata">> := #{<<"creationTime">> := _},
            <<"revoked">> := false,
            <<"token">> := LegacyRootTokenWithSubject
        }}, ozt:rpc(token_logic, get_named_token, [?PROVIDER(ProviderId), TokenId])),
        % Make sure the migrated token has exactly the same serialized form as the original one
        Token = ozt:rpc(od_token, named_token_to_token, [TokenId, NamedToken]),
        ?assertEqual({ok, LegacyRootTokenSerialized}, tokens:serialize(Token)),
        % The legacy macaroon_auth record should have been removed
        ?assertEqual({error, not_found}, ozt:rpc(macaroon_auth, get, [TokenId])),
        % The migrated token should still work
        ?assertMatch({true, ?PROVIDER(ProviderId)}, ozt_tokens:authenticate(LegacyRootToken))
    end, [PToken1, PToken2, PToken3, PToken4]).


upgrade_from_19_02_x_storages(_Config) ->
    {P1, SpacesMap1} = create_legacy_provider_with_n_spaces(8),
    {P2, SpacesMap2} = create_legacy_provider_with_n_spaces(32),

    ?assertEqual({ok, 2}, ozt:rpc(node_manager_plugin, upgrade_cluster, [1])),
    ozt:reconcile_entity_graph(),

    {ok, P1Doc} = ozt:rpc(od_provider, get, [P1]),
    {ok, P2Doc} = ozt:rpc(od_provider, get, [P2]),

    % Check that there are no legacy spaces left
    ?assertEqual({ok, #{}}, ozt:rpc(provider_logic, get_legacy_spaces, [P1Doc])),
    ?assertEqual({ok, #{}}, ozt:rpc(provider_logic, get_legacy_spaces, [P2Doc])),

    % Check that virtual storage has been created
    ?assertEqual(true, ozt:rpc(provider_logic, has_storage, [P1, P1])),
    ?assertEqual(true, ozt:rpc(provider_logic, has_storage, [P2, P2])),

    % Check spaces are supported by correct storage
    lists:foreach(fun(SpaceId) ->
        ?assertEqual(true, ozt:rpc(space_logic, is_supported_by_storage, [SpaceId, P1])),
        ?assertEqual(true, ozt:rpc(space_logic, is_supported_by_provider, [SpaceId, P1])),
        ?assertEqual(true, ozt:rpc(storage_logic, supports_space, [P1, SpaceId])),
        ?assertEqual(true, ozt:rpc(provider_logic, supports_space, [P1, SpaceId])),

        ?assertEqual(false, ozt:rpc(space_logic, is_supported_by_storage, [SpaceId, P2])),
        ?assertEqual(false, ozt:rpc(space_logic, is_supported_by_provider, [SpaceId, P2])),
        ?assertEqual(false, ozt:rpc(storage_logic, supports_space, [P2, SpaceId])),
        ?assertEqual(false, ozt:rpc(provider_logic, supports_space, [P2, SpaceId]))
    end, maps:keys(SpacesMap1)),

    lists:foreach(fun(SpaceId) ->
        ?assertEqual(true, ozt:rpc(space_logic, is_supported_by_storage, [SpaceId, P2])),
        ?assertEqual(true, ozt:rpc(space_logic, is_supported_by_provider, [SpaceId, P2])),
        ?assertEqual(true, ozt:rpc(storage_logic, supports_space, [P2, SpaceId])),
        ?assertEqual(true, ozt:rpc(provider_logic, supports_space, [P2, SpaceId])),

        ?assertEqual(false, ozt:rpc(space_logic, is_supported_by_storage, [SpaceId, P1])),
        ?assertEqual(false, ozt:rpc(space_logic, is_supported_by_provider, [SpaceId, P1])),
        ?assertEqual(false, ozt:rpc(storage_logic, supports_space, [P1, SpaceId])),
        ?assertEqual(false, ozt:rpc(provider_logic, supports_space, [P1, SpaceId]))
    end, maps:keys(SpacesMap2)),

    ?assertEqual(
        SpacesMap1,
        ozt:rpc(entity_graph, get_relations_with_attrs, [effective, bottom_up, od_space, P1Doc#document.value])
    ),
    ?assertEqual(
        SpacesMap2,
        ozt:rpc(entity_graph, get_relations_with_attrs, [effective, bottom_up, od_space, P2Doc#document.value])
    ),

    maps:map(fun(SpaceId, _SupportSize) ->
        ?assertEqual(
            [{od_storage, P1}],
            ozt:rpc(entity_graph, get_intermediaries, [bottom_up, od_space, SpaceId, P1Doc#document.value])
        )
    end, SpacesMap1),

    maps:map(fun(SpaceId, _SupportSize) ->
        ?assertEqual(
            [{od_storage, P2}],
            ozt:rpc(entity_graph, get_intermediaries, [bottom_up, od_space, SpaceId, P2Doc#document.value])
        )
    end, SpacesMap2).


upgrade_from_20_02_0_beta3_space_support_info(_Config) ->
    SpaceAlpha = ozt_spaces:create(),
    SpaceBeta = ozt_spaces:create(),
    SpaceGamma = ozt_spaces:create(),
    SpaceDelta = ozt_spaces:create(),
    SpaceOmega = ozt_spaces:create(),

    {P1, _} = create_legacy_provider_with_spaces([SpaceAlpha, SpaceBeta, SpaceGamma]),
    {P2, _} = create_legacy_provider_with_spaces([SpaceAlpha, SpaceGamma, SpaceDelta]),
    {P3, _} = create_legacy_provider_with_spaces([SpaceGamma, SpaceDelta, SpaceOmega]),

    ?assertEqual({ok, 2}, ozt:rpc(node_manager_plugin, upgrade_cluster, [1])),
    ?assertEqual({ok, 3}, ozt:rpc(node_manager_plugin, upgrade_cluster, [2])),
    ozt:reconcile_entity_graph(),

    DefaultParameters = support_parameters:build(global, eager),

    AlphaRecord = ozt_spaces:get(SpaceAlpha),
    BetaRecord = ozt_spaces:get(SpaceBeta),
    GammaRecord = ozt_spaces:get(SpaceGamma),
    DeltaRecord = ozt_spaces:get(SpaceDelta),
    OmegaRecord = ozt_spaces:get(SpaceOmega),

    ?assertEqual(AlphaRecord#od_space.support_parameters_per_provider, #{
        P1 => DefaultParameters, P2 => DefaultParameters
    }),
    ?assertEqual(BetaRecord#od_space.support_parameters_per_provider, #{
        P1 => DefaultParameters
    }),
    ?assertEqual(GammaRecord#od_space.support_parameters_per_provider, #{
        P1 => DefaultParameters, P2 => DefaultParameters, P3 => DefaultParameters
    }),
    ?assertEqual(DeltaRecord#od_space.support_parameters_per_provider, #{
        P2 => DefaultParameters, P3 => DefaultParameters
    }),
    ?assertEqual(OmegaRecord#od_space.support_parameters_per_provider, #{
        P3 => DefaultParameters
    }),

    ?assertEqual(AlphaRecord#od_space.support_stage_per_provider, #{
        P1 => ?LEGACY_SUPPORT_STAGE_DETAILS, P2 => ?LEGACY_SUPPORT_STAGE_DETAILS
    }),
    ?assertEqual(BetaRecord#od_space.support_stage_per_provider, #{
        P1 => ?LEGACY_SUPPORT_STAGE_DETAILS
    }),
    ?assertEqual(GammaRecord#od_space.support_stage_per_provider, #{
        P1 => ?LEGACY_SUPPORT_STAGE_DETAILS, P2 => ?LEGACY_SUPPORT_STAGE_DETAILS, P3 => ?LEGACY_SUPPORT_STAGE_DETAILS
    }),
    ?assertEqual(DeltaRecord#od_space.support_stage_per_provider, #{
        P2 => ?LEGACY_SUPPORT_STAGE_DETAILS, P3 => ?LEGACY_SUPPORT_STAGE_DETAILS
    }),
    ?assertEqual(OmegaRecord#od_space.support_stage_per_provider, #{
        P3 => ?LEGACY_SUPPORT_STAGE_DETAILS
    }),

    ?assertEqual(sync_progress_per_provider(SpaceAlpha), #{
        P1 => #{P1 => {1, 0}, P2 => {1, 0}},
        P2 => #{P1 => {1, 0}, P2 => {1, 0}}
    }),
    ?assertEqual(sync_progress_per_provider(SpaceBeta), #{
        P1 => #{P1 => {1, 0}}
    }),
    ?assertEqual(sync_progress_per_provider(SpaceGamma), #{
        P1 => #{P1 => {1, 0}, P2 => {1, 0}, P3 => {1, 0}},
        P2 => #{P1 => {1, 0}, P2 => {1, 0}, P3 => {1, 0}},
        P3 => #{P1 => {1, 0}, P2 => {1, 0}, P3 => {1, 0}}
    }),
    ?assertEqual(sync_progress_per_provider(SpaceDelta), #{
        P2 => #{P2 => {1, 0}, P3 => {1, 0}},
        P3 => #{P2 => {1, 0}, P3 => {1, 0}}
    }),
    ?assertEqual(sync_progress_per_provider(SpaceOmega), #{
        P3 => #{P3 => {1, 0}}
    }).


upgrade_from_19_02_space_lifecycle_with_providers_of_different_versions(_Config) ->
    SpaceAlpha = ozt_spaces:create(),
    SpaceBeta = ozt_spaces:create(),
    {P1, _} = create_legacy_provider_with_spaces([SpaceAlpha, SpaceBeta]),
    {P2, _} = create_legacy_provider_with_spaces([]),
    {P3, _} = create_legacy_provider_with_spaces([SpaceAlpha]),

    ?assertEqual({ok, 2}, ozt:rpc(node_manager_plugin, upgrade_cluster, [1])),
    ?assertEqual({ok, 3}, ozt:rpc(node_manager_plugin, upgrade_cluster, [2])),

    GetSupportStagePerProvider = fun(SpaceId) ->
        Space = ozt_spaces:get(SpaceId),
        Space#od_space.support_stage_per_provider
    end,

    ?assertEqual(GetSupportStagePerProvider(SpaceAlpha), #{
        P1 => ?LEGACY_SUPPORT_STAGE_DETAILS,
        P3 => ?LEGACY_SUPPORT_STAGE_DETAILS
    }),
    ?assertEqual(GetSupportStagePerProvider(SpaceBeta), #{
        P1 => ?LEGACY_SUPPORT_STAGE_DETAILS
    }),

    % at this point, all providers are in legacy versions

    % support SpaceBeta using the legacy support procedure
    ozt_providers:support_space_with_legacy_storage(P2, SpaceBeta),
    ?assertEqual(GetSupportStagePerProvider(SpaceBeta), #{
        P1 => ?LEGACY_SUPPORT_STAGE_DETAILS,
        P2 => ?LEGACY_SUPPORT_STAGE_DETAILS
    }),

    % simulate P2 being upgraded to the newest version
    P2Storage = ozt_providers:create_storage(P2),
    upgrade_legacy_support(P2, P2Storage, SpaceBeta),
    ?assertEqual(GetSupportStagePerProvider(SpaceBeta), #{
        P1 => ?LEGACY_SUPPORT_STAGE_DETAILS,
        P2 => #support_stage_details{provider_stage = active, per_storage = #{P2Storage => active}}
    }),

    % simulate P3 being upgraded to the newest version
    P3Storage = ozt_providers:create_storage(P3),
    upgrade_legacy_support(P3, P3Storage, SpaceAlpha),
    ?assertEqual(GetSupportStagePerProvider(SpaceAlpha), #{
        P1 => ?LEGACY_SUPPORT_STAGE_DETAILS,
        P3 => #support_stage_details{provider_stage = active, per_storage = #{P3Storage => active}}
    }),
    ?assertEqual(sync_progress_per_provider(SpaceAlpha), #{
        P1 => #{P1 => {1, 0}, P3 => {1, 0}},
        P3 => #{P1 => {1, 0}, P3 => {1, 0}}
    }),

    % support the SpaceBeta with P3 using the modern procedure
    ozt_providers:support_space(P3, P3Storage, SpaceBeta),
    ?assertEqual(GetSupportStagePerProvider(SpaceBeta), #{
        P1 => ?LEGACY_SUPPORT_STAGE_DETAILS,
        P2 => #support_stage_details{provider_stage = active, per_storage = #{P2Storage => active}},
        P3 => #support_stage_details{provider_stage = active, per_storage = #{P3Storage => active}}
    }),
    ?assertEqual(sync_progress_per_provider(SpaceBeta), #{
        P1 => #{P1 => {1, 0}, P2 => {1, 0}, P3 => {1, 0}},
        P2 => #{P1 => {1, 0}, P2 => {1, 0}, P3 => {1, 0}},
        P3 => #{P1 => {1, 0}, P2 => {1, 0}, P3 => {1, 0}}
    }).

%% @todo VFS-6311 implement the following tests when space lifecycle integration is complete
%%  * space is unsupported by a legacy provider
%%  * space is re-supported by a legacy provider (before or after upgrade)
%%  * support size is modified by a legacy provider
%% Consider moving the tests to a new SUITE (space_lifecycle?)

%%%===================================================================
%%% Helper functions
%%%===================================================================

create_n_legacy_client_tokens(UserId, Count) ->
    lists:map(fun(_) ->
        Secret = tokens:generate_secret(),
        {ok, Doc} = ozt:rpc(onedata_auth, save, [#document{
            value = #onedata_auth{secret = Secret, user_id = UserId}
        }]),
        TokenId = Doc#document.key,
        ExpirationTime = ozt:cluster_time_seconds() + 31536000, % 1 year
        Macaroon = macaroon:create(ozt:get_domain(), Secret, TokenId),
        Confined = macaroon:add_first_party_caveat(Macaroon, ["time < ", integer_to_binary(ExpirationTime)]),
        Token = #token{
            version = 1,
            onezone_domain = ozt:get_domain(),
            id = TokenId,
            persistence = named,
            type = ?ACCESS_TOKEN,
            macaroon = Confined
        },
        {ok, Serialized} = tokens:serialize(Token),
        ozt:rpc(od_user, update, [UserId, fun(User = #od_user{client_tokens = ClientTokens}) ->
            {ok, User#od_user{client_tokens = [Serialized | ClientTokens]}}
        end]),
        {UserId, TokenId, Secret, Serialized}
    end, lists:seq(1, Count)).


create_legacy_provider() ->
    ProviderId = datastore_key:new(),
    Secret = tokens:generate_secret(),
    {ok, RootTokenId} = ozt:rpc(macaroon_auth, create, [
        Secret, ?PROVIDER(ProviderId)
    ]),
    Prototype = #token{
        version = 1,
        onezone_domain = ozt:get_domain(),
        id = RootTokenId,
        persistence = named,
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
    {ok, _} = ozt:rpc(od_provider, create, [
        #document{key = ProviderId, value = ProviderRecord}
    ]),
    ozt:rpc(cluster_logic, create_oneprovider_cluster, [
        undefined, ProviderId
    ]),
    {ok, SerializedRootToken} = tokens:serialize(RootToken),
    {ProviderId, RootTokenId, Secret, SerializedRootToken}.


create_legacy_provider_with_n_spaces(SpacesNum) ->
    SpaceIds = lists:map(fun(_) -> ozt_spaces:create() end, lists:seq(1, SpacesNum)),
    create_legacy_provider_with_spaces(SpaceIds).


create_legacy_provider_with_spaces(SpaceIds) ->
    {ProviderId, _, _, _} = create_legacy_provider(),
    Spaces = lists:foldl(fun(SpaceId, Acc) ->
        Acc#{SpaceId => ozt_spaces:minimum_support_size()}
    end, #{}, SpaceIds),
    {ok, _} = ozt:rpc(od_provider, update, [ProviderId, fun(Provider) ->
        {ok, Provider#od_provider{legacy_spaces = Spaces}}
    end]),
    {ProviderId, Spaces}.


upgrade_legacy_support(ProviderId, StorageId, SpaceId) ->
    Token = ozt_providers:get_root_token(ProviderId),
    ?assertMatch({ok, _}, ozt_gs:connect_and_request(oneprovider, {token, Token}, #gs_req_graph{
        gri = ?GRI(od_storage, StorageId, {upgrade_legacy_support, SpaceId}),
        operation = create,
        data = #{}
    })).


sync_progress_per_provider(SpaceId) ->
    {ok, #space_stats{
        sync_progress_per_provider = SyncProgressPerProvider
    }} = ozt:rpc(space_logic, get_stats, [?ROOT, SpaceId]),
    SyncProgressPerProvider.


list_all_tokens() ->
    {ok, Tokens} = ozt:rpc(token_logic, list, [?ROOT]),
    Tokens.


list_user_named_tokens(UserId) ->
    {ok, Tokens} = ozt:rpc(token_logic, list_user_named_tokens, [?ROOT, UserId]),
    Tokens.


list_provider_named_tokens(ProviderId) ->
    {ok, Tokens} = ozt:rpc(token_logic, list_provider_named_tokens, [?ROOT, ProviderId]),
    Tokens.
