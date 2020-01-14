%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning share API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(share_api_test_SUITE).
-author("Bartosz Walkowicz").

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    list_test/1,
    create_test/1,
    get_test/1,
    update_test/1,
    delete_test/1,
    choose_provider_for_public_view_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        update_test,
        delete_test,
        choose_provider_for_public_view_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_test(Config) ->
    % Make sure that shares created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME2),

    ExpShares = lists:map(
        fun(SpaceId) ->
            ShareId = ?UNIQUE_STRING,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, SpaceId
            ),
            ShareId
        end, [S1, S1, S1, S2, S2]
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SHARES_LIST]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/shares">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"shares">> => ExpShares}
        },
        logic_spec = #logic_spec{
            module = share_logic,
            function = list,
            args = [auth],
            expected_result = ?OK_LIST(ExpShares)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also share_logic:exist function
    lists:foreach(
        fun(ShareId) ->
            ?assert(oz_test_utils:call_oz(
                Config, share_logic, exists, [ShareId])
            )
        end, ExpShares
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, share_logic, exists, [<<"asdiucyaie827346w">>])
    ).


create_test(Config) ->
    % create space with 3 users:
    %   U3 gets the SPACE_MANAGE_SHARES privilege
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_MANAGE_SHARES
    ),
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:space_add_user(Config, S1, U3),
    oz_test_utils:space_set_user_privileges(Config, S1, U3, [
        ?SPACE_MANAGE_SHARES
    ], []),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    VerifyFun = fun(ShareId) ->
        {ok, Share} = oz_test_utils:get_share(Config, ShareId),
        PublicURL = oz_test_utils:get_share_public_url(Config, ShareId),
        ?assertMatch(#od_share{
            name = ?CORRECT_NAME, space = S1,
            root_file = ?ROOT_FILE_ID, public_url = PublicURL
        }, Share),
        true
    end,

    BadDataValues = [
        {<<"shareId">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"shareId">>)},
        {<<"shareId">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"shareId">>)},
        {<<"rootFileId">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"rootFileId">>)},
        {<<"rootFileId">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"rootFileId">>)},
        {<<"fileType">>, 1234, ?ERROR_BAD_VALUE_ATOM(<<"fileType">>)},
        {<<"fileType">>, <<"">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"fileType">>, [file, dir])},
        {<<"fileType">>, atom, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"fileType">>, [file, dir])}
    ],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U2},
                {user, U3}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        % CREATE operation is not supported in REST API (reserved for Oneprovider logic via GraphSync)
        logic_spec = #logic_spec{
            module = share_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_share, aspect = instance},
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"handleId">> => null,
                <<"name">> => ?CORRECT_NAME,
                <<"rootFileId">> => ?ROOT_FILE_ID,
                <<"spaceId">> => S1,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    VerifyFun(Id)
                end
            })
        },
        data_spec = DataSpec = #data_spec{
            required = [
                <<"shareId">>, <<"name">>, <<"rootFileId">>, <<"spaceId">>
            ],
            optional = [
                <<"fileType">>
            ],
            correct_values = #{
                <<"shareId">> => [fun() -> ?UNIQUE_STRING end],
                <<"name">> => [?CORRECT_NAME],
                <<"rootFileId">> => [?ROOT_FILE_ID],
                <<"fileType">> => [file, dir],
                <<"spaceId">> => [S1]
            },
            bad_values = lists:append([
                [{<<"spaceId">>, <<"">>, ?ERROR_FORBIDDEN},
                    {<<"spaceId">>, <<"asdq4ewfs">>, ?ERROR_FORBIDDEN}],
                BadDataValues,
                ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)])
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Root client bypasses authorization checks,
    % hence wrong values of handleServiceId or resourceId
    % cause validation errors rather than authorization errors.
    RootApiTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_SHARES_CREATE]},
                root
            ]
        },
        data_spec = DataSpec#data_spec{
            bad_values = lists:append([
                [{<<"spaceId">>, <<"">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"spaceId">>)},
                    {<<"spaceId">>, 1234, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"spaceId">>)}],
                BadDataValues,
                ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)])
        }
    },
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec)).


get_test(Config) ->
    get_test(Config, ?SHARE_ID_2, dir),
    get_test(Config, ?SHARE_ID_1, file).

get_test(Config, ShareId, FileType) ->
    % create space with 2 users:
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, ShareId} = oz_test_utils:create_share(Config, ?ROOT, #{
        <<"shareId">> => ShareId,
        <<"spaceId">> => S1,
        <<"name">> => ?SHARE_NAME1,
        <<"rootFileId">> => ?ROOT_FILE_ID,
        <<"fileType">> => FileType
    }),
    SharePublicURL = oz_test_utils:get_share_public_url(Config, ShareId),
    SharePublicDetails = #{
        <<"name">> => ?SHARE_NAME1,
        <<"publicUrl">> => SharePublicURL,
        <<"rootFileId">> => ?ROOT_FILE_ID,
        <<"fileType">> => atom_to_binary(FileType, utf8),
        <<"handleId">> => null
    },
    SharePrivateDetails = SharePublicDetails#{<<"spaceId">> => S1},

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SHARES_VIEW]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/shares/">>, ShareId],
            expected_code = ?HTTP_200_OK,
            expected_body = SharePrivateDetails#{
                <<"handleId">> => null,
                % In REST, the rootFileId GUID is converted to ObjectID
                <<"rootFileId">> => element(2, {ok, _} = file_id:guid_to_objectid(?ROOT_FILE_ID)),
                <<"shareId">> => ShareId
            }
        },
        logic_spec = #logic_spec{
            module = share_logic,
            function = get,
            args = [auth, ShareId],
            expected_result = ?OK_TERM(
                fun(#od_share{
                    name = ShareName, public_url = PublicURL,
                    space = Space, handle = undefined,
                    root_file = RootFile, file_type = ReceivedFileType
                }) ->
                    ?assertEqual(?SHARE_NAME1, ShareName),
                    ?assertEqual(?ROOT_FILE_ID, RootFile),
                    ?assertEqual(FileType, ReceivedFileType),
                    ?assertEqual(Space, S1),
                    ?assertEqual(SharePublicURL, PublicURL)
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_share, id = ShareId, aspect = instance},
            expected_result = ?OK_MAP_CONTAINS(SharePrivateDetails#{
                <<"handleId">> => null,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(ShareId, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check public data
    GetPublicDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {admin, [?OZ_SHARES_VIEW]},
                {user, NonAdmin},
                {user, U1},
                {user, U2}
            ]
        },
        logic_spec = #logic_spec{
            module = share_logic,
            function = get_public_data,
            args = [auth, ShareId],
            expected_result = ?OK_MAP_CONTAINS(SharePublicDetails#{
                <<"fileType">> => FileType % atom is expected
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_share, id = ShareId, aspect = instance, scope = public},
            expected_result = ?OK_MAP_CONTAINS(SharePublicDetails#{
                <<"handleId">> => null,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(ShareId, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPublicDataApiTestSpec)).


update_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_MANAGE_SHARES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        ShareId = ?UNIQUE_STRING,
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
        ),
        #{shareId => ShareId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{shareId := ShareId}, Data) ->
        {ok, Share} = oz_test_utils:get_share(Config, ShareId),
        ExpName = case ShouldSucceed of
            false -> ?SHARE_NAME1;
            true -> maps:get(<<"name">>, Data)
        end,
        ?assertEqual(ExpName, Share#od_share.name)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SHARES_UPDATE]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/shares/">>, shareId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = share_logic,
            function = update,
            args = [auth, shareId, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_share, id = shareId, aspect = instance},
            expected_result = ?OK_RES
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            correct_values = #{<<"name">> => [?CORRECT_NAME]},
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_MANAGE_SHARES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        ShareId = ?UNIQUE_STRING,
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
        ),
        #{shareId => ShareId}
    end,
    DeleteEntityFun = fun(#{shareId := ShareId} = _Env) ->
        oz_test_utils:delete_share(Config, ShareId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{shareId := ShareId} = _Env, _) ->
        {ok, Shares} = oz_test_utils:list_shares(Config),
        ?assertEqual(lists:member(ShareId, Shares), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SHARES_DELETE]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        % DELETE operation is not supported in REST API (reserved for Oneprovider logic via GraphSync)
        logic_spec = #logic_spec{
            module = share_logic,
            function = delete,
            args = [auth, shareId],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_share, id = shareId, aspect = instance},
            expected_result = ?OK_RES
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


% The SUITE is run on a single node cluster to test caching of chosen providers
% (the cache is local for each node).
choose_provider_for_public_view_test(Config) ->
    % Onezone version is mocked in init_per_testcase to <<"19.09.3">>
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId)),

    {ok, {AlphaOffline, _AlphaToken}} = oz_test_utils:create_provider(Config),
    {ok, {BetaOffline, _BetaToken}} = oz_test_utils:create_provider(Config),
    {ok, {GammaLegacy, GammaToken}} = oz_test_utils:create_provider(Config),
    {ok, {DeltaLegacy, DeltaToken}} = oz_test_utils:create_provider(Config),
    {ok, {SigmaUpToDate, SigmaToken}} = oz_test_utils:create_provider(Config),
    {ok, {OmegaUpToDate, OmegaToken}} = oz_test_utils:create_provider(Config),

    ProviderVersion = fun(PrId) ->
        case PrId of
            AlphaOffline -> <<"19.09.1">>;
            BetaOffline -> <<"18.02.3">>;
            GammaLegacy -> <<"19.02.1">>;
            DeltaLegacy -> <<"19.02.0-rc1">>;
            SigmaUpToDate -> <<"19.09.0-rc1">>;
            OmegaUpToDate -> <<"19.09.2">>
        end
    end,

    AllProviders = [
        AlphaOffline, BetaOffline,
        GammaLegacy, DeltaLegacy,
        SigmaUpToDate, OmegaUpToDate
    ],

    lists:foreach(fun(ProviderId) ->
        oz_test_utils:support_space_by_provider(Config, ProviderId, SpaceId),
        update_provider_version(Config, ProviderId, ProviderVersion(ProviderId))
    end, AllProviders),

    Connections = #{
        GammaLegacy => start_provider_graphsync_channel(Config, GammaToken),
        DeltaLegacy => start_provider_graphsync_channel(Config, DeltaToken),
        SigmaUpToDate => start_provider_graphsync_channel(Config, SigmaToken),
        OmegaUpToDate => start_provider_graphsync_channel(Config, OmegaToken)
    },

    ShareId = str_utils:rand_hex(16),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, SpaceId
    ),

    ChooseProvider = fun() -> oz_test_utils:call_oz(
        Config, share_logic, choose_provider_for_public_view, [ShareId]
    ) end,

    % One of the up to date providers should be chosen
    {ChosenProviderId, ChosenProviderVersion} = ChooseProvider(),
    ?assert(lists:member(ChosenProviderId, [SigmaUpToDate, OmegaUpToDate])),
    ?assertEqual(ProviderVersion(ChosenProviderId), ChosenProviderVersion),
    % Choice should be cached and reused
    ?assertEqual({ChosenProviderId, ChosenProviderVersion}, ChooseProvider()),

    % If the chosen provider goes down, another up to date provider should be chosen
    terminate_provider_graphsync_channel(Config, ChosenProviderId, maps:get(ChosenProviderId, Connections)),
    [TheOtherUpToDate] = [SigmaUpToDate, OmegaUpToDate] -- [ChosenProviderId],
    ?assertEqual({TheOtherUpToDate, ProviderVersion(TheOtherUpToDate)}, ChooseProvider()),
    ?assertEqual({TheOtherUpToDate, ProviderVersion(TheOtherUpToDate)}, ChooseProvider()),

    % When the second up to date provider goes down, a legacy one should be picked
    terminate_provider_graphsync_channel(Config, TheOtherUpToDate, maps:get(TheOtherUpToDate, Connections)),
    {LegacyProviderId, LegacyProviderVersion} = ChooseProvider(),
    ?assert(lists:member(LegacyProviderId, [GammaLegacy, DeltaLegacy])),
    ?assertEqual(ProviderVersion(LegacyProviderId), LegacyProviderVersion),
    ?assertEqual({LegacyProviderId, ProviderVersion(LegacyProviderId)}, ChooseProvider()),

    % If one of the up to date providers go up, eventually it should be used again
    % (after the cache expiration)
    terminate_provider_graphsync_channel(Config, LegacyProviderId, maps:get(LegacyProviderId, Connections)),
    NewConnections = Connections#{
        SigmaUpToDate => start_provider_graphsync_channel(Config, SigmaToken)
    },
    ?assertEqual({SigmaUpToDate, ProviderVersion(SigmaUpToDate)}, ChooseProvider(), 60),

    % If all providers go down, undefined result should be immediately returned
    terminate_provider_graphsync_channel(Config, GammaLegacy, maps:get(GammaLegacy, NewConnections)),
    terminate_provider_graphsync_channel(Config, DeltaLegacy, maps:get(DeltaLegacy, NewConnections)),
    terminate_provider_graphsync_channel(Config, SigmaUpToDate, maps:get(SigmaUpToDate, NewConnections)),
    terminate_provider_graphsync_channel(Config, OmegaUpToDate, maps:get(OmegaUpToDate, NewConnections)),
    ?assertEqual({undefined, undefined}, ChooseProvider()),

    % After some providers go online again and the cache expires, they should be picked again
    start_provider_graphsync_channel(Config, DeltaToken),
    start_provider_graphsync_channel(Config, OmegaToken),
    ?assertEqual({OmegaUpToDate, ProviderVersion(OmegaUpToDate)}, ChooseProvider(), 60).


%% @private
update_provider_version(Config, ProviderId, Version) ->
    {DummyGuiHash, _IndexContent} = oz_test_utils:deploy_dummy_gui(Config, ?OP_WORKER_GUI),
    ?assertEqual(ok, oz_test_utils:call_oz(Config, cluster_logic, update_version_info, [
        ?PROVIDER(ProviderId), ProviderId, ?WORKER, {Version, <<"build-123">>, DummyGuiHash}
    ])).


start_provider_graphsync_channel(Config, ProviderToken) ->
    Url = oz_test_utils:graph_sync_url(Config, provider),
    SSlOpts = [{secure, only_verify_peercert}, {cacerts, oz_test_utils:gui_ca_certs(Config)}],
    {ok, GsClient, _} = gs_client:start_link(
        Url, {token, ProviderToken}, ?SUPPORTED_PROTO_VERSIONS, fun(_) -> ok end, SSlOpts
    ),
    GsClient.


terminate_provider_graphsync_channel(Config, ProviderId, ClientPid) ->
    gs_client:kill(ClientPid),
    ?assertMatch(
        false,
        oz_test_utils:call_oz(Config, provider_connection, is_online, [ProviderId]),
        60
    ).

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


init_per_testcase(choose_provider_for_public_view_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, oz_worker, [passthrough]),
    ok = test_utils:mock_expect(Nodes, oz_worker, get_release_version, fun() ->
        <<"19.09.3">>
    end),
    Config;
init_per_testcase(_, Config) ->
    Config.


end_per_testcase(choose_provider_for_public_view_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, oz_worker);
end_per_testcase(_, _Config) ->
    ok.

