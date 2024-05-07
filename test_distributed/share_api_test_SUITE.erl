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
    get_shared_file_or_directory_data_test/1,
    choose_provider_for_public_share_handling_test/1
]).

all() ->
    ?ALL([
        list_test,
        create_test,
        get_test,
        update_test,
        delete_test,
        get_shared_file_or_directory_data_test,
        choose_provider_for_public_share_handling_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_test(Config) ->
    % Make sure that shares created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, SpacesOwner} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(SpacesOwner), ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(SpacesOwner), ?SPACE_NAME2),

    ExpShares = lists:map(
        fun(SpaceId) ->
            ShareId = ?UNIQUE_STRING,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, ?SHARE_NAME1, SpaceId
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
                {user, SpacesOwner},
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
        % TODO VFS-4520 Tests for GraphSync API
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
    % create space with 4 users:
    %   Owner effectively has all the privileges
    %   U3 gets the SPACE_MANAGE_SHARES privilege
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {SpaceId, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_MANAGE_SHARES
    ),
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:space_add_user(Config, SpaceId, U3),
    oz_test_utils:space_set_user_privileges(Config, SpaceId, U3, [
        ?SPACE_MANAGE_SHARES
    ], []),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ProposedShareId = datastore_key:new(),
    RootFileId = ?GEN_ROOT_FILE_ID(SpaceId, ProposedShareId),

    VerifyFun = fun(ShareId, Data) ->
        ?assertEqual(ShareId, ProposedShareId),
        {ok, Share} = oz_test_utils:get_share(Config, ShareId),
        ExpectedFileType = maps:get(<<"fileType">>, Data, dir),
        ExpectedDescription = maps:get(<<"description">>, Data, <<"">>),
        ?assertMatch(#od_share{
            name = ?CORRECT_NAME, description = ExpectedDescription,
            space = SpaceId, root_file = RootFileId,
            file_type = ExpectedFileType
        }, Share),
        true
    end,

    EnvTearDownFun = fun(_) ->
        ozt:rpc(share_logic, delete, [?ROOT, ProposedShareId])
    end,

    NotAGuid = datastore_key:new(),
    ExampleFileUuid = datastore_key:new(),
    NotAShareFileGuid = file_id:pack_guid(ExampleFileUuid, SpaceId),
    ShareFileGuidWithBadSpaceId = file_id:pack_share_guid(ExampleFileUuid, datastore_key:new(), ProposedShareId),
    ShareFileGuidWithUndefinedShareId = file_id:pack_share_guid(ExampleFileUuid, SpaceId, undefined),
    ShareFileGuidWithBadFileUuid = file_id:pack_share_guid(<<"">>, SpaceId, ProposedShareId),
    BadDataValues = [
        {<<"shareId">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"shareId">>)},
        {<<"shareId">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"shareId">>)},
        {<<"description">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"description">>)},
        {<<"description">>, ?RAND_UNICODE_STR(100001), ?ERROR_BAD_VALUE_TEXT_TOO_LARGE(<<"description">>, 100000)},
        {<<"rootFileId">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"rootFileId">>)},
        {<<"rootFileId">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"rootFileId">>)},
        {<<"rootFileId">>, NotAGuid, ?ERROR_BAD_DATA(<<"rootFileId">>)},
        {<<"rootFileId">>, ExampleFileUuid, ?ERROR_BAD_DATA(<<"rootFileId">>)},
        {<<"rootFileId">>, NotAShareFileGuid, ?ERROR_BAD_DATA(<<"rootFileId">>)},
        {<<"rootFileId">>, ShareFileGuidWithBadSpaceId, ?ERROR_BAD_DATA(<<"rootFileId">>)},
        {<<"rootFileId">>, ShareFileGuidWithUndefinedShareId, ?ERROR_BAD_DATA(<<"rootFileId">>)},
        {<<"rootFileId">>, ShareFileGuidWithBadFileUuid, ?ERROR_BAD_DATA(<<"rootFileId">>)},
        {<<"fileType">>, 1234, ?ERROR_BAD_VALUE_ATOM(<<"fileType">>)},
        {<<"fileType">>, <<"">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"fileType">>, [file, dir])},
        {<<"fileType">>, atom, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"fileType">>, [file, dir])}
    ],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, Owner},
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
            expected_result = ?OK_ENV(fun(_Env, Data) ->
                ?OK_TERM(fun(Result) -> VerifyFun(Result, Data) end)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_share, aspect = instance},
            expected_result_op = ?OK_ENV(fun(_, Data) ->
                ?OK_MAP_CONTAINS(#{
                    <<"handleId">> => null,
                    <<"name">> => ?CORRECT_NAME,
                    <<"rootFileId">> => RootFileId,
                    <<"spaceId">> => SpaceId,
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
                        VerifyFun(Id, Data)
                    end
                })
            end)
        },
        data_spec = DataSpec = #data_spec{
            required = [
                <<"shareId">>, <<"name">>, <<"rootFileId">>, <<"spaceId">>
            ],
            optional = [
                <<"description">>, <<"fileType">>
            ],
            correct_values = #{
                <<"shareId">> => [ProposedShareId],
                <<"name">> => [?CORRECT_NAME],
                <<"description">> => [<<"">>, ?RAND_UNICODE_STR(769)],
                <<"rootFileId">> => [RootFileId],
                <<"fileType">> => [file, dir],
                <<"spaceId">> => [SpaceId]
            },
            bad_values = lists:append([
                [{<<"spaceId">>, <<"">>, ?ERROR_FORBIDDEN},
                    {<<"spaceId">>, <<"asdq4ewfs">>, ?ERROR_FORBIDDEN}],
                BadDataValues,
                ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)])
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, undefined, EnvTearDownFun, undefined)),

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
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec, undefined, EnvTearDownFun, undefined)).


get_test(Config) ->
    get_test(Config, ?SHARE_ID_2, dir),
    get_test(Config, ?SHARE_ID_1, file).

get_test(Config, ShareId, FileType) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ShareData = #{
        <<"shareId">> => ShareId,
        <<"spaceId">> => S1,
        <<"name">> => ?SHARE_NAME1,
        <<"description">> => str_utils:rand_hex(rand:uniform(1000) - 1),
        <<"rootFileId">> => ?GEN_ROOT_FILE_ID(S1, ShareId),
        <<"fileType">> => FileType
    },
    {ok, ShareId} = oz_test_utils:create_share(Config, ?USER(Owner), ShareData),

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SHARES_VIEW]},
                {user, Owner},
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
            expected_body = api_test_expect:private_share(rest, ShareId, ShareData, ?SUB(user, Owner))
        },
        logic_spec = #logic_spec{
            module = share_logic,
            function = get,
            args = [auth, ShareId],
            expected_result = api_test_expect:private_share(logic, ShareId, ShareData, ?SUB(user, Owner))
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_share, id = ShareId, aspect = instance},
            expected_result_op = api_test_expect:private_share(gs, ShareId, ShareData, ?SUB(user, Owner))
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
                {user, Owner},
                {user, NonAdmin},
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/shares/">>, ShareId, <<"/public">>],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:public_share(rest, ShareId, ShareData)
        },
        logic_spec = #logic_spec{
            module = share_logic,
            function = get_public_data,
            args = [auth, ShareId],
            expected_result = api_test_expect:public_share(logic, ShareId, ShareData)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_share, id = ShareId, aspect = instance, scope = public},
            expected_result_op = api_test_expect:public_share(gs, ShareId, ShareData)
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPublicDataApiTestSpec)),

    % Make sure the advertised public rest url works
    ?assert(rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            url => oz_test_utils:call_oz(Config, share_logic, build_public_rest_url, [ShareId]),
            path => <<"">>
        },
        expect => api_test_expect:public_share(rest, ShareId, ShareData)
    })).


update_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_MANAGE_SHARES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    InitialName = <<"Share name">>,
    InitialDescription = <<"This is a share">>,
    EnvSetUpFun = fun() ->
        ShareId = ?UNIQUE_STRING,
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, #{
                <<"shareId">> => ShareId,
                <<"name">> => InitialName,
                <<"description">> => InitialDescription,
                <<"rootFileId">> => ?GEN_ROOT_FILE_ID(S1, ShareId),
                <<"spaceId">> => S1
            }
        ),
        #{shareId => ShareId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{shareId := ShareId}, Data) ->
        {ok, Share} = oz_test_utils:get_share(Config, ShareId),
        ExpName = case ShouldSucceed of
            false -> InitialName;
            true -> maps:get(<<"name">>, Data, InitialName)
        end,
        ExpDescription = case ShouldSucceed of
            false -> InitialDescription;
            true -> maps:get(<<"description">>, Data, InitialDescription)
        end,
        ?assertEqual(ExpName, Share#od_share.name),
        ?assertEqual(ExpDescription, Share#od_share.description)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SHARES_UPDATE]},
                {user, Owner},
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
            expected_result_op = ?OK_RES
        },
        data_spec = #data_spec{
            at_least_one = [
                <<"name">>,
                <<"description">>
            ],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"description">> => [<<"">>, ?RAND_UNICODE_STR(1397)]
            },
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME) ++ [
                {<<"description">>, ?RAND_UNICODE_STR(100001), ?ERROR_BAD_VALUE_TEXT_TOO_LARGE(<<"description">>, 100000)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   UserWithManageSharesPriv gets the SPACE_MANAGE_SHARES privilege
    %   UserWithoutManageSharesPriv gets all remaining privileges
    {SpaceId, Owner, UserWithoutManageSharesPriv, UserWithManageSharesPriv} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_MANAGE_SHARES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    HandleServiceId = ozt_users:create_handle_service_for(Owner),

    lists:foreach(fun(ScenarioType) ->
        EnvSetUpFun = fun() ->
            ShareId = datastore_key:new(),
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, ?SHARE_NAME1, SpaceId
            ),
            HandleId = case ScenarioType of
                with_handle -> ozt_users:create_handle_for(Owner, HandleServiceId, ShareId);
                without_handle -> undefined
            end,
            #{shareId => ShareId, handleId => HandleId}
        end,
        DeleteEntityFun = fun(#{shareId := ShareId} = _Env) ->
            oz_test_utils:delete_share(Config, ShareId)
        end,
        VerifyEndFun = fun(ShouldSucceed, #{shareId := ShareId, handleId := HandleId} = _Env, _) ->
            ?assertEqual({ok, not ShouldSucceed}, ozt:rpc(od_share, exists, [ShareId])),
            {ok, Shares} = oz_test_utils:list_shares(Config),
            ?assertEqual(lists:member(ShareId, Shares), not ShouldSucceed),
            case ScenarioType of
                without_handle ->
                    ok;
                with_handle ->
                    % when a share is deleted and it had a handle attached, the handle should be deleted too
                    ?assertEqual({ok, not ShouldSucceed}, ozt:rpc(od_handle, exists, [HandleId])),
                    %% @TODO VFS-11906 remove redundant checks
                    ?assertEqual(not ShouldSucceed, lists:member(HandleId, ozt_handles:list())),
                    ?assertEqual(not ShouldSucceed, lists:member(HandleId,
                        [HId || #handle_listing_entry{handle_id = HId} <- ozt_handles:gather_by_all_prefixes()]
                    ))
            end
        end,

        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = lists:flatten([
                    root,
                    {admin, case ScenarioType of
                        with_handle -> [?OZ_SHARES_DELETE, ?OZ_HANDLES_DELETE];
                        without_handle -> [?OZ_SHARES_DELETE]
                    end},
                    {user, Owner},
                    % the UserWithManageSharesPriv does not have rights to remove the handle, hence
                    % he will only be authorized to delete the share if it does not have any handle
                    case ScenarioType of
                        with_handle -> [];
                        without_handle -> {user, UserWithManageSharesPriv}
                    end
                ]),
                unauthorized = [nobody],
                forbidden = lists:flatten([
                    {user, UserWithoutManageSharesPriv},
                    {user, NonAdmin},
                    case ScenarioType of
                        with_handle -> {user, UserWithManageSharesPriv};
                        without_handle -> []
                    end
                ])
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
                expected_result_op = ?OK_RES
            }
        },
        ?assert(api_test_scenarios:run_scenario(delete_entity,
            [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
        ))
    end, [with_handle, without_handle]).


get_shared_file_or_directory_data_test(Config) ->
    lists:foreach(fun(SubpathWithQs) ->
        get_shared_file_or_directory_data_test_base(Config, SubpathWithQs)
    end, example_shared_data_subpaths()).

get_shared_file_or_directory_data_test_base(Config, SubpathWithQs) ->
    ShareId = datastore_key:new(),
    {SpaceId, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(Config, ?SPACE_VIEW),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, ShareId} = oz_test_utils:create_share(Config, ?USER(Owner), #{
        <<"shareId">> => ShareId,
        <<"spaceId">> => SpaceId,
        <<"name">> => ?SHARE_NAME1,
        <<"description">> => str_utils:rand_hex(rand:uniform(1000) - 1),
        <<"rootFileId">> => ?GEN_ROOT_FILE_ID(SpaceId, ShareId),
        <<"fileType">> => file
    }),
    CorrectObjectId = gen_example_object_id(ShareId),

    CheckResult = fun(ObjectId, ExpectedResult) ->
        ExpectedCode = case ExpectedResult of
            {ok, _} -> ?HTTP_307_TEMPORARY_REDIRECT;
            {error, _} = Err1 -> errors:to_http_code(Err1)
        end,
        ExpectedHeaders = case ExpectedResult of
            {ok, URL} -> fun(#{?HDR_LOCATION := Location}) -> Location =:= URL end;
            {error, _} -> undefined
        end,
        ExpectedBody = case ExpectedResult of
            {ok, _} -> undefined;
            {error, _} = Err2 -> #{<<"error">> => errors:to_json(Err2)}
        end,
        api_test_utils:run_tests(Config, #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    nobody,
                    {admin, [?OZ_SHARES_VIEW]},
                    {user, Owner},
                    {user, NonAdmin},
                    {user, U1},
                    {user, U2}
                ]
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/shares/data/">>, ObjectId, SubpathWithQs],
                expected_code = ExpectedCode,
                expected_headers = ExpectedHeaders,
                expected_body = ExpectedBody
            }
        })
    end,

    % the space is not supported yet
    ?assert(CheckResult(CorrectObjectId, ?ERROR_SERVICE_UNAVAILABLE)),

    % the space gets support, but the provider is offline
    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config),
    oz_test_utils:support_space_by_provider(Config, ProviderId, SpaceId),
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(CorrectObjectId, ?ERROR_SERVICE_UNAVAILABLE)),

    % the provider connects, but it is in legacy version
    update_provider_version(Config, ProviderId, <<"19.02.3">>),
    start_provider_graphsync_channel(Config, ProviderId, ProviderToken),
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(CorrectObjectId, ?ERROR_NOT_IMPLEMENTED)),

    % the provider is connected and in up-to-date version
    update_provider_version(Config, ProviderId, <<"20.02.1">>),
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(CorrectObjectId, {ok, expected_shared_data_redirect(
        Config, ProviderId, CorrectObjectId, SubpathWithQs
    )})),

    % the ObjectId is not valid
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(<<"blahblahblah">>, ?ERROR_BAD_DATA(<<"FileId">>))),

    % the ObjectId is not a share guid
    NonShareGuid = file_id:pack_guid(str_utils:rand_hex(16), str_utils:rand_hex(16)),
    {ok, NonShareObjectId} = file_id:guid_to_objectid(NonShareGuid),
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(NonShareObjectId, ?ERROR_BAD_DATA(<<"FileId">>))),

    % the share does not exist
    NonExistingShareObjectId = gen_example_object_id(<<"non-existent-share">>),
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(NonExistingShareObjectId, ?ERROR_NOT_FOUND)).


% The SUITE is run on a single node cluster to test caching of chosen providers
% (the cache is local for each node).
choose_provider_for_public_share_handling_test(Config) ->
    {ok, UserId} = oz_test_utils:create_user(Config),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(UserId)),

    {ok, {AlphaOffline, _AlphaToken}} = oz_test_utils:create_provider(Config),
    {ok, {BetaOffline, _BetaToken}} = oz_test_utils:create_provider(Config),
    {ok, {GammaLegacy, GammaToken}} = oz_test_utils:create_provider(Config),
    {ok, {DeltaLegacy, DeltaToken}} = oz_test_utils:create_provider(Config),
    {ok, {SigmaUpToDate, SigmaToken}} = oz_test_utils:create_provider(Config),
    {ok, {OmegaUpToDate, OmegaToken}} = oz_test_utils:create_provider(Config),

    <<OzWorkerReleaseLine:5/binary, _/binary>> = oz_test_utils:call_oz(Config, oz_worker, get_release_version, []),

    ProviderVersion = fun(PrId) ->
        case PrId of
            AlphaOffline -> <<OzWorkerReleaseLine/binary, ".1">>;
            BetaOffline -> <<"18.02.3">>;
            GammaLegacy -> <<"19.02.1">>;
            DeltaLegacy -> <<"19.02.0-rc1">>;
            SigmaUpToDate -> <<OzWorkerReleaseLine/binary, ".0-rc1">>;
            OmegaUpToDate -> <<OzWorkerReleaseLine/binary, ".2">>
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
        GammaLegacy => start_provider_graphsync_channel(Config, GammaLegacy, GammaToken),
        DeltaLegacy => start_provider_graphsync_channel(Config, DeltaLegacy, DeltaToken),
        SigmaUpToDate => start_provider_graphsync_channel(Config, SigmaUpToDate, SigmaToken),
        OmegaUpToDate => start_provider_graphsync_channel(Config, OmegaUpToDate, OmegaToken)
    },

    ShareId = str_utils:rand_hex(16),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ShareId, ?SHARE_NAME1, SpaceId
    ),

    ChooseProvider = fun() ->
        {ok, {PrId, PrVersion}} = oz_test_utils:call_oz(
            Config, share_logic, choose_provider_for_public_share_handling, [ShareId]
        ),
        {PrId, PrVersion}
    end,

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % One of the up to date providers should be chosen
    {ChosenProviderId, ChosenProviderVersion} = ChooseProvider(),
    ?assert(lists:member(ChosenProviderId, [SigmaUpToDate, OmegaUpToDate])),
    ?assertEqual(ProviderVersion(ChosenProviderId), ChosenProviderVersion),
    % Choice should be cached and reused
    ?assertEqual({ChosenProviderId, ChosenProviderVersion}, ChooseProvider()),
    check_shares_data_redirector(Config, ShareId, ChosenProviderId, ProviderVersion(ChosenProviderId)),

    % If the chosen provider goes down, another up to date provider should be chosen
    terminate_provider_graphsync_channel(Config, ChosenProviderId, maps:get(ChosenProviderId, Connections)),
    [TheOtherUpToDate] = [SigmaUpToDate, OmegaUpToDate] -- [ChosenProviderId],
    ?assertEqual({TheOtherUpToDate, ProviderVersion(TheOtherUpToDate)}, ChooseProvider()),
    ?assertEqual({TheOtherUpToDate, ProviderVersion(TheOtherUpToDate)}, ChooseProvider()),
    check_shares_data_redirector(Config, ShareId, TheOtherUpToDate, ProviderVersion(TheOtherUpToDate)),

    % When the second up to date provider goes down, a legacy one should be picked
    terminate_provider_graphsync_channel(Config, TheOtherUpToDate, maps:get(TheOtherUpToDate, Connections)),
    {LegacyProviderId, LegacyProviderVersion} = ChooseProvider(),
    ?assert(lists:member(LegacyProviderId, [GammaLegacy, DeltaLegacy])),
    ?assertEqual(ProviderVersion(LegacyProviderId), LegacyProviderVersion),
    ?assertEqual({LegacyProviderId, ProviderVersion(LegacyProviderId)}, ChooseProvider()),
    check_shares_data_redirector(Config, ShareId, LegacyProviderId, ProviderVersion(LegacyProviderId)),

    % If one of the up to date providers go up, eventually it should be used again
    % (after the cache expiration)
    terminate_provider_graphsync_channel(Config, LegacyProviderId, maps:get(LegacyProviderId, Connections)),
    NewConnections = Connections#{
        SigmaUpToDate => start_provider_graphsync_channel(Config, SigmaUpToDate, SigmaToken)
    },
    ?assertEqual({SigmaUpToDate, ProviderVersion(SigmaUpToDate)}, ChooseProvider(), 60),
    check_shares_data_redirector(Config, ShareId, SigmaUpToDate, ProviderVersion(SigmaUpToDate)),

    % If all providers go down, undefined result should be immediately returned
    terminate_provider_graphsync_channel(Config, GammaLegacy, maps:get(GammaLegacy, NewConnections)),
    terminate_provider_graphsync_channel(Config, DeltaLegacy, maps:get(DeltaLegacy, NewConnections)),
    terminate_provider_graphsync_channel(Config, SigmaUpToDate, maps:get(SigmaUpToDate, NewConnections)),
    terminate_provider_graphsync_channel(Config, OmegaUpToDate, maps:get(OmegaUpToDate, NewConnections)),
    ?assertEqual({undefined, undefined}, ChooseProvider()),
    check_shares_data_redirector(Config, ShareId, undefined, undefined),

    % After some providers go online again and the cache expires, they should be picked again
    start_provider_graphsync_channel(Config, DeltaLegacy, DeltaToken),
    start_provider_graphsync_channel(Config, OmegaUpToDate, OmegaToken),
    ?assertEqual({OmegaUpToDate, ProviderVersion(OmegaUpToDate)}, ChooseProvider(), 60),
    check_shares_data_redirector(Config, ShareId, OmegaUpToDate, ProviderVersion(OmegaUpToDate)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
update_provider_version(Config, ProviderId, Version) ->
    {DummyGuiHash, _IndexContent} = oz_test_utils:deploy_dummy_gui(Config, ?OP_WORKER_GUI),
    ?assertEqual(ok, oz_test_utils:call_oz(Config, cluster_logic, update_version_info, [
        ?PROVIDER(ProviderId), ProviderId, ?WORKER, {Version, <<"build-123">>, DummyGuiHash}
    ])).


%% @private
start_provider_graphsync_channel(Config, ProviderId, ProviderToken) ->
    Url = oz_test_utils:graph_sync_url(Config, oneprovider),
    SSlOpts = [{secure, only_verify_peercert}, {cacerts, oz_test_utils:gui_ca_certs(Config)}],
    {ok, GsClient, _} = gs_client:start_link(
        Url, {token, ProviderToken}, ?SUPPORTED_PROTO_VERSIONS, fun(_) -> ok end, SSlOpts
    ),
    ?assertMatch(
        true,
        oz_test_utils:call_oz(Config, provider_connections, is_online, [ProviderId]),
        60
    ),
    GsClient.


%% @private
terminate_provider_graphsync_channel(Config, ProviderId, ClientPid) ->
    gs_client:kill(ClientPid),
    ?assertMatch(
        false,
        oz_test_utils:call_oz(Config, provider_connections, is_online, [ProviderId]),
        60
    ).


%% @private
example_shared_data_subpaths() -> [
    <<"/content">>, <<"/content?not=significant&key=value">>,
    <<"/children">>, <<"/children?offset=100&limit=3">>,
    <<"">>, <<"?attribute=size">>,
    <<"/">>, <<"/?attribute=size">>,
    <<"/metadata/xattrs">>, <<"/metadata/xattrs?attribute=license">>,
    <<"/metadata/json">>, <<"/metadata/json?filter_type=keypath&filter=key1.key2.[2].key3">>,
    <<"/metadata/rdf">>, <<"/metadata/rdf?not=significant">>,
    <<"/some/invalid/path/should/work/anyway">>,
    <<"/other-invalid-path?arg=value">>
].


%% @private
check_shares_data_redirector(Config, ShareId, ProviderId, ProviderVersion) ->
    lists:foreach(fun(SubpathWithQs) ->
        check_shares_data_redirector(Config, ShareId, ProviderId, ProviderVersion, SubpathWithQs)
    end, example_shared_data_subpaths()).

check_shares_data_redirector(Config, ShareId, ProviderId, ProviderVersion, SubpathWithQs) ->
    OzDomain = oz_test_utils:oz_domain(Config),

    ExampleObjectId = gen_example_object_id(ShareId),
    Result = http_client:get(
        str_utils:format_bin("https://~s/api/v3/onezone/shares/data/~s~s", [OzDomain, ExampleObjectId, SubpathWithQs]),
        #{}, <<"">>, [{ssl_options, [{cacerts, oz_test_utils:gui_ca_certs(Config)}]}]
    ),
    ParsedResult = case Result of
        {ok, ?HTTP_307_TEMPORARY_REDIRECT, #{?HDR_LOCATION := Url}, _} ->
            {ok, Url};
        {ok, _, _, ErrorJson} ->
            errors:from_json(maps:get(<<"error">>, json_utils:decode(ErrorJson)))
    end,
    case ProviderId of
        undefined ->
            ?assertEqual(ParsedResult, ?ERROR_SERVICE_UNAVAILABLE);
        _ ->
            case ProviderVersion of
                <<"18.02", _/binary>> ->
                    ?assertEqual(ParsedResult, ?ERROR_NOT_IMPLEMENTED);
                <<"19.02", _/binary>> ->
                    ?assertEqual(ParsedResult, ?ERROR_NOT_IMPLEMENTED);
                _ ->
                    ?assertEqual(ParsedResult, {ok, expected_shared_data_redirect(
                        Config, ProviderId, ExampleObjectId, SubpathWithQs
                    )})
            end
    end.


%% @private
gen_example_object_id(ShareId) ->
    ExampleShareGuid = file_id:pack_share_guid(str_utils:rand_hex(16), str_utils:rand_hex(16), ShareId),
    {ok, ExampleObjectId} = file_id:guid_to_objectid(ExampleShareGuid),
    ExampleObjectId.


%% @private
expected_shared_data_redirect(Config, ProviderId, ObjectId, SubpathWithQs) ->
    {ok, #od_provider{domain = OpDomain}} = oz_test_utils:get_provider(Config, ProviderId),
    % if the path ends with a slash, it is trimmed
    ExpectedSubpathWithQs = case SubpathWithQs of
        <<"/">> -> <<"">>;
        <<"/?", Rest/binary>> -> <<"?", Rest/binary>>;
        Other -> Other
    end,
    str_utils:format_bin("https://~s/api/v3/oneprovider/data/~s~s", [
        OpDomain, ObjectId, ExpectedSubpathWithQs
    ]).


%% @private
clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId) ->
    oz_test_utils:call_oz(Config, node_cache, clear, [{chosen_provider_for_share_handling, SpaceId}]).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    ozt:init_per_suite(Config).

end_per_suite(_Config) ->
    application:stop(hackney),
    ssl:stop().

init_per_testcase(choose_provider_for_public_share_handling_test, Config) ->
    % do not freeze time in this testcase as the logic uses an expiring cache
    Config;
init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    ozt_mocks:mock_handle_proxy(),
    Config.

end_per_testcase(choose_provider_for_public_share_handling_test, _Config) ->
    ok;
end_per_testcase(_, _Config) ->
    ozt_mocks:unmock_handle_proxy(),
    ozt_mocks:unfreeze_time().
