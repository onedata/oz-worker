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
-include_lib("ctool/include/onedata_file.hrl").
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


-define(GEN_NAME_BAD_VALUES(), lists:map(fun(BadName) ->
    {<<"name">>, BadName, ?ERR_BAD_DATA(
        <<"name">>, <<"Bad value: ", (?SHARE_NAME_REQUIREMENTS_DESCRIPTION)/binary>>
    )}
end, [
    <<"">>,
    <<".">>,
    <<"..">>,
    <<"dir/file.txt">>,
    <<"file-with-null\0.txt">>
])).


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

    RootFileUuid = datastore_key:new(),
    ProposedShareId = datastore_key:new(),
    RootFileShareGuid = ?GEN_ROOT_FILE_GUID(RootFileUuid, SpaceId, ProposedShareId),

    VerifyFun = fun(ShareId, Data) ->
        ?assertEqual(ShareId, ProposedShareId),
        {ok, Share} = oz_test_utils:get_share(Config, ShareId),
        % TODO VFS-VFS-12490 [file, dir] deprecated, left for BC, can be removed in 23.02.*
        ExpectedFileType = case maps:get(<<"fileType">>, Data, ?DIRECTORY_TYPE) of
            <<"file">> -> ?REGULAR_FILE_TYPE;
            <<"dir">> -> ?DIRECTORY_TYPE;
            ModernType -> ModernType
        end,
        ExpectedDescription = maps:get(<<"description">>, Data, <<"">>),
        ?assertMatch(#od_share{
            name = ?CORRECT_NAME, description = ExpectedDescription,
            space = SpaceId, root_file_uuid = RootFileUuid,
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
    BadDataValues = ?GEN_NAME_BAD_VALUES() ++ [
        {<<"shareId">>, <<"">>, ?ERR_BAD_VALUE_EMPTY(<<"shareId">>)},
        {<<"shareId">>, 1234, ?ERR_BAD_VALUE_STRING(<<"shareId">>)},
        {<<"description">>, 1234, ?ERR_BAD_VALUE_STRING(<<"description">>)},
        {<<"description">>, ?RAND_UNICODE_STR(100001), ?ERR_BAD_VALUE_TEXT_TOO_LARGE(<<"description">>, 100000)},
        {<<"rootFileId">>, <<"">>, ?ERR_BAD_VALUE_EMPTY(<<"rootFileId">>)},
        {<<"rootFileId">>, 1234, ?ERR_BAD_VALUE_STRING(<<"rootFileId">>)},
        {<<"rootFileId">>, NotAGuid, ?ERR_BAD_DATA(<<"rootFileId">>, undefined)},
        {<<"rootFileId">>, ExampleFileUuid, ?ERR_BAD_DATA(<<"rootFileId">>, undefined)},
        {<<"rootFileId">>, NotAShareFileGuid, ?ERR_BAD_DATA(<<"rootFileId">>, undefined)},
        {<<"rootFileId">>, ShareFileGuidWithBadSpaceId, ?ERR_BAD_DATA(<<"rootFileId">>, undefined)},
        {<<"rootFileId">>, ShareFileGuidWithUndefinedShareId, ?ERR_BAD_DATA(<<"rootFileId">>, undefined)},
        {<<"rootFileId">>, ShareFileGuidWithBadFileUuid, ?ERR_BAD_DATA(<<"rootFileId">>, undefined)},
        {<<"fileType">>, 1234, ?ERR_BAD_VALUE_STRING(<<"fileType">>)},
        {<<"fileType">>, <<"">>, ?ERR_BAD_VALUE_NOT_ALLOWED(<<"fileType">>, [?REGULAR_FILE_TYPE, ?DIRECTORY_TYPE])},
        {<<"fileType">>, atom, ?ERR_BAD_VALUE_NOT_ALLOWED(<<"fileType">>, [?REGULAR_FILE_TYPE, ?DIRECTORY_TYPE])}
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
                    <<"rootFileId">> => RootFileShareGuid,
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
                <<"rootFileId">> => [RootFileShareGuid],
                % TODO VFS-VFS-12490 [file, dir] deprecated, left for BC, can be removed in 23.02.*
                <<"fileType">> => [?REGULAR_FILE_TYPE, ?DIRECTORY_TYPE, <<"file">>, <<"dir">>],
                <<"spaceId">> => [SpaceId]
            },
            bad_values = lists:flatten([
                {<<"spaceId">>, <<"">>, ?ERR_FORBIDDEN},
                {<<"spaceId">>, <<"asdq4ewfs">>, ?ERR_FORBIDDEN},
                BadDataValues
            ])
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
            bad_values = lists:flatten([
                [{<<"spaceId">>, <<"">>, ?ERR_BAD_VALUE_ID_NOT_FOUND(<<"spaceId">>)},
                    {<<"spaceId">>, 1234, ?ERR_BAD_VALUE_ID_NOT_FOUND(<<"spaceId">>)}],
                BadDataValues
            ])
        }
    },
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec, undefined, EnvTearDownFun, undefined)).


get_test(Config) ->
    get_test(Config, ?DIRECTORY_TYPE),
    get_test(Config, ?REGULAR_FILE_TYPE).

get_test(Config, FileType) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ShareId = datastore_key:new(),
    ShareData = #{
        <<"shareId">> => ShareId,
        <<"spaceId">> => S1,
        <<"name">> => ?SHARE_NAME1,
        <<"description">> => str_utils:rand_hex(rand:uniform(1000) - 1),
        <<"rootFileId">> => ?GEN_ROOT_FILE_GUID(S1, ShareId),
        <<"fileType">> => FileType
    },
    {ok, ShareId} = oz_test_utils:create_share(Config, ?USER(Owner), ShareData#{
        % TODO VFS-VFS-12490 [file, dir] deprecated, left for BC, can be removed in 23.02.*
        <<"fileType">> => case ?RAND_BOOL() of
            true ->
                FileType;
            false ->
                case FileType of
                    ?DIRECTORY_TYPE -> <<"dir">>;
                    ?REGULAR_FILE_TYPE -> <<"file">>
                end
        end
    }),

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
            url => oz_test_utils:call_oz(Config, od_share, build_public_rest_url, [ShareId]),
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
                <<"rootFileId">> => ?GEN_ROOT_FILE_GUID(S1, ShareId),
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
            bad_values = ?GEN_NAME_BAD_VALUES() ++ [
                {<<"description">>, ?RAND_UNICODE_STR(100001), ?ERR_BAD_VALUE_TEXT_TOO_LARGE(<<"description">>, 100000)}
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
                    ?assertEqual(not ShouldSucceed, lists:member(HandleId, ozt_handles:list()))
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
        <<"rootFileId">> => ?GEN_ROOT_FILE_GUID(SpaceId, ShareId),
        <<"fileType">> => ?REGULAR_FILE_TYPE
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
    ?assert(CheckResult(CorrectObjectId, ?ERR_SERVICE_UNAVAILABLE)),

    % the space gets support, but the provider is offline
    {ok, {ProviderId, _ProviderToken}} = oz_test_utils:create_provider(Config),
    oz_test_utils:support_space_by_provider(Config, ProviderId, SpaceId),
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(CorrectObjectId, ?ERR_SERVICE_UNAVAILABLE)),

    % the provider connects, but it is in legacy version
    update_provider_version(Config, ProviderId, <<"19.02.3">>),
    start_provider_graphsync_channel(Config, ProviderId),
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(CorrectObjectId, ?ERR_NOT_IMPLEMENTED)),

    % the provider is connected and in up-to-date version
    update_provider_version(Config, ProviderId, <<"20.02.1">>),
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(CorrectObjectId, {ok, expected_shared_data_redirect(
        Config, ProviderId, CorrectObjectId, SubpathWithQs
    )})),

    % the ObjectId is not valid
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(<<"blahblahblah">>, ?ERR_BAD_DATA(<<"FileId">>, undefined))),

    % the ObjectId is not a share guid
    NonShareGuid = file_id:pack_guid(str_utils:rand_hex(16), str_utils:rand_hex(16)),
    {ok, NonShareObjectId} = file_id:guid_to_objectid(NonShareGuid),
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(NonShareObjectId, ?ERR_BAD_DATA(<<"FileId">>, undefined))),

    % the share does not exist
    NonExistingShareObjectId = gen_example_object_id(<<"non-existent-share">>),
    clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId),
    ?assert(CheckResult(NonExistingShareObjectId, ?ERROR_NOT_FOUND)).


% The SUITE is run on a single node cluster to test caching of chosen providers
% (the cache is local for each node).
choose_provider_for_public_share_handling_test(Config) ->
    UserId = ozt_users:create(),
    SpaceId = ozt_users:create_space_for(UserId),

    AlphaOffline = ozt_providers:create(),
    BetaOffline = ozt_providers:create(),
    EpsilonOfflineReadonly = ozt_providers:create(),

    GammaLegacy = ozt_providers:create(),
    DeltaLegacy = ozt_providers:create(),
    RhoLegacyReadonly = ozt_providers:create(),

    SigmaUpToDate = ozt_providers:create(),
    OmegaUpToDate = ozt_providers:create(),
    TauUpToDateReadonly = ozt_providers:create(),

    <<OzWorkerReleaseLine:5/binary, _/binary>> = ozt:rpc(oz_worker, get_release_version, []),

    ProviderVersion = fun(PrId) ->
        case PrId of
            AlphaOffline -> <<OzWorkerReleaseLine/binary, ".1">>;
            BetaOffline -> <<"18.02.3">>;
            EpsilonOfflineReadonly -> <<OzWorkerReleaseLine/binary, ".1">>;

            GammaLegacy -> <<"19.02.1">>;
            DeltaLegacy -> <<"20.02.1">>;
            RhoLegacyReadonly -> <<"20.02.19">>;

            SigmaUpToDate -> <<OzWorkerReleaseLine/binary, ".0-rc1">>;
            OmegaUpToDate -> <<OzWorkerReleaseLine/binary, ".2">>;
            TauUpToDateReadonly -> <<OzWorkerReleaseLine/binary, ".0-alpha37">>
        end
    end,

    AllProviders = [
        AlphaOffline, BetaOffline, EpsilonOfflineReadonly,
        GammaLegacy, DeltaLegacy, RhoLegacyReadonly,
        SigmaUpToDate, OmegaUpToDate, TauUpToDateReadonly
    ],

    ReadonlyProviders = [EpsilonOfflineReadonly, RhoLegacyReadonly, TauUpToDateReadonly],

    lists:foreach(fun(ProviderId) ->
        % at least one storage must be readwrite for the provider to be prioritized as most eligible,
        % all storages must be readonly for the provider to get a lower priority
        ozt_providers:support_space(ProviderId, ozt_providers:create_storage(ProviderId, #{
            <<"name">> => ?RAND_STR(),
            <<"readonly">> => lists:member(ProviderId, ReadonlyProviders)
        }), SpaceId),
        % randomly generate 0-4 more supports
        utils:repeat(?RAND_INT(0, 4), fun() ->
            ozt_providers:support_space(ProviderId, ozt_providers:create_storage(ProviderId, #{
                <<"name">> => ?RAND_STR(),
                <<"readonly">> => lists:member(ProviderId, ReadonlyProviders) orelse ?RAND_BOOL()
            }), SpaceId)
        end),
        update_provider_version(Config, ProviderId, ProviderVersion(ProviderId))
    end, AllProviders),
    ozt:reconcile_entity_graph(),

    Connections = #{
        GammaLegacy => start_provider_graphsync_channel(Config, GammaLegacy),
        DeltaLegacy => start_provider_graphsync_channel(Config, DeltaLegacy),
        RhoLegacyReadonly => start_provider_graphsync_channel(Config, RhoLegacyReadonly),
        SigmaUpToDate => start_provider_graphsync_channel(Config, SigmaUpToDate),
        OmegaUpToDate => start_provider_graphsync_channel(Config, OmegaUpToDate),
        TauUpToDateReadonly => start_provider_graphsync_channel(Config, TauUpToDateReadonly)
    },

    ShareId = ozt_shares:create(SpaceId),

    ChooseProvider = fun() ->
        oz_test_utils:call_oz(Config, od_share, choose_provider_for_public_share_handling, [ShareId])
    end,

    VerifyProviderChoice = fun(ExpectedChoice, Attempts) ->
        try
            case ExpectedChoice of
                [] ->
                    ?assertMatch(?ERR_SERVICE_UNAVAILABLE, ChooseProvider(), Attempts),
                    check_shares_data_redirector(Config, ShareId, undefined, undefined),
                    % the choice should be cached and reused
                    ?assertMatch(?ERR_SERVICE_UNAVAILABLE, ChooseProvider(), Attempts),
                    undefined;
                _ ->
                    ?assert(lists:member(element(1, element(2, ChooseProvider())), ExpectedChoice), Attempts),
                    {ok, {ChosenProviderId, ChosenProviderVersion}} = ChooseProvider(),
                    ?assertEqual(ProviderVersion(ChosenProviderId), ChosenProviderVersion),
                    check_shares_data_redirector(Config, ShareId, ChosenProviderId, ProviderVersion(ChosenProviderId)),
                    % the choice should be cached and reused
                    ?assertEqual({ok, {ChosenProviderId, ChosenProviderVersion}}, ChooseProvider()),
                    ChosenProviderId
            end
        catch Class:Reason:Stacktrace ->
            ?ct_pal_exception("provider choice not as expected", Class, Reason, Stacktrace),
            error(test_failed)
        end
    end,

    % one of the up-to-date readwrite providers should be chosen
    FirstChoice = VerifyProviderChoice([SigmaUpToDate, OmegaUpToDate], 1),

    % if the chosen provider goes down, another up-to-date provider should be chosen
    terminate_provider_graphsync_channel(Config, FirstChoice, maps:get(FirstChoice, Connections)),
    SecondChoice = VerifyProviderChoice([SigmaUpToDate, OmegaUpToDate] -- [FirstChoice], 1),

    % if all of them are down, the up-to-date readonly one should he chosen
    terminate_provider_graphsync_channel(Config, SecondChoice, maps:get(SecondChoice, Connections)),
    TauUpToDateReadonly = VerifyProviderChoice([TauUpToDateReadonly], 1),

    % only if it goes down, one of the legacy ones should be picked (but not a readonly one)
    terminate_provider_graphsync_channel(Config, TauUpToDateReadonly, maps:get(TauUpToDateReadonly, Connections)),
    VerifyProviderChoice([GammaLegacy, DeltaLegacy], 1),

    % if one of the up-to-date providers go up, eventually it should be used again
    % (after the cache expiration)
    NewConnections = Connections#{
        TauUpToDateReadonly => start_provider_graphsync_channel(Config, TauUpToDateReadonly)
    },
    VerifyProviderChoice([TauUpToDateReadonly], 60),

    % If all providers go down, undefined result should be immediately returned
    terminate_provider_graphsync_channel(Config, GammaLegacy, maps:get(GammaLegacy, NewConnections)),
    terminate_provider_graphsync_channel(Config, DeltaLegacy, maps:get(DeltaLegacy, NewConnections)),
    terminate_provider_graphsync_channel(Config, RhoLegacyReadonly, maps:get(RhoLegacyReadonly, NewConnections)),
    terminate_provider_graphsync_channel(Config, TauUpToDateReadonly, maps:get(TauUpToDateReadonly, NewConnections)),
    VerifyProviderChoice([], 1),

    % after a provider goes online again and the cache expires, they should be picked again
    start_provider_graphsync_channel(Config, RhoLegacyReadonly),
    VerifyProviderChoice([RhoLegacyReadonly], 60),

    % after a provider with a better priority goes up and the cache expires, it should be used again
    start_provider_graphsync_channel(Config, TauUpToDateReadonly),
    VerifyProviderChoice([TauUpToDateReadonly], 60),

    start_provider_graphsync_channel(Config, SigmaUpToDate),
    VerifyProviderChoice([SigmaUpToDate], 60).


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
start_provider_graphsync_channel(Config, ProviderId) ->
    ProviderToken = ozt_tokens:ensure_serialized(ozt_providers:get_root_token(ProviderId)),
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
        str_utils:format_bin("https://~ts/api/v3/onezone/shares/data/~ts~ts", [OzDomain, ExampleObjectId, SubpathWithQs]),
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
            ?assertEqual(ParsedResult, ?ERR_SERVICE_UNAVAILABLE);
        _ ->
            case ProviderVersion of
                <<"18.02", _/binary>> ->
                    ?assertEqual(ParsedResult, ?ERR_NOT_IMPLEMENTED);
                <<"19.02", _/binary>> ->
                    ?assertEqual(ParsedResult, ?ERR_NOT_IMPLEMENTED);
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
    str_utils:format_bin("https://~ts/api/v3/oneprovider/data/~ts~ts", [
        OpDomain, ObjectId, ExpectedSubpathWithQs
    ]).


%% @private
clear_cached_chosen_provider_for_public_share_handling(Config, SpaceId) ->
    oz_test_utils:call_oz(Config, node_cache, clear, [{chosen_provider_for_request_handling, SpaceId}]).

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
