%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning handle basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(handle_misc_api_test_SUITE).
-author("Bartosz Walkowicz").

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("http/handlers/oai.hrl").
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
    list_privileges_test/1,
    create_test/1,
    get_test/1,
    update_test/1,
    delete_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        list_privileges_test,
        get_test,
        update_test,
        delete_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_test(Config) ->
    % Make sure that handles created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [
        ?OZ_HANDLE_SERVICES_CREATE
    ], []),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?DOI_SERVICE
    ),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),

    ExpHandles = lists:map(
        fun(_) ->
            ShareId = ?UNIQUE_STRING,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, ?SHARE_NAME1, S1
            ),
            ozt_users:create_handle_for(U1, HService, ShareId)
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_LIST]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/handles">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handles">> => ExpHandles}
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = list,
            args = [auth],
            expected_result = ?OK_LIST(ExpHandles)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also handle_logic:exist function
    lists:foreach(
        fun(Handle) ->
            ?assert(oz_test_utils:call_oz(
                Config, handle_logic, exists, [Handle])
            )
        end, ExpHandles
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, handle_logic, exists, [<<"asdiucyaie827346w">>])
    ).


list_privileges_test(Config) ->

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/handles/privileges">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"member">> => [atom_to_binary(P, utf8) || P <- privileges:handle_member()],
                <<"admin">> => [atom_to_binary(P, utf8) || P <- privileges:handle_admin()]
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_test(Config) ->
    % create pid and doi handle services with 2 users:
    %   U2 gets the HANDLE_SERVICE_REGISTER_HANDLE privilege
    %   U1 gets all remaining privileges
    {DoiHService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_REGISTER_HANDLE
    ),
    {ok, PidHService} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?PID_SERVICE
    ),
    oz_test_utils:handle_service_set_user_privileges(
        Config, PidHService, U1, [], [?HANDLE_SERVICE_REGISTER_HANDLE]
    ),
    {ok, U2} = oz_test_utils:handle_service_add_user(Config, PidHService, U2),
    oz_test_utils:handle_service_set_user_privileges(
        Config, PidHService, U2, [?HANDLE_SERVICE_REGISTER_HANDLE], []
    ),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2,
        privileges:space_privileges(), []
    ),

    {ok, ShareIdThatAlreadyHasAHandle} = oz_test_utils:create_share(
        Config, ?ROOT, datastore_key:new(), ?SHARE_NAME1, S1
    ),
    ozt_handles:create(DoiHService, ShareIdThatAlreadyHasAHandle),

    MetadataPrefix = ?RAND_ELEMENT(ozt_handles:supported_metadata_prefixes()),
    RawMetadata = ozt_handles:example_input_metadata(MetadataPrefix),

    EnvSetUpFun = fun() ->
        ShareId = datastore_key:new(),
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, ShareId, ?SHARE_NAME1, S1
        ),
        #{shareId => ShareId}
    end,

    VerifyResult = fun(#{shareId := ShareId}, HandleId, HService) ->
        Handle = ozt_handles:get(HandleId),
        ?assert(is_binary(Handle#od_handle.public_handle)),
        ?assertEqual(<<"Share">>, Handle#od_handle.resource_type),
        ?assertEqual(ShareId, Handle#od_handle.resource_id),
        ?assertEqual(ozt_handles:expected_final_metadata(Handle), Handle#od_handle.metadata),
        ?assertEqual(MetadataPrefix, Handle#od_handle.metadata_prefix),
        ?assertEqual(HService, Handle#od_handle.handle_service),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_CREATE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/handles">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(Env, Data) ->
                HService = maps:get(<<"handleServiceId">>, Data),
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/handles/">>]),
                    [HandleId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyResult(Env, HandleId, HService)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(Env, Data) ->
                HService = maps:get(<<"handleServiceId">>, Data),
                ?OK_TERM(fun(Result) -> VerifyResult(Env, Result, HService) end)
            end)
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [
                <<"handleServiceId">>, <<"resourceType">>,
                <<"resourceId">>, <<"metadata">>, <<"metadataPrefix">>
            ],
            correct_values = #{
                <<"handleServiceId">> => [DoiHService, PidHService],
                <<"resourceType">> => [<<"Share">>],
                <<"resourceId">> => [fun(#{shareId := ShareId} = _Env) -> ShareId end],
                <<"metadata">> => [RawMetadata],
                <<"metadataPrefix">> => [MetadataPrefix]
            },
            bad_values = [
                {<<"handleServiceId">>, <<"">>,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"handleServiceId">>)},
                {<<"handleServiceId">>, 1234,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"handleServiceId">>)},
                {<<"resourceType">>, <<"">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resourceType">>,
                        [<<"Share">>])},
                {<<"resourceType">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"resourceType">>)},
                {<<"resourceId">>, <<"">>,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"resourceId">>)},
                {<<"resourceId">>, ShareIdThatAlreadyHasAHandle,
                    ?ERROR_ALREADY_EXISTS},
                {<<"resourceId">>, 1234,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"resourceId">>)},
                {<<"metadataPrefix">>, <<"bad_metadata">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"metadataPrefix">>, ozt_handles:supported_metadata_prefixes())},
                {<<"metadata">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"metadata">>)},
                {<<"metadata">>, ?RAND_UNICODE_STR(100001),
                    ?ERROR_BAD_VALUE_TEXT_TOO_LARGE(<<"metadata">>, 100000)},
                {<<"metadata">>, <<"null">>, ?ERROR_BAD_VALUE_XML(<<"metadata">>)},
                {<<"metadata">>, <<"<a></b>">>, ?ERROR_BAD_VALUE_XML(<<"metadata">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, undefined)).


get_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [
        ?OZ_HANDLE_SERVICES_CREATE
    ], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?DOI_SERVICE
    ),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ?SHARE_ID_1, ?SHARE_NAME1, S1
    ),

    MetadataPrefix = ?RAND_ELEMENT(ozt_handles:supported_metadata_prefixes()),
    RawMetadata = ozt_handles:example_input_metadata(MetadataPrefix),
    HandleData = #{
        <<"handleServiceId">> => HService,
        <<"resourceType">> => <<"Share">>,
        <<"resourceId">> => ShareId,
        <<"metadataPrefix">> => MetadataPrefix,
        <<"metadata">> => RawMetadata
    },
    {ok, HandleId} = oz_test_utils:create_handle(
        Config, ?USER(U1), HandleData
    ),
    ExpFinalMetadata = ozt_handles:expected_final_metadata(HandleId),
    oz_test_utils:handle_set_user_privileges(Config, HandleId, U1, [], [
        ?HANDLE_VIEW
    ]),
    {ok, U2} = oz_test_utils:handle_add_user(Config, HandleId, U2),
    oz_test_utils:handle_set_user_privileges(Config, HandleId, U2, [
        ?HANDLE_VIEW
    ], []),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:handle_privileges(),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {admin, [?OZ_HANDLES_VIEW]},
                {user, NonAdmin},
                {user, U1}
            ]
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = get,
            args = [auth, HandleId],
            expected_result = ?OK_TERM(
                fun(#od_handle{
                    resource_type = <<"Share">>,

                    metadata = Metadata, resource_id = ResourceId,
                    handle_service = HServiceId,
                    users = Users, groups = #{},
                    eff_users = EffUsers, eff_groups = #{},

                    bottom_up_dirty = false
                }) ->
                    ?assertEqual(ExpFinalMetadata, Metadata),
                    ?assertEqual(ShareId, ResourceId),
                    ?assertEqual(HService, HServiceId),
                    ?assertEqual(Users, #{
                        U1 => AllPrivs -- [?HANDLE_VIEW], U2 => [?HANDLE_VIEW]}
                    ),
                    ?assertEqual(EffUsers, #{
                        U1 => {AllPrivs -- [?HANDLE_VIEW], [{od_handle, <<"self">>}]},
                        U2 => {[?HANDLE_VIEW], [{od_handle, <<"self">>}]}
                    })
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_handle, id = HandleId, aspect = instance},
            expected_result_op = ?OK_MAP_CONTAINS(#{
                <<"effectiveUsers">> => #{
                    U1 => AllPrivsBin -- [<<"handle_view">>],
                    U2 => [<<"handle_view">>]
                },
                <<"effectiveGroups">> => #{},
                <<"handleServiceId">> => HService,
                <<"metadata">> => ExpFinalMetadata,
                <<"resourceId">> => ShareId,
                <<"resourceType">> => <<"Share">>,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(HandleId, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check protected data
    GetProtectedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_VIEW]},
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/handles/">>, HandleId],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:protected_handle(rest, HandleId, HandleData, ?SUB(user, U1))
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = get_protected_data,
            args = [auth, HandleId],
            expected_result = api_test_expect:protected_handle(logic, HandleId, HandleData, ?SUB(user, U1))
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)),

    % Get and check public data
    GetPublicDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {admin, [?OZ_HANDLES_VIEW]},
                {user, NonAdmin},
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/handles/">>, HandleId, <<"/public">>],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:public_handle(rest, HandleId, HandleData)
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = get_public_data,
            args = [auth, HandleId],
            expected_result = api_test_expect:public_handle(logic, HandleId, HandleData)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_handle, id = HandleId, aspect = instance, scope = public},
            expected_result_op = api_test_expect:public_handle(gs, HandleId, HandleData)
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPublicDataApiTestSpec)).


update_test(Config) ->
    lists:foreach(fun(MetadataPrefix) ->
        update_test(Config, MetadataPrefix)
    end, ozt_handles:supported_metadata_prefixes()).

update_test(Config, MetadataPrefix) ->
    MemberWithoutPrivs = ozt_users:create(),
    MemberWithPrivs = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    HServiceAdminWithManageHandles = ozt_users:create(),
    HServiceMemberWithoutPrivs = ozt_users:create(),
    HService = ozt_users:create_handle_service_for(HServiceAdminWithManageHandles),
    ozt_handle_services:add_user(
        HService, HServiceMemberWithoutPrivs, privileges:handle_service_admin() -- [?HANDLE_SERVICE_MANAGE_HANDLES]
    ),

    Space = ozt_users:create_space_for(MemberWithoutPrivs),

    PreexistingRawMetadata = ozt_handles:example_input_metadata(MetadataPrefix, 1),
    TargetRawMetadata = ozt_handles:example_input_metadata(MetadataPrefix, 2),

    AllPrivs = privileges:handle_privileges(),
    EnvSetUpFun = fun() ->
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, datastore_key:new(), ?SHARE_NAME1, Space
        ),

        HandleId = ozt_handles:create(HService, ShareId, MetadataPrefix, PreexistingRawMetadata),

        {ok, MemberWithoutPrivs} = oz_test_utils:handle_add_user(Config, HandleId, MemberWithoutPrivs),
        oz_test_utils:handle_set_user_privileges(Config, HandleId, MemberWithoutPrivs,
            AllPrivs -- [?HANDLE_UPDATE], [?HANDLE_UPDATE]
        ),
        {ok, MemberWithPrivs} = oz_test_utils:handle_add_user(Config, HandleId, MemberWithPrivs),
        oz_test_utils:handle_set_user_privileges(Config, HandleId, MemberWithPrivs,
            [?HANDLE_UPDATE], AllPrivs -- [?HANDLE_UPDATE]
        ),
        #{handleId => HandleId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{handleId := HandleId}, _Data) ->
        Handle = ozt_handles:get(HandleId),
        ExpMetadata = case ShouldSucceed of
            false -> ozt_handles:expected_final_metadata(HandleId, 1);
            true -> ozt_handles:expected_final_metadata(HandleId, 2)
        end,
        ?assertEqual(ExpMetadata, Handle#od_handle.metadata)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_UPDATE]},
                {user, MemberWithPrivs},
                {user, HServiceAdminWithManageHandles}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, MemberWithoutPrivs},
                {user, HServiceMemberWithoutPrivs},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/handles/">>, handleId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = update,
            args = [auth, handleId, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_handle, id = handleId, aspect = instance},
            expected_result_op = ?OK_RES
        },
        data_spec = #data_spec{
            required = [<<"metadata">>],
            correct_values = #{<<"metadata">> => [TargetRawMetadata]},
            bad_values = [
                {<<"metadata">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"metadata">>)},
                {<<"metadata">>, ?RAND_UNICODE_STR(100001),
                    ?ERROR_BAD_VALUE_TEXT_TOO_LARGE(<<"metadata">>, 100000)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    MemberWithoutPrivs = ozt_users:create(),
    MemberWithPrivs = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    HServiceAdminWithManageHandles = ozt_users:create(),
    HServiceMemberWithoutPrivs = ozt_users:create(),
    HService = ozt_users:create_handle_service_for(HServiceAdminWithManageHandles),
    ozt_handle_services:add_user(
        HService, HServiceMemberWithoutPrivs, privileges:handle_service_admin() -- [?HANDLE_SERVICE_MANAGE_HANDLES]
    ),

    Space = ozt_users:create_space_for(MemberWithoutPrivs),

    AllHandlePrivs = privileges:handle_privileges(),
    EnvSetUpFun = fun() ->
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, datastore_key:new(), ?SHARE_NAME1, Space
        ),
        HandleId = ozt_handles:create(HService, ShareId),

        {ok, MemberWithoutPrivs} = oz_test_utils:handle_add_user(Config, HandleId, MemberWithoutPrivs),
        oz_test_utils:handle_set_user_privileges(Config, HandleId, MemberWithoutPrivs,
            AllHandlePrivs -- [?HANDLE_DELETE], [?HANDLE_DELETE]
        ),
        {ok, MemberWithPrivs} = oz_test_utils:handle_add_user(Config, HandleId, MemberWithPrivs),
        oz_test_utils:handle_set_user_privileges(Config, HandleId, MemberWithPrivs,
            [?HANDLE_DELETE], AllHandlePrivs -- [?HANDLE_DELETE]
        ),
        #{handleId => HandleId}
    end,
    DeleteEntityFun = fun(#{handleId := HandleId} = _Env) ->
        oz_test_utils:delete_handle(Config, HandleId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{handleId := HandleId} = _Env, _) ->
        ?assertEqual({ok, not ShouldSucceed}, ozt:rpc(od_handle, exists, [HandleId])),
        ?assertEqual(not ShouldSucceed, lists:member(HandleId, ozt_handles:list()))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_DELETE]},
                {user, MemberWithPrivs},
                {user, HServiceAdminWithManageHandles}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, MemberWithoutPrivs},
                {user, HServiceMemberWithoutPrivs},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/handles/">>, handleId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = delete,
            args = [auth, handleId],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_handle, id = handleId, aspect = instance},
            expected_result_op = ?OK_RES
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


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

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    ozt_mocks:mock_handle_proxy(),
    Config.

end_per_testcase(_, _Config) ->
    ozt:delete_all_entities(),
    ozt_mocks:unmock_handle_proxy(),
    ozt_mocks:unfreeze_time().
