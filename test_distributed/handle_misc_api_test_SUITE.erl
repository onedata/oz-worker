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
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/api_errors.hrl").

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
                Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
            ),
            {ok, HandleId} = oz_test_utils:create_handle(
                Config, ?USER(U1), ?HANDLE(HService, ShareId)
            ),
            HandleId
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
        % TODO gs
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
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ?SHARE_ID_1, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
    ),

    VerifyFun = fun(HandleId, HService) ->
        {ok, Handle} = oz_test_utils:get_handle(Config, HandleId),
        ?assertEqual(<<"Share">>, Handle#od_handle.resource_type),
        ?assertEqual(ShareId, Handle#od_handle.resource_id),
        ?assertEqual(?DC_METADATA, Handle#od_handle.metadata),
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
            expected_headers = ?OK_ENV(fun(_Env, Data) ->
                HService = maps:get(<<"handleServiceId">>, Data),
                fun(#{<<"Location">> := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/handles/">>]),
                    [HandleId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(HandleId, HService)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(_Env, Data) ->
                HService = maps:get(<<"handleServiceId">>, Data),
                ?OK_TERM(fun(Result) -> VerifyFun(Result, HService) end)
            end)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [
                <<"handleServiceId">>, <<"resourceType">>,
                <<"resourceId">>, <<"metadata">>
            ],
            correct_values = #{
                <<"handleServiceId">> => [DoiHService, PidHService],
                <<"resourceType">> => [<<"Share">>],
                <<"resourceId">> => [ShareId],
                <<"metadata">> => [?DC_METADATA]
            },
            bad_values = [
                {<<"handleServiceId">>, <<"">>,
                    ?ERROR_BAD_VALUE_EMPTY(<<"handleServiceId">>)},
                {<<"handleServiceId">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"handleServiceId">>)},
                {<<"resourceType">>, <<"">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resourceType">>,
                        [<<"Share">>])},
                {<<"resourceType">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"resourceType">>)},
                {<<"resourceId">>, <<"">>,
                    ?ERROR_BAD_VALUE_EMPTY(<<"resourceId">>)},
                {<<"resourceId">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"resourceId">>)},
                {<<"metadata">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"metadata">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


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
        Config, ?ROOT, ?SHARE_ID_1, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
    ),

    HandleDetails = #{
        <<"handleServiceId">> => HService,
        <<"resourceType">> => <<"Share">>,
        <<"resourceId">> => ShareId,
        <<"metadata">> => ?DC_METADATA
    },
    {ok, HandleId} = oz_test_utils:create_handle(
        Config, ?USER(U1), HandleDetails
    ),
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
                    ?assertEqual(?DC_METADATA, Metadata),
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
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"effectiveUsers">> => #{
                    U1 => AllPrivsBin -- [<<"handle_view">>],
                    U2 => [<<"handle_view">>]
                },
                <<"effectiveGroups">> => #{},
                <<"handleServiceId">> => HService,
                <<"metadata">> => ?DC_METADATA,
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
    GetSharedDataApiTestSpec = #api_test_spec{
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
            expected_body = {contains, HandleDetails#{
                <<"handleId">> => HandleId
            }}
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = get_protected_data,
            args = [auth, HandleId],
            expected_result = ?OK_MAP_CONTAINS(HandleDetails)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, GetSharedDataApiTestSpec)).


update_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    ShareId = ?UNIQUE_STRING,
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
    ),

    AllPrivs = privileges:handle_privileges(),
    EnvSetUpFun = fun() ->
        {ok, HandleId} = oz_test_utils:create_handle(
            Config, ?ROOT, ?HANDLE(HService, ShareId)
        ),

        {ok, U1} = oz_test_utils:handle_add_user(Config, HandleId, U1),
        oz_test_utils:handle_set_user_privileges(Config, HandleId, U1,
            AllPrivs -- [?HANDLE_UPDATE], [?HANDLE_UPDATE]
        ),
        {ok, U2} = oz_test_utils:handle_add_user(Config, HandleId, U2),
        oz_test_utils:handle_set_user_privileges(Config, HandleId, U2,
            [?HANDLE_UPDATE], AllPrivs -- [?HANDLE_UPDATE]
        ),
        #{handleId => HandleId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{handleId := HandleId}, Data) ->
        {ok, Handle} = oz_test_utils:get_handle(Config, HandleId),
        ExpMetadata = case ShouldSucceed of
            false -> ?DC_METADATA;
            true -> maps:get(<<"metadata">>, Data)
        end,
        ?assertEqual(ExpMetadata, Handle#od_handle.metadata)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_UPDATE]},
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
            path = [<<"/handles/">>, handleId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = update,
            args = [auth, handleId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_handle, id = handleId, aspect = instance},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [<<"metadata">>],
            correct_values = #{<<"metadata">> => [?DC_METADATA2]},
            bad_values = [
                {<<"metadata">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"metadata">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    ShareId = ?UNIQUE_STRING,
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
    ),

    AllHandlePrivs = privileges:handle_privileges(),
    EnvSetUpFun = fun() ->
        {ok, HandleId} = oz_test_utils:create_handle(
            Config, ?ROOT, ?HANDLE(HService, ShareId)
        ),

        {ok, U1} = oz_test_utils:handle_add_user(Config, HandleId, U1),
        oz_test_utils:handle_set_user_privileges(Config, HandleId, U1,
            AllHandlePrivs -- [?HANDLE_DELETE], [?HANDLE_DELETE]
        ),
        {ok, U2} = oz_test_utils:handle_add_user(Config, HandleId, U2),
        oz_test_utils:handle_set_user_privileges(Config, HandleId, U2,
            [?HANDLE_DELETE], AllHandlePrivs -- [?HANDLE_DELETE]
        ),
        #{handleId => HandleId}
    end,
    DeleteEntityFun = fun(#{handleId := HandleId} = _Env) ->
        oz_test_utils:delete_handle(Config, HandleId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{handleId := HandleId} = _Env, _) ->
        {ok, Handles} = oz_test_utils:list_handles(Config),
        ?assertEqual(lists:member(HandleId, Handles), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_DELETE]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
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
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_handle, id = handleId, aspect = instance},
            expected_result = ?OK
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
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


init_per_testcase(_, Config) ->
    oz_test_utils:mock_handle_proxy(Config),
    Config.


end_per_testcase(_, Config) ->
    oz_test_utils:delete_all_entities(Config),
    oz_test_utils:unmock_handle_proxy(Config).
