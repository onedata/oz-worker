%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user handles API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(user_handles_api_test_SUITE).
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
    list_handles_test/1,
    create_handle_test/1,
    get_handle_test/1,
    leave_handle_test/1,

    list_eff_handles_test/1,
    get_eff_handle_test/1
]).

all() ->
    ?ALL([
        list_handles_test,
        create_handle_test,
        get_handle_test,
        leave_handle_test,

        list_eff_handles_test,
        get_eff_handle_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_handles_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME2),
    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),

    % Create 3 handles, 2 for S1 and 1 for S2
    ExpHandles = lists:map(
        fun(SpaceId) ->
            ShareId = ?UNIQUE_STRING,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, SpaceId
            ),
            {ok, HandleId} = oz_test_utils:create_handle(
                Config, ?ROOT, ?HANDLE(HServiceId, ShareId)
            ),
            {ok, U1} = oz_test_utils:handle_add_user(Config, HandleId, U1),
            {ok, U2} = oz_test_utils:handle_add_user(Config, HandleId, U2),
            HandleId
        end, [S1, S1, S2]
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/handles">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handles">> => ExpHandles}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_USERS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_handles,
            args = [auth, U1],
            expected_result = ?OK_LIST(ExpHandles)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


create_handle_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, U1} = oz_test_utils:handle_service_add_user(Config, HService, U1),
    {ok, U2} = oz_test_utils:handle_service_add_user(Config, HService, U2),

    {ok, ShareIdThatAlreadyHasAHandle} = oz_test_utils:create_share(
        Config, ?ROOT, datastore_key:new(), ?SHARE_NAME1, ?ROOT_FILE_ID, S1
    ),
    {ok, _} = oz_test_utils:create_handle(
        Config, ?ROOT, ?HANDLE(HService, ShareIdThatAlreadyHasAHandle)
    ),

    AllPrivs = privileges:handle_privileges(),
    ExpResourceType = <<"Share">>,

    EnvSetUpFun = fun() ->
        ShareId = datastore_key:new(),
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
        ),
        #{shareId => ShareId}
    end,

    VerifyResult = fun(#{shareId := ShareId}, HandleId) ->
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        {ok, Handle} = oz_test_utils:get_handle(Config, HandleId),
        ?assertEqual(ExpResourceType, Handle#od_handle.resource_type),
        ?assertEqual(ShareId, Handle#od_handle.resource_id),
        ?assertEqual(HService, Handle#od_handle.handle_service),

        [User] = ?assertMatch([_], maps:keys(Handle#od_handle.users)),
        ?assertEqual(#{User => AllPrivs}, Handle#od_handle.users),
        ?assertEqual(#{User => {AllPrivs, [{od_handle, <<"self">>}]}}, Handle#od_handle.eff_users),
        ?assertEqual(#{}, Handle#od_handle.groups),
        ?assertEqual(#{}, Handle#od_handle.eff_groups),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, U1}]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/handles">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(Env, _Data) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/user/handles/">>]),
                    [HandleId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyResult(Env, HandleId)
                end
            end)
        },
        data_spec = DataSpec = #data_spec{
            required = [
                <<"handleServiceId">>,
                <<"resourceType">>,
                <<"resourceId">>,
                <<"metadata">>
            ],
            correct_values = #{
                <<"handleServiceId">> => [HService],
                <<"resourceType">> => [<<"Share">>],
                <<"resourceId">> => [fun(#{shareId := ShareId} = _Env) -> ShareId end],
                <<"metadata">> => [?DC_METADATA]
            },
            bad_values = [
                {<<"handleServiceId">>, <<"">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"handleServiceId">>)},
                {<<"handleServiceId">>, 1234, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"handleServiceId">>)},
                {<<"resourceType">>, <<"">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resourceType">>,
                        [<<"Share">>])},
                {<<"resourceType">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"resourceType">>)},
                {<<"resourceId">>, <<"">>, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"resourceId">>)},
                {<<"resourceId">>, 1234, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"resourceId">>)},
                {<<"resourceId">>, ShareIdThatAlreadyHasAHandle, ?ERROR_ALREADY_EXISTS},
                {<<"metadata">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"metadata">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, undefined)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {admin, [?OZ_HANDLES_CREATE, ?OZ_USERS_ADD_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_handle,
            args = [auth, U1, data],
            expected_result = ?OK_ENV(fun(Env, _Data) ->
                ?OK_TERM(fun(HandleId) -> VerifyResult(Env, HandleId) end)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_handle, aspect = instance},
            auth_hint = ?AS_USER(U1),
            expected_result = ?OK_ENV(fun(#{shareId := ShareId} = Env, _Data) ->
                ?OK_MAP_CONTAINS(#{
                    <<"metadata">> => ?DC_METADATA,
                    <<"handleServiceId">> => HService,
                    <<"resourceType">> => ExpResourceType,
                    <<"resourceId">> => ShareId,
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
                        VerifyResult(Env, Id)
                    end
                })
            end)
        },
        data_spec = DataSpec#data_spec{
            bad_values = []
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2, EnvSetUpFun, undefined, undefined)),

    % Root client bypasses authorization checks,
    % hence wrong values of handleServiceId or resourceId
    % cause validation errors rather than authorization errors.
    RootApiTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root
            ]
        },
        gs_spec = undefined,
        data_spec = DataSpec#data_spec{
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
                {<<"resourceId">>, 1234,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"resourceId">>)},
                {<<"metadata">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"metadata">>)},
                {<<"metadata">>, <<"">>,
                    ?ERROR_BAD_VALUE_EMPTY(<<"metadata">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec, EnvSetUpFun, undefined, undefined)).


get_handle_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, ShareId} = oz_test_utils:create_share(Config, ?ROOT,
        ?UNIQUE_STRING, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
    ),
    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),

    HandleData = ?HANDLE(HServiceId, ShareId),
    {ok, HandleId} = oz_test_utils:create_handle(Config, ?ROOT, HandleData),
    {ok, U1} = oz_test_utils:handle_add_user(Config, HandleId, U1),
    {ok, U2} = oz_test_utils:handle_add_user(Config, HandleId, U2),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/handles/">>, HandleId],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:protected_handle(rest, HandleId, HandleData, ?SUB(nobody))
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_VIEW]},
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_handle,
            args = [auth, U1, HandleId],
            expected_result = api_test_expect:protected_handle(logic, HandleId, HandleData, ?SUB(nobody))
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


leave_handle_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),

    EnvSetUpFun = fun() ->
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, datastore_key:new(), ?SHARE_NAME1, ?ROOT_FILE_ID, S1
        ),
        {ok, HandleId} = oz_test_utils:create_handle(
            Config, ?ROOT, ?HANDLE(HServiceId, ShareId)
        ),
        {ok, U1} = oz_test_utils:handle_add_user(Config, HandleId, U1),
        #{handleId => HandleId}
    end,
    DeleteEntityFun = fun(#{handleId := HandleId} = _Env) ->
        oz_test_utils:handle_remove_user(Config, HandleId, U1)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{handleId := HandleId} = _Env, _) ->
        {ok, Users} = oz_test_utils:handle_get_users(Config, HandleId),
        ?assertEqual(lists:member(U1, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/handles/">>, handleId],
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS]},
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = leave_handle,
            args = [auth, U1, handleId],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])).


list_eff_handles_test(Config) ->
    {
        [{H1, _}, {H2, _}, {H3, _}, {H4, _}, {H5, _}],
        _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_handles_env(Config),

    ExpHandles = [H1, H2, H3, H4, H5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/effective_handles">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handles">> => ExpHandles}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_USERS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_handles,
            args = [auth, U1],
            expected_result = ?OK_LIST(ExpHandles)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % Check also user_logic:has_eff_handle function
    lists:foreach(
        fun(Handle) ->
            ?assert(oz_test_utils:call_oz(
                Config, user_logic, has_eff_handle, [U2, Handle])
            )
        end, ExpHandles
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, user_logic, has_eff_handle, [U2, <<"asdiucyaie827346w">>])
    ).


get_eff_handle_test(Config) ->
    {
        EffHandlesList, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_handles_env(Config),

    lists:foreach(fun({HandleId, HandleData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    {user, U1},
                    {user, U2}
                ]
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/user/effective_handles/">>, HandleId],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:protected_handle(rest, HandleId, HandleData, ?SUB(nobody))
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

        % Check that regular client can't make request
        % on behalf of other client
        ApiTestSpec2 = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_HANDLES_VIEW]},
                    {user, U1}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, U2},
                    {user, NonAdmin}
                ]
            },
            logic_spec = #logic_spec{
                module = user_logic,
                function = get_eff_handle,
                args = [auth, U1, HandleId],
                expected_result = api_test_expect:protected_handle(logic, HandleId, HandleData, ?SUB(nobody))
            }
            % TODO VFS-4520 Tests for GraphSync API
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

    end, EffHandlesList).


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
    ozt_mocks:unmock_handle_proxy(),
    ozt_mocks:unfreeze_time().
