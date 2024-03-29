%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning harvester basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_misc_api_test_SUITE).
-author("Michal Stanisz").

-include("http/rest.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
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
    create_test/1,
    list_test/1,
    list_privileges_test/1,
    get_test/1,
    get_gui_plugin_config_test/1,
    update_test/1,
    update_gui_plugin_config_test/1,
    delete_test/1,
    delete_harvested_metadata_test/1,

    create_index_test/1,
    list_indices_test/1,
    get_index_test/1,
    get_index_stats_test/1,
    update_index_test/1,
    delete_index_test/1,
    delete_index_metadata_test/1,
    query_index_test/1,

    submit_batch_test/1,
    submit_batch_index_stats_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        list_privileges_test,
        get_test,
        get_gui_plugin_config_test,
        update_test,
        update_gui_plugin_config_test,
        delete_test,
        delete_harvested_metadata_test,

        create_index_test,
        list_indices_test,
        get_index_test,
        get_index_stats_test,
        update_index_test,
        delete_index_test,
        delete_index_metadata_test,
        query_index_test,

        submit_batch_test,
        submit_batch_index_stats_test
    ]).


-define(TEST_DATA, <<"test_data">>).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    VerifyFun = fun(HarvesterId, Data) ->
        ExpConfig = maps:get(<<"guiPluginConfig">>, Data, #{}),
        % dummy defaults needed so get_env does not throw if env not defined
        ExpBackendType = maps:get(
            <<"harvestingBackendType">>, Data, oz_test_utils:get_env(Config, default_harvesting_backend_type, dummy_default)
        ),
        ExpEndpoint = utils:null_to_undefined(maps:get(<<"harvestingBackendEndpoint">>, Data,
            oz_test_utils:get_env(Config, default_harvesting_backend_endpoint, <<"dummy_default">>)
        )),
        {ok, Harvester} = oz_test_utils:get_harvester(Config, HarvesterId),
        ?assertEqual(?CORRECT_NAME, Harvester#od_harvester.name),
        ?assertEqual(ExpBackendType, Harvester#od_harvester.backend),
        ?assertEqual(ExpEndpoint, Harvester#od_harvester.endpoint),
        ?assertEqual(ExpConfig, Harvester#od_harvester.gui_plugin_config),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_CREATE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/harvesters">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, Data) ->
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/harvesters/">>]),
                    [HarvesterId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(HarvesterId, Data)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(_, Data) ->
                ?OK_TERM(fun(HarvesterId) -> VerifyFun(HarvesterId, Data) end)
            end)
        },
        data_spec = DataSpec = #data_spec{
            required = [<<"name">>, <<"harvestingBackendType">>, <<"harvestingBackendEndpoint">>],
            optional = [<<"guiPluginConfig">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"harvestingBackendEndpoint">> => [?HARVESTER_ENDPOINT1],
                <<"harvestingBackendType">> => [?HARVESTER_MOCK_BACKEND],
                <<"guiPluginConfig">> => [?HARVESTER_GUI_PLUGIN_CONFIG]
            },
            bad_values = [
                {<<"harvestingBackendType">>, <<"not_existing_backend">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"harvestingBackendType">>, ?ALL_HARVESTING_BACKENDS(Config))},
                {<<"harvestingBackendEndpoint">>, <<"bad_endpoint">>,
                    ?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(<<"Elasticsearch">>)},
                {<<"harvestingBackendEndpoint">>, null, ?ERROR_BAD_VALUE_EMPTY(<<"harvestingBackendEndpoint">>)}
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check that backend type and endpoint are optional when defaults are set in Onezone 
    oz_test_utils:set_env(Config, default_harvesting_backend_type, ?HARVESTER_MOCK_BACKEND),
    oz_test_utils:set_env(Config, default_harvesting_backend_endpoint, ?HARVESTER_ENDPOINT2),
    ApiTestSpec1 = ApiTestSpec#api_test_spec{
        data_spec = DataSpec#data_spec{
            required = [<<"name">>],
            optional = [<<"harvestingBackendType">>, <<"harvestingBackendEndpoint">>, <<"guiPluginConfig">>]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec1)).


list_test(Config) ->
    % Make sure that harvesters created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    {ok, H2} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    {ok, H3} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    {ok, H4} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    {ok, H5} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    ExpHarvesters = [H1, H2, H3, H4, H5],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_LIST]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/harvesters">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"harvesters">> => ExpHarvesters}
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = list,
            args = [auth],
            expected_result = ?OK_LIST(ExpHarvesters)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also harvester_logic:exist function
    lists:foreach(
        fun(HarvesterId) ->
            ?assert(oz_test_utils:call_oz(
                Config, harvester_logic, exists, [HarvesterId])
            )
        end, ExpHarvesters
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, harvester_logic, exists, [<<"asdiucyaie827346w">>])
    ).


list_privileges_test(Config) ->

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/harvesters/privileges">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"member">> => [atom_to_binary(P, utf8) || P <- privileges:harvester_member()],
                <<"manager">> => [atom_to_binary(P, utf8) || P <- privileges:harvester_manager()],
                <<"admin">> => [atom_to_binary(P, utf8) || P <- privileges:harvester_admin()]
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, {P1, T1}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, T2}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),

    AllPrivs = privileges:harvester_privileges(),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U1,
        AllPrivs -- [?HARVESTER_VIEW], [?HARVESTER_VIEW]
    ),
    oz_test_utils:harvester_add_user(Config, H1, U2),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U2,
        [?HARVESTER_VIEW], AllPrivs -- [?HARVESTER_VIEW]
    ),

    {ok, S} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),

    oz_test_utils:harvester_add_space(Config, H1, S),
    oz_test_utils:support_space_by_provider(Config, P1, S),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1, T1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {admin, [?OZ_HARVESTERS_VIEW]},
                {provider, P2, T2},
                {user, NonAdmin},
                {user, U1}
            ]
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get,
            args = [auth, H1],
            expected_result = ?OK_TERM(
                fun(#od_harvester{
                    name = Name, users = Users, groups = #{},
                    spaces = Spaces,
                    backend = Backend, public = Public,
                    indices = Indices,
                    eff_users = EffUsers, eff_groups = #{},
                    bottom_up_dirty = false
                }) ->
                    ?assertEqual(?HARVESTER_NAME1, Name),
                    ?assertEqual(?HARVESTER_MOCK_BACKEND, Backend),
                    ?assertEqual(false, Public),
                    ?assertEqual(#{}, Indices),
                    ?assertEqual(Users, #{
                        U1 => AllPrivs -- [?HARVESTER_VIEW],
                        U2 => [?HARVESTER_VIEW]}
                    ),
                    ?assertEqual(Spaces, [S]),
                    ?assertEqual(EffUsers, #{
                        U1 => {AllPrivs -- [?HARVESTER_VIEW], [{od_harvester, <<"self">>}]},
                        U2 => {[?HARVESTER_VIEW], [{od_harvester, <<"self">>}]}
                    })
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_harvester, id = H1, aspect = instance, scope = private},
            expected_result_op = ?OK_MAP_CONTAINS(#{
                <<"indices">> => [],
                <<"spaces">> => [S],
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(H1, Id)
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
                {admin, [?OZ_HARVESTERS_VIEW]},
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
            path = [<<"/harvesters/">>, H1],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:protected_harvester(rest, H1, ?HARVESTER_CREATE_DATA, ?SUB(user, U1))
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_protected_data,
            args = [auth, H1],
            expected_result = api_test_expect:protected_harvester(logic, H1, ?HARVESTER_CREATE_DATA, ?SUB(user, U1))
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)),

    % If the harvester is not public, the public scope cannot be fetched
    GetPublicDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, U2},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_public_data,
            args = [auth, H1],
            expected_result = api_test_expect:shared_or_public_harvester(logic, H1, ?HARVESTER_CREATE_DATA)
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPublicDataApiTestSpec)),

    % If the harvester is made public, anyone can fetch the public scope
    oz_test_utils:update_harvester(Config, H1, #{<<"public">> => true}),
    ?assert(api_test_utils:run_tests(Config, GetPublicDataApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {admin, [?OZ_HARVESTERS_VIEW]},
                {user, U1},
                {user, U2},
                {user, NonAdmin}
            ]
        }
    })).


get_gui_plugin_config_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:harvester_privileges(),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U1,
        AllPrivs -- [?HARVESTER_VIEW], [?HARVESTER_VIEW]
    ),
    oz_test_utils:harvester_add_user(Config, H1, U2),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U2,
        [?HARVESTER_VIEW], AllPrivs -- [?HARVESTER_VIEW]
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_VIEW]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/harvesters/">>, H1, <<"/gui_plugin_config">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"guiPluginConfig">> => ?HARVESTER_GUI_PLUGIN_CONFIG
            }
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_gui_plugin_config,
            args = [auth, H1],
            expected_result = ?OK_TERM(
                fun(Conf) -> ?assertEqual(?HARVESTER_GUI_PLUGIN_CONFIG, Conf) end
            )
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Test that anyone can get gui plugin config from public harvester
    oz_test_utils:update_harvester(Config, H1, #{<<"public">> => true}),

    GetPublicDataApiTestSpec1 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {admin, [?OZ_HARVESTERS_VIEW]},
                {user, U1},
                {user, U2},
                {user, NonAdmin}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPublicDataApiTestSpec1)).


update_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA(?CORRECT_NAME)),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U1, [], [
            ?HARVESTER_UPDATE
        ]),
        oz_test_utils:harvester_add_user(Config, H1, U2),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U2, [
            ?HARVESTER_UPDATE
        ], []),
        #{harvesterId => H1}
    end,
    Endpoint = ?HARVESTER_ENDPOINT2,

    ExpValueFun = fun(ShouldSucceed, Key, Data, Default) ->
        case ShouldSucceed of
            false -> Default;
            true -> maps:get(Key, Data, Default)
        end
    end,

    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId} = _Env, Data) ->
        {ok, Harvester} = oz_test_utils:get_harvester(Config, HarvesterId),

        ExpName = ExpValueFun(ShouldSucceed, <<"name">>, Data, ?CORRECT_NAME),
        ExpEndpoint = ExpValueFun(ShouldSucceed, <<"harvestingBackendEndpoint">>, Data, ?HARVESTER_ENDPOINT1),
        ExpBackend = ExpValueFun(ShouldSucceed, <<"harvestingBackendType">>, Data, ?HARVESTER_MOCK_BACKEND),
        ExpPublic = ExpValueFun(ShouldSucceed, <<"public">>, Data, false),

        ?assertEqual(ExpName, Harvester#od_harvester.name),
        ?assertEqual(utils:null_to_undefined(ExpEndpoint), Harvester#od_harvester.endpoint),
        ?assertEqual(ExpBackend, Harvester#od_harvester.backend),
        ?assertEqual(ExpPublic, Harvester#od_harvester.public)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_UPDATE]},
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
            path = [<<"/harvesters/">>, harvesterId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = update,
            args = [auth, harvesterId, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_harvester, id = harvesterId, aspect = instance},
            expected_result_op = ?OK_RES
        },
        data_spec = #data_spec{
            at_least_one = [<<"name">>, <<"harvestingBackendEndpoint">>, <<"harvestingBackendType">>, <<"public">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"harvestingBackendEndpoint">> => [Endpoint],
                <<"harvestingBackendType">> => [?HARVESTER_MOCK_BACKEND2],
                <<"public">> => [true, false]
            },
            bad_values = [
                {<<"harvestingBackendType">>, <<"not_existing_backend">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"harvestingBackendType">>, ?ALL_HARVESTING_BACKENDS(Config))},
                {<<"public">>, not_boolean, ?ERROR_BAD_VALUE_BOOLEAN(<<"public">>)},
                {<<"harvestingBackendEndpoint">>, <<"bad_endpoint">>,
                    ?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(<<"Elasticsearch">>)},
                {<<"harvestingBackendEndpoint">>, null, ?ERROR_BAD_VALUE_EMPTY(<<"harvestingBackendEndpoint">>)}
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


update_gui_plugin_config_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA(?CORRECT_NAME)),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U1, [], [
            ?HARVESTER_UPDATE
        ]),
        oz_test_utils:harvester_add_user(Config, H1, U2),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U2, [
            ?HARVESTER_UPDATE
        ], []),
        #{harvesterId => H1}
    end,
    Conf = ?HARVESTER_GUI_PLUGIN_CONFIG#{<<"x">> => <<"y">>},

    ExpValueFun = fun(ShouldSucceed, Key, Data, Default) ->
        case ShouldSucceed of
            false -> Default;
            true -> maps:get(Key, Data, Default)
        end
    end,

    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId} = _Env, Data) ->
        {ok, Harvester} = oz_test_utils:get_harvester(Config, HarvesterId),

        ExpConfig = ExpValueFun(ShouldSucceed, <<"guiPluginConfig">>, Data, ?HARVESTER_GUI_PLUGIN_CONFIG),

        ?assertEqual(ExpConfig, Harvester#od_harvester.gui_plugin_config)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_UPDATE]},
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
            path = [<<"/harvesters/">>, harvesterId, <<"/gui_plugin_config">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = update_gui_plugin_config,
            args = [auth, harvesterId, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_harvester, id = harvesterId, aspect = gui_plugin_config},
            expected_result_op = ?OK_RES
        },
        data_spec = #data_spec{
            required = [<<"guiPluginConfig">>],
            correct_values = #{
                <<"guiPluginConfig">> => [Conf]
            },
            bad_values = [{<<"guiPluginConfig">>, <<"bad_config">>, ?ERROR_BAD_VALUE_JSON(<<"guiPluginConfig">>)}]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    OzNodes = ?config(oz_worker_nodes, Config),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
        oz_test_utils:harvester_set_user_privileges(
            Config, H1, U1, [], [?HARVESTER_DELETE]
        ),
        oz_test_utils:harvester_add_user(Config, H1, U2),
        oz_test_utils:harvester_set_user_privileges(
            Config, H1, U2, [?HARVESTER_DELETE], []
        ),
        oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
        #{harvesterId => H1}
    end,
    DeleteEntityFun = fun(#{harvesterId := HarvesterId} = _Env) ->
        oz_test_utils:delete_harvester(Config, HarvesterId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId} = _Env, _Data) ->
        {ok, Harvesters} = oz_test_utils:list_harvesters(Config),
        test_utils:mock_assert_num_calls(OzNodes, ?HARVESTER_MOCK_BACKEND, delete_index, 3, 0),
        ?assertEqual(lists:member(HarvesterId, Harvesters), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_DELETE]},
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
            path = [<<"/harvesters/">>, harvesterId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = delete,
            args = [auth, harvesterId],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_harvester, id = harvesterId, aspect = instance},
            expected_result_op = ?OK_RES
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


delete_harvested_metadata_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    OzNodes = ?config(oz_worker_nodes, Config),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
        oz_test_utils:harvester_set_user_privileges(
            Config, H1, U1, [], [?HARVESTER_DELETE]
        ),
        oz_test_utils:harvester_add_user(Config, H1, U2),
        oz_test_utils:harvester_set_user_privileges(
            Config, H1, U2, [?HARVESTER_DELETE], []
        ),
        oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
        oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
        #{harvesterId => H1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId} = _Env, _Data) ->
        case ShouldSucceed of
            true ->
                mock_assert_num_calls_sum(OzNodes, ?HARVESTER_MOCK_BACKEND, delete_index, 2, 2);
            _ ->
                test_utils:mock_assert_num_calls(OzNodes, ?HARVESTER_MOCK_BACKEND, delete_index, 2, 0)
        end,
        lists:foreach(fun(Node) ->
            rpc:call(Node, meck, reset, [?HARVESTER_MOCK_BACKEND])
        end, OzNodes),
        % assert that harvester was not deleted
        {ok, Harvesters} = oz_test_utils:list_harvesters(Config),
        ?assert(lists:member(HarvesterId, Harvesters))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_DELETE]},
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
            path = [<<"/harvesters/">>, harvesterId, <<"/metadata">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = delete_harvested_metadata,
            args = [auth, harvesterId],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_harvester, id = harvesterId, aspect = metadata},
            expected_result_op = ?OK_RES
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)).


create_index_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA(?CORRECT_NAME)),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U1, [], [?HARVESTER_UPDATE]),
    oz_test_utils:harvester_add_user(Config, H1, U2),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U2, [?HARVESTER_UPDATE], []),

    VerifyFun = fun(IndexId, Data) ->
        {ok, Harvester} = oz_test_utils:get_harvester(Config, H1),
        Indices = Harvester#od_harvester.indices,
        ExpSchema = maps:get(<<"schema">>, Data, undefined),
        ExpGuiPluginName = utils:null_to_undefined(maps:get(<<"guiPluginName">>, Data, undefined)),

        ?assertEqual(true, lists:member(IndexId, maps:keys(Indices))),
        Index = maps:get(IndexId, Indices),
        ?assertEqual(?CORRECT_NAME, Index#harvester_index.name),
        ?assertEqual(ExpSchema, Index#harvester_index.schema),
        ?assertEqual(ExpGuiPluginName, Index#harvester_index.gui_plugin_name),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_HARVESTERS_UPDATE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/harvesters/">>, H1, <<"/indices">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, Data) ->
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/harvesters/">>, H1, <<"/indices/">>]),
                    [IndexId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(IndexId, Data)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = create_index,
            args = [auth, H1, data],
            expected_result = ?OK_ENV(fun(_, Data) ->
                ?OK_TERM(fun(IndexId) -> VerifyFun(IndexId, Data) end)
            end)
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            optional = [<<"schema">>, <<"guiPluginName">>, <<"includeMetadata">>,
                <<"includeFileDetails">>, <<"retryOnRejection">>, <<"includeRejectionReason">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"schema">> => [?HARVESTER_INDEX_SCHEMA],
                <<"guiPluginName">> => [?CORRECT_NAME, null],
                <<"includeMetadata">> => [lists_utils:random_sublist(od_harvester:all_metadata_types(), 1, all)],
                <<"includeFileDetails">> => [lists_utils:random_sublist(od_harvester:all_file_details(), 0, all)],
                <<"retryOnRejection">> => [true, false],
                <<"includeRejectionReason">> => [true, false]
            },
            bad_values = [
                {<<"schema">>, <<>>, ?ERROR_BAD_VALUE_EMPTY(<<"schema">>)},
                {<<"schema">>, 12321, ?ERROR_BAD_VALUE_BINARY(<<"schema">>)},
                {<<"guiPluginName">>, 12321, ?ERROR_BAD_VALUE_BINARY(<<"guiPluginName">>)},
                {<<"includeMetadata">>, json, ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"includeMetadata">>)},
                {<<"includeMetadata">>, [], ?ERROR_BAD_VALUE_EMPTY(<<"includeMetadata">>)},
                {<<"includeMetadata">>, [asd], ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"includeMetadata">>,
                    od_harvester:all_metadata_types())},
                {<<"includeFileDetails">>, fileName, ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"includeFileDetails">>)},
                {<<"includeFileDetails">>, [asd], ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"includeFileDetails">>,
                    od_harvester:all_file_details())},
                {<<"retryOnRejection">>, 12321, ?ERROR_BAD_VALUE_BOOLEAN(<<"retryOnRejection">>)},
                {<<"includeRejectionReason">>, 12321, ?ERROR_BAD_VALUE_BOOLEAN(<<"includeRejectionReason">>)}
            ] ++ ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_index_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:harvester_privileges(),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U1,
        AllPrivs -- [?HARVESTER_VIEW], [?HARVESTER_VIEW]
    ),
    oz_test_utils:harvester_add_user(Config, H1, U2),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U2,
        [?HARVESTER_VIEW], AllPrivs -- [?HARVESTER_VIEW]
    ),

    {ok, IndexId} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_VIEW]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/harvesters/">>, H1, <<"/indices/">>, IndexId],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"indexId">> => IndexId,
                <<"name">> => ?HARVESTER_INDEX_NAME,
                <<"schema">> => ?HARVESTER_INDEX_SCHEMA,
                <<"guiPluginName">> => null,
                <<"includeMetadata">> => [<<"json">>],
                <<"includeFileDetails">> => [],
                <<"includeRejectionReason">> => false,
                <<"retryOnRejection">> => false
            }
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_index,
            args = [auth, H1, IndexId],
            expected_result = ?OK_TERM(
                fun(#harvester_index{
                    name = Name,
                    schema = Schema,
                    gui_plugin_name = GuiPluginName,
                    include_metadata = IncludeMetadata,
                    include_file_details = IncludeFileDetails,
                    include_rejection_reason = IncludeRejectionReason,
                    retry_on_rejection = RetryOnRejection
                }) ->
                    ?assertEqual(?HARVESTER_INDEX_NAME, Name),
                    ?assertEqual(?HARVESTER_INDEX_SCHEMA, Schema),
                    ?assertEqual(undefined, GuiPluginName),
                    ?assertEqual([json], IncludeMetadata),
                    ?assertEqual([], IncludeFileDetails),
                    ?assertEqual(false, IncludeRejectionReason),
                    ?assertEqual(false, RetryOnRejection)
                end
            )
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % No one can get public index data when harvester is private
    GetPublicDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            forbidden = [
                {admin, [?OZ_HARVESTERS_VIEW]},
                {user, U1},
                {user, U2},
                {user, NonAdmin}
            ],
            unauthorized = [nobody]
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_public_index,
            args = [auth, H1, IndexId],
            expected_result = ?OK_MAP_CONTAINS(#{<<"guiPluginName">> => undefined})
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPublicDataApiTestSpec)),

    % Test that anyone can get public index data from public harvester
    oz_test_utils:update_harvester(Config, H1, #{<<"public">> => true}),

    GetPublicDataApiTestSpec1 = GetPublicDataApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {admin, [?OZ_HARVESTERS_VIEW]},
                {user, U1},
                {user, U2},
                {user, NonAdmin}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPublicDataApiTestSpec1)).


get_index_stats_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:harvester_privileges(),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U1,
        AllPrivs -- [?HARVESTER_VIEW], [?HARVESTER_VIEW]
    ),
    oz_test_utils:harvester_add_user(Config, H1, U2),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U2,
        [?HARVESTER_VIEW], AllPrivs -- [?HARVESTER_VIEW]
    ),

    {ok, IndexId} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_VIEW]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/harvesters/">>, H1, <<"/indices/">>, IndexId, <<"/stats">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{}
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_index_stats,
            args = [auth, H1, IndexId],
            expected_result = ?OK_MAP(#{})
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_index_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA(?CORRECT_NAME)),
        {ok, IndexId} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U1, [], [
            ?HARVESTER_UPDATE
        ]),
        oz_test_utils:harvester_add_user(Config, H1, U2),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U2, [
            ?HARVESTER_UPDATE
        ], []),
        #{harvesterId => H1, indexId => IndexId}
    end,


    ExpValueFun = fun(ShouldSucceed, Key, Data, Default) ->
        case ShouldSucceed of
            false -> Default;
            true -> maps:get(Key, Data, Default)
        end
    end,

    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId, indexId := IndexId} = _Env, Data) ->
        {ok, #od_harvester{indices = Indices}} = oz_test_utils:get_harvester(Config, HarvesterId),

        #harvester_index{
            name = ActualName,
            gui_plugin_name = ActualGuiPluginName
        } = maps:get(IndexId, Indices),

        ExpName = ExpValueFun(ShouldSucceed, <<"name">>, Data, ?HARVESTER_INDEX_NAME),
        ExpGuiPluginName = utils:null_to_undefined(ExpValueFun(ShouldSucceed, <<"guiPluginName">>, Data, undefined)),

        ?assertEqual(ExpName, ActualName),
        ?assertEqual(ExpGuiPluginName, ActualGuiPluginName)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_UPDATE]},
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
            path = [<<"/harvesters/">>, harvesterId, <<"/indices/">>, indexId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = update_index,
            args = [auth, harvesterId, indexId, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_harvester, id = harvesterId, aspect = {index, indexId}},
            expected_result_op = ?OK_RES
        },
        data_spec = #data_spec{
            at_least_one = [<<"name">>, <<"guiPluginName">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"guiPluginName">> => [?CORRECT_NAME, null]
            },
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_index_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    OzNodes = ?config(oz_worker_nodes, Config),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA(?CORRECT_NAME)),
        {ok, IndexId} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U1, [], [?HARVESTER_UPDATE]),
        oz_test_utils:harvester_add_user(Config, H1, U2),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U2, [?HARVESTER_UPDATE], []),
        #{harvesterId => H1, indexId => IndexId}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId, indexId := IndexId} = _Env, _Data) ->
        {ok, Harvester} = oz_test_utils:get_harvester(Config, HarvesterId),
        ExpIndices = case ShouldSucceed of
            false -> [IndexId];
            _ -> []
        end,
        test_utils:mock_assert_num_calls(OzNodes, ?HARVESTER_MOCK_BACKEND, delete_index, 3, 0),
        ?assertEqual(ExpIndices, maps:keys(Harvester#od_harvester.indices))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_HARVESTERS_UPDATE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/harvesters/">>, harvesterId, <<"/indices/">>, indexId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = delete_index,
            args = [auth, harvesterId, indexId],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_harvester, id = harvesterId, aspect = {index, indexId}},
            expected_result_op = ?OK_RES
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)).


delete_index_metadata_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    OzNodes = ?config(oz_worker_nodes, Config),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA(?CORRECT_NAME)),
        {ok, IndexId} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U1, [], [?HARVESTER_DELETE]),
        oz_test_utils:harvester_add_user(Config, H1, U2),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U2, [?HARVESTER_DELETE], []),
        #{harvesterId => H1, indexId => IndexId}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId, indexId := IndexId} = _Env, _Data) ->
        {ok, Harvester} = oz_test_utils:get_harvester(Config, HarvesterId),
        case ShouldSucceed of
            true ->
                mock_assert_num_calls_sum(OzNodes, ?HARVESTER_MOCK_BACKEND, delete_index, 2, 1);
            _ ->
                test_utils:mock_assert_num_calls(OzNodes, ?HARVESTER_MOCK_BACKEND, delete_index, 2, 0)
        end,
        % assert that index was not deleted from harvester
        ?assertEqual([IndexId], maps:keys(Harvester#od_harvester.indices))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_HARVESTERS_DELETE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/harvesters/">>, harvesterId, <<"/indices/">>, indexId, <<"/metadata">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = delete_index_metadata,
            args = [auth, harvesterId, indexId],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_harvester, id = harvesterId, aspect = {index_metadata, indexId}},
            expected_result_op = ?OK_RES
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)).


query_index_test(Config) ->
    {ok, Member} = oz_test_utils:create_user(Config),
    {ok, NonMember} = oz_test_utils:create_user(Config),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    oz_test_utils:harvester_add_user(Config, H1, Member),
    {ok, IndexId} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Member},
                {admin, [?OZ_HARVESTERS_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonMember}
            ]
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = query_index,
            args = [auth, H1, IndexId, data],
            expected_result = ?OK_MAP(?HARVESTER_MOCKED_QUERY_DATA_MAP)
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/harvesters/">>, H1, <<"/indices/">>, IndexId, <<"/query">>],
            expected_code = ?HTTP_200_OK,
            expected_body = ?HARVESTER_MOCKED_QUERY_DATA_MAP
        },
        data_spec = #data_spec{
            required = [<<"indexId">>, <<"path">>, <<"method">>],
            optional = [<<"body">>],
            correct_values = #{
                <<"path">> => [<<"example_request">>],
                <<"method">> => [<<"get">>, <<"post">>],
                <<"indexId">> => [IndexId],
                <<"body">> => [?TEST_DATA]
            },
            bad_values = [
                {<<"method">>, <<"bad_method">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"method">>, [post, get])},
                {<<"path">>, <<>>, ?ERROR_BAD_VALUE_EMPTY(<<"path">>)},
                {<<"body">>, <<>>, ?ERROR_BAD_VALUE_EMPTY(<<"body">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Test query request curl generation
    PrivateHarvesterApiTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, Member}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonMember}
            ]
        },
        rest_spec = undefined,
        logic_spec = LogicSpec = #logic_spec{
            module = harvester_logic,
            function = gen_curl_query,
            args = [auth, H1, IndexId, private, data],
            expected_result = ?OK_BINARY
        }
    },
    ?assert(api_test_utils:run_tests(Config, PrivateHarvesterApiTestSpec)),

    % check that generated curl works properly
    {ok, PrivateCurlBinary} = oz_test_utils:call_oz(Config, harvester_logic, gen_curl_query,
        [?USER(Member), H1, IndexId, private, #{<<"method">> => <<"post">>, <<"path">> => <<"path">>}]),
    PrivateCurlInsecure = binary:replace(PrivateCurlBinary, <<"curl">>, <<"curl -k -s">>),
    {ok, AccessToken} = oz_test_utils:create_client_token(Config, Member),
    PrivateCurlWithToken = binary:replace(PrivateCurlInsecure, <<"$TOKEN">>, AccessToken),
    ?assertEqual(?HARVESTER_MOCKED_QUERY_DATA_MAP, json_utils:decode(os:cmd(binary_to_list(PrivateCurlWithToken)))),

    % public curl query cannot be generated if the harvester is not public
    ?assert(api_test_utils:run_tests(Config, PrivateHarvesterApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, Member},
                {user, NonMember}
            ]
        },
        logic_spec = LogicSpec#logic_spec{
            args = [auth, H1, IndexId, public, data]
        }
    })),

    % test that anyone can query a public harvester
    oz_test_utils:update_harvester(Config, H1, #{<<"public">> => true}),

    PublicHarvesterApiTestSpec = PrivateHarvesterApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                nobody,
                {user, Member},
                {user, NonMember}
            ]
        },
        logic_spec = LogicSpec#logic_spec{
            args = [auth, H1, IndexId, public, data]
        }
    },
    ?assert(api_test_utils:run_tests(Config, PublicHarvesterApiTestSpec)),

    % check that generated curl works properly
    {ok, PublicCurlBinary} = oz_test_utils:call_oz(Config, harvester_logic, gen_curl_query,
        [?NOBODY, H1, IndexId, public, #{<<"method">> => <<"post">>, <<"path">> => <<"path">>}]),
    PublicCurlInsecure = binary:replace(PublicCurlBinary, <<"curl">>, <<"curl -k -s">>),
    ?assertEqual(?HARVESTER_MOCKED_QUERY_DATA_MAP, json_utils:decode(os:cmd(binary_to_list(PublicCurlInsecure)))).


list_indices_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:harvester_privileges(),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U1,
        AllPrivs -- [?HARVESTER_VIEW], [?HARVESTER_VIEW]
    ),
    oz_test_utils:harvester_add_user(Config, H1, U2),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U2,
        [?HARVESTER_VIEW], AllPrivs -- [?HARVESTER_VIEW]
    ),

    ExpectedIndices = lists:map(fun(_) ->
        {ok, IndexId} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
        IndexId
    end, lists:seq(0, 4)),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_VIEW]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/harvesters/">>, H1, <<"/indices">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"indices">> => ExpectedIndices
            }
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = list_indices,
            args = [auth, H1],
            expected_result = ?OK_LIST(ExpectedIndices)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Test that anyone can list indices from public harvester
    oz_test_utils:update_harvester(Config, H1, #{<<"public">> => true}),

    GetPublicDataApiTestSpec1 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {admin, [?OZ_HARVESTERS_VIEW]},
                {user, U1},
                {user, U2},
                {user, NonAdmin}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPublicDataApiTestSpec1)).


submit_batch_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    oz_test_utils:harvester_add_user(Config, H1, U1),

    {ok, {P1, T1}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S1} = oz_test_utils:support_space_by_provider(Config, P1, S1),
    {ok, {P2, T2}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),

    oz_test_utils:harvester_add_space(Config, H1, S1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, IndexId} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, P1, T1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {provider, P2, T2}
            ]
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = submit_batch,
            args = [auth, H1, S1, data],
            expected_result = ?OK_MAP(#{})
        },
        data_spec = #data_spec{
            required = [<<"indices">>, <<"maxSeq">>, <<"maxStreamSeq">>, <<"batch">>],
            correct_values = #{
                <<"batch">> => [?HARVESTER_BATCH(<<"fileId">>)],
                <<"indices">> => [[IndexId]],
                <<"maxStreamSeq">> => [100],
                <<"maxSeq">> => [1000]
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


submit_batch_index_stats_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    oz_test_utils:harvester_add_user(Config, H1, U1),

    {ok, {P1, _T1}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S1} = oz_test_utils:support_space_by_provider(Config, P1, S1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    oz_test_utils:harvester_add_space(Config, H1, S1),

    {ok, Index1} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
    {ok, Index2} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
    {ok, FailingIndex} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA(<<"fail">>)),

    % Created indices have empty stats
    {ok, #{S1 := #{P1 := Stats1_0}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index1),
    {ok, #{S1 := #{P1 := Stats2_0}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index2),
    {ok, #{S1 := #{P1 := Stats3_0}}} = oz_test_utils:harvester_get_index_stats(Config, H1, FailingIndex),
    assert_index_stats(Stats1_0, 0, 0, null, null),
    assert_index_stats(Stats2_0, 0, 0, null, null),
    assert_index_stats(Stats3_0, 0, 0, null, null),

    % Batch submit updates stats in given indices
    ?assertEqual({ok, ?NO_FAILED_INDICES}, oz_test_utils:harvester_submit_batch(
        Config, P1, H1, [Index1, Index2], S1,
        [
            ?HARVESTER_MOCK_BATCH_ENTRY(1, delete),
            ?HARVESTER_MOCK_BATCH_ENTRY(2, submit),
            ?HARVESTER_MOCK_BATCH_ENTRY(4, delete),
            ?HARVESTER_MOCK_BATCH_ENTRY(8, submit)
        ], 8, 10)),
    {ok, #{S1 := #{P1 := Stats1_1}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index1),
    {ok, #{S1 := #{P1 := Stats2_1}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index2),
    {ok, #{S1 := #{P1 := Stats3_1}}} = oz_test_utils:harvester_get_index_stats(Config, H1, FailingIndex),
    assert_index_stats(Stats1_1, 8, 10, null, {'not', null}),
    assert_index_stats(Stats2_1, 8, 10, null, {'not', null}),
    assert_index_stats(Stats3_1, 0, 0, null, null),


    % Failed index submit updates max seq and does not prevent other indices from update
    ?assertEqual({ok, ?FAILED_INDICES([FailingIndex], 11)}, oz_test_utils:harvester_submit_batch(
        Config, P1, H1, [Index1, Index2, FailingIndex], S1,
        [
            ?HARVESTER_MOCK_BATCH_ENTRY(11, delete),
            ?HARVESTER_MOCK_BATCH_ENTRY(12, submit),
            ?HARVESTER_MOCK_BATCH_ENTRY(14, delete),
            ?HARVESTER_MOCK_BATCH_ENTRY(18, submit)
        ], 19, 20)),
    {ok, #{S1 := #{P1 := Stats1_2}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index1),
    {ok, #{S1 := #{P1 := Stats2_2}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index2),
    {ok, #{S1 := #{P1 := Stats3_2}}} = oz_test_utils:harvester_get_index_stats(Config, H1, FailingIndex),
    assert_index_stats(Stats1_2, 19, 20, null, {'not', null}),
    assert_index_stats(Stats2_2, 19, 20, null, {'not', null}),
    assert_index_stats(Stats3_2, 0, 20, <<"error_index">>, {'not', null}),


    % Error is stored when operation in batch fails
    ?assertEqual({ok, ?FAILED_INDICES([Index1, Index2], 24)}, oz_test_utils:harvester_submit_batch(
        Config, P1, H1, [Index1, Index2], S1,
        [
            ?HARVESTER_MOCK_BATCH_ENTRY(21, delete),
            ?HARVESTER_MOCK_BATCH_ENTRY(22, submit),
            ?HARVESTER_MOCK_BATCH_ENTRY(24, fail),
            ?HARVESTER_MOCK_BATCH_ENTRY(28, submit)
        ], 28, 30)),
    {ok, #{S1 := #{P1 := Stats1_3}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index1),
    {ok, #{S1 := #{P1 := Stats2_3}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index2),
    assert_index_stats(Stats1_3, 22, 30, <<"error_seq">>, {'not', null}),
    assert_index_stats(Stats2_3, 22, 30, <<"error_seq">>, {'not', null}),


    % Correct seqs are stored when indices fail on different seqs
    ?assertEqual({ok, ?FAILED_INDICES([Index1, Index2, {FailingIndex, 31}], 34)}, oz_test_utils:harvester_submit_batch(
        Config, P1, H1, [Index1, Index2, FailingIndex], S1,
        [
            ?HARVESTER_MOCK_BATCH_ENTRY(31, delete),
            ?HARVESTER_MOCK_BATCH_ENTRY(32, submit),
            ?HARVESTER_MOCK_BATCH_ENTRY(34, fail),
            ?HARVESTER_MOCK_BATCH_ENTRY(38, submit)
        ], 38, 40)),
    {ok, #{S1 := #{P1 := Stats1_4}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index1),
    {ok, #{S1 := #{P1 := Stats2_4}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index2),
    {ok, #{S1 := #{P1 := Stats3_4}}} = oz_test_utils:harvester_get_index_stats(Config, H1, FailingIndex),
    assert_index_stats(Stats1_4, 32, 40, <<"error_seq">>, {'not', null}),
    assert_index_stats(Stats2_4, 32, 40, <<"error_seq">>, {'not', null}),
    assert_index_stats(Stats3_4, 0, 40, <<"error_index">>, {'not', null}),


    % Not existing index is ignored
    ?assertEqual({ok, ?NO_FAILED_INDICES}, oz_test_utils:harvester_submit_batch(
        Config, P1, H1, [<<"not_exiting_index">>], S1,
        [
            ?HARVESTER_MOCK_BATCH_ENTRY(31, submit)
        ], 38, 40)),


    % Empty batch updates stats
    ?assertEqual({ok, ?NO_FAILED_INDICES}, oz_test_utils:harvester_submit_batch(
        Config, P1, H1, [Index1, Index2, FailingIndex], S1,
        [], 48, 50)),
    {ok, #{S1 := #{P1 := Stats1_5}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index1),
    {ok, #{S1 := #{P1 := Stats2_5}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index2),
    {ok, #{S1 := #{P1 := Stats3_5}}} = oz_test_utils:harvester_get_index_stats(Config, H1, FailingIndex),
    assert_index_stats(Stats1_5, 48, 50, null, {'not', null}),
    assert_index_stats(Stats2_5, 48, 50, null, {'not', null}),
    assert_index_stats(Stats3_5, 48, 50, null, {'not', null}).


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
    ozt_mocks:mock_harvesting_backends(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unmock_harvesting_backends(),
    ozt_mocks:unfreeze_time().

%%%===================================================================
%%% Internal functions
%%%===================================================================

mock_assert_num_calls_sum(OzNodes, Module, Fun, Arity, Value) ->
    Sum = lists:foldl(fun(Node, Acc) ->
        Num = rpc:call(Node, meck, num_calls, [Module, Fun, Arity]),
        Acc + Num
    end, 0, OzNodes),
    ?assertEqual(Value, Sum),
    lists:foreach(fun(Node) ->
        rpc:call(Node, meck, reset, [Module])
    end, OzNodes).

assert_index_stats(Stats, CurrentSeq, MaxSeq, Error, LastUpdate) ->
    ?assertEqual(CurrentSeq, maps:get(<<"currentSeq">>, Stats)),
    ?assertEqual(MaxSeq, maps:get(<<"maxSeq">>, Stats)),
    ?assertEqual(Error, maps:get(<<"error">>, Stats)),
    case LastUpdate of
        {'not', Value} ->
            ?assertNotEqual(Value, maps:get(<<"lastUpdate">>, Stats));
        _ ->
            ?assertEqual(LastUpdate, maps:get(<<"lastUpdate">>, Stats))
    end.
