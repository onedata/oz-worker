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

-include("rest.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/api_errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_testcase/2, end_per_testcase/2,
    init_per_suite/1, end_per_suite/1
]).
-export([
    create_test/1,
    list_test/1,
    get_test/1,
    get_config_test/1,
    update_test/1,
    update_config_test/1,
    delete_test/1,
    
    submit_entry_test/1,
    delete_entry_test/1,
    query_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        get_config_test,
        update_test,
        update_config_test,
        delete_test,
        
        submit_entry_test,
        delete_entry_test,
        query_test
    ]).


-define(TEST_DATA, <<"test_data">>).

%%%===================================================================
%%% Test functions
%%%===================================================================


create_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    [Node | _] = ?config(oz_worker_nodes, Config),
    VerifyFun = fun(HarvesterId) ->
        {ok, Harvester} = oz_test_utils:get_harvester(Config, HarvesterId),
        ?assertEqual(?CORRECT_NAME, Harvester#od_harvester.name),
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
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/harvesters/">>]),
                [HarvesterId] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(HarvesterId)
            end
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = create,
            args = [client, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        data_spec = #data_spec{
            required = [<<"name">>, <<"endpoint">>, <<"plugin">>, <<"config">>, 
                <<"entryTypeField">>, <<"acceptedEntryTypes">>],
            optional = [<<"defaultEntryType">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"endpoint">> => [?HARVESTER_ENDPOINT],
                <<"plugin">> => [?HARVESTER_PLUGIN_BINARY],
                <<"config">> => [?HARVESTER_CONFIG],
                <<"entryTypeField">> => [?HARVESTER_ENTRY_TYPE_FIELD],
                <<"acceptedEntryTypes">> => [?HARVESTER_ACCEPTED_ENTRY_TYPES],
                <<"defaultEntryType">> => [<<"deafult">>]
            },
            bad_values = 
                [{<<"plugin">>, <<"not_existing_plugin">>, 
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"plugin">>, 
                        rpc:call(Node, onezone_plugins, get_plugins, [harvester_plugin]))} 
                    | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_test(Config) ->
    % Make sure that harvesters created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_DATA),
    {ok, H2} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_DATA),
    {ok, H3} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_DATA),
    {ok, H4} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_DATA),
    {ok, H5} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_DATA),
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
            args = [client],
            expected_result = ?OK_LIST(ExpHarvesters)
        }
        % TODO gs
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


get_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    AllPrivs = privileges:harvester_privileges(),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_DATA),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U1,
        AllPrivs -- [?HARVESTER_VIEW], [?HARVESTER_VIEW]
    ),
    oz_test_utils:harvester_add_user(Config, H1, U2),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U2,
        [?HARVESTER_VIEW], AllPrivs -- [?HARVESTER_VIEW]
    ),

    {ok, S} = oz_test_utils:create_space(
        Config, ?USER(U1), ?SPACE_NAME1
    ),

    oz_test_utils:harvester_add_space(Config, H1, S),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {admin, [?OZ_HARVESTERS_VIEW]},
                {user, NonAdmin},
                {user, U1}
            ]
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get,
            args = [client, H1],
            expected_result = ?OK_TERM(
                fun(#od_harvester{
                    name = Name, users = Users, groups = #{},
                    spaces = Spaces,
                    eff_users = EffUsers, eff_groups = #{},
                    bottom_up_dirty = false
                }) ->
                    ?assertEqual(?HARVESTER_NAME1, Name),
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
            expected_body = #{
                <<"harvesterId">> => H1,
                <<"name">> => ?HARVESTER_NAME1
            }
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_protected_data,
            args = [client, H1],
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?HARVESTER_NAME1,
                <<"entryTypeField">> => ?HARVESTER_ENTRY_TYPE_FIELD,
                <<"acceptedEntryTypes">> => ?HARVESTER_ACCEPTED_ENTRY_TYPES,
                <<"defaultEntryType">> => undefined
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)).


get_config_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    AllPrivs = privileges:harvester_privileges(),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_DATA),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U1,
        AllPrivs -- [?HARVESTER_VIEW], [?HARVESTER_VIEW]
    ),
    oz_test_utils:harvester_add_user(Config, H1, U2),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U2,
        [?HARVESTER_VIEW], AllPrivs -- [?HARVESTER_VIEW]
    ),

    {ok, S} = oz_test_utils:create_space(
        Config, ?USER(U1), ?SPACE_NAME1
    ),

    oz_test_utils:harvester_add_space(Config, H1, S),

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
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_config,
            args = [client, H1],
            expected_result = ?OK_TERM(
                fun(Conf) -> ?assertEqual(?HARVESTER_CONFIG, Conf) end
            )
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_DATA(?CORRECT_NAME)),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U1, [], [
            ?HARVESTER_UPDATE
        ]),
        oz_test_utils:harvester_add_user(Config, H1, U2),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U2, [
            ?HARVESTER_UPDATE
        ], []),
        #{harvesterId => H1}
    end,
    Endpoint = <<"172.17.0.2:9200">>,
    Plugin = ?HARVESTER_MOCK_PLUGIN_BINARY,
    AcceptedEntryTypes = [<<"type1">>],
    
    ExpValueFun = fun(ShouldSucceed, Key, Data, Default) ->
        case ShouldSucceed of
            false -> Default;
            true -> maps:get(Key, Data, Default)
        end
    end,
    
    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId} = _Env, Data) ->
        {ok, Harvester} = oz_test_utils:get_harvester(Config, HarvesterId),
        
        ExpName = ExpValueFun(ShouldSucceed, <<"name">>, Data, ?CORRECT_NAME),
        ExpEndpoint = ExpValueFun(ShouldSucceed, <<"endpoint">>, Data, ?HARVESTER_ENDPOINT),
        ExpPlugin = ExpValueFun(ShouldSucceed, <<"plugin">>, Data, ?HARVESTER_PLUGIN_BINARY),
        ExpPublic = ExpValueFun(ShouldSucceed, <<"public">>, Data, false),
        ExpEntryTypeField = ExpValueFun(ShouldSucceed, <<"entryTypeField">>, Data, ?HARVESTER_ENTRY_TYPE_FIELD),
        ExpAcceptedEntryTypes = ExpValueFun(ShouldSucceed, <<"acceptedEntryTypes">>, Data, ?HARVESTER_ACCEPTED_ENTRY_TYPES),
        ExpDefaultEntryType = ExpValueFun(ShouldSucceed, <<"defaultEntryType">>, Data, undefined),
        
        ?assertEqual(ExpName, Harvester#od_harvester.name),
        ?assertEqual(ExpEndpoint, Harvester#od_harvester.endpoint),
        ?assertEqual(binary_to_atom(ExpPlugin, utf8), Harvester#od_harvester.plugin),
        ?assertEqual(ExpPublic, Harvester#od_harvester.public),
        ?assertEqual(ExpEntryTypeField, Harvester#od_harvester.entry_type_field),
        ?assertEqual(ExpAcceptedEntryTypes, Harvester#od_harvester.accepted_entry_types),
        ?assertEqual(ExpDefaultEntryType, Harvester#od_harvester.default_entry_type)
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
            args = [client, harvesterId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_harvester, id = harvesterId, aspect = instance},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            at_least_one = [<<"name">>, <<"endpoint">>, <<"plugin">>, 
                <<"public">>, <<"entryTypeField">>, <<"acceptedEntryTypes">>, <<"defaultEntryType">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"endpoint">> => [Endpoint],
                <<"plugin">> => [Plugin],
                <<"public">> => [true, false],
                <<"entryTypeField">> => [<<"type1">>],
                <<"acceptedEntryTypes">> => [AcceptedEntryTypes],
                <<"defaultEntryType">> => [<<"default">>]
            },
            bad_values =
            [{<<"plugin">>, <<"not_existing_plugin">>,
                ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"plugin">>,
                    rpc:call(Node, onezone_plugins, get_plugins, [harvester_plugin]))},
             {<<"public">>, not_boolean, ?ERROR_BAD_VALUE_BOOLEAN(<<"public">>)}
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


update_config_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_DATA(?CORRECT_NAME)),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U1, [], [
            ?HARVESTER_UPDATE
        ]),
        oz_test_utils:harvester_add_user(Config, H1, U2),
        oz_test_utils:harvester_set_user_privileges(Config, H1, U2, [
            ?HARVESTER_UPDATE
        ], []),
        #{harvesterId => H1}
    end,
    Conf = ?HARVESTER_CONFIG#{<<"x">> => <<"y">>},

    ExpValueFun = fun(ShouldSucceed, Key, Data, Default) ->
        case ShouldSucceed of
            false -> Default;
            true -> maps:get(Key, Data, Default)
        end
    end,

    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId} = _Env, Data) ->
        {ok, Harvester} = oz_test_utils:get_harvester(Config, HarvesterId),

        ExpConfig = ExpValueFun(ShouldSucceed, <<"config">>, Data, ?HARVESTER_CONFIG),

        ?assertEqual(ExpConfig, Harvester#od_harvester.config)
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
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = update_config,
            args = [client, harvesterId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_harvester, id = harvesterId, aspect = config},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [<<"config">>],
            correct_values = #{
                <<"config">> => [Conf]
            },
            bad_values = [{<<"config">>, <<"bad_config">>, ?ERROR_BAD_VALUE_JSON(<<"config">>)}]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_DATA),
        oz_test_utils:harvester_set_user_privileges(
            Config, H1, U1, [], [?HARVESTER_DELETE]
        ),
        oz_test_utils:harvester_add_user(Config, H1, U2),
        oz_test_utils:harvester_set_user_privileges(
            Config, H1, U2, [?HARVESTER_DELETE], []
        ),
        #{harvesterId => H1}
    end,
    DeleteEntityFun = fun(#{harvesterId := HarvesterId} = _Env) ->
        oz_test_utils:delete_harvester(Config, HarvesterId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId} = _Env, _) ->
        {ok, Harvesters} = oz_test_utils:list_harvesters(Config),
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
            args = [client, harvesterId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_harvester, id = harvesterId, aspect = instance},
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


submit_entry_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, 
        ?HARVESTER_DATA(?HARVESTER_NAME1, ?HARVESTER_MOCK_PLUGIN_BINARY)),
    oz_test_utils:harvester_add_user(Config, H1, U1),
    
    {ok, {P1, M1}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S1} = oz_test_utils:support_space(
        Config, P1, S1, oz_test_utils:minimum_support_size(Config)
    ),
    {ok, {P2, M2}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    
    oz_test_utils:harvester_add_space(Config, H1, S1),
    
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    {ok, FileId} = file_id:guid_to_objectid(file_id:pack_guid(<<"1234">>, S1)),
    
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1,M1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {provider, P2,M2}
            ]
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = submit_entry,
            args = [client, H1, FileId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_harvester, id = H1, aspect = {entry, FileId}},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [<<"payload">>],
            correct_values = #{<<"payload">> => [<<"example_payload">>]}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


delete_entry_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, 
        ?HARVESTER_DATA(?HARVESTER_NAME1, ?HARVESTER_MOCK_PLUGIN_BINARY)),
    oz_test_utils:harvester_add_user(Config, H1, U1),

    {ok, {P1, M1}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S1} = oz_test_utils:support_space(
        Config, P1, S1, oz_test_utils:minimum_support_size(Config)
    ),
    {ok, {P2, M2}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),

    oz_test_utils:harvester_add_space(Config, H1, S1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    {ok, FileId} = file_id:guid_to_objectid(file_id:pack_guid(<<"1234">>, S1)),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, P1,M1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {provider, P2,M2}
            ]
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = delete_entry,
            args = [client, H1, FileId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_harvester, id = H1, aspect = {entry, FileId}},
            expected_result = ?OK
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


query_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT,  
        ?HARVESTER_DATA(?HARVESTER_NAME1, ?HARVESTER_MOCK_PLUGIN_BINARY)),
    oz_test_utils:harvester_add_user(Config, H1, U1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2}
            ]
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = query,
            args = [client, H1, data],
            expected_result = ?OK_BINARY(?TEST_DATA)
        },
        data_spec = #data_spec{
            required = [<<"path">>, <<"method">>],
            optional = [<<"body">>],
            correct_values = #{
                <<"path">> => [<<"example_request">>],
                <<"method">> => [<<"get">>, <<"post">>],
                <<"body">> => [<<"example_data">>]
            },
            bad_values = [
                {<<"method">>, <<"bad_method">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"method">>, [post, get])},
                {<<"path">>, <<>>, ?ERROR_BAD_VALUE_EMPTY(<<"path">>)},
                {<<"body">>, <<>>, ?ERROR_BAD_VALUE_EMPTY(<<"body">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    
    oz_test_utils:update_harvester(Config, H1, #{<<"public">> => true}),
    
    % Anyone can query public harvester
    PublicHarvesterApiTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, PublicHarvesterApiTestSpec)).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(submit_entry_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, ?HARVESTER_MOCK_PLUGIN, [non_strict]),
    test_utils:mock_expect(Nodes, ?HARVESTER_MOCK_PLUGIN, submit_entry, fun(_,_,_,_) -> ok end),
    init_per_testcase(plugin_tests, Config);
init_per_testcase(delete_entry_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, ?HARVESTER_MOCK_PLUGIN, [non_strict]),
    test_utils:mock_expect(Nodes, ?HARVESTER_MOCK_PLUGIN, delete_entry, fun(_,_,_) -> ok end),
    init_per_testcase(plugin_tests, Config);
init_per_testcase(query_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, ?HARVESTER_MOCK_PLUGIN, [no_history, non_strict]),
    test_utils:mock_expect(Nodes, ?HARVESTER_MOCK_PLUGIN, query, fun(_,_,_) -> {ok, ?TEST_DATA} end),
    test_utils:mock_expect(Nodes, ?HARVESTER_MOCK_PLUGIN, query_validator, fun() -> 
        erlang:apply(binary_to_atom(?HARVESTER_PLUGIN_BINARY, utf8), query_validator, []) end),
    init_per_testcase(plugin_tests, Config);
init_per_testcase(plugin_tests, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, onezone_plugins),
    test_utils:mock_expect(Nodes, ?HARVESTER_MOCK_PLUGIN, type, fun() -> harvester_plugin end),
    test_utils:mock_expect(Nodes, onezone_plugins, get_plugins, fun(_) -> [?HARVESTER_MOCK_PLUGIN] end),
    Config;
init_per_testcase(update_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, onezone_plugins),
    test_utils:mock_new(Nodes, ?HARVESTER_MOCK_PLUGIN, [no_history, non_strict]),
    test_utils:mock_expect(Nodes, ?HARVESTER_MOCK_PLUGIN, type, fun() -> harvester_plugin end),
    test_utils:mock_expect(Nodes, onezone_plugins, get_plugins, 
        fun(_) -> [?HARVESTER_MOCK_PLUGIN, binary_to_atom(?HARVESTER_PLUGIN_BINARY, utf8)] end),
    Config;
init_per_testcase(_, Config) ->
    Config.


end_per_testcase(Case, Config) when
    Case =:= submit_entry_test;
    Case =:= delete_entry_test;
    Case =:= update_test;
    Case =:= query_test ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, ?HARVESTER_MOCK_PLUGIN),
    test_utils:mock_unload(Nodes, onezone_plugins);
end_per_testcase(_, _Config) ->
    ok.

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

