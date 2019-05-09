%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user harvesters API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(user_harvesters_api_test_SUITE).
-author("Michal Stanisz").

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
    init_per_testcase/2, end_per_testcase/2,
    init_per_suite/1, end_per_suite/1
]).
-export([
    list_harvesters_test/1,
    create_harvester_test/1,
    join_harvester_test/1,
    get_harvester_test/1,
    leave_harvester_test/1,

    list_eff_harvesters_test/1,
    get_eff_harvester_test/1
]).

all() ->
    ?ALL([
        list_harvesters_test,
        create_harvester_test,
        join_harvester_test,
        get_harvester_test,
        leave_harvester_test,

        list_eff_harvesters_test,
        get_eff_harvester_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_harvesters_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    ExpHarvesters = lists:map(
        fun(_) ->
            {ok, HarvesterId} = oz_test_utils:create_harvester(
                Config, ?USER(U1), ?HARVESTER_CREATE_DATA
            ),
            {ok, U2} = oz_test_utils:harvester_add_user(Config, HarvesterId, U2),
            HarvesterId
        end, lists:seq(1, 5)
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
            path = <<"/user/harvesters">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"harvesters">> => ExpHarvesters}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
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
            function = get_harvesters,
            args = [client, U1],
            expected_result = ?OK_LIST(ExpHarvesters)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


create_harvester_test(Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U2, [?OZ_HARVESTERS_CREATE], []),

    ExpName = ?CORRECT_NAME,
    AllPrivs = privileges:harvester_privileges(),

    VerifyFun = fun(HarvesterId) ->
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        {ok, Harvester} = oz_test_utils:get_harvester(Config, HarvesterId),
        ?assertEqual(ExpName, Harvester#od_harvester.name),

        [User] = ?assertMatch([_], maps:keys(Harvester#od_harvester.users)),
        ?assertEqual(#{User => AllPrivs}, Harvester#od_harvester.users),
        ?assertEqual(#{User => {AllPrivs, [{od_harvester, <<"self">>}]}}, Harvester#od_harvester.eff_users),
        ?assertEqual(#{}, Harvester#od_harvester.groups),
        ?assertEqual(#{}, Harvester#od_harvester.eff_groups),

        true
                end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2},
                {admin, [?OZ_USERS_ADD_RELATIONSHIPS, ?OZ_HARVESTERS_CREATE]}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/harvesters">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/user/harvesters/">>]),
                [HarvesterId] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(HarvesterId)
            end
        },
        data_spec = #data_spec{
            required = [<<"name">>, <<"endpoint">>, <<"plugin">>],
            optional = [<<"guiPluginConfig">>],
            correct_values = #{
                <<"name">> => [ExpName],
                <<"endpoint">> => [?HARVESTER_ENDPOINT1],
                <<"plugin">> => [?HARVESTER_MOCK_PLUGIN_BINARY],
                <<"guiPluginConfig">> => [?HARVESTER_GUI_PLUGIN_CONFIG]
            },
            bad_values =
            [{<<"plugin">>, <<"not_existing_plugin">>,
                ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"plugin">>,
                    rpc:call(Node, onezone_plugins, get_plugins, [harvester_plugin]))}
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [{user, U2}]
        }, 
            
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_harvester,
            args = [client, U1, data],
            expected_result = ?OK_TERM(VerifyFun)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


join_harvester_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, HarvesterId} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
        {ok, Macaroon} = oz_test_utils:harvester_invite_user_token(
            Config, ?ROOT, HarvesterId
        ),
        {ok, Token} = onedata_macaroons:serialize(Macaroon),
        #{
            harvesterId => HarvesterId,
            token => Token,
            macaroonId => macaroon:identifier(Macaroon)
        }
        end,
    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId, macaroonId := MacaroonId} = _Env, _) ->
        {ok, Harvesters} = oz_test_utils:user_get_harvesters(Config, U1),
        ?assertEqual(lists:member(HarvesterId, Harvesters), ShouldSucceed),
        case ShouldSucceed of
            true ->
                oz_test_utils:assert_token_not_exists(Config, MacaroonId);
            false -> ok
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/harvesters/join">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{harvesterId := HarvesterId} = _Env, _) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config, [<<"/user/harvesters/">>, HarvesterId]),
                    ?assertEqual(ExpLocation, Location),
                    true
                end
            end)
        },
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [fun(#{token := Token} = _Env) -> Token end]
            },
            bad_values = [
                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"token">>)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, <<"123qwe">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_USERS_ADD_RELATIONSHIPS]},
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [{user, U2}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = join_harvester,
            args = [client, U1, data],
            expected_result = ?OK_ENV(fun(#{harvesterId := HarvesterId} = _Env, _) ->
                ?OK_BINARY(HarvesterId)
            end)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check that token is not consumed upon failed operation
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, Harvester} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    {ok, Macaroon} = oz_test_utils:harvester_invite_user_token(
        Config, ?ROOT, Harvester
    ),
    {ok, Token} = onedata_macaroons:serialize(Macaroon),

    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/harvesters/join">>,
            expected_code = ?HTTP_400_BAD_REQUEST
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = join_harvester,
            args = [client, U1, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_user, U1, od_harvester, Harvester))
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [Token]}
        }
    },
    VerifyEndFun1 = fun(_ShouldSucceed,_Env,_) ->
        oz_test_utils:assert_token_exists(Config, macaroon:identifier(Macaroon))
    end,
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec1, undefined, undefined, VerifyEndFun1
    )).


get_harvester_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    {ok, U1} = oz_test_utils:harvester_add_user(Config, H1, U1),
    {ok, U2} = oz_test_utils:harvester_add_user(Config, H1, U2),


    ExpData = ?HARVESTER_PROTECTED_DATA(?HARVESTER_NAME1),
    
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/harvesters/">>, H1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpData#{<<"harvesterId">> => H1}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_VIEW]},
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
            function = get_harvester,
            args = [client, U1, H1],
            expected_result = ?OK_MAP_CONTAINS(ExpData#{<<"plugin">> => ?HARVESTER_MOCK_PLUGIN})
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


leave_harvester_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
        {ok, U1} = oz_test_utils:harvester_add_user(Config, H1, U1),
        {ok, U2} = oz_test_utils:harvester_add_user(Config, H1, U2),
        #{harvesterId => H1}
    end,
    DeleteEntityFun = fun(#{harvesterId := HarvesterId} = _Env) ->
        oz_test_utils:user_leave_harvester(Config, U1, HarvesterId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId} = _Env, _) ->
        {ok, Harvesters} = oz_test_utils:user_get_harvesters(Config, U1),
        ?assertEqual(lists:member(HarvesterId, Harvesters), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/harvesters/">>, harvesterId],
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_USERS_REMOVE_RELATIONSHIPS, ?OZ_HARVESTERS_REMOVE_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = leave_harvester,
            args = [client, U1, harvesterId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])).


list_eff_harvesters_test(Config) ->
    {
        [{H1, _}, {H2, _}, {H3, _}, {H4, _}, {H5, _}],
        _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_harvesters_env(Config),

    oz_test_utils:user_set_oz_privileges(Config, U2, [?OZ_HARVESTERS_CREATE], []),
    {ok, H6} = oz_test_utils:create_harvester(Config, ?USER(U2), ?HARVESTER_CREATE_DATA),
    {ok, U1} = oz_test_utils:harvester_add_user(Config, H6, U1),

    ExpHarvesters = [H1, H2, H3, H4, H5, H6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/effective_harvesters">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"harvesters">> => ExpHarvesters}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_USERS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_harvesters,
            args = [client, U2],
            expected_result = ?OK_LIST(ExpHarvesters)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % check also user_logic:has_eff_harvester function
    lists:foreach(
        fun(HarvestersId) ->
            ?assert(oz_test_utils:call_oz(
                Config, user_logic, has_eff_harvester, [U2, HarvestersId])
            )
        end, ExpHarvesters
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, user_logic, has_eff_harvester, [U2, <<"asdiucyaie827346w">>])
    ).


get_eff_harvester_test(Config) ->
    {
        EffHarvestersList, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_harvesters_env(Config),

    oz_test_utils:user_set_oz_privileges(Config, U2, [?OZ_HARVESTERS_CREATE], []),
    {ok, H6} = oz_test_utils:create_harvester(Config, ?USER(U2), ?HARVESTER_CREATE_DATA),
    H6Details = ?HARVESTER_PROTECTED_DATA(?HARVESTER_NAME1),
    {ok, U1} = oz_test_utils:harvester_add_user(Config, H6, U1),

    NewEffHarvestersList = [{H6, H6Details} | EffHarvestersList],
    lists:foreach(
        fun({HarvesterId, HarvesterDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        {user, U1},
                        {user, U2}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/user/effective_harvesters/">>, HarvesterId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = HarvesterDetails#{<<"harvesterId">> => HarvesterId}
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

            % Check that regular client can't make request
            % on behalf of other client
            ApiTestSpec2 = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {admin, [?OZ_HARVESTERS_VIEW]},
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
                    function = get_eff_harvester,
                    args = [client, U1, HarvesterId],
                    expected_result = ?OK_MAP_CONTAINS(HarvesterDetails#{<<"plugin">> => ?HARVESTER_MOCK_PLUGIN})
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, NewEffHarvestersList
    ).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(_, Config) ->
    oz_test_utils:mock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN).

end_per_testcase(_, Config) ->
    oz_test_utils:unmock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN).

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

