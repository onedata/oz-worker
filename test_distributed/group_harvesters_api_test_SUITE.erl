%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning group harvesters API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_harvesters_api_test_SUITE).
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
-include_lib("ctool/include/errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_testcase/2, end_per_testcase/2,
    init_per_suite/1, end_per_suite/1
]).
-export([
    list_harvesters_test/1,
    get_harvester_details_test/1,
    create_harvester_test/1,
    join_harvester_test/1,
    leave_harvester_test/1,
    list_eff_harvesters_test/1,
    get_eff_harvester_details_test/1
]).

all() ->
    ?ALL([
        list_harvesters_test,
        get_harvester_details_test,
        create_harvester_test,
        join_harvester_test,
        leave_harvester_test,
        list_eff_harvesters_test,
        get_eff_harvester_details_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_harvesters_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, H1} = oz_test_utils:group_create_harvester(Config, G1, ?HARVESTER_CREATE_DATA),
    {ok, H2} = oz_test_utils:group_create_harvester(Config, G1, ?HARVESTER_CREATE_DATA),
    {ok, H3} = oz_test_utils:group_create_harvester(Config, G1, ?HARVESTER_CREATE_DATA),
    {ok, H4} = oz_test_utils:group_create_harvester(Config, G1, ?HARVESTER_CREATE_DATA),
    ExpHarvesters = [H1, H2, H3, H4],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/harvesters">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"harvesters">> => ExpHarvesters}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_harvesters,
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpHarvesters)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_harvester_details_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, H1} = oz_test_utils:group_create_harvester(Config, G1, ?HARVESTER_CREATE_DATA),

    ExpData = ?HARVESTER_PROTECTED_DATA(?HARVESTER_NAME1),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_HARVESTERS_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/harvesters/">>, H1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpData#{<<"harvesterId">> => H1}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_harvester,
            args = [auth, G1, H1],
            expected_result = ?OK_MAP_CONTAINS(ExpData#{<<"plugin">> => ?HARVESTER_MOCK_PLUGIN})
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_harvester_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_ADD_HARVESTER privilege
    %   U1 gets all remaining privileges
    [Node | _] = ?config(oz_worker_nodes, Config),
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_HARVESTER
    ),
    oz_test_utils:group_set_oz_privileges(Config, G1, [?OZ_HARVESTERS_CREATE], []),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:harvester_privileges(),

    VerifyFun = fun(HarvesterId) ->
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        {ok, Harvester} = oz_test_utils:get_harvester(Config, HarvesterId),
        ?assertEqual(?CORRECT_NAME, Harvester#od_harvester.name),

        ?assertEqual(#{}, Harvester#od_harvester.users),
        ?assertEqual(
            #{
                U1 => {AllPrivs, [{od_group, G1}]},
                U2 => {AllPrivs, [{od_group, G1}]}
            },
            Harvester#od_harvester.eff_users
        ),
        ?assertEqual(#{G1 => AllPrivs}, Harvester#od_harvester.groups),
        ?assertEqual(#{G1 => {AllPrivs, [{od_harvester, <<"self">>}]}}, Harvester#od_harvester.eff_groups),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/harvesters">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/groups/">>, G1, <<"/harvesters/">>]),
                [HarvesterId] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(HarvesterId)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_harvester,
            args = [auth, G1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        data_spec = #data_spec{
            required = [<<"name">>, <<"endpoint">>, <<"plugin">>],
            optional = [<<"guiPluginConfig">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


join_harvester_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_ADD_HARVESTER privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_HARVESTER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, Creator} = oz_test_utils:create_user(Config),
        oz_test_utils:user_set_oz_privileges(Config, Creator, [?OZ_HARVESTERS_CREATE], []),
        {ok, HarvesterId} = oz_test_utils:create_harvester(Config, ?USER(Creator), ?HARVESTER_CREATE_DATA),
        {ok, Token} = oz_test_utils:harvester_invite_group_token(
            Config, ?USER(Creator), HarvesterId
        ),
        {ok, Serialized} = tokens:serialize(Token),
        #{
            harvesterId => HarvesterId,
            token => Serialized,
            tokenNonce => Token#token.id
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId, tokenNonce := TokenId} = _Env, _) ->
        {ok, Harvesters} = oz_test_utils:group_get_harvesters(Config, G1),
        ?assertEqual(lists:member(HarvesterId, Harvesters), ShouldSucceed),
        case ShouldSucceed of
            true ->
                oz_test_utils:assert_invite_token_usage_limit_reached(Config, true, TokenId);
            false -> ok
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/harvesters/join">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{harvesterId := HarvesterId} = _Env, _) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config,
                        [<<"/groups/">>, G1, <<"/harvesters/">>, HarvesterId]
                    ),
                    ?assertMatch(ExpLocation, Location),
                    true
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_harvester,
            args = [auth, G1, data],
            expected_result = ?OK_ENV(fun(#{harvesterId := HarvesterId} = _Env, _) ->
                ?OK_BINARY(HarvesterId)
            end)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [fun(#{token := Token} = _Env) ->
                    Token
            end]
            },
            bad_values = [
                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"token">>)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, <<"123qwe">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check that token is not consumed upon failed operation
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, Harvester} = oz_test_utils:create_harvester(Config, ?USER(U1),
        ?HARVESTER_CREATE_DATA
    ),
    {ok, Token2} = oz_test_utils:harvester_invite_group_token(
        Config, ?USER(U1), Harvester
    ),
    {ok, Serialized2} = tokens:serialize(Token2),
    oz_test_utils:harvester_add_group(Config, Harvester, G1),

    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/harvesters/join">>],
            expected_code = ?HTTP_409_CONFLICT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_harvester,
            args = [auth, G1, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_group, G1, od_harvester, Harvester))
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [Serialized2]}
        }
    },
    VerifyEndFun1 = fun(_ShouldSucceed,_Env,_) ->
        oz_test_utils:assert_invite_token_usage_limit_reached(Config, false, Token2#token.id)
    end,
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec1, undefined, undefined, VerifyEndFun1
    )).


leave_harvester_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_REMOVE_HARVESTER privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_REMOVE_HARVESTER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:group_create_harvester(
            Config, G1, ?HARVESTER_CREATE_DATA
        ),
        #{harvesterId => H1}
                  end,
    DeleteEntityFun = fun(#{harvesterId := HarvesterId} = _Env) ->
        oz_test_utils:harvester_remove_group(Config, HarvesterId, G1)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId} = _Env, _) ->
        {ok, Harvesters} = oz_test_utils:group_get_harvesters(Config, G1),
        ?assertEqual(lists:member(HarvesterId, Harvesters), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_HARVESTERS_REMOVE_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/harvesters/">>, harvesterId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_harvester,
            args = [auth, G1, harvesterId],
            expected_result = ?OK_RES
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_eff_harvesters_test(Config) ->
    {
        [{H1, _}, {H2, _}, {H3, _}, {H4, _}, {H5, _}],
        [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_harvesters_env(Config),

    ExpHarvesters = [H1, H2, H3, H4, H5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/effective_harvesters">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"harvesters">> => ExpHarvesters}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_harvesters,
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpHarvesters)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_harvester function
    lists:foreach(
        fun(HarvestersId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_harvester, [G1, HarvestersId])
            )
        end, ExpHarvesters
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_harvester,
        [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_harvester_details_test(Config) ->
    {
        EffHarvestersList, [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_harvesters_env(Config),

    lists:foreach(
        fun({HarvesterId, HarvesterDetails}) ->
            ApiTestSpec = #api_test_spec{
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
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/groups/">>, G1, <<"/effective_harvesters/">>, HarvesterId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = HarvesterDetails#{<<"harvesterId">> => HarvesterId}
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_harvester,
                    args = [auth, G1, HarvesterId],
                    expected_result = ?OK_MAP_CONTAINS(HarvesterDetails#{<<"plugin">> => ?HARVESTER_MOCK_PLUGIN})
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffHarvestersList
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

