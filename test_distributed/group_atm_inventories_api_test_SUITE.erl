%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning group atm_inventories API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_atm_inventories_api_test_SUITE).
-author("Lukasz Opiola").

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
    list_atm_inventories_test/1,
    get_atm_inventory_details_test/1,
    create_atm_inventory_test/1,
    join_atm_inventory_test/1,
    leave_atm_inventory_test/1,
    list_eff_atm_inventories_test/1,
    get_eff_atm_inventory_details_test/1
]).

all() ->
    ?ALL([
        list_atm_inventories_test,
        get_atm_inventory_details_test,
        create_atm_inventory_test,
        join_atm_inventory_test,
        leave_atm_inventory_test,
        list_eff_atm_inventories_test,
        get_eff_atm_inventory_details_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

list_atm_inventories_test(Config) ->
    {SubjectGroup, UserWithoutView, UserWithView} = api_test_scenarios:create_basic_group_env(Config, ?GROUP_VIEW),
    NonAdmin = ozt_users:create(),

    ExpAtmInventories = [
        ozt_groups:create_atm_inventory_for(SubjectGroup),
        ozt_groups:create_atm_inventory_for(SubjectGroup),
        ozt_groups:create_atm_inventory_for(SubjectGroup),
        ozt_groups:create_atm_inventory_for(SubjectGroup)
    ],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, UserWithView},
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutView}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, SubjectGroup, <<"/atm_inventories">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_inventories">> => ExpAtmInventories}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_atm_inventories,
            args = [auth, SubjectGroup],
            expected_result = ?OK_LIST(ExpAtmInventories)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_atm_inventory_details_test(Config) ->
    {SubjectGroup, UserWithoutView, UserWithView} = api_test_scenarios:create_basic_group_env(Config, ?GROUP_VIEW),
    NonAdmin = ozt_users:create(),

    AtmInventoryData = #{<<"name">> => ?CORRECT_NAME},
    AtmInventory = ozt_groups:create_atm_inventory_for(SubjectGroup, AtmInventoryData),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, UserWithView},
                {admin, [?OZ_ATM_INVENTORIES_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutView}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, SubjectGroup, <<"/atm_inventories/">>, AtmInventory],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:protected_atm_inventory(rest, AtmInventory, AtmInventoryData, ?SUB(nobody))
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_atm_inventory,
            args = [auth, SubjectGroup, AtmInventory],
            expected_result = api_test_expect:protected_atm_inventory(logic, AtmInventory, AtmInventoryData, ?SUB(nobody))
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_atm_inventory_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_ADD_ATM_INVENTORY privilege
    %   U1 gets all remaining privileges
    {SubjectGroup, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_ATM_INVENTORY
    ),
    NonAdmin = ozt_users:create(),

    ExpName = ?CORRECT_NAME,
    AllPrivs = privileges:atm_inventory_privileges(),

    VerifyFun = fun(AtmInventory) ->
        ozt:reconcile_entity_graph(),
        AtmInventoryRecord = ozt_atm_inventories:get(AtmInventory),
        ?assertEqual(ExpName, AtmInventoryRecord#od_atm_inventory.name),

        ?assertEqual(#{}, AtmInventoryRecord#od_atm_inventory.users),
        ?assertEqual(
            #{
                UserWithoutPrivilege => {AllPrivs, [{od_group, SubjectGroup}]},
                UserWithPrivilege => {AllPrivs, [{od_group, SubjectGroup}]}
            },
            AtmInventoryRecord#od_atm_inventory.eff_users
        ),
        ?assertEqual(#{SubjectGroup => AllPrivs}, AtmInventoryRecord#od_atm_inventory.groups),
        ?assertEqual(#{SubjectGroup => {AllPrivs, [{od_atm_inventory, <<"self">>}]}}, AtmInventoryRecord#od_atm_inventory.eff_groups),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, UserWithPrivilege}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutPrivilege},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, SubjectGroup, <<"/atm_inventories">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{?HDR_LOCATION := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/groups/">>, SubjectGroup, <<"/atm_inventories/">>]),
                [AtmInventory] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(AtmInventory)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_atm_inventory,
            args = [auth, SubjectGroup, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            correct_values = #{<<"name">> => [ExpName]},
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


join_atm_inventory_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_ADD_ATM_INVENTORY privilege
    %   U1 gets all remaining privileges
    {SubjectGroup, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_ATM_INVENTORY
    ),
    NonAdmin = ozt_users:create(),

    EnvSetUpFun = fun() ->
        Creator = ozt_users:create(),
        AtmInventory = ozt_users:create_atm_inventory_for(Creator),
        Token = ozt_atm_inventories:create_group_invite_token(AtmInventory, Creator),
        {ok, Serialized} = tokens:serialize(Token),
        #{
            atm_inventory_id => AtmInventory,
            token => Serialized,
            token_id => Token#token.id
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{atm_inventory_id := AtmInventory, token_id := TokenId} = _Env, _) ->
        AtmInventories = ozt_groups:get_atm_inventories(SubjectGroup),
        ?assertEqual(lists:member(AtmInventory, AtmInventories), ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, UserWithPrivilege}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutPrivilege}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, SubjectGroup, <<"/atm_inventories/join">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{atm_inventory_id := AtmInventory} = _Env, _) ->
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    ExpLocation = ?URL(Config,
                        [<<"/groups/">>, SubjectGroup, <<"/atm_inventories/">>, AtmInventory]
                    ),
                    ?assertMatch(ExpLocation, Location),
                    true
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_atm_inventory,
            args = [auth, SubjectGroup, data],
            expected_result = ?OK_ENV(fun(#{atm_inventory_id := AtmInventory} = _Env, _) ->
                ?OK_BINARY(AtmInventory)
            end)
        },
        % TODO VFS-4520 Tests for GraphSync API
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
    AtmInventory = ozt_users:create_atm_inventory_for(UserWithoutPrivilege),
    ozt_atm_inventories:add_group(AtmInventory, SubjectGroup),
    Token2 = ozt_atm_inventories:create_group_invite_token(AtmInventory, UserWithoutPrivilege),
    {ok, Serialized2} = tokens:serialize(Token2),

    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, UserWithPrivilege}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, SubjectGroup, <<"/atm_inventories/join">>],
            expected_code = ?HTTP_409_CONFLICT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_atm_inventory,
            args = [auth, SubjectGroup, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_group, SubjectGroup, od_atm_inventory, AtmInventory))
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [Serialized2]}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec1)).


leave_atm_inventory_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_REMOVE_ATM_INVENTORY privilege
    %   U1 gets all remaining privileges
    {SubjectGroup, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_REMOVE_ATM_INVENTORY
    ),
    NonAdmin = ozt_users:create(),

    EnvSetUpFun = fun() ->
        AtmInventory = ozt_groups:create_atm_inventory_for(SubjectGroup),
        #{atm_inventory_id => AtmInventory}
    end,
    DeleteEntityFun = fun(#{atm_inventory_id := AtmInventory} = _Env) ->
        ozt_atm_inventories:remove_group(AtmInventory, SubjectGroup)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{atm_inventory_id := AtmInventory} = _Env, _) ->
        AtmInventories = ozt_groups:get_atm_inventories(SubjectGroup),
        ?assertEqual(lists:member(AtmInventory, AtmInventories), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, UserWithPrivilege},
                {admin, [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_ATM_INVENTORIES_REMOVE_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutPrivilege}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, SubjectGroup, <<"/atm_inventories/">>, atm_inventory_id],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_atm_inventory,
            args = [auth, SubjectGroup, atm_inventory_id],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_eff_atm_inventories_test(Config) ->
    {
        [{H1, _}, {H2, _}, {H3, _}, {H4, _}, {H5, _}],
        [{SubjectGroup, _} | _Groups], {UserWithView, UserWithoutView, NonAdmin}
    } = api_test_scenarios:create_eff_atm_inventories_env(Config),

    ExpAtmInventories = [H1, H2, H3, H4, H5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, UserWithView},
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutView},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, SubjectGroup, <<"/effective_atm_inventories">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_inventories">> => ExpAtmInventories}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_atm_inventories,
            args = [auth, SubjectGroup],
            expected_result = ?OK_LIST(ExpAtmInventories)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_atm_inventory function
    lists:foreach(fun(AtmInventoriesId) ->
        ?assert(ozt:rpc(group_logic, has_eff_atm_inventory, [SubjectGroup, AtmInventoriesId]))
    end, ExpAtmInventories),
    ?assert(not ozt:rpc(group_logic, has_eff_atm_inventory, [SubjectGroup, <<"asdiucyaie827346w">>])).


get_eff_atm_inventory_details_test(Config) ->
    {
        EffAtmInventoriesList, [{SubjectGroup, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_atm_inventories_env(Config),

    lists:foreach(fun({AtmInventory, AtmInventoryData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_ATM_INVENTORIES_VIEW]},
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
                path = [<<"/groups/">>, SubjectGroup, <<"/effective_atm_inventories/">>, AtmInventory],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:protected_atm_inventory(rest, AtmInventory, AtmInventoryData, ?SUB(nobody))
            },
            logic_spec = #logic_spec{
                module = group_logic,
                function = get_eff_atm_inventory,
                args = [auth, SubjectGroup, AtmInventory],
                expected_result = api_test_expect:protected_atm_inventory(logic, AtmInventory, AtmInventoryData, ?SUB(nobody))
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffAtmInventoriesList).


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
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time().
