%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning atm_inventory basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(atm_inventory_misc_api_test_SUITE).
-author("Lukasz Opiola").

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
    get_atm_lambdas_test/1,
    get_atm_workflow_schemas_test/1,
    update_test/1,
    delete_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        list_privileges_test,
        get_test,
        get_atm_lambdas_test,
        get_atm_workflow_schemas_test,
        update_test,
        delete_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    User = ozt_users:create(),
    VerifyFun = fun(AtmInventoryId, _) ->
        AtmInventory = ozt_atm_inventories:get(AtmInventoryId),
        ?assertEqual(?CORRECT_NAME, AtmInventory#od_atm_inventory.name),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_CREATE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/atm_inventories">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, Data) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/atm_inventories/">>]),
                    [AtmInventoryId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(AtmInventoryId, Data)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(_, Data) ->
                ?OK_TERM(fun(AtmInventoryId) -> VerifyFun(AtmInventoryId, Data) end)
            end)
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            correct_values = #{<<"name">> => [?CORRECT_NAME]},
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_test(Config) ->
    % Make sure that atm_inventories created in other tests are deleted.
    ozt:delete_all_entities(),

    User = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    AI1 = ozt_users:create_atm_inventory_for(User),
    AI2 = ozt_users:create_atm_inventory_for(User),
    AI3 = ozt_users:create_atm_inventory_for(User),
    AI4 = ozt_users:create_atm_inventory_for(User),
    AI5 = ozt_users:create_atm_inventory_for(User),
    ExpAtmInventories = [AI1, AI2, AI3, AI4, AI5],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_LIST]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/atm_inventories">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_inventories">> => ExpAtmInventories}
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = list,
            args = [auth],
            expected_result = ?OK_LIST(ExpAtmInventories)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also atm_inventory_logic:exist function
    lists:foreach(
        fun(AtmInventoryId) ->
            ?assert(ozt:rpc(atm_inventory_logic, exists, [AtmInventoryId]))
        end, ExpAtmInventories
    ),
    ?assertNot(ozt:rpc(atm_inventory_logic, exists, [<<"asdiucyaie827346w">>])).


list_privileges_test(Config) ->
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/atm_inventories/privileges">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"member">> => [atom_to_binary(P, utf8) || P <- privileges:atm_inventory_member()],
                <<"manager">> => [atom_to_binary(P, utf8) || P <- privileges:atm_inventory_manager()],
                <<"admin">> => [atom_to_binary(P, utf8) || P <- privileges:atm_inventory_admin()]
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_test(Config) ->
    UserWithoutViewPrivs = ozt_users:create(),
    UserWithViewPrivs = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    AtmInventoryName = ?UNIQUE_STRING,
    AtmInventoryData = #{<<"name">> => AtmInventoryName},
    AtmInventory = ozt_users:create_atm_inventory_for(UserWithoutViewPrivs, AtmInventoryData),

    AllPrivs = privileges:atm_inventory_privileges(),
    ozt_atm_inventories:set_user_privileges(AtmInventory, UserWithoutViewPrivs, AllPrivs -- [?ATM_INVENTORY_VIEW]),
    ozt_atm_inventories:add_user(AtmInventory, UserWithViewPrivs, [?ATM_INVENTORY_VIEW]),
    ozt:reconcile_entity_graph(),

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, UserWithViewPrivs}
            ],
            unauthorized = [nobody],
            forbidden = [
                {admin, [?OZ_ATM_INVENTORIES_VIEW]},
                {user, NonAdmin},
                {user, UserWithoutViewPrivs}
            ]
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get,
            args = [auth, AtmInventory],
            expected_result = ?OK_TERM(
                fun(#od_atm_inventory{
                    name = Name, users = Users, groups = #{},
                    eff_users = EffUsers, eff_groups = #{},
                    bottom_up_dirty = false
                }) ->
                    ?assertEqual(AtmInventoryName, Name),
                    ?assertEqual(Users, #{
                        UserWithoutViewPrivs => AllPrivs -- [?ATM_INVENTORY_VIEW],
                        UserWithViewPrivs => [?ATM_INVENTORY_VIEW]}
                    ),
                    ?assertEqual(EffUsers, #{
                        UserWithoutViewPrivs => {AllPrivs -- [?ATM_INVENTORY_VIEW], [{od_atm_inventory, <<"self">>}]},
                        UserWithViewPrivs => {[?ATM_INVENTORY_VIEW], [{od_atm_inventory, <<"self">>}]}
                    })
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_atm_inventory, id = AtmInventory, aspect = instance, scope = private},
            expected_result = ?OK_MAP_CONTAINS(AtmInventoryData#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(AtmInventory, Id)
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
                {admin, [?OZ_ATM_INVENTORIES_VIEW]},
                {user, UserWithoutViewPrivs},
                {user, UserWithViewPrivs}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_inventories/">>, AtmInventory],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:protected_atm_inventory(rest, AtmInventory, AtmInventoryData, ?SUB(user, UserWithoutViewPrivs))
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_protected_data,
            args = [auth, AtmInventory],
            expected_result = api_test_expect:protected_atm_inventory(logic, AtmInventory, AtmInventoryData, ?SUB(user, UserWithoutViewPrivs))
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)).


get_atm_lambdas_test(Config) ->
    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    AtmInventory = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventory, AnotherMember, []),

    ExpAtmLambdas = lists:map(fun(_) ->
        ozt_atm_lambdas:create(AtmInventory)
    end, lists:seq(1, 10)),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW]},
                {user, Creator},
                {user, AnotherMember}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/atm_lambdas">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_lambdas">> => ExpAtmLambdas}
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_atm_lambdas,
            args = [auth, AtmInventory],
            expected_result = ?OK_LIST(ExpAtmLambdas)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_atm_workflow_schemas_test(Config) ->
    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    AtmInventory = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventory, AnotherMember, []),

    ExpAtmWorkflowSchemas = lists:map(fun(_) ->
        ozt_atm_workflow_schemas:create(AtmInventory)
    end, lists:seq(1, 10)),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW]},
                {user, Creator},
                {user, AnotherMember}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/atm_workflow_schemas">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_workflow_schemas">> => ExpAtmWorkflowSchemas}
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_atm_workflow_schemas,
            args = [auth, AtmInventory],
            expected_result = ?OK_LIST(ExpAtmWorkflowSchemas)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_test(Config) ->
    UserWithUpdatePrivs = ozt_users:create(),
    UserWithoutUpdatePrivs = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AllPrivs = privileges:atm_inventory_privileges(),

    InitialName = ?UNIQUE_STRING,
    TargetName = ?CORRECT_NAME,

    EnvSetUpFun = fun() ->
        AtmInventory = ozt_users:create_atm_inventory_for(UserWithoutUpdatePrivs, #{<<"name">> => InitialName}),
        ozt_atm_inventories:set_user_privileges(AtmInventory, UserWithoutUpdatePrivs, AllPrivs -- [?ATM_INVENTORY_UPDATE]),
        ozt_atm_inventories:add_user(AtmInventory, UserWithUpdatePrivs, [?ATM_INVENTORY_UPDATE]),
        #{atm_inventory_id => AtmInventory}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{atm_inventory_id := AtmInventoryId} = _Env, _Data) ->
        AtmInventory = ozt_atm_inventories:get(AtmInventoryId),
        ExpName = case ShouldSucceed of
            false -> InitialName;
            true -> TargetName
        end,
        ?assertEqual(ExpName, AtmInventory#od_atm_inventory.name)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_UPDATE]},
                {user, UserWithUpdatePrivs}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutUpdatePrivs},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/atm_inventories/">>, atm_inventory_id],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = update,
            args = [auth, atm_inventory_id, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_atm_inventory, id = atm_inventory_id, aspect = instance},
            expected_result = ?OK_RES
        },
        data_spec = #data_spec{
            at_least_one = [<<"name">>],
            correct_values = #{<<"name">> => [TargetName]},
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    UserWithDeletePrivs = ozt_users:create(),
    UserWithoutDeletePrivs = ozt_users:create(),
    NonAdmin = ozt_users:create(),
    AllPrivs = privileges:atm_inventory_privileges(),

    EnvSetUpFun = fun() ->
        AtmInventoryId = ozt_users:create_atm_inventory_for(UserWithoutDeletePrivs),
        ozt_atm_inventories:set_user_privileges(AtmInventoryId, UserWithoutDeletePrivs, AllPrivs -- [?ATM_INVENTORY_DELETE]),
        ozt_atm_inventories:add_user(AtmInventoryId, UserWithDeletePrivs, [?ATM_INVENTORY_DELETE]),
        AtmLambdas = lists:map(fun(_) ->
            ozt_atm_lambdas:create(AtmInventoryId)
        end, lists:seq(1, rand:uniform(5))),
        AtmWfSchemas = lists:map(fun(_) ->
            ozt_atm_workflow_schemas:create(AtmInventoryId)
        end, lists:seq(1, rand:uniform(5))),
        #{atm_inventory_id => AtmInventoryId, atm_lambdas => AtmLambdas, atm_workflow_schemas => AtmWfSchemas}
    end,
    DeleteEntityFun = fun(#{atm_inventory_id := AtmInventoryId} = _Env) ->
        ozt_atm_inventories:delete(AtmInventoryId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{
        atm_inventory_id := AtmInventoryId, atm_lambdas := AtmLambdas, atm_workflow_schemas := AtmWfSchemas
    }, _Data) ->
        ?assertEqual(not ShouldSucceed, lists:member(AtmInventoryId, ozt_atm_inventories:list())),
        % referenced atm_lambdas SHOULD NOT be removed if an inventory is
        [?assert(ozt_atm_lambdas:exists(AL)) || AL <- AtmLambdas],
        % inventory's atm_workflow schemas SHOULD be removed along with the inventory
        [?assertEqual(not ShouldSucceed, ozt_atm_workflow_schemas:exists(AWS)) || AWS <- AtmWfSchemas]
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_DELETE]},
                {user, UserWithDeletePrivs}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutDeletePrivs},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/atm_inventories/">>, atm_inventory_id],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = delete,
            args = [auth, atm_inventory_id],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_atm_inventory, id = atm_inventory_id, aspect = instance},
            expected_result = ?OK_RES
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
    ozt:init_per_suite(Config).

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time().
