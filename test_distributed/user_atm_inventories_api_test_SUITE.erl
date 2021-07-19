%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user atm_inventories API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(user_atm_inventories_api_test_SUITE).
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
    create_atm_inventory_test/1,
    join_atm_inventory_test/1,
    get_atm_inventory_test/1,
    leave_atm_inventory_test/1,

    list_eff_atm_inventories_test/1,
    get_eff_atm_inventory_test/1
]).

all() ->
    ?ALL([
        list_atm_inventories_test,
        create_atm_inventory_test,
        join_atm_inventory_test,
        get_atm_inventory_test,
        leave_atm_inventory_test,

        list_eff_atm_inventories_test,
        get_eff_atm_inventory_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

list_atm_inventories_test(Config) ->
    Creator = ozt_users:create(),
    AnotherUser = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    ExpAtmInventories = lists:map(fun(_) ->
        AtmInventory = ozt_users:create_atm_inventory_for(Creator),
        ozt_atm_inventories:add_user(AtmInventory, AnotherUser),
        AtmInventory
    end, lists:seq(1, 5)),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, Creator},
                {user, AnotherUser}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/atm_inventories">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_inventories">> => ExpAtmInventories}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Creator},
                {admin, [?OZ_USERS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, AnotherUser},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_atm_inventories,
            args = [auth, Creator],
            expected_result = ?OK_LIST(ExpAtmInventories)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


create_atm_inventory_test(Config) ->
    FirstCreator = ozt_users:create(),
    AnotherCreator = ozt_users:create(),

    ExpName = ?CORRECT_NAME,
    AllPrivs = privileges:atm_inventory_privileges(),

    VerifyFun = fun(AtmInventory) ->
        ozt:reconcile_entity_graph(),
        AtmInventoryRecord = ozt_atm_inventories:get(AtmInventory),
        ?assertEqual(ExpName, AtmInventoryRecord#od_atm_inventory.name),

        [User] = ?assertMatch([_], maps:keys(AtmInventoryRecord#od_atm_inventory.users)),
        ?assertEqual(#{User => AllPrivs}, AtmInventoryRecord#od_atm_inventory.users),
        ?assertEqual(#{User => {AllPrivs, [{od_atm_inventory, <<"self">>}]}}, AtmInventoryRecord#od_atm_inventory.eff_users),
        ?assertEqual(#{}, AtmInventoryRecord#od_atm_inventory.groups),
        ?assertEqual(#{}, AtmInventoryRecord#od_atm_inventory.eff_groups),

        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, FirstCreator},
                {user, AnotherCreator},
                {admin, [?OZ_USERS_ADD_RELATIONSHIPS, ?OZ_ATM_INVENTORIES_CREATE]}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/atm_inventories">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/user/atm_inventories/">>]),
                [AtmInventory] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(AtmInventory)
            end
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            correct_values = #{<<"name">> => [ExpName]},
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, FirstCreator}
            ],
            unauthorized = [nobody],
            forbidden = [{user, AnotherCreator}]
        },

        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_atm_inventory,
            args = [auth, FirstCreator, data],
            expected_result = ?OK_TERM(VerifyFun)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


join_atm_inventory_test(Config) ->
    JoiningUser = ozt_users:create(),
    AnotherUser = ozt_users:create(),

    EnvSetUpFun = fun() ->
        Creator = ozt_users:create(),
        AtmInventory = ozt_users:create_atm_inventory_for(Creator),
        Token = ozt_atm_inventories:create_user_invite_token(AtmInventory, Creator),
        {ok, Serialized} = tokens:serialize(Token),
        #{
            atm_inventory_id => AtmInventory,
            token => Serialized,
            token_id => Token#token.id
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{atm_inventory_id := AtmInventory, token_id := TokenId} = _Env, _) ->
        AtmInventories = ozt_users:get_atm_inventories(JoiningUser),
        ?assertEqual(lists:member(AtmInventory, AtmInventories), ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, JoiningUser}]},
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/atm_inventories/join">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{atm_inventory_id := AtmInventory} = _Env, _) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config, [<<"/user/atm_inventories/">>, AtmInventory]),
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
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, <<"123qwe">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)}
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
                {admin, [?OZ_USERS_ADD_RELATIONSHIPS]},
                {user, JoiningUser}
            ],
            unauthorized = [nobody],
            forbidden = [{user, AnotherUser}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = join_atm_inventory,
            args = [auth, JoiningUser, data],
            expected_result = ?OK_ENV(fun(#{atm_inventory_id := AtmInventory} = _Env, _) ->
                ?OK_BINARY(AtmInventory)
            end)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check that token is not consumed upon failed operation
    AtmInventory = ozt_users:create_atm_inventory_for(JoiningUser),
    Token2 = ozt_atm_inventories:create_user_invite_token(AtmInventory, JoiningUser),
    {ok, Serialized2} = tokens:serialize(Token2),

    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, JoiningUser}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/atm_inventories/join">>,
            expected_code = ?HTTP_409_CONFLICT
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = join_atm_inventory,
            args = [auth, JoiningUser, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_user, JoiningUser, od_atm_inventory, AtmInventory))
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [Serialized2]}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec1)).


get_atm_inventory_test(Config) ->
    FirstMember = ozt_users:create(),
    AnotherMember = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    AtmInventoryName = ?CORRECT_NAME,
    AtmInventoryData = #{<<"name">> => AtmInventoryName},
    AtmInventory = ozt_atm_inventories:create(AtmInventoryName),
    ozt_atm_inventories:add_user(AtmInventory, FirstMember),
    ozt_atm_inventories:add_user(AtmInventory, AnotherMember),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, FirstMember},
                {user, AnotherMember}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/atm_inventories/">>, AtmInventory],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:protected_atm_inventory(rest, AtmInventory, AtmInventoryData, ?SUB(nobody))
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW]},
                {user, FirstMember}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, AnotherMember},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_atm_inventory,
            args = [auth, FirstMember, AtmInventory],
            expected_result = api_test_expect:protected_atm_inventory(logic, AtmInventory, AtmInventoryData, ?SUB(nobody))
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


leave_atm_inventory_test(Config) ->
    SubjectUser = ozt_users:create(),
    AnotherUser = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    EnvSetUpFun = fun() ->
        AtmInventory = ozt_atm_inventories:create(),
        ozt_atm_inventories:add_user(AtmInventory, SubjectUser),
        ozt_atm_inventories:add_user(AtmInventory, AnotherUser),
        #{atm_inventory_id => AtmInventory}
    end,
    DeleteEntityFun = fun(#{atm_inventory_id := AtmInventory} = _Env) ->
        ozt_atm_inventories:remove_user(AtmInventory, SubjectUser)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{atm_inventory_id := AtmInventory} = _Env, _) ->
        AtmInventories = ozt_users:get_atm_inventories(SubjectUser),
        ?assertEqual(lists:member(AtmInventory, AtmInventories), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, SubjectUser}]},
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/atm_inventories/">>, atm_inventory_id],
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
                {user, SubjectUser},
                {admin, [?OZ_USERS_REMOVE_RELATIONSHIPS, ?OZ_ATM_INVENTORIES_REMOVE_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, AnotherUser},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = leave_atm_inventory,
            args = [auth, SubjectUser, atm_inventory_id],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])).


list_eff_atm_inventories_test(Config) ->
    {
        [{H1, _}, {H2, _}, {H3, _}, {H4, _}, {H5, _}],
        _Groups, {User1, User2, NonAdmin}
    } = api_test_scenarios:create_eff_atm_inventories_env(Config),

    AnotherAtmInventory = ozt_users:create_atm_inventory_for(User2),
    ozt_atm_inventories:add_user(AnotherAtmInventory, User1),

    ExpAtmInventories = [H1, H2, H3, H4, H5, AnotherAtmInventory],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, User1},
                {user, User2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/effective_atm_inventories">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"atm_inventories">> => ExpAtmInventories}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User2},
                {admin, [?OZ_USERS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User1},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_atm_inventories,
            args = [auth, User2],
            expected_result = ?OK_LIST(ExpAtmInventories)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % check also user_logic:has_eff_atm_inventory function
    lists:foreach(fun(AtmInventoryId) ->
        ?assert(ozt:rpc(user_logic, has_eff_atm_inventory, [User2, AtmInventoryId]))
    end, ExpAtmInventories),
    ?assert(not ozt:rpc(user_logic, has_eff_atm_inventory, [User2, <<"asdiucyaie827346w">>])).


get_eff_atm_inventory_test(Config) ->
    {
        EffAtmInventoriesList, _Groups, {User1, User2, NonAdmin}
    } = api_test_scenarios:create_eff_atm_inventories_env(Config),

    AnotherAtmInventoryData = #{<<"name">> => ?CORRECT_NAME},
    AnotherAtmInventory = ozt_users:create_atm_inventory_for(User2, AnotherAtmInventoryData),
    ozt_atm_inventories:add_user(AnotherAtmInventory, User1),

    NewEffAtmInventoriesList = [{AnotherAtmInventory, AnotherAtmInventoryData} | EffAtmInventoriesList],
    lists:foreach(fun({AtmInventory, AtmInventoryData}) ->
        Creator = case AtmInventory of
            AnotherAtmInventory -> ?SUB(user, User2);
            _ -> ?SUB(nobody)
        end,
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    {user, User1},
                    {user, User2}
                ]
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/user/effective_atm_inventories/">>, AtmInventory],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:protected_atm_inventory(rest, AtmInventory, AtmInventoryData, Creator)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

        % Check that regular client can't make request
        % on behalf of other client
        ApiTestSpec2 = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_ATM_INVENTORIES_VIEW]},
                    {user, User1}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, User2},
                    {user, NonAdmin}
                ]
            },
            logic_spec = #logic_spec{
                module = user_logic,
                function = get_eff_atm_inventory,
                args = [auth, User1, AtmInventory],
                expected_result = api_test_expect:protected_atm_inventory(logic, AtmInventory, AtmInventoryData, Creator)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

    end, NewEffAtmInventoriesList).


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
