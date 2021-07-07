%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning atm_inventory groups API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(atm_inventory_groups_api_test_SUITE).
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
    add_group_test/1,
    add_group_with_privileges_test/1,
    create_group_test/1,
    remove_group_test/1,
    list_groups_test/1,
    get_group_test/1,

    get_group_privileges_test/1,
    update_group_privileges_test/1,

    list_eff_groups_test/1,
    get_eff_group_test/1,
    get_eff_group_privileges_test/1,
    get_eff_group_membership_intermediaries/1
]).

all() ->
    ?ALL([
        add_group_test,
        add_group_with_privileges_test,
        create_group_test,
        remove_group_test,
        list_groups_test,
        get_group_test,

        get_group_privileges_test,
        update_group_privileges_test,

        list_eff_groups_test,
        get_eff_group_test,
        get_eff_group_privileges_test,
        get_eff_group_membership_intermediaries
    ]).

-define(ALL_PRIVS, privileges:atm_inventory_privileges()).

%%%===================================================================
%%% Test functions
%%%===================================================================

add_group_test(Config) ->
    Creator = ozt_users:create(),
    UserNoAddGroupPriv = ozt_users:create(),
    UserNoAddAtmInventoryPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    SubjectGroup = ozt_users:create_group_for(Creator),
    AtmInventory = ozt_users:create_atm_inventory_for(Creator),

    ozt_atm_inventories:add_user(AtmInventory, UserNoAddGroupPriv, ?ALL_PRIVS -- [?ATM_INVENTORY_ADD_GROUP]),
    ozt_groups:add_user(SubjectGroup, UserNoAddGroupPriv, privileges:group_privileges()),

    ozt_atm_inventories:add_user(AtmInventory, UserNoAddAtmInventoryPriv, ?ALL_PRIVS),
    ozt_groups:add_user(SubjectGroup, UserNoAddAtmInventoryPriv, privileges:group_privileges() -- [?GROUP_ADD_ATM_INVENTORY]),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, _) ->
            ?assert(lists:member(SubjectGroup, ozt_atm_inventories:get_groups(AtmInventory))),
            ozt_atm_inventories:remove_group(AtmInventory, SubjectGroup);
        (false = _ShouldSucceed, _, _) ->
            ?assertNot(lists:member(SubjectGroup, ozt_atm_inventories:get_groups(AtmInventory)))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, Creator},
                root,
                {admin, [?OZ_ATM_INVENTORIES_ADD_RELATIONSHIPS, ?OZ_GROUPS_ADD_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserNoAddGroupPriv},
                {user, UserNoAddAtmInventoryPriv}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/groups/">>, SubjectGroup],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/atm_inventories/">>, AtmInventory, <<"/groups/">>, SubjectGroup]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = add_group,
            args = [auth, AtmInventory, SubjectGroup, data],
            expected_result = ?OK_BINARY(SubjectGroup)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )).


add_group_with_privileges_test(Config) ->
    Creator = ozt_users:create(),
    UserNoSetPrivsPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    SubjectGroup = ozt_users:create_group_for(Creator),
    AtmInventory = ozt_users:create_atm_inventory_for(Creator),

    ozt_atm_inventories:add_user(AtmInventory, UserNoSetPrivsPriv, ?ALL_PRIVS -- [?ATM_INVENTORY_SET_PRIVILEGES]),
    ozt_groups:add_user(SubjectGroup, UserNoSetPrivsPriv, privileges:group_privileges()),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, Data) ->
            ExpPrivs = maps:get(<<"privileges">>, Data),
            ActualPrivs = ozt_atm_inventories:get_group_privileges(AtmInventory, SubjectGroup),
            ?assertEqual(lists:sort(ExpPrivs), lists:sort(ActualPrivs)),
            ozt_atm_inventories:remove_group(AtmInventory, SubjectGroup);
        (false = _ShouldSucceed, _, _) ->
            Groups = ozt_atm_inventories:get_groups(AtmInventory),
            ?assertNot(lists:member(SubjectGroup, Groups))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, Creator},
                root,
                {admin, [?OZ_ATM_INVENTORIES_ADD_RELATIONSHIPS, ?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_ATM_INVENTORIES_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserNoSetPrivsPriv}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/groups/">>, SubjectGroup],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/atm_inventories/">>, AtmInventory, <<"/groups/">>, SubjectGroup]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = add_group,
            args = [auth, AtmInventory, SubjectGroup, data],
            expected_result = ?OK_BINARY(SubjectGroup)
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [?ATM_INVENTORY_UPDATE, ?ATM_INVENTORY_REMOVE_GROUP],
                    [?ATM_INVENTORY_ADD_USER, ?ATM_INVENTORY_VIEW]
                ]
            },
            bad_values = [
                {<<"privileges">>, <<"">>,
                    ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>)}
            ]
        }
    },

    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )).


create_group_test(Config) ->
    % create atm_inventory with 2 users:
    %   U2 gets the ATM_INVENTORY_ADD_GROUP privilege
    %   U1 gets all remaining privileges
    {AtmInventory, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_ADD_GROUP
    ),
    NonAdmin = ozt_users:create(),

    VerifyFun = fun(GroupId, ExpType) ->
        GroupRecord = ozt_groups:get(GroupId),
        ?assertEqual(?CORRECT_NAME, GroupRecord#od_group.name),
        ?assertEqual(ExpType, GroupRecord#od_group.type),
        Groups = ozt_atm_inventories:get_groups(AtmInventory),
        ?assert(lists:member(GroupId, Groups)),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, UserWithPrivilege},
                {admin, [?OZ_ATM_INVENTORIES_ADD_RELATIONSHIPS, ?OZ_GROUPS_CREATE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutPrivilege}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/groups">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                BaseURL = ?URL(Config, [<<"/atm_inventories/">>, AtmInventory, <<"/groups/">>]),

                fun(#{<<"Location">> := Location} = _Headers) ->
                    [GroupId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(GroupId, ExpType)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = create_group,
            args = [auth, AtmInventory, data],
            expected_result = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                ?OK_TERM(fun(AtmInventoryId) -> VerifyFun(AtmInventoryId, ExpType) end)
            end)
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [<<"name">>],
            optional = [<<"type">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"type">> => ?GROUP_TYPES
            },
            bad_values = [
                {<<"type">>, kingdom,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>, ?GROUP_TYPES)},
                {<<"type">>, 1234, ?ERROR_BAD_VALUE_ATOM(<<"type">>)}
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


remove_group_test(Config) ->
    % create atm_inventory with 2 users:
    %   U2 gets the ATM_INVENTORY_REMOVE_GROUP privilege
    %   U1 gets all remaining privileges
    {AtmInventory, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_REMOVE_GROUP
    ),
    NonAdmin = ozt_users:create(),

    EnvSetUpFun = fun() ->
        SubjectGroup = ozt_groups:create(),
        ozt_atm_inventories:add_group(AtmInventory, SubjectGroup),
        #{subjectGroup => SubjectGroup}
    end,
    DeleteEntityFun = fun(#{subjectGroup := SubjectGroup} = _Env) ->
        ozt_atm_inventories:remove_group(AtmInventory, SubjectGroup)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{subjectGroup := SubjectGroup} = _Env, _) ->
        Groups = ozt_atm_inventories:get_groups(AtmInventory),
        ?assertEqual(lists:member(SubjectGroup, Groups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS]},
                {user, UserWithPrivilege}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutPrivilege},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/groups/">>, subjectGroup],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = remove_group,
            args = [auth, AtmInventory, subjectGroup],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_groups_test(Config) ->
    % create atm_inventory with 2 users:
    %   U2 gets the ATM_INVENTORY_VIEW privilege
    %   U1 gets all remaining privileges
    {AtmInventory, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_VIEW
    ),
    NonAdmin = ozt_users:create(),

    ExpGroups = lists:map(fun(_) ->
        Group = ozt_groups:create(),
        ozt_atm_inventories:add_group(AtmInventory, Group),
        Group
    end, lists:seq(1, 5)),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_LIST_RELATIONSHIPS]},
                {user, UserWithPrivilege}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutPrivilege}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/groups">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_groups,
            args = [auth, AtmInventory],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_group_test(Config) ->
    % create atm_inventory with 2 users:
    %   U2 gets the ATM_INVENTORY_VIEW privilege
    %   U1 gets all remaining privileges
    {AtmInventory, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_VIEW
    ),
    NonAdmin = ozt_users:create(),

    GroupData = #{<<"name">> => ?GROUP_NAME1, <<"type">> => ?GROUP_TYPE1},
    SubjectGroup = ozt_groups:create(GroupData),
    ozt_atm_inventories:add_group(AtmInventory, SubjectGroup),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_VIEW]},
                {user, UserWithPrivilege}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutPrivilege}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/groups/">>, SubjectGroup],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:shared_group(rest, SubjectGroup, GroupData)
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_group,
            args = [auth, AtmInventory, SubjectGroup],
            expected_result = api_test_expect:shared_group(logic, SubjectGroup, GroupData)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = SubjectGroup, aspect = instance, scope = shared
            },
            auth_hint = ?THROUGH_ATM_INVENTORY(AtmInventory),
            expected_result = api_test_expect:shared_group(gs, SubjectGroup, GroupData)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_group_privileges_test(Config) ->
    % create atm_inventory with 2 users:
    %   U2 gets the ATM_INVENTORY_VIEW privilege
    %   U1 gets all remaining privileges
    {AtmInventory, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_VIEW_PRIVILEGES
    ),
    NonAdmin = ozt_users:create(),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get group privileges and sometimes not)
    GroupCreator = ozt_users:create(),
    SubjectGroup = ozt_users:create_group_for(GroupCreator),
    ozt_atm_inventories:add_group(AtmInventory, SubjectGroup),

    InitialPrivs = privileges:atm_inventory_member(),
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],

    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        CurrentPrivs = ozt_atm_inventories:get_group_privileges(AtmInventory, SubjectGroup),
        ozt_atm_inventories:set_group_privileges(
            AtmInventory, SubjectGroup, (CurrentPrivs ++ PrivsToGrant) -- PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW_PRIVILEGES]},
                {user, UserWithPrivilege}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutPrivilege}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [
                <<"/atm_inventories/">>, AtmInventory, <<"/groups/">>, SubjectGroup, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_group_privileges,
            args = [auth, AtmInventory, SubjectGroup],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, ?ALL_PRIVS, [],
        {user, GroupCreator}, ?ATM_INVENTORY_VIEW_PRIVILEGES
    ])).


update_group_privileges_test(Config) ->
    % create atm_inventory with 2 users:
    %   U2 gets the ATM_INVENTORY_SET_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {AtmInventory, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_SET_PRIVILEGES
    ),
    NonAdmin = ozt_users:create(),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to update group privileges and sometimes not)
    GroupCreator = ozt_users:create(),
    SubjectGroup = ozt_users:create_group_for(GroupCreator),
    ozt_atm_inventories:add_group(AtmInventory, SubjectGroup),

    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        CurrentPrivs = ozt_atm_inventories:get_group_privileges(AtmInventory, SubjectGroup),
        ozt_atm_inventories:set_group_privileges(
            AtmInventory, SubjectGroup, (CurrentPrivs ++ PrivsToGrant) -- PrivsToRevoke
        )
    end,
    GetPrivsFun = fun() ->
        ozt_atm_inventories:get_group_privileges(AtmInventory, SubjectGroup)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_SET_PRIVILEGES]},
                {user, UserWithPrivilege}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutPrivilege}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [
                <<"/atm_inventories/">>, AtmInventory, <<"/groups/">>, SubjectGroup, <<"/privileges">>
            ],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = update_group_privileges,
            args = [auth, AtmInventory, SubjectGroup, data],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, ?ALL_PRIVS,
        {user, GroupCreator}, ?ATM_INVENTORY_SET_PRIVILEGES
    ])).


list_eff_groups_test(Config) ->
    {AtmInventory,
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}, {G6, _}],
        _EffUsers, {UserWithoutView, UserWithView, NonAdmin}
    } = api_test_scenarios:create_atm_inventory_eff_users_env(Config),


    ExpGroups = [G1, G2, G3, G4, G5, G6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_LIST_RELATIONSHIPS]},
                {user, UserWithView}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutView},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/effective_groups">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_eff_groups,
            args = [auth, AtmInventory],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also atm_inventory_logic:has_eff_group function
    lists:foreach(fun(GroupId) ->
        ?assert(ozt:rpc(atm_inventory_logic, has_eff_group, [AtmInventory, GroupId]))
    end, ExpGroups),
    ?assert(not ozt:rpc(atm_inventory_logic, has_eff_group, [AtmInventory, <<"asdiucyaie827346w">>])).


get_eff_group_test(Config) ->
    {
        AtmInventory, EffGroups, _EffUsers, {UserWithoutView, UserWithView, NonAdmin}
    } = api_test_scenarios:create_atm_inventory_eff_users_env(Config),

    lists:foreach(fun({GroupId, GroupData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_GROUPS_VIEW]},
                    {user, UserWithView}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, UserWithoutView},
                    {user, NonAdmin}
                ]
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/atm_inventories/">>, AtmInventory, <<"/effective_groups/">>, GroupId],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:shared_group(rest, GroupId, GroupData)
            },
            logic_spec = #logic_spec{
                module = atm_inventory_logic,
                function = get_eff_group,
                args = [auth, AtmInventory, GroupId],
                expected_result = api_test_expect:shared_group(logic, GroupId, GroupData)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_group, id = GroupId,
                    aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_ATM_INVENTORY(AtmInventory),
                expected_result = api_test_expect:shared_group(gs, GroupId, GroupData)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffGroups).


get_eff_group_privileges_test(Config) ->
    %% Create environment with the following relations:
    %%
    %%                  AtmInventory
    %%                 /  ||  \
    %%                /   ||   \
    %%     [~atm_inventory_view]  ||  [atm_inventory_view]
    %%           /        ||        \
    %%        User1      /  \      User2
    %%                  /    \
    %%                 /      \
    %%             Group1    Group2
    %%                |         |
    %%                |         |
    %%             Group3       |
    %%                  \       |
    %%                   \      |
    %%                    Group4
    %%                      |
    %%                    User3
    %%      <<user>>
    %%      NonAdmin

    % create atm_inventory with 2 users:
    %   U2 gets the ATM_INVENTORY_VIEW privilege
    %   U1 gets all remaining privileges
    {AtmInventory, UserWithoutView, UserWithView} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_VIEW_PRIVILEGES
    ),
    NonAdmin = ozt_users:create(),

    % User whose eff privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get user privileges and sometimes not)
    SubjectUser = ozt_users:create(),

    Group1 = ozt_groups:create(),
    Group2 = ozt_groups:create(),
    Group3 = ozt_groups:create(),
    Group4 = ozt_groups:create(),

    ozt_atm_inventories:add_group(AtmInventory, Group1),
    ozt_atm_inventories:add_group(AtmInventory, Group2),
    ozt_groups:add_child(Group1, Group3),
    ozt_groups:add_child(Group2, Group4),
    ozt_groups:add_child(Group3, Group4),
    ozt_groups:add_user(Group4, SubjectUser),

    ozt:reconcile_entity_graph(),

    InitialPrivs = privileges:atm_inventory_member(),
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],

    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        % In case of GRANT, randomly split privileges into four
        % parts and update groups with the privileges. G3 eff_privileges
        % should contain the sum of those. In case of revoke, the
        % privileges must be revoked for all 3 entities.
        #{1 := PrivsToGrant1, 2 := PrivsToGrant2} = lists:foldl(
            fun(Privilege, AccMap) ->
                Index = rand:uniform(2),
                AccMap#{
                    Index => [Privilege | maps:get(Index, AccMap)]
                }
            end, #{1 => [], 2 => []}, PrivsToGrant),

        CurrentGroup1Privs = ozt_atm_inventories:get_group_privileges(AtmInventory, Group1),
        CurrentGroup2Privs = ozt_atm_inventories:get_group_privileges(AtmInventory, Group2),
        ozt_atm_inventories:set_group_privileges(
            AtmInventory, Group1, (CurrentGroup1Privs ++ PrivsToGrant1) -- PrivsToRevoke
        ),
        ozt_atm_inventories:set_group_privileges(
            AtmInventory, Group2, (CurrentGroup2Privs ++ PrivsToGrant2) -- PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW_PRIVILEGES]},
                {user, UserWithView}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutView},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [
                <<"/atm_inventories/">>, AtmInventory, <<"/effective_groups/">>, Group4,
                <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_eff_group_privileges,
            args = [auth, AtmInventory, Group4],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, ?ALL_PRIVS, [],
        {user, SubjectUser}, ?ATM_INVENTORY_VIEW_PRIVILEGES
    ])).


get_eff_group_membership_intermediaries(Config) ->
    %% Create environment with the following relations:
    %%
    %%      AtmInventory1       AtmInventory2     AtmInventory3
    %%       | |  \     /   |  \     /  | \
    %%       | |   \   /    |   \   /   |  \
    %%       |  \   Group2  |   Group3   \  Group4 (no view privs)
    %%       |   \   /      |   /  |      \  /
    %%        \   \ /       |  /   |      User2 (no view privs)
    %%         \  Group1----|-'    |
    %%          \     \     |     /
    %%           \     \    |    /
    %%            \     \   |   /
    %%             '------UserGroup
    %%                      |
    %%                  User1 (view privs)
    %%      <<user>>
    %%      NonAdmin

    User1 = ozt_users:create(),
    User2 = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    UserGroup = ozt_users:create_group_for(User1),
    Group1 = ozt_groups:create(),
    Group2 = ozt_groups:create(),
    Group3 = ozt_groups:create(),
    Group4 = ozt_groups:create(),

    AtmInventory1 = ozt_atm_inventories:create(),
    AtmInventory2 = ozt_atm_inventories:create(),
    AtmInventory3 = ozt_atm_inventories:create(),

    ozt_groups:add_user(Group4, User2),

    ozt_atm_inventories:add_user(AtmInventory1, User1),
    ozt_atm_inventories:add_user(AtmInventory3, User2, ?ALL_PRIVS -- [?ATM_INVENTORY_VIEW]),

    ozt_groups:add_child(Group1, UserGroup),
    ozt_groups:add_child(Group2, Group1),
    ozt_groups:add_child(Group3, Group1),
    ozt_groups:add_child(Group3, UserGroup),

    ozt_atm_inventories:add_group(AtmInventory1, UserGroup),
    ozt_atm_inventories:add_group(AtmInventory1, Group1),
    ozt_atm_inventories:add_group(AtmInventory1, Group2),
    ozt_atm_inventories:add_group(AtmInventory2, UserGroup),
    ozt_atm_inventories:add_group(AtmInventory2, Group2),
    ozt_atm_inventories:add_group(AtmInventory2, Group3),
    ozt_atm_inventories:add_group(AtmInventory3, Group3),
    ozt_atm_inventories:add_group(AtmInventory3, Group4, ?ALL_PRIVS -- [?ATM_INVENTORY_VIEW]),

    ozt:reconcile_entity_graph(),

    % {AtmInventoryId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}
    ExpectedMembershipIntermediaries = [
        {AtmInventory1, UserGroup, [User1], ordsets:from_list([
            {od_atm_inventory, ?SELF_INTERMEDIARY},
            {od_group, Group1},
            {od_group, Group2}
        ])},
        {AtmInventory1, Group1, [User1], ordsets:from_list([
            {od_atm_inventory, ?SELF_INTERMEDIARY},
            {od_group, Group2}
        ])},
        {AtmInventory1, Group2, [User1], ordsets:from_list([
            {od_atm_inventory, ?SELF_INTERMEDIARY}
        ])},

        {AtmInventory2, UserGroup, [User1], ordsets:from_list([
            {od_atm_inventory, ?SELF_INTERMEDIARY},
            {od_group, Group2},
            {od_group, Group3}
        ])},
        {AtmInventory2, Group1, [User1], ordsets:from_list([
            {od_group, Group2},
            {od_group, Group3}
        ])},
        {AtmInventory2, Group2, [User1], ordsets:from_list([
            {od_atm_inventory, ?SELF_INTERMEDIARY}
        ])},
        {AtmInventory2, Group3, [User1], ordsets:from_list([
            {od_atm_inventory, ?SELF_INTERMEDIARY}
        ])},

        {AtmInventory3, UserGroup, [User1], ordsets:from_list([
            {od_group, Group3}
        ])},
        {AtmInventory3, Group1, [User1], ordsets:from_list([
            {od_group, Group3}
        ])},
        {AtmInventory3, Group3, [User1], ordsets:from_list([
            {od_atm_inventory, ?SELF_INTERMEDIARY}
        ])},
        {AtmInventory3, Group4, [User1, User2], ordsets:from_list([
            {od_atm_inventory, ?SELF_INTERMEDIARY}
        ])}
    ],

    lists:foreach(fun({AtmInventoryId, GroupId, CorrectUsers, ExpIntermediariesRaw}) ->
        ExpIntermediaries = lists:map(fun({Type, Id}) ->
            #{<<"type">> => gri:serialize_type(Type), <<"id">> => Id}
        end, ExpIntermediariesRaw),
        CorrectUserClients = [{user, U} || U <- CorrectUsers],
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_ATM_INVENTORIES_VIEW]}
                ] ++ CorrectUserClients,
                unauthorized = [nobody],
                forbidden = [{user, NonAdmin}, {user, User1}, {user, User2}] -- CorrectUserClients
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/atm_inventories/">>, AtmInventoryId, <<"/effective_groups/">>, GroupId, <<"/membership">>],
                expected_code = ?HTTP_200_OK,
                expected_body = #{<<"intermediaries">> => ExpIntermediaries}
            },
            logic_spec = #logic_spec{
                module = atm_inventory_logic,
                function = get_eff_group_membership_intermediaries,
                args = [auth, AtmInventoryId, GroupId],
                expected_result = ?OK_LIST(ExpIntermediariesRaw)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, ExpectedMembershipIntermediaries).


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
