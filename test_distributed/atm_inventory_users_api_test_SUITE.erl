%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning atm_inventory users API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(atm_inventory_users_api_test_SUITE).
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

-define(OZ_NODES(Config), ?config(oz_worker_nodes, Config)).

-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    add_user_test/1,
    add_user_with_privileges_test/1,
    remove_user_test/1,
    list_users_test/1,
    get_user_test/1,

    get_user_privileges_test/1,
    update_user_privileges_test/1,

    list_eff_users_test/1,
    get_eff_user_test/1,
    get_eff_user_privileges_test/1,
    get_eff_user_membership_intermediaries/1
]).

all() ->
    ?ALL([
        add_user_test,
        add_user_with_privileges_test,
        remove_user_test,
        list_users_test,
        get_user_test,

        get_user_privileges_test,
        update_user_privileges_test,

        list_eff_users_test,
        get_eff_user_test,
        get_eff_user_privileges_test,
        get_eff_user_membership_intermediaries
    ]).

-define(ALL_PRIVS, privileges:atm_inventory_privileges()).

%%%===================================================================
%%% Test functions
%%%===================================================================

add_user_test(Config) ->
    Creator = ozt_users:create(),
    EffectiveUser = ozt_users:create(),
    EffectiveUserWithoutAddUserPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    AtmInventory = ozt_users:create_atm_inventory_for(Creator),

    % EffectiveUser belongs to the atm_inventory effectively via SubGroup1, with the
    % effective privilege to ADD_USER, so he should be able to add himself to the atm_inventory
    SubGroup1 = ozt_users:create_group_for(EffectiveUser),
    ozt_atm_inventories:add_group(AtmInventory, SubGroup1, [?ATM_INVENTORY_ADD_USER]),

    % EffectiveUserWithoutAddUserPriv belongs to the atm_inventory effectively via SubGroup2,
    % but without the effective privilege to ADD_USER, so he should NOT be able
    % to add himself to the atm_inventory
    SubGroup2 = ozt_users:create_group_for(EffectiveUserWithoutAddUserPriv),
    ozt_atm_inventories:add_group(AtmInventory, SubGroup2, []),

    lists:foreach(fun({ClientClassification, SubjectUser}) ->
        VerifyEndFun = fun
            (true = _ShouldSucceed, _, _) ->
                ?assert(lists:member(SubjectUser, ozt_atm_inventories:get_users(AtmInventory))),
                ozt_atm_inventories:remove_user(AtmInventory, SubjectUser);
            (false = _ShouldSucceed, _, _) ->
                ?assertNot(lists:member(SubjectUser, ozt_atm_inventories:get_users(AtmInventory)))
        end,

        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = lists:flatten([
                    root,
                    {admin, [?OZ_ATM_INVENTORIES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS]},
                    case ClientClassification of
                        correct -> {user, SubjectUser};
                        forbidden -> []
                    end
                ]),
                unauthorized = [nobody],
                forbidden = lists:flatten([
                    {user, Creator},
                    {user, NonAdmin},
                    case ClientClassification of
                        correct -> [];
                        forbidden -> {user, SubjectUser}
                    end
                ])
            },
            rest_spec = #rest_spec{
                method = put,
                path = [<<"/atm_inventories/">>, AtmInventory, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_201_CREATED,
                expected_headers = fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    ExpLocation = ?URL(Config, [<<"/atm_inventories/">>, AtmInventory, <<"/users/">>, SubjectUser]),
                    ?assertEqual(ExpLocation, Location),
                    true
                end
            },
            logic_spec = #logic_spec{
                module = atm_inventory_logic,
                function = add_user,
                args = [auth, AtmInventory, SubjectUser, data],
                expected_result = ?OK_BINARY(SubjectUser)
            }
            % TODO VFS-4520 Tests for GraphSync API
        },
        ?assert(api_test_utils:run_tests(
            Config, ApiTestSpec, undefined, undefined, VerifyEndFun
        ))
    end, [{correct, EffectiveUser}, {forbidden, EffectiveUserWithoutAddUserPriv}]).


add_user_with_privileges_test(Config) ->
    Creator = ozt_users:create(),
    EffectiveUser = ozt_users:create(),
    EffectiveUserWithoutAddUserPriv = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    AtmInventory = ozt_users:create_atm_inventory_for(Creator),

    % EffectiveUser belongs to the atm_inventory effectively via SubGroup1, with the
    % effective privilege to ADD_USER, so he should be able to add himself to the atm_inventory
    SubGroup1 = ozt_users:create_group_for(EffectiveUser),
    ozt_atm_inventories:add_group(AtmInventory, SubGroup1, [?ATM_INVENTORY_ADD_USER, ?ATM_INVENTORY_SET_PRIVILEGES]),

    % EffectiveUserWithoutAddUserPriv belongs to the atm_inventory effectively via SubGroup2,
    % but without the effective privilege to ADD_USER, so he should NOT be able
    % to add himself to the atm_inventory
    SubGroup2 = ozt_users:create_group_for(EffectiveUserWithoutAddUserPriv),
    ozt_atm_inventories:add_group(AtmInventory, SubGroup2, []),

    lists:foreach(fun({ClientClassification, SubjectUser}) ->
        VerifyEndFun = fun
            (true = _ShouldSucceed, _, Data) ->
                ExpPrivs = maps:get(<<"privileges">>, Data),
                ActualPrivs = ozt_atm_inventories:get_user_privileges(AtmInventory, SubjectUser),
                ?assertEqual(lists:sort(ExpPrivs), lists:sort(ActualPrivs)),
                ozt_atm_inventories:remove_user(AtmInventory, SubjectUser);
            (false = ShouldSucceed, _, _) ->
                Users = ozt_atm_inventories:get_users(AtmInventory),
                ?assertEqual(lists:member(SubjectUser, Users), ShouldSucceed)
        end,

        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = lists:flatten([
                    root,
                    {admin, [?OZ_ATM_INVENTORIES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS, ?OZ_ATM_INVENTORIES_SET_PRIVILEGES]},
                    case ClientClassification of
                        correct -> {user, SubjectUser};
                        forbidden -> []
                    end
                ]),
                unauthorized = [nobody],
                forbidden = lists:flatten([
                    {user, Creator},
                    {user, NonAdmin},
                    case ClientClassification of
                        correct -> [];
                        forbidden -> {user, SubjectUser}
                    end
                ])
            },
            rest_spec = #rest_spec{
                method = put,
                path = [<<"/atm_inventories/">>, AtmInventory, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_201_CREATED,
                expected_headers = fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    ExpLocation = ?URL(Config, [<<"/atm_inventories/">>, AtmInventory, <<"/users/">>, SubjectUser]),
                    ?assertEqual(ExpLocation, Location),
                    true
                end
            },
            logic_spec = #logic_spec{
                module = atm_inventory_logic,
                function = add_user,
                args = [auth, AtmInventory, SubjectUser, data],
                expected_result = ?OK_BINARY(SubjectUser)
            },
            % TODO VFS-4520 Tests for GraphSync API
            data_spec = #data_spec{
                required = [<<"privileges">>],
                correct_values = #{
                    <<"privileges">> => [
                        [?ATM_INVENTORY_UPDATE],
                        [?ATM_INVENTORY_VIEW]
                    ]
                },
                bad_values = [
                    {<<"privileges">>, <<"">>,
                        ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>)},
                    {<<"privileges">>, [?ATM_INVENTORY_VIEW, ?GROUP_VIEW],
                        ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"privileges">>, ?ALL_PRIVS)}
                ]
            }
        },
        ?assert(api_test_utils:run_tests(
            Config, ApiTestSpec, undefined, undefined, VerifyEndFun
        ))
    end, [{correct, EffectiveUser}, {forbidden, EffectiveUserWithoutAddUserPriv}]).


remove_user_test(Config) ->
    % create atm_inventory with 2 users:
    %   UserWithPrivilege gets the ATM_INVENTORY_REMOVE_USER privilege
    %   UserWithoutPrivilege gets all remaining privileges
    {AtmInventory, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_REMOVE_USER
    ),
    NonAdmin = ozt_users:create(),

    EnvSetUpFun = fun() ->
        SubjectUser = ozt_users:create(),
        ozt_atm_inventories:add_user(AtmInventory, SubjectUser),
        #{subjectUser => SubjectUser}
    end,
    DeleteEntityFun = fun(#{subjectUser := SubjectUser} = _Env) ->
        ozt_atm_inventories:remove_user(AtmInventory, SubjectUser)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{subjectUser := SubjectUser} = _Env, _) ->
        Users = ozt_atm_inventories:get_users(AtmInventory),
        ?assertEqual(lists:member(SubjectUser, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS]},
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
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/users/">>, subjectUser],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = remove_user,
            args = [auth, AtmInventory, subjectUser],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_users_test(Config) ->
    % create atm_inventory with 2 users:
    %   UserWithPrivilege gets the ATM_INVENTORY_VIEW privilege
    %   UserWithoutPrivilege gets all remaining privileges
    {AtmInventory, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_VIEW
    ),
    AnotherMember = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    ozt_atm_inventories:add_user(AtmInventory, AnotherMember),
    ExpUsers = [UserWithoutPrivilege, UserWithPrivilege, AnotherMember],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_LIST_RELATIONSHIPS]},
                {user, UserWithPrivilege},
                {user, AnotherMember}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutPrivilege}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_users,
            args = [auth, AtmInventory],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_user_test(Config) ->
    CreatorData = #{<<"fullName">> => <<"creator">>, <<"username">> => <<"creator">>},
    MemberData = #{<<"fullName">> => <<"member">>, <<"username">> => <<"member">>},

    Creator = ozt_users:create(CreatorData),
    MemberWithViewPrivs = ozt_users:create(),
    MemberWithoutViewPrivs = ozt_users:create(),
    AnotherMember = ozt_users:create(MemberData),
    NonAdmin = ozt_users:create(),

    AtmInventory = ozt_users:create_atm_inventory_for(Creator),
    ozt_atm_inventories:add_user(AtmInventory, MemberWithViewPrivs, [?ATM_INVENTORY_VIEW]),
    ozt_atm_inventories:add_user(AtmInventory, MemberWithoutViewPrivs, privileges:atm_inventory_admin() -- [?ATM_INVENTORY_VIEW]),
    ozt_atm_inventories:add_user(AtmInventory, AnotherMember),

    % Shared data about creator should be available even if he is not longer in the atm_inventory
    ozt_atm_inventories:remove_user(AtmInventory, Creator),
    ozt:reconcile_entity_graph(),

    lists:foreach(fun({SubjectUser, UserData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_VIEW]},
                    {user, SubjectUser},
                    {user, MemberWithViewPrivs}
                ] ++ case SubjectUser of
                    % Every member of the atm_inventory should be able to see the creator details
                    Creator -> [{user, MemberWithoutViewPrivs}];
                    _ -> []
                end,
                unauthorized = [nobody],
                forbidden = [
                    {user, NonAdmin}
                ] ++ case SubjectUser of
                    Creator -> [];
                    _ -> [{user, MemberWithoutViewPrivs}]
                end
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/atm_inventories/">>, AtmInventory, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:shared_user(rest, SubjectUser, UserData)
            },
            logic_spec = #logic_spec{
                module = atm_inventory_logic,
                function = get_user,
                args = [auth, AtmInventory, SubjectUser],
                expected_result = api_test_expect:shared_user(logic, SubjectUser, UserData)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = SubjectUser, aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_ATM_INVENTORY(AtmInventory),
                expected_result = api_test_expect:shared_user(gs, SubjectUser, UserData)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, [{Creator, CreatorData}, {AnotherMember, MemberData}]).


get_user_privileges_test(Config) ->
    % create atm_inventory with 2 users:
    %   UserWithPrivilege gets the ATM_INVENTORY_VIEW_PRIVILEGES privilege
    %   UserWithoutPrivilege gets all remaining privileges
    {AtmInventory, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_VIEW_PRIVILEGES
    ),
    NonAdmin = ozt_users:create(),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get user privileges and sometimes not)
    SubjectUser = ozt_users:create(),
    ozt_atm_inventories:add_user(AtmInventory, SubjectUser),

    InitialPrivs = privileges:atm_inventory_member(),
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        CurrentPrivs = ozt_atm_inventories:get_user_privileges(AtmInventory, SubjectUser),
        ozt_atm_inventories:set_user_privileges(
            AtmInventory, SubjectUser, (CurrentPrivs ++ PrivsToGrant) -- PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_ATM_INVENTORIES_VIEW_PRIVILEGES]},
                {user, UserWithPrivilege},
                % user can always see his own privileges
                {user, SubjectUser}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutPrivilege},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/users/">>, SubjectUser, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_user_privileges,
            args = [auth, AtmInventory, SubjectUser],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, ?ALL_PRIVS, [],
        {user, SubjectUser}, ?ATM_INVENTORY_VIEW_PRIVILEGES, false, SubjectUser
    ])).


update_user_privileges_test(Config) ->
    % create atm_inventory with 2 users:
    %   UserWithPrivilege gets the ATM_INVENTORY_SET_PRIVILEGES privilege
    %   UserWithoutPrivilege gets all remaining privileges
    {AtmInventory, UserWithoutPrivilege, UserWithPrivilege} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_SET_PRIVILEGES
    ),
    NonAdmin = ozt_users:create(),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to update user privileges and sometimes not)
    SubjectUser = ozt_users:create(),
    ozt_atm_inventories:add_user(AtmInventory, SubjectUser),

    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        CurrentPrivs = ozt_atm_inventories:get_user_privileges(AtmInventory, SubjectUser),
        ozt_atm_inventories:set_user_privileges(
            AtmInventory, SubjectUser, (CurrentPrivs ++ PrivsToGrant) -- PrivsToRevoke
        )
    end,
    GetPrivsFun = fun() ->
        ozt_atm_inventories:get_user_privileges(AtmInventory, SubjectUser)
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
                {user, UserWithoutPrivilege},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/users/">>, SubjectUser, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = update_user_privileges,
            args = [auth, AtmInventory, SubjectUser, data],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, ?ALL_PRIVS,
        {user, SubjectUser}, ?ATM_INVENTORY_SET_PRIVILEGES
    ])).


list_eff_users_test(Config) ->
    {
        AtmInventory, _Groups, [{U3, _}, {U4, _}, {U5, _}, {U6, _}], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_atm_inventory_eff_users_env(Config),


    ExpUsers = [U1, U2, U3, U4, U5, U6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_ATM_INVENTORIES_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/atm_inventories/">>, AtmInventory, <<"/effective_users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_eff_users,
            args = [auth, AtmInventory],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check also atm_inventory_logic:has_eff_user function
    lists:foreach(fun(UserId) ->
        ?assert(ozt:rpc(atm_inventory_logic, has_eff_user, [AtmInventory, UserId]))
    end, ExpUsers),
    ?assert(not ozt:rpc(atm_inventory_logic, has_eff_user, [AtmInventory, <<"asdiucyaie827346w">>])).


get_eff_user_test(Config) ->
    {
        AtmInventory, _Groups, EffUsers, {UserWithoutView, UserWithView, NonAdmin}
    } = api_test_scenarios:create_atm_inventory_eff_users_env(Config),

    lists:foreach(fun({UserId, UserData}) ->

        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_VIEW]},
                    {user, UserWithView}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, NonAdmin},
                    {user, UserWithoutView}
                ]
            },
            rest_spec = #rest_spec{
                method = get,
                path = [
                    <<"/atm_inventories/">>, AtmInventory, <<"/effective_users/">>, UserId
                ],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:shared_user(rest, UserId, UserData)
            },
            logic_spec = #logic_spec{
                module = atm_inventory_logic,
                function = get_eff_user,
                args = [auth, AtmInventory, UserId],
                expected_result = api_test_expect:shared_user(logic, UserId, UserData)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = UserId,
                    aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_ATM_INVENTORY(AtmInventory),
                expected_result = api_test_expect:shared_user(gs, UserId, UserData)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffUsers).


get_eff_user_privileges_test(Config) ->
    %% Create environment with the following relations:
    %%
    %%                  AtmInventory
    %%                 /  ||    \
    %%                /   ||     \
    %%   [~atm_inv_view]  ||  [atm_inv_view]
    %%             /      ||       \
    %% UserWithoutView   /  \      UserWithView
    %%                  /    \
    %%                 /      \
    %%             Group1    Group2
    %%                |         |
    %%                |         |
    %%             Group3       |
    %%                  \       |
    %%                   \      |
    %%                 SubjectUser
    %%      <<user>>
    %%      NonAdmin

    % create atm_inventory with 2 users:
    %   UserWithView gets the ATM_INVENTORY_VIEW_PRIVILEGES privilege
    %   UserWithoutView gets all remaining privileges
    {AtmInventory, UserWithoutView, UserWithView} = api_test_scenarios:create_basic_atm_inventory_env(
        ?ATM_INVENTORY_VIEW_PRIVILEGES
    ),
    NonAdmin = ozt_users:create(),

    SubjectUser = ozt_users:create(),

    Group1 = ozt_groups:create(),
    Group2 = ozt_groups:create(),
    Group3 = ozt_groups:create(),

    ozt_atm_inventories:add_group(AtmInventory, Group1),
    ozt_atm_inventories:add_group(AtmInventory, Group2),
    ozt_groups:add_child(Group1, Group3),
    ozt_groups:add_user(Group3, SubjectUser),
    ozt_groups:add_user(Group2, SubjectUser),

    ozt:reconcile_entity_graph(),

    InitialPrivs = privileges:atm_inventory_member(),
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],

    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        % In case of GRANT, randomly split privileges into four
        % parts and update groups with the privileges. Group3 eff_privileges
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
                {user, UserWithView},
                % user can always see his own privileges
                {user, SubjectUser}
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
                <<"/atm_inventories/">>, AtmInventory, <<"/effective_users/">>, SubjectUser,
                <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = atm_inventory_logic,
            function = get_eff_user_privileges,
            args = [auth, AtmInventory, SubjectUser],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, ?ALL_PRIVS, [],
        {user, SubjectUser}, ?ATM_INVENTORY_VIEW_PRIVILEGES, false, SubjectUser
    ])).


get_eff_user_membership_intermediaries(Config) ->
    %% Create environment with the following relations:
    %%
    %% AtmInventory1  AtmInventory2  AtmInventory3
    %%       | |  \     /   |  \     /   \
    %%       | |   \   /    |   \   /     User2 (no view privs)
    %%       |  \   Group2  |   Group3
    %%       |   \   /      |   /  |
    %%        \   \ /       |  /   |
    %%         \  Group1----|-'    |
    %%          \     \     |     /
    %%           \     \    |    /
    %%            \     \   |   /
    %%             '------User1 (view privs)
    %%
    %%      <<user>>
    %%      NonAdmin

    UserWithView = ozt_users:create(),
    UserWithoutView = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    Group1 = ozt_users:create_group_for(UserWithView),
    Group2 = ozt_groups:create(),
    Group3 = ozt_users:create_group_for(UserWithView),

    AtmInventory1 = ozt_atm_inventories:create(),
    AtmInventory2 = ozt_users:create_atm_inventory_for(UserWithView),
    AtmInventory3 = ozt_atm_inventories:create(),

    ozt_atm_inventories:add_user(AtmInventory1, UserWithView),
    ozt_atm_inventories:add_user(AtmInventory3, UserWithoutView, privileges:atm_inventory_admin() -- [?ATM_INVENTORY_VIEW]),

    ozt_groups:add_child(Group2, Group1),
    ozt_groups:add_child(Group3, Group1),

    ozt_atm_inventories:add_group(AtmInventory3, Group3),
    ozt_atm_inventories:add_group(AtmInventory1, Group1),
    ozt_atm_inventories:add_group(AtmInventory1, Group2),
    ozt_atm_inventories:add_group(AtmInventory2, Group2),
    ozt_atm_inventories:add_group(AtmInventory2, Group3),

    ozt:reconcile_entity_graph(),

    % {AtmInventoryId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}
    ExpectedMembershipIntermediaries = [
        {AtmInventory1, UserWithView, [UserWithView], ordsets:from_list([
            {od_atm_inventory, ?SELF_INTERMEDIARY},
            {od_group, Group1},
            {od_group, Group2}
        ])},

        {AtmInventory2, UserWithView, [UserWithView], ordsets:from_list([
            {od_atm_inventory, ?SELF_INTERMEDIARY},
            {od_group, Group2},
            {od_group, Group3}
        ])},

        {AtmInventory3, UserWithView, [UserWithView], ordsets:from_list([
            {od_group, Group3}
        ])},
        {AtmInventory3, UserWithoutView, [UserWithView, UserWithoutView], ordsets:from_list([
            {od_atm_inventory, ?SELF_INTERMEDIARY}
        ])}
    ],

    lists:foreach(fun({AtmInventoryId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}) ->
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
                forbidden = [{user, NonAdmin}, {user, UserWithView}, {user, UserWithoutView}] -- CorrectUserClients
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/atm_inventories/">>, AtmInventoryId, <<"/effective_users/">>, SubjectUser, <<"/membership">>],
                expected_code = ?HTTP_200_OK,
                expected_body = #{<<"intermediaries">> => ExpIntermediaries}
            },
            logic_spec = #logic_spec{
                module = atm_inventory_logic,
                function = get_eff_user_membership_intermediaries,
                args = [auth, AtmInventoryId, SubjectUser],
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
