%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning
%%% handle service groups API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(hservice_groups_api_test_SUITE).
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
    add_group_test/1,
    remove_group_test/1,
    list_groups_test/1,
    get_group_test/1,

    get_group_privileges_test/1,
    update_group_privileges_test/1,

    list_eff_groups_test/1,
    get_eff_group_test/1,
    get_eff_group_privileges_test/1
]).

all() ->
    ?ALL([
        add_group_test,
        remove_group_test,
        list_groups_test,
        get_group_test,

        get_group_privileges_test,
        update_group_privileges_test,

        list_eff_groups_test,
        get_eff_group_test,
        get_eff_group_privileges_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


add_group_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_UPDATE privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_UPDATE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    AllPrivs = privileges:handle_service_privileges(),

    VerifyEndFun =
        fun
            (true = _ShouldSucceed, _, Data) ->
                ExpPrivs = lists:sort(maps:get(<<"privileges">>, Data)),
                {ok, Privs} = oz_test_utils:handle_service_get_group_privileges(
                    Config, HService, G1
                ),
                ?assertEqual(ExpPrivs, lists:sort(Privs)),
                oz_test_utils:handle_service_remove_group(
                    Config, HService, G1
                );
            (false = ShouldSucceed, _, _) ->
                {ok, Groups} = oz_test_utils:handle_service_get_groups(
                    Config, HService
                ),
                ?assertEqual(lists:member(G1, Groups), ShouldSucceed)
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_ADD_RELATIONSHIPS, ?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/handle_services/">>, HService, <<"/groups/">>, G1],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{?HDR_LOCATION := Location} = _Headers) ->
                ExpLocation = ?URL(Config,
                    [<<"/handle_services/">>, HService, <<"/groups/">>, G1]
                ),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = add_group,
            args = [auth, HService, G1, data],
            expected_result = ?OK_BINARY(G1)
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [?RAND_SUBLIST(privileges:handle_service_admin())]
            },
            bad_values = [
                {<<"privileges">>, <<"">>,
                    ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>)},
                {<<"privileges">>, [space_view, group_view],
                    ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"privileges">>, AllPrivs)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )).


remove_group_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_UPDATE privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_UPDATE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
        {ok, G1} = oz_test_utils:handle_service_add_group(
            Config, HService, G1
        ),
        #{groupId => G1}
    end,
    DeleteEntityFun = fun(#{groupId := Group} = _Env) ->
        oz_test_utils:handle_service_remove_group(Config, HService, Group)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, _) ->
        {ok, SubGroups} = oz_test_utils:handle_service_get_groups(
            Config, HService
        ),
        ?assertEqual(lists:member(GroupId, SubGroups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS]},
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
            path = [<<"/handle_services/">>, HService, <<"/groups/">>, groupId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = remove_group,
            args = [auth, HService, groupId],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_groups_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_VIEW privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ExpGroups = lists:map(
        fun(_) ->
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?ROOT, ?GROUP_NAME1
            ),
            oz_test_utils:handle_service_add_group(
                Config, HService, GroupId
            ),
            GroupId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS]},
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
            path = [<<"/handle_services/">>, HService, <<"/groups">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_groups,
            args = [auth, HService],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_group_test(Config) ->
    % create handle service with 3 users:
    %   UnprivilegedMember gets the HANDLE_SERVICE_VIEW privilege
    %   PrivilegedMember gets all remaining privileges
    %   UnprivilegedMemberFromTheGroup does not get the HANDLE_SERVICE_VIEW privilege, but belongs to the group
    {HServiceId, UnprivilegedMember, PrivilegedMember} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    GroupData = #{<<"name">> => ?GROUP_NAME1, <<"type">> => ?GROUP_TYPE1},
    {ok, GroupId} = oz_test_utils:create_group(Config, ?ROOT, GroupData),
    oz_test_utils:handle_service_add_group(Config, HServiceId, GroupId),

    UnprivilegedMemberFromTheGroup = ozt_users:create(),
    ozt_handle_services:add_user(
        HServiceId, UnprivilegedMemberFromTheGroup, ?RAND_SUBLIST(privileges:handle_service_admin() -- [?HANDLE_SERVICE_VIEW])
    ),
    ozt_groups:add_user(GroupId, UnprivilegedMemberFromTheGroup, ?RAND_SUBLIST(privileges:group_admin())),
    ozt:reconcile_entity_graph(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_VIEW]},
                {user, PrivilegedMember},
                {user, UnprivilegedMemberFromTheGroup}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UnprivilegedMember}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/handle_services/">>, HServiceId, <<"/groups/">>, GroupId],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:shared_group(rest, GroupId, GroupData)
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_group,
            args = [auth, HServiceId, GroupId],
            expected_result = api_test_expect:shared_group(logic, GroupId, GroupData)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = GroupId, aspect = instance, scope = shared
            },
            auth_hint = ?THROUGH_HANDLE_SERVICE(HServiceId),
            expected_result_op = api_test_expect:shared_group(gs, GroupId, GroupData)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_group_privileges_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_VIEW privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U3), ?GROUP_NAME1),
    {ok, G1} = oz_test_utils:handle_service_add_group(Config, HService, G1),

    AllPrivs = privileges:handle_service_privileges(),
    InitialPrivs = privileges:handle_service_member(),
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:handle_service_set_group_privileges(
            Config, HService, G1, PrivsToGrant, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_VIEW_PRIVILEGES]},
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
            path = [
                <<"/handle_services/">>, HService,
                <<"/groups/">>, G1, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_group_privileges,
            args = [auth, HService, G1],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?HANDLE_SERVICE_VIEW
    ])).


update_group_privileges_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_UPDATE privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_UPDATE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U3), ?GROUP_NAME1),
    {ok, G1} = oz_test_utils:handle_service_add_group(Config, HService, G1),

    AllPrivs = privileges:handle_service_privileges(),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:handle_service_set_group_privileges(
            Config, HService, G1, PrivsToGrant, PrivsToRevoke
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:handle_service_get_group_privileges(
            Config, HService, G1
        ),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_SET_PRIVILEGES]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [
                <<"/handle_services/">>, HService,
                <<"/groups/">>, G1, <<"/privileges">>
            ],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = update_group_privileges,
            args = [auth, HService, G1, data],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?HANDLE_SERVICE_UPDATE
    ])).


list_eff_groups_test(Config) ->
    {HService,
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}, {G6, _}],
        _EffUsers, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_hservice_eff_users_env(Config),

    ExpGroups = [G1, G2, G3, G4, G5, G6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/handle_services/">>, HService, <<"/effective_groups">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_eff_groups,
            args = [auth, HService],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:has_eff_group function
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:call_oz(
                Config, handle_service_logic, has_eff_group, [
                    HService, GroupId
                ])
            )
        end, ExpGroups
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, handle_service_logic, has_eff_group, [
            HService, <<"asdiucyaie827346w">>
        ]
    )).


get_eff_group_test(Config) ->
    {
        HServiceId, EffGroups, _EffUsers, {UnprivilegedMember, PrivilegedMember, NonAdmin}
    } = api_test_scenarios:create_hservice_eff_users_env(Config),

    UnprivilegedMemberFromTheGroup = ozt_users:create(),
    ozt_handle_services:add_user(
        HServiceId, UnprivilegedMemberFromTheGroup, ?RAND_SUBLIST(privileges:handle_service_admin() -- [?HANDLE_SERVICE_VIEW])
    ),
    lists:foreach(fun({GroupId, _}) ->
        ozt_groups:add_user(GroupId, UnprivilegedMemberFromTheGroup, ?RAND_SUBLIST(privileges:group_admin()))
    end, EffGroups),
    ozt:reconcile_entity_graph(),

    lists:foreach(fun({GroupId, GroupData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_GROUPS_VIEW]},
                    {user, PrivilegedMember},
                    {user, UnprivilegedMemberFromTheGroup}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, UnprivilegedMember},
                    {user, NonAdmin}
                ]
            },
            rest_spec = #rest_spec{
                method = get,
                path = [
                    <<"/handle_services/">>, HServiceId,
                    <<"/effective_groups/">>, GroupId
                ],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:shared_group(rest, GroupId, GroupData)
            },
            logic_spec = #logic_spec{
                module = handle_service_logic,
                function = get_eff_group,
                args = [auth, HServiceId, GroupId],
                expected_result = api_test_expect:shared_group(logic, GroupId, GroupData)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_group, id = GroupId,
                    aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_HANDLE_SERVICE(HServiceId),
                expected_result_op = api_test_expect:shared_group(gs, GroupId, GroupData)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffGroups).


get_eff_group_privileges_test(Config) ->
    %% Create environment with the following relations:
    %%
    %%               HandleService
    %%              /     ||      \
    %%             /      ||       \
    %%  [~hservice_view]  ||    [hservice_view]
    %%           /        ||         \
    %%        User1      /  \       User2
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

    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_VIEW privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose eff privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G4} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:handle_service_add_group(Config, HService, G1),
    {ok, G2} = oz_test_utils:handle_service_add_group(Config, HService, G2),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),
    {ok, G4} = oz_test_utils:group_add_group(Config, G2, G4),
    {ok, G4} = oz_test_utils:group_add_group(Config, G3, G4),
    {ok, U3} = oz_test_utils:group_add_user(Config, G4, U3),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:handle_service_privileges(),
    InitialPrivs = privileges:handle_service_member(),
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

        oz_test_utils:handle_service_set_group_privileges(
            Config, HService, G1, PrivsToGrant1, PrivsToRevoke
        ),
        oz_test_utils:handle_service_set_group_privileges(
            Config, HService, G2, PrivsToGrant2, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_VIEW_PRIVILEGES]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [
                <<"/handle_services/">>, HService,
                <<"/effective_groups/">>, G4, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_eff_group_privileges,
            args = [auth, HService, G4],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?HANDLE_SERVICE_VIEW
    ])).


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
