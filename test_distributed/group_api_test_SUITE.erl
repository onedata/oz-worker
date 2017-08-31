%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C): 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning groups API (REST + logic).
%%% @end
%%%-------------------------------------------------------------------
-module(group_api_test_SUITE).
-author("Bartosz Walkowicz").

-include("rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("cluster_worker/include/api_errors.hrl").

-include("api_test_utils.hrl").


%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    create_test/1,
    list_test/1,
    get_test/1,
    update_test/1,
    delete_test/1,
    get_oz_privileges_test/1,
    update_oz_privileges_test/1,
    delete_oz_privileges_test/1,
    get_eff_oz_privileges_test/1,

    list_users_test/1,
    create_user_invite_token_test/1,
    get_user_details_test/1,
    add_user_test/1,
    remove_user_test/1,
    list_user_privileges_test/1,
    set_user_privileges_test/1,
    list_eff_users_test/1,
    get_eff_user_details_test/1,
    list_eff_user_privileges_test/1,

    list_parents_test/1,
    join_group_test/1,
    leave_group_test/1,
    get_parent_details_test/1,
    list_eff_parents_test/1,
    get_eff_parent_details_test/1,

    list_children_test/1,
    create_group_invite_token_test/1,
    get_child_details_test/1,
    add_group_test/1,
    remove_group_test/1,
    list_child_privileges_test/1,
    set_child_privileges_test/1,
    get_eff_children_test/1,
    get_eff_child_details_test/1,
    get_eff_child_privileges_test/1,

    list_spaces_test/1,
    get_space_details_test/1,
    create_space_test/1,
    join_space_test/1,
    leave_space_test/1,
    list_eff_spaces_test/1,
    get_eff_space_details_test/1,

    list_eff_providers_test/1,
    get_eff_provider_details_test/1,

    list_handle_services_test/1,
    create_handle_service_test/1,
    get_handle_service_details_test/1,
    leave_handle_service_test/1,
    list_eff_handle_services_test/1,
    get_eff_handle_service_details_test/1,

    list_handles_test/1,
    create_handle_test/1,
    get_handle_details_test/1,
    leave_handle_test/1,
    list_eff_handles_test/1,
    get_eff_handle_details_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        update_test,
        delete_test,
        get_oz_privileges_test,
        update_oz_privileges_test,
        delete_oz_privileges_test,
        get_eff_oz_privileges_test,

        list_users_test,
        create_user_invite_token_test,
        get_user_details_test,
        add_user_test,
        remove_user_test,
        list_user_privileges_test,
        set_user_privileges_test,
        list_eff_users_test,
        get_eff_user_details_test,
        list_eff_user_privileges_test,

        list_parents_test,
        join_group_test,
        leave_group_test,
        get_parent_details_test,
        list_eff_parents_test,
        get_eff_parent_details_test,

        list_children_test,
        create_group_invite_token_test,
        get_child_details_test,
        add_group_test,
        remove_group_test,
        list_child_privileges_test,
        set_child_privileges_test,
        get_eff_children_test,
        get_eff_child_details_test,
        get_eff_child_privileges_test,

        list_spaces_test,
        get_space_details_test,
        create_space_test,
        join_space_test,
        leave_space_test,
        list_eff_spaces_test,
        get_eff_space_details_test,

        list_eff_providers_test,
        get_eff_provider_details_test,

        list_handle_services_test,
        create_handle_service_test,
        get_handle_service_details_test,
        remove_handle_service_test,
        list_eff_handle_services_test,
        get_eff_handle_service_details_test,

        list_handles_test,
        create_handle_test,
        get_handle_details_test,
        remove_handle_test,
        list_eff_handles_test,
        get_eff_handle_details_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


create_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    ExpName = <<"G1">>,

    VerifyFun =
        fun(GroupId, ExpType) ->
            {ok, Group} = oz_test_utils:get_group(Config, GroupId),
            ?assertEqual(ExpName, Group#od_group.name),
            ?assertEqual(ExpType, Group#od_group.type),
            true
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/groups">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(
                fun(_, DataSet) ->
                    ExpType = maps:get(<<"type">>, DataSet, role),
                    fun(#{<<"location">> := Location} = _Headers) ->
                        [GroupId] = binary:split(
                            Location, <<"/groups/">>, [trim_all]
                        ),
                        VerifyFun(GroupId, ExpType)
                    end
                end
            )
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create,
            args = [client, data],
            expected_result = ?OK_ENV(
                fun(_, DataSet) ->
                    ExpType = maps:get(<<"type">>, DataSet, role),
                    ?OK_TERM(fun(GroupId) -> VerifyFun(GroupId, ExpType) end)
                end
            )
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_group, aspect = instance},
            auth_hint = ?AS_USER(U1),
            expected_result = ?OK_ENV(
                fun(_, DataSet) ->
                    ExpType = maps:get(<<"type">>, DataSet, role),
                    ?OK_MAP_CONTAINS(
                        #{
                            <<"name">> => ExpName,
                            <<"type">> => atom_to_binary(ExpType, utf8),
                            <<"gri">> =>
                            fun(EncodedGri) ->
                                #gri{id = GroupId} = oz_test_utils:decode_gri(
                                    Config, EncodedGri
                                ),
                                VerifyFun(GroupId, ExpType)
                            end
                        }
                    )
                end
            )
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            optional = [<<"type">>],
            correct_values = #{
                <<"name">> => [ExpName],
                <<"type">> => [organization, unit, team, role]
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)},
                {<<"type">>, kingdom,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>,
                        [organization,unit,team,role])},
                {<<"type">>, 1234, ?ERROR_BAD_VALUE_ATOM(<<"type">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_test(Config) ->
    % Make sure that groups created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - Admin:
    %%               privileges: [oz_groups_list]
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%         - G2:
    %%             owner: U1
    %%         - G3:
    %%             owner: U1
    %%         - G4:
    %%             owner: U1
    %%         - G5:
    %%             owner: U1

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST
    ]),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U1), <<"G2">>),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U1), <<"G3">>),
    {ok, G4} = oz_test_utils:create_group(Config, ?USER(U1), <<"G4">>),
    {ok, G5} = oz_test_utils:create_group(Config, ?USER(U1), <<"G5">>),

    ExpGroups = [G1, G2, G3, G4, G5, <<"all_users">>, <<"admins">>],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/groups">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % additionally check exists/1 fun from group_logic only
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:does_group_exist(Config, GroupId))
        end, ExpGroups
    ).



get_test(Config) ->
    ExpName = <<"G1">>,
    ExpType = unit,
    ExpTypeBinary = atom_to_binary(ExpType, utf8),

    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - Admin:
    %%               privileges: [oz_groups_list]
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: User
    %%             users:
    %%                 - U2:
    %%                     privileges: [~group_view]

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST
    ]),

    {ok, G1} = oz_test_utils:create_group(
        Config, ?USER(U1), #{<<"name">> => ExpName, <<"type">> => ExpType}
    ),
    oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, revoke, [
        ?GROUP_VIEW
    ]),

    AllGroupPrivs = oz_test_utils:call_oz(
        Config, privileges, group_privileges, []
    ),
    AllGroupPrivsBinary = [atom_to_binary(Priv, utf8) || Priv <- AllGroupPrivs],

    % get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, Admin},
                {user, NonAdmin},
                {user, U2}
            ]
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get,
            args = [client, G1],
            expected_result = ?OK_TERM(
                fun(#od_group{
                    name = Name, type = Type,

                    oz_privileges = [], eff_oz_privileges = [],

                    parents = [], eff_parents = #{},
                    children = #{}, eff_children = #{},

                    users = Users,
                    spaces = [],
                    handle_services = [],
                    handles = [],

                    eff_users = EffUsers,
                    eff_spaces = #{}, eff_providers = #{},
                    eff_handle_services = #{}, eff_handles = #{}
                }) ->
                    ?assertEqual(ExpType, Type),
                    ?assertEqual(ExpName, Name),
                    ?assertEqual(Users, #{U1 => AllGroupPrivs, U2 => []}),
                    ?assertEqual(EffUsers, #{
                        U1 => {AllGroupPrivs, [{od_group, G1}]},
                        U2 => {[], [{od_group, G1}]}
                    })
            end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_group, id = G1, aspect = instance},
            expected_result = ?OK_MAP(#{
                <<"children">> => #{},
                <<"effectiveChildren">> => #{},
                <<"name">> => ExpName,
                <<"type">> => ExpTypeBinary,
                <<"parents">> => [],
                <<"spaces">> => [],
                <<"effectiveUsers">> => #{U1 => AllGroupPrivsBinary, U2 => []},
                <<"users">> => #{U1 => AllGroupPrivsBinary, U2 => []},
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = GroupId} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(G1, GroupId)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % get and check protected data
    GetProtectedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"groupId">> => G1,
                <<"name">> => ExpName,
                <<"type">> => ExpTypeBinary
            }
        },
        logic_spec = LogicSpec = #logic_spec{
            module = group_logic,
            function = get_protected_data,
            args = [client, G1],
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"type">> => ExpType
            })
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = G1, aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"type">> => ExpTypeBinary,
                <<"gri">> =>
                fun(EncodedGri) ->
                    #gri{id = GroupId} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(G1, GroupId)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)),

    % get and check shared data
    GetSharedDataApiTestSpec = GetProtectedDataApiTestSpec#api_test_spec{
        rest_spec = undefined,
        logic_spec = LogicSpec#logic_spec{
            function = get_shared_data
        },
        gs_spec = GsSpec#gs_spec{
            gri = #gri{
                type = od_group, id = G1, aspect = instance, scope = shared
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetSharedDataApiTestSpec)).


update_test(Config) ->
    % Create 3 users, 2 of them make members of group to create,
    % one with group_update privilege and other without.
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),

    % Create a group
    InitialGroupName = <<"G1">>,
    InitFun =
        fun() ->
            {ok, G1} = oz_test_utils:create_group(
                Config, ?USER(U1), InitialGroupName
            ),
            oz_test_utils:add_user_to_group(Config, G1, U2),
            oz_test_utils:group_set_user_privileges(
                Config, G1, U2, revoke, [?GROUP_UPDATE]
            ),
            #{groupId => G1}
        end,
    VerifyEndFun =
        fun
            (#{correctData := true, groupId := GroupId} = _Env, Data) ->
                {ok, Group} = oz_test_utils:get_group(Config, GroupId),
                ExpType = maps:get(<<"type">>, Data, role),
                ExpName = maps:get(<<"name">>, Data, InitialGroupName),
                ?assertEqual(ExpName, Group#od_group.name),
                ?assertEqual(ExpType, Group#od_group.type),
                ok;
            (_, _) ->
                ok
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/groups/">>, groupId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = update,
            args = [client, groupId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{
                type = od_group, id = groupId, aspect = instance
            },
            expected_result = ?OK
        },
        data_spec = #data_spec{
            at_least_one = [
                <<"name">>,
                <<"type">>
            ],
            correct_values = #{
                <<"name">> => [InitialGroupName],
                <<"type">> => [organization]
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)},
                {<<"type">>, kingdom,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>,
                        [organization,unit,team,role])},
                {<<"type">>, 1234, ?ERROR_BAD_VALUE_ATOM(<<"type">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, InitFun, VerifyEndFun)).


delete_test(Config) ->
    % Create 3 users, 2 of them make members of group to create,
    % one with group_delete privilege and other without.
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),

    GroupName = <<"G1">>,
    InitFun =
        fun() ->
            {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), GroupName),
            oz_test_utils:add_user_to_group(Config, G1, U2),
            oz_test_utils:group_set_user_privileges(Config, G1, U2, revoke, [
                ?GROUP_DELETE
            ]),
            #{groupId => G1}
        end,
    VerifyEndFun =
        fun
            (#{correctData := true, groupId := GroupId} = _Env, _) ->
                ?assertMatch(
                    ?ERROR_NOT_FOUND,
                    oz_test_utils:call_oz(
                        Config, group_logic, get, [?ROOT, GroupId]
                    )
                );
            (_, _) ->
                ok
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, groupId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = delete,
            args = [client, groupId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{
                type = od_group, id = groupId, aspect = instance
            },
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, InitFun, VerifyEndFun]
    )).


get_oz_privileges_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - Admin:
    %%               privileges: [oz_view_privileges]
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1

    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_VIEW_PRIVILEGES
    ]),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),

    % Get all possible privileges and define expected initial privileges ->
    % by default, new group should not have any oz privileges
    AllPrivs = oz_test_utils:call_oz(Config, privileges, oz_privileges, []),
    InitialPrivs = [],

    SetPrivsFun =
        fun(Operation, Privs) ->
            oz_test_utils:set_group_oz_privileges(Config, G1, Operation, Privs)
        end,

    % Do not include user in clients as his group privileges
    % will be changing, which means that he will be sometimes authorized,
    % sometimes not.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_oz_privileges,
            args = [client, G1],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs, [],
        {user, U1}, ?OZ_VIEW_PRIVILEGES
    ])).


update_oz_privileges_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - Admin:
    %%               privileges: [oz_set_privileges]
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_SET_PRIVILEGES
    ]),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),

    % Get all possible privileges and define expected initial privileges ->
    % by default, new group should not have any oz privileges
    AllPrivs = oz_test_utils:call_oz(Config, privileges, oz_privileges, []),

    SetPrivsFun =
        fun(Operation, Privs) ->
            oz_test_utils:set_group_oz_privileges(Config, G1, Operation, Privs)
        end,
    GetPrivsFun =
        fun() ->
            {ok, Privs} = oz_test_utils:get_group_oz_privileges(Config, G1),
            Privs
        end,

    % Do not include user in clients as his group privileges
    % will be changing, which means that he will be sometimes authorized,
    % sometimes not.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/groups/">>, G1, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = update_oz_privileges,
            args = [client, G1, data],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U1}, ?OZ_SET_PRIVILEGES
    ])).


delete_oz_privileges_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - Admin:
    %%               privileges: [oz_set_privileges]
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_SET_PRIVILEGES
    ]),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),

    % Get all possible privileges and define expected initial privileges ->
    % by default, new group should not have any oz privileges
    AllPrivs = oz_test_utils:call_oz(Config, privileges, oz_privileges, []),

    SetPrivsFun =
        fun(Operation, Privs) ->
            oz_test_utils:set_group_oz_privileges(Config, G1, Operation, Privs)
        end,
    GetPrivsFun =
        fun() ->
            {ok, Privs} = oz_test_utils:get_group_oz_privileges(Config, G1),
            Privs
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/privileges">>],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = delete_oz_privileges,
            args = [client, G1],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U1}, ?OZ_SET_PRIVILEGES
    ])).


get_eff_oz_privileges_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - Admin:
    %%               privileges: [oz_view_privileges]
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%         - G2:
    %%             subgroups: [G1]
    %%         - G3:
    %%             subgroups: [G2]

    % Create two users, grant one of them the privilege to view OZ privileges.
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_VIEW_PRIVILEGES
    ]),

    {G1, G2, G3} = oz_test_utils:create_3_nested_groups(Config, U1),

    AllPrivs = oz_test_utils:call_oz(Config, privileges, oz_privileges, []),
    InitialPrivs = [],

    SetPrivsFun =
        fun(Operation, Privs) ->
            % In case of SET and GRANT, randomly split privileges into four
            % parts and update groups with the privileges. G3 eff_privileges
            % should contain the sum of those. In case of revoke, the
            % privileges must be revoked for all 3 entities.
            PartitionScheme =
                case Operation of
                    revoke ->
                        [{G1, Privs}, {G2, Privs}, {G3, Privs}];
                    _ -> % Covers (set|grant)
                        Parts = lists:foldl(
                            fun(Privilege, AccMap) ->
                                Index = rand:uniform(3),
                                AccMap#{Index => [Privilege | maps:get(Index, AccMap)]}
                            end, #{1=>[], 2=>[], 3=>[]}, Privs),
                        [
                            {G1, maps:get(1, Parts)},
                            {G2, maps:get(2, Parts)},
                            {G3, maps:get(3, Parts)}
                        ]
                end,
            lists:foreach(
                fun({GroupId, Privileges}) ->
                    oz_test_utils:set_group_oz_privileges(
                        Config, GroupId, Operation, Privileges
                    )
                end, PartitionScheme
            )
        end,

    % Do not include U1 in clients as his group privileges
    % will be changing, which means that he will be sometimes authorized,
    % sometimes not.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/effective_privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_oz_privileges,
            args = [client, G1],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs, [],
        {user, U1}, ?OZ_VIEW_PRIVILEGES
    ])).


list_users_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - Admin:
    %%             privileges: [oz_groups_list_users]
    %%         - NonAdmin
    %%         - User1
    %%         - User2
    %%         - User3
    %%     groups:
    %%         - G1:
    %%             owner: User1
    %%             users:
    %%                 - User2:
    %%                     privileges: [group_view]
    %%                 - User3
    %%                     privileges: [~group_view]

    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST_USERS
    ]),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User1), <<"G1">>),
    {ok, User2} = oz_test_utils:add_user_to_group(Config, G1, User2),
    oz_test_utils:group_set_user_privileges(Config, G1, User2, set, [
        ?GROUP_VIEW
    ]),
    {ok, User3} = oz_test_utils:add_user_to_group(Config, G1, User3),
    oz_test_utils:group_set_user_privileges(Config, G1, User3, revoke, [
        ?GROUP_VIEW
    ]),

    ExpUsers = [User1, User2, User3],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {user, User1},
                {user, User2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, User3}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_users,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_user_invite_token_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - User1
    %%         - User2
    %%         - User3
    %%     groups:
    %%         - G1:
    %%             owner: User1
    %%             users:
    %%                 - User2:
    %%                     privileges: [group_invite_user]
    %%                 - User3

    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User3} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User1), <<"G1">>),
    {ok, User2} = oz_test_utils:add_user_to_group(Config, G1, User2),
    oz_test_utils:group_set_user_privileges(Config, G1, User2, set, [
        ?GROUP_INVITE_USER
    ]),
    {ok, User3} = oz_test_utils:add_user_to_group(Config, G1, User3),

    % we will keep all tokens generated until now in a process, we will query
    GeneratedTokens =
        fun Loop(Tokens) ->
            receive
                {Pid, Token} ->
                    case lists:member(Token, Tokens) of
                        false ->
                            Pid ! true,
                            Loop([Token | Tokens]);
                        true ->
                            Pid ! false,
                            Loop(Tokens)
                    end
            end
        end,
    TokensProc = spawn_link(fun() -> GeneratedTokens([]) end),

    VerifyFun =
        fun(Token) ->
            TokensProc ! {self(), Token},
            IsTokenUnique =
                receive
                    Response -> Response
                after 1000 ->
                    false
                end,
            ?assert(IsTokenUnique),
            true
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User1},
                {user, User2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User3}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/users/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body =
                fun(#{<<"token">> := Token}) ->
                    VerifyFun(Token)
                end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_user_invite_token,
            args = [client, G1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_user_details_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - Admin:
    %%             privileges: [oz_groups_list_users]
    %%         - NonAdmin
    %%         - User1
    %%         - User2
    %%         - User3
    %%     groups:
    %%         - G1:
    %%             owner: User1
    %%             users:
    %%                 - User2:
    %%                     privileges: [group_view]
    %%                 - User3:
    %%                     privileges: [~group_view]

    ExpAlias = ExpLogin = ExpName = <<"user1">>,
    ExpEmails = [<<"john.doe@uruk.com">>],
    ExpMap = #{
        <<"alias">> => ExpAlias,
        <<"login">> => ExpLogin,
        <<"name">> => ExpName
    },

    {ok, User1} = oz_test_utils:create_user(Config, #od_user{
        name = ExpName,
        login = ExpLogin,
        alias = ExpAlias,
        email_list = ExpEmails
    }),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST_USERS
    ]),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User1), <<"G1">>),
    {ok, User2} = oz_test_utils:add_user_to_group(Config, G1, User2),
    oz_test_utils:group_set_user_privileges(Config, G1, User2, set, [
        ?GROUP_VIEW
    ]),
    {ok, User3} = oz_test_utils:add_user_to_group(Config, G1, User3),
    oz_test_utils:group_set_user_privileges(Config, G1, User3, revoke, [
        ?GROUP_VIEW
    ]),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {user, User1},
                {user, User2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, User3}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/users/">>, User1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpMap#{<<"userId">> => User1}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_user,
            args = [client, G1, User1],
            expected_result = ?OK_MAP(ExpMap)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_user, id = User1, aspect = instance, scope = shared
            },
            auth_hint = ?THROUGH_GROUP(G1),
            expected_result = ?OK_MAP(#{
                <<"login">> => ExpLogin,
                <<"name">> => ExpName,
                <<"gri">> =>
                fun(EncodedGri) ->
                    #gri{id = UserId} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(User1, UserId)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


add_user_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - Admin:
    %%             privileges: [oz_groups_add_members]
    %%         - NonAdmin
    %%         - User1
    %%         - User2
    %%     groups:
    %%         - G1:
    %%             owner: User1

    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_ADD_MEMBERS
    ]),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User1), <<"G1">>),

    VerifyEndFun =
        fun
            (#{correctData := true} = _Env, Data) ->
                Privs = lists:sort(maps:get(<<"privileges">>, Data)),
                {ok, ActualPrivs} = oz_test_utils:get_group_user_privileges(
                    Config, G1, User2
                ),
                ?assertEqual(Privs, lists:sort(ActualPrivs)),
                oz_test_utils:group_remove_user(Config, G1, User2);
            (_, _) ->
                ok
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/groups/">>, G1, <<"/users/">>, User2],
            expected_code = ?HTTP_201_CREATED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = add_user,
            args = [client, G1, User2, data],
            expected_result = ?OK_BINARY(User2)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [group_join_group, group_remove_group],
                    [group_invite_user, group_view]
                ]
            },
            bad_values = [
                {<<"privileges">>, <<"">>,
                    ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>)}
            ]
        }
    },

    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, fun api_test_utils:init_env/0, VerifyEndFun
    )).


remove_user_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - Admin:
    %%             privileges: [oz_groups_remove_members]
    %%         - NonAdmin
    %%         - User1
    %%         - User2
    %%         - User3
    %%         - User4
    %%     groups:
    %%         - G1:
    %%             owner: User1
    %%             users:
    %%                 - User2:
    %%                     privileges: [group_remove_user]
    %%                 - User3

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U4} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_REMOVE_MEMBERS
    ]),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_REMOVE_USER
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_REMOVE_USER
    ]),

    InitFun =
        fun() ->
            {ok, U4} = oz_test_utils:add_user_to_group(Config, G1, U4),
            #{}
        end,
    VerifyEndFun =
        fun
            (#{correctData := true} = _Env, _) ->
                ?assertMatch(
                    ?ERROR_NOT_FOUND,
                    oz_test_utils:call_oz(
                        Config, group_logic, get_user, [?ROOT, G1, U4]
                    )
                );
            (_, _) ->
                oz_test_utils:group_remove_user(Config, G1, U4)
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U3},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/users/">>, U4],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = remove_user,
            args = [client, G1, U4],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, InitFun, VerifyEndFun]
    )).


list_user_privileges_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - NonAdmin
    %%         - U1
    %%         - U2
    %%         - U3
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3

    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),

    % Get all possible privileges and define expected initial privileges ->
    % by default, new group should not have any oz privileges
    AllPrivs = oz_test_utils:call_oz(Config, privileges, group_privileges, []),
    InitialPrivs = [group_view],

    SetPrivsFun =
        fun(Operation, Privs) ->
            oz_test_utils:group_set_user_privileges(
                Config, G1, U3, Operation, Privs
            )
        end,

    % Do not include user in clients as his group privileges
    % will be changing, which means that he will be sometimes authorized,
    % sometimes not.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/groups/">>, G1, <<"/users/">>, U3, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_user_privileges,
            args = [client, G1, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs,
        [], {user, U3}, ?GROUP_VIEW
    ])).


set_user_privileges_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - NonAdmin
    %%         - U1
    %%         - U2
    %%         - U3
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_set_privileges]
    %%                 - U3

    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_SET_PRIVILEGES
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),

    % Get all possible privileges and define expected initial privileges ->
    % by default, new group should not have any oz privileges
    AllPrivs = oz_test_utils:call_oz(Config, privileges, group_privileges, []),

    SetPrivsFun =
        fun(Operation, Privs) ->
            oz_test_utils:group_set_user_privileges(
                Config, G1, U3, Operation, Privs
            )
        end,
    GetPrivsFun =
        fun() ->
            {ok, Privs} = oz_test_utils:get_group_user_privileges(
                Config, G1, U3
            ),
            Privs
        end,

    % Do not include user in clients as his group privileges
    % will be changing, which means that he will be sometimes authorized,
    % sometimes not.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/groups/">>, G1, <<"/users/">>, U3, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = update_user_privileges,
            args = [client, G1, U3, data],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?GROUP_SET_PRIVILEGES
    ])).


list_eff_users_test(Config) ->
    {
        G1, [{U1, _}, {U2, _}, {U3, _}, {U4, _}, {U5, _}], NonAdmin
    } = create_eff_group_users_env(Config),

    ExpUsers = [U1, U2, U3, U4, U5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U3},
                {user, U4},
                {user, U5}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/effective_users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_users,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
    },

    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % additionally check has_eff_user/2 fun from group_logic only
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:does_group_exist(Config, GroupId))
        end, ExpUsers
    ).



get_eff_user_details_test(Config) ->
    {
        G1,
        [{U1, _}, {U2, _}, {U3, _}, {U4, _}, {U5, _}] = EffUsersList,
        NonAdmin
    } = create_eff_group_users_env(Config),

    lists:foreach(
        fun({UserId, UserDetails}) ->

            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = lists:usort([
                        root,
                        {user, U1},
                        {user, U4},
                        {user, U5},
                        {user, UserId}
                    ]),
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                        | lists:delete(
                            {user, UserId}, [{user, U2}, {user, U3}]
                        )
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/groups/">>, G1, <<"/effective_users/">>, UserId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = UserDetails#{<<"userId">> => UserId}
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_user,
                    args = [client, G1, UserId],
                    expected_result = ?OK_MAP(UserDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_user, id = UserId, aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_GROUP(G1),
                    expected_result = ?OK_MAP(
                        (maps:remove(<<"alias">>, UserDetails))#{
                            <<"gri">> =>
                            fun(EncodedGri) ->
                                #gri{id = UId} = oz_test_utils:decode_gri(
                                    Config, EncodedGri
                                ),
                                ?assertEqual(UId, UserId)
                            end
                    })
                }
            },

            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffUsersList
    ).


create_eff_group_users_env(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - U4
    %%         - U5
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             users:
    %%                 - U1:
    %%                     privileges: [group_view]
    %%                 - U2:
    %%                     privileges: [~group_view]
    %%             subgroups:
    %%                 - G2:
    %%                     privileges: [~group_view]
    %%                 - G3:
    %%                     privileges: [group_view]
    %%         - G2:
    %%             owner: U3
    %%         - G3:
    %%             users: [U4]
    %%             subgroups: [G4]
    %%         - G4:
    %%             owner: U5

    Users = [{U1, _}, {U2, _}, {U3, _}, {U4, _}, {U5, _}] = lists:foldr(
        fun(Idx, Acc) ->
            Alias = list_to_binary("user" ++ [Idx + 48]),
            UserDetails = #{
                <<"alias">> => Alias,
                <<"login">> => Alias,
                <<"name">> => Alias
            },
            {ok, UserId} = oz_test_utils:create_user(Config, #od_user{
                name = Alias,
                login = Alias,
                alias = Alias
            }),

            [{UserId, UserDetails} | Acc]
        end, [], lists:seq(1, 5)
    ),

    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {_G4, G3, G1} = oz_test_utils:create_3_nested_groups(Config, U5),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U3), <<"G2">>),
    {ok, G2} = oz_test_utils:add_group_to_group(Config, G1, G2),
    oz_test_utils:group_set_group_privileges(Config, G1, G2, revoke, [
        ?GROUP_VIEW
    ]),

    {ok, U1} = oz_test_utils:add_user_to_group(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, set, [
        ?GROUP_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, revoke, [
        ?GROUP_VIEW
    ]),

    {ok, U4} = oz_test_utils:add_user_to_group(Config, G3, U4),

    {G1, Users, NonAdmin}.


list_eff_user_privileges_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             users:
    %%                 - U1
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3:
    %%                     privileges: [~group_view]
    %%             subgroups: [G2, G4]
    %%         - G2:
    %%             subgroups: [G3]
    %%         - G3:
    %%             owner: U1
    %%         - G4:
    %%             users:
    %%                 - U1:
    %%                     privileges: [~group_view]

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {_G3, G2, G1} = oz_test_utils:create_3_nested_groups(Config, U1),
    {ok, G4} = oz_test_utils:create_group(Config, ?ROOT, <<"G4">>),
    {ok, G4} = oz_test_utils:add_group_to_group(Config, G1, G4),

    {ok, U1} = oz_test_utils:add_user_to_group(Config, G1, U1),
    {ok, U1} = oz_test_utils:add_user_to_group(Config, G4, U1),
    oz_test_utils:group_set_user_privileges(Config, G4, U1, revoke, [
        ?GROUP_VIEW
    ]),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_VIEW
    ]),

    AllPrivs = oz_test_utils:call_oz(Config, privileges, group_privileges, []),
    InitialPrivs = [group_view],

    SetPrivsFun =
        fun(Operation, Privs) ->
            % In case of SET and GRANT, randomly split privileges into four
            % parts and update groups with the privileges. G3 eff_privileges
            % should contain the sum of those. In case of revoke, the
            % privileges must be revoked for all 3 entities.
            {UserPrivs, PartitionScheme} =
                case Operation of
                    revoke ->
                        {Privs, [{G2, Privs}, {G4, Privs}]};
                    _ -> % Covers (set|grant)
                        Parts = lists:foldl(
                            fun(Privilege, AccMap) ->
                                Index = rand:uniform(3),
                                AccMap#{Index => [Privilege | maps:get(Index, AccMap)]}
                            end, #{1=>[], 2=>[], 3=>[]}, Privs),
                        {maps:get(1, Parts), [
                            {G2, maps:get(2, Parts)}, {G4, maps:get(3, Parts)}
                        ]}
                end,
            oz_test_utils:group_set_user_privileges(
                Config, G1, U1, Operation, UserPrivs
            ),
            lists:foreach(
                fun({GroupId, Privileges}) ->
                    oz_test_utils:group_set_group_privileges(
                        Config, G1, GroupId, Operation, Privileges
                    )
                end, PartitionScheme
            )
        end,

    % Do not include U1 in clients as his group privileges
    % will be changing, which means that he will be sometimes authorized,
    % sometimes not.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U3},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [
                <<"/groups/">>, G1, <<"/effective_users/">>, U1,
                <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_user_privileges,
            args = [client, G1, U1],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs, [],
        {user, U1}, ?GROUP_VIEW
    ])).


list_parents_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             subgroups: [G5]
    %%         - G2:
    %%             owner: U1
    %%             subgroups: [G5]
    %%         - G3:
    %%             owner: U1
    %%             subgroups: [G5]
    %%         - G4:
    %%             owner: U1
    %%             subgroups: [G5]
    %%         - G5:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3:
    %%                     privileges: [~group_view]

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U1), <<"G2">>),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U1), <<"G3">>),
    {ok, G4} = oz_test_utils:create_group(Config, ?USER(U1), <<"G4">>),
    {ok, G5} = oz_test_utils:create_group(Config, ?USER(U1), <<"G5">>),

    oz_test_utils:add_group_to_group(Config, G1, G5),
    oz_test_utils:add_group_to_group(Config, G2, G5),
    oz_test_utils:add_group_to_group(Config, G3, G5),
    oz_test_utils:add_group_to_group(Config, G4, G5),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G5, U2),
    oz_test_utils:group_set_user_privileges(Config, G5, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G5, U3),
    oz_test_utils:group_set_user_privileges(Config, G5, U3, revoke, [
        ?GROUP_VIEW
    ]),

    ExpGroups = [G1, G2, G3, G4],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G5, <<"/parents">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"parent_groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_parents,
            args = [client, G5],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


join_group_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%         - G2:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_join_group]
    %%                 - U3:
    %%                     privileges: [~group_join_group]

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U1), <<"G2">>),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G2, U2),
    oz_test_utils:group_set_user_privileges(Config, G2, U2, set, [
        ?GROUP_JOIN_GROUP
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G2, U3),
    oz_test_utils:group_set_user_privileges(Config, G2, U3, revoke, [
        ?GROUP_JOIN_GROUP
    ]),

    GenTokenFun =
        fun() ->
            {ok, Macaroon} = oz_test_utils:group_invite_group_token(
                Config, ?ROOT, G1
            ),
            {ok, Token} = token_utils:serialize62(Macaroon),
            Token
        end,
    EndFun =
        fun
            (#{correctData := true} = _Env, _) ->
                oz_test_utils:group_remove_group(Config, G1, G2);
            (_, _) ->
                ok
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G2, <<"/parents/join">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers =
                fun(#{<<"location">> := Location} = _Headers) ->
                    [GroupId, ParentGroupId] = binary:split(
                        Location,
                        [<<"/groups/">>, <<"/nested/">>],
                        [trim_all, global]
                    ),
                    ?assertEqual(GroupId, G2),
                    ?assertEqual(ParentGroupId, G1),
                    true
                end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_group,
            args = [client, G2, data],
            expected_result = ?OK_BINARY(G1)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [GenTokenFun]
            },
            bad_values = [
                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, <<"123qwe">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, fun api_test_utils:init_env/0, EndFun
    )).


leave_group_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             subgroups: [G2]
    %%         - G2:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_leave_group]
    %%                 - U3:
    %%                     privileges: [~group_leave_group]

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U1), <<"G2">>),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G2, U2),
    oz_test_utils:group_set_user_privileges(Config, G2, U2, set, [
        % TODO VFS-3351 ?GROUP_LEAVE_GROUP
        ?GROUP_UPDATE
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G2, U3),
    oz_test_utils:group_set_user_privileges(Config, G2, U3, revoke, [
        % TODO VFS-3351 ?GROUP_LEAVE_GROUP
        ?GROUP_UPDATE
    ]),

    InitFun =
        fun() ->
            {ok, G2} = oz_test_utils:add_group_to_group(Config, G1, G2),
            #{}
        end,
    VerifyEndFun =
        fun
            (#{correctData := true} = _Env, _) ->
                ?assertMatch(
                    ?ERROR_NOT_FOUND,
                    oz_test_utils:call_oz(
                        Config, group_logic, remove_group, [?ROOT, G1, G2]
                    )
                );
            (_, _) ->
                oz_test_utils:group_remove_group(Config, G1, G2)
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G2, <<"/parents/">>, G1],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_group,
            args = [client, G2, G1],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, InitFun, VerifyEndFun]
    )).


get_parent_details_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             subgroups: [G2]
    %%         - G2:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3:
    %%                     privileges: [~group_view]

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    ExpName = <<"G1">>,
    ExpType = organization,
    {ok, G1} = oz_test_utils:create_group(
        Config, ?USER(U1), #{<<"name">> => ExpName, <<"type">> => ExpType}
    ),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U1), <<"G2">>),

    oz_test_utils:add_group_to_group(Config, G1, G2),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G2, U2),
    oz_test_utils:group_set_user_privileges(Config, G2, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G2, U3),
    oz_test_utils:group_set_user_privileges(Config, G2, U3, revoke, [
        ?GROUP_VIEW
    ]),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2},
                {user, U3}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G2, <<"/parents/">>, G1],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"groupId">> => G1,
                <<"name">> => ExpName,
                <<"type">> => atom_to_binary(ExpType, utf8)
            }
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_parent,
            args = [client, G2, G1],
            expected_result = ?OK_MAP(#{
                    <<"name">> => ExpName,
                    <<"type">> => ExpType
                })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = G1, aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_GROUP(G2),
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"type">> => atom_to_binary(ExpType, utf8),
                <<"gri">> =>
                fun(EncodedGri) ->
                    #gri{id = GroupId} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(G1, GroupId)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_eff_parents_test(Config) ->
    {
        G5, [{G1, _}, {G2, _}, {G3, _}, {G4, _}], {U1, U2, NonAdmin}
    } = create_eff_parent_groups_env(Config),

    ExpGroups = [G1, G2, G3, G4],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/groups/">>, G5, <<"/effective_parents">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"parents">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_parents,
            args = [client, G5],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_eff_parent_details_test(Config) ->
    {
        G5, EffParentsList, {U1, U2, NonAdmin}
    } = create_eff_parent_groups_env(Config),

    lists:foreach(
        fun({GroupId, GroupDetails}) ->
            ExpType = maps:get(<<"type">>, GroupDetails, role),

            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
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
                    path = [<<"/groups/">>, G5, <<"/effective_parents/">>, GroupId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = #{
                        <<"groupId">> => GroupId,
                        <<"name">> => maps:get(<<"name">>, GroupDetails),
                        <<"type">> => atom_to_binary(
                            maps:get(<<"type">>, GroupDetails), utf8
                        )
                    }
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_parent,
                    args = [client, G5, GroupId],
                    expected_result = ?OK_MAP(GroupDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_group, id = GroupId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_GROUP(G5),
                    expected_result = ?OK_MAP(
                        GroupDetails#{
                            <<"type">> => atom_to_binary(ExpType, utf8),
                            <<"gri">> =>
                            fun(EncodedGri) ->
                                #gri{id = Gid} = oz_test_utils:decode_gri(
                                    Config, EncodedGri
                                ),
                                ?assertEqual(Gid, GroupId)
                            end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffParentsList
    ).


create_eff_parent_groups_env(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             subgroups: [G2]
    %%         - G2:
    %%             subgroups: [G3]
    %%         - G3:
    %%             subgroups: [G5]
    %%         - G4:
    %%             subgroups: [G5]
    %%         - G5:
    %%             users:
    %%                 - U1:
    %%                     privileges: [group_view]
    %%                 - U2:
    %%                     privileges: [~group_view]

    Groups = [{G1, _}, {G2, _}, {G3, _}, {G4, _}] = lists:foldr(
        fun(Idx, Acc) ->
            GroupDetails = #{
                <<"name">> => erlang:iolist_to_binary(
                    [<<"G">>, integer_to_binary(Idx)]
                ),
                <<"type">> => lists:nth(
                    rand:uniform(4), [unit, organization, team, role]
                )
            },
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?ROOT, GroupDetails
            ),
            [{GroupId, GroupDetails} | Acc]
        end, [], lists:seq(1, 4)
    ),

    {ok, G2} = oz_test_utils:add_group_to_group(Config, G1, G2),
    {ok, G3} = oz_test_utils:add_group_to_group(Config, G2, G3),

    {ok, G5} = oz_test_utils:create_group(Config, ?ROOT, <<"G5">>),
    {ok, G5} = oz_test_utils:add_group_to_group(Config, G3, G5),
    {ok, G5} = oz_test_utils:add_group_to_group(Config, G4, G5),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, U1} = oz_test_utils:add_user_to_group(Config, G5, U1),
    oz_test_utils:group_set_user_privileges(Config, G5, U1, set, [
        ?GROUP_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G5, U2),
    oz_test_utils:group_set_user_privileges(Config, G5, U2, revoke, [
        ?GROUP_VIEW
    ]),

    {G5, Groups, {U1, U2, NonAdmin}}.


list_children_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%         - G2:
    %%             owner: U1
    %%         - G3:
    %%             owner: U1
    %%         - G4:
    %%             owner: U1
    %%         - G5:
    %%             owner: U1
    %%             subgroups: [G1, G2, G3, G4]
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3:
    %%                     privileges: [~group_view]

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U1), <<"G2">>),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U1), <<"G3">>),
    {ok, G4} = oz_test_utils:create_group(Config, ?USER(U1), <<"G4">>),
    {ok, G5} = oz_test_utils:create_group(Config, ?USER(U1), <<"G5">>),

    oz_test_utils:add_group_to_group(Config, G5, G1),
    oz_test_utils:add_group_to_group(Config, G5, G2),
    oz_test_utils:add_group_to_group(Config, G5, G3),
    oz_test_utils:add_group_to_group(Config, G5, G4),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G5, U2),
    oz_test_utils:group_set_user_privileges(Config, G5, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G5, U3),
    oz_test_utils:group_set_user_privileges(Config, G5, U3, revoke, [
        ?GROUP_VIEW
    ]),

    ExpGroups = [G1, G2, G3, G4],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G5, <<"/children">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"nested_groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_children,
            args = [client, G5],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_group_invite_token_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - User1
    %%         - User2
    %%         - User3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: User1
    %%             users:
    %%                 - User2:
    %%                     privileges: [group_invite_group]
    %%                 - User3
    %%                     privileges: [~group_invite_group]

    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User1), <<"G1">>),
    {ok, User2} = oz_test_utils:add_user_to_group(Config, G1, User2),
    oz_test_utils:group_set_user_privileges(Config, G1, User2, set, [
        ?GROUP_INVITE_GROUP
    ]),
    {ok, User3} = oz_test_utils:add_user_to_group(Config, G1, User3),
    oz_test_utils:group_set_user_privileges(Config, G1, User3, revoke, [
        ?GROUP_INVITE_GROUP
    ]),

    % we will keep all tokens generated until now in a process, we will query
    GeneratedTokens =
        fun Loop(Tokens) ->
            receive
                {Pid, Token} ->
                    case lists:member(Token, Tokens) of
                        false ->
                            Pid ! true,
                            Loop([Token | Tokens]);
                        true ->
                            Pid ! false,
                            Loop(Tokens)
                    end
            end
        end,
    TokensProc = spawn_link(fun() -> GeneratedTokens([]) end),
    VerifyFun =
        fun(Token) ->
            TokensProc ! {self(), Token},
            IsTokenUnique =
                receive
                    Response -> Response
                after 1000 ->
                    false
                end,
            ?assert(IsTokenUnique),
            true
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User1},
                {user, User2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User3},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/children/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body =
            fun(#{<<"token">> := Token}) ->
                VerifyFun(Token)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_group_invite_token,
            args = [client, G1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_child_details_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             subgroups: [G2]
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3:
    %%                     privileges: [~group_view]
    %%         - G2:
    %%             owner: U1

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    ExpName = <<"G2">>,
    ExpType = team,
    ExpTypeBinary = atom_to_binary(ExpType, utf8),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, G2} = oz_test_utils:create_group(
        Config, ?USER(U1), #{<<"name">> => ExpName, <<"type">> => ExpType}
    ),

    oz_test_utils:add_group_to_group(Config, G1, G2),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_VIEW
    ]),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/children/">>, G2],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"groupId">> => G2,
                <<"name">> => ExpName,
                <<"type">> => ExpTypeBinary
            }
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_child,
            args = [client, G1, G2],
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"type">> => ExpType
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = G2, aspect = instance, scope = shared
            },
            auth_hint = ?THROUGH_GROUP(G1),
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"type">> => ExpTypeBinary,
                <<"gri">> =>
                fun(EncodedGri) ->
                    #gri{id = Gid} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Gid, G2)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


add_group_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - Admin:
    %%             privileges: [oz_groups_add_members]
    %%         - NonAdmin
    %%         - User1
    %%     groups:
    %%         - G1:
    %%             owner: User1
    %%         - G2:
    %%             owner: User1

    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_ADD_MEMBERS
    ]),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User1), <<"G1">>),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(User1), <<"G2">>),

    VerifyEndFun =
        fun
            (#{correctData := true} = _Env, Data) ->
                Privs = lists:sort(maps:get(<<"privileges">>, Data)),
                {ok, ActualPrivs} = oz_test_utils:get_group_subgroup_privileges(
                    Config, G1, G2
                ),
                ?assertEqual(Privs, lists:sort(ActualPrivs)),
                oz_test_utils:group_remove_group(Config, G1, G2);
            (_, _) ->
                ok
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/groups/">>, G1, <<"/children/">>, G2],
            expected_code = ?HTTP_201_CREATED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = add_group,
            args = [client, G1, G2, data],
            expected_result = ?OK_BINARY(G2)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [group_join_group, group_remove_group],
                    [group_invite_user, group_view]
                ]
            },
            bad_values = [
                {<<"privileges">>, <<"">>,
                    ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>)}
            ]
        }
    },

    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, fun api_test_utils:init_env/0, VerifyEndFun
    )).


remove_group_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - Admin:
    %%             privileges: [oz_groups_remove_members]
    %%         - NonAdmin
    %%         - User1
    %%         - User2
    %%         - User3
    %%     groups:
    %%         - G1:
    %%             owner: User1
    %%             subgroups:
    %%                 - G2
    %%             users:
    %%                 - User2:
    %%                     privileges: [group_remove_group]
    %%                 - User3
    %%                     privileges: [~group_remove_group]
    %%         - G2:
    %%             owner: User1

    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_REMOVE_MEMBERS
    ]),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User1), <<"G1">>),
    {ok, User2} = oz_test_utils:add_user_to_group(Config, G1, User2),
    oz_test_utils:group_set_user_privileges(Config, G1, User2, set, [
        ?GROUP_REMOVE_GROUP
    ]),
    {ok, User3} = oz_test_utils:add_user_to_group(Config, G1, User3),
    oz_test_utils:group_set_user_privileges(Config, G1, User3, revoke, [
        ?GROUP_REMOVE_GROUP
    ]),

    {ok, G2} = oz_test_utils:create_group(Config, ?USER(User1), <<"G2">>),

    InitFun =
        fun() ->
            {ok, G2} = oz_test_utils:add_group_to_group(Config, G1, G2),
            #{}
        end,
    VerifyEndFun =
        fun
            (#{correctData := true} = _Env, _) ->
                ?assertMatch(
                    ?ERROR_NOT_FOUND,
                    oz_test_utils:call_oz(
                        Config, group_logic, get_child, [?ROOT, G1, G2]
                    )
                );
            (_, _) ->
                oz_test_utils:group_remove_group(Config, G1, G2)
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {user, User1},
                {user, User2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User3},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/children/">>, G2],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = remove_group,
            args = [client, G1, G2],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, InitFun, VerifyEndFun]
    )).


list_child_privileges_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - User
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - U4
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             subgroups:
    %%                 - G2
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3
    %%                     privileges: [~group_view]
    %%         - G2:
    %%             owner: U4

    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U4} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U4), <<"G2">>),
    {ok, G2} = oz_test_utils:add_group_to_group(Config, G1, G2),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_VIEW
    ]),

    % Get all possible privileges and define expected initial privileges ->
    % by default, new group should not have any oz privileges
    AllPrivs = oz_test_utils:call_oz(Config, privileges, group_privileges, []),
    InitialPrivs = [group_view],

    SetPrivsFun =
        fun(Operation, Privs) ->
            oz_test_utils:group_set_group_privileges(
                Config, G1, G2, Operation, Privs
            )
        end,

    % Do not include user in clients as his group privileges
    % will be changing, which means that he will be sometimes authorized,
    % sometimes not.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/children/">>, G2, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_child_privileges,
            args = [client, G1, G2],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs,
        [], {user, U4}, ?GROUP_VIEW
    ])).


set_child_privileges_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - User
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - U4
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             subgroups:
    %%                 - G2
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_set_privileges]
    %%                 - U3
    %%                     privileges: [~group_set_privileges]
    %%         - G2:
    %%             owner: U4

    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U4} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U4), <<"G2">>),
    {ok, G2} = oz_test_utils:add_group_to_group(Config, G1, G2),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_SET_PRIVILEGES
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_SET_PRIVILEGES
    ]),

    % Get all possible privileges and define expected initial privileges ->
    % by default, new group should not have any oz privileges
    AllPrivs = oz_test_utils:call_oz(Config, privileges, group_privileges, []),

    SetPrivsFun =
        fun(Operation, Privs) ->
            oz_test_utils:group_set_group_privileges(
                Config, G1, G2, Operation, Privs
            )
        end,
    GetPrivsFun =
        fun() ->
            {ok, Privs} = oz_test_utils:get_group_subgroup_privileges(
                Config, G1, G2
            ),
            Privs
        end,

    % Do not include user in clients as his group privileges
    % will be changing, which means that he will be sometimes authorized,
    % sometimes not.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/groups/">>, G1, <<"/children/">>, G2, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = update_child_privileges,
            args = [client, G1, G2, data],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U4}, ?GROUP_SET_PRIVILEGES
    ])).


get_eff_children_test(Config) ->
    {G1,
        [{G2, _}, {G3, _}, {G4, _}, {G5, _}, {G6, _}],
        {U1, U2, NonAdmin}
    } = create_eff_child_groups_env(Config),

    ExpGroups = [G2, G3, G4, G5, G6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/groups/">>, G1, <<"/effective_children">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"children">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_children,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_eff_child_details_test(Config) ->
    {G1,
        EffChildList, {U1, U2, NonAdmin}
    } = create_eff_child_groups_env(Config),

    lists:foreach(
        fun({GroupId, GroupDetails}) ->
            GroupDetailsBinary = GroupDetails#{
                <<"type">> => atom_to_binary(
                    maps:get(<<"type">>, GroupDetails), utf8
                )
            },
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
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
                    path = [<<"/groups/">>, G1, <<"/effective_children/">>, GroupId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = GroupDetailsBinary#{
                        <<"groupId">> => GroupId
                    }
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_child,
                    args = [client, G1, GroupId],
                    expected_result = ?OK_MAP(GroupDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_group, id = GroupId,
                        aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_GROUP(G1),
                    expected_result = ?OK_MAP(GroupDetailsBinary#{
                        <<"gri">> =>
                        fun(EncodedGri) ->
                            #gri{id = Gid} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Gid, GroupId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffChildList
    ).



create_eff_child_groups_env(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             subgroups: [G2, G6]
    %%             users:
    %%                 - U1:
    %%                     privileges: [group_view]
    %%                 - U2:
    %%                     privileges: [~group_view]
    %%         - G2
    %%             subgroups: [G3]
    %%         - G3:
    %%             subgroups: [G4, G5]
    %%         - G4
    %%         - G5
    %%         - G6

    Groups = [{G2, _}, {G3, _}, {G4, _}, {G5, _}, {G6, _}] =
        lists:foldr(
            fun(Idx, Acc) ->
                GroupDetails = #{
                    <<"name">> => erlang:iolist_to_binary(
                        [<<"G">>, integer_to_binary(Idx)]
                    ),
                    <<"type">> => lists:nth(
                        rand:uniform(4), [unit, organization, team, role]
                    )
                },
                {ok, GroupId} = oz_test_utils:create_group(
                    Config, ?ROOT, GroupDetails
                ),
                [{GroupId, GroupDetails} | Acc]
            end, [], lists:seq(2, 6)
        ),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, <<"G1">>),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U1} = oz_test_utils:add_user_to_group(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, set, [
        ?GROUP_VIEW
    ]),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, revoke, [
        ?GROUP_VIEW
    ]),

    {ok, G2} = oz_test_utils:add_group_to_group(Config, G1, G2),
    {ok, G3} = oz_test_utils:add_group_to_group(Config, G2, G3),
    {ok, G4} = oz_test_utils:add_group_to_group(Config, G3, G4),
    {ok, G5} = oz_test_utils:add_group_to_group(Config, G3, G5),
    {ok, G6} = oz_test_utils:add_group_to_group(Config, G1, G6),

    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {G1, Groups, {U1, U2, NonAdmin}}.


get_eff_child_privileges_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3:
    %%                     privileges: [~group_view]
    %%             subgroups: [G2, G3]
    %%         - G2:
    %%             subgroups: [G3]
    %%         - G3:
    %%             owner: U1

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {G3, G2, G1} = oz_test_utils:create_3_nested_groups(Config, U1),
    {ok, G3} = oz_test_utils:add_group_to_group(Config, G1, G3),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_VIEW
    ]),

    AllPrivs = oz_test_utils:call_oz(Config, privileges, group_privileges, []),
    InitialPrivs = [group_view],

    SetPrivsFun =
        fun(Operation, Privs) ->
            % In case of SET and GRANT, randomly split privileges into four
            % parts and update groups with the privileges. G3 eff_privileges
            % should contain the sum of those. In case of revoke, the
            % privileges must be revoked for all 3 entities.
            PartitionScheme =
                case Operation of
                    revoke ->
                        [{G2, Privs}, {G3, Privs}];
                    _ -> % Covers (set|grant)
                        Parts = lists:foldl(
                            fun(Privilege, AccMap) ->
                                Index = rand:uniform(2),
                                AccMap#{
                                    Index => [Privilege | maps:get(Index, AccMap)]
                                }
                            end, #{1=>[], 2=>[]}, Privs),
                        [
                            {G2, maps:get(1, Parts)}, {G3, maps:get(2, Parts)}
                        ]
                end,
            lists:foreach(
                fun({GroupId, Privileges}) ->
                    oz_test_utils:group_set_group_privileges(
                        Config, G1, GroupId, Operation, Privileges
                    )
                end, PartitionScheme
            )
        end,

    % Do not include U1 in clients as his group privileges
    % will be changing, which means that he will be sometimes authorized,
    % sometimes not.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U3},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [
                <<"/groups/">>, G1, <<"/effective_children/">>, G3,
                <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_child_privileges,
            args = [client, G1, G3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs, [],
        {user, U1}, ?GROUP_VIEW
    ])).


list_spaces_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3:
    %%                     privileges: [~group_view]
    %%             spaces:
    %%                 - S1
    %%                 - S2
    %%                 - S3
    %%                 - S4

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, <<"S1">>),
    {ok, S2} = oz_test_utils:create_space_for_group(Config, G1, <<"S2">>),
    {ok, S3} = oz_test_utils:create_space_for_group(Config, G1, <<"S3">>),
    {ok, S4} = oz_test_utils:create_space_for_group(Config, G1, <<"S4">>),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_VIEW
    ]),

    ExpSpaces = [S1, S2, S3, S4],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/spaces">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_spaces,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_space_details_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3:
    %%                     privileges: [~group_view]
    %%             spaces:
    %%                 - S1

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),

    ExpName = <<"S1">>,
    ExpMap = #{<<"name">> => ExpName, <<"providersSupports">> => #{}},
    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, ExpName),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_VIEW
    ]),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/spaces/">>, S1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpMap#{<<"spaceId">> => S1}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_space,
            args = [client, G1, S1],
            expected_result = ?OK_MAP(ExpMap)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_space, id = S1, aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_GROUP(G1),
            expected_result = ?OK_MAP(ExpMap#{
                <<"gri">> =>
                fun(EncodedGri) ->
                    #gri{id = SpaceId} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(SpaceId, S1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_space_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_create_space]
    %%                 - U3:
    %%                     privileges: [~group_create_space]

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_CREATE_SPACE
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_CREATE_SPACE
    ]),

    ExpName = <<"SpaceName">>,
    AllPrivs = oz_test_utils:call_oz(Config, privileges, space_privileges, []),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    VerifyFun =
        fun(SpaceId) ->
            {ok, Space} = oz_test_utils:get_space(Config, SpaceId),
            ?assertEqual(ExpName, Space#od_space.name),
            true
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U3},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/spaces">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers =
                fun(#{<<"location">> := Location} = _Headers) ->
                    [_, SpaceId] = binary:split(
                        Location, <<"/spaces/">>, [trim_all]
                    ),
                    VerifyFun(SpaceId)
                end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_space,
            args = [client, G1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_space, aspect = instance},
            auth_hint = ?AS_GROUP(G1),
            expected_result = ?OK_MAP(#{
                <<"effectiveGroups">> => #{G1 => AllPrivsBin},
                <<"effectiveUsers">> => #{
                    U1 => AllPrivsBin,
                    U2 => AllPrivsBin,
                    U3 => AllPrivsBin
                },
                <<"groups">> => #{G1 => AllPrivsBin},
                <<"name">> => ExpName,
                <<"providers">> => #{},
                <<"shares">> => [],
                <<"users">> => #{},
                <<"gri">> =>
                fun(EncodedGri) ->
                    #gri{id = SpaceId} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    VerifyFun(SpaceId)
                end
            })
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            correct_values = #{
                <<"name">> => [ExpName]
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


join_space_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_join_space]
    %%                 - U3:
    %%                     privileges: [~group_join_space]
    %%     spaces:
    %%         - S1

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_JOIN_SPACE
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_JOIN_SPACE
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, <<"S1">>),

    GenTokenFun =
        fun() ->
            {ok, Macaroon} = oz_test_utils:space_invite_group_token(
                Config, ?ROOT, S1
            ),
            {ok, Token} = token_utils:serialize62(Macaroon),
            Token
        end,
    EndFun =
        fun
            (#{correctData := true} = _Env, _) ->
                oz_test_utils:space_remove_group(Config, S1, G1);
            (_, _) ->
                ok
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/spaces/join">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers =
                fun(#{<<"location">> := Location} = _Headers) ->
                    [GroupId, SpaceId] = binary:split(
                        Location,
                        [<<"/spaces/">>, <<"/groups/">>],
                        [global, trim_all]
                    ),
                    ?assertEqual(GroupId, G1),
                    ?assertEqual(SpaceId, S1),
                    true
                end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_space,
            args = [client, G1, data],
            expected_result = ?OK_BINARY(S1)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [GenTokenFun]
            },
            bad_values = [
                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, <<"123qwe">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, fun api_test_utils:init_env/0, EndFun
    )).


leave_space_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_leave_space]
    %%                 - U3:
    %%                     privileges: [~group_leave_space]
    %%             spaces: [S1]
    %%     spaces:
    %%         - S1

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_LEAVE_SPACE
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_LEAVE_SPACE
    ]),

    InitFun =
        fun() ->
            {ok, S1} = oz_test_utils:create_space_for_group(
                Config, G1, <<"S1">>
            ),
            #{spaceId => S1}
        end,
    VerifyEndFun =
        fun
            (#{correctData := true, spaceId := SpaceId} = _Env, _) ->
                ?assertMatch(
                    ?ERROR_NOT_FOUND,
                    oz_test_utils:call_oz(
                        Config, space_logic, remove_group, [?ROOT, SpaceId, G1]
                    )
                );
            (#{spaceId := SpaceId} = _Env, _) ->
                oz_test_utils:space_remove_group(Config, SpaceId, G1)
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/spaces/">>, spaceId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_space,
            args = [client, G1, spaceId],
            expected_result = ?OK
        }
    % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, InitFun, VerifyEndFun]
    )).



list_eff_spaces_test(Config) ->
    {G5,
        [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}],
        {U1, U2, NonAdmin}
    } = create_eff_spaces_env(Config),

    ExpSpaces = [S1, S2, S3, S4, S5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/groups/">>, G5, <<"/effective_spaces">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_spaces,
            args = [client, G5],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_eff_space_details_test(Config) ->
    {G5,
        EffSpacesList, {U1, _U2, NonAdmin}
    } = create_eff_spaces_env(Config),

    lists:foreach(
        fun({SpaceId, SpaceDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, U1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/groups/">>, G5, <<"/effective_spaces/">>, SpaceId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = SpaceDetails#{<<"spaceId">> => SpaceId}
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_space,
                    args = [client, G5, SpaceId],
                    expected_result = ?OK_MAP(SpaceDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_space, id = SpaceId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_GROUP(G5),
                    expected_result = ?OK_MAP(SpaceDetails#{
                        <<"gri">> =>
                        fun(EncodedGri) ->
                            #gri{id = Sid} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Sid, SpaceId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffSpacesList
    ).


create_eff_spaces_env(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             subgroups: [G2]
    %%             spaces: [S1, S2]
    %%         - G2:
    %%             subgroups: [G3]
    %%             spaces: [S3]
    %%         - G3:
    %%             subgroups: [G5]
    %%         - G4:
    %%             subgroups: [G5]
    %%             spaces: [S4]
    %%         - G5:
    %%             users:
    %%                 - U1:
    %%                     privileges: [group_view]
    %%                 - U2:
    %%                     privileges: [~group_view]
    %%             spaces: [S5]
    %%     spaces:
    %%         - S1
    %%         - S2
    %%         - S3
    %%         - S4
    %%         - S5

    {
        G5, [{G1, _}, {G2, _}, {_G3, _}, {G4, _}], Users
    } = create_eff_parent_groups_env(Config),

    Spaces = lists:map(
        fun({Idx, GroupId}) ->
            SpaceDetails = #{
                <<"name">> => erlang:iolist_to_binary(
                    [<<"S">>, integer_to_binary(Idx)]
                )
            },
            {ok, SpaceId} = oz_test_utils:create_space_for_group(
                Config, GroupId, SpaceDetails
            ),
            {SpaceId, SpaceDetails#{<<"providersSupports">> => #{}}}
        end, [{1, G1}, {2, G1}, {3, G2}, {4, G4}, {5, G5}]
    ),

    {G5, Spaces, Users}.


list_eff_providers_test(Config) ->
    {G5,
        [{P1, _}, {P2, _}, {P3, _}, {P4, _}],
        {U1, U2, NonAdmin}
    } = create_eff_providers_env(Config),

    ExpProviders = [P1, P2, P3, P4],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/groups/">>, G5, <<"/effective_providers">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"providers">> => ExpProviders}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_providers,
            args = [client, G5],
            expected_result = ?OK_LIST(ExpProviders)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_eff_provider_details_test(Config) ->
    {G5,
        EffProvidersList, {U1, U2, NonAdmin}
    } = create_eff_providers_env(Config),

    lists:foreach(
        fun({ProvId, ProvDetails}) ->
            NewProvDetails = ProvDetails#{
                <<"clientName">> => maps:get(<<"name">>, ProvDetails)
            },
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, U2},
                        {user, U1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/groups/">>, G5, <<"/effective_providers/">>, ProvId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = NewProvDetails#{<<"providerId">> => ProvId}
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_provider,
                    args = [client, G5, ProvId],
                    expected_result = ?OK_MAP(NewProvDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_provider, id = ProvId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_GROUP(G5),
                    expected_result = ?OK_MAP(NewProvDetails#{
                        <<"gri">> =>
                        fun(EncodedGri) ->
                            #gri{id = Pid} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Pid, ProvId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffProvidersList
    ).


create_eff_providers_env(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             subgroups: [G2]
    %%             spaces: [S1, S2]
    %%         - G2:
    %%             subgroups: [G3]
    %%             spaces: [S3]
    %%         - G3:
    %%             subgroups: [G5]
    %%         - G4:
    %%             subgroups: [G5]
    %%             spaces: [S4]
    %%         - G5:
    %%             users:
    %%                 - U1:
    %%                     privileges: [group_view]
    %%                 - U2:
    %%                     privileges: [~group_view]
    %%             subgroups: [G6]
    %%             spaces: [S5]
    %%     spaces:
    %%         - S1:
    %%             providers: [P1]
    %%         - S2:
    %%             providers: [P2]
    %%         - S3:
    %%             providers: [P3]
    %%         - S4:
    %%             providers: [P1]
    %%         - S5:
    %%             providers: [P4]

    {
        G5, [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}], Users
    } = create_eff_spaces_env(Config),

    Providers = [{P1, _}, {P2, _}, {P3, _}, {P4, _}] = lists:foldr(
        fun(Idx, Acc) ->
            {_, CSRFile, _} = oz_test_utils:generate_provider_cert_files(),
            {ok, CSR} = file:read_file(CSRFile),
            ProvDetails = #{
                <<"name">> => <<"Prov", (Idx+48)/integer>>,
                <<"urls">> => [<<"127.0.0.1">>],
                <<"redirectionPoint">> => <<"https://127.0.0.1">>,
                <<"csr">> => CSR,
                <<"latitude">> => rand:uniform() * 90,
                <<"longitude">> => rand:uniform() * 180
            },
            {ok, {ProvId, _}} = oz_test_utils:create_provider(
                Config, ProvDetails
            ),
            [{ProvId, maps:remove(<<"csr">>, ProvDetails)} | Acc]
        end, [], lists:seq(1, 4)
    ),

    lists:foreach(
        fun({ProvId, SpaceId}) ->
            {ok, Macaroon} = oz_test_utils:space_invite_provider_token(
                Config, ?ROOT, SpaceId
            ),
            {ok, Token} = token_utils:serialize62(Macaroon),
            {ok, SpaceId} = oz_test_utils:support_space(
                Config, ?ROOT, ProvId, Token,
                oz_test_utils:minimum_support_size(Config)
            )
        end, [{P1, S1}, {P2, S2}, {P3, S3}, {P1, S4}, {P4, S5}]
    ),

    {G5, Providers, Users}.


list_handle_services_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - NonAdmin
    %%         - User1
    %%         - User2
    %%         - User3
    %%     groups:
    %%         - G1:
    %%             owner: User1
    %%             users:
    %%                 - User2:
    %%                     privileges: [group_view]
    %%                 - User3
    %%                     privileges: [~group_view]
    %%             handle_services:
    %%                 - HS1
    %%                 - HS2
    %%                 - HS3

    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User1), <<"G1">>),
    {ok, User2} = oz_test_utils:add_user_to_group(Config, G1, User2),
    oz_test_utils:group_set_user_privileges(Config, G1, User2, set, [
        ?GROUP_VIEW
    ]),
    {ok, User3} = oz_test_utils:add_user_to_group(Config, G1, User3),
    oz_test_utils:group_set_user_privileges(Config, G1, User3, revoke, [
        ?GROUP_VIEW
    ]),

    ExpHandleServices = lists:map(
        fun(Idx) ->
            {ok, HSid} = oz_test_utils:create_handle_service(
                Config, ?ROOT, <<"HS", (Idx+48)/integer>>,
                <<"https://dot.com">>, #{asd => 1}
            ),
            {ok, G1} = oz_test_utils:add_group_to_handle_service(
                Config, HSid, G1
            ),
            HSid
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User1},
                {user, User2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, User3}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/handle_services">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handle_services">> => ExpHandleServices}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_handle_services,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpHandleServices)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_handle_service_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - NonAdmin
    %%         - User1
    %%         - User2:
    %%             privileges: [oz_handle_services_create]
    %%         - User3
    %%     groups:
    %%         - G1:
    %%             owner: User1
    %%             users:
    %%                 - User2
    %%                 - User3

    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, User2, grant, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),

    {ok, User3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User1), <<"G1">>),
    {ok, User2} = oz_test_utils:add_user_to_group(Config, G1, User2),
    {ok, User3} = oz_test_utils:add_user_to_group(Config, G1, User3),

    ExpName = <<"HS">>,
    ExpProxyEndPoint = <<"http://dot.com">>,
    ExpProperties = #{<<"asd">> => 1},

    AllPrivs = oz_test_utils:call_oz(
        Config, privileges, handle_service_privileges, []
    ),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    VerifyFun =
        fun(HSid) ->
            {ok, HS} = oz_test_utils:get_handle_service(Config, HSid),
            ?assertEqual(ExpName, HS#od_handle_service.name),
            ?assertEqual(
                ExpProxyEndPoint, HS#od_handle_service.proxy_endpoint
            ),
            ?assertEqual(
                ExpProperties, HS#od_handle_service.service_properties
            ),
            true
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, User1},
                {user, User3}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/handle_services">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                [GroupId, HSid] = binary:split(
                    Location,
                    [<<"/groups/">>, <<"/handle_services/">>],
                    [trim_all, global]
                ),
                ?assertEqual(GroupId, G1),
                VerifyFun(HSid)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_handle_service,
            args = [client, G1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_handle_service, aspect = instance},
            auth_hint = ?AS_GROUP(G1),
            expected_result = ?OK_MAP(#{
                <<"effectiveGroups">> => #{G1 => AllPrivsBin},
                <<"effectiveUsers">> => #{
                    User1 => AllPrivsBin,
                    User2 => AllPrivsBin,
                    User3 => AllPrivsBin
                },
                <<"name">> => ExpName,
                <<"gri">> =>
                fun(EncodedGri) ->
                    #gri{id = HSid} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    VerifyFun(HSid)
                end
            })
        },
        data_spec = #data_spec{
            required = [
                <<"name">>,
                <<"proxyEndpoint">>,
                <<"serviceProperties">>
            ],
            correct_values = #{
                <<"name">> => [ExpName],
                <<"proxyEndpoint">> => [ExpProxyEndPoint],
                <<"serviceProperties">> => [ExpProperties]
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)},
                {<<"proxyEndpoint">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"proxyEndpoint">>)},
                {<<"serviceProperties">>, 1234,
                    ?ERROR_BAD_VALUE_JSON(<<"serviceProperties">>)},
                {<<"serviceProperties">>, #{},
                    ?ERROR_BAD_VALUE_EMPTY(<<"serviceProperties">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_handle_service_details_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - NonAdmin:
    %%             privileges: [oz_handle_services_list]
    %%         - User1
    %%         - User2
    %%     groups:
    %%         - G1:
    %%             owner: User1
    %%             users:
    %%                 - User2
    %%             handle_services:
    %%                 - HS1

    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, NonAdmin, grant, [
        ?OZ_HANDLE_SERVICES_LIST
    ]),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User1), <<"G1">>),
    {ok, User2} = oz_test_utils:add_user_to_group(Config, G1, User2),

    ExpName = <<"HS">>,
    ExpProxyEndPoint = <<"http://dot.com">>,
    ExpProperties = #{<<"asd">> => 1},
    {ok, HSid} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ExpName, ExpProxyEndPoint, ExpProperties
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle_service(
        Config, HSid, G1
    ),

    ExpMap = #{
        <<"name">> => ExpName,
        <<"proxyEndpoint">> => ExpProxyEndPoint,
        <<"serviceProperties">> => ExpProperties
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User1},
                {user, User2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/handle_services/">>, HSid],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpMap#{
                <<"handleServiceId">> => HSid
            }
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_handle_service,
            args = [client, G1, HSid],
            expected_result = ?OK_MAP(ExpMap)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


leave_handle_service_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - NonAdmin:
    %%         - User1
    %%         - User2
    %%     groups:
    %%         - G1:
    %%             owner: User1
    %%             users:
    %%                 - User2
    %%                 - User3
    %%             handle_services:
    %%                 - HS1

    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User3} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User1), <<"G1">>),
    {ok, User2} = oz_test_utils:add_user_to_group(Config, G1, User2),
    oz_test_utils:group_set_user_privileges(Config, G1, User2, set, [
        ?GROUP_UPDATE
    ]),

    {ok, User3} = oz_test_utils:add_user_to_group(Config, G1, User3),
    oz_test_utils:group_set_user_privileges(Config, G1, User3, revoke, [
        ?GROUP_UPDATE
    ]),

    {ok, HSid} = oz_test_utils:create_handle_service(
        Config, ?ROOT, <<"HS">>, <<"http://dot.com">>, #{<<"asd">> => 1}
    ),

    InitFun =
        fun() ->
            {ok, G1} = oz_test_utils:add_group_to_handle_service(
                Config, HSid, G1
            ),
            #{}
        end,
    VerifyEndFun =
        fun
            (#{correctData := true} = _Env, _) ->
                ?assertMatch(
                    ?ERROR_NOT_FOUND,
                    oz_test_utils:call_oz(
                        Config, group_logic, get_handle_service,
                        [?ROOT, G1, HSid]
                    )
                );
            (#{} = _Env, _) ->
                oz_test_utils:group_leave_handle_service(Config, G1, HSid)
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User1},
                {user, User2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/handle_services/">>, HSid],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_handle_service,
            args = [client, G1, HSid],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, InitFun, VerifyEndFun
    )).


list_eff_handle_services_test(Config) ->
    {G5,
        [{HS1, _}, {HS2, _}, {HS3, _}, {HS4, _}, {HS5, _}],
        {U1, U2, NonAdmin}
    } = create_eff_handle_services_env(Config),

    ExpHandleServices = [HS1, HS2, HS3, HS4, HS5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/groups/">>, G5, <<"/effective_handle_services">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handle_services">> => ExpHandleServices}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_handle_services,
            args = [client, G5],
            expected_result = ?OK_LIST(ExpHandleServices)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_eff_handle_service_details_test(Config) ->
    {G5,
        EffHandleServicesList, {U1, _U2, NonAdmin}
    } = create_eff_handle_services_env(Config),

    lists:foreach(
        fun({HSId, HSDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, U1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/groups/">>, G5,
                        <<"/effective_handle_services/">>, HSId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = HSDetails#{<<"handleServiceId">> => HSId}
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_handle_service,
                    args = [client, G5, HSId],
                    expected_result = ?OK_MAP(HSDetails)
                }
                % TODO gs
%%                gs_spec = #gs_spec{
%%                    operation = get,
%%                    gri = #gri{
%%                        type = od_handle_service, id = HSId,
%%                        aspect = instance, scope = protected
%%                    },
%%                    auth_hint = ?THROUGH_GROUP(G5),
%%                    expected_result = ?OK_MAP(HSDetails#{
%%                        <<"gri">> =>
%%                        fun(EncodedGri) ->
%%                            #gri{id = Id} = oz_test_utils:decode_gri(
%%                                Config, EncodedGri
%%                            ),
%%                            ?assertEqual(Id, HSId)
%%                        end
%%                    })
%%                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffHandleServicesList
    ).


create_eff_handle_services_env(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             subgroups: [G2]
    %%             handle_services: [HS1, HS2]
    %%         - G2:
    %%             subgroups: [G3]
    %%             handle_services: [HS3]
    %%         - G3:
    %%             subgroups: [G5]
    %%         - G4:
    %%             subgroups: [G5]
    %%             handle_services: [HS4]
    %%         - G5:
    %%             users:
    %%                 - U1:
    %%                     privileges: [group_view]
    %%                 - U2:
    %%                     privileges: [~group_view]
    %%             handle_services: [HS5]
    %%     handle_services:
    %%         - HS1
    %%         - HS2
    %%         - HS3
    %%         - HS4
    %%         - HS5

    {
        G5, [{G1, _}, {G2, _}, {_G3, _}, {G4, _}], Users
    } = create_eff_parent_groups_env(Config),

    HandleServices = lists:foldr(
        fun({Idx, GroupId}, Acc) ->
            Name = <<"HS", (Idx+48)/integer>>,
            ProxyEndpoint = <<"https://dot", (Idx+48)/integer, ".com">>,
            Properties = #{<<"asd">> => 1},
            {ok, HSid} = oz_test_utils:create_handle_service(
                Config, ?ROOT, Name, ProxyEndpoint, Properties
            ),
            HSDetails = #{
                <<"name">> => Name,
                <<"proxyEndpoint">> => ProxyEndpoint,
                <<"serviceProperties">> => Properties
            },
            {ok, GroupId} = oz_test_utils:add_group_to_handle_service(
                Config, HSid, GroupId
            ),
            [{HSid, HSDetails} | Acc]
        end, [], [{1, G1}, {2, G1}, {3, G2}, {4, G4}, {5, G5}]
    ),

    {G5, HandleServices, Users}.


list_handles_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3:
    %%                     privileges: [~group_view]
    %%             spaces:
    %%                 - S1
    %%                 - S2
    %%                 - S3
    %%     spaces:
    %%         - S1:
    %%             shares:
    %%                 - sh1
    %%                 - sh2
    %%         - S2
    %%                 - sh3
    %%     shares:
    %%         - sh1:
    %%             handles: [H1, H2]
    %%         - sh2
    %%             handles: [H3]

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, <<"S1">>),
    {ok, S2} = oz_test_utils:create_space_for_group(Config, G1, <<"S2">>),

    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_VIEW
    ]),

    {ok, HSid} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),

    ExpHandles = lists:map(
        fun({Idx, SpaceId}) ->
            ShareId = <<"shareId", (Idx+48)/integer>>,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, <<"share">>, <<"file">>, SpaceId
            ),
            {ok, HandleId} = oz_test_utils:create_handle(
                Config, ?ROOT, ?HANDLE(HSid, ShareId)
            ),
            {ok, G1} = oz_test_utils:add_group_to_handle(
                Config, HandleId, G1
            ),
            HandleId
        end, [{1, S1}, {2, S1}, {3, S2}]
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U3}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/handles">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handles">> => ExpHandles}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_handles,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpHandles)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_handle_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             spaces:
    %%                 - S1
    %%             handle_services: [HS]
    %%     spaces:
    %%         - S1:
    %%             shares:
    %%                 - sh1

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, <<"S1">>),

    {ok, HandleServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle_service(
        Config, HandleServiceId, G1
    ),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, <<"share1">>, <<"share">>, <<"file">>, S1
    ),

    AllPrivs = oz_test_utils:call_oz(
        Config, privileges, handle_privileges, []
    ),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    ExpResourceType = <<"Share">>,
    VerifyFun =
        fun(HandleId) ->
            {ok, Handle} = oz_test_utils:get_handle(Config, HandleId),
            ?assertEqual(ExpResourceType, Handle#od_handle.resource_type),
            ?assertEqual(ShareId, Handle#od_handle.resource_id),
            ?assertEqual(HandleServiceId, Handle#od_handle.handle_service),
            ?assertEqual(#{G1 => AllPrivs}, Handle#od_handle.groups),
            ?assertEqual(
                #{G1 => {AllPrivs, [{od_handle, HandleId}]}},
                    Handle#od_handle.eff_groups
            ),
            ?assertEqual(#{}, Handle#od_handle.users),
            ?assertEqual(
                #{U1 => {AllPrivs, [{od_group, G1}]}},
                Handle#od_handle.eff_users
            ),
            true
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/handles">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                [GroupId, HandleId] = binary:split(
                    Location,
                    [<<"/groups/">>, <<"/handles/">>],
                    [trim_all, global]
                ),
                ?assertEqual(GroupId, G1),
                VerifyFun(HandleId)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_handle,
            args = [client, G1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_handle, aspect = instance},
            auth_hint = ?AS_GROUP(G1),
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"effectiveGroups">> => #{G1 => AllPrivsBin},
                <<"effectiveUsers">> => #{U1 => AllPrivsBin},
                <<"metadata">> => ?DC_METADATA,
                <<"handleServiceId">> => HandleServiceId,
                <<"resourceType">> => ExpResourceType,
                <<"resourceId">> => ShareId,
                <<"gri">> =>
                fun(EncodedGri) ->
                    #gri{id = HandleId} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    VerifyFun(HandleId)
                end
            })
        },
        data_spec = #data_spec{
            required = [
                <<"handleServiceId">>,
                <<"resourceType">>,
                <<"resourceId">>,
                <<"metadata">>
            ],
            correct_values = #{
                <<"handleServiceId">> => [HandleServiceId],
                <<"resourceType">> => [<<"Share">>],
                <<"resourceId">> => [ShareId],
                <<"metadata">> => [?DC_METADATA]
            },
            bad_values = [
%%                {<<"handleServiceId">>, <<"">>,
%%                    ?ERROR_BAD_VALUE_EMPTY(<<"handleServiceId">>)},
%%                {<<"handleServiceId">>, 1234,
%%                    ?ERROR_BAD_VALUE_BINARY(<<"handleServiceId">>)},
                {<<"resourceType">>, <<"">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resourceType">>,
                        [<<"Share">>])},
                {<<"resourceType">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"resourceType">>)},
%%                {<<"resourceId">>, <<"">>,
%%                    ?ERROR_BAD_VALUE_EMPTY(<<"handleServiceId">>)},
%%                {<<"resourceId">>, 1234,
%%                    ?ERROR_BAD_VALUE_BINARY(<<"handleServiceId">>)},
                {<<"metadata">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"metadata">>)},
                {<<"metadata">>, <<"">>,
                    ?ERROR_BAD_VALUE_EMPTY(<<"metadata">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_handle_details_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_view]
    %%                 - U3:
    %%                     privileges: [~group_view]
    %%             spaces:
    %%                 - S1
    %%             handle_services: [HS]
    %%             handles: [H1]
    %%     spaces:
    %%         - S1:
    %%             shares:
    %%                 - sh1
    %%     handles:
    %%         - H1

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_VIEW
    ]),

    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, <<"S1">>),

    {ok, HandleServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle_service(
        Config, HandleServiceId, G1
    ),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, <<"share1">>, <<"share">>, <<"file">>, S1
    ),

    HandleDetails = ?HANDLE(HandleServiceId, ShareId),
    {ok, HandleId} = oz_test_utils:create_handle(
        Config, ?ROOT, HandleDetails
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle(
        Config, HandleId, G1
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
%%                {user, U3},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/handles/">>, HandleId],
            expected_code = ?HTTP_200_OK,
            expected_body = {contains, HandleDetails}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_handle,
            args = [client, G1, HandleId],
            expected_result = ?OK_MAP_CONTAINS(HandleDetails)
        }
        % TODO gs
%%        gs_spec = #gs_spec{
%%            operation = get,
%%            gri = #gri{
%%                type = od_handle, id = HandleId,
%%                aspect = instance, scope = protected
%%            },
%%            auth_hint = ?THROUGH_GROUP(G1),
%%            expected_result = ?OK_MAP_CONTAINS(HandleDetails#{
%%                fun(EncodedGri) ->
%%                    #gri{id = Hid} = oz_test_utils:decode_gri(
%%                        Config, EncodedGri
%%                    ),
%%                    ?assertEqual(Hid, HandleId)
%%                end
%%            })
%%        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


leave_handle_test(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - U3
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             owner: U1
    %%             users:
    %%                 - U2:
    %%                     privileges: [group_update]
    %%                 - U3:
    %%                     privileges: [~group_update]
    %%             spaces:
    %%                 - S1
    %%             handle_services: [HS]
    %%             handles: [H1]
    %%     spaces:
    %%         - S1:
    %%             shares:
    %%                 - sh1
    %%     handles:
    %%         - H1

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_UPDATE
    ]),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_UPDATE
    ]),

    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, <<"S1">>),
    {ok, HandleServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle_service(
        Config, HandleServiceId, G1
    ),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, <<"share1">>, <<"share">>, <<"file">>, S1
    ),

    InitFun =
        fun() ->
            {ok, HandleId} = oz_test_utils:create_handle(
                Config, ?ROOT, ?HANDLE(HandleServiceId, ShareId)
            ),
            {ok, G1} = oz_test_utils:add_group_to_handle(
                Config, HandleId, G1
            ),
            #{handleId => HandleId}
        end,
    VerifyEndFun =
        fun
            (#{correctData := true, handleId := HandleId} = _Env, _) ->
                ?assertMatch(
                    ?ERROR_NOT_FOUND,
                    oz_test_utils:call_oz(
                        Config, group_logic, get_handle, [?ROOT, G1, HandleId]
                    )
                );
            (#{handleId := HandleId}, _) ->
                ?assertMatch(
                    {ok, _},
                    oz_test_utils:call_oz(
                        Config, group_logic, get_handle, [?ROOT, G1, HandleId]
                    )
                )
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U3},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/handles/">>, handleId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_handle,
            args = [client, G1, handleId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, InitFun, VerifyEndFun
    )).


list_eff_handles_test(Config) ->
    {G5,
        [{H1, _}, {H2, _}, {H3, _}, {H4, _}, {H5, _}],
        {U1, U2, NonAdmin}
    } = create_eff_handles_env(Config),

    ExpHandles = [H1, H2, H3, H4, H5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/groups/">>, G5, <<"/effective_handles">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handles">> => ExpHandles}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_handles,
            args = [client, G5],
            expected_result = ?OK_LIST(ExpHandles)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_eff_handle_details_test(Config) ->
    {G5,
        EffHandlesList, {U1, _U2, NonAdmin}
    } = create_eff_handles_env(Config),

    lists:foreach(
        fun({HandleId, HandleDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, U1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/groups/">>, G5,
                        <<"/effective_handles/">>, HandleId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = {
                        contains, HandleDetails#{<<"handleId">> => HandleId}
                    }
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_handle,
                    args = [client, G5, HandleId],
                    expected_result = ?OK_MAP_CONTAINS(HandleDetails)
                }
                % TODO gs
%%                gs_spec = #gs_spec{
%%                    operation = get,
%%                    gri = #gri{
%%                        type = od_handle, id = HandleId,
%%                        aspect = instance, scope = protected
%%                    },
%%                    auth_hint = ?THROUGH_GROUP(G5),
%%                    expected_result = ?OK_MAP(HandleDetails#{
%%                        <<"gri">> =>
%%                        fun(EncodedGri) ->
%%                            #gri{id = Id} = oz_test_utils:decode_gri(
%%                                Config, EncodedGri
%%                            ),
%%                            ?assertEqual(Id, HandleId)
%%                        end
%%                    })
%%                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffHandlesList
    ).


create_eff_handles_env(Config) ->
    %% create following test environment:
    %%
    %%     users:
    %%         - U1
    %%         - U2
    %%         - NonAdmin
    %%     groups:
    %%         - G1:
    %%             subgroups: [G2]
    %%             spaces: [S1]
    %%             handles: [H1]
    %%         - G2:
    %%             subgroups: [G3]
    %%             spaces: [S2]
    %%             handles: [H2]
    %%         - G3:
    %%             subgroups: [G5]
    %%             spaces: [S3]
    %%             handles: [H3]
    %%         - G4:
    %%             subgroups: [G5]
    %%             spaces: [S4]
    %%             handles: [H4]
    %%         - G5:
    %%             users:
    %%                 - U1:
    %%                     privileges: [group_view]
    %%                 - U2:
    %%                     privileges: [~group_view]
    %%             subgroups: [G6]
    %%             handle_services: [HS]
    %%             spaces: [S5]
    %%             handles: [H5]
    %%     spaces:
    %%         - S1:
    %%             shares: [Share1]
    %%         - S2:
    %%             shares: [Share2]
    %%         - S3:
    %%             shares: [Share3]
    %%         - S4:
    %%             shares: [Share4]
    %%         - S5:
    %%             shares: [Share5]
    %%     handles:
    %%         - H1
    %%         - H2
    %%         - H3
    %%         - H4
    %%         - H5

    {
        G5, [{G1, _}, {G2, _}, {G3, _}, {G4, _}], Users
    } = create_eff_parent_groups_env(Config),

    {ok, HandleServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, G5} = oz_test_utils:add_group_to_handle_service(
        Config, HandleServiceId, G5
    ),

    Handles = lists:map(
        fun({Idx, GroupId}) ->
            {ok, SpaceId} = oz_test_utils:create_space_for_group(
                Config, GroupId, <<"Space", (Idx+48)/integer>>
            ),
            ShareId = <<"shareId", (Idx+48)/integer>>,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, <<"share">>, <<"file">>, SpaceId
            ),
            HandleDetails = ?HANDLE(HandleServiceId, ShareId),
            {ok, HandleId} = oz_test_utils:create_handle(
                Config, ?ROOT, HandleDetails
            ),
            {ok, GroupId} = oz_test_utils:add_group_to_handle(
                Config, HandleId, GroupId
            ),
            {HandleId, HandleDetails}
        end, [{1, G1}, {2, G2}, {3, G3}, {4, G4}, {5, G5}]
    ),

    {G5, Handles, Users}.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


init_per_testcase(list_handles_test, Config) ->
    oz_test_utils:mock_handle_proxy(Config),
    Config;
init_per_testcase(create_handle_test, Config) ->
    oz_test_utils:mock_handle_proxy(Config),
    Config;
init_per_testcase(get_handle_details_test, Config) ->
    oz_test_utils:mock_handle_proxy(Config),
    Config;
init_per_testcase(leave_handle_test, Config) ->
    oz_test_utils:mock_handle_proxy(Config),
    Config;
init_per_testcase(list_eff_handles_test, Config) ->
    oz_test_utils:mock_handle_proxy(Config),
    Config;
init_per_testcase(get_eff_handle_details_test, Config) ->
    oz_test_utils:mock_handle_proxy(Config),
    Config;
init_per_testcase(_, Config) ->
    Config.


end_per_testcase(list_handles_test, Config) ->
    oz_test_utils:unmock_handle_proxy(Config);
end_per_testcase(create_handle_test, Config) ->
    oz_test_utils:unmock_handle_proxy(Config);
end_per_testcase(get_handle_details_test, Config) ->
    oz_test_utils:unmock_handle_proxy(Config);
end_per_testcase(leave_handle_test, Config) ->
    oz_test_utils:unmock_handle_proxy(Config);
end_per_testcase(list_eff_handles_test, Config) ->
    oz_test_utils:unmock_handle_proxy(Config);
end_per_testcase(get_eff_handle_details_test, Config) ->
    oz_test_utils:unmock_handle_proxy(Config);
end_per_testcase(_, _Config) ->
    ok.
