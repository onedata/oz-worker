%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning groups basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_misc_api_test_SUITE).
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
-include_lib("ctool/include/api_errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    create_test/1,
    list_test/1,
    list_privileges_test/1,
    get_test/1,
    update_test/1,
    delete_test/1,
    protected_group_test/1,
    get_oz_privileges_test/1,
    update_oz_privileges_test/1,
    delete_oz_privileges_test/1,
    get_eff_oz_privileges_test/1,

    list_eff_providers_test/1,
    get_eff_provider_test/1,
    get_spaces_in_eff_provider_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        list_privileges_test,
        get_test,
        update_test,
        delete_test,
        protected_group_test,
        get_oz_privileges_test,
        update_oz_privileges_test,
        delete_oz_privileges_test,
        get_eff_oz_privileges_test,

        list_eff_providers_test,
        get_eff_provider_test,
        get_spaces_in_eff_provider_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


create_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),

    VerifyFun = fun(GroupId, ExpType) ->
        {ok, Group} = oz_test_utils:get_group(Config, GroupId),
        ?assertEqual(?CORRECT_NAME, Group#od_group.name),
        ?assertEqual(ExpType, Group#od_group.type),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_CREATE]}
            ],
            unauthorized = [nobody],
            forbidden = [{user, U1}]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/groups">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                BaseURL = ?URL(Config, [<<"/groups/">>]),
                fun(#{<<"Location">> := Location} = _Headers) ->
                    [GroupId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(GroupId, ExpType)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                ?OK_TERM(fun(GroupId) -> VerifyFun(GroupId, ExpType) end)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_group, aspect = instance},
            expected_result = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                ?OK_MAP_CONTAINS(#{
                    <<"name">> => ?CORRECT_NAME,
                    <<"type">> => atom_to_binary(ExpType, utf8),
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
                        VerifyFun(Id, ExpType)
                    end
                })
            end)
        },
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


list_test(Config) ->
    % Make sure that groups created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G4} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G5} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    ExpGroups = [G1, G2, G3, G4, G5, <<"all_users">>, <<"admins">>],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_LIST]}
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
            args = [auth],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:exist function
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, exists, [GroupId])
            )
        end, ExpGroups
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, exists, [<<"asdiucyaie827346w">>])
    ).


list_privileges_test(Config) ->

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/groups/privileges">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"member">> => [atom_to_binary(P, utf8) || P <- privileges:group_member()],
                <<"manager">> => [atom_to_binary(P, utf8) || P <- privileges:group_manager()],
                <<"admin">> => [atom_to_binary(P, utf8) || P <- privileges:group_admin()]
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1),
        #{<<"name">> => ?GROUP_NAME1, <<"type">> => ?GROUP_TYPE1}
    ),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, [], [
        ?GROUP_VIEW
    ]),
    oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, [
        ?GROUP_VIEW
    ], []),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:group_privileges(),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {admin, [?OZ_GROUPS_VIEW]},
                {user, NonAdmin},
                {user, U1}
            ]
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get,
            args = [auth, G1],
            expected_result = ?OK_TERM(
                fun(#od_group{
                    name = Name, type = Type,
                    oz_privileges = [], eff_oz_privileges = [],

                    parents = [], eff_parents = #{},
                    children = #{}, eff_children = #{},

                    users = Users, spaces = [],
                    handle_services = [], handles = [],

                    eff_users = EffUsers,
                    eff_spaces = #{}, eff_providers = #{},
                    eff_handle_services = #{}, eff_handles = #{}
                }) ->
                    ?assertEqual(?GROUP_NAME1, Name),
                    ?assertEqual(?GROUP_TYPE1, Type),
                    ?assertEqual(Users, #{
                        U1 => AllPrivs -- [?GROUP_VIEW], U2 => [?GROUP_VIEW]}
                    ),
                    ?assertEqual(EffUsers, #{
                        U1 => {AllPrivs -- [?GROUP_VIEW], [{od_group, <<"self">>}]},
                        U2 => {[?GROUP_VIEW], [{od_group, <<"self">>}]}
                    })
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_group, id = G1, aspect = instance},
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"children">> => #{},
                <<"effectiveChildren">> => #{},
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1_BIN,
                <<"parents">> => [],
                <<"spaces">> => [],
                <<"effectiveUsers">> => #{
                    U1 => AllPrivsBin -- [<<"group_view">>],
                    U2 => [<<"group_view">>]
                },
                <<"users">> => #{
                    U1 => AllPrivsBin -- [<<"group_view">>],
                    U2 => [<<"group_view">>]
                },
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(G1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check shared data
    GetSharedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_VIEW]},
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        logic_spec = LogicSpec = #logic_spec{
            module = group_logic,
            function = get_shared_data,
            args = [auth, G1],
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1
            })
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = G1, aspect = instance, scope = shared
            },
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1_BIN,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(G1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetSharedDataApiTestSpec)),

    % Get and check protected data
    GetProtectedDataApiTestSpec = GetSharedDataApiTestSpec#api_test_spec{
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"groupId">> => G1,
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1_BIN
            }
        },
        logic_spec = LogicSpec#logic_spec{
            function = get_protected_data
        },
        gs_spec = GsSpec#gs_spec{
            gri = #gri{
                type = od_group, id = G1, aspect = instance, scope = protected
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)).


update_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
        oz_test_utils:group_set_user_privileges(Config, G1, U1, [], [
            ?GROUP_UPDATE
        ]),
        oz_test_utils:group_add_user(Config, G1, U2),
        oz_test_utils:group_set_user_privileges(Config, G1, U2, [
            ?GROUP_UPDATE
        ], []),
        #{groupId => G1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, Data) ->
        {ok, Group} = oz_test_utils:get_group(Config, GroupId),
        {ExpType, ExpName} = case ShouldSucceed of
            false ->
                {?DEFAULT_GROUP_TYPE, ?GROUP_NAME1};
            true ->
                {
                    maps:get(<<"type">>, Data, ?DEFAULT_GROUP_TYPE),
                    maps:get(<<"name">>, Data, ?GROUP_NAME1)
                }
        end,
        ?assertEqual(ExpName, Group#od_group.name),
        ?assertEqual(ExpType, Group#od_group.type)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_UPDATE]}
            ],
            unauthorized = [nobody],
            forbidden = [{user, U1}]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/groups/">>, groupId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = update,
            args = [auth, groupId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_group, id = groupId, aspect = instance},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            at_least_one = [<<"name">>, <<"type">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"type">> => [?GROUP_TYPE2]
            },
            bad_values = [
                {<<"type">>, kingdom,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>, ?GROUP_TYPES)},
                {<<"type">>, 1234, ?ERROR_BAD_VALUE_ATOM(<<"type">>)}
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
        oz_test_utils:group_set_user_privileges(Config, G1, U1, [], [
            ?GROUP_DELETE
        ]),
        oz_test_utils:group_add_user(Config, G1, U2),
        oz_test_utils:group_set_user_privileges(Config, G1, U2, [
            ?GROUP_DELETE
        ], []),
        #{groupId => G1}
    end,
    DeleteEntityFun = fun(#{groupId := GroupId} = _Env) ->
        oz_test_utils:delete_group(Config, GroupId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, _) ->
        {ok, Groups} = oz_test_utils:list_groups(Config),
        ?assertEqual(lists:member(GroupId, Groups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_DELETE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, groupId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = delete,
            args = [auth, groupId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_group, id = groupId, aspect = instance},
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


protected_group_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, GroupId} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    oz_test_utils:group_set_user_privileges(Config, GroupId, U1, [
        ?GROUP_DELETE
    ], []),
    oz_test_utils:mark_group_protected(Config, GroupId),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_GROUPS_DELETE]}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, GroupId],
            expected_code = ?HTTP_403_FORBIDDEN
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = delete,
            args = [auth, GroupId],
            expected_result = ?ERROR_REASON(?ERROR_PROTECTED_GROUP)
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_group, id = GroupId, aspect = instance},
            expected_result = ?ERROR_REASON(?ERROR_PROTECTED_GROUP)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Verify that group exists
    {ok, Groups} = oz_test_utils:list_groups(Config),
    ?assertEqual(lists:member(GroupId, Groups), true).


get_oz_privileges_test(Config) ->
    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get group privileges and sometimes not)
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),

    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    InitialPrivs = [],
    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:group_set_oz_privileges(Config, G1, PrivsToGrant, PrivsToRevoke)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_VIEW_PRIVILEGES]}
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
            args = [auth, G1],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U1}, ?OZ_VIEW_PRIVILEGES
    ])).


update_oz_privileges_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to update group privileges and sometimes not)
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:group_set_oz_privileges(Config, G1, PrivsToGrant, PrivsToRevoke),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config)
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:group_get_oz_privileges(Config, G1),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SET_PRIVILEGES]}
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
            args = [auth, G1, data],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U1}, ?OZ_SET_PRIVILEGES
    ])).


delete_oz_privileges_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:group_set_oz_privileges(Config, G1, PrivsToGrant, PrivsToRevoke),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config)
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:group_get_oz_privileges(Config, G1),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = delete_oz_privileges,
            args = [auth, G1],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U1}, ?OZ_SET_PRIVILEGES
    ])).


get_eff_oz_privileges_test(Config) ->
    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get group privileges and sometimes not)
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {Bottom, Mid, Top} = oz_test_utils:create_3_nested_groups(Config, U1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    InitialPrivs = [],
    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        % In case of GRANT, randomly split privileges into four
        % parts and update groups with the privileges. G3 eff_privileges
        % should contain the sum of those. In case of revoke, the
        % privileges must be revoked for all 3 entities.
        #{1 := PrivsToGrant1, 2 := PrivsToGrant2, 3 := PrivsToGrant3} = lists:foldl(
            fun(Privilege, AccMap) ->
                Index = rand:uniform(3),
                AccMap#{
                    Index => [Privilege | maps:get(Index, AccMap)]
                }
            end, #{1 => [], 2 => [], 3 => []}, PrivsToGrant),

        oz_test_utils:group_set_oz_privileges(Config, Bottom, PrivsToGrant1, PrivsToRevoke),
        oz_test_utils:group_set_oz_privileges(Config, Bottom, PrivsToGrant2, PrivsToRevoke),
        oz_test_utils:group_set_oz_privileges(Config, Bottom, PrivsToGrant3, PrivsToRevoke),

        oz_test_utils:ensure_entity_graph_is_up_to_date(Config)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_VIEW_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, Bottom, <<"/effective_privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_oz_privileges,
            args = [auth, Bottom],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U1}, ?OZ_VIEW_PRIVILEGES
    ])).


list_eff_providers_test(Config) ->
    {
        [{P1, _}, {P2, _}, {P3, _}, {P4, _}],
        _Spaces, [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    ExpProviders = [P1, P2, P3, P4],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                % U2 does not have the GROUP_VIEW privilege
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/effective_providers">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"providers">> => ExpProviders}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_providers,
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpProviders)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_provider function
    lists:foreach(
        fun(ProviderId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_provider, [G1, ProviderId])
            )
        end, ExpProviders
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_provider, [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_provider_test(Config) ->
    {
        EffProvidersList, _Spaces, [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    lists:foreach(
        fun({ProvId, ProvDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {admin, [?OZ_PROVIDERS_VIEW]},
                        {user, U1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        % U2 does not have the GROUP_VIEW privilege
                        {user, U2},
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/groups/">>, G1, <<"/effective_providers/">>, ProvId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = ProvDetails#{<<"providerId">> => ProvId}
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_provider,
                    args = [auth, G1, ProvId],
                    expected_result = ?OK_MAP_CONTAINS(ProvDetails)
                }
                % @todo gs
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffProvidersList
    ).


get_spaces_in_eff_provider_test(Config) ->
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_DETAILS(?UNIQUE_STRING)#{<<"subdomainDelegation">> => false}
    ),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?UNIQUE_STRING),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U2), ?UNIQUE_STRING),

    {ok, S1_1} = oz_test_utils:create_space(Config, ?USER(U1), ?UNIQUE_STRING),
    oz_test_utils:space_add_group(Config, S1_1, G1),
    {ok, S1_2} = oz_test_utils:create_space(Config, ?USER(U1), ?UNIQUE_STRING),
    oz_test_utils:space_add_group(Config, S1_2, G1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U2), ?UNIQUE_STRING),
    oz_test_utils:space_add_group(Config, S2, G2),

    oz_test_utils:support_space(Config, ProviderId, S1_1),
    oz_test_utils:support_space(Config, ProviderId, S1_2),
    oz_test_utils:support_space(Config, ProviderId, S2),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AnotherUser = fun(User) -> case User of
        U1 -> U2;
        U2 -> U1
    end end,

    lists:foreach(
        fun({ClientUserId, GroupId, GroupSpaces}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, ClientUserId}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, AnotherUser(ClientUserId)},
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/groups/">>, GroupId, <<"/effective_providers/">>, ProviderId, <<"/spaces">>],
                    expected_code = ?HTTP_200_OK,
                    expected_body = #{<<"spaces">> => GroupSpaces}
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_spaces_in_eff_provider,
                    args = [auth, GroupId, ProviderId],
                    expected_result = ?OK_LIST(GroupSpaces)
                }
                % @todo gs
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
        end, [{U1, G1, [S1_1, S1_2]}, {U2, G2, [S2]}]
    ).


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
