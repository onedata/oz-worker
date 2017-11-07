%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C): 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning groups basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_basic_api_test_SUITE).
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
    init_per_suite/1, end_per_suite/1
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
    get_eff_oz_privileges_test/1
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
        get_eff_oz_privileges_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    ExpName = <<"G1">>,

    VerifyFun = fun(GroupId, ExpType) ->
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
            expected_headers = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, role),
                fun(#{<<"location">> := Location} = _Headers) ->
                    <<"/groups/", GroupId/binary>> = Location,
                    VerifyFun(GroupId, ExpType)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create,
            args = [client, data],
            expected_result = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, role),
                ?OK_TERM(fun(GroupId) -> VerifyFun(GroupId, ExpType) end)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_group, aspect = instance},
            auth_hint = ?AS_USER(U1),
            expected_result = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, role),
                ?OK_MAP_CONTAINS(#{
                    <<"name">> => ExpName,
                    <<"type">> => atom_to_binary(ExpType, utf8),
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = oz_test_utils:decode_gri(
                            Config, EncodedGri
                        ),
                        VerifyFun(Id, ExpType)
                    end
                })
            end)
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


get_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST
    ]),

    ExpName = <<"G1">>,
    ExpType = unit,
    ExpTypeBinary = atom_to_binary(ExpType, utf8),
    {ok, G1} = oz_test_utils:create_group(
        Config, ?USER(U1), #{<<"name">> => ExpName, <<"type">> => ExpType}
    ),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
        ?GROUP_VIEW
    ]),
    oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),

    AllPrivs = oz_test_utils:call_oz(Config, privileges, group_privileges, []),
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
                {user, Admin},
                {user, NonAdmin},
                {user, U1}
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

                    users = Users, spaces = [],
                    handle_services = [], handles = [],

                    eff_users = EffUsers,
                    eff_spaces = #{}, eff_providers = #{},
                    eff_handle_services = #{}, eff_handles = #{}
                }) ->
                    ?assertEqual(ExpType, Type),
                    ?assertEqual(ExpName, Name),
                    ?assertEqual(Users, #{
                        U1 => AllPrivs -- [group_view],
                        U2 => [group_view]}
                    ),
                    ?assertEqual(EffUsers, #{
                        U1 => {AllPrivs -- [group_view], [{od_group, G1}]},
                        U2 => {[group_view], [{od_group, G1}]}
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
                <<"effectiveUsers">> => #{
                    U1 => AllPrivsBin -- [<<"group_view">>],
                    U2 => [<<"group_view">>]
                },
                <<"users">> => #{
                    U1 => AllPrivsBin -- [<<"group_view">>],
                    U2 => [<<"group_view">>]
                },
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
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
                {user, Admin},
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        logic_spec = LogicSpec = #logic_spec{
            module = group_logic,
            function = get_shared_data,
            args = [client, G1],
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"type">> => ExpType
            })
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = G1, aspect = instance, scope = shared
            },
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"type">> => ExpTypeBinary,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
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
                <<"name">> => ExpName,
                <<"type">> => ExpTypeBinary
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
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    % Create a group
    InitialName = <<"G1">>,
    EnvSetUpFun = fun() ->
        {ok, G1} = oz_test_utils:create_group(
            Config, ?USER(U1), InitialName
        ),
        oz_test_utils:group_set_user_privileges(
            Config, G1, U1, revoke, [?GROUP_UPDATE]
        ),
        oz_test_utils:add_user_to_group(Config, G1, U2),
        oz_test_utils:group_set_user_privileges(
            Config, G1, U2, set, [?GROUP_UPDATE]
        ),
        #{groupId => G1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, Data) ->
        {ok, Group} = oz_test_utils:get_group(Config, GroupId),
        {ExpType, ExpName} = case ShouldSucceed of
            false ->
                {role, InitialName};
            true ->
                {
                    maps:get(<<"type">>, Data, role),
                    maps:get(<<"name">>, Data, InitialName)
                }
        end,
        ?assertEqual(ExpName, Group#od_group.name),
        ?assertEqual(ExpType, Group#od_group.type)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
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
            args = [client, groupId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_group, id = groupId, aspect = instance},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            at_least_one = [
                <<"name">>,
                <<"type">>
            ],
            correct_values = #{
                <<"name">> => [fun() ->
                    UniqueInt = erlang:unique_integer([positive]),
                    <<"Group", (integer_to_binary(UniqueInt))/binary>>
                end],
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
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    GroupName = <<"G1">>,
    EnvSetUpFun = fun() ->
        {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), GroupName),
        oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
            ?GROUP_DELETE
        ]),
        oz_test_utils:add_user_to_group(Config, G1, U2),
        oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
            ?GROUP_DELETE
        ]),
        #{groupId => G1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, _) ->
        {ok, Groups} = oz_test_utils:list_groups(Config),
        ?assertEqual(lists:member(GroupId, Groups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1}
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
            gri = #gri{type = od_group, id = groupId, aspect = instance},
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun]
    )).


get_oz_privileges_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_VIEW_PRIVILEGES
    ]),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),

    InitialPrivs = [],
    AllPrivs = oz_test_utils:call_oz(Config, privileges, oz_privileges, []),
    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:set_group_oz_privileges(Config, G1, Operation, Privs)
    end,

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
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U1}, ?OZ_VIEW_PRIVILEGES
    ])).


update_oz_privileges_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_SET_PRIVILEGES
    ]),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),

    AllPrivs = oz_test_utils:call_oz(Config, privileges, oz_privileges, []),
    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:set_group_oz_privileges(Config, G1, Operation, Privs)
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:get_group_oz_privileges(Config, G1),
        Privs
    end,

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
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_SET_PRIVILEGES
    ]),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),

    AllPrivs = oz_test_utils:call_oz(Config, privileges, oz_privileges, []),
    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:set_group_oz_privileges(Config, G1, Operation, Privs)
    end,
    GetPrivsFun = fun() ->
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
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_VIEW_PRIVILEGES
    ]),

    {Bottom, Mid, Top} = oz_test_utils:create_3_nested_groups(Config, U1),

    InitialPrivs = [],
    AllPrivs = oz_test_utils:call_oz(Config, privileges, oz_privileges, []),
    SetPrivsFun = fun(Operation, Privs) ->
        % In case of SET and GRANT, randomly split privileges into four
        % parts and update groups with the privileges. G3 eff_privileges
        % should contain the sum of those. In case of revoke, the
        % privileges must be revoked for all 3 entities.
        PartitionScheme =
            case Operation of
                revoke ->
                    [{Bottom, Privs}, {Mid, Privs}, {Top, Privs}];
                _ -> % Covers (set|grant)
                    #{1 := Privs1, 2 := Privs2, 3 := Privs3} = lists:foldl(
                        fun(Privilege, AccMap) ->
                            Index = rand:uniform(3),
                            AccMap#{
                                Index => [Privilege | maps:get(Index, AccMap)]
                            }
                        end, #{1 => [], 2 => [], 3 => []}, Privs),
                    [{Bottom, Privs1}, {Mid, Privs2}, {Top, Privs3}]
            end,
        lists:foreach(
            fun({GroupId, Privileges}) ->
                oz_test_utils:set_group_oz_privileges(
                    Config, GroupId, Operation, Privileges
                )
            end, PartitionScheme
        )
    end,

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
            path = [<<"/groups/">>, Bottom, <<"/effective_privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_oz_privileges,
            args = [client, Bottom],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U1}, ?OZ_VIEW_PRIVILEGES
    ])).


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
