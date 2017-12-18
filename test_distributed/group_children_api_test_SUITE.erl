%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning group children API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_children_api_test_SUITE).
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


-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    list_children_test/1,
    create_group_invite_token_test/1,
    get_child_details_test/1,
    add_child_test/1,
    remove_child_test/1,
    get_child_privileges_test/1,
    update_child_privileges_test/1,
    get_eff_children_test/1,
    get_eff_child_details_test/1,
    get_eff_child_privileges_test/1
]).

all() ->
    ?ALL([
        list_children_test,
        create_group_invite_token_test,
        get_child_details_test,
        add_child_test,
        remove_child_test,
        get_child_privileges_test,
        update_child_privileges_test,
        get_eff_children_test,
        get_eff_child_details_test,
        get_eff_child_privileges_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_children_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, set, [
        ?OZ_GROUPS_LIST_GROUPS
    ]),

    ExpChildren = lists:map(
        fun(_) ->
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?ROOT, ?GROUP_NAME2
            ),
            oz_test_utils:group_add_group(Config, G1, GroupId),
            GroupId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
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
            path = [<<"/groups/">>, G1, <<"/children">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"nested_groups">> => ExpChildren}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_children,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpChildren)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_group_invite_token_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_INVITE_GROUP privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_INVITE_GROUP
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/groups/">>, G1, <<"/children/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) ->
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
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, set, [
        ?OZ_GROUPS_LIST_GROUPS
    ]),

    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT,
        #{<<"name">> => ?GROUP_NAME2, <<"type">> => ?GROUP_TYPE2}
    ),
    oz_test_utils:group_add_group(Config, G1, G2),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
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
            path = [<<"/groups/">>, G1, <<"/children/">>, G2],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"groupId">> => G2,
                <<"name">> => ?GROUP_NAME2,
                <<"type">> => ?GROUP_TYPE2_BIN
            }
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_child,
            args = [client, G1, G2],
            expected_result = ?OK_MAP(#{
                <<"name">> => ?GROUP_NAME2,
                <<"type">> => ?GROUP_TYPE2
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = G2, aspect = instance, scope = shared
            },
            auth_hint = ?THROUGH_GROUP(G1),
            expected_result = ?OK_MAP(#{
                <<"name">> => ?GROUP_NAME2,
                <<"type">> => ?GROUP_TYPE2_BIN,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, G2)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


add_child_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_ADD_MEMBERS
    ]),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User), ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(User), ?GROUP_NAME2),

    VerifyEndFun =
        fun
            (true = _ShouldSucceed, _, Data) ->
                ExpPrivs = lists:sort(maps:get(<<"privileges">>, Data)),
                {ok, Privs} = oz_test_utils:group_get_group_privileges(
                    Config, G1, G2
                ),
                ?assertEqual(ExpPrivs, lists:sort(Privs)),
                oz_test_utils:group_remove_group(Config, G1, G2);
            (false = ShouldSucceed, _, _) ->
                {ok, SubGroups} = oz_test_utils:group_get_children(Config, G1),
                ?assertEqual(lists:member(G2, SubGroups), ShouldSucceed)
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/groups/">>, G1, <<"/children/">>, G2],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                ExpLocation = <<"/groups/", G1/binary, "/children/", G2/binary>>,
                ?assertEqual(ExpLocation, Location),
                true
            end
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
                    [?GROUP_JOIN_GROUP, ?GROUP_REMOVE_GROUP],
                    [?GROUP_INVITE_USER, ?GROUP_VIEW]
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


remove_child_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_REMOVE_GROUP privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_REMOVE_GROUP
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_REMOVE_MEMBERS
    ]),

    EnvSetUpFun = fun() ->
        {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME2),
        {ok, G2} = oz_test_utils:group_add_group(Config, G1, G2),
        #{groupId => G2}
    end,
    DeleteEntityFun = fun(#{groupId := GroupId} = _Env) ->
        oz_test_utils:group_remove_group(Config, G1, GroupId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, _) ->
        {ok, SubGroups} = oz_test_utils:group_get_children(Config, G1),
        ?assertEqual(lists:member(GroupId, SubGroups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
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
            path = [<<"/groups/">>, G1, <<"/children/">>, groupId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = remove_group,
            args = [client, G1, groupId],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


get_child_privileges_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get group privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U3), ?GROUP_NAME2),
    {ok, G2} = oz_test_utils:group_add_group(Config, G1, G2),

    AllPrivs = oz_test_utils:all_group_privileges(Config),
    InitialPrivs = [?GROUP_VIEW],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:group_set_group_privileges(
            Config, G1, G2, Operation, Privs
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
                <<"/groups/">>, G1, <<"/children/">>, G2, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
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
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?GROUP_VIEW
    ])).


update_child_privileges_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_SET_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_SET_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to update group privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U3), ?GROUP_NAME2),
    {ok, G2} = oz_test_utils:group_add_group(Config, G1, G2),

    AllPrivs = oz_test_utils:all_group_privileges(Config),
    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:group_set_group_privileges(
            Config, G1, G2, Operation, Privs
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:group_get_group_privileges(
            Config, G1, G2
        ),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
                <<"/groups/">>, G1, <<"/children/">>, G2, <<"/privileges">>
            ],
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
        {user, U3}, ?GROUP_SET_PRIVILEGES
    ])).


get_eff_children_test(Config) ->
    {
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}, {G6, _}], _Users
    } = api_test_scenarios:create_eff_child_groups_env(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST_GROUPS
    ]),

    {ok, U1} = oz_test_utils:group_add_user(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, set,
        oz_test_utils:all_group_privileges(Config) -- [?GROUP_VIEW]
    ),
    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),

    ExpGroups = [G2, G3, G4, G5, G6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_child function
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_child, [G1, GroupId])
            )
        end, ExpGroups
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_child, [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_child_details_test(Config) ->
    {
        [{G1, _} | EffChildren], _Users
    } = api_test_scenarios:create_eff_child_groups_env(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST_GROUPS
    ]),

    {ok, U1} = oz_test_utils:group_add_user(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, set,
        oz_test_utils:all_group_privileges(Config) -- [?GROUP_VIEW]
    ),
    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),

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
                        {user, Admin},
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
                        <<"/groups/">>, G1, <<"/effective_children/">>, GroupId
                    ],
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
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Gid} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Gid, GroupId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffChildren
    ).


get_eff_child_privileges_test(Config) ->
    %% Create environment with following relations:
    %%
    %%           User2          User3
    %%              \            /
    %%               \          /
    %%        [group_view]   [~group_view]
    %%                 \      /
    %%                  Group1
    %%                 /    \
    %%                /      \
    %%             Group2     \
    %%                   \    |
    %%                    \   |
    %%                    Group3
    %%                      |
    %%                    User1

    AllPrivs = oz_test_utils:all_group_privileges(Config),
    InitialPrivs = [?GROUP_VIEW],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get group privileges and sometimes not)
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST_GROUPS
    ]),

    {G3, G2, G1} = oz_test_utils:create_3_nested_groups(Config, U1),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),

    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:group_add_user(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, set,
        AllPrivs -- [?GROUP_VIEW]
    ),

    SetPrivsFun = fun(Operation, Privs) ->
        % In case of SET and GRANT, randomly split privileges into four
        % parts and update groups with the privileges. G3 eff_privileges
        % should contain the sum of those. In case of revoke, the
        % privileges must be revoked for all 2 entities.
        PartitionScheme =
            case Operation of
                revoke ->
                    [{G2, Privs}, {G3, Privs}];
                _ -> % Covers (set|grant)
                    #{1 := Privs1, 2 := Privs2} = lists:foldl(
                        fun(Privilege, AccMap) ->
                            Index = rand:uniform(2),
                            AccMap#{
                                Index => [Privilege | maps:get(Index, AccMap)]
                            }
                        end, #{1 => [], 2 => []}, Privs),
                    [{G2, Privs1}, {G3, Privs2}]
            end,
        lists:foreach(
            fun({GroupId, Privileges}) ->
                oz_test_utils:group_set_group_privileges(
                    Config, G1, GroupId, Operation, Privileges
                )
            end, PartitionScheme
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U3},
                {user, Admin},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [
                <<"/groups/">>, G1,
                <<"/effective_children/">>, G3, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
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
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U1}, ?GROUP_VIEW
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
