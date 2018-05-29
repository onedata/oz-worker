%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning groups users API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_users_api_test_SUITE).
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
-include_lib("ctool/include/api_errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    list_users_test/1,
    create_user_invite_token_test/1,
    get_user_details_test/1,
    add_user_test/1,
    remove_user_test/1,
    list_user_privileges_test/1,
    update_user_privileges_test/1,
    list_eff_users_test/1,
    get_eff_user_details_test/1,
    list_eff_user_privileges_test/1
]).

all() ->
    ?ALL([
        list_users_test,
        create_user_invite_token_test,
        get_user_details_test,
        add_user_test,
        remove_user_test,
        list_user_privileges_test,
        update_user_privileges_test,
        list_eff_users_test,
        get_eff_user_details_test,
        list_eff_user_privileges_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_users_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST_USERS
    ]),

    {ok, U3} = oz_test_utils:group_add_user(Config, G1, U3),
    ExpUsers = [U1, U2, U3],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {user, U2},
                {user, U3}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
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
    % create group with 2 users:
    %   U2 gets the GROUP_INVITE_USER privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_INVITE_USER
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
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST_USERS
    ]),

    ExpAlias = ExpName = <<"user1">>,
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{
        name = ExpName,
        alias = ExpAlias
    }),
    {ok, U3} = oz_test_utils:group_add_user(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, set, []),

    ExpDetails = #{
        <<"alias">> => ExpAlias,
        <<"name">> => ExpName
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {user, U2},
                {user, U3}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/users/">>, U3],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpDetails#{<<"userId">> => U3}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_user,
            args = [client, G1, U3],
            expected_result = ?OK_MAP(ExpDetails)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_user, id = U3, aspect = instance, scope = shared
            },
            auth_hint = ?THROUGH_GROUP(G1),
            expected_result = ?OK_MAP(ExpDetails#{
                <<"login">> => ExpAlias, % TODO VFS-4506 deprecated, included for backward compatibility
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = UserId} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(U3, UserId)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


add_user_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_ADD_MEMBERS
    ]),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),

    VerifyEndFun =
        fun
            (true = _ShouldSucceed, _, Data) ->
                ExpPrivs = lists:sort(maps:get(<<"privileges">>, Data)),
                {ok, Privs} = oz_test_utils:group_get_user_privileges(
                    Config, G1, U2
                ),
                ?assertEqual(ExpPrivs, lists:sort(Privs)),
                oz_test_utils:group_remove_user(Config, G1, U2);
            (false = ShouldSucceed, _, _) ->
                {ok, Users} = oz_test_utils:group_get_users(Config, G1),
                ?assertEqual(lists:member(U2, Users), ShouldSucceed)
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/groups/">>, G1, <<"/users/">>, U2],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/groups/">>, G1, <<"/users/">>, U2]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = add_user,
            args = [client, G1, U2, data],
            expected_result = ?OK_BINARY(U2)
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
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )).


remove_user_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_REMOVE_USER privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_REMOVE_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_REMOVE_MEMBERS
    ]),

    EnvSetUpFun = fun() ->
        {ok, U4} = oz_test_utils:create_user(Config, #od_user{}),
        {ok, U4} = oz_test_utils:group_add_user(Config, G1, U4),
        #{userId => U4}
    end,
    DeleteEntityFun = fun(#{userId := User} = _Env) ->
        oz_test_utils:group_remove_user(Config, G1, User)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := User} = _Env, _) ->
            {ok, Users} = oz_test_utils:group_get_users(Config, G1),
            ?assertEqual(lists:member(User, Users), not ShouldSucceed)
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
            path = [<<"/groups/">>, G1, <<"/users/">>, userId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = remove_user,
            args = [client, G1, userId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_user_privileges_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:group_add_user(Config, G1, U3),

    AllPrivs = oz_test_utils:all_group_privileges(Config),
    InitialPrivs = [?GROUP_VIEW],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],

    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:group_set_user_privileges(
            Config, G1, U3, Operation, Privs
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
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/users/">>, U3, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
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
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?GROUP_VIEW
    ])).


update_user_privileges_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_SET_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_SET_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to update user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:group_add_user(Config, G1, U3),

    AllPrivs = oz_test_utils:all_group_privileges(Config),
    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:group_set_user_privileges(
            Config, G1, U3, Operation, Privs
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:group_get_user_privileges(Config, G1, U3),
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
                {user, U1},
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
        [{G1, _} | _Groups], [{U3, _}, {U4, _}, {U5, _}, {U6, _}] = _EffUsers
    } = api_test_scenarios:create_eff_child_groups_env(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST_USERS
    ]),

    {ok, U1} = oz_test_utils:group_add_user(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, set,
        oz_test_utils:all_group_privileges(Config) -- [?GROUP_VIEW]
    ),
    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),

    ExpUsers = [U1, U2, U3, U4, U5, U6],
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

    % Check also group_logic:has_eff_user function
    lists:foreach(
        fun(UserId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_user, [G1, UserId])
            )
        end, ExpUsers
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_user, [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_user_details_test(Config) ->
    {
        [{G1, _} | _Groups], EffUsers
    } = api_test_scenarios:create_eff_child_groups_env(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_GROUPS_LIST_USERS
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
        fun({UserId, UserDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = lists:usort([
                        root,
                        {user, Admin},
                        {user, U2}
                    ]),
                    unauthorized = [nobody],
                    forbidden = [
                        {user, U1},
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/groups/">>, G1, <<"/effective_users/">>, UserId
                    ],
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
                        type = od_user, id = UserId,
                        aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_GROUP(G1),
                    expected_result = ?OK_MAP(UserDetails#{
                            <<"login">> => maps:get(<<"alias">>, UserDetails),
                            <<"gri">> => fun(EncodedGri) ->
                                #gri{id = UId} = oz_test_utils:decode_gri(
                                    Config, EncodedGri
                                ),
                                ?assertEqual(UId, UserId)
                            end
                    })
                }
            },

            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffUsers
    ).


list_eff_user_privileges_test(Config) ->
    %% Create environment with following relations:
    %%
    %%            User2         User3
    %%               \           /
    %%         [group_view]  [~group_view]
    %%                  \      /
    %%                   Group1
    %%                  /   |  \
    %%                 /    |   \
    %%              Group2  |   Group4
    %%               /      |      |
    %%              /       |  [~group_view]
    %%            Group3    |      |
    %%              \       |     /
    %%          [all_privs] |    /
    %%                  \   |   /
    %%                    User1
    %%      <<user>>
    %%      NonAdmin

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
        ?OZ_GROUPS_LIST_USERS
    ]),

    {_G3, G2, G1} = oz_test_utils:create_3_nested_groups(Config, U1),
    {ok, G4} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G4} = oz_test_utils:group_add_group(Config, G1, G4),

    {ok, U1} = oz_test_utils:group_add_user(Config, G1, U1),
    {ok, U1} = oz_test_utils:group_add_user(Config, G4, U1),
    oz_test_utils:group_set_user_privileges(Config, G4, U1, set,
        AllPrivs -- [?GROUP_VIEW]
    ),

    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),
    {ok, U3} = oz_test_utils:group_add_user(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, revoke, [
        ?GROUP_VIEW
    ]),

    SetPrivsFun = fun(Operation, Privs) ->
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
                            AccMap#{
                                Index => [Privilege | maps:get(Index, AccMap)]
                            }
                        end, #{1 => [], 2 => [], 3 => []}, Privs),
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
                <<"/effective_users/">>, U1, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
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
