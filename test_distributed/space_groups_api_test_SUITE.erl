%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C): 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning space groups API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(space_groups_api_test_SUITE).
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
    add_group_test/1,
    create_group_invite_token_test/1,
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
        create_group_invite_token_test,
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
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_SPACES_ADD_MEMBERS
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User), ?GROUP_NAME1),

    VerifyEndFun =
        fun
            (true = _ShouldSucceed, _, Data) ->
                Privs = lists:sort(maps:get(<<"privileges">>, Data)),
                {ok, ActualPrivs} = oz_test_utils:get_space_group_privileges(
                    Config, S1, G1
                ),
                ?assertEqual(Privs, lists:sort(ActualPrivs)),
                oz_test_utils:space_remove_group(Config, S1, G1);
            (false = ShouldSucceed, _, _) ->
                {ok, Groups} = oz_test_utils:get_space_groups(Config, S1),
                ?assertEqual(lists:member(G1, Groups), ShouldSucceed)
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
            path = [<<"/spaces/">>, S1, <<"/groups/">>, G1],
            expected_code = ?HTTP_201_CREATED
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = add_group,
            args = [client, S1, G1, data],
            expected_result = ?OK_BINARY(G1)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [space_update, space_remove_group],
                    [space_invite_user, space_view]
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


create_group_invite_token_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_INVITE_GROUP
    ]),
    {ok, U2} = oz_test_utils:add_user_to_space(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_INVITE_GROUP
    ]),

    % We will keep all tokens generated until now in a process, we will query
    GeneratedTokens = fun Loop(Tokens) ->
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

    VerifyFun = fun(Token) ->
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
            path = [<<"/spaces/">>, S1, <<"/groups/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = create_group_invite_token,
            args = [client, S1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


remove_group_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_SPACES_REMOVE_MEMBERS
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_REMOVE_GROUP
    ]),
    {ok, U2} = oz_test_utils:add_user_to_space(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_REMOVE_GROUP
    ]),

    EnvSetUpFun = fun() ->
        {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME2),
        {ok, G2} = oz_test_utils:add_group_to_space(Config, S1, G2),
        #{groupId => G2}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, _) ->
        {ok, SubGroups} = oz_test_utils:get_space_groups(Config, S1),
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
            path = [<<"/spaces/">>, S1, <<"/groups/">>, groupId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = remove_group,
            args = [client, S1, groupId],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun]
    )).


list_groups_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, set, [
        ?OZ_SPACES_LIST_GROUPS
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_space(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_VIEW
    ]),

    ExpGroups = lists:map(
        fun(_) ->
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?ROOT, ?GROUP_NAME1
            ),
            oz_test_utils:add_group_to_space(Config, S1, GroupId),
            GroupId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
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
            path = [<<"/spaces/">>, S1, <<"/groups">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_groups,
            args = [client, S1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_group_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, set, [
        ?OZ_SPACES_LIST_GROUPS
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_space(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_VIEW
    ]),

    {ok, G1} = oz_test_utils:create_group(
        Config, ?ROOT,
        #{<<"name">> => ?GROUP_NAME1, <<"type">> => ?GROUP_TYPE1}
    ),
    oz_test_utils:add_group_to_space(Config, S1, G1),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
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
            path = [<<"/spaces/">>, S1, <<"/groups/">>, G1],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"groupId">> => G1,
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1_BIN
            }
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_group,
            args = [client, S1, G1],
            expected_result = ?OK_MAP(#{
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = G1, aspect = instance, scope = shared
            },
            auth_hint = ?THROUGH_SPACE(S1),
            expected_result = ?OK_MAP(#{
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1_BIN,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, G1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_group_privileges_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get group privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_space(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_VIEW
    ]),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U3), <<"G2">>),
    {ok, G1} = oz_test_utils:add_group_to_space(Config, S1, G1),

    AllPrivs = oz_test_utils:get_space_privileges(Config),
    InitialPrivs = [space_view, space_write_data],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:space_set_group_privileges(
            Config, S1, G1, Operation, Privs
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
                <<"/spaces/">>, S1, <<"/groups/">>, G1, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_group_privileges,
            args = [client, S1, G1],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?SPACE_VIEW
    ])).


update_group_privileges_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to update group privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_SET_PRIVILEGES
    ]),
    {ok, U2} = oz_test_utils:add_user_to_space(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_SET_PRIVILEGES
    ]),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U3), ?GROUP_NAME1),
    {ok, G1} = oz_test_utils:add_group_to_space(Config, S1, G1),

    AllPrivs = oz_test_utils:get_space_privileges(Config),
    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:space_set_group_privileges(
            Config, S1, G1, Operation, Privs
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:get_space_group_privileges(Config, S1, G1),
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
                <<"/spaces/">>, S1, <<"/groups/">>, G1, <<"/privileges">>
            ],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = update_group_privileges,
            args = [client, S1, G1, data],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?SPACE_SET_PRIVILEGES
    ])).


list_eff_groups_test(Config) ->
    {S1,
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}, {G6, _}],
        _EffUsers, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_space_eff_users_env(Config),

    ExpGroups = [G1, G2, G3, G4, G5, G6],
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
            path = [<<"/spaces/">>, S1, <<"/effective_groups">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_eff_groups,
            args = [client, S1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:has_eff_group function
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:call_oz(
                Config, space_logic, has_eff_group, [S1, GroupId])
            )
        end, ExpGroups
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, space_logic, has_eff_group, [S1, <<"asdiucyaie827346w">>])
    ).


get_eff_group_test(Config) ->
    {
        S1, EffGroups, _EffUsers, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_space_eff_users_env(Config),

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
                        <<"/spaces/">>, S1, <<"/effective_groups/">>, GroupId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = GroupDetailsBinary#{
                        <<"groupId">> => GroupId
                    }
                },
                logic_spec = #logic_spec{
                    module = space_logic,
                    function = get_eff_group,
                    args = [client, S1, GroupId],
                    expected_result = ?OK_MAP(GroupDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_group, id = GroupId,
                        aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_SPACE(S1),
                    expected_result = ?OK_MAP(GroupDetailsBinary#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Id, GroupId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffGroups
    ).


get_eff_group_privileges_test(Config) ->
    %% Create environment with following relations:
    %%
    %%                  Space
    %%                 /  ||  \
    %%                /   ||   \
    %%     [~space_view]  ||  [space_view]
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

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose eff privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_space(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_VIEW
    ]),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G4} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:add_group_to_space(Config, S1, G1),
    {ok, G2} = oz_test_utils:add_group_to_space(Config, S1, G2),
    {ok, G3} = oz_test_utils:add_group_to_group(Config, G1, G3),
    {ok, G4} = oz_test_utils:add_group_to_group(Config, G2, G4),
    {ok, G4} = oz_test_utils:add_group_to_group(Config, G3, G4),
    {ok, U3} = oz_test_utils:add_user_to_group(Config, G4, U3),

    AllPrivs = oz_test_utils:get_space_privileges(Config),
    InitialPrivs = [?SPACE_VIEW, ?SPACE_WRITE_DATA],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],

    SetPrivsFun = fun(Operation, Privs) ->
        % In case of SET and GRANT, randomly split privileges into four
        % parts and update groups with the privileges. G3 eff_privileges
        % should contain the sum of those. In case of revoke, the
        % privileges must be revoked for all 3 entities.
        PartitionScheme =
            case Operation of
                revoke ->
                    [{G1, Privs}, {G2, Privs}];
                _ -> % Covers (set|grant)
                    #{1 := Privs1, 2 := Privs2} = lists:foldl(
                        fun(Privilege, AccMap) ->
                            Index = rand:uniform(2),
                            AccMap#{
                                Index => [Privilege | maps:get(Index, AccMap)]
                            }
                        end, #{1 => [], 2 => []}, Privs),
                    [{G1, Privs1}, {G2, Privs2}]
            end,
        lists:foreach(
            fun({GroupId, Privileges}) ->
                oz_test_utils:space_set_group_privileges(
                    Config, S1, GroupId, Operation, Privileges
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
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [
                <<"/spaces/">>, S1, <<"/effective_groups/">>, G4,
                <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_eff_group_privileges,
            args = [client, S1, G4],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?SPACE_VIEW
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
