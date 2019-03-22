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
    list_users_test/1,
    create_user_invite_token_test/1,
    get_user_details_test/1,
    add_user_test/1,
    add_user_with_privileges_test/1,
    remove_user_test/1,
    list_user_privileges_test/1,
    update_user_privileges_test/1,
    list_eff_users_test/1,
    get_eff_user_details_test/1,
    list_eff_user_privileges_test/1,
    get_eff_user_membership_intermediaries/1
]).

all() ->
    ?ALL([
        list_users_test,
        create_user_invite_token_test,
        get_user_details_test,
        add_user_test,
        add_user_with_privileges_test,
        remove_user_test,
        list_user_privileges_test,
        update_user_privileges_test,
        list_eff_users_test,
        get_eff_user_details_test,
        list_eff_user_privileges_test,
        get_eff_user_membership_intermediaries
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

    {ok, U3} = oz_test_utils:group_add_user(Config, G1, U3),
    ExpUsers = [U1, U2, U3],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]},
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
        Config, ?GROUP_ADD_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
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
            expected_body = fun(#{<<"token">> := Token}) ->
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
    {ok, Creator} = oz_test_utils:create_user(Config, #od_user{name = <<"creator">>, alias = <<"creator">>}),
    {ok, MemberWithViewPrivs} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, MemberWithoutViewPrivs} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Member} = oz_test_utils:create_user(Config, #od_user{name = <<"member">>, alias = <<"member">>}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, Group} = oz_test_utils:create_group(Config, ?USER(Creator), ?GROUP_NAME1),
    {ok, _} = oz_test_utils:group_add_user(Config, Group, MemberWithViewPrivs),
    {ok, _} = oz_test_utils:group_add_user(Config, Group, MemberWithoutViewPrivs),
    {ok, _} = oz_test_utils:group_add_user(Config, Group, Member),

    oz_test_utils:group_set_user_privileges(Config, Group, MemberWithViewPrivs, [?GROUP_VIEW], []),
    oz_test_utils:group_set_user_privileges(Config, Group, MemberWithoutViewPrivs, [], [?GROUP_VIEW]),

    % Shared data about creator should be available even if he is not longer in the group
    oz_test_utils:group_remove_user(Config, Group, Creator),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    lists:foreach(fun({SubjectUser, ExpName, ExpAlias}) ->
        ExpDetails = #{
            <<"alias">> => ExpAlias,
            <<"name">> => ExpName
        },
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_VIEW]},
                    {user, SubjectUser},
                    {user, MemberWithViewPrivs}
                ] ++ case SubjectUser of
                    % Every member of the group should be able to see the creator details
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
                path = [<<"/groups/">>, Group, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_200_OK,
                expected_body = ExpDetails#{
                    <<"userId">> => SubjectUser,
                    % TODO VFS-4506 deprecated, included for backward compatibility
                    <<"login">> => ExpAlias
                }
            },
            logic_spec = #logic_spec{
                module = group_logic,
                function = get_user,
                args = [client, Group, SubjectUser],
                expected_result = ?OK_MAP_CONTAINS(ExpDetails)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = SubjectUser, aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_GROUP(Group),
                expected_result = ?OK_MAP(ExpDetails#{
                    <<"gri">> => fun(EncodedGri) ->
                        ?assertMatch(
                            #gri{id = SubjectUser},
                            oz_test_utils:decode_gri(Config, EncodedGri)
                        )
                    end,
                    % TODO VFS-4506 deprecated, included for backward compatibility
                    <<"login">> => ExpAlias
                })
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, [{Creator, <<"creator">>, <<"creator">>}, {Member, <<"member">>, <<"member">>}]).


add_user_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, EffectiveUser} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, EffectiveUserWithoutInvitePriv} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),

    % EffectiveUser belongs to group G1 effectively via SubGroup1, with the
    % effective privilege to INVITE_USER, so he should be able to join the parent
    % group as a user
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:group_add_group(Config, G1, SubGroup1),
    oz_test_utils:group_set_group_privileges(Config, G1, SubGroup1, [?GROUP_ADD_USER], []),

    % EffectiveUserWithoutInvitePriv belongs to group G1 effectively via SubGroup2,
    % but without the effective privilege to INVITE_USER, so he should NOT be able
    % to join the parent group as a user
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutInvitePriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:group_add_group(Config, G1, SubGroup2),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:group_get_users(Config, G1),
            ?assert(lists:member(EffectiveUser, Users)),
            oz_test_utils:group_remove_user(Config, G1, EffectiveUser);
        (false = _ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:group_get_users(Config, G1),
            ?assertNot(lists:member(EffectiveUser, Users))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS]},
                {user, EffectiveUser}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin},
                {user, EffectiveUserWithoutInvitePriv}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/groups/">>, G1, <<"/users/">>, EffectiveUser],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/groups/">>, G1, <<"/users/">>, EffectiveUser]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = add_user,
            args = [client, G1, EffectiveUser, data],
            expected_result = ?OK_BINARY(EffectiveUser)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [],
            correct_values = #{},
            bad_values = []
        }
    },

    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )).


add_user_with_privileges_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, EffectiveUser} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, EffectiveUserWithoutInvitePriv} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),

    % EffectiveUser belongs to group G1 effectively via SubGroup1, with the
    % effective privilege to INVITE_USER and SET_PRIVILEGES, so he should be
    % able to join the parent group as a user with given privileges
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:group_add_group(Config, G1, SubGroup1),
    oz_test_utils:group_set_group_privileges(Config, G1, SubGroup1, [?GROUP_ADD_USER, ?GROUP_SET_PRIVILEGES], []),

    % EffectiveUserWithoutInvitePriv belongs to group G1 effectively via SubGroup2,
    % but without the effective privilege to INVITE_USER and SET_PRIVILEGES, so
    % he should NOT be able to join the parent group as a user with given privileges
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutInvitePriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:group_add_group(Config, G1, SubGroup2),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, Data) ->
            ExpPrivs = lists:sort(maps:get(<<"privileges">>, Data)),
            {ok, Privs} = oz_test_utils:group_get_user_privileges(
                Config, G1, EffectiveUser
            ),
            ?assertEqual(ExpPrivs, lists:sort(Privs)),
            oz_test_utils:group_remove_user(Config, G1, EffectiveUser);
        (false = ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:group_get_users(Config, G1),
            ?assertEqual(lists:member(EffectiveUser, Users), ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS, ?OZ_GROUPS_SET_PRIVILEGES]},
                {user, EffectiveUser}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin},
                {user, EffectiveUserWithoutInvitePriv}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/groups/">>, G1, <<"/users/">>, EffectiveUser],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/groups/">>, G1, <<"/users/">>, EffectiveUser]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = add_user,
            args = [client, G1, EffectiveUser, data],
            expected_result = ?OK_BINARY(EffectiveUser)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [?GROUP_ADD_PARENT, ?GROUP_REMOVE_CHILD],
                    [?GROUP_ADD_USER, ?GROUP_VIEW]
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
                {admin, [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS]},
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
        Config, ?GROUP_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:group_add_user(Config, G1, U3),

    AllPrivs = privileges:group_privileges(),
    InitialPrivs = [?GROUP_VIEW],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],

    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:group_set_user_privileges(
            Config, G1, U3, PrivsToGrant, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_VIEW_PRIVILEGES]},
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
        {user, U3}, ?GROUP_VIEW_PRIVILEGES
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

    AllPrivs = privileges:group_privileges(),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:group_set_user_privileges(
            Config, G1, U3, PrivsToGrant, PrivsToRevoke
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
                {admin, [?OZ_GROUPS_SET_PRIVILEGES]},
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

    AllGroupPrivs = privileges:group_privileges(),
    {ok, U1} = oz_test_utils:group_add_user(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1,
        AllGroupPrivs -- [?GROUP_VIEW], [?GROUP_VIEW]
    ),
    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2,
        [?GROUP_VIEW], AllGroupPrivs -- [?GROUP_VIEW]
    ),

    ExpUsers = [U1, U2, U3, U4, U5, U6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]},
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

    AllGroupPrivs = privileges:group_privileges(),
    {ok, U1} = oz_test_utils:group_add_user(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1,
        AllGroupPrivs -- [?GROUP_VIEW], [?GROUP_VIEW]
    ),
    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2,
        [?GROUP_VIEW], AllGroupPrivs -- [?GROUP_VIEW]
    ),

    lists:foreach(
        fun({UserId, UserDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = lists:usort([
                        root,
                        {admin, [?OZ_USERS_VIEW]},
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
                    expected_body = UserDetails#{
                        <<"userId">> => UserId,
                        % TODO VFS-4506 deprecated, included for backward compatibility
                        <<"login">> => maps:get(<<"alias">>, UserDetails)
                    }
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_user,
                    args = [client, G1, UserId],
                    expected_result = ?OK_MAP_CONTAINS(UserDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_user, id = UserId,
                        aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_GROUP(G1),
                    expected_result = ?OK_MAP(UserDetails#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = UId} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(UId, UserId)
                        end,
                        % TODO VFS-4506 deprecated, included for backward compatibility
                        <<"login">> => maps:get(<<"alias">>, UserDetails)
                    })
                }
            },

            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffUsers
    ).


list_eff_user_privileges_test(Config) ->
    %% Create environment with the following relations:
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

    AllPrivs = privileges:group_privileges(),
    InitialPrivs = [?GROUP_VIEW],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get group privileges and sometimes not)
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {_G3, G2, G1} = oz_test_utils:create_3_nested_groups(Config, U1),
    {ok, G4} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G4} = oz_test_utils:group_add_group(Config, G1, G4),

    {ok, U1} = oz_test_utils:group_add_user(Config, G1, U1),
    {ok, U1} = oz_test_utils:group_add_user(Config, G4, U1),
    oz_test_utils:group_set_user_privileges(Config, G4, U1,
        AllPrivs, [?GROUP_VIEW_PRIVILEGES]
    ),

    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, [
        ?GROUP_VIEW_PRIVILEGES
    ], []),
    {ok, U3} = oz_test_utils:group_add_user(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3, [], [
        ?GROUP_VIEW_PRIVILEGES
    ]),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        % In case GRANT, randomly split privileges into four
        % parts and update groups with the privileges. G3 eff_privileges
        % should contain the sum of those. In case of revoke, the
        % privileges must be revoked for all 3 entities.
        #{
            1 := PrivsToGrant1, 2 := PrivsToGrant2, 3 := PrivsToGrant3
        } = lists:foldl(
            fun(Privilege, AccMap) ->
                Index = rand:uniform(3),
                AccMap#{
                    Index => [Privilege | maps:get(Index, AccMap)]
                }
            end, #{1 => [], 2 => [], 3 => []}, PrivsToGrant),

        oz_test_utils:group_set_user_privileges(
            Config, G1, U1, PrivsToGrant1, PrivsToRevoke
        ),
        oz_test_utils:group_set_group_privileges(
            Config, G1, G2, PrivsToGrant2, PrivsToRevoke
        ),
        oz_test_utils:group_set_group_privileges(
            Config, G1, G4, PrivsToGrant3, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_VIEW_PRIVILEGES]},
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
        {user, U1}, ?GROUP_VIEW_PRIVILEGES
    ])).


get_eff_user_membership_intermediaries(Config) ->
    %% Create environment with the following relations:
    %%
    %%                   Group1    Group5
    %%                  /   |  \     /
    %%                 /    |   \   /
    %%              Group2  |   Group4
    %%               /      |   /  |  \
    %%              /       |  /   |   \
    %%            Group3----|-'    |   User2 (no view privs)
    %%                \     |     /
    %%                 \    |    /
    %%                  \   |   /
    %%                    User1 (view privs)
    %%
    %%      <<user>>
    %%      NonAdmin

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G4} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G5} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    oz_test_utils:group_add_user(Config, G4, U2),
    oz_test_utils:group_set_user_privileges(Config, G4, U2, [], [?GROUP_VIEW]),

    oz_test_utils:group_add_group(Config, G1, G2),
    oz_test_utils:group_add_group(Config, G1, G4),
    oz_test_utils:group_add_group(Config, G2, G3),
    oz_test_utils:group_add_group(Config, G4, G3),
    oz_test_utils:group_add_group(Config, G5, G4),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % {GroupId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}
    ExpectedMembershipIntermediaries = [
        {G1, U1, [U1, U2], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY},
            {od_group, G2},
            {od_group, G4}
        ])},
        {G1, U2, [U1, U2], ordsets:from_list([
            {od_group, G4}
        ])},

        {G2, U1, [U1], ordsets:from_list([
            {od_group, G3}
        ])},
        {G3, U1, [U1], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY}
        ])},

        {G4, U1, [U1], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY},
            {od_group, G3}
        ])},
        {G4, U2, [U1, U2], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY}
        ])},

        {G5, U1, [U1, U2], ordsets:from_list([
            {od_group, G4}
        ])},
        {G5, U2, [U1, U2], ordsets:from_list([
            {od_group, G4}
        ])}
    ],

    lists:foreach(fun({GroupId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}) ->
        ExpIntermediaries = lists:map(fun({Type, Id}) ->
            #{<<"type">> => gs_protocol_plugin:encode_entity_type(Type), <<"id">> => Id}
        end, ExpIntermediariesRaw),
        CorrectUserClients = [{user, U} || U <- CorrectUsers],
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_GROUPS_VIEW]}
                ] ++ CorrectUserClients,
                unauthorized = [nobody],
                forbidden = [{user, NonAdmin}, {user, U1}, {user, U2}] -- CorrectUserClients
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/groups/">>, GroupId, <<"/effective_users/">>, SubjectUser, <<"/membership">>],
                expected_code = ?HTTP_200_OK,
                expected_body = #{<<"intermediaries">> => ExpIntermediaries}
            },
            logic_spec = #logic_spec{
                module = group_logic,
                function = get_eff_user_membership_intermediaries,
                args = [client, GroupId, SubjectUser],
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
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().
