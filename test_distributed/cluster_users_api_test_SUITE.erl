%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning cluster users API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_users_api_test_SUITE).
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
-include_lib("ctool/include/api_errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    add_user_test/1,
    add_user_with_privileges_test/1,
    create_user_invite_token_test/1,
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
        create_user_invite_token_test,
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


%%%===================================================================
%%% Test functions
%%%===================================================================


add_user_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, EffectiveUser} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, EffectiveUserWithoutInvitePriv} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
    ClusterId = ProviderId,

    % EffectiveUser belongs to cluster C1 effectively via SubGroup1, with the
    % effective privilege to ADD_USER, so he should be able to join the cluster as a user
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:cluster_add_group(Config, ClusterId, SubGroup1),
    oz_test_utils:cluster_set_group_privileges(Config, ClusterId, SubGroup1, [?CLUSTER_ADD_USER], []),

    % EffectiveUserWithoutInvitePriv belongs to group C1 effectively via SubGroup2,
    % but without the effective privilege to ADD_USER, so he should NOT be able
    % to join the parent group as a user
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutInvitePriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:cluster_add_group(Config, ClusterId, SubGroup2),
    oz_test_utils:cluster_set_group_privileges(Config, ClusterId, SubGroup2, [], [?CLUSTER_ADD_USER]),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:cluster_get_users(Config, ClusterId),
            ?assert(lists:member(EffectiveUser, Users)),
            oz_test_utils:cluster_remove_user(Config, ClusterId, EffectiveUser);
        (false = _ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:cluster_get_users(Config, ClusterId),
            ?assertNot(lists:member(EffectiveUser, Users))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/clusters/">>, ClusterId, <<"/users/">>, EffectiveUser],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/clusters/">>, ClusterId, <<"/users/">>, EffectiveUser]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = add_user,
            args = [client, ClusterId, EffectiveUser, data],
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

    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
    ClusterId = ProviderId,

    AllPrivs = privileges:cluster_privileges(),

    % EffectiveUser belongs to cluster C1 effectively via SubGroup1, with the
    % effective privilege to ADD_USER, so he should be able to join the cluster as a user
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:cluster_add_group(Config, ClusterId, SubGroup1),
    oz_test_utils:cluster_set_group_privileges(Config, ClusterId, SubGroup1, [?CLUSTER_ADD_USER, ?CLUSTER_SET_PRIVILEGES], []),

    % EffectiveUserWithoutInvitePriv belongs to group C1 effectively via SubGroup2,
    % but without the effective privilege to ADD_USER, so he should NOT be able
    % to join the parent group as a user
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutInvitePriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:cluster_add_group(Config, ClusterId, SubGroup2),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, Data) ->
            Privs = lists:sort(maps:get(<<"privileges">>, Data)),
            {ok, ActualPrivs} = oz_test_utils:cluster_get_user_privileges(
                Config, ClusterId, EffectiveUser
            ),
            ?assertEqual(Privs, lists:sort(ActualPrivs)),
            oz_test_utils:cluster_remove_user(Config, ClusterId, EffectiveUser);
        (false = _ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:cluster_get_users(Config, ClusterId),
            ?assertNot(lists:member(EffectiveUser, Users))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS, ?OZ_CLUSTERS_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/clusters/">>, ClusterId, <<"/users/">>, EffectiveUser],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/clusters/">>, ClusterId, <<"/users/">>, EffectiveUser]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = add_user,
            args = [client, ClusterId, EffectiveUser, data],
            expected_result = ?OK_BINARY(EffectiveUser)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [?CLUSTER_UPDATE, ?CLUSTER_REMOVE_GROUP],
                    [?CLUSTER_ADD_USER, ?CLUSTER_VIEW]
                ]
            },
            bad_values = [
                {<<"privileges">>, <<"">>,
                    ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>)},
                {<<"privileges">>, [?CLUSTER_VIEW, ?GROUP_VIEW],
                    ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"privileges">>, AllPrivs)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )).


create_user_invite_token_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_ADD_USER privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_ADD_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_ADD_RELATIONSHIPS]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/clusters/">>, C1, <<"/users/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = create_user_invite_token,
            args = [client, C1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


remove_user_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_REMOVE_USER privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_REMOVE_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
        {ok, U3} = oz_test_utils:cluster_add_user(Config, C1, U3),
        #{userId => U3}
    end,
    DeleteEntityFun = fun(#{userId := User} = _Env) ->
        oz_test_utils:cluster_remove_user(Config, C1, User)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := User} = _Env, _) ->
        {ok, Users} = oz_test_utils:cluster_get_users(Config, C1),
        ?assertEqual(lists:member(User, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/clusters/">>, C1, <<"/users/">>, userId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = remove_user,
            args = [client, C1, userId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_users_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_VIEW privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_VIEW
    ),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, U3} = oz_test_utils:cluster_add_user(Config, C1, U3),
    ExpUsers = [U1, U2, U3],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_LIST_RELATIONSHIPS]},
                {user, U2},
                {user, U3},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/clusters/">>, C1, <<"/users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_users,
            args = [client, C1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_user_test(Config) ->
    {ok, Creator} = oz_test_utils:create_user(Config, #od_user{name = <<"creator">>, alias = <<"creator">>}),
    {ok, MemberWithViewPrivs} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, MemberWithoutViewPrivs} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Member} = oz_test_utils:create_user(Config, #od_user{name = <<"member">>, alias = <<"member">>}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, {ProviderId, ProviderMacaroon}} = oz_test_utils:create_provider(Config, Creator, ?PROVIDER_NAME1),
    ClusterId = ProviderId,

    {ok, _} = oz_test_utils:cluster_add_user(Config, ClusterId, MemberWithViewPrivs),
    {ok, _} = oz_test_utils:cluster_add_user(Config, ClusterId, MemberWithoutViewPrivs),
    {ok, _} = oz_test_utils:cluster_add_user(Config, ClusterId, Member),

    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, MemberWithViewPrivs, [?CLUSTER_VIEW], []),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, MemberWithoutViewPrivs, [], [?CLUSTER_VIEW]),

    % Shared data about creator should be available even if he is not longer in the cluster
    oz_test_utils:cluster_remove_user(Config, ClusterId, Creator),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    lists:foreach(fun({SubjectUser, ExpName, ExpAlias}) ->
        ExpUserDetails = #{
            <<"alias">> => ExpAlias,
            <<"name">> => ExpName
        },
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_VIEW]},
                    {user, SubjectUser},
                    {user, MemberWithViewPrivs},
                    {provider, ProviderId, ProviderMacaroon}
                ] ++ case SubjectUser of
                    % Every member of the cluster should be able to see the creator details
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
                path = [<<"/clusters/">>, ClusterId, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_200_OK,
                expected_body = ExpUserDetails#{
                    <<"userId">> => SubjectUser,
                    % TODO VFS-4506 deprecated, included for backward compatibility
                    <<"login">> => ExpAlias
                }
            },
            logic_spec = #logic_spec{
                module = cluster_logic,
                function = get_user,
                args = [client, ClusterId, SubjectUser],
                expected_result = ?OK_MAP_CONTAINS(ExpUserDetails)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = SubjectUser, aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_CLUSTER(ClusterId),
                expected_result = ?OK_MAP(ExpUserDetails#{
                    <<"gri">> => fun(EncodedGri) ->
                        ?assertMatch(
                            #gri{id = SubjectUser},
                            oz_test_utils:decode_gri(Config, EncodedGri)
                        )
                    end,
                    % TODO VFS-4506 deprecated, included for backward compatibility
                    <<"login">> => maps:get(<<"alias">>, ExpUserDetails)
                })
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, [{Creator, <<"creator">>, <<"creator">>}, {Member, <<"member">>, <<"member">>}]).


get_user_privileges_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_VIEW privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:cluster_add_user(Config, C1, U3),

    AllPrivs = privileges:cluster_privileges(),
    InitialPrivs = privileges:cluster_user(),
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:cluster_set_user_privileges(
            Config, C1, U3, PrivsToGrant, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_VIEW_PRIVILEGES]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/clusters/">>, C1, <<"/users/">>, U3, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_user_privileges,
            args = [client, C1, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?CLUSTER_VIEW_PRIVILEGES
    ])).


update_user_privileges_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_SET_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_SET_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to update user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:cluster_add_user(Config, C1, U3),

    AllPrivs = privileges:cluster_privileges(),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:cluster_set_user_privileges(
            Config, C1, U3, PrivsToGrant, PrivsToRevoke
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:cluster_get_user_privileges(Config, C1, U3),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_SET_PRIVILEGES]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/clusters/">>, C1, <<"/users/">>, U3, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = update_user_privileges,
            args = [client, C1, U3, data],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?CLUSTER_SET_PRIVILEGES
    ])).


list_eff_users_test(Config) ->
    {
        C1, _Groups, [{U3, _}, {U4, _}, {U5, _}, {U6, _}],
        {U1, U2, NonAdmin}, {P1, P1Macaroon}
    } = api_test_scenarios:create_cluster_eff_users_env(Config),


    ExpUsers = [U1, U2, U3, U4, U5, U6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_CLUSTERS_LIST_RELATIONSHIPS]},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/clusters/">>, C1, <<"/effective_users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_eff_users,
            args = [client, C1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check also cluster_logic:has_eff_user function
    lists:foreach(
        fun(UserId) ->
            ?assert(oz_test_utils:call_oz(
                Config, cluster_logic, has_eff_user, [C1, UserId])
            )
        end, ExpUsers
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, cluster_logic, has_eff_user, [C1, <<"asdiucyaie827346w">>])
    ).


get_eff_user_test(Config) ->
    {
        C1, _Groups, EffUsers, {U1, U2, NonAdmin}, {P1, P1Macaroon}
    } = api_test_scenarios:create_cluster_eff_users_env(Config),

    lists:foreach(
        fun({UserId, UserDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {admin, [?OZ_USERS_VIEW]},
                        {user, U2},
                        {provider, P1, P1Macaroon}
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
                        <<"/clusters/">>, C1, <<"/effective_users/">>, UserId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = UserDetails#{
                        <<"userId">> => UserId,
                        % TODO VFS-4506 deprecated, included for backward compatibility
                        <<"login">> => maps:get(<<"alias">>, UserDetails)
                    }
                },
                logic_spec = #logic_spec{
                    module = cluster_logic,
                    function = get_eff_user,
                    args = [client, C1, UserId],
                    expected_result = ?OK_MAP_CONTAINS(UserDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_user, id = UserId,
                        aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_CLUSTER(C1),
                    expected_result = ?OK_MAP(UserDetails#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Id, UserId)
                        end,
                        % TODO VFS-4506 deprecated, included for backward compatibility
                        <<"login">> => maps:get(<<"alias">>, UserDetails)
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffUsers
    ).


get_eff_user_privileges_test(Config) ->
    %% Create environment with the following relations:
    %%
    %%                  Cluster
    %%                 /  ||  \
    %%                /   ||   \
    %%     [~cluster_view]  ||  [cluster_view]
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
    %%                     User3
    %%      <<user>>
    %%      NonAdmin

    % create cluster with 2 users:
    %   U2 gets the CLUSTER_VIEW privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose eff privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:cluster_add_group(Config, C1, G1),
    {ok, G2} = oz_test_utils:cluster_add_group(Config, C1, G2),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),
    {ok, U3} = oz_test_utils:group_add_user(Config, G3, U3),
    {ok, U3} = oz_test_utils:group_add_user(Config, G2, U3),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:cluster_privileges(),
    InitialPrivs = privileges:cluster_user(),
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

        oz_test_utils:cluster_set_group_privileges(Config, C1, G1, PrivsToGrant1, PrivsToRevoke),
        oz_test_utils:cluster_set_group_privileges(Config, C1, G2, PrivsToGrant2, PrivsToRevoke)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_VIEW_PRIVILEGES]},
                {user, U2},
                {provider, P1, P1Macaroon}
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
                <<"/clusters/">>, C1, <<"/effective_users/">>, U3,
                <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_eff_user_privileges,
            args = [client, C1, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?CLUSTER_VIEW_PRIVILEGES
    ])).


get_eff_user_membership_intermediaries(Config) ->
    %% Create environment with the following relations:
    %%
    %%      Cluster1       Cluster2     Cluster3
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

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),

    {ok, {P1, P1Macaroon}} = oz_test_utils:create_provider(Config, undefined, ?PROVIDER_NAME1),
    C1 = P1,
    {ok, {P2, P2Macaroon}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
    C2 = P2,
    {ok, {P3, P3Macaroon}} = oz_test_utils:create_provider(Config, undefined, ?PROVIDER_NAME1),
    C3 = P3,

    oz_test_utils:cluster_add_user(Config, C1, U1),
    oz_test_utils:cluster_add_user(Config, C3, U2),
    oz_test_utils:cluster_set_user_privileges(Config, C3, U2, [], [?CLUSTER_VIEW]),

    oz_test_utils:group_add_group(Config, G2, G1),
    oz_test_utils:group_add_group(Config, G3, G1),

    oz_test_utils:cluster_add_group(Config, C3, G3),
    oz_test_utils:cluster_add_group(Config, C1, G1),
    oz_test_utils:cluster_add_group(Config, C1, G2),
    oz_test_utils:cluster_add_group(Config, C2, G2),
    oz_test_utils:cluster_add_group(Config, C2, G3),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % {ClusterId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}
    ExpectedMembershipIntermediaries = [
        {C1, U1, [{provider, P1, P1Macaroon}], [U1], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY},
            {od_group, G1},
            {od_group, G2}
        ])},

        {C2, U1, [{provider, P2, P2Macaroon}], [U1], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY},
            {od_group, G2},
            {od_group, G3}
        ])},

        {C3, U1, [{provider, P3, P3Macaroon}], [U1], ordsets:from_list([
            {od_group, G3}
        ])},
        {C3, U2, [{provider, P3, P3Macaroon}], [U1, U2], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY}
        ])}
    ],

    lists:foreach(fun({ClusterId, SubjectUser, CorrectProviderClients, CorrectUsers, ExpIntermediariesRaw}) ->
        ExpIntermediaries = lists:map(fun({Type, Id}) ->
            #{<<"type">> => gs_protocol_plugin:encode_entity_type(Type), <<"id">> => Id}
        end, ExpIntermediariesRaw),
        CorrectUserClients = [{user, U} || U <- CorrectUsers],
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_CLUSTERS_VIEW]}
                ] ++ CorrectUserClients ++ CorrectProviderClients,
                unauthorized = [nobody],
                forbidden = [
                    {user, NonAdmin}, {user, U1}, {user, U2},
                    {provider, P1, P1Macaroon}, {provider, P2, P2Macaroon}, {provider, P3, P3Macaroon}
                ] -- (CorrectUserClients ++ CorrectProviderClients)
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/clusters/">>, ClusterId, <<"/effective_users/">>, SubjectUser, <<"/membership">>],
                expected_code = ?HTTP_200_OK,
                expected_body = #{<<"intermediaries">> => ExpIntermediaries}
            },
            logic_spec = #logic_spec{
                module = cluster_logic,
                function = get_eff_user_membership_intermediaries,
                args = [client, ClusterId, SubjectUser],
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
