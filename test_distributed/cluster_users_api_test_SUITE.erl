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
-include_lib("ctool/include/errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
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
    {ok, Creator} = oz_test_utils:create_user(Config),
    {ok, EffectiveUser} = oz_test_utils:create_user(Config),
    {ok, EffectiveUserWithoutAddUserPriv} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, Creator, ?PROVIDER_NAME1),
    ClusterId = ProviderId,

    % EffectiveUser belongs to cluster C1 effectively via SubGroup1, with the
    % effective privilege to ADD_USER, so he should be able to add himself to the cluster
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:cluster_add_group(Config, ClusterId, SubGroup1),
    oz_test_utils:cluster_set_group_privileges(Config, ClusterId, SubGroup1, [?CLUSTER_ADD_USER], []),

    % EffectiveUserWithoutAddUserPriv belongs to group C1 effectively via SubGroup2,
    % but without the effective privilege to ADD_USER, so he should NOT be able
    % to add himself to the cluster
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutAddUserPriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:cluster_add_group(Config, ClusterId, SubGroup2),
    oz_test_utils:cluster_set_group_privileges(Config, ClusterId, SubGroup2, [], [?CLUSTER_ADD_USER]),

    lists:foreach(fun({ClientClassification, SubjectUser}) ->
        VerifyEndFun = fun
            (true = _ShouldSucceed, _, _) ->
                {ok, Users} = oz_test_utils:cluster_get_users(Config, ClusterId),
                ?assert(lists:member(SubjectUser, Users)),
                oz_test_utils:cluster_remove_user(Config, ClusterId, SubjectUser);
            (false = _ShouldSucceed, _, _) ->
                {ok, Users} = oz_test_utils:cluster_get_users(Config, ClusterId),
                ?assertNot(lists:member(SubjectUser, Users))
        end,

        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = lists:flatten([
                    root,
                    {admin, [?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS]},
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
                path = [<<"/clusters/">>, ClusterId, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_201_CREATED,
                expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config, [<<"/clusters/">>, ClusterId, <<"/users/">>, SubjectUser]),
                    ?assertEqual(ExpLocation, Location),
                    true
                end
            },
            logic_spec = #logic_spec{
                module = cluster_logic,
                function = add_user,
                args = [auth, ClusterId, SubjectUser, data],
                expected_result = ?OK_BINARY(SubjectUser)
            },
            % TODO VFS-4520 Tests for GraphSync API
            data_spec = #data_spec{
                required = [],
                correct_values = #{},
                bad_values = []
            }
        },
        ?assert(api_test_utils:run_tests(
            Config, ApiTestSpec, undefined, undefined, VerifyEndFun
        ))
    end, [{correct, EffectiveUser}, {forbidden, EffectiveUserWithoutAddUserPriv}]).


add_user_with_privileges_test(Config) ->
    {ok, Creator} = oz_test_utils:create_user(Config),
    {ok, EffectiveUser} = oz_test_utils:create_user(Config),
    {ok, EffectiveUserWithoutAddUserPriv} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, Creator, ?PROVIDER_NAME1),
    ClusterId = ProviderId,

    AllPrivs = privileges:cluster_privileges(),

    % EffectiveUser belongs to cluster C1 effectively via SubGroup1, with the
    % effective privilege to ADD_USER, so he should be able to add himself to the cluster
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:cluster_add_group(Config, ClusterId, SubGroup1),
    oz_test_utils:cluster_set_group_privileges(Config, ClusterId, SubGroup1, [?CLUSTER_ADD_USER, ?CLUSTER_SET_PRIVILEGES], []),

    % EffectiveUserWithoutAddUserPriv belongs to group C1 effectively via SubGroup2,
    % but without the effective privilege to ADD_USER, so he should NOT be able
    % to add himself to the cluster
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutAddUserPriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:cluster_add_group(Config, ClusterId, SubGroup2),

    lists:foreach(fun({ClientClassification, SubjectUser}) ->
        VerifyEndFun = fun
            (true = _ShouldSucceed, _, Data) ->
                Privs = lists:sort(maps:get(<<"privileges">>, Data)),
                {ok, ActualPrivs} = oz_test_utils:cluster_get_user_privileges(
                    Config, ClusterId, SubjectUser
                ),
                ?assertEqual(Privs, lists:sort(ActualPrivs)),
                oz_test_utils:cluster_remove_user(Config, ClusterId, SubjectUser);
            (false = _ShouldSucceed, _, _) ->
                {ok, Users} = oz_test_utils:cluster_get_users(Config, ClusterId),
                ?assertNot(lists:member(SubjectUser, Users))
        end,

        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = lists:flatten([
                    root,
                    {admin, [?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS, ?OZ_CLUSTERS_SET_PRIVILEGES]},
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
                path = [<<"/clusters/">>, ClusterId, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_201_CREATED,
                expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config, [<<"/clusters/">>, ClusterId, <<"/users/">>, SubjectUser]),
                    ?assertEqual(ExpLocation, Location),
                    true
                end
            },
            logic_spec = #logic_spec{
                module = cluster_logic,
                function = add_user,
                args = [auth, ClusterId, SubjectUser, data],
                expected_result = ?OK_BINARY(SubjectUser)
            },
            % TODO VFS-4520 Tests for GraphSync API
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
        ))
    end, [{correct, EffectiveUser}, {forbidden, EffectiveUserWithoutAddUserPriv}]).


create_user_invite_token_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_ADD_USER privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Token}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_ADD_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_CLUSTERS_ADD_RELATIONSHIPS]},
                {user, U2},
                {provider, P1, P1Token}
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
            args = [auth, C1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


remove_user_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_REMOVE_USER privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Token}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_REMOVE_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, U3} = oz_test_utils:create_user(Config),
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
                {provider, P1, P1Token}
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
            args = [auth, C1, userId],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_users_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_VIEW privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Token}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_VIEW
    ),
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, U3} = oz_test_utils:cluster_add_user(Config, C1, U3),
    ExpUsers = [U1, U2, U3],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_LIST_RELATIONSHIPS]},
                {user, U2},
                {user, U3},
                {provider, P1, P1Token}
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
            args = [auth, C1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_user_test(Config) ->
    CreatorData = #{<<"fullName">> => <<"creator">>, <<"username">> => <<"creator">>},
    {ok, Creator} = oz_test_utils:create_user(Config, CreatorData),
    {ok, MemberWithViewPrivs} = oz_test_utils:create_user(Config),
    {ok, MemberWithoutViewPrivs} = oz_test_utils:create_user(Config),
    MemberData = #{<<"fullName">> => <<"member">>, <<"username">> => <<"member">>},
    {ok, Member} = oz_test_utils:create_user(Config, MemberData),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config, Creator, ?PROVIDER_NAME1),
    ClusterId = ProviderId,

    {ok, _} = oz_test_utils:cluster_add_user(Config, ClusterId, MemberWithViewPrivs),
    {ok, _} = oz_test_utils:cluster_add_user(Config, ClusterId, MemberWithoutViewPrivs),
    {ok, _} = oz_test_utils:cluster_add_user(Config, ClusterId, Member),

    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, MemberWithViewPrivs, [?CLUSTER_VIEW], []),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, MemberWithoutViewPrivs, [], [?CLUSTER_VIEW]),

    % Shared data about creator should be available even if he is not longer in the cluster
    oz_test_utils:cluster_remove_user(Config, ClusterId, Creator),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    lists:foreach(fun({SubjectUser, UserData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_VIEW]},
                    {user, SubjectUser},
                    {user, MemberWithViewPrivs},
                    {provider, ProviderId, ProviderToken}
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
                expected_body = api_test_expect:shared_user(rest, SubjectUser, UserData)
            },
            logic_spec = #logic_spec{
                module = cluster_logic,
                function = get_user,
                args = [auth, ClusterId, SubjectUser],
                expected_result = api_test_expect:shared_user(logic, SubjectUser, UserData)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = SubjectUser, aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_CLUSTER(ClusterId),
                expected_result = api_test_expect:shared_user(gs, SubjectUser, UserData)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, [{Creator, CreatorData}, {Member, MemberData}]).


get_user_privileges_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_VIEW_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Token}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:cluster_add_user(Config, C1, U3),

    AllPrivs = privileges:cluster_privileges(),
    InitialPrivs = privileges:cluster_member(),
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
                {provider, P1, P1Token},
                % user can always see his own privileges
                {user, U3}
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
            args = [auth, C1, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?CLUSTER_VIEW_PRIVILEGES, false, U3
    ])).


update_user_privileges_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_SET_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Token}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_SET_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to update user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),
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
                {provider, P1, P1Token}
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
            args = [auth, C1, U3, data],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?CLUSTER_SET_PRIVILEGES
    ])).


list_eff_users_test(Config) ->
    {
        C1, _Groups, [{U3, _}, {U4, _}, {U5, _}, {U6, _}],
        {U1, U2, NonAdmin}, {P1, P1Token}
    } = api_test_scenarios:create_cluster_eff_users_env(Config),


    ExpUsers = [U1, U2, U3, U4, U5, U6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_CLUSTERS_LIST_RELATIONSHIPS]},
                {provider, P1, P1Token}
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
            args = [auth, C1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO VFS-4520 Tests for GraphSync API
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
        C1, _Groups, EffUsers, {U1, U2, NonAdmin}, {P1, P1Token}
    } = api_test_scenarios:create_cluster_eff_users_env(Config),

    lists:foreach(fun({UserId, UserData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_VIEW]},
                    {user, U2},
                    {provider, P1, P1Token}
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
                expected_body = api_test_expect:shared_user(rest, UserId, UserData)
            },
            logic_spec = #logic_spec{
                module = cluster_logic,
                function = get_eff_user,
                args = [auth, C1, UserId],
                expected_result = api_test_expect:shared_user(logic, UserId, UserData)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = UserId,
                    aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_CLUSTER(C1),
                expected_result = api_test_expect:shared_user(gs, UserId, UserData)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffUsers).


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
    %   U2 gets the CLUSTER_VIEW_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Token}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, SubjectUser} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:cluster_add_group(Config, C1, G1),
    {ok, G2} = oz_test_utils:cluster_add_group(Config, C1, G2),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),
    {ok, SubjectUser} = oz_test_utils:group_add_user(Config, G3, SubjectUser),
    {ok, SubjectUser} = oz_test_utils:group_add_user(Config, G2, SubjectUser),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:cluster_privileges(),
    InitialPrivs = privileges:cluster_member(),
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
                {provider, P1, P1Token},
                % user can always see his own privileges
                {user, SubjectUser}
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
                <<"/clusters/">>, C1, <<"/effective_users/">>, SubjectUser,
                <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_eff_user_privileges,
            args = [auth, C1, SubjectUser],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, SubjectUser}, ?CLUSTER_VIEW_PRIVILEGES, false, SubjectUser
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

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, undefined, ?PROVIDER_NAME1),
    C1 = P1,
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
    C2 = P2,
    {ok, {P3, P3Token}} = oz_test_utils:create_provider(Config, undefined, ?PROVIDER_NAME1),
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
        {C1, U1, [{provider, P1, P1Token}], [U1], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY},
            {od_group, G1},
            {od_group, G2}
        ])},

        {C2, U1, [{provider, P2, P2Token}], [U1], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY},
            {od_group, G2},
            {od_group, G3}
        ])},

        {C3, U1, [{provider, P3, P3Token}], [U1], ordsets:from_list([
            {od_group, G3}
        ])},
        {C3, U2, [{provider, P3, P3Token}], [U1, U2], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY}
        ])}
    ],

    lists:foreach(fun({ClusterId, SubjectUser, CorrectProviderClients, CorrectUsers, ExpIntermediariesRaw}) ->
        ExpIntermediaries = lists:map(fun({Type, Id}) ->
            #{<<"type">> => gri:serialize_type(Type), <<"id">> => Id}
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
                    {provider, P1, P1Token}, {provider, P2, P2Token}, {provider, P3, P3Token}
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
                args = [auth, ClusterId, SubjectUser],
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
    ozt:init_per_suite(Config).

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time().
