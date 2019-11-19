%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning harvester users API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_users_api_test_SUITE).
-author("Michal Stanisz").

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

-define(OZ_NODES(Config), ?config(oz_worker_nodes, Config)).

-export([
    all/0,
    init_per_testcase/2, end_per_testcase/2,
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
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, EffectiveUser} = oz_test_utils:create_user(Config),
    {ok, EffectiveUserWithoutInvitePriv} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),

    % EffectiveUser belongs to harvester H1 effectively via SubGroup1, with the
    % effective privilege to INVITE_USER, so he should be able to join the harvester as a user
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:harvester_add_group(Config, H1, SubGroup1),
    oz_test_utils:harvester_set_group_privileges(Config, H1, SubGroup1, [?HARVESTER_ADD_USER], []),

    % EffectiveUserWithoutInvitePriv belongs to group H1 effectively via SubGroup2,
    % but without the effective privilege to INVITE_USER, so he should NOT be able
    % to join the parent group as a user
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutInvitePriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:harvester_add_group(Config, H1, SubGroup2),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:harvester_get_users(Config, H1),
            ?assert(lists:member(EffectiveUser, Users)),
            oz_test_utils:harvester_remove_user(Config, H1, EffectiveUser);
        (false = _ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:harvester_get_users(Config, H1),
            ?assertNot(lists:member(EffectiveUser, Users))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/harvesters/">>, H1, <<"/users/">>, EffectiveUser],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/harvesters/">>, H1, <<"/users/">>, EffectiveUser]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = add_user,
            args = [auth, H1, EffectiveUser, data],
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
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, EffectiveUser} = oz_test_utils:create_user(Config),
    {ok, EffectiveUserWithoutInvitePriv} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),

    AllPrivs = privileges:harvester_privileges(),

    % EffectiveUser belongs to harvester H1 effectively via SubGroup1, with the
    % effective privilege to INVITE_USER, so he should be able to join the harvester as a user
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:harvester_add_group(Config, H1, SubGroup1),
    oz_test_utils:harvester_set_group_privileges(Config, H1, SubGroup1, [?HARVESTER_ADD_USER, ?HARVESTER_SET_PRIVILEGES], []),

    % EffectiveUserWithoutInvitePriv belongs to group H1 effectively via SubGroup2,
    % but without the effective privilege to INVITE_USER, so he should NOT be able
    % to join the parent group as a user
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutInvitePriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:harvester_add_group(Config, H1, SubGroup2),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, Data) ->
            Privs = lists:sort(maps:get(<<"privileges">>, Data)),
            {ok, ActualPrivs} = oz_test_utils:harvester_get_user_privileges(
                Config, H1, EffectiveUser
            ),
            ?assertEqual(Privs, lists:sort(ActualPrivs)),
            oz_test_utils:harvester_remove_user(Config, H1, EffectiveUser);
        (false = ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:harvester_get_users(Config, H1),
            ?assertEqual(lists:member(EffectiveUser, Users), ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS, ?OZ_HARVESTERS_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/harvesters/">>, H1, <<"/users/">>, EffectiveUser],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/harvesters/">>, H1, <<"/users/">>, EffectiveUser]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = add_user,
            args = [auth, H1, EffectiveUser, data],
            expected_result = ?OK_BINARY(EffectiveUser)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [?HARVESTER_UPDATE],
                    [?HARVESTER_VIEW]
                ]
            },
            bad_values = [
                {<<"privileges">>, <<"">>,
                    ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>)},
                {<<"privileges">>, [?HARVESTER_VIEW, ?GROUP_VIEW],
                    ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"privileges">>, AllPrivs)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )).


create_user_invite_token_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_ADD_USER privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_ADD_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_HARVESTERS_ADD_RELATIONSHIPS]},
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
            path = [<<"/harvesters/">>, H1, <<"/users/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = create_user_invite_token,
            args = [auth, H1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


remove_user_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_REMOVE_USER privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_REMOVE_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, U3} = oz_test_utils:create_user(Config),
        {ok, U3} = oz_test_utils:harvester_add_user(Config, H1, U3),
        #{userId => U3}
    end,
    DeleteEntityFun = fun(#{userId := User} = _Env) ->
        oz_test_utils:harvester_remove_user(Config, H1, User)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := User} = _Env, _) ->
        {ok, Users} = oz_test_utils:harvester_get_users(Config, H1),
        ?assertEqual(lists:member(User, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS]},
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
            path = [<<"/harvesters/">>, H1, <<"/users/">>, userId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = remove_user,
            args = [auth, H1, userId],
            expected_result = ?OK_RES
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_users_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_VIEW privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_VIEW
    ),
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, U3} = oz_test_utils:harvester_add_user(Config, H1, U3),
    ExpUsers = [U1, U2, U3],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_LIST_RELATIONSHIPS]},
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
            path = [<<"/harvesters/">>, H1, <<"/users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_users,
            args = [auth, H1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_user_test(Config) ->
    {ok, Creator} = oz_test_utils:create_user(Config, #{
        <<"fullName">> => <<"creator">>, <<"username">> => <<"creator">>
    }),
    {ok, MemberWithViewPrivs} = oz_test_utils:create_user(Config),
    {ok, MemberWithoutViewPrivs} = oz_test_utils:create_user(Config),
    {ok, Member} = oz_test_utils:create_user(Config, #{
        <<"fullName">> => <<"member">>, <<"username">> => <<"member">>
    }),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    oz_test_utils:user_set_oz_privileges(Config, Creator, [?OZ_HARVESTERS_CREATE], []),
    {ok, Harvester} = oz_test_utils:create_harvester(Config, ?USER(Creator), ?HARVESTER_CREATE_DATA),
    {ok, _} = oz_test_utils:harvester_add_user(Config, Harvester, MemberWithViewPrivs),
    {ok, _} = oz_test_utils:harvester_add_user(Config, Harvester, MemberWithoutViewPrivs),
    {ok, _} = oz_test_utils:harvester_add_user(Config, Harvester, Member),

    oz_test_utils:harvester_set_user_privileges(Config, Harvester, MemberWithViewPrivs, [?HARVESTER_VIEW], []),
    oz_test_utils:harvester_set_user_privileges(Config, Harvester, MemberWithoutViewPrivs, [], [?HARVESTER_VIEW]),

    % Shared data about creator should be available even if he is not longer in the harvester
    oz_test_utils:harvester_remove_user(Config, Harvester, Creator),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    lists:foreach(fun({SubjectUser, ExpFullName, ExpUsername}) ->
        ExpUserDetails = #{
            <<"username">> => ExpUsername,
            <<"fullName">> => ExpFullName
        },
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_VIEW]},
                    {user, SubjectUser},
                    {user, MemberWithViewPrivs}
                ] ++ case SubjectUser of
                         % Every member of the harvester should be able to see the creator details
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
                path = [<<"/harvesters/">>, Harvester, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_200_OK,
                expected_body = ExpUserDetails#{
                    <<"userId">> => SubjectUser,

                    % TODO VFS-4506 deprecated, included for backward compatibility
                    <<"name">> => ExpFullName,
                    <<"login">> => ExpUsername,
                    <<"alias">> => ExpUsername
                }
            },
            logic_spec = #logic_spec{
                module = harvester_logic,
                function = get_user,
                args = [auth, Harvester, SubjectUser],
                expected_result = ?OK_MAP_CONTAINS(ExpUserDetails)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = SubjectUser, aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_HARVESTER(Harvester),
                expected_result = ?OK_MAP_CONTAINS(ExpUserDetails#{
                    <<"gri">> => fun(EncodedGri) ->
                        ?assertMatch(
                            #gri{id = SubjectUser},
                            gri:deserialize(EncodedGri)
                        )
                    end,

                    % TODO VFS-4506 deprecated, included for backward compatibility
                    <<"name">> => ExpFullName,
                    <<"login">> => ExpUsername,
                    <<"alias">> => ExpUsername
                })
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, [{Creator, <<"creator">>, <<"creator">>}, {Member, <<"member">>, <<"member">>}]).


get_user_privileges_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_VIEW_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:harvester_add_user(Config, H1, U3),

    AllPrivs = privileges:harvester_privileges(),
    InitialPrivs = privileges:harvester_member(),
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:harvester_set_user_privileges(
            Config, H1, U3, PrivsToGrant, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_VIEW_PRIVILEGES]},
                {user, U2},
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
            path = [<<"/harvesters/">>, H1, <<"/users/">>, U3, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_user_privileges,
            args = [auth, H1, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?HARVESTER_VIEW_PRIVILEGES, false, U3
    ])).


update_user_privileges_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_SET_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_SET_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to update user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:harvester_add_user(Config, H1, U3),

    AllPrivs = privileges:harvester_privileges(),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:harvester_set_user_privileges(
            Config, H1, U3, PrivsToGrant, PrivsToRevoke
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:harvester_get_user_privileges(Config, H1, U3),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_SET_PRIVILEGES]},
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
            path = [<<"/harvesters/">>, H1, <<"/users/">>, U3, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = update_user_privileges,
            args = [auth, H1, U3, data],
            expected_result = ?OK_RES
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?HARVESTER_SET_PRIVILEGES
    ])).


list_eff_users_test(Config) ->
    {
        H1, _Groups, [{U3, _}, {U4, _}, {U5, _}, {U6, _}], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_harvester_eff_users_env(Config),


    ExpUsers = [U1, U2, U3, U4, U5, U6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_HARVESTERS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/harvesters/">>, H1, <<"/effective_users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_eff_users,
            args = [auth, H1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check also harvester_logic:has_eff_user function
    lists:foreach(
        fun(UserId) ->
            ?assert(oz_test_utils:call_oz(
                Config, harvester_logic, has_eff_user, [H1, UserId])
            )
        end, ExpUsers
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, harvester_logic, has_eff_user, [H1, <<"asdiucyaie827346w">>])
    ).


get_eff_user_test(Config) ->
    {
        H1, _Groups, EffUsers, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_harvester_eff_users_env(Config),

    lists:foreach(
        fun({UserId, UserDetails}) ->

            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {admin, [?OZ_USERS_VIEW]},
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
                        <<"/harvesters/">>, H1, <<"/effective_users/">>, UserId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = UserDetails#{
                        <<"userId">> => UserId,

                        % TODO VFS-4506 deprecated, included for backward compatibility
                        <<"name">> => maps:get(<<"fullName">>, UserDetails),
                        <<"login">> => maps:get(<<"username">>, UserDetails),
                        <<"alias">> => maps:get(<<"username">>, UserDetails)
                    }
                },
                logic_spec = #logic_spec{
                    module = harvester_logic,
                    function = get_eff_user,
                    args = [auth, H1, UserId],
                    expected_result = ?OK_MAP_CONTAINS(UserDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_user, id = UserId,
                        aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_HARVESTER(H1),
                    expected_result = ?OK_MAP_CONTAINS(UserDetails#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = gri:deserialize(EncodedGri),
                            ?assertEqual(Id, UserId)
                        end,

                        % TODO VFS-4506 deprecated, included for backward compatibility
                        <<"name">> => maps:get(<<"fullName">>, UserDetails),
                        <<"login">> => maps:get(<<"username">>, UserDetails),
                        <<"alias">> => maps:get(<<"username">>, UserDetails)
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffUsers
    ).


get_eff_user_privileges_test(Config) ->
    %% Create environment with the following relations:
    %%
    %%                  Harvester
    %%                 /  ||    \
    %%                /   ||     \
    %% [~harvester_view]  ||  [harvester_view]
    %%             /      ||       \
    %%         User1     /  \      User2
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

    % create harvester with 2 users:
    %   U2 gets the HARVESTER_VIEW_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose eff privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:harvester_add_group(Config, H1, G1),
    {ok, G2} = oz_test_utils:harvester_add_group(Config, H1, G2),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),
    {ok, U3} = oz_test_utils:group_add_user(Config, G3, U3),
    {ok, U3} = oz_test_utils:group_add_user(Config, G2, U3),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:harvester_privileges(),
    InitialPrivs = privileges:harvester_member(),
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

        oz_test_utils:harvester_set_group_privileges(Config, H1, G1, PrivsToGrant1, PrivsToRevoke),
        oz_test_utils:harvester_set_group_privileges(Config, H1, G2, PrivsToGrant2, PrivsToRevoke)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_VIEW_PRIVILEGES]},
                {user, U2},
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
            path = [
                <<"/harvesters/">>, H1, <<"/effective_users/">>, U3,
                <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_eff_user_privileges,
            args = [auth, H1, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?HARVESTER_VIEW_PRIVILEGES, false, U3
    ])).


get_eff_user_membership_intermediaries(Config) ->
    %% Create environment with the following relations:
    %%
    %%      Harvester1       Harvester2     Harvester3
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
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    {ok, H2} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    {ok, H3} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),

    oz_test_utils:harvester_add_user(Config, H1, U1),
    oz_test_utils:harvester_add_user(Config, H3, U2),
    oz_test_utils:harvester_set_user_privileges(Config, H3, U2, [], [?HARVESTER_VIEW]),

    oz_test_utils:group_add_group(Config, G2, G1),
    oz_test_utils:group_add_group(Config, G3, G1),

    oz_test_utils:harvester_add_group(Config, H3, G3),
    oz_test_utils:harvester_add_group(Config, H1, G1),
    oz_test_utils:harvester_add_group(Config, H1, G2),
    oz_test_utils:harvester_add_group(Config, H2, G2),
    oz_test_utils:harvester_add_group(Config, H2, G3),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % {HarvesterId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}
    ExpectedMembershipIntermediaries = [
        {H1, U1, [U1], ordsets:from_list([
            {od_harvester, ?SELF_INTERMEDIARY},
            {od_group, G1},
            {od_group, G2}
        ])},

        {H2, U1, [U1], ordsets:from_list([
            {od_harvester, ?SELF_INTERMEDIARY},
            {od_group, G2},
            {od_group, G3}
        ])},

        {H3, U1, [U1], ordsets:from_list([
            {od_group, G3}
        ])},
        {H3, U2, [U1, U2], ordsets:from_list([
            {od_harvester, ?SELF_INTERMEDIARY}
        ])}
    ],

    lists:foreach(fun({HarvesterId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}) ->
        ExpIntermediaries = lists:map(fun({Type, Id}) ->
            #{<<"type">> => gri:serialize_type(Type, regular), <<"id">> => Id}
        end, ExpIntermediariesRaw),
        CorrectUserClients = [{user, U} || U <- CorrectUsers],
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_HARVESTERS_VIEW]}
                ] ++ CorrectUserClients,
                unauthorized = [nobody],
                forbidden = [{user, NonAdmin}, {user, U1}, {user, U2}] -- CorrectUserClients
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/harvesters/">>, HarvesterId, <<"/effective_users/">>, SubjectUser, <<"/membership">>],
                expected_code = ?HTTP_200_OK,
                expected_body = #{<<"intermediaries">> => ExpIntermediaries}
            },
            logic_spec = #logic_spec{
                module = harvester_logic,
                function = get_eff_user_membership_intermediaries,
                args = [auth, HarvesterId, SubjectUser],
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

init_per_testcase(_, Config) ->
    oz_test_utils:mock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN).

end_per_testcase(_, Config) ->
    oz_test_utils:unmock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN).

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

