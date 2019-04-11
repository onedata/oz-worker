%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning space users API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(space_users_api_test_SUITE).
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

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),

    % EffectiveUser belongs to space S1 effectively via SubGroup1, with the
    % effective privilege to INVITE_USER, so he should be able to join the space as a user
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:space_add_group(Config, S1, SubGroup1),
    oz_test_utils:space_set_group_privileges(Config, S1, SubGroup1, [?SPACE_ADD_USER], []),

    % EffectiveUserWithoutInvitePriv belongs to group S1 effectively via SubGroup2,
    % but without the effective privilege to INVITE_USER, so he should NOT be able
    % to join the parent group as a user
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutInvitePriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:space_add_group(Config, S1, SubGroup2),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:space_get_users(Config, S1),
            ?assert(lists:member(EffectiveUser, Users)),
            oz_test_utils:space_remove_user(Config, S1, EffectiveUser);
        (false = _ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:space_get_users(Config, S1),
            ?assertNot(lists:member(EffectiveUser, Users))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/spaces/">>, S1, <<"/users/">>, EffectiveUser],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/spaces/">>, S1, <<"/users/">>, EffectiveUser]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = add_user,
            args = [client, S1, EffectiveUser, data],
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

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),

    AllPrivs = privileges:space_privileges(),

    % EffectiveUser belongs to space S1 effectively via SubGroup1, with the
    % effective privilege to INVITE_USER, so he should be able to join the space as a user
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:space_add_group(Config, S1, SubGroup1),
    oz_test_utils:space_set_group_privileges(Config, S1, SubGroup1, [?SPACE_ADD_USER, ?SPACE_SET_PRIVILEGES], []),

    % EffectiveUserWithoutInvitePriv belongs to group S1 effectively via SubGroup2,
    % but without the effective privilege to INVITE_USER, so he should NOT be able
    % to join the parent group as a user
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutInvitePriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:space_add_group(Config, S1, SubGroup2),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, Data) ->
            Privs = lists:sort(maps:get(<<"privileges">>, Data)),
            {ok, ActualPrivs} = oz_test_utils:space_get_user_privileges(
                Config, S1, EffectiveUser
            ),
            ?assertEqual(Privs, lists:sort(ActualPrivs)),
            oz_test_utils:space_remove_user(Config, S1, EffectiveUser);
        (false = ShouldSucceed, _, _) ->
            {ok, Users} = oz_test_utils:space_get_users(Config, S1),
            ?assertEqual(lists:member(EffectiveUser, Users), ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS, ?OZ_SPACES_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/spaces/">>, S1, <<"/users/">>, EffectiveUser],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/spaces/">>, S1, <<"/users/">>, EffectiveUser]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = add_user,
            args = [client, S1, EffectiveUser, data],
            expected_result = ?OK_BINARY(EffectiveUser)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [?SPACE_UPDATE, ?SPACE_REMOVE_GROUP],
                    [?SPACE_WRITE_DATA, ?SPACE_VIEW]
                ]
            },
            bad_values = [
                {<<"privileges">>, <<"">>,
                    ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>)},
                {<<"privileges">>, [?SPACE_VIEW, ?GROUP_VIEW],
                    ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"privileges">>, AllPrivs)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )).


create_user_invite_token_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_INVITE_USER privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_ADD_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_ADD_RELATIONSHIPS]},
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
            path = [<<"/spaces/">>, S1, <<"/users/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = create_user_invite_token,
            args = [client, S1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


remove_user_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_REMOVE_USER privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_REMOVE_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
        {ok, U3} = oz_test_utils:space_add_user(Config, S1, U3),
        #{userId => U3}
    end,
    DeleteEntityFun = fun(#{userId := User} = _Env) ->
        oz_test_utils:space_remove_user(Config, S1, User)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := User} = _Env, _) ->
        {ok, Users} = oz_test_utils:space_get_users(Config, S1),
        ?assertEqual(lists:member(User, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS]},
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
            path = [<<"/spaces/">>, S1, <<"/users/">>, userId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = remove_user,
            args = [client, S1, userId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_users_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_VIEW privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, U3} = oz_test_utils:space_add_user(Config, S1, U3),
    ExpUsers = [U1, U2, U3],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_LIST_RELATIONSHIPS]},
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
            path = [<<"/spaces/">>, S1, <<"/users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_users,
            args = [client, S1],
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

    {ok, Space} = oz_test_utils:create_space(Config, ?USER(Creator), ?SPACE_NAME1),
    {ok, _} = oz_test_utils:space_add_user(Config, Space, MemberWithViewPrivs),
    {ok, _} = oz_test_utils:space_add_user(Config, Space, MemberWithoutViewPrivs),
    {ok, _} = oz_test_utils:space_add_user(Config, Space, Member),

    oz_test_utils:space_set_user_privileges(Config, Space, MemberWithViewPrivs, [?SPACE_VIEW], []),
    oz_test_utils:space_set_user_privileges(Config, Space, MemberWithoutViewPrivs, [], [?SPACE_VIEW]),

    % Shared data about creator should be available even if he is not longer in the space
    oz_test_utils:space_remove_user(Config, Space, Creator),

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
                    {user, MemberWithViewPrivs}
                ] ++ case SubjectUser of
                    % Every member of the space should be able to see the creator details
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
                path = [<<"/spaces/">>, Space, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_200_OK,
                expected_body = ExpUserDetails#{
                    <<"userId">> => SubjectUser,
                    % TODO VFS-4506 deprecated, included for backward compatibility
                    <<"login">> => ExpAlias
                }
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = get_user,
                args = [client, Space, SubjectUser],
                expected_result = ?OK_MAP_CONTAINS(ExpUserDetails)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = SubjectUser, aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_SPACE(Space),
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
    % create space with 2 users:
    %   U2 gets the SPACE_VIEW_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:space_add_user(Config, S1, U3),

    AllPrivs = privileges:space_privileges(),
    InitialPrivs = privileges:space_user(),
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:space_set_user_privileges(
            Config, S1, U3, PrivsToGrant, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW_PRIVILEGES]},
                {user, U2},
                % user always can see own privileges
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
            path = [<<"/spaces/">>, S1, <<"/users/">>, U3, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_user_privileges,
            args = [client, S1, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?SPACE_VIEW_PRIVILEGES, U3
    ])).


update_user_privileges_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_SET_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_SET_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to update user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:space_add_user(Config, S1, U3),

    AllPrivs = privileges:space_privileges(),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:space_set_user_privileges(
            Config, S1, U3, PrivsToGrant, PrivsToRevoke
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:space_get_user_privileges(Config, S1, U3),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_SET_PRIVILEGES]},
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
            path = [<<"/spaces/">>, S1, <<"/users/">>, U3, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = update_user_privileges,
            args = [client, S1, U3, data],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?SPACE_SET_PRIVILEGES
    ])).


list_eff_users_test(Config) ->
    {
        S1, _Groups, [{U3, _}, {U4, _}, {U5, _}, {U6, _}], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_space_eff_users_env(Config),


    ExpUsers = [U1, U2, U3, U4, U5, U6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_SPACES_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/effective_users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_eff_users,
            args = [client, S1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check also space_logic:has_eff_user function
    lists:foreach(
        fun(UserId) ->
            ?assert(oz_test_utils:call_oz(
                Config, space_logic, has_eff_user, [S1, UserId])
            )
        end, ExpUsers
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, space_logic, has_eff_user, [S1, <<"asdiucyaie827346w">>])
    ).


get_eff_user_test(Config) ->
    {
        S1, _Groups, EffUsers, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_space_eff_users_env(Config),

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
                        <<"/spaces/">>, S1, <<"/effective_users/">>, UserId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = UserDetails#{
                        <<"userId">> => UserId,
                        % TODO VFS-4506 deprecated, included for backward compatibility
                        <<"login">> => maps:get(<<"alias">>, UserDetails)
                    }
                },
                logic_spec = #logic_spec{
                    module = space_logic,
                    function = get_eff_user,
                    args = [client, S1, UserId],
                    expected_result = ?OK_MAP_CONTAINS(UserDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_user, id = UserId,
                        aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_SPACE(S1),
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
    %%                     User3
    %%      <<user>>
    %%      NonAdmin

    % create space with 2 users:
    %   U2 gets the SPACE_VIEW_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose eff privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:space_add_group(Config, S1, G1),
    {ok, G2} = oz_test_utils:space_add_group(Config, S1, G2),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),
    {ok, U3} = oz_test_utils:group_add_user(Config, G3, U3),
    {ok, U3} = oz_test_utils:group_add_user(Config, G2, U3),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:space_privileges(),
    InitialPrivs = privileges:space_user(),
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

        oz_test_utils:space_set_group_privileges(Config, S1, G1, PrivsToGrant1, PrivsToRevoke),
        oz_test_utils:space_set_group_privileges(Config, S1, G2, PrivsToGrant2, PrivsToRevoke)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW_PRIVILEGES]},
                {user, U2},
                % user always can see own privileges
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
                <<"/spaces/">>, S1, <<"/effective_users/">>, U3,
                <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_eff_user_privileges,
            args = [client, S1, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?SPACE_VIEW_PRIVILEGES, U3
    ])).


get_eff_user_membership_intermediaries(Config) ->
    %% Create environment with the following relations:
    %%
    %%      Space1       Space2     Space3
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

    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S3} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),

    oz_test_utils:space_add_user(Config, S1, U1),
    oz_test_utils:space_add_user(Config, S3, U2),
    oz_test_utils:space_set_user_privileges(Config, S3, U2, [], [?SPACE_VIEW]),

    oz_test_utils:group_add_group(Config, G2, G1),
    oz_test_utils:group_add_group(Config, G3, G1),

    oz_test_utils:space_add_group(Config, S3, G3),
    oz_test_utils:space_add_group(Config, S1, G1),
    oz_test_utils:space_add_group(Config, S1, G2),
    oz_test_utils:space_add_group(Config, S2, G2),
    oz_test_utils:space_add_group(Config, S2, G3),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % {SpaceId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}
    ExpectedMembershipIntermediaries = [
        {S1, U1, [U1], ordsets:from_list([
            {od_space, ?SELF_INTERMEDIARY},
            {od_group, G1},
            {od_group, G2}
        ])},

        {S2, U1, [U1], ordsets:from_list([
            {od_space, ?SELF_INTERMEDIARY},
            {od_group, G2},
            {od_group, G3}
        ])},

        {S3, U1, [U1], ordsets:from_list([
            {od_group, G3}
        ])},
        {S3, U2, [U1, U2], ordsets:from_list([
            {od_space, ?SELF_INTERMEDIARY}
        ])}
    ],

    lists:foreach(fun({SpaceId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}) ->
        ExpIntermediaries = lists:map(fun({Type, Id}) ->
            #{<<"type">> => gs_protocol_plugin:encode_entity_type(Type), <<"id">> => Id}
        end, ExpIntermediariesRaw),
        CorrectUserClients = [{user, U} || U <- CorrectUsers],
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_SPACES_VIEW]}
                ] ++ CorrectUserClients,
                unauthorized = [nobody],
                forbidden = [{user, NonAdmin}, {user, U1}, {user, U2}] -- CorrectUserClients
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/spaces/">>, SpaceId, <<"/effective_users/">>, SubjectUser, <<"/membership">>],
                expected_code = ?HTTP_200_OK,
                expected_body = #{<<"intermediaries">> => ExpIntermediaries}
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = get_eff_user_membership_intermediaries,
                args = [client, SpaceId, SubjectUser],
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
