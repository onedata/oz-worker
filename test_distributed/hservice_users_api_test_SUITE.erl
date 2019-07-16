%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning
%%% handle service users API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(hservice_users_api_test_SUITE).
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
    remove_user_test/1,
    list_users_test/1,
    get_user_test/1,

    get_user_privileges_test/1,
    update_user_privileges_test/1,

    list_eff_users_test/1,
    get_eff_user_test/1,
    get_eff_user_privileges_test/1
]).

all() ->
    ?ALL([
        add_user_test,
        remove_user_test,
        list_users_test,
        get_user_test,

        get_user_privileges_test,
        update_user_privileges_test,

        list_eff_users_test,
        get_eff_user_test,
        get_eff_user_privileges_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


add_user_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_UPDATE privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_UPDATE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:handle_service_privileges(),

    VerifyEndFun =
        fun
            (true = _ShouldSucceed, _, Data) ->
                ExpPrivs = lists:sort(maps:get(<<"privileges">>, Data)),
                {ok, Privs} = oz_test_utils:handle_service_get_user_privileges(
                    Config, HService, U3
                ),
                ?assertEqual(ExpPrivs, lists:sort(Privs)),
                oz_test_utils:handle_service_remove_user(
                    Config, HService, U3
                );
            (false = ShouldSucceed, _, _) ->
                {ok, Users} = oz_test_utils:handle_service_get_users(
                    Config, HService
                ),
                ?assertEqual(lists:member(U3, Users), ShouldSucceed)
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/handle_services/">>, HService, <<"/users/">>, U3],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config,
                    [<<"/handle_services/">>, HService, <<"/users/">>, U3]
                ),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = add_user,
            args = [auth, HService, U3, data],
            expected_result = ?OK_BINARY(U3)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_DELETE],
                    [?HANDLE_SERVICE_REGISTER_HANDLE, ?HANDLE_SERVICE_VIEW]
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


remove_user_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_UPDATE privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_UPDATE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, U3} = oz_test_utils:create_user(Config),
        {ok, U3} = oz_test_utils:handle_service_add_user(
            Config, HService, U3
        ),
        #{userId => U3}
    end,
    DeleteEntityFun = fun(#{userId := User} = _Env) ->
        oz_test_utils:handle_service_remove_user(Config, HService, User)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := User} = _Env, _) ->
        {ok, Users} = oz_test_utils:handle_service_get_users(Config, HService),
        ?assertEqual(lists:member(User, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS]},
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
            path = [<<"/handle_services/">>, HService, <<"/users/">>, userId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = remove_user,
            args = [auth, HService, userId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_users_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_VIEW privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:handle_service_add_user(Config, HService, U3),

    ExpUsers = [U1, U2, U3],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS]},
                {user, U2},
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
            path = [<<"/handle_services/">>, HService, <<"/users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_users,
            args = [auth, HService],
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

    oz_test_utils:user_set_oz_privileges(Config, Creator, [
        ?OZ_HANDLE_SERVICES_CREATE
    ], []),
    {ok, HService} = oz_test_utils:create_handle_service(Config, ?USER(Creator), ?DOI_SERVICE),
    {ok, _} = oz_test_utils:handle_service_add_user(Config, HService, MemberWithViewPrivs),
    {ok, _} = oz_test_utils:handle_service_add_user(Config, HService, MemberWithoutViewPrivs),
    {ok, _} = oz_test_utils:handle_service_add_user(Config, HService, Member),

    oz_test_utils:handle_service_set_user_privileges(
        Config, HService, MemberWithViewPrivs, [?HANDLE_SERVICE_VIEW], []
    ),
    oz_test_utils:handle_service_set_user_privileges(
        Config, HService, MemberWithoutViewPrivs, [], [?HANDLE_SERVICE_VIEW]
    ),

    % Shared data about creator should be available even if he is not longer in the handle_service
    oz_test_utils:handle_service_remove_user(Config, HService, Creator),

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
                    % Every member of the handle service should be able to see the creator details
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
                path = [<<"/handle_services/">>, HService, <<"/users/">>, SubjectUser],
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
                module = handle_service_logic,
                function = get_user,
                args = [auth, HService, SubjectUser],
                expected_result = ?OK_MAP_CONTAINS(ExpUserDetails)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = SubjectUser, aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_HANDLE_SERVICE(HService),
                expected_result = ?OK_MAP_CONTAINS(ExpUserDetails#{
                    <<"gri">> => fun(EncodedGri) ->
                        ?assertMatch(
                            #gri{id = SubjectUser},
                            oz_test_utils:decode_gri(Config, EncodedGri)
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
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_VIEW privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:handle_service_add_user(Config, HService, U3),

    AllPrivs = privileges:handle_service_privileges(),
    InitialPrivs = [?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_REGISTER_HANDLE],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:handle_service_set_user_privileges(
            Config, HService, U3, PrivsToGrant, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_VIEW_PRIVILEGES]},
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
                <<"/handle_services/">>, HService,
                <<"/users/">>, U3, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_user_privileges,
            args = [auth, HService, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?HANDLE_SERVICE_VIEW, false, U3
    ])).


update_user_privileges_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_UPDATE privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_UPDATE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:handle_service_add_user(Config, HService, U3),

    AllPrivs = privileges:handle_service_privileges(),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:handle_service_set_user_privileges(
            Config, HService, U3, PrivsToGrant, PrivsToRevoke
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:handle_service_get_user_privileges(
            Config, HService, U3
        ),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_SET_PRIVILEGES]},
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
                <<"/handle_services/">>, HService,
                <<"/users/">>, U3, <<"/privileges">>
            ],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = update_user_privileges,
            args = [auth, HService, U3, data],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?HANDLE_SERVICE_UPDATE
    ])).


list_eff_users_test(Config) ->
    {
        HService, _Groups, [{U3, _}, {U4, _}, {U5, _}, {U6, _}],
        {U1, U2, NonAdmin}
    } = api_test_scenarios:create_hservice_eff_users_env(Config),

    ExpUsers = [U1, U2, U3, U4, U5, U6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS]},
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
            path = [<<"/handle_services/">>, HService, <<"/effective_users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_eff_users,
            args = [auth, HService],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check also handle_service_logic:has_eff_user function
    lists:foreach(
        fun(UserId) ->
            ?assert(oz_test_utils:call_oz(
                Config, handle_service_logic, has_eff_user, [HService, UserId])
            )
        end, ExpUsers
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, handle_service_logic, has_eff_user, [
            HService, <<"asdiucyaie827346w">>
        ]
    )).


get_eff_user_test(Config) ->
    {
        HService, _Groups, EffUsers, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_hservice_eff_users_env(Config),

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
                        <<"/handle_services/">>, HService,
                        <<"/effective_users/">>, UserId
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
                    module = handle_service_logic,
                    function = get_eff_user,
                    args = [auth, HService, UserId],
                    expected_result = ?OK_MAP_CONTAINS(UserDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_user, id = UserId,
                        aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_HANDLE_SERVICE(HService),
                    expected_result = ?OK_MAP_CONTAINS(UserDetails#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
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
    %%               HandleService
    %%              /     ||      \
    %%             /      ||       \
    %%  [~hservice_view]  ||    [hservice_view]
    %%           /        ||         \
    %%        User1      /  \       User2
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

    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_VIEW privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose eff privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:handle_service_add_group(Config, HService, G1),
    {ok, G2} = oz_test_utils:handle_service_add_group(Config, HService, G2),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),
    {ok, U3} = oz_test_utils:group_add_user(Config, G3, U3),
    {ok, U3} = oz_test_utils:group_add_user(Config, G2, U3),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:handle_service_privileges(),
    InitialPrivs = [?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_REGISTER_HANDLE],
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

        oz_test_utils:handle_service_set_group_privileges(
            Config, HService, G1, PrivsToGrant1, PrivsToRevoke
        ),
        oz_test_utils:handle_service_set_group_privileges(
            Config, HService, G2, PrivsToGrant2, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_VIEW_PRIVILEGES]},
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
                <<"/handle_services/">>, HService,
                <<"/effective_users/">>, U3, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_eff_user_privileges,
            args = [auth, HService, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?HANDLE_SERVICE_VIEW, false, U3
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
