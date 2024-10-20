%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning handle users API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(handle_users_api_test_SUITE).
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
-include_lib("ctool/include/errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
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
    % create handle with 2 users:
    %   U2 gets the HANDLE_UPDATE privilege
    %   U1 gets all remaining privileges
    {HandleId, U1, U2} = api_test_scenarios:create_basic_handle_env(
        Config, ?HANDLE_UPDATE
    ),
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:handle_privileges(),

    VerifyEndFun =
        fun
            (true = _ShouldSucceed, _, Data) ->
                ExpPrivs = lists:sort(maps:get(<<"privileges">>, Data)),
                {ok, Privs} = oz_test_utils:handle_get_user_privileges(
                    Config, HandleId, U3
                ),
                ?assertEqual(ExpPrivs, lists:sort(Privs)),
                oz_test_utils:handle_remove_user(Config, HandleId, U3);
            (false = ShouldSucceed, _, _) ->
                {ok, Users} = oz_test_utils:handle_get_users(Config, HandleId),
                ?assertEqual(lists:member(U3, Users), ShouldSucceed)
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS]},
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
            path = [<<"/handles/">>, HandleId, <<"/users/">>, U3],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{?HDR_LOCATION := Location} = _Headers) ->
                ExpLocation = ?URL(Config,
                    [<<"/handles/">>, HandleId, <<"/users/">>, U3]
                ),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = add_user,
            args = [auth, HandleId, U3, data],
            expected_result = ?OK_BINARY(U3)
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [?HANDLE_UPDATE, ?HANDLE_VIEW],
                    [?HANDLE_DELETE]
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
    % create handle with 2 users:
    %   U2 gets the HANDLE_UPDATE privilege
    %   U1 gets all remaining privileges
    {HandleId, U1, U2} = api_test_scenarios:create_basic_handle_env(
        Config, ?HANDLE_UPDATE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, U3} = oz_test_utils:create_user(Config),
        {ok, U3} = oz_test_utils:handle_add_user(Config, HandleId, U3),
        #{userId => U3}
    end,
    DeleteEntityFun = fun(#{userId := User} = _Env) ->
        oz_test_utils:handle_remove_user(Config, HandleId, User)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := User} = _Env, _) ->
        {ok, Users} = oz_test_utils:handle_get_users(Config, HandleId),
        ?assertEqual(lists:member(User, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS]},
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
            path = [<<"/handles/">>, HandleId, <<"/users/">>, userId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = remove_user,
            args = [auth, HandleId, userId],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_users_test(Config) ->
    % create handle with 2 users:
    %   U2 gets the HANDLE_VIEW privilege
    %   U1 gets all remaining privileges
    {HandleId, U1, U2} = api_test_scenarios:create_basic_handle_env(
        Config, ?HANDLE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:handle_add_user(Config, HandleId, U3),

    ExpUsers = [U1, U2, U3],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_LIST_RELATIONSHIPS]},
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
            path = [<<"/handles/">>, HandleId, <<"/users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = get_users,
            args = [auth, HandleId],
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

    oz_test_utils:user_set_oz_privileges(Config, Creator, [
        ?OZ_HANDLE_SERVICES_CREATE
    ], []),
    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?USER(Creator), ?DOI_SERVICE
    ),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(Creator), ?SPACE_NAME1),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ?SHARE_ID_1, ?SHARE_NAME1, S1
    ),

    HandleId = ozt_users:create_handle_for(Creator, HService, ShareId),
    {ok, _} = oz_test_utils:handle_add_user(Config, HandleId, MemberWithViewPrivs),
    {ok, _} = oz_test_utils:handle_add_user(Config, HandleId, MemberWithoutViewPrivs),
    {ok, _} = oz_test_utils:handle_add_user(Config, HandleId, Member),

    oz_test_utils:handle_set_user_privileges(Config, HandleId, MemberWithViewPrivs, [?HANDLE_VIEW], []),
    oz_test_utils:handle_set_user_privileges(Config, HandleId, MemberWithoutViewPrivs, [], [?HANDLE_VIEW]),

    % Shared data about creator should be available even if he is not longer in the handle
    oz_test_utils:handle_remove_user(Config, HandleId, Creator),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    lists:foreach(fun({SubjectUser, UserData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_VIEW]},
                    {user, SubjectUser},
                    {user, MemberWithViewPrivs}
                ] ++ case SubjectUser of
                    % Every member of the handle should be able to see the creator details
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
                path = [<<"/handles/">>, HandleId, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:shared_user(rest, SubjectUser, UserData)
            },
            logic_spec = #logic_spec{
                module = handle_logic,
                function = get_user,
                args = [auth, HandleId, SubjectUser],
                expected_result = api_test_expect:shared_user(logic, SubjectUser, UserData)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = SubjectUser, aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_HANDLE(HandleId),
                expected_result_op = api_test_expect:shared_user(gs, SubjectUser, UserData)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, [{Creator, CreatorData}, {Member, MemberData}]).


get_user_privileges_test(Config) ->
    % create handle with 2 users:
    %   U2 gets the HANDLE_VIEW privilege
    %   U1 gets all remaining privileges
    {HandleId, U1, U2} = api_test_scenarios:create_basic_handle_env(
        Config, ?HANDLE_VIEW
    ),
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:handle_add_user(Config, HandleId, U3),

    AllPrivs = privileges:handle_privileges(),
    InitialPrivs = [?HANDLE_VIEW],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:handle_set_user_privileges(
            Config, HandleId, U3, PrivsToGrant, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_VIEW_PRIVILEGES]},
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
                <<"/handles/">>, HandleId, <<"/users/">>, U3, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = get_user_privileges,
            args = [auth, HandleId, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?HANDLE_VIEW, false, U3
    ])).


update_user_privileges_test(Config) ->
    % create handle with 2 users:
    %   U2 gets the HANDLE_UPDATE privilege
    %   U1 gets all remaining privileges
    {HandleId, U1, U2} = api_test_scenarios:create_basic_handle_env(
        Config, ?HANDLE_UPDATE
    ),
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:handle_add_user(Config, HandleId, U3),

    AllPrivs = privileges:handle_privileges(),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:handle_set_user_privileges(
            Config, HandleId, U3, PrivsToGrant, PrivsToRevoke
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:handle_get_user_privileges(
            Config, HandleId, U3
        ),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_SET_PRIVILEGES]},
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
                <<"/handles/">>, HandleId, <<"/users/">>, U3, <<"/privileges">>
            ],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = update_user_privileges,
            args = [auth, HandleId, U3, data],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?HANDLE_UPDATE
    ])).


list_eff_users_test(Config) ->
    {
        HandleId, _Groups, [{U3, _}, {U4, _}, {U5, _}, {U6, _}],
        {U1, U2, NonAdmin}
    } = api_test_scenarios:create_handle_eff_users_env(Config),

    ExpUsers = [U1, U2, U3, U4, U5, U6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_LIST_RELATIONSHIPS]},
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
            path = [<<"/handles/">>, HandleId, <<"/effective_users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = get_eff_users,
            args = [auth, HandleId],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check also handle_logic:has_eff_user function
    lists:foreach(
        fun(UserId) ->
            ?assert(oz_test_utils:call_oz(
                Config, handle_logic, has_eff_user, [HandleId, UserId])
            )
        end, ExpUsers
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, handle_logic, has_eff_user, [HandleId, <<"asdiucyaie827346w">>]
    )).


get_eff_user_test(Config) ->
    {
        HandleId, _Groups, EffUsers, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_handle_eff_users_env(Config),

    lists:foreach(fun({UserId, UserData}) ->

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
                    <<"/handles/">>, HandleId,
                    <<"/effective_users/">>, UserId
                ],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:shared_user(rest, UserId, UserData)
            },
            logic_spec = #logic_spec{
                module = handle_logic,
                function = get_eff_user,
                args = [auth, HandleId, UserId],
                expected_result = api_test_expect:shared_user(logic, UserId, UserData)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = UserId,
                    aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_HANDLE(HandleId),
                expected_result_op = api_test_expect:shared_user(gs, UserId, UserData)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffUsers).


get_eff_user_privileges_test(Config) ->
    %% Create environment with the following relations:
    %%
    %%                  Handle
    %%                 /  ||   \
    %%                /   ||    \
    %%               /    ||     \
    %%              /     ||      \
    %%    [~handle_view]  ||   [handle_view]
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

    % create handle with 2 users:
    %   U2 gets the HANDLE_VIEW privilege
    %   U1 gets all remaining privileges
    {HandleId, U1, U2} = api_test_scenarios:create_basic_handle_env(
        Config, ?HANDLE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, SubjectUser} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:handle_add_group(Config, HandleId, G1),
    {ok, G2} = oz_test_utils:handle_add_group(Config, HandleId, G2),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),
    {ok, SubjectUser} = oz_test_utils:group_add_user(Config, G3, SubjectUser),
    {ok, SubjectUser} = oz_test_utils:group_add_user(Config, G2, SubjectUser),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:handle_privileges(),
    InitialPrivs = [?HANDLE_VIEW],
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

        oz_test_utils:handle_set_group_privileges(
            Config, HandleId, G1, PrivsToGrant1, PrivsToRevoke
        ),
        oz_test_utils:handle_set_group_privileges(
            Config, HandleId, G2, PrivsToGrant2, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLES_VIEW_PRIVILEGES]},
                {user, U2},
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
                <<"/handles/">>, HandleId,
                <<"/effective_users/">>, SubjectUser, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = get_eff_user_privileges,
            args = [auth, HandleId, SubjectUser],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, SubjectUser}, ?HANDLE_VIEW, false, SubjectUser
    ])).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    ozt:init_per_suite(Config).

end_per_suite(_Config) ->
    application:stop(hackney),
    ssl:stop().

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    ozt_mocks:mock_handle_proxy(),
    Config.

end_per_testcase(_, _Config) ->
    ozt:delete_all_entities(),
    ozt_mocks:unmock_handle_proxy(),
    ozt_mocks:unfreeze_time().
