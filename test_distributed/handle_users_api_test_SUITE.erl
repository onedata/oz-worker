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
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    AllPrivs = oz_test_utils:all_handle_privileges(Config),

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
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
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
            args = [client, HandleId, U3, data],
            expected_result = ?OK_BINARY(U3)
        },
        % TODO gs
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
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
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
            args = [client, HandleId, userId],
            expected_result = ?OK
        }
        % TODO gs
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
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:handle_add_user(Config, HandleId, U3),

    ExpUsers = [U1, U2, U3],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            args = [client, HandleId],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_user_test(Config) ->
    % create handle with 2 users:
    %   U2 gets the HANDLE_VIEW privilege
    %   U1 gets all remaining privileges
    {HandleId, U1, U2} = api_test_scenarios:create_basic_handle_env(
        Config, ?HANDLE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    ExpAlias = ExpName = ?USER_NAME1,
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{
        name = ExpName,
        alias = ExpAlias
    }),
    {ok, U3} = oz_test_utils:handle_add_user(Config, HandleId, U3),

    ExpUserDetails = #{
        <<"alias">> => ExpAlias,
        <<"name">> => ExpName
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/handles/">>, HandleId, <<"/users/">>, U3],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpUserDetails#{<<"userId">> => U3}
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = get_user,
            args = [client, HandleId, U3],
            expected_result = ?OK_MAP(ExpUserDetails)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_user, id = U3, aspect = instance, scope = shared
            },
            auth_hint = ?THROUGH_HANDLE(HandleId),
            expected_result = ?OK_MAP(ExpUserDetails#{
                <<"login">> => ExpAlias, % TODO VFS-4506 deprecated, included for backward compatibility
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = UserId} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(UserId, U3)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_user_privileges_test(Config) ->
    % create handle with 2 users:
    %   U2 gets the HANDLE_VIEW privilege
    %   U1 gets all remaining privileges
    {HandleId, U1, U2} = api_test_scenarios:create_basic_handle_env(
        Config, ?HANDLE_VIEW
    ),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:handle_add_user(Config, HandleId, U3),

    AllPrivs = oz_test_utils:all_handle_privileges(Config),
    InitialPrivs = [?HANDLE_VIEW],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:handle_set_user_privileges(
            Config, HandleId, U3, Operation, Privs
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
                <<"/handles/">>, HandleId, <<"/users/">>, U3, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = get_user_privileges,
            args = [client, HandleId, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?HANDLE_VIEW
    ])).


update_user_privileges_test(Config) ->
    % create handle with 2 users:
    %   U2 gets the HANDLE_UPDATE privilege
    %   U1 gets all remaining privileges
    {HandleId, U1, U2} = api_test_scenarios:create_basic_handle_env(
        Config, ?HANDLE_UPDATE
    ),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:handle_add_user(Config, HandleId, U3),

    AllPrivs = oz_test_utils:all_handle_privileges(Config),
    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:handle_set_user_privileges(
            Config, HandleId, U3, Operation, Privs
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
            args = [client, HandleId, U3, data],
            expected_result = ?OK
        }
        % TODO gs
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
            args = [client, HandleId],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
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

    lists:foreach(
        fun({UserId, UserDetails}) ->

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
                        <<"/handles/">>, HandleId,
                        <<"/effective_users/">>, UserId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = UserDetails#{<<"userId">> => UserId}
                },
                logic_spec = #logic_spec{
                    module = handle_logic,
                    function = get_eff_user,
                    args = [client, HandleId, UserId],
                    expected_result = ?OK_MAP(UserDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_user, id = UserId,
                        aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_HANDLE(HandleId),
                    expected_result = ?OK_MAP(UserDetails#{
                            <<"login">> => maps:get(<<"alias">>, UserDetails),
                            <<"gri">> => fun(EncodedGri) ->
                                #gri{id = Id} = oz_test_utils:decode_gri(
                                    Config, EncodedGri
                                ),
                                ?assertEqual(Id, UserId)
                            end
                        })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffUsers
    ).


get_eff_user_privileges_test(Config) ->
    %% Create environment with following relations:
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
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose eff privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:handle_add_group(Config, HandleId, G1),
    {ok, G2} = oz_test_utils:handle_add_group(Config, HandleId, G2),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),
    {ok, U3} = oz_test_utils:group_add_user(Config, G3, U3),
    {ok, U3} = oz_test_utils:group_add_user(Config, G2, U3),

    AllPrivs = oz_test_utils:all_handle_privileges(Config),
    InitialPrivs = [?HANDLE_VIEW],
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
                oz_test_utils:handle_set_group_privileges(
                    Config, HandleId, GroupId, Operation, Privileges
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
                <<"/handles/">>, HandleId,
                <<"/effective_users/">>, U3, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = handle_logic,
            function = get_eff_user_privileges,
            args = [client, HandleId, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?HANDLE_VIEW
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


init_per_testcase(_, Config) ->
    oz_test_utils:mock_handle_proxy(Config),
    Config.

end_per_testcase(_, Config) ->
    oz_test_utils:delete_all_entities(Config),
    oz_test_utils:unmock_handle_proxy(Config).
