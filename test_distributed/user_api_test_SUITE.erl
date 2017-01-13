%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user API (REST + logic).
%%% @end
%%%-------------------------------------------------------------------
-module(user_api_test_SUITE).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("errors.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-include("api_test_utils.hrl").


%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    create_test/1,
    create_client_token_test/1,
    authorize_test/1,

    get_test/1,
    list_test/1,
    get_oz_privileges_test/1,
    get_eff_oz_privileges_test/1,
    get_default_space_test/1,
    get_space_alias_test/1,
    get_default_provider_test/1,

    update_test/1,
    update_oz_privileges_test/1,
    set_default_space_test/1,
    set_space_alias_test/1,
    set_default_provider_test/1,

    delete_test/1,
    delete_oz_privileges_test/1,
    delete_client_token_test/1,
    unset_default_space_test/1,
    delete_space_alias_test/1,
    unset_default_provider_test/1,

    join_group_test/1,
    join_space_test/1,
    get_groups_test/1,
    get_spaces_test/1,
    get_eff_providers_test/1,
    get_handle_services_test/1,
    get_handles_test/1,
    leave_group_test/1,
    leave_space_test/1,
    leave_handle_service_test/1,
    leave_handle_test/1
]).

all() ->
    ?ALL([
        create_test,
        create_client_token_test,
        authorize_test,

        get_test,
        list_test,
        get_oz_privileges_test,
        get_eff_oz_privileges_test,
        get_default_space_test,
        get_space_alias_test,
        get_default_provider_test,

        update_test,
        update_oz_privileges_test,
        set_default_space_test,
        set_space_alias_test,
        set_default_provider_test,

        delete_test,
        delete_oz_privileges_test,
        delete_client_token_test,
        unset_default_space_test,
        delete_space_alias_test,
        unset_default_provider_test,

        join_group_test,
        join_space_test,
        get_groups_test,
        get_spaces_test,
        get_eff_providers_test,
        get_handle_services_test,
        get_handles_test,
        leave_group_test,
        leave_space_test,
        leave_handle_service_test,
        leave_handle_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    UserRecord = #od_user{name = <<"Name">>, login = <<"login">>},
    {ok, UserId} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_user_logic, create, [UserRecord]
    )),
    {ok, User} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_user_logic, get, [?USER(UserId), UserId]
    )),
    #od_user{name = Name, login = Login} = User,
    ?assertEqual(Name, <<"Name">>),
    ?assertEqual(Login, <<"login">>),
    % Try to create a user with given Id
    PredefinedUserId = <<"ausdhf87adsga87ht2q7hrw">>,
    {ok, PredefinedUserId} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_user_logic, create, [UserRecord, PredefinedUserId]
    )),
    {ok, User2} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_user_logic, get, [?USER(PredefinedUserId), PredefinedUserId]
    )),
    #od_user{name = Name, login = Login} = User2,
    ?assertEqual(Name, <<"Name">>),
    ?assertEqual(Login, <<"login">>),
    % Second try should fail (such id exists)
    ?assertMatch(?ERROR_BAD_VALUE_ID_OCCUPIED(<<"userId">>), oz_test_utils:call_oz(
        Config, n_user_logic, create, [UserRecord, PredefinedUserId]
    )).


create_client_token_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        data_spec = #data_spec{},
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/client_tokens">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"token">> => {check_type, binary}}
        },
        logic_spec = #logic_spec{
            operation = create,
            module = n_user_logic,
            function = create_client_token,
            args = [client, User],
            expected_result = ?OK_BINARY
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    % The test should have created some client tokens, check if
    % they are valid
    {ok, #od_user{
        client_tokens = ClientTokens
    }} = oz_test_utils:get_user(Config, User),
    lists:foreach(
        fun(ClientToken) ->
            {ok, Macaroon} = oz_test_utils:call_oz(
                Config, token_utils, deserialize, [ClientToken]
            ),
            ?assertEqual({ok, User}, oz_test_utils:call_oz(
                Config,
                auth_logic,
                validate_token,
                [<<>>, Macaroon, [], undefined, undefined]
            ))
        end, ClientTokens).


authorize_test(Config) ->
    % Create a provider and a user.
    {ok, {Provider, _, _}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    % Generate an auth token, parse the token for 3rd party caveats and check
    % if authorize endpoint works as expected.
    AuthToken = oz_test_utils:call_oz(
        Config, auth_logic, gen_token, [User, Provider]
    ),
    {ok, Macaroon} = oz_test_utils:call_oz(
        Config, token_utils, deserialize, [AuthToken]
    ),
    Caveats = macaroon:third_party_caveats(Macaroon),
    lists:foreach(
        fun({_, CaveatId}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [{user, User}]
                },
                data_spec = #data_spec{
                    required = [<<"identifier">>],
                    correct_values = #{
                        <<"identifier">> => CaveatId
                    },
                    bad_values = [
                        {<<"identifier">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"identifier">>)},
                        {<<"identifier">>, 123, ?ERROR_BAD_VALUE_BINARY(<<"identifier">>)},
                        {<<"identifier">>, <<"Sdfsdf">>, ?ERROR_BAD_VALUE_IDENTIFIER(<<"identifier">>)}
                    ]
                },
                rest_spec = #rest_spec{
                    method = post,
                    path = <<"/user/authorize">>,
                    expected_code = ?HTTP_200_OK,
                    expected_body = {check_type, binary}
                },
                logic_spec = #logic_spec{
                    operation = create,
                    module = n_user_logic,
                    function = authorize,
                    args = [client, User, data],
                    expected_result = ?OK_BINARY
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
        end, Caveats),
    ok.



get_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{
        name = ExpName = <<"Name">>,
        login = ExpLogin = <<"Login">>,
        alias = ExpAlias = <<"Alias">>,
        email_list = ExpEmailList = [<<"em1@google.com">>, <<"em2@google.com">>]
    }),
    % Create two users, grant one of them the privilege to list users.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_USERS_LIST
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    ExpectedBody = #{
        <<"name">> => ExpName,
        <<"login">> => ExpLogin,
        <<"alias">> => ExpAlias,
        <<"emailList">> => ExpEmailList
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User},
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpectedBody#{<<"userId">> => User}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = get_data,
            args = [client, User],
            expected_result = ?OK_MAP(ExpectedBody)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check REST endpoint dedicated for authorized user.
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"userId">> => User,
                <<"name">> => ExpName,
                <<"login">> => ExpLogin,
                <<"alias">> => ExpAlias,
                <<"emailList">> => ExpEmailList
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


list_test(Config) ->
    % Make sure that users created in other tests are deleted.
    ok = oz_test_utils:delete_all_entities(Config),
    % Create two users, grant one of them the privilege to list users.
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_USERS_LIST
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    % Create a provider, which should not have privileges to list users.
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),

    % For now, there are only two users
    ExpUsers = [Admin, NonAdmin],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}, {provider, P1, KeyFile, CertFile}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/users">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpUsers)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Create some users
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    % Now, more users are expected
    ExpUsers2 = [Admin, NonAdmin, U1, U2, U3],
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}, {provider, P1, KeyFile, CertFile}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/users">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers2}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpUsers2)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_oz_privileges_test(Config) ->
    % Create two users, grant one of them the privilege to view OZ privileges.
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_VIEW_PRIVILEGES
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    % A new user should not be able to view his oz_privileges
    NotAuthorizedApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        rest_spec = NotAuthorizedRestSpec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User, <<"/privileges">>],
            expected_code = ?HTTP_403_FORBIDDEN
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = get_oz_privileges,
            args = [client, User],
            expected_result = ?ERROR_REASON(?ERROR_FORBIDDEN)
        }
    },
    ?assert(api_test_utils:run_tests(Config, NotAuthorizedApiTestSpec)),
    % Also, check path dedicated for user that presents auth
    NotAuthorizedApiTestSpecForUser = NotAuthorizedApiTestSpec#api_test_spec{
        rest_spec = NotAuthorizedRestSpec#rest_spec{
            path = <<"/user/privileges">>
        }
    },
    ?assert(api_test_utils:run_tests(Config, NotAuthorizedApiTestSpecForUser)),

    % By default, a new user should not have any oz privileges
    InitialPrivs = [],
    % Do not include user in clients as his privileges will be changing, which
    % means that he will be sometimes authorized, sometimes not.
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => []}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = get_oz_privileges,
            args = [client, User],
            expected_result = ?OK_LIST([])
        }
    },
    AllPrivs = oz_test_utils:call_oz(Config, privileges, oz_privileges, []),
    SetPrivsFun = fun(Operation, Privs) ->
        oz_test_utils:set_user_oz_privileges(Config, User, Operation, Privs)
    end,
    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs, []
    ])),
    % Also, check path dedicated for user that presents auth
    oz_test_utils:set_user_oz_privileges(Config, User, set, [?OZ_VIEW_PRIVILEGES]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ApiTestSpecForUser = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/privileges">>
        }
    },
    % ?OZ_VIEW_PRIVILEGES will be granted in every test so the user has
    % rights to view his privileges.
    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpecForUser, SetPrivsFun, AllPrivs,
        [?OZ_VIEW_PRIVILEGES], [?OZ_VIEW_PRIVILEGES]
    ])).


get_eff_oz_privileges_test(Config) ->
    % Create two users, grant one of them the privilege to view OZ privileges.
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_VIEW_PRIVILEGES
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    % A new user should not be able to view his eff_oz_privileges
    NotAuthorizedApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        rest_spec = NotAuthorizedRestSpec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User, <<"/effective_privileges">>],
            expected_code = ?HTTP_403_FORBIDDEN
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = get_eff_oz_privileges,
            args = [client, User],
            expected_result = ?ERROR_REASON(?ERROR_FORBIDDEN)
        }
    },
    ?assert(api_test_utils:run_tests(Config, NotAuthorizedApiTestSpec)),
    % Also, check path dedicated for user that presents auth
    NotAuthorizedApiTestSpecForUser = NotAuthorizedApiTestSpec#api_test_spec{
        rest_spec = NotAuthorizedRestSpec#rest_spec{
            path = <<"/user/privileges">>
        }
    },
    ?assert(api_test_utils:run_tests(Config, NotAuthorizedApiTestSpecForUser)),

    % Create some groups hierarchy for the user
    {BottomGroup, MiddleGroup, TopGroup} = oz_test_utils:create_3_nested_groups(
        Config, User
    ),
    % By default, a new user should not have any effective oz privileges
    InitialPrivs = [],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User, <<"/effective_privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => []}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = get_eff_oz_privileges,
            args = [client, User],
            expected_result = ?OK_LIST([])
        }
    },
    AllPrivs = oz_test_utils:call_oz(Config, privileges, oz_privileges, []),
    SetPrivsFun = fun(Operation, Privs) ->
        % In case of SET and GRANT, randomly split privileges into four parts
        % and update the user and his groups with the privileges. His
        % eff_privileges should contain the sum of those. In case of revoke, the
        % privileges must be revoked for all 4 entities.
        case Operation of
            revoke ->
                oz_test_utils:set_user_oz_privileges(
                    Config, User, revoke, Privs
                ),
                oz_test_utils:set_group_oz_privileges(
                    Config, BottomGroup, revoke, Privs
                ),
                oz_test_utils:set_group_oz_privileges(
                    Config, MiddleGroup, revoke, Privs
                ),
                oz_test_utils:set_group_oz_privileges(
                    Config, TopGroup, revoke, Privs
                );
            _ -> % Covers (set|grant)
                Parts = lists:foldl(
                    fun(Privilege, AccMap) ->
                        Index = rand:uniform(4),
                        AccMap#{Index => [Privilege | maps:get(Index, AccMap)]}
                    end, #{1=>[], 2=>[], 3=>[], 4=>[]}, Privs),
                oz_test_utils:set_user_oz_privileges(
                    Config, User, Operation, maps:get(1, Parts)
                ),
                oz_test_utils:set_group_oz_privileges(
                    Config, BottomGroup, Operation, maps:get(2, Parts)
                ),
                oz_test_utils:set_group_oz_privileges(
                    Config, MiddleGroup, Operation, maps:get(3, Parts)
                ),
                oz_test_utils:set_group_oz_privileges(
                    Config, TopGroup, Operation, maps:get(4, Parts)
                )
        end,
        oz_test_utils:ensure_eff_graph_up_to_date(Config)
    end,
    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs, []
    ])),
    % Also, check path dedicated for user that presents auth
    oz_test_utils:set_user_oz_privileges(Config, User, set, [?OZ_VIEW_PRIVILEGES]),
    oz_test_utils:set_group_oz_privileges(Config, BottomGroup, set, []),
    oz_test_utils:set_group_oz_privileges(Config, MiddleGroup, set, []),
    oz_test_utils:set_group_oz_privileges(Config, TopGroup, set, []),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ApiTestSpecForUser = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/effective_privileges">>
        }
    },
    % ?OZ_VIEW_PRIVILEGES will be granted in every test so the user has
    % rights to view his privileges.
    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpecForUser, SetPrivsFun, AllPrivs,
        [?OZ_VIEW_PRIVILEGES], [?OZ_VIEW_PRIVILEGES]
    ])).


get_default_space_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    % Newly created user should not have a default space
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = <<"/user/default_space">>,
            expected_code = ?HTTP_404_NOT_FOUND
        },
        logic_spec = LogicSpec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = get_default_space,
            args = [client, User],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    % Set a default space for user
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User), <<"sp">>),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    oz_test_utils:set_user_default_space(Config, User, Space),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaceId">> => Space}
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?OK_BINARY(Space)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),
    % Unset the default space and check if it's not present again
    oz_test_utils:unset_user_default_space(Config, User),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).



get_space_alias_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User), <<"sp">>),
    % Newly created space should not have an alias
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = [<<"/user/spaces/">>, Space, <<"/alias">>],
            expected_code = ?HTTP_404_NOT_FOUND
        },
        logic_spec = LogicSpec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = get_space_alias,
            args = [client, User, Space],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    % Set an alias for given space
    Alias = <<"Alias">>,
    ok = oz_test_utils:set_user_space_alias(Config, User, Space, Alias),
    ApiTestSpec = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"alias">> => Alias}
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?OK_BINARY(Alias)
        }
    },
    % Unset the space alias and check if it's not present again
    ok = oz_test_utils:unset_user_space_alias(Config, User, Space),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_default_provider_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    % Newly created user should not have a default provider
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = <<"/user/default_provider">>,
            expected_code = ?HTTP_404_NOT_FOUND
        },
        logic_spec = LogicSpec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = get_default_provider,
            args = [client, User],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    % Set a default provider for user
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User), <<"sp">>),
    {ok, {Provider, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P1">>),
    {ok, Macaroon} = oz_test_utils:space_invite_provider_token(Config, ?ROOT, Space),
    {ok, Space} = oz_test_utils:support_space(
        Config, ?PROVIDER(Provider), Provider,
        Macaroon, oz_test_utils:minimum_support_size(Config)
    ),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    oz_test_utils:set_user_default_provider(Config, User, Provider),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"providerId">> => Provider}
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?OK_BINARY(Provider)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),
    % Unset the default provider and check if it's not present again
    oz_test_utils:unset_user_default_provider(Config, User),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).



update_test(Config) ->

    ok.


update_oz_privileges_test(Config) ->
    ok.


set_default_space_test(Config) ->
    ok.


set_space_alias_test(Config) ->
    ok.


set_default_provider_test(Config) ->
    ok.



delete_test(Config) ->
    ok.


delete_oz_privileges_test(Config) ->
    ok.


delete_client_token_test(Config) ->
    ok.


unset_default_space_test(Config) ->
    ok.


delete_space_alias_test(Config) ->
    ok.


unset_default_provider_test(Config) ->
    ok.



join_group_test(Config) ->
    ok.


join_space_test(Config) ->
    ok.


get_groups_test(Config) ->
    ok.


get_spaces_test(Config) ->
    ok.


get_eff_providers_test(Config) ->
    ok.


get_handle_services_test(Config) ->
    ok.


get_handles_test(Config) ->
    ok.


leave_group_test(Config) ->
    ok.


leave_space_test(Config) ->
    ok.


leave_handle_service_test(Config) ->
    ok.


leave_handle_test(Config) ->
    ok.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(etls),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    timer:sleep(24 * 3600 * 1000),
    hackney:stop(),
    application:stop(etls).
