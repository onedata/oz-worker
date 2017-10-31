%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(user_api_test_SUITE).
-author("Lukasz Opiola").

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


%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    create_test/1,
    authorize_test/1,
    list_test/1,
    get_test/1,
    get_self_test/1,
    update_test/1,
    delete_test/1,
    delete_self_test/1,

    create_client_token_test/1,
    list_client_tokens_test/1,
    delete_client_token_test/1,
    set_default_space_test/1,
    get_default_space_test/1,
    unset_default_space_test/1,
    set_default_provider_test/1,
    get_default_provider_test/1,
    unset_default_provider_test/1,

    list_groups_test/1,
    create_group_test/1,
    join_group_test/1,
    get_group_test/1,
    leave_group_test/1,
    list_eff_groups_test/1,
    get_eff_group_test/1,

    create_space_test/1,
    list_spaces_test/1,
    join_space_test/1,
    get_space_test/1,
    leave_space_test/1,
    set_space_alias_test/1,
    get_space_alias_test/1,
    delete_space_alias_test/1,
    list_eff_spaces_test/1,
    get_eff_space_test/1,

    list_eff_providers_test/1,
    get_eff_provider_test/1,

    create_handle_service_test/1,
    list_handle_services_test/1,
    get_handle_service_test/1,
    leave_handle_service_test/1,
    list_eff_handle_services_test/1,
    get_eff_handle_service_test/1,

    create_handle_test/1,
    list_handles_test/1,
    get_handle_test/1,
    leave_handle_test/1,
    list_eff_handles_test/1,
    get_eff_handle_test/1
]).

all() ->
    ?ALL([
        create_test,
        authorize_test,
        list_test,
        get_test,
        get_self_test,
        update_test,
        delete_test,
        delete_self_test,

        create_client_token_test,
        list_client_tokens_test,
        delete_client_token_test,
        set_default_space_test,
        get_default_space_test,
        unset_default_space_test,
        set_default_provider_test,
        get_default_provider_test,
        unset_default_provider_test,

        list_groups_test,
        create_group_test,
        join_group_test,
        get_group_test,
        leave_group_test,
        list_eff_groups_test,
        get_eff_group_test,

        create_space_test,
        list_spaces_test,
        join_space_test,
        get_space_test,
        leave_space_test,
        set_space_alias_test,
        get_space_alias_test,
        delete_space_alias_test,
        list_eff_spaces_test,
        get_eff_space_test,

        list_eff_providers_test,
        get_eff_provider_test,

        create_handle_service_test,
        list_handle_services_test,
        get_handle_service_test,
        leave_handle_service_test,
        list_eff_handle_services_test,
        get_eff_handle_service_test,

        create_handle_test,
        list_handles_test,
        get_handle_test,
        leave_handle_test,
        list_eff_handles_test,
        get_eff_handle_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    ExpName = <<"name">>,
    ExpLogin = <<"login">>,
    UserRecord = #od_user{name = ExpName, login = ExpLogin},

    % Try to create user without predefined id
    {ok, UserId} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, user_logic, create, [UserRecord]
    )),
    {ok, User} = oz_test_utils:get_user(Config, UserId),
    ?assertEqual(User#od_user.name, ExpName),
    ?assertEqual(User#od_user.login, ExpLogin),

    % Try to create a user with given Id
    PredefinedUserId = <<"ausdhf87adsga87ht2q7hrw">>,
    {ok, PredefinedUserId} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, user_logic, create, [UserRecord, PredefinedUserId]
    )),
    {ok, User2} = oz_test_utils:get_user(Config, PredefinedUserId),
    ?assertEqual(User2#od_user.name, ExpName),
    ?assertEqual(User2#od_user.login, ExpLogin),

    % Second try should fail (such id exists)
    ?assertMatch(?ERROR_BAD_VALUE_ID_OCCUPIED(<<"userId">>),
        oz_test_utils:call_oz(
            Config, user_logic, create, [UserRecord, PredefinedUserId]
        )
    ).


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
                rest_spec = #rest_spec{
                    method = post,
                    path = <<"/user/authorize">>,
                    expected_code = ?HTTP_200_OK,
                    expected_body = {check_type, binary}
                },
                logic_spec = #logic_spec{
                    module = user_logic,
                    function = authorize,
                    args = [data],
                    expected_result = ?OK_BINARY
                },
                % TODO gs
                data_spec = #data_spec{
                    required = [<<"identifier">>],
                    correct_values = #{
                        <<"identifier">> => [CaveatId]
                    },
                    bad_values = [
                        {<<"identifier">>, <<"">>,
                            ?ERROR_BAD_VALUE_EMPTY(<<"identifier">>)},
                        {<<"identifier">>, 123,
                            ?ERROR_BAD_VALUE_BINARY(<<"identifier">>)},
                        {<<"identifier">>, <<"Sdfsdf">>,
                            ?ERROR_BAD_VALUE_IDENTIFIER(<<"identifier">>)}
                    ]
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
        end, Caveats
    ).


list_test(Config) ->
    % Make sure that users created in other tests are deleted.
    ok = oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_USERS_LIST
    ]),
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),

    ExpUsers = [Admin, NonAdmin, U1, U2, U3],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P1, KeyFile, CertFile}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/users">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = user_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_test(Config) ->
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, User} = oz_test_utils:create_user(Config, #od_user{
        name = ExpName = <<"UserName">>,
        login = ExpLogin = <<"UserLogin">>,
        alias = ExpAlias = <<"UserAlias">>,
        email_list = ExpEmailList = [<<"a@a.a">>, <<"b@b.b">>]
    }),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_USERS_LIST
    ]),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),
    {ok, S1} = oz_test_utils:support_space(
        Config, P1, S1, oz_test_utils:minimum_support_size(Config)
    ),

    ProtectedData = #{
        <<"name">> => ExpName,
        <<"login">> => ExpLogin,
        <<"emailList">> => ExpEmailList,
        % TODO VFS-2918
        <<"linkedAccounts">> => []
    },
    ProtectedDataWithAlias = ProtectedData#{
        <<"alias">> => ExpAlias
    },

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, P1, KeyFile, CertFile},
                {user, NonAdmin},
                {user, Admin}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = get,
            args = [client, User],
            expected_result = ?OK_TERM(
                fun(#od_user{
                    name = Name, login = Login, alias = Alias,
                    email_list = EmailList,
                    basic_auth_enabled = false,
                    linked_accounts = [],

                    default_space = undefined,
                    default_provider = undefined,
                    chosen_provider = undefined,
                    client_tokens = [],
                    space_aliases = #{},

                    oz_privileges = [], eff_oz_privileges = [],

                    groups = [], eff_groups = #{},
                    spaces = [SpaceId],
                    eff_spaces = EffSpaces,
                    eff_providers = EffProviders,
                    handle_services = [], eff_handle_services = #{},
                    handles = [], eff_handles = #{}
                }) ->
                    ?assertEqual(ExpName, Name),
                    ?assertEqual(ExpLogin, Login),
                    ?assertEqual(ExpAlias, Alias),
                    ?assertEqual(ExpEmailList, EmailList),
                    ?assertEqual(SpaceId, S1),
                    ?assertEqual(EffSpaces, #{S1 => [{od_user, User}]}),
                    ?assertEqual(EffProviders, #{P1 => [{od_space, S1}]})
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_user, id = User, aspect = instance},
            expected_result = ?OK_MAP(#{
                <<"defaultSpaceId">> => null,
                <<"effectiveGroups">> => [],
                <<"effectiveHandleServices">> => [],
                <<"effectiveHandles">> => [],
                <<"effectiveSpaces">> => [S1],
                <<"emailList">> => ExpEmailList,
                <<"linkedAccounts">> => [],
                <<"login">> => ExpLogin,
                <<"name">> => ExpName,
                <<"spaceAliases">> => #{},
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(User, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check protected data
    GetProtectedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User},
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, P1, KeyFile, CertFile},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User],
            expected_code = ?HTTP_200_OK,
            expected_body = ProtectedDataWithAlias#{<<"userId">> => User}
        },
        logic_spec = LogicSpec = #logic_spec{
            module = user_logic,
            function = get_protected_data,
            args = [client, User],
            expected_result = ?OK_MAP(ProtectedDataWithAlias)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_user, id = User,
                aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP(ProtectedData#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, User)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)),

    % Get and check shared data
    GetSharedDataApiTestSpec = GetProtectedDataApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User},
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = LogicSpec#logic_spec{
            function = get_shared_data,
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"login">> => ExpLogin,
                <<"alias">> => ExpAlias
            })
        },
        gs_spec = GsSpec#gs_spec{
            gri = #gri{
                type = od_user, id = User, aspect = instance, scope = shared
            },
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"login">> => ExpLogin,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, User)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetSharedDataApiTestSpec)).


get_self_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{
        name = ExpName = <<"Name">>,
        login = ExpLogin = <<"Login">>,
        alias = ExpAlias = <<"Alias">>,
        email_list = ExpEmailList = [
            <<"em1@google.com">>, <<"em2@google.com">>
        ]
    }),

    ProtectedData = #{
        <<"name">> => ExpName,
        <<"login">> => ExpLogin,
        <<"emailList">> => ExpEmailList,
        % TODO VFS-2918
        <<"linkedAccounts">> => []
    },

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user">>,
            expected_code = ?HTTP_200_OK,
            expected_body = ProtectedData#{
                <<"userId">> => User,
                <<"alias">> => ExpAlias
            }
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_user, id = ?SELF,
                aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP(ProtectedData#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, User)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_test(Config) ->
    InitialName = <<"U1">>,

    EnvSetUpFun = fun() ->
        {ok, User} = oz_test_utils:create_user(Config, #od_user{
            name = InitialName
        }),
        #{userId => User}
    end,
    VerifyEndFun =
        fun
            (ShouldSucceed, #{userId := UserId} = _Env, Data) ->
                {ok, User} = oz_test_utils:get_user(Config, UserId),
                {ExpName, ExpAlias} = case ShouldSucceed of
                    false ->
                        {InitialName, <<"">>};
                    true ->
                        {
                            maps:get(<<"name">>, Data, InitialName),
                            maps:get(<<"alias">>, Data, <<"">>)
                        }
                    end,
                ?assertEqual(ExpName, User#od_user.name),
                ?assertEqual(ExpAlias, User#od_user.alias)
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, userId}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = <<"/user">>,
            expected_code = ?HTTP_204_NO_CONTENT
        },
        gs_spec = GsSpec = #gs_spec{
            operation = update,
            gri = #gri{type = od_user, id = ?SELF, aspect = instance},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            at_least_one = [
                <<"name">>,
                <<"alias">>
            ],
            correct_values = #{
                <<"name">> => [InitialName],
                <<"alias">> => [fun() ->
                    UniqueInt = erlang:unique_integer([positive]),
                    <<"alias", (integer_to_binary(UniqueInt))/binary>>
                end]
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)},
                {<<"alias">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"alias">>)},
                {<<"alias">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"alias">>)},
                {<<"alias">>, <<"Resu1">>, ?ERROR_BAD_VALUE_ALIAS(<<"alias">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = update,
            args = [client, userId, data],
            expected_result = ?OK
        },
        gs_spec = GsSpec#gs_spec{
            gri = #gri{type = od_user, id = userId, aspect = instance}
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_USERS_DELETE
    ]),

    EnvSetUpFun = fun() ->
        {ok, UserId} = oz_test_utils:create_user(Config, #od_user{}),
        #{userId => UserId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := UserId} = _Env, _) ->
        {ok, Users} = oz_test_utils:list_users(Config),
        ?assertEqual(lists:member(UserId, Users), not ShouldSucceed)
    end,

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
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/users/">>, userId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = delete,
            args = [client, userId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{
                type = od_user, id = userId, aspect = instance
            },
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, {user, userId}]
    )).


delete_self_test(Config) ->
    EnvSetUpFun = fun() ->
        {ok, UserId} = oz_test_utils:create_user(Config, #od_user{}),
        #{userId => UserId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := UserId} = _Env, _) ->
        {ok, Users} = oz_test_utils:list_users(Config),
        ?assertEqual(lists:member(UserId, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, userId}]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = <<"/user">>,
            expected_code = ?HTTP_202_ACCEPTED
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_user, id = ?SELF, aspect = instance},
            expected_result = ?OK
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


create_client_token_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    VerifyFun = fun(ClientToken) ->
        {ok, Macaroon} = oz_test_utils:call_oz(
            Config, token_utils, deserialize, [ClientToken]
        ),
        ?assertEqual({ok, User}, oz_test_utils:call_oz(
            Config, auth_logic, validate_token,
            [<<>>, Macaroon, [], undefined, undefined]
        )),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/client_tokens">>,
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_client_token,
            args = [client, User],
            expected_result = ?OK_TERM(VerifyFun)
        },
        rest_spec = undefined
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


list_client_tokens_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Macaroon} = oz_test_utils:create_client_token(Config, User),

    ExpTokens = [Macaroon | lists:map(
        fun(_) ->
            {ok, Token} = oz_test_utils:create_client_token(Config, User),
            Token
        end, lists:seq(1, 5)
    )],

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User, Macaroon}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/client_tokens">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"tokens">> => ExpTokens}
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            operation = get,
            module = user_logic,
            function = list_client_tokens,
            args = [client, User],
            expected_result = ?OK_LIST(ExpTokens)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


delete_client_token_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, Token} = oz_test_utils:create_client_token(Config, User),
        #{token => Token}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{token := Token} = _Env, _) ->
        {ok, Tokens} = oz_test_utils:list_client_tokens(Config, User),
        ?assertEqual(lists:member(Token, Tokens), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/client_tokens/">>, token],
            expected_code = ?HTTP_202_ACCEPTED
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = delete_client_token,
            args = [client, User, token],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


set_default_space_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, <<"S1">>),

    EnvSetUpFun = fun() ->
        {ok, SpaceId} = oz_test_utils:create_space(
            Config, ?USER(User), <<"Space">>
        ),
        #{spaceId => SpaceId}
    end,
    VerifyEndFun =
        fun
            (true = _ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
                ?assertMatch(
                    {ok, SpaceId},
                    oz_test_utils:get_user_default_space(Config, User)
                );
            (false = _ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
                ?assertNotEqual(
                    {ok, SpaceId},
                    oz_test_utils:call_oz(
                        Config, user_logic, get_default_space, [?ROOT, User]
                    )
                )
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = <<"/user/default_space">>,
            expected_code = ?HTTP_204_NO_CONTENT
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"spaceId">>],
            correct_values = #{
                <<"spaceId">> => [spaceId]
            },
            bad_values = [
                {<<"spaceId">>, <<"">>,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"spaceId">>)},
                {<<"spaceId">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"spaceId">>)},
                {<<"spaceId">>, S1,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"spaceId">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = set_default_space,
            args = [client, User, data],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


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
            module = user_logic,
            function = get_default_space,
            args = [client, User],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Set a default space for user
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),
    oz_test_utils:set_user_default_space(Config, User, Space),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaceId">> => Space}
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?OK_BINARY(Space)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % Unset the default space and check if it's not present again
    oz_test_utils:unset_user_default_space(Config, User),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


unset_default_space_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),

    EnvSetUpFun = fun() ->
        ok = oz_test_utils:set_user_default_space(Config, User, S1),
        #{}
    end,
    VerifyEndFun =
        fun
            (true = _ShouldSucceed, _, _) ->
                ?assertMatch(
                    ?ERROR_NOT_FOUND,
                    oz_test_utils:call_oz(
                        Config, user_logic, get_default_space, [?ROOT, User]
                    )
                );
            (false = _ShouldSucceed, _, _) ->
                ?assertMatch(
                    {ok, S1},
                    oz_test_utils:get_user_default_space(Config, User)
                )
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = <<"/user/default_space">>,
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = unset_default_space,
            args = [client, User],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


set_default_provider_test(Config) ->
    {ok, {P1, _, _}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),

    EnvSetUpFun = fun() ->
        UniqueInt = erlang:unique_integer([positive]),
        {ok, {ProviderId, _, _}} = oz_test_utils:create_provider_and_certs(
            Config, <<"Provider", (integer_to_binary(UniqueInt))/binary>>
        ),
        {ok, S1} = oz_test_utils:support_space(
            Config, ProviderId, S1, oz_test_utils:minimum_support_size(Config)
        ),
        #{providerId => ProviderId}
    end,
    VerifyEndFun =
        fun
            (true = _ShouldSucceed, #{providerId := ProviderId} = _Env, _) ->
                ?assertMatch(
                    {ok, ProviderId},
                    oz_test_utils:get_user_default_provider(Config, User)
                );
            (false = _ShouldSucceed, #{providerId := ProviderId} = _Env, _) ->
                ?assertNotEqual(
                    {ok, ProviderId},
                    oz_test_utils:call_oz(
                        Config, user_logic, get_default_provider, [?ROOT, User]
                    )
                )
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = <<"/user/default_provider">>,
            expected_code = ?HTTP_204_NO_CONTENT
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"providerId">>],
            correct_values = #{
                <<"providerId">> => [providerId]
            },
            bad_values = [
                {<<"providerId">>, <<"">>,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"providerId">>)},
                {<<"providerId">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"providerId">>)},
                {<<"providerId">>, P1,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"providerId">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = set_default_provider,
            args = [client, User, data],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


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
            module = user_logic,
            function = get_default_provider,
            args = [client, User],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Set a default provider for user
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),
    {ok, {P1, _, _}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, Macaroon} = oz_test_utils:space_invite_provider_token(
        Config, ?ROOT, S1
    ),
    {ok, S1} = oz_test_utils:support_space(
        Config, ?PROVIDER(P1), P1,
        Macaroon, oz_test_utils:minimum_support_size(Config)
    ),
    oz_test_utils:set_user_default_provider(Config, User, P1),

    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"providerId">> => P1}
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?OK_BINARY(P1)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % Unset the default provider and check if it's not present again
    oz_test_utils:unset_user_default_provider(Config, User),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


unset_default_provider_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),
    {ok, {P1, _, _}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, S1} = oz_test_utils:support_space(
        Config, P1, S1, oz_test_utils:minimum_support_size(Config)
    ),

    EnvSetUpFun = fun() ->
        ok = oz_test_utils:set_user_default_provider(Config, User, P1),
        #{}
    end,
    VerifyEndFun =
        fun
            (true = _ShouldSucceed, _, _) ->
                ?assertMatch(
                    ?ERROR_NOT_FOUND,
                    oz_test_utils:call_oz(
                        Config, user_logic, get_default_provider, [?ROOT, User]
                    )
                );
            (false = _ShouldSucceed, _, _) ->
                ?assertMatch(
                    {ok, P1},
                    oz_test_utils:get_user_default_provider(Config, User)
                )
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = <<"/user/default_provider">>,
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = unset_default_provider,
            args = [client, User],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


list_groups_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    ExpGroups = lists:map(
        fun(Idx) ->
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?USER(User), <<"Group", (Idx+48)/integer>>
            ),
            GroupId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/groups">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            operation = get,
            module = user_logic,
            function = get_groups,
            args = [client, User],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


create_group_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    AllPrivs = oz_test_utils:call_oz(Config, privileges, group_privileges, []),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    VerifyFun = fun(GroupId, ExpName, ExpType) ->
        {ok, Group} = oz_test_utils:get_group(Config, GroupId),
        ?assertEqual(ExpName, Group#od_group.name),
        ?assertEqual(ExpType, Group#od_group.type),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/groups">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, Data) ->
                    ExpName = maps:get(<<"name">>, Data),
                    ExpType = maps:get(<<"type">>, Data, role),
                    fun(#{<<"location">> := Location} = _Headers) ->
                        % TODO 2918
%%                        <<"/user/groups/", GroupId/binary>> = Location,
                        <<"/groups/", GroupId/binary>> = Location,
                        VerifyFun(GroupId, ExpName, ExpType)
                    end
                end
            )
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_group, aspect = instance},
            auth_hint = ?AS_USER(User),
            expected_result = ?OK_ENV(fun(_, Data) ->
                ExpName = maps:get(<<"name">>, Data),
                ExpType = maps:get(<<"type">>, Data, role),

                ?OK_MAP(#{
                    <<"children">> => #{},
                    <<"effectiveChildren">> => #{},
                    <<"users">> => #{User => AllPrivsBin},
                    <<"effectiveUsers">> => #{User => AllPrivsBin},
                    <<"name">> => ExpName,
                    <<"parents">> => [],
                    <<"spaces">> => [],
                    <<"type">> => atom_to_binary(ExpType, utf8),
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = oz_test_utils:decode_gri(
                            Config, EncodedGri
                        ),
                        VerifyFun(Id, ExpName, ExpType)
                    end
                })
            end)
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            optional = [<<"type">>],
            correct_values = #{
                <<"name">> => [fun() ->
                    UniqueInt = erlang:unique_integer([positive]),
                    <<"Group", (integer_to_binary(UniqueInt))/binary>>
                end],
                <<"type">> => [organization, unit, team, role]
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)},
                {<<"type">>, kingdom,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>,
                        [organization,unit,team,role])},
                {<<"type">>, 1234, ?ERROR_BAD_VALUE_ATOM(<<"type">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_group,
            args = [client, User, data],
            expected_result = ?OK_ENV(fun(_, Data) ->
                ExpName = maps:get(<<"name">>, Data),
                ExpType = maps:get(<<"type">>, Data, role),
                ?OK_TERM(fun(GroupId) ->
                    VerifyFun(GroupId, ExpName, ExpType)
                end)
            end)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


join_group_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, <<"G1">>),

    GenTokenFun = fun() ->
        {ok, Macaroon} = oz_test_utils:group_invite_user_token(
            Config, ?ROOT, G1
        ),
        {ok, Token} = token_utils:serialize62(Macaroon),
        Token
    end,
    VerifyEndFun = fun
        (true = _ShouldSucceed, _, _) ->
            {ok, Groups} = oz_test_utils:get_user_groups(Config, User),
            ?assertEqual(lists:member(G1, Groups), true),
            oz_test_utils:group_remove_user(Config, G1, User),
            {ok, NewGroups} = oz_test_utils:get_user_groups(Config, User),
            ?assertEqual(lists:member(G1, NewGroups), false);
        (false = _ShouldSucceed, _, _) ->
            {ok, Groups} = oz_test_utils:get_user_groups(Config, User),
            ?assertEqual(lists:member(G1, Groups), false)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/groups/join">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                <<"/user/groups/", GroupId/binary>> = Location,
                ?assertEqual(GroupId, G1),
                true
            end
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [GenTokenFun]
            },
            bad_values = [
                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, <<"123qwe">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = join_group,
            args = [client, User, data],
            expected_result = ?OK_BINARY(G1)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, undefined, undefined, VerifyEndFun
    )).


get_group_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    ExpName = <<"G2">>,
    ExpType = team,
    ExpTypeBinary = atom_to_binary(ExpType, utf8),
    {ok, Group} = oz_test_utils:create_group(
        Config, ?USER(User), #{<<"name">> => ExpName, <<"type">> => ExpType}
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/groups/">>, Group],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"groupId">> => Group,
                <<"name">> => ExpName,
                <<"type">> => ExpTypeBinary
            }
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = Group,
                aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_USER(User),
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"type">> => ExpTypeBinary,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, Group)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_group,
            args = [client, User, Group],
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"type">> => ExpType
            })
        },
        gs_spec = undefined
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


leave_group_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, GroupId} = oz_test_utils:create_group(
            Config, ?USER(User), <<"Group">>
        ),
        #{groupId => GroupId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, _) ->
        {ok, ChildrenGroups} = oz_test_utils:get_user_groups(Config, User),
        ?assertEqual(lists:member(GroupId, ChildrenGroups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/groups/">>, groupId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = leave_group,
            args = [client, User, groupId],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun]
    )).


list_eff_groups_test(Config) ->
    {
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_groups_env(Config),

    ExpGroups = [G1, G2, G3, G4, G5],
    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/effective_groups">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"effective_groups">> => ExpGroups}
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_groups,
            args = [client, U2],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_eff_group_test(Config) ->
    {
        EffGroups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_groups_env(Config),

    lists:foreach(
        fun({GroupId, GroupDetails}) ->
            User = lists:nth(rand:uniform(2), [U1, U2]),
            ExpType = maps:get(<<"type">>, GroupDetails, role),

            ApiTestSpec = #api_test_spec{
                client_spec = ClientSpec = #client_spec{
                    correct = [
                        root,
                        {user, User}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/user/effective_groups/">>, GroupId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = GroupDetails#{
                        <<"groupId">> => GroupId,
                        <<"type">> => atom_to_binary(
                            maps:get(<<"type">>, GroupDetails), utf8
                        )
                    }
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_group, id = GroupId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_USER(User),
                    expected_result = ?OK_MAP(
                        GroupDetails#{
                            <<"type">> => atom_to_binary(ExpType, utf8),
                            <<"gri">> => fun(EncodedGri) ->
                                #gri{id = Id} = oz_test_utils:decode_gri(
                                    Config, EncodedGri
                                ),
                                ?assertEqual(Id, GroupId)
                            end
                        })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

            % For apis that allow it check also
            % unauthorized and forbidden clients
            ApiTestSpec2 = ApiTestSpec#api_test_spec{
                client_spec = ClientSpec#client_spec{
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = undefined,
                logic_spec = #logic_spec{
                    module = user_logic,
                    function = get_eff_group,
                    args = [client, User, GroupId],
                    expected_result = ?OK_MAP(GroupDetails)
                },
                gs_spec = undefined
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, EffGroups
    ).


create_space_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    ExpName = <<"SpaceName">>,
    AllPrivs = oz_test_utils:call_oz(Config, privileges, space_privileges, []),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    VerifyFun = fun(SpaceId) ->
        {ok, Space} = oz_test_utils:get_space(Config, SpaceId),
        ?assertEqual(ExpName, Space#od_space.name),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/spaces">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                % TODO 2918
%%                <<"/user/spaces/", SpaceId/binary>> = Location,
                <<"/spaces/", SpaceId/binary>> = Location,
                VerifyFun(SpaceId)
            end
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_space, aspect = instance},
            auth_hint = ?AS_USER(User),
            expected_result = ?OK_MAP(#{
                <<"effectiveGroups">> => #{},
                <<"effectiveUsers">> => #{User => AllPrivsBin},
                <<"groups">> => #{},
                <<"name">> => ExpName,
                <<"providers">> => #{},
                <<"shares">> => [],
                <<"users">> => #{User => AllPrivsBin},
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    VerifyFun(Id)
                end
            })
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            correct_values = #{
                <<"name">> => [ExpName]
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_space,
            args = [client, User, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = undefined
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


list_spaces_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(User), <<"S2">>),
    {ok, S3} = oz_test_utils:create_space(Config, ?USER(User), <<"S3">>),
    {ok, S4} = oz_test_utils:create_space(Config, ?USER(User), <<"S4">>),
    ExpSpaces = [S1, S2, S3, S4],

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/spaces">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"default">> => <<"undefined">>,
                <<"spaces">> => ExpSpaces}
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_spaces,
            args = [client, User],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


join_space_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, SpaceId} = oz_test_utils:create_space(Config, ?ROOT, <<"Sp">>),
        #{spaceId => SpaceId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:get_user_spaces(Config, User),
        ?assertEqual(lists:member(SpaceId, Spaces), ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/spaces/join">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{spaceId := SpaceId} = _Env, _) ->
                fun(#{<<"location">> := Location} = _Headers) ->
                    <<"/user/spaces/", Id/binary>> = Location,
                    ?assertEqual(SpaceId, Id),
                    true
                end
            end)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [fun(#{spaceId := SpaceId} = _Env) ->
                    {ok, Macaroon} = oz_test_utils:space_invite_user_token(
                        Config, ?ROOT, SpaceId
                    ),
                    {ok, Token} = token_utils:serialize62(Macaroon),
                    Token
                end]
            },
            bad_values = [
                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, <<"123qwe">>,
                    ?ERROR_BAD_VALUE_TOKEN(<<"token">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = join_space,
            args = [client, User, data],
            expected_result = ?OK_ENV(fun(#{spaceId := SpaceId} = _Env, _) ->
                ?OK_BINARY(SpaceId)
            end)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


get_space_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    ExpName = <<"S1">>,
    ExpMap = #{<<"name">> => ExpName, <<"providersSupports">> => #{}},
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ExpName),

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/spaces/">>, S1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpMap#{<<"spaceId">> => S1}
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_space, id = S1, aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_USER(User),
            expected_result = ?OK_MAP(ExpMap#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, S1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_space,
            args = [client, User, S1],
            expected_result = ?OK_MAP(ExpMap)
        },
        gs_spec = undefined
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


leave_space_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, SpaceId} = oz_test_utils:create_space(
            Config, ?USER(User), <<"Sp">>
        ),
        #{spaceId => SpaceId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:get_user_spaces(Config, User),
        ?assertEqual(lists:member(SpaceId, Spaces), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/spaces/">>, spaceId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = leave_space,
            args = [client, User, spaceId],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun]
    )).


set_space_alias_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),

    VerifyEndFun =
        fun
            (true = _ShouldSucceed, _, #{<<"alias">> := ExpAlias} = _Data) ->
                ?assertMatch(
                    {ok, ExpAlias},
                    oz_test_utils:get_user_space_alias(Config, User, S1)
                ),
                oz_test_utils:unset_user_space_alias(Config, User, S1);
            (false = _ShouldSucceed, _, _) ->
                ?assertMatch(
                    ?ERROR_NOT_FOUND,
                    oz_test_utils:call_oz(
                        Config, user_logic, get_space_alias, [?ROOT, User, S1]
                    )
                )
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/user/spaces/">>, S1, <<"/alias">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"alias">>],
            correct_values = #{
                <<"alias">> => [fun() ->
                    UniqueInt = erlang:unique_integer([positive]),
                    <<"Space", (integer_to_binary(UniqueInt))/binary>>
                end]
            },
            bad_values = [
                {<<"alias">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"alias">>)},
                {<<"alias">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"alias">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = set_space_alias,
            args = [client, User, S1, data],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, undefined, undefined, VerifyEndFun
    )).


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
            module = user_logic,
            function = get_space_alias,
            args = [client, User, Space],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Set an alias for given space
    ExpAlias = <<"Alias">>,
    ok = oz_test_utils:set_user_space_alias(Config, User, Space, ExpAlias),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"alias">> => ExpAlias}
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?OK_BINARY(ExpAlias)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % Unset the space alias and check if it's not present again
    ok = oz_test_utils:unset_user_space_alias(Config, User, Space),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


delete_space_alias_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),

    EnvSetUpFun = fun() ->
        UniqueInt = erlang:unique_integer([positive]),
        Alias = <<"Space", (integer_to_binary(UniqueInt))/binary>>,
        oz_test_utils:set_user_space_alias(Config, User, S1, Alias),
        #{<<"alias">> => Alias}
    end,
    VerifyEndFun =
        fun
            (true = _ShouldSucceed, _, _) ->
                ?assertMatch(
                    ?ERROR_NOT_FOUND,
                    oz_test_utils:call_oz(
                        Config, user_logic, get_space_alias, [?ROOT, User, S1]
                    )
                );
            (false = _ShouldSucceed, _, #{<<"alias">> := ExpAlias} = _Data) ->
                ?assertMatch(
                    {ok, ExpAlias},
                    oz_test_utils:get_user_space_alias(Config, User, S1)
                )
        end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/spaces/">>, S1, <<"/alias">>],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = delete_space_alias,
            args = [client, User, S1],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


list_eff_spaces_test(Config) ->
    {
        [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}],
        _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_spaces_env(Config),

    % Create also space for user only
    {ok, S6} = oz_test_utils:create_space(Config, ?USER(U2), <<"S6">>),

    ExpSpaces = [S1, S2, S3, S4, S5, S6],
    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/effective_spaces">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_spaces,
            args = [client, U2],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_eff_space_test(Config) ->
    {
        EffSpacesList, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_spaces_env(Config),

    % Create also space for user only
    {ok, S6} = oz_test_utils:create_space(Config, ?USER(U2), <<"S6">>),
    S6Details = #{<<"name">> => <<"S6">>, <<"providersSupports">> => #{}},
    {ok, U1} = oz_test_utils:add_user_to_space(Config, S6, U1),
    NewEffSpacesList = [{S6, S6Details} | EffSpacesList],

    lists:foreach(
        fun({SpaceId, SpaceDetails}) ->
            User = lists:nth(rand:uniform(2), [U1, U2]),

            ApiTestSpec = #api_test_spec{
                client_spec = ClientSpec = #client_spec{
                    correct = [
                        root,
                        {user, User}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/user/effective_spaces/">>, SpaceId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = SpaceDetails#{<<"spaceId">> => SpaceId}
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_space, id = SpaceId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_USER(User),
                    expected_result = ?OK_MAP(SpaceDetails#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Id, SpaceId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

            % For apis that allow it check also
            % unauthorized and forbidden clients
            ApiTestSpec2 = ApiTestSpec#api_test_spec{
                client_spec = ClientSpec#client_spec{
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = undefined,
                logic_spec = #logic_spec{
                    module = user_logic,
                    function = get_eff_space,
                    args = [client, User, SpaceId],
                    expected_result = ?OK_MAP(SpaceDetails)
                },
                gs_spec = undefined
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, NewEffSpacesList
    ).


list_eff_providers_test(Config) ->
    {
        [{P1, _}, {P2, _}, {P3, _}, {P4, _}],
        _Spaces, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    ExpProviders = [P1, P2, P3, P4],
    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/effective_providers">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"providers">> => ExpProviders}
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_providers,
            args = [client, U2],
            expected_result = ?OK_LIST(ExpProviders)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_eff_provider_test(Config) ->
    {
        EffProviders, _Spaces, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    lists:foreach(
        fun({ProvId, ProvDetails}) ->
            User = lists:nth(rand:uniform(2), [U1, U2]),

            NewProvDetails = ProvDetails#{
                <<"clientName">> => maps:get(<<"name">>, ProvDetails)
            },
            ApiTestSpec = #api_test_spec{
                client_spec = ClientSpec = #client_spec{
                    correct = [
                        root,
                        {user, User}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/user/effective_providers/">>, ProvId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = NewProvDetails#{<<"providerId">> => ProvId}
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_provider, id = ProvId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_USER(User),
                    expected_result = ?OK_MAP(NewProvDetails#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Id, ProvId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

            % For apis that allow it check also
            % unauthorized and forbidden clients
            ApiTestSpec2 = ApiTestSpec#api_test_spec{
                client_spec = ClientSpec#client_spec{
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = undefined,
                logic_spec = #logic_spec{
                    module = user_logic,
                    function = get_eff_provider,
                    args = [client, User, ProvId],
                    expected_result = ?OK_MAP(NewProvDetails)
                },
                gs_spec = undefined
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, EffProviders
    ).


create_handle_service_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, User, grant, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),

    ExpName = <<"HS">>,
    ExpProxyEndPoint = <<"http://dot.com">>,
    ExpProperties = #{<<"asd">> => 1},

    VerifyFun = fun(HServiceId) ->
        {ok, HS} = oz_test_utils:get_handle_service(Config, HServiceId),
        ?assertEqual(ExpName, HS#od_handle_service.name),
        ?assertEqual(ExpProxyEndPoint, HS#od_handle_service.proxy_endpoint),
        ?assertEqual(ExpProperties, HS#od_handle_service.service_properties),
        true
    end,

    AllPrivs = oz_test_utils:call_oz(
        Config, privileges, handle_service_privileges, []
    ),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/handle_services">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                <<"/handle_services/", HServiceId/binary>> = Location,
                VerifyFun(HServiceId)
            end
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_handle_service, aspect = instance},
            auth_hint = ?AS_USER(User),
            expected_result = ?OK_MAP(#{
                <<"effectiveGroups">> => #{},
                <<"effectiveUsers">> => #{User => AllPrivsBin},
                <<"name">> => ExpName,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    VerifyFun(Id)
                end
            })
        },
        data_spec = #data_spec{
            required = [
                <<"name">>,
                <<"proxyEndpoint">>,
                <<"serviceProperties">>
            ],
            correct_values = #{
                <<"name">> => [ExpName],
                <<"proxyEndpoint">> => [ExpProxyEndPoint],
                <<"serviceProperties">> => [ExpProperties]
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)},
                {<<"proxyEndpoint">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"proxyEndpoint">>)},
                {<<"serviceProperties">>, 1234,
                    ?ERROR_BAD_VALUE_JSON(<<"serviceProperties">>)},
                {<<"serviceProperties">>, #{},
                    ?ERROR_BAD_VALUE_EMPTY(<<"serviceProperties">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_handle_service,
            args = [client, User, data],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


list_handle_services_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    ExpHandleServices = lists:map(
        fun(Idx) ->
            {ok, HServiceId} = oz_test_utils:create_handle_service(
                Config, ?ROOT, <<"HS", (Idx+48)/integer>>,
                <<"https://dot.com">>, #{asd => 1}
            ),

            {ok, User} = oz_test_utils:add_user_to_handle_service(
                Config, HServiceId, User
            ),
            HServiceId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/handle_services">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handle_services">> => ExpHandleServices}
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_handle_services,
            args = [client, User],
            expected_result = ?OK_LIST(ExpHandleServices)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_handle_service_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    ExpName = <<"HS">>,
    ExpProxyEndPoint = <<"http://dot.com">>,
    ExpProperties = #{<<"asd">> => 1},
    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ExpName, ExpProxyEndPoint, ExpProperties
    ),
    {ok, User} = oz_test_utils:add_user_to_handle_service(
        Config, HServiceId, User
    ),

    ExpHServiceDetails = #{
        <<"name">> => ExpName,
        <<"proxyEndpoint">> => ExpProxyEndPoint,
        <<"serviceProperties">> => ExpProperties
    },
    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/handle_services/">>, HServiceId],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpHServiceDetails#{
                <<"handleServiceId">> => HServiceId
            }
        }
        % TODO gs
%%        gs_spec = #gs_spec{
%%            operation = get,
%%            gri = #gri{
%%                type = od_handle_service, id = HServiceId,
%%                aspect = instance, scope = protected
%%            },
%%            auth_hint = ?THROUGH_USER(User),
%%            expected_result = ?OK_MAP(ExpHServiceDetails#{
%%                <<"gri">> => fun(EncodedGri) ->
%%                    #gri{id = Id} = oz_test_utils:decode_gri(
%%                        Config, EncodedGri
%%                    ),
%%                    ?assertEqual(Id, HServiceId)
%%                end
%%            })
%%        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_handle_service,
            args = [client, User, HServiceId],
            expected_result = ?OK_MAP(ExpHServiceDetails)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


leave_handle_service_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpEnv = fun() ->
        {ok, HServiceId} = oz_test_utils:create_handle_service(
            Config, ?ROOT, <<"HS">>, <<"http://dot.com">>, #{<<"asd">> => 1}
        ),
        {ok, User} = oz_test_utils:add_user_to_handle_service(
            Config, HServiceId, User
        ),
        #{hserviceId => HServiceId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{hserviceId := HServiceId} = _Env, _) ->
        {ok, Users} = oz_test_utils:get_handle_service_users(
            Config, HServiceId
        ),
        ?assertEqual(lists:member(User, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/handle_services/">>, hserviceId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = leave_handle_service,
            args = [client, User, hserviceId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpEnv, undefined, VerifyEndFun
    )).


list_eff_handle_services_test(Config) ->
    {
        [{HS1, _}, {HS2, _}, {HS3, _}, {HS4, _}, {HS5, _}],
        _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_handle_services_env(Config),

    % Create additional HService for user only
    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, U2} = oz_test_utils:add_user_to_handle_service(
        Config, HServiceId, U2
    ),

    ExpHandleServices = [HS1, HS2, HS3, HS4, HS5, HServiceId],
    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/effective_handle_services">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handle_services">> => ExpHandleServices}
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_handle_services,
            args = [client, U2],
            expected_result = ?OK_LIST(ExpHandleServices)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_eff_handle_service_test(Config) ->
    {
        EffHandleServices, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_handle_services_env(Config),

    % Create additional HService for user only
    {ok, HS6} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, U2} = oz_test_utils:add_user_to_handle_service(
        Config, HS6, U2
    ),
    {ok, U1} = oz_test_utils:add_user_to_handle_service(
        Config, HS6, U1
    ),

    NewEffHandleServices = [{HS6, ?DOI_SERVICE} | EffHandleServices],
    lists:foreach(
        fun({HServiceId, HServiceDetails}) ->
            User = lists:nth(rand:uniform(2), [U1, U2]),

            ApiTestSpec = #api_test_spec{
                client_spec = ClientSpec = #client_spec{
                    correct = [
                        root,
                        {user, User}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/user/effective_handle_services/">>, HServiceId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = HServiceDetails#{
                        <<"handleServiceId">> => HServiceId
                    }
                }
                % TODO gs
%%                gs_spec = #gs_spec{
%%                    operation = get,
%%                    gri = #gri{
%%                        type = od_handle_service, id = HServiceId,
%%                        aspect = instance, scope = protected
%%                    },
%%                    auth_hint = ?THROUGH_USER(User),
%%                    expected_result = ?OK_MAP(HServiceDetails#{
%%                        <<"gri">> => fun(EncodedGri) ->
%%                            #gri{id = Id} = oz_test_utils:decode_gri(
%%                                Config, EncodedGri
%%                            ),
%%                            ?assertEqual(Id, HServiceId)
%%                        end
%%                    })
%%                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

            % For apis that allow it check also
            % unauthorized and forbidden clients
            ApiTestSpec2 = ApiTestSpec#api_test_spec{
                client_spec = ClientSpec#client_spec{
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = undefined,
                logic_spec = #logic_spec{
                    module = user_logic,
                    function = get_eff_handle_service,
                    args = [client, User, HServiceId],
                    expected_result = ?OK_MAP(HServiceDetails)
                },
                gs_spec = undefined
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, NewEffHandleServices
    ).


create_handle_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),

    UniqueInt = erlang:unique_integer([positive]),
    ShareId = <<"Share", (integer_to_binary(UniqueInt))/binary>>,
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ShareId, <<"share">>, <<"file">>, S1
    ),

    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, User} = oz_test_utils:add_user_to_handle_service(
        Config, HServiceId, User
    ),

    AllPrivs = oz_test_utils:call_oz(
        Config, privileges, handle_privileges, []
    ),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    ExpResourceType = <<"Share">>,
    VerifyFun = fun(HandleId) ->
        {ok, Handle} = oz_test_utils:get_handle(Config, HandleId),
        ?assertEqual(ExpResourceType, Handle#od_handle.resource_type),
        ?assertEqual(ShareId, Handle#od_handle.resource_id),
        ?assertEqual(HServiceId, Handle#od_handle.handle_service),
        ?assertEqual(#{}, Handle#od_handle.groups),
        ?assertEqual(#{}, Handle#od_handle.eff_groups),
        ?assertEqual(#{User => AllPrivs}, Handle#od_handle.users),
        ?assertEqual(
            #{User => {AllPrivs, [{od_handle, HandleId}]}},
            Handle#od_handle.eff_users
        ),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/handles">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                % TODO 2918
%%                <<"/user/handles/", HandleId/binary>> = Location,
                <<"/handles/", HandleId/binary>> = Location,
                VerifyFun(HandleId)
            end
        },
        data_spec = DataSpec = #data_spec{
            required = [
                <<"handleServiceId">>,
                <<"resourceType">>,
                <<"resourceId">>,
                <<"metadata">>
            ],
            correct_values = #{
                <<"handleServiceId">> => [HServiceId],
                <<"resourceType">> => [<<"Share">>],
                <<"resourceId">> => [ShareId],
                <<"metadata">> => [?DC_METADATA]
            },
            bad_values = [
                % one cannot check privileges of hs if it does not exist so 403
                {<<"handleServiceId">>, <<"">>, ?ERROR_FORBIDDEN},
                {<<"handleServiceId">>, 1234, ?ERROR_FORBIDDEN},
                {<<"resourceType">>, <<"">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resourceType">>,
                        [<<"Share">>])},
                {<<"resourceType">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"resourceType">>)},
                % one cannot check privileges of resource
                % if it does not exist so 403
                {<<"resourceId">>, <<"">>, ?ERROR_FORBIDDEN},
                {<<"resourceId">>, 1234, ?ERROR_FORBIDDEN},
                {<<"metadata">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"metadata">>)},
                {<<"metadata">>, <<"">>,
                    ?ERROR_BAD_VALUE_EMPTY(<<"metadata">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_handle,
            args = [client, User, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_handle, aspect = instance},
            auth_hint = ?AS_USER(User),
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"effectiveGroups">> => #{},
                <<"effectiveUsers">> => #{User => AllPrivsBin},
                <<"metadata">> => ?DC_METADATA,
                <<"handleServiceId">> => HServiceId,
                <<"resourceType">> => ExpResourceType,
                <<"resourceId">> => ShareId,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    VerifyFun(Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % root bypass authorize so malformed data result in errors
    % in validation step not authorize
    RootApiTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [root]
        },
        gs_spec = undefined,
        data_spec = DataSpec#data_spec{
            bad_values = [
                % one cannot check privileges of hs if it does not exist so 403
                {<<"handleServiceId">>, <<"">>,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"handleServiceId">>)},
                {<<"handleServiceId">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"handleServiceId">>)},
                {<<"resourceType">>, <<"">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resourceType">>,
                        [<<"Share">>])},
                {<<"resourceType">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"resourceType">>)},
                % one cannot check privileges of resource
                % if it does not exist so 403
                {<<"resourceId">>, <<"">>,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"resourceId">>)},
                {<<"resourceId">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"resourceId">>)},
                {<<"metadata">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"metadata">>)},
                {<<"metadata">>, <<"">>,
                    ?ERROR_BAD_VALUE_EMPTY(<<"metadata">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec)).


list_handles_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(User), <<"S2">>),

    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),

    % Create 3 handles, 2 for S1 and 1 for S2
    ExpHandles = lists:map(
        fun(SpaceId) ->
            UniqueInt = erlang:unique_integer([positive]),
            ShareId = <<"Share", (integer_to_binary(UniqueInt))/binary>>,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, <<"share">>, <<"file">>, SpaceId
            ),
            {ok, HandleId} = oz_test_utils:create_handle(
                Config, ?ROOT, ?HANDLE(HServiceId, ShareId)
            ),
            {ok, User} = oz_test_utils:add_user_to_handle(
                Config, HandleId, User
            ),
            HandleId
        end, [S1, S1, S2]
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/handles">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handles">> => ExpHandles}
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_handles,
            args = [client, User],
            expected_result = ?OK_LIST(ExpHandles)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_handle_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),

    UniqueInt = erlang:unique_integer([positive]),
    ShareId = <<"Share", (integer_to_binary(UniqueInt))/binary>>,
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ShareId, <<"share">>, <<"file">>, S1
    ),

    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, User} = oz_test_utils:add_user_to_handle_service(
        Config, HServiceId, User
    ),

    HandleDetails = ?HANDLE(HServiceId, ShareId),
    {ok, HandleId} = oz_test_utils:create_handle(Config, ?ROOT, HandleDetails),
    {ok, User} = oz_test_utils:add_user_to_handle(Config, HandleId, User),

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/handles/">>, HandleId],
            expected_code = ?HTTP_200_OK,
            expected_body = {contains, HandleDetails}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_handle,
            args = [client, User, HandleId],
            expected_result = ?OK_MAP_CONTAINS(HandleDetails)
        }
        % TODO gs
%%        gs_spec = #gs_spec{
%%            operation = get,
%%            gri = #gri{
%%                type = od_handle, id = HandleId,
%%                aspect = instance, scope = protected
%%            },
%%            auth_hint = ?THROUGH_USER(User),
%%            expected_result = ?OK_MAP_CONTAINS(HandleDetails#{
%%                <<"gri">> => fun(EncodedGri) ->
%%                    #gri{id = Id} = oz_test_utils:decode_gri(
%%                        Config, EncodedGri
%%                    ),
%%                    ?assertEqual(Id, HandleId)
%%                end
%%            })
%%        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


leave_handle_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), <<"S1">>),

    UniqueInt = erlang:unique_integer([positive]),
    ShareId = <<"Share", (integer_to_binary(UniqueInt))/binary>>,
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ShareId, <<"share">>, <<"file">>, S1
    ),

    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, User} = oz_test_utils:add_user_to_handle_service(
        Config, HServiceId, User
    ),

    EnvSetUpFun = fun() ->
        {ok, HandleId} = oz_test_utils:create_handle(
            Config, ?ROOT, ?HANDLE(HServiceId, ShareId)
        ),
        {ok, User} = oz_test_utils:add_user_to_handle(Config, HandleId, User),
        #{handleId => HandleId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{handleId := HandleId} = _Env, _) ->
        {ok, Users} = oz_test_utils:get_handle_users(Config, HandleId),
        ?assertEqual(lists:member(User, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/handles/">>, handleId],
            expected_code = ?HTTP_202_ACCEPTED
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % For apis that allow it check also unauthorized and forbidden clients
    {ok, SomeUser} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, SomeUser}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = leave_handle,
            args = [client, User, handleId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


list_eff_handles_test(Config) ->
    {
        [{H1, _}, {H2, _}, {H3, _}, {H4, _}, {H5, _}],
        _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_handles_env(Config),

    ExpHandles = [H1, H2, H3, H4, H5],
    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/effective_handles">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handles">> => ExpHandles}
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check that some other user cannot list ours eff handles
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_handles,
            args = [client, U2],
            expected_result = ?OK_LIST(ExpHandles)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_eff_handle_test(Config) ->
    {
        EffHandlesList, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_handles_env(Config),

    lists:foreach(
        fun({HandleId, HandleDetails}) ->
            User = lists:nth(rand:uniform(2), [U1, U2]),

            ApiTestSpec = #api_test_spec{
                client_spec = ClientSpec = #client_spec{
                    correct = [
                        root,
                        {user, User}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/user/effective_handles/">>, HandleId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = {
                        contains, HandleDetails#{<<"handleId">> => HandleId}
                    }
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

            % For apis that allow it check also
            % unauthorized and forbidden clients
            ApiTestSpec2 = ApiTestSpec#api_test_spec{
                client_spec = ClientSpec#client_spec{
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = undefined,
                logic_spec = #logic_spec{
                    module = user_logic,
                    function = get_eff_handle,
                    args = [client, User, HandleId],
                    expected_result = ?OK_MAP_CONTAINS(HandleDetails)
                }
                % TODO gs
%%                gs_spec = #gs_spec{
%%                    operation = get,
%%                    gri = #gri{
%%                        type = od_handle, id = HandleId,
%%                        aspect = instance, scope = protected
%%                    },
%%                    auth_hint = ?THROUGH_USER(User),
%%                    expected_result = ?OK_MAP(HandleDetails#{
%%                        <<"gri">> => fun(EncodedGri) ->
%%                            #gri{id = Id} = oz_test_utils:decode_gri(
%%                                Config, EncodedGri
%%                            ),
%%                            ?assertEqual(Id, HandleId)
%%                        end
%%                    })
%%                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, EffHandlesList
    ).


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


init_per_testcase(create_handle_test, Config) ->
    init_per_testcase(handle_tests, Config);
init_per_testcase(list_handles_test, Config) ->
    init_per_testcase(handle_tests, Config);
init_per_testcase(get_handle_test, Config) ->
    init_per_testcase(handle_tests, Config);
init_per_testcase(leave_handle_test, Config) ->
    init_per_testcase(handle_tests, Config);
init_per_testcase(list_eff_handles_test, Config) ->
    init_per_testcase(handle_tests, Config);
init_per_testcase(get_eff_handle_test, Config) ->
    init_per_testcase(handle_tests, Config);
init_per_testcase(handle_tests, Config) ->
    oz_test_utils:mock_handle_proxy(Config),
    Config;
init_per_testcase(_, Config) ->
    Config.


end_per_testcase(create_handle_test, Config) ->
    end_per_testcase(handle_tests, Config);
end_per_testcase(list_handles_test, Config) ->
    end_per_testcase(handle_tests, Config);
end_per_testcase(get_handle_test, Config) ->
    end_per_testcase(handle_tests, Config);
end_per_testcase(leave_handle_test, Config) ->
    end_per_testcase(handle_tests, Config);
end_per_testcase(list_eff_handles_test, Config) ->
    end_per_testcase(handle_tests, Config);
end_per_testcase(get_eff_handle_test, Config) ->
    end_per_testcase(handle_tests, Config);
end_per_testcase(handle_tests, Config) ->
    oz_test_utils:unmock_handle_proxy(Config);
end_per_testcase(_, _Config) ->
    ok.
