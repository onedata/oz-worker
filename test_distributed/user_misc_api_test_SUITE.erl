%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user misc API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(user_misc_api_test_SUITE).
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
    init_per_suite/1, end_per_suite/1
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

    set_default_provider_test/1,
    get_default_provider_test/1,
    unset_default_provider_test/1,

    list_eff_providers_test/1,
    get_eff_provider_test/1
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

        set_default_provider_test,
        get_default_provider_test,
        unset_default_provider_test,

        list_eff_providers_test,
        get_eff_provider_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


create_test(Config) ->
    ExpName = ?USER_NAME1,
    ExpAlias = ?USER_ALIAS1,
    UserRecord = #od_user{name = ExpName, alias = ExpAlias},

    % Try to create user without predefined id
    {ok, UserId} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, user_logic, create, [UserRecord]
    )),
    {ok, User} = oz_test_utils:get_user(Config, UserId),
    ?assertEqual(User#od_user.name, ExpName),
    ?assertEqual(User#od_user.alias, ExpAlias),

    % Try to create a user with given Id
    PredefinedUserId = <<"ausdhf87adsga87ht2q7hrw">>,
    {ok, PredefinedUserId} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, user_logic, create, [UserRecord, PredefinedUserId]
    )),
    {ok, User2} = oz_test_utils:get_user(Config, PredefinedUserId),
    ?assertEqual(User2#od_user.name, ExpName),
    ?assertEqual(User2#od_user.alias, ExpAlias),

    % Second try should fail (such id exists)
    ?assertMatch(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"userId">>),
        oz_test_utils:call_oz(
            Config, user_logic, create, [UserRecord, PredefinedUserId]
        )
    ).


authorize_test(Config) ->
    % Create a provider and a user.
    {ok, {Provider, _}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME1
    ),
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    % Generate an auth token, parse the token for 3rd party caveats and check
    % if authorize endpoint works as expected.
    AuthToken = oz_test_utils:call_oz(
        Config, auth_logic, gen_token, [User, Provider]
    ),
    {ok, Macaroon} = onedata_macaroons:deserialize(AuthToken),
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
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_USERS_LIST
    ]),
    {ok, {P1, P1Macaroon}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME1
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
                {provider, P1, P1Macaroon}
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also user_logic:exists function
    lists:foreach(
        fun(User) ->
            ?assert(oz_test_utils:call_oz(Config, user_logic, exists, [User]))
        end, ExpUsers
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, user_logic, exists, [<<"asdiucyaie827346w">>])
    ).


get_test(Config) ->
    {ok, {P1, P1Macaroon}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME1
    ),
    {ok, User} = oz_test_utils:create_user(Config, #od_user{
        name = ExpName = <<"UserName">>,
        alias = ExpAlias = <<"UserAlias">>,
        email_list = ExpEmailList = [<<"a@a.a">>, <<"b@b.b">>]
    }),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_USERS_LIST
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    {ok, S1} = oz_test_utils:support_space(
        Config, P1, S1, oz_test_utils:minimum_support_size(Config)
    ),

    ProtectedData = #{
        <<"name">> => ExpName,
        <<"alias">> => ExpAlias,
        <<"emailList">> => ExpEmailList,
        <<"linkedAccounts">> => []
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
                {provider, P1, P1Macaroon},
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
                    name = Name, alias = Alias,
                    email_list = EmailList,
                    basic_auth_enabled = false,
                    linked_accounts = [],

                    default_space = undefined,
                    default_provider = undefined,
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
                <<"alias">> => ExpAlias,
                <<"login">> => ExpAlias,% TODO VFS-4506 deprecated, included for backward compatibility
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
                {user, NonAdmin},
                {provider, P1, P1Macaroon}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User],
            expected_code = ?HTTP_200_OK,
            expected_body = ProtectedData#{<<"userId">> => User}
        },
        logic_spec = LogicSpec = #logic_spec{
            module = user_logic,
            function = get_protected_data,
            args = [client, User],
            expected_result = ?OK_MAP(ProtectedData)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_user, id = User, aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP(ProtectedData#{
                <<"login">> => ExpAlias,% TODO VFS-4506 deprecated, included for backward compatibility
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
        rest_spec = undefined,
        logic_spec = LogicSpec#logic_spec{
            function = get_shared_data,
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"alias">> => ExpAlias
            })
        },
        gs_spec = GsSpec#gs_spec{
            gri = #gri{
                type = od_user, id = User, aspect = instance, scope = shared
            },
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"alias">> => ExpAlias,
                <<"login">> => ExpAlias,% TODO VFS-4506 deprecated, included for backward compatibility
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
        alias = ExpAlias = <<"Alias">>,
        email_list = ExpEmailList = [
            <<"em1@google.com">>, <<"em2@google.com">>
        ]
    }),

    ProtectedData = #{
        <<"name">> => ExpName,
        <<"alias">> => ExpAlias,
        <<"emailList">> => ExpEmailList,
        <<"linkedAccounts">> => []
    },

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, User}]},
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user">>,
            expected_code = ?HTTP_200_OK,
            expected_body = ProtectedData#{<<"userId">> => User}
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_user, id = ?SELF,
                aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP(ProtectedData#{
                <<"login">> => ExpAlias, % TODO VFS-4506 deprecated, included for backward compatibility
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
    UsedAlias = ?UNIQUE_STRING,
    {ok, _U1} = oz_test_utils:create_user(Config, #od_user{alias = UsedAlias}),

    % Trying to set owned alias again should not raise any error
    OwnedAlias = ?UNIQUE_STRING,
    EnvSetUpFun = fun() ->
        {ok, User} = oz_test_utils:create_user(Config, #od_user{
            name = ?USER_NAME1, alias = OwnedAlias
        }),
        #{userId => User}
    end,
    EnvTeardownFun = fun(#{userId := UserId} = _Env) ->
        oz_test_utils:delete_user(Config, UserId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := UserId} = _Env, Data) ->
        {ok, User} = oz_test_utils:get_user(Config, UserId),
        {ExpName, Alias} = case ShouldSucceed of
            false ->
                {?USER_NAME1, OwnedAlias};
            true ->
                {
                    maps:get(<<"name">>, Data, ?USER_NAME1),
                    maps:get(<<"alias">>, Data, OwnedAlias)
                }
        end,
        ExpAlias = case Alias of
            null -> undefined;
            _ -> Alias
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
            at_least_one = [<<"name">>, <<"alias">>],
            correct_values = #{
                <<"name">> => [?CORRECT_USER_NAME],
                <<"alias">> => [fun() -> ?UNIQUE_STRING end, OwnedAlias, null]
            },
            bad_values = [
                {<<"alias">>, <<"">>, ?ERROR_BAD_VALUE_ALIAS},
                {<<"alias">>, <<"_asd">>, ?ERROR_BAD_VALUE_ALIAS},
                {<<"alias">>, <<"-asd">>, ?ERROR_BAD_VALUE_ALIAS},
                {<<"alias">>, <<"asd_">>, ?ERROR_BAD_VALUE_ALIAS},
                {<<"alias">>, <<"verylongaliaswithatleast15chars">>, ?ERROR_BAD_VALUE_ALIAS},
                {<<"alias">>, 1234, ?ERROR_BAD_VALUE_ALIAS},
                {<<"alias">>, UsedAlias,
                    ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"alias">>)},
                {<<"name">>, <<"a_d">>, ?ERROR_BAD_VALUE_USER_NAME},
                {<<"name">>, <<"_ad">>, ?ERROR_BAD_VALUE_USER_NAME},
                {<<"name">>, <<"ad_">>, ?ERROR_BAD_VALUE_USER_NAME}
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_USER_NAME)
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, EnvTeardownFun, VerifyEndFun
    )),

    % Check that regular client can't make request on behalf of other client
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
        Config, ApiTestSpec2, EnvSetUpFun, EnvTeardownFun, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_USERS_DELETE
    ]),

    EnvSetUpFun = fun() ->
        {ok, UserId} = oz_test_utils:create_user(Config, #od_user{}),
        #{userId => UserId}
    end,
    DeleteEntityFun = fun(#{userId := UserId} = _Env) ->
        oz_test_utils:delete_user(Config, UserId)
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
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = delete,
            args = [client, userId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_user, id = userId, aspect = instance},
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )),

    % Also check that user can delete himself
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{correct = [{user, userId}]}
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
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
        client_spec = #client_spec{correct = [{user, userId}]},
        rest_spec = #rest_spec{
            method = delete,
            path = <<"/user">>,
            expected_code = ?HTTP_204_NO_CONTENT
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
        {ok, Macaroon} = onedata_macaroons:deserialize(ClientToken),
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

    % Check that regular client can't make request on behalf of other client
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

    % Check that regular client can't make request on behalf of other client
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
            expected_code = ?HTTP_204_NO_CONTENT
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check that regular client can't make request on behalf of other client
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


set_default_provider_test(Config) ->
    {ok, {P1, _}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME1
    ),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),

    EnvSetUpFun = fun() ->
        {ok, {ProviderId, _}} = oz_test_utils:create_provider(
            Config, ?PROVIDER_NAME2
        ),
        {ok, S1} = oz_test_utils:support_space(
            Config, ProviderId, S1, oz_test_utils:minimum_support_size(Config)
        ),
        #{providerId => ProviderId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{providerId := ProviderId} = _Env, _) ->
        Result = oz_test_utils:call_oz(
            Config, user_logic, get_default_provider, [?ROOT, U1]
        ),
        case ShouldSucceed of
            true -> ?assertMatch({ok, ProviderId}, Result);
            false -> ?assertNotMatch({ok, ProviderId}, Result)
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = put,
            path = <<"/user/default_provider">>,
            expected_code = ?HTTP_204_NO_CONTENT
        },
        data_spec = #data_spec{
            required = [<<"providerId">>],
            correct_values = #{
                <<"providerId">> => [providerId]
            },
            bad_values = [
                {<<"providerId">>, <<"">>,
                    ?ERROR_BAD_VALUE_EMPTY(<<"providerId">>)},
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

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = set_default_provider,
            args = [client, U1, data],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


get_default_provider_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % Newly created user should not have a default provider
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
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
            args = [client, U1],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Set a default provider for user
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),

    {ok, {P1, _}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME1
    ),
    {ok, S1} = oz_test_utils:support_space(
        Config, P1, S1, oz_test_utils:minimum_support_size(Config)
    ),
    oz_test_utils:user_set_default_provider(Config, U1, P1),

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

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec3 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_default_provider,
            args = [client, U1],
            expected_result = ?OK_BINARY(P1)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)),

    % Unset the default provider and check if it's not present again
    oz_test_utils:user_unset_default_provider(Config, U1),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


unset_default_provider_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),

    EnvSetUpFun = fun() ->
        {ok, {ProviderId, _}} = oz_test_utils:create_provider(
            Config, ?PROVIDER_NAME2
        ),
        {ok, S1} = oz_test_utils:support_space(
            Config, ProviderId, S1, oz_test_utils:minimum_support_size(Config)
        ),
        oz_test_utils:user_set_default_provider(Config, U1, ProviderId),
        #{providerId => ProviderId}
    end,
    DeleteEntityFun = fun(_Env) ->
        oz_test_utils:user_unset_default_provider(Config, U1)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{providerId := ProviderId} = _Env, _) ->
        Result = oz_test_utils:call_oz(
            Config, user_logic, get_default_provider, [?ROOT, U1]
        ),
        case ShouldSucceed of
            true -> ?assertMatch(?ERROR_NOT_FOUND, Result);
            false -> ?assertMatch({ok, ProviderId}, Result)
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = delete,
            path = <<"/user/default_provider">>,
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = unset_default_provider,
            args = [client, U1],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])).


list_eff_providers_test(Config) ->
    {
        [{P1, _}, {P2, _}, {P3, _}, {P4, _}],
        _Spaces, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    ExpProviders = [P1, P2, P3, P4],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/effective_providers">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"providers">> => ExpProviders}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
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
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_providers,
            args = [client, U2],
            expected_result = ?OK_LIST(ExpProviders)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % check also user_logic:has_eff_provider function
    lists:foreach(
        fun(ProviderId) ->
            ?assert(oz_test_utils:call_oz(
                Config, user_logic, has_eff_provider, [U2, ProviderId])
            )
        end, ExpProviders
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, user_logic, has_eff_provider, [U2, <<"asdiucyaie827346w">>])
    ).


get_eff_provider_test(Config) ->
    {
        EffProviders, _Spaces, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    lists:foreach(
        fun({ProvId, ProvDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        {user, U1},
                        {user, U2}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/user/effective_providers/">>, ProvId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = ProvDetails#{<<"providerId">> => ProvId}
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

            % Check that regular client can't make request
            % on behalf of other client
            ApiTestSpec2 = #api_test_spec{
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
                rest_spec = undefined,
                logic_spec = #logic_spec{
                    module = user_logic,
                    function = get_eff_provider,
                    args = [client, U2, ProvId],
                    expected_result = ?OK_MAP(ProvDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_provider, id = ProvId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_USER(U2),
                    expected_result = ?OK_MAP(ProvDetails#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Id, ProvId)
                        end
                    })
                }

            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, EffProviders
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
