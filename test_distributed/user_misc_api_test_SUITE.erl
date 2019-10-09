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
    init_per_suite/1, end_per_suite/1
]).
-export([
    create_test/1,
    create_with_predefined_id_test/1,
    preauthorize_test/1,
    list_test/1,
    get_test/1,
    get_self_test/1,
    update_test/1,
    change_password_test/1,
    update_basic_auth_config/1,
    delete_test/1,
    delete_self_test/1,

    create_client_token_test/1,
    list_client_tokens_test/1,
    delete_client_token_test/1,

    set_default_provider_test/1,
    get_default_provider_test/1,
    unset_default_provider_test/1,
    acquire_idp_access_token_test/1,

    list_eff_providers_test/1,
    get_eff_provider_test/1,
    get_spaces_in_eff_provider_test/1
]).

all() ->
    ?ALL([
        create_test,
        create_with_predefined_id_test,
        preauthorize_test,
        list_test,
        get_test,
        get_self_test,
        update_test,
        change_password_test,
        update_basic_auth_config,
        delete_test,
        delete_self_test,

        create_client_token_test,
        list_client_tokens_test,
        delete_client_token_test,

        set_default_provider_test,
        get_default_provider_test,
        unset_default_provider_test,
        acquire_idp_access_token_test,

        list_eff_providers_test,
        get_eff_provider_test,
        get_spaces_in_eff_provider_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    TestCases = [
        %   fullName        username       password
        {default_value, default_value, default_value},

        {default_value, ?UNIQUE_STRING, default_value},
        {?UNIQUE_STRING, default_value, default_value},
        {?UNIQUE_STRING, ?UNIQUE_STRING, default_value},

        {default_value, ?UNIQUE_STRING, ?UNIQUE_STRING},
        {?UNIQUE_STRING, ?UNIQUE_STRING, ?UNIQUE_STRING}
    ],

    lists:foreach(fun({FullName, Username, Password}) ->


        ExpFullName = case FullName of
            default_value -> ?DEFAULT_FULL_NAME;
            _ -> FullName
        end,

        ExpUsername = case Username of
            default_value -> undefined;
            _ -> Username
        end,

        ExpBasicAuthEnabled = case Password of
            default_value -> false;
            _ -> true
        end,

        EnvSetUp = fun() ->
            {ok, NonAdmin} = oz_test_utils:create_user(Config),
            #{non_admin => NonAdmin}
        end,

        EnvTearDown = fun(_) ->
            oz_test_utils:delete_all_entities(Config)
        end,

        VerifyFun = fun(UserId) ->
            {ok, User} = oz_test_utils:get_user(Config, UserId),
            ?assertEqual(ExpFullName, User#od_user.full_name),
            ?assertEqual(ExpUsername, User#od_user.username),
            ?assertEqual(ExpBasicAuthEnabled, User#od_user.basic_auth_enabled),
            case Password of
                default_value ->
                    ok;
                _ ->
                    ?assert(onedata_passwords:verify(Password, User#od_user.password_hash))
            end,
            true
        end,

        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_CREATE]}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, non_admin}
                ]
            },
            rest_spec = #rest_spec{
                method = post,
                path = <<"/users">>,
                expected_code = ?HTTP_201_CREATED,
                expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/users/">>]),
                    [UserId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(UserId)
                end
            },
            logic_spec = #logic_spec{
                module = user_logic,
                function = create,
                args = [auth, data],
                expected_result = ?OK_TERM(VerifyFun)
            },
            gs_spec = #gs_spec{
                operation = create,
                gri = #gri{type = od_user, aspect = instance},
                expected_result = ?OK_MAP_CONTAINS(#{
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
                        VerifyFun(Id)
                    end
                })
            },
            data_spec = #data_spec{
                required = lists:flatten([
                    case FullName of default_value -> []; _ -> <<"fullName">> end,
                    case Username of default_value -> []; _ -> <<"username">> end,
                    case Password of default_value -> []; _ -> <<"password">> end
                ]),
                correct_values = lists:foldl(fun maps:merge/2, #{}, [
                    case FullName of default_value -> #{}; Val -> #{<<"fullName">> => [Val]} end,
                    case Username of default_value -> #{}; Val -> #{<<"username">> => [Val]} end,
                    case Password of default_value -> #{}; Val -> #{<<"password">> => [Val]} end
                ]),
                bad_values = ?BAD_VALUES_FULL_NAME(?ERROR_BAD_VALUE_FULL_NAME)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUp, EnvTearDown, undefined))
    end, TestCases).


create_with_predefined_id_test(Config) ->
    % Creating users with predefined ids is reserved for internal Onezone logic
    % (?ROOT auth).
    PredefinedUserId = <<"ausdhf87adsga87ht2q7hrw">>,
    ExpFullName = ?USER_FULL_NAME1,
    ExpUsername = ?UNIQUE_STRING,
    UserData = #{<<"fullName">> => ExpFullName, <<"username">> => ExpUsername},
    {ok, PredefinedUserId} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, user_logic, create, [?ROOT, PredefinedUserId, UserData]
    )),
    {ok, User} = oz_test_utils:get_user(Config, PredefinedUserId),
    ?assertEqual(ExpFullName, User#od_user.full_name),
    ?assertEqual(ExpUsername, User#od_user.username),

    % Second try should fail (such id exists)
    ?assertMatch(?ERROR_ALREADY_EXISTS,
        oz_test_utils:call_oz(
            Config, user_logic, create, [?ROOT, PredefinedUserId, #{<<"fullName">> => ?UNIQUE_STRING}]
        )
    ),

    % Reusing the already occupied username should fail
    ?assertMatch(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"username">>),
        oz_test_utils:call_oz(
            Config, user_logic, create, [?ROOT, #{<<"username">> => ExpUsername}]
        )
    ).


preauthorize_test(Config) ->
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, SubjectUser} = oz_test_utils:create_user(Config),
    {ok, AnotherUser} = oz_test_utils:create_user(Config),

    {ok, Space} = oz_test_utils:create_space(Config, ?USER(SubjectUser), ?UNIQUE_STRING),
    {ok, Space} = oz_test_utils:support_space(Config, Provider, Space),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    TokenCaveats = [
        #cv_time{valid_until = 3600 + oz_test_utils:cluster_time_seconds(Config)},
        #cv_audience{whitelist = [?AUD(?OP_WORKER, Provider)]}
    ],

    {ok, OldAccessToken} = oz_test_utils:create_client_token(Config, SubjectUser),
    {ok, NewAccessTokenDeserialized} = oz_test_utils:call_oz(
        Config, token_logic, create_user_named_token, [?USER(SubjectUser), SubjectUser, ?UNIQUE_STRING, ?ACCESS_TOKEN, TokenCaveats, #{}]
    ),
    {ok, NewAccessToken} = tokens:serialize(NewAccessTokenDeserialized),
    {ok, TemporaryTokenDeserialized} = oz_test_utils:call_oz(
        Config, token_logic, create_user_temporary_token, [?USER(SubjectUser), SubjectUser, ?ACCESS_TOKEN, TokenCaveats]
    ),
    {ok, TemporaryToken} = tokens:serialize(TemporaryTokenDeserialized),

    ErrAudCavUnverified = ?ERROR_REASON(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_audience{whitelist = [?AUD(?OP_WORKER, Provider)]})
    ),
    CheckSuccess = fun(ExpCaveats) ->
        ?OK_TERM(fun({Subject, Caveats}) ->
            Subject =:= ?SUB(user, SubjectUser) andalso Caveats =:= ExpCaveats
        end)
    end,

    Testcases = [
        {{user, SubjectUser}, OldAccessToken, CheckSuccess([])},
        {{user, SubjectUser}, NewAccessToken, ErrAudCavUnverified},
        {{user, SubjectUser}, TemporaryToken, ErrAudCavUnverified},
        {{user, AnotherUser}, OldAccessToken, CheckSuccess([])},
        {{user, AnotherUser}, NewAccessToken, ErrAudCavUnverified},
        {{user, AnotherUser}, TemporaryToken, ErrAudCavUnverified},
        {{provider, Provider, ProviderToken}, OldAccessToken, CheckSuccess([])},
        {{provider, Provider, ProviderToken}, NewAccessToken, CheckSuccess(TokenCaveats)},
        {{provider, Provider, ProviderToken}, TemporaryToken, CheckSuccess(TokenCaveats)}
    ],

    lists:foreach(fun({Client, Token, ExpResult}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [Client]
            },
            %% @TODO VFS-5727 - implement REST API for new tokens
            logic_spec = #logic_spec{
                module = user_logic,
                function = preauthorize,
                args = [auth, data],
                expected_result = ExpResult
            },
            data_spec = #data_spec{
                required = [<<"token">>],
                optional = [<<"peerIp">>],
                correct_values = #{
                    <<"peerIp">> => [<<"187.10.4.217">>],
                    <<"token">> => [Token]
                },
                bad_values = [
                    {<<"peerIp">>, <<"">>,
                        ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                    {<<"peerIp">>, 123,
                        ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                    {<<"peerIp">>, <<"187.257.4.217">>,
                        ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                    {<<"token">>, <<"Sdfsdf">>,
                        ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                    {<<"token">>, 123,
                        ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)}
                ]
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, Testcases).


list_test(Config) ->
    % Make sure that users created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, Admin} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, Admin, [
        ?OZ_USERS_LIST
    ], []),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
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
                {provider, P1, P1Token}
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
            args = [auth],
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
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME1
    ),
    {ok, User} = oz_test_utils:create_user(Config, #{
        <<"fullName">> => ExpFullName = <<"UserName">>,
        <<"username">> => ExpUsername = <<"UserUsername">>
    }),
    ExpEmailList = [<<"a@a.a">>, <<"b@b.b">>],
    oz_test_utils:call_oz(Config, od_user, update, [User, fun(UserRecord) ->
        {ok, UserRecord#od_user{emails = ExpEmailList}}
    end]),

    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    {ok, S1} = oz_test_utils:support_space(
        Config, P1, S1, oz_test_utils:minimum_support_size(Config)
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ProtectedData = #{
        <<"fullName">> => ExpFullName,
        <<"username">> => ExpUsername,
        <<"emails">> => ExpEmailList,
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
                {provider, P1, P1Token},
                {user, NonAdmin},
                {admin, [?OZ_USERS_VIEW]}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = get,
            args = [auth, User],
            expected_result = ?OK_TERM(
                fun(#od_user{
                    full_name = FullName, username = Username,
                    emails = EmailList,
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
                    ?assertEqual(ExpFullName, FullName),
                    ?assertEqual(ExpUsername, Username),
                    ?assertEqual(ExpEmailList, EmailList),
                    ?assertEqual(SpaceId, S1),
                    ?assertEqual(EffSpaces, #{S1 => [{od_user, <<"self">>}]}),
                    ?assertEqual(EffProviders, #{P1 => [{od_space, S1}]})
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_user, id = User, aspect = instance},
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"defaultSpaceId">> => null,
                <<"effectiveGroups">> => [],
                <<"effectiveHandleServices">> => [],
                <<"effectiveHandles">> => [],
                <<"effectiveSpaces">> => [S1],
                <<"emails">> => ExpEmailList,
                <<"linkedAccounts">> => [],
                <<"fullName">> => ExpFullName,
                <<"username">> => ExpUsername,
                <<"spaceAliases">> => #{},
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(User, Id)
                end,

                % TODO VFS-4506 deprecated fields, included for backward compatibility
                <<"name">> => ExpFullName,
                <<"login">> => ExpUsername,
                <<"alias">> => ExpUsername,
                <<"emailList">> => ExpEmailList
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
                {admin, [?OZ_USERS_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P1, P1Token}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User],
            expected_code = ?HTTP_200_OK,
            expected_body = ProtectedData#{
                <<"userId">> => User,
                <<"basicAuthEnabled">> => false,

                % TODO VFS-4506 deprecated fields, included for backward compatibility
                <<"name">> => ExpFullName,
                <<"login">> => ExpUsername,
                <<"alias">> => ExpUsername,
                <<"emailList">> => ExpEmailList
            }
        },
        logic_spec = LogicSpec = #logic_spec{
            module = user_logic,
            function = get_protected_data,
            args = [auth, User],
            expected_result = ?OK_MAP_CONTAINS(ProtectedData)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_user, id = User, aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP_CONTAINS(ProtectedData#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(Id, User)
                end,

                % TODO VFS-4506 deprecated fields, included for backward compatibility
                <<"name">> => ExpFullName,
                <<"login">> => ExpUsername,
                <<"alias">> => ExpUsername,
                <<"emailList">> => ExpEmailList
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)),

    % Get and check shared data
    GetSharedDataApiTestSpec = GetProtectedDataApiTestSpec#api_test_spec{
        rest_spec = undefined,
        logic_spec = LogicSpec#logic_spec{
            function = get_shared_data,
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"fullName">> => ExpFullName,
                <<"username">> => ExpUsername
            })
        },
        gs_spec = GsSpec#gs_spec{
            gri = #gri{
                type = od_user, id = User, aspect = instance, scope = shared
            },
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"fullName">> => ExpFullName,
                <<"username">> => ExpUsername,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(Id, User)
                end,

                % TODO VFS-4506 deprecated fields, included for backward compatibility
                <<"name">> => ExpFullName,
                <<"login">> => ExpUsername,
                <<"alias">> => ExpUsername
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetSharedDataApiTestSpec)).


get_self_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #{
        <<"fullName">> => ExpFullName = <<"FullName">>,
        <<"username">> => ExpUsername = <<"Username">>
    }),
    ExpEmailList = [<<"em1@google.com">>, <<"em2@google.com">>],
    oz_test_utils:call_oz(Config, od_user, update, [User, fun(UserRecord) ->
        {ok, UserRecord#od_user{emails = ExpEmailList}}
    end]),

    ProtectedData = #{
        <<"fullName">> => ExpFullName,
        <<"username">> => ExpUsername,
        <<"emails">> => ExpEmailList,
        <<"linkedAccounts">> => [],

        % TODO VFS-4506 deprecated, included for backward compatibility
        <<"name">> => ExpFullName,
        <<"login">> => ExpUsername,
        <<"alias">> => ExpUsername,
        <<"emailList">> => ExpEmailList
    },

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, User}]},
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user">>,
            expected_code = ?HTTP_200_OK,
            expected_body = ProtectedData#{
                <<"userId">> => User,
                <<"basicAuthEnabled">> => false
            }
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_user, id = ?SELF,
                aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP_CONTAINS(ProtectedData#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(Id, User)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_test(Config) ->
    OccupiedUsername = ?UNIQUE_STRING,
    oz_test_utils:create_user(Config, #{<<"username">> => OccupiedUsername}),

    CurrentUsername = ?UNIQUE_STRING,
    EnvSetUpFun = fun() ->
        {ok, UserId} = oz_test_utils:create_user(Config, #{
            <<"fullName">> => ?USER_FULL_NAME1, <<"username">> => CurrentUsername
        }),
        #{userId => UserId}
    end,
    EnvTeardownFun = fun(#{userId := UserId} = _Env) ->
        oz_test_utils:delete_user(Config, UserId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := UserId} = _Env, Data) ->
        {ok, UserRecord} = oz_test_utils:get_user(Config, UserId),
        {ExpFullName, ExpUsername} = case ShouldSucceed of
            false ->
                {?USER_FULL_NAME1, CurrentUsername};
            true ->
                {
                    maps:get(<<"fullName">>, Data, ?USER_FULL_NAME1),
                    maps:get(<<"username">>, Data, CurrentUsername)
                }
        end,
        ?assertEqual(ExpFullName, UserRecord#od_user.full_name),
        ?assertEqual(ExpUsername, UserRecord#od_user.username)
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
            expected_result = ?OK_RES
        },
        data_spec = DataSpec = #data_spec{
            at_least_one = [<<"fullName">>, <<"username">>],
            correct_values = #{
                <<"fullName">> => [?CORRECT_USER_NAME],
                % Trying to set current username again should not raise any error
                <<"username">> => [CurrentUsername, fun() -> ?UNIQUE_STRING end]
            },
            bad_values = [
                {<<"username">>, <<"">>, ?ERROR_BAD_VALUE_USERNAME},
                {<<"username">>, <<"_asd">>, ?ERROR_BAD_VALUE_USERNAME},
                {<<"username">>, <<"-asd">>, ?ERROR_BAD_VALUE_USERNAME},
                {<<"username">>, <<"asd_">>, ?ERROR_BAD_VALUE_USERNAME},
                {<<"username">>, null, ?ERROR_BAD_VALUE_USERNAME},
                {<<"username">>, <<"verylongusernamewithatleast20chars">>, ?ERROR_BAD_VALUE_USERNAME},
                {<<"username">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"username">>)},
                {<<"username">>, OccupiedUsername,
                    ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"username">>)},
                {<<"fullName">>, <<"a_d">>, ?ERROR_BAD_VALUE_FULL_NAME},
                {<<"fullName">>, <<"_ad">>, ?ERROR_BAD_VALUE_FULL_NAME},
                {<<"fullName">>, <<"ad_">>, ?ERROR_BAD_VALUE_FULL_NAME}
                | ?BAD_VALUES_FULL_NAME(?ERROR_BAD_VALUE_FULL_NAME)
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, EnvTeardownFun, VerifyEndFun
    )),

    % Check that regular client can't make request on behalf of other client
    {ok, SomeUser} = oz_test_utils:create_user(Config),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            correct = [{admin, [?OZ_USERS_UPDATE]}],
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = update,
            args = [auth, userId, data],
            expected_result = ?OK_RES
        },
        gs_spec = GsSpec#gs_spec{
            gri = #gri{type = od_user, id = userId, aspect = instance}
        },
        data_spec = DataSpec#data_spec{
            correct_values = #{
                <<"fullName">> => [?CORRECT_USER_NAME],
                <<"username">> => [fun() -> ?UNIQUE_STRING end, CurrentUsername]
            }
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, EnvTeardownFun, VerifyEndFun
    )).


change_password_test(Config) ->
    TestCases = [
        % {WasBasicAuthEnabled, OldPassword, ExpHttpCode, ExpResult}
        {false, undefined, ?HTTP_400_BAD_REQUEST, ?ERROR_REASON(?ERROR_BASIC_AUTH_DISABLED)},
        {true, undefined, ?HTTP_204_NO_CONTENT, ?OK_RES},
        {true, ?UNIQUE_STRING, ?HTTP_204_NO_CONTENT, ?OK_RES}
    ],

    lists:foreach(fun({WasBasicAuthEnabled, OldPassword, ExpHttpCode, ExpResult}) ->
        EnvSetUpFun = fun() ->
            UserData = case OldPassword of
                undefined -> #{<<"username">> => ?UNIQUE_STRING};
                _ -> #{<<"username">> => ?UNIQUE_STRING, <<"password">> => OldPassword}
            end,
            {ok, UserId} = oz_test_utils:create_user(Config, UserData),
            oz_test_utils:call_oz(Config, user_logic, toggle_basic_auth, [?ROOT, UserId, WasBasicAuthEnabled]),
            #{userId => UserId}
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
                path = <<"/user/password">>,
                expected_code = ExpHttpCode
            },
            gs_spec = GsSpec = #gs_spec{
                operation = update,
                gri = #gri{type = od_user, id = ?SELF, aspect = password},
                expected_result = ExpResult
            },
            data_spec = #data_spec{
                required = [<<"oldPassword">>, <<"newPassword">>],
                correct_values = #{
                    <<"oldPassword">> => [gs_protocol:undefined_to_null(OldPassword)],
                    <<"newPassword">> => [?UNIQUE_STRING]
                },
                bad_values = [
                    {<<"oldPassword">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"oldPassword">>)},
                    {<<"oldPassword">>, 34.5, ?ERROR_BAD_VALUE_BINARY(<<"oldPassword">>)},
                    {<<"oldPassword">>, <<"bad-old-password">>, case WasBasicAuthEnabled of
                        false -> ?ERROR_BASIC_AUTH_DISABLED;
                        true -> ?ERROR_BAD_BASIC_CREDENTIALS
                    end},
                    {<<"newPassword">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"newPassword">>)},
                    {<<"newPassword">>, 34.5, ?ERROR_BAD_VALUE_BINARY(<<"newPassword">>)},
                    {<<"newPassword">>, null, ?ERROR_BAD_VALUE_PASSWORD},
                    {<<"newPassword">>, <<"">>, ?ERROR_BAD_VALUE_PASSWORD},
                    {<<"newPassword">>, <<"short">>, ?ERROR_BAD_VALUE_PASSWORD}
                ]
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, undefined)),

        % Check that regular client can't make request on behalf of other client
        {ok, SomeUser} = oz_test_utils:create_user(Config),
        ApiTestSpec2 = ApiTestSpec#api_test_spec{
            client_spec = ClientSpec#client_spec{
                unauthorized = [nobody],
                forbidden = [{user, SomeUser}]
            },
            rest_spec = undefined,
            logic_spec = #logic_spec{
                module = user_logic,
                function = change_password,
                args = [auth, userId, data],
                expected_result = ExpResult
            },
            gs_spec = GsSpec#gs_spec{
                gri = #gri{type = od_user, id = userId, aspect = password}
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec2, EnvSetUpFun, undefined, undefined))
    end, TestCases).


update_basic_auth_config(Config) ->
    TestCases = [
        % {WasBasicAuthEnabled, OldPassword, Data, ExpHttpCode, ExpResult}
        {
            false, undefined, #{<<"newPassword">> => ?UNIQUE_STRING},
            ?HTTP_400_BAD_REQUEST, ?ERROR_REASON(?ERROR_BASIC_AUTH_DISABLED)
        },
        {
            false, ?UNIQUE_STRING, #{<<"newPassword">> => ?UNIQUE_STRING},
            ?HTTP_400_BAD_REQUEST, ?ERROR_REASON(?ERROR_BASIC_AUTH_DISABLED)
        },
        {
            true, undefined, #{<<"newPassword">> => ?UNIQUE_STRING},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },
        {
            true, ?UNIQUE_STRING, #{<<"newPassword">> => ?UNIQUE_STRING},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },

        {
            false, undefined, #{<<"newPassword">> => ?UNIQUE_STRING, <<"basicAuthEnabled">> => false},
            ?HTTP_400_BAD_REQUEST, ?ERROR_REASON(?ERROR_BASIC_AUTH_DISABLED)
        },
        {
            false, ?UNIQUE_STRING, #{<<"newPassword">> => ?UNIQUE_STRING, <<"basicAuthEnabled">> => false},
            ?HTTP_400_BAD_REQUEST, ?ERROR_REASON(?ERROR_BASIC_AUTH_DISABLED)
        },
        {
            true, undefined, #{<<"newPassword">> => ?UNIQUE_STRING, <<"basicAuthEnabled">> => false},
            ?HTTP_400_BAD_REQUEST, ?ERROR_REASON(?ERROR_BASIC_AUTH_DISABLED)
        },
        {
            true, ?UNIQUE_STRING, #{<<"newPassword">> => ?UNIQUE_STRING, <<"basicAuthEnabled">> => false},
            ?HTTP_400_BAD_REQUEST, ?ERROR_REASON(?ERROR_BASIC_AUTH_DISABLED)
        },

        {
            false, undefined, #{<<"basicAuthEnabled">> => false},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },
        {
            true, undefined, #{<<"basicAuthEnabled">> => false},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },
        {
            false, ?UNIQUE_STRING, #{<<"basicAuthEnabled">> => false},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },
        {
            true, ?UNIQUE_STRING, #{<<"basicAuthEnabled">> => false},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },

        {
            false, undefined, #{<<"basicAuthEnabled">> => true},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },
        {
            true, undefined, #{<<"basicAuthEnabled">> => true},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },
        {
            false, ?UNIQUE_STRING, #{<<"basicAuthEnabled">> => true},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },
        {
            true, ?UNIQUE_STRING, #{<<"basicAuthEnabled">> => true},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },

        {
            false, undefined, #{<<"newPassword">> => ?UNIQUE_STRING, <<"basicAuthEnabled">> => true},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },
        {
            false, ?UNIQUE_STRING, #{<<"newPassword">> => ?UNIQUE_STRING, <<"basicAuthEnabled">> => true},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },
        {
            true, undefined, #{<<"newPassword">> => ?UNIQUE_STRING, <<"basicAuthEnabled">> => true},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        },
        {
            true, ?UNIQUE_STRING, #{<<"newPassword">> => ?UNIQUE_STRING, <<"basicAuthEnabled">> => true},
            ?HTTP_204_NO_CONTENT, ?OK_RES
        }
    ],

    lists:foreach(fun({WasBasicAuthEnabled, OldPassword, Data, ExpHttpCode, ExpResult}) ->
        {ok, NonAdmin} = oz_test_utils:create_user(Config),

        EnvSetUpFun = fun() ->
            UserData = case OldPassword of
                undefined -> #{<<"username">> => ?UNIQUE_STRING};
                _ -> #{<<"username">> => ?UNIQUE_STRING, <<"password">> => OldPassword}
            end,
            {ok, UserId} = oz_test_utils:create_user(Config, UserData),
            oz_test_utils:call_oz(Config, user_logic, toggle_basic_auth, [?ROOT, UserId, WasBasicAuthEnabled]),

            #{userId => UserId}
        end,

        VerifyEndFun = fun(WasDataCorrect, #{userId := UserId}, RequestData) ->
            % Success depends if the test case generator picked correct data and
            % we expected the whole operation to be successful (it might fail
            % due to different reasons than request Data).
            ShouldSucceed = WasDataCorrect andalso ExpResult =:= ?OK_RES,
            {ok, User} = oz_test_utils:get_user(Config, UserId),
            case ShouldSucceed of
                true ->
                    BasicAuthEnabled = maps:get(<<"basicAuthEnabled">>, RequestData, WasBasicAuthEnabled),
                    ?assertEqual(User#od_user.basic_auth_enabled, BasicAuthEnabled),
                    CurrentPassword = case RequestData of
                        #{<<"newPassword">> := NewPassword} -> NewPassword;
                        _ -> OldPassword
                    end,
                    case {BasicAuthEnabled, CurrentPassword} of
                        {false, _} -> ok;
                        {true, undefined} -> ok;
                        _ -> ?assert(onedata_passwords:verify(CurrentPassword, User#od_user.password_hash))
                    end;
                false ->
                    ?assertEqual(WasBasicAuthEnabled, User#od_user.basic_auth_enabled),
                    case {WasBasicAuthEnabled, OldPassword} of
                        {false, _} -> ok;
                        {true, undefined} -> ok;
                        _ -> ?assert(onedata_passwords:verify(OldPassword, User#od_user.password_hash))
                    end
            end
        end,

        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_MANAGE_PASSWORDS]}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, userId},
                    {user, NonAdmin}
                ]
            },
            rest_spec = #rest_spec{
                method = patch,
                path = [<<"/users/">>, userId, <<"/basic_auth">>],
                expected_code = ExpHttpCode
            },
            logic_spec = #logic_spec{
                module = user_logic,
                function = update_basic_auth_config,
                args = [auth, userId, data],
                expected_result = ExpResult
            },
            data_spec = #data_spec{
                required = maps:keys(Data),
                correct_values = maps:map(fun(_Key, Val) ->
                    [Val]
                end, Data),
                bad_values = [
                    {<<"newPassword">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"newPassword">>)},
                    {<<"newPassword">>, 34.5, ?ERROR_BAD_VALUE_BINARY(<<"newPassword">>)},
                    {<<"newPassword">>, null, ?ERROR_BAD_VALUE_PASSWORD},
                    {<<"newPassword">>, <<"">>, ?ERROR_BAD_VALUE_PASSWORD},
                    {<<"newPassword">>, <<"short">>, ?ERROR_BAD_VALUE_PASSWORD}
                ]
            }
        },

        ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun))
    end, TestCases).


delete_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, UserId} = oz_test_utils:create_user(Config),
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
                {admin, [?OZ_USERS_DELETE]}
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
            args = [auth, userId],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_user, id = userId, aspect = instance},
            expected_result = ?OK_RES
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
        {ok, UserId} = oz_test_utils:create_user(Config),
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
            expected_result = ?OK_RES
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


create_client_token_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),

    VerifyFun = fun(ClientToken) ->
        {ok, Token} = tokens:deserialize(ClientToken),
        ?assertEqual({true, ?USER(User)}, oz_test_utils:call_oz(
            Config, token_auth, check_token_auth, [Token, undefined, undefined]
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
    {ok, SomeUser} = oz_test_utils:create_user(Config),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_client_token,
            args = [auth, User],
            expected_result = ?OK_TERM(VerifyFun)
        },
        rest_spec = undefined
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


list_client_tokens_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, Token} = oz_test_utils:create_client_token(Config, User),

    ExpTokens = [Token | lists:map(
        fun(_) ->
            {ok, Serialized} = oz_test_utils:create_client_token(Config, User),
            Serialized
        end, lists:seq(1, 5)
    )],

    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [
                root,
                {user, User, Token}
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
    {ok, SomeUser} = oz_test_utils:create_user(Config),
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
            args = [auth, User],
            expected_result = ?OK_LIST(ExpTokens)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


delete_client_token_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),

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
    {ok, SomeUser} = oz_test_utils:create_user(Config),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            unauthorized = [nobody],
            forbidden = [{user, SomeUser}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = delete_client_token,
            args = [auth, User, token],
            expected_result = ?OK_RES
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
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),

    EnvSetUpFun = fun() ->
        {ok, {ProviderId, _}} = oz_test_utils:create_provider(
            Config, ?PROVIDER_NAME2
        ),
        {ok, S1} = oz_test_utils:support_space(
            Config, ProviderId, S1, oz_test_utils:minimum_support_size(Config)
        ),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

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
            args = [auth, U1, data],
            expected_result = ?OK_RES
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )).


get_default_provider_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

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
            args = [auth, U1],
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
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
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
            args = [auth, U1],
            expected_result = ?OK_BINARY(P1)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)),

    % Unset the default provider and check if it's not present again
    oz_test_utils:user_unset_default_provider(Config, U1),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


unset_default_provider_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),

    EnvSetUpFun = fun() ->
        {ok, {ProviderId, _}} = oz_test_utils:create_provider(
            Config, ?PROVIDER_NAME2
        ),
        {ok, S1} = oz_test_utils:support_space(
            Config, ProviderId, S1, oz_test_utils:minimum_support_size(Config)
        ),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

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
            args = [auth, U1],
            expected_result = ?OK_RES
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])).


acquire_idp_access_token_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % Offline access disabled in given IdP
    oz_test_utils:overwrite_auth_config(Config, #{
        openidConfig => #{
            enabled => true
        },
        supportedIdps => [
            {dummyIdP, #{
                protocol => openid,
                protocolConfig => #{
                    plugin => default_oidc_plugin,
                    offlineAccess => false
                }
            }}
        ]
    }),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = post,
            path = <<"/user/idp_access_token/dummyIdP">>,
            expected_code = ?HTTP_400_BAD_REQUEST
        },
        logic_spec = LogicSpec = #logic_spec{
            module = user_logic,
            function = acquire_idp_access_token,
            args = [auth, U1, dummyIdP],
            expected_result = ?ERROR_REASON(?ERROR_BAD_VALUE_NOT_ALLOWED(<<"idp">>, []))
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),


    % Inexistent IdP
    oz_test_utils:overwrite_auth_config(Config, #{
        openidConfig => #{
            enabled => true
        },
        supportedIdps => [
            {dummyIdP, #{
                protocol => openid,
                protocolConfig => #{
                    plugin => default_oidc_plugin,
                    offlineAccess => true
                }
            }}
        ]
    }),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/idp_access_token/inexistentIdP">>,
            expected_code = ?HTTP_400_BAD_REQUEST
        },
        logic_spec = LogicSpec#logic_spec{
            args = [auth, U1, inexistentIdP],
            expected_result = ?ERROR_REASON(?ERROR_BAD_VALUE_NOT_ALLOWED(<<"idp">>, [dummyIdP]))
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),


    % Newly created user should not have any IdP access tokens cached
    ApiTestSpec3 = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/idp_access_token/dummyIdP">>,
            expected_code = ?HTTP_404_NOT_FOUND
        },
        logic_spec = LogicSpec#logic_spec{
            args = [auth, U1, dummyIdP],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)),

    % Simulate user login
    DummyAccessToken = <<"abcdef">>,
    Now = oz_test_utils:cluster_time_seconds(Config),
    oz_test_utils:call_oz(Config, linked_accounts, merge, [
        U1, #linked_account{
            idp = dummyIdP,
            subject_id = <<"123">>,
            access_token = {DummyAccessToken, Now + 3600}
        }
    ]),

    VerifyFun = fun(Token, Ttl) ->
        Token =:= DummyAccessToken andalso Ttl =< 3600
    end,

    ApiTestSpec4 = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/idp_access_token/dummyIdP">>,
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token, <<"ttl">> := Ttl}) ->
                VerifyFun(Token, Ttl)
            end
        },
        logic_spec = LogicSpec#logic_spec{
            args = [auth, U1, dummyIdP],
            expected_result = ?OK_TERM(fun({Token, Ttl}) ->
                VerifyFun(Token, Ttl)
            end)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec4)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec5 = ApiTestSpec4#api_test_spec{
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
        rest_spec = undefined
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec5)).




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
                {user, U2},
                {admin, [?OZ_USERS_LIST_RELATIONSHIPS]}
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
            args = [auth, U2],
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
                        {admin, [?OZ_PROVIDERS_VIEW]},
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
                    args = [auth, U2, ProvId],
                    expected_result = ?OK_MAP_CONTAINS(ProvDetails)
                }
                % @todo gs

            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, EffProviders
    ).


get_spaces_in_eff_provider_test(Config) ->
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_DETAILS(?UNIQUE_STRING)#{<<"subdomainDelegation">> => false}
    ),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1_1} = oz_test_utils:create_space(Config, ?USER(U1), ?UNIQUE_STRING),
    {ok, S1_2} = oz_test_utils:create_space(Config, ?USER(U1), ?UNIQUE_STRING),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U2), ?UNIQUE_STRING),

    oz_test_utils:support_space(Config, ProviderId, S1_1),
    oz_test_utils:support_space(Config, ProviderId, S1_2),
    oz_test_utils:support_space(Config, ProviderId, S2),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AnotherUser = fun(User) -> case User of
        U1 -> U2;
        U2 -> U1
    end end,

    lists:foreach(
        fun({UserId, UserSpaces}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        {user, UserId}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/user/effective_providers/">>, ProviderId, <<"/spaces">>],
                    expected_code = ?HTTP_200_OK,
                    expected_body = #{<<"spaces">> => UserSpaces}
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

            % Check that regular client can't make request
            % on behalf of other client
            ApiTestSpec2 = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, UserId}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, AnotherUser(UserId)},
                        {user, NonAdmin}
                    ]
                },
                rest_spec = undefined,
                logic_spec = #logic_spec{
                    module = user_logic,
                    function = get_spaces_in_eff_provider,
                    args = [auth, UserId, ProviderId],
                    expected_result = ?OK_LIST(UserSpaces)
                }
                % @todo gs

            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, [{U1, [S1_1, S1_2]}, {U2, [S2]}]
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
