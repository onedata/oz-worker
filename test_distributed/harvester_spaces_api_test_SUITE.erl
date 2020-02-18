%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning harvester spaces API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_spaces_api_test_SUITE).
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


-export([
    all/0,
    init_per_testcase/2, end_per_testcase/2,
    init_per_suite/1, end_per_suite/1
]).
-export([
    join_space_test/1,
    add_space_test/1,
    create_space_invite_token_test/1,
    remove_space_test/1,
    list_spaces_test/1,
    get_space_test/1,

    list_eff_providers_test/1,
    get_eff_provider_test/1
]).

all() ->
    ?ALL([
        join_space_test,
        add_space_test,
        create_space_invite_token_test,
        remove_space_test,
        list_spaces_test,
        get_space_test,

        list_eff_providers_test,
        get_eff_provider_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================

join_space_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_ADD_SPACE privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_ADD_SPACE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, Creator} = oz_test_utils:create_user(Config),
        {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(Creator)),
        {ok, Token} = oz_test_utils:call_oz(Config, token_logic, create_user_named_token, [
            ?USER(Creator), Creator, #{
                <<"name">> => ?UNIQUE_STRING,
                <<"type">> => ?INVITE_TOKEN(?HARVESTER_JOIN_SPACE, SpaceId),
                <<"usageLimit">> => 1
            }
        ]),
        {ok, Serialized} = tokens:serialize(Token),
        #{
            spaceId => SpaceId,
            token => Serialized,
            tokenNonce => Token#token.id
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId, tokenNonce := TokenId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:harvester_get_spaces(Config, H1),
        case ShouldSucceed of
            true ->
                ?assertEqual(lists:member(SpaceId, Spaces), ShouldSucceed),
                oz_test_utils:assert_invite_token_usage_limit_reached(Config, true, TokenId);
            false -> ok
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_HARVESTERS_ADD_RELATIONSHIPS]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/harvesters/">>, H1, <<"/spaces/join">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{spaceId := SpaceId} = _Env, _) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config,
                        [<<"/harvesters/">>, H1, <<"/spaces/">>, SpaceId]
                    ),
                    ?assertMatch(ExpLocation, Location),
                    true
                end
            end)
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = join_space,
            args = [auth, H1, data],
            expected_result = ?OK_ENV(fun(#{spaceId := SpaceId} = _Env, _) ->
                ?OK_BINARY(SpaceId)
            end)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [fun(#{token := Token} = _Env) ->
                    Token
                end]
            },
            bad_values = [
                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"token">>)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, <<"123qwe">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check that token is not consumed upon failed operation
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(U1)),
    {ok, Token2} = oz_test_utils:call_oz(Config, token_logic, create_user_named_token, [
        ?USER(U1), U1, #{
            <<"name">> => ?UNIQUE_STRING,
            <<"type">> => ?INVITE_TOKEN(?HARVESTER_JOIN_SPACE, Space),
            <<"usageLimit">> => 1
        }
    ]),
    {ok, Serialized2} = tokens:serialize(Token2),
    oz_test_utils:harvester_add_space(Config, H1, Space),

    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_HARVESTERS_ADD_RELATIONSHIPS]},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/harvesters/">>, H1, <<"/spaces/join">>],
            expected_code = ?HTTP_409_CONFLICT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = join_space,
            args = [auth, H1, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_harvester, H1, od_space, Space))
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [Serialized2]}
        }
    },
    VerifyEndFun1 = fun(_ShouldSucceed,_Env,_) ->
        oz_test_utils:assert_invite_token_usage_limit_reached(Config, false, Token2#token.id)
    end,
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec1, undefined, undefined, VerifyEndFun1
    )).


add_space_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, UserNoAddSpacePriv} = oz_test_utils:create_user(Config),
    {ok, UserNoAddHarvesterPriv} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    oz_test_utils:user_set_oz_privileges(Config, User, [?OZ_HARVESTERS_CREATE], []),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(User), ?HARVESTER_CREATE_DATA),

    oz_test_utils:harvester_add_user(Config, H1, UserNoAddSpacePriv),
    oz_test_utils:space_add_user(Config, S1, UserNoAddSpacePriv),
    oz_test_utils:harvester_set_user_privileges(Config, H1, UserNoAddSpacePriv,
        privileges:harvester_privileges() -- [?HARVESTER_ADD_SPACE], [?HARVESTER_ADD_SPACE]
    ),
    oz_test_utils:space_set_user_privileges(Config, S1, UserNoAddSpacePriv,
        privileges:space_privileges(), []
    ),

    oz_test_utils:harvester_add_user(Config, H1, UserNoAddHarvesterPriv),
    oz_test_utils:space_add_user(Config, S1, UserNoAddHarvesterPriv),
    oz_test_utils:space_set_user_privileges(Config, S1, UserNoAddHarvesterPriv,
        privileges:space_privileges() -- [?SPACE_ADD_HARVESTER], [?SPACE_ADD_HARVESTER]
    ),
    oz_test_utils:harvester_set_user_privileges(Config, H1, UserNoAddHarvesterPriv,
        privileges:harvester_privileges(), []
    ),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, _) ->
            {ok, Spaces} = oz_test_utils:harvester_get_spaces(Config, H1),
            ?assert(lists:member(S1, Spaces)),
            oz_test_utils:harvester_remove_space(Config, H1, S1);
        (false = _ShouldSucceed, _, _) ->
            {ok, Spaces} = oz_test_utils:harvester_get_spaces(Config, H1),
            ?assertNot(lists:member(S1, Spaces))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, User},
                root,
                {admin, [?OZ_HARVESTERS_ADD_RELATIONSHIPS, ?OZ_SPACES_ADD_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserNoAddSpacePriv},
                {user, UserNoAddHarvesterPriv}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/harvesters/">>, H1, <<"/spaces/">>, S1],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/harvesters/">>, H1, <<"/spaces/">>, S1]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = add_space,
            args = [auth, H1, S1],
            expected_result = ?OK_BINARY(S1)
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

create_space_invite_token_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_ADD_SPACE privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_ADD_SPACE
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
            path = [<<"/harvesters/">>, H1, <<"/spaces/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = create_space_invite_token,
            args = [auth, H1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).



remove_space_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_REMOVE_SPACE privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_REMOVE_SPACE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
        {ok, S1} = oz_test_utils:harvester_add_space(Config, H1, S1),
        #{spaceId => S1}
    end,
    DeleteEntityFun = fun(#{spaceId := SpaceId} = _Env) ->
        oz_test_utils:harvester_remove_space(Config, H1, SpaceId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:harvester_get_spaces(Config, H1),
        ?assertEqual(lists:member(SpaceId, Spaces), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_REMOVE_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS]},
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
            path = [<<"/harvesters/">>, H1, <<"/spaces/">>, spaceId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = remove_space,
            args = [auth, H1, spaceId],
            expected_result = ?OK_RES
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_spaces_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_VIEW privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ExpSpaces = lists:map(
        fun(_) ->
            {ok, SpaceId} = oz_test_utils:create_space(
                Config, ?ROOT, ?SPACE_NAME1
            ),
            oz_test_utils:harvester_add_space(Config, H1, SpaceId),
            SpaceId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_LIST_RELATIONSHIPS]},
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
            path = [<<"/harvesters/">>, H1, <<"/spaces">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_spaces,
            args = [auth, H1],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_space_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_VIEW privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(
        Config, ?ROOT, ?SPACE_NAME1
    ),
    oz_test_utils:harvester_add_space(Config, H1, S1),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW]},
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
            path = [<<"/harvesters/">>, H1, <<"/spaces/">>, S1],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"spaceId">> => S1,
                <<"name">> => ?SPACE_NAME1,
                <<"providers">> => #{}
            }
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_space,
            args = [auth, H1, S1],
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?SPACE_NAME1,
                <<"providers">> => #{}
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_space, id = S1, aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_HARVESTER(H1),
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?SPACE_NAME1,
                <<"providers">> => #{},
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(Id, S1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_eff_providers_test(Config) ->
    {H1, [{P1, _}, {P2, P2Details}, {P3, _}], _Spaces, {U1, NonAdmin}} =
        api_test_scenarios:create_harvester_eff_providers_env(Config),

    ExpProviders = [P1, P2, P3],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_LIST_RELATIONSHIPS]},
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P2, maps:get(<<"providerRootToken">>, P2Details)}
            ]
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_eff_providers,
            args = [auth, H1],
            expected_result = ?OK_LIST(ExpProviders)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also harvester_logic:has_eff_provider function
    lists:foreach(
        fun(ProviderId) ->
            ?assert(oz_test_utils:call_oz(
                Config, harvester_logic, has_eff_provider, [H1, ProviderId])
            )
        end, ExpProviders
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, harvester_logic, has_eff_provider, [H1, <<"asdiucyaie827346w">>])
    ).


get_eff_provider_test(Config) ->
    {H1, Providers, _Spaces, {U1, NonAdmin}} =
        api_test_scenarios:create_harvester_eff_providers_env(Config),

    lists:foreach(fun({ProviderId, ProviderDetails}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_PROVIDERS_VIEW]},
                    {user, U1}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, NonAdmin}
                ]
            },
            logic_spec = #logic_spec{
                module = harvester_logic,
                function = get_eff_provider,
                args = [auth, H1, ProviderId],
                expected_result = ?OK_MAP(#{
                    <<"name">> => maps:get(<<"name">>, ProviderDetails)
                })
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, Providers).

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

