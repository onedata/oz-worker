%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user spaces API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(user_spaces_api_test_SUITE).
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
    list_spaces_test/1,
    create_space_test/1,
    join_space_test/1,
    get_space_test/1,
    leave_space_test/1,

    set_space_alias_test/1,
    get_space_alias_test/1,
    delete_space_alias_test/1,

    list_eff_spaces_test/1,
    get_eff_space_test/1
]).

all() ->
    ?ALL([
        list_spaces_test,
        create_space_test,
        join_space_test,
        get_space_test,
        leave_space_test,

        set_space_alias_test,
        get_space_alias_test,
        delete_space_alias_test,

        list_eff_spaces_test,
        get_eff_space_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_spaces_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ExpSpaces = lists:map(
        fun(_) ->
            {ok, SpaceId} = oz_test_utils:create_space(
                Config, ?USER(U1), ?SPACE_NAME1
            ),
            {ok, U2} = oz_test_utils:space_add_user(Config, SpaceId, U2),
            SpaceId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/spaces">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_USERS_LIST_RELATIONSHIPS]}
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
            function = get_spaces,
            args = [auth, U1],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


create_space_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),

    ExpName = ?SPACE_NAME1,
    AllPrivs = privileges:space_privileges(),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    VerifyFun = fun(SpaceId) ->
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        {ok, Space} = oz_test_utils:get_space(Config, SpaceId),
        ?assertEqual(ExpName, Space#od_space.name),

        [User] = ?assertMatch([_], maps:keys(Space#od_space.users)),
        ?assertEqual(#{User => AllPrivs}, Space#od_space.users),
        ?assertEqual(#{User => {AllPrivs, [{od_space, <<"self">>}]}}, Space#od_space.eff_users),
        ?assertEqual(#{}, Space#od_space.groups),
        ?assertEqual(#{}, Space#od_space.eff_groups),

        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2},
                {admin, [?OZ_USERS_ADD_RELATIONSHIPS, ?OZ_SPACES_ADD_RELATIONSHIPS]}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/spaces">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/user/spaces/">>]),
                [SpaceId] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(SpaceId)
            end
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            correct_values = #{
                <<"name">> => [ExpName]
            },
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [{user, U2}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_space,
            args = [auth, U1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_space, aspect = instance},
            auth_hint = ?AS_USER(U1),
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"groups">> => #{},
                <<"name">> => ExpName,
                <<"providers">> => #{},
                <<"shares">> => [],
                <<"users">> => #{U1 => AllPrivsBin},
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    VerifyFun(Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


join_space_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, SpaceId} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
        {ok, Macaroon} = oz_test_utils:space_invite_user_token(
            Config, ?ROOT, SpaceId
        ),
        {ok, Token} = macaroons:serialize(Macaroon),
        #{
            spaceId => SpaceId, 
            token => Token,
            macaroonId => macaroon:identifier(Macaroon)
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId, macaroonId := MacaroonId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:user_get_spaces(Config, U1),
        ?assertEqual(lists:member(SpaceId, Spaces), ShouldSucceed),
        case ShouldSucceed of
            true ->
                oz_test_utils:assert_token_not_exists(Config, MacaroonId);
            false -> ok
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/spaces/join">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{spaceId := SpaceId} = _Env, _) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config, [<<"/user/spaces/">>, SpaceId]),
                    ?assertEqual(ExpLocation, Location),
                    true
                end
            end)
        },
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [fun(#{token := Token} = _Env) ->
                    Token
                end]
            },
            bad_values = [
                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"token">>)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, <<"123qwe">>,
                    ?ERROR_BAD_VALUE_TOKEN(<<"token">>)}
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
                {admin, [?OZ_USERS_ADD_RELATIONSHIPS]},
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [{user, U2}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = join_space,
            args = [auth, U1, data],
            expected_result = ?OK_ENV(fun(#{spaceId := SpaceId} = _Env, _) ->
                ?OK_BINARY(SpaceId)
            end)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )),
    
    % Check that token is not consumed upon failed operation
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(U1),
        #{<<"name">> => ?SPACE_NAME1}
    ),
    {ok, Macaroon} = oz_test_utils:space_invite_user_token(
        Config, ?ROOT, Space
    ),
    {ok, Token} = macaroons:serialize(Macaroon),
    
    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/spaces/join">>,
            expected_code = ?HTTP_400_BAD_REQUEST
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = join_space,
            args = [auth, U1, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_user, U1, od_space, Space))
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [Token]}
        }
    },
    VerifyEndFun1 = fun(_ShouldSucceed,_Env,_) ->
            oz_test_utils:assert_token_exists(Config, macaroon:identifier(Macaroon))
    end,
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec1, undefined, undefined, VerifyEndFun1
    )).


get_space_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),
    ExpDetails = #{<<"name">> => ?SPACE_NAME1, <<"providers">> => #{}},

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/spaces/">>, S1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpDetails#{<<"spaceId">> => S1}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW]},
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
            function = get_space,
            args = [auth, U1, S1],
            expected_result = ?OK_MAP_CONTAINS(ExpDetails)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_space, id = S1, aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_USER(U1),
            expected_result = ?OK_MAP_CONTAINS(ExpDetails#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, S1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


leave_space_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
        {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),
        #{spaceId => S1}
    end,
    DeleteEntityFun = fun(#{spaceId := SpaceId} = _Env) ->
        oz_test_utils:user_leave_space(Config, U1, SpaceId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:user_get_spaces(Config, U1),
        ?assertEqual(lists:member(SpaceId, Spaces), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/spaces/">>, spaceId],
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_USERS_REMOVE_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = leave_space,
            args = [auth, U1, spaceId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])).


set_space_alias_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),

    VerifyEndFun = fun(ShouldSucceed, _, #{<<"alias">> := ExpAlias} = _Data) ->
        Result = oz_test_utils:call_oz(
            Config, user_logic, get_space_alias, [?ROOT, U1, S1]
        ),
        case ShouldSucceed of
            true -> ?assertMatch({ok, ExpAlias}, Result);
            false -> ?assertNotMatch({ok, ExpAlias}, Result)
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/user/spaces/">>, S1, <<"/alias">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        data_spec = #data_spec{
            required = [<<"alias">>],
            correct_values = #{
                <<"alias">> => [fun() -> ?UNIQUE_STRING end]
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
            function = set_space_alias,
            args = [auth, U1, S1, data],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, undefined, undefined, VerifyEndFun
    )).


get_space_alias_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),

    % Newly created space should not have an alias
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = [<<"/user/spaces/">>, S1, <<"/alias">>],
            expected_code = ?HTTP_404_NOT_FOUND
        },
        logic_spec = LogicSpec = #logic_spec{
            module = user_logic,
            function = get_space_alias,
            args = [auth, U1, S1],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Set an alias for given space
    ExpAlias = <<"Alias">>,
    oz_test_utils:user_set_space_alias(Config, U1, S1, ExpAlias),

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
            function = get_space_alias,
            args = [auth, U1, S1],
            expected_result = ?OK_BINARY(ExpAlias)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)),

    % Unset the space alias and check if it's not present again
    ok = oz_test_utils:user_unset_space_alias(Config, U1, S1),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


delete_space_alias_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),

    EnvSetUpFun = fun() ->
        Alias = ?UNIQUE_STRING,
        oz_test_utils:user_set_space_alias(Config, U1, S1, Alias),
        #{alias => Alias}
    end,
    DeleteEntityFun = fun(_Env) ->
        oz_test_utils:user_unset_space_alias(Config, U1, S1)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{alias := ExpAlias} = _Env, _) ->
        Result = oz_test_utils:call_oz(
            Config, user_logic, get_space_alias, [?ROOT, U1, S1]
        ),
        case ShouldSucceed of
            true -> ?assertMatch(?ERROR_NOT_FOUND, Result);
            false -> ?assertMatch({ok, ExpAlias}, Result)
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/spaces/">>, S1, <<"/alias">>],
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
            function = delete_space_alias,
            args = [auth, U1, S1],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])).


list_eff_spaces_test(Config) ->
    {
        [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}],
        _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_spaces_env(Config),

    {ok, S6} = oz_test_utils:create_space(Config, ?USER(U2), ?SPACE_NAME1),
    {ok, U1} = oz_test_utils:space_add_user(Config, S6, U1),

    ExpSpaces = [S1, S2, S3, S4, S5, S6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/effective_spaces">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
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
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_spaces,
            args = [auth, U2],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % check also user_logic:has_eff_space function
    lists:foreach(
        fun(SpacesId) ->
            ?assert(oz_test_utils:call_oz(
                Config, user_logic, has_eff_space, [U2, SpacesId])
            )
        end, ExpSpaces
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, user_logic, has_eff_space, [U2, <<"asdiucyaie827346w">>])
    ).


get_eff_space_test(Config) ->
    {
        EffSpacesList, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_spaces_env(Config),

    {ok, S6} = oz_test_utils:create_space(Config, ?USER(U2), ?SPACE_NAME1),
    S6Details = #{<<"name">> => ?SPACE_NAME1, <<"providers">> => #{}},
    {ok, U1} = oz_test_utils:space_add_user(Config, S6, U1),

    NewEffSpacesList = [{S6, S6Details} | EffSpacesList],
    lists:foreach(
        fun({SpaceId, SpaceDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        {user, U1},
                        {user, U2}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/user/effective_spaces/">>, SpaceId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = SpaceDetails#{<<"spaceId">> => SpaceId}
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

            % Check that regular client can't make request
            % on behalf of other client
            ApiTestSpec2 = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {admin, [?OZ_SPACES_VIEW]},
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
                    function = get_eff_space,
                    args = [auth, U1, SpaceId],
                    expected_result = ?OK_MAP_CONTAINS(SpaceDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_space, id = SpaceId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_USER(U1),
                    expected_result = ?OK_MAP_CONTAINS(SpaceDetails#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Id, SpaceId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, NewEffSpacesList
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
