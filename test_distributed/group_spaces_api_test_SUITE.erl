%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning group spaces API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_spaces_api_test_SUITE).
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
    get_space_details_test/1,
    create_space_test/1,
    join_space_test/1,
    leave_space_test/1,
    list_eff_spaces_test/1,
    get_eff_space_details_test/1
]).

all() ->
    ?ALL([
        list_spaces_test,
        get_space_details_test,
        create_space_test,
        join_space_test,
        leave_space_test,
        list_eff_spaces_test,
        get_eff_space_details_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_spaces_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:group_create_space(Config, G1, ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:group_create_space(Config, G1, ?SPACE_NAME1),
    {ok, S3} = oz_test_utils:group_create_space(Config, G1, ?SPACE_NAME1),
    {ok, S4} = oz_test_utils:group_create_space(Config, G1, ?SPACE_NAME1),
    ExpSpaces = [S1, S2, S3, S4],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/spaces">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_spaces,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_space_details_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:group_create_space(Config, G1, ?SPACE_NAME1),
    ExpDetails = #{<<"name">> => ?SPACE_NAME1, <<"providers">> => #{}},

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_SPACES_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/spaces/">>, S1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpDetails#{<<"spaceId">> => S1}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_space,
            args = [client, G1, S1],
            expected_result = ?OK_MAP_CONTAINS(ExpDetails)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_space, id = S1, aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_GROUP(G1),
            expected_result = ?OK_MAP(ExpDetails#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, S1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_space_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_ADD_SPACE privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_SPACE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:space_privileges(),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    VerifyFun = fun(SpaceId) ->
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        {ok, Space} = oz_test_utils:get_space(Config, SpaceId),
        ?assertEqual(?SPACE_NAME1, Space#od_space.name),

        ?assertEqual(#{}, Space#od_space.users),
        ?assertEqual(
            #{
                U1 => {AllPrivs, [{od_group, G1}]},
                U2 => {AllPrivs, [{od_group, G1}]}
            },
            Space#od_space.eff_users
        ),
        ?assertEqual(#{G1 => AllPrivs}, Space#od_space.groups),
        ?assertEqual(#{G1 => {AllPrivs, [{od_space, <<"self">>}]}}, Space#od_space.eff_groups),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS]},
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
            path = [<<"/groups/">>, G1, <<"/spaces">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/groups/">>, G1, <<"/spaces/">>]),
                [SpaceId] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(SpaceId)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_space,
            args = [client, G1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_space, aspect = instance},
            auth_hint = ?AS_GROUP(G1),
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"groups">> => #{G1 => AllPrivsBin},
                <<"name">> => ?SPACE_NAME1,
                <<"providers">> => #{},
                <<"shares">> => [],
                <<"users">> => #{},
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
            correct_values = #{<<"name">> => [?SPACE_NAME1]},
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


join_space_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_ADD_SPACE privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_SPACE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, SpaceId} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
        {ok, Macaroon} = oz_test_utils:space_invite_group_token(
            Config, ?ROOT, SpaceId
        ),
        {ok, Token} = onedata_macaroons:serialize(Macaroon),
        #{
            spaceId => SpaceId,
            token => Token,
            macaroonId => macaroon:identifier(Macaroon)
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId, macaroonId := MacaroonId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:group_get_spaces(Config, G1),
        ?assertEqual(lists:member(SpaceId, Spaces), ShouldSucceed),
        case ShouldSucceed of
            true ->
                oz_test_utils:assert_token_not_exists(Config, MacaroonId);
            false -> ok
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
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
            path = [<<"/groups/">>, G1, <<"/spaces/join">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{spaceId := SpaceId} = _Env, _) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config,
                        [<<"/groups/">>, G1, <<"/spaces/">>, SpaceId]
                    ),
                    ?assertMatch(ExpLocation, Location),
                    true
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_space,
            args = [client, G1, data],
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
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, <<"123qwe">>,
                    ?ERROR_BAD_VALUE_TOKEN(<<"token">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),
    
    % Check that token is not consumed upon failed operation
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(U1),
        #{<<"name">> => ?SPACE_NAME1}
    ),
    {ok, Macaroon1} = oz_test_utils:space_invite_group_token(
        Config, ?ROOT, Space
    ),
    {ok, Token} = onedata_macaroons:serialize(Macaroon1),
    oz_test_utils:space_add_group(Config, Space, G1),
    
    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/spaces/join">>],
            expected_code = ?HTTP_400_BAD_REQUEST
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_space,
            args = [client, G1, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_group, G1, od_space, Space))
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [Token]}
        }
    },
    VerifyEndFun1 = fun(_ShouldSucceed,_Env,_) ->
            oz_test_utils:assert_token_exists(Config, macaroon:identifier(Macaroon1))
    end,
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec1, undefined, undefined, VerifyEndFun1
    )).


leave_space_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_LEAVE_SPACE privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_LEAVE_SPACE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:group_create_space(
            Config, G1, ?SPACE_NAME1
        ),
        #{spaceId => S1}
    end,
    DeleteEntityFun = fun(#{spaceId := SpaceId} = _Env) ->
        oz_test_utils:space_remove_group(Config, SpaceId, G1)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:group_get_spaces(Config, G1),
        ?assertEqual(lists:member(SpaceId, Spaces), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/spaces/">>, spaceId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_space,
            args = [client, G1, spaceId],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_eff_spaces_test(Config) ->
    {
        [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}],
        [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_spaces_env(Config),

    ExpSpaces = [S1, S2, S3, S4, S5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/effective_spaces">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_spaces,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_space function
    lists:foreach(
        fun(SpacesId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_space, [G1, SpacesId])
            )
        end, ExpSpaces
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_space,
        [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_space_details_test(Config) ->
    {
        EffSpacesList, [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_spaces_env(Config),

    lists:foreach(
        fun({SpaceId, SpaceDetails}) ->
            ApiTestSpec = #api_test_spec{
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
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/groups/">>, G1, <<"/effective_spaces/">>, SpaceId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = SpaceDetails#{<<"spaceId">> => SpaceId}
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_space,
                    args = [client, G1, SpaceId],
                    expected_result = ?OK_MAP_CONTAINS(SpaceDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_space, id = SpaceId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_GROUP(G1),
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
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffSpacesList
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
