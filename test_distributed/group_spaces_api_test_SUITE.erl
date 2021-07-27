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
-include_lib("ctool/include/errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
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
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO VFS-4520 Tests for GraphSync API
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

    SpaceData = #{<<"name">> => ?SPACE_NAME1},
    {ok, S1} = oz_test_utils:group_create_space(Config, G1, SpaceData),

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
            expected_body = api_test_expect:protected_space(rest, S1, SpaceData, ?SUB(nobody))
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_space,
            args = [auth, G1, S1],
            expected_result = api_test_expect:protected_space(logic, S1, SpaceData, ?SUB(nobody))
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_space, id = S1, aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_GROUP(G1),
            expected_result = api_test_expect:protected_space(gs, S1, SpaceData, ?SUB(nobody))
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

        case maps:is_key(U2, Space#od_space.users) of
            true ->
                % creating user is automatically added as an owner of the space (and
                % hence a direct member too) - given that he is a member of the group
                ?assertMatch(#{U2 := AllPrivs}, Space#od_space.users),
                ?assertMatch([U2], Space#od_space.owners),
                ?assertMatch(
                    #{
                        U1 := {AllPrivs, [{od_group, G1}]},
                        U2 := {AllPrivs, [{od_group, G1}, {od_space, <<"self">>}]}
                    },
                    Space#od_space.eff_users
                );
            false ->
                ?assertEqual(#{}, Space#od_space.users),
                ?assertEqual([], Space#od_space.owners),
                ?assertMatch(
                    #{
                        U1 := {AllPrivs, [{od_group, G1}]},
                        U2 := {AllPrivs, [{od_group, G1}]}
                    },
                    Space#od_space.eff_users
                )
        end,

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
            expected_headers = fun(#{?HDR_LOCATION := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/groups/">>, G1, <<"/spaces/">>]),
                [SpaceId] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(SpaceId)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_space,
            args = [auth, G1, data],
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
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
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
        {ok, Creator} = oz_test_utils:create_user(Config),
        {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(Creator), ?SPACE_NAME1),
        {ok, Token} = oz_test_utils:space_invite_group_token(
            Config, ?USER(Creator), SpaceId
        ),
        {ok, Serialized} = tokens:serialize(Token),
        #{
            spaceId => SpaceId,
            token => Serialized,
            tokenNonce => Token#token.id
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId, tokenNonce := TokenId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:group_get_spaces(Config, G1),
        ?assertEqual(lists:member(SpaceId, Spaces), ShouldSucceed),
        case ShouldSucceed of
            true ->
                oz_test_utils:assert_invite_token_usage_limit_reached(Config, true, TokenId);
            false -> ok
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
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
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
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
            args = [auth, G1, data],
            expected_result = ?OK_ENV(fun(#{spaceId := SpaceId} = _Env, _) ->
                ?OK_BINARY(SpaceId)
            end)
        },
        % TODO VFS-4520 Tests for GraphSync API
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
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(U1),
        #{<<"name">> => ?SPACE_NAME1}
    ),
    {ok, Token2} = oz_test_utils:space_invite_group_token(
        Config, ?USER(U1), Space
    ),
    {ok, Serialized2} = tokens:serialize(Token2),
    oz_test_utils:space_add_group(Config, Space, G1),

    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/spaces/join">>],
            expected_code = ?HTTP_409_CONFLICT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_space,
            args = [auth, G1, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_group, G1, od_space, Space))
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [Serialized2]}
        }
    },
    VerifyEndFun1 = fun(_ShouldSucceed, _Env, _) ->
        oz_test_utils:assert_invite_token_usage_limit_reached(Config, false, Token2#token.id)
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
            args = [auth, G1, spaceId],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
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
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO VFS-4520 Tests for GraphSync API
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

    lists:foreach(fun({SpaceId, SpaceData}) ->
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
                expected_body = api_test_expect:protected_space(rest, SpaceId, SpaceData, ?SUB(nobody))
            },
            logic_spec = #logic_spec{
                module = group_logic,
                function = get_eff_space,
                args = [auth, G1, SpaceId],
                expected_result = api_test_expect:protected_space(logic, SpaceId, SpaceData, ?SUB(nobody))
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_space, id = SpaceId,
                    aspect = instance, scope = protected
                },
                auth_hint = ?THROUGH_GROUP(G1),
                expected_result = api_test_expect:protected_space(gs, SpaceId, SpaceData, ?SUB(nobody))
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffSpacesList).


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
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time().
