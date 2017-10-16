%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C): 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning groups spaces and provider
%%% API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_spaces_api_test_SUITE).
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
-include_lib("cluster_worker/include/api_errors.hrl").

-include("api_test_utils.hrl").


%% API
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
    get_eff_space_details_test/1,

    list_eff_providers_test/1,
    get_eff_provider_details_test/1
]).

all() ->
    ?ALL([
        list_spaces_test,
        get_space_details_test,
        create_space_test,
        join_space_test,
        leave_space_test,
        list_eff_spaces_test,
        get_eff_space_details_test,

        list_eff_providers_test,
        get_eff_provider_details_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_spaces_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
        ?GROUP_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),

    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, <<"S1">>),
    {ok, S2} = oz_test_utils:create_space_for_group(Config, G1, <<"S2">>),
    {ok, S3} = oz_test_utils:create_space_for_group(Config, G1, <<"S3">>),
    {ok, S4} = oz_test_utils:create_space_for_group(Config, G1, <<"S4">>),
    ExpSpaces = [S1, S2, S3, S4],

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
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
        ?GROUP_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),

    ExpName = <<"S1">>,
    ExpMap = #{<<"name">> => ExpName, <<"providersSupports">> => #{}},
    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, ExpName),

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
            path = [<<"/groups/">>, G1, <<"/spaces/">>, S1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpMap#{<<"spaceId">> => S1}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_space,
            args = [client, G1, S1],
            expected_result = ?OK_MAP(ExpMap)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_space, id = S1, aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_GROUP(G1),
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_space_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
        ?GROUP_CREATE_SPACE
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_CREATE_SPACE
    ]),

    ExpName = <<"SpaceName">>,
    AllPrivs = oz_test_utils:call_oz(Config, privileges, space_privileges, []),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    VerifyFun = fun(SpaceId) ->
        {ok, Space} = oz_test_utils:get_space(Config, SpaceId),
        ?assertEqual(ExpName, Space#od_space.name),
        true
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
            method = post,
            path = [<<"/groups/">>, G1, <<"/spaces">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                [GroupId, SpaceId] = binary:split(
                    Location,
                    [<<"/groups/">>, <<"/spaces/">>],
                    [global, trim_all]
                ),
                ?assertEqual(GroupId, G1),
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
            expected_result = ?OK_MAP(#{
                <<"effectiveGroups">> => #{G1 => AllPrivsBin},
                <<"effectiveUsers">> => #{
                    U1 => AllPrivsBin,
                    U2 => AllPrivsBin
                },
                <<"groups">> => #{G1 => AllPrivsBin},
                <<"name">> => ExpName,
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
            correct_values = #{
                <<"name">> => [ExpName]
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


join_space_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
        ?GROUP_JOIN_SPACE
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_JOIN_SPACE
    ]),

    EnvSetUpFun = fun() ->
        {ok, SpaceId} = oz_test_utils:create_space(Config, ?ROOT, <<"Sp">>),
        #{spaceId => SpaceId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:get_group_spaces(Config, G1),
        ?assertEqual(lists:member(SpaceId, Spaces), ShouldSucceed)
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
            method = post,
            path = [<<"/groups/">>, G1, <<"/spaces/join">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{spaceId := SpaceId} = _Env, _) ->
                fun(#{<<"location">> := Location} = _Headers) ->
                    [GroupId, SpaceId] = binary:split(
                        Location,
                        [<<"/groups/">>, <<"/spaces/">>],
                        [global, trim_all]
                    ),
                    ?assertEqual(GroupId, G1),
                    ?assertEqual(SpaceId, SpaceId),
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
                <<"token">> => [fun(#{spaceId := SpaceId} = _Env) ->
                    {ok, Macaroon} = oz_test_utils:space_invite_group_token(
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
    )).


leave_space_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
        ?GROUP_LEAVE_SPACE
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_LEAVE_SPACE
    ]),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, <<"S1">>),
        #{spaceId => S1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:get_group_spaces(Config, G1),
        ?assertEqual(lists:member(SpaceId, Spaces), not ShouldSucceed)
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
            method = delete,
            path = [<<"/groups/">>, G1, <<"/spaces/">>, spaceId],
            expected_code = ?HTTP_202_ACCEPTED
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
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun]
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
                    expected_result = ?OK_MAP(SpaceDetails)
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


list_eff_providers_test(Config) ->
    {
        [{P1, _}, {P2, _}, {P3, _}, {P4, _}],
        _Spaces, [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    ExpProviders = [P1, P2, P3, P4],
    ApiTestSpec = #api_test_spec{
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
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/effective_providers">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"providers">> => ExpProviders}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_providers,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpProviders)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_provider function
    lists:foreach(
        fun(ProviderId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_provider, [G1, ProviderId])
            )
        end, ExpProviders
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_provider, [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_provider_details_test(Config) ->
    {
        EffProvidersList, _Spaces, [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    lists:foreach(
        fun({ProvId, ProvDetails}) ->
            NewProvDetails = ProvDetails#{
                <<"clientName">> => maps:get(<<"name">>, ProvDetails)
            },
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, U2},
                        {user, U1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/groups/">>, G1, <<"/effective_providers/">>, ProvId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = NewProvDetails#{<<"providerId">> => ProvId}
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_provider,
                    args = [client, G1, ProvId],
                    expected_result = ?OK_MAP(NewProvDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_provider, id = ProvId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_GROUP(G1),
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
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffProvidersList
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
