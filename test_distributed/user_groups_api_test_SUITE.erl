%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user groups API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(user_groups_api_test_SUITE).
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
    list_groups_test/1,
    create_group_test/1,
    join_group_test/1,
    get_group_test/1,
    leave_group_test/1,

    list_eff_groups_test/1,
    get_eff_group_test/1
]).

all() ->
    ?ALL([
        list_groups_test,
        create_group_test,
        join_group_test,
        get_group_test,
        leave_group_test,

        list_eff_groups_test,
        get_eff_group_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_groups_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    ExpGroups = lists:map(
        fun(_) ->
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?USER(U1), ?GROUP_NAME1
            ),
            {ok, U2} = oz_test_utils:group_add_user(Config, GroupId, U2),
            GroupId
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
            path = <<"/user/groups">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

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
        rest_spec = undefined,
        logic_spec = #logic_spec{
            operation = get,
            module = user_logic,
            function = get_groups,
            args = [client, U1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


create_group_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    AllPrivs = oz_test_utils:all_group_privileges(Config),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    VerifyFun = fun(GroupId, ExpName, ExpType) ->
        {ok, Group} = oz_test_utils:get_group(Config, GroupId),
        ?assertEqual(ExpName, Group#od_group.name),
        ?assertEqual(ExpType, Group#od_group.type),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/groups">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, Data) ->
                    ExpName = maps:get(<<"name">>, Data),
                    ExpType = maps:get(<<"type">>, Data, role),
                    BaseURL = ?URL(Config, [<<"/user/groups/">>]),
                    fun(#{<<"Location">> := Location} = _Headers) ->
                        [GroupId] = binary:split(
                            Location, [BaseURL], [global, trim_all]
                        ),
                        VerifyFun(GroupId, ExpName, ExpType)
                    end
                end
            )
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            optional = [<<"type">>],
            correct_values = #{
                <<"name">> => [fun() -> ?UNIQUE_STRING end],
                <<"type">> => ?GROUP_TYPES
            },
            bad_values = [
                {<<"type">>, kingdom,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>, ?GROUP_TYPES)},
                {<<"type">>, 1234, ?ERROR_BAD_VALUE_ATOM(<<"type">>)}
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
            ]
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
            function = create_group,
            args = [client, U1, data],
            expected_result = ?OK_ENV(fun(_, Data) ->
                ExpName = maps:get(<<"name">>, Data),
                ExpType = maps:get(<<"type">>, Data, role),
                ?OK_TERM(fun(GroupId) ->
                    VerifyFun(GroupId, ExpName, ExpType)
                end)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_group, aspect = instance},
            auth_hint = ?AS_USER(U1),
            expected_result = ?OK_ENV(fun(_, Data) ->
                ExpName = maps:get(<<"name">>, Data),
                ExpType = maps:get(<<"type">>, Data, role),
                ?OK_MAP(#{
                    <<"children">> => #{},
                    <<"effectiveChildren">> => #{},
                    <<"users">> => #{U1 => AllPrivsBin},
                    <<"effectiveUsers">> => #{U1 => AllPrivsBin},
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
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


join_group_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, _) ->
            {ok, Groups} = oz_test_utils:user_get_groups(Config, U1),
            ?assertEqual(lists:member(G1, Groups), true),
            oz_test_utils:group_remove_user(Config, G1, U1),
            {ok, NewGroups} = oz_test_utils:user_get_groups(Config, U1),
            ?assertEqual(lists:member(G1, NewGroups), false);
        (false = _ShouldSucceed, _, _) ->
            {ok, Groups} = oz_test_utils:user_get_groups(Config, U1),
            ?assertEqual(lists:member(G1, Groups), false)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/groups/join">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/user/groups/">>, G1]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [fun() ->
                    {ok, Macaroon} = oz_test_utils:group_invite_user_token(
                        Config, ?ROOT, G1
                    ),
                    {ok, Token} = onedata_macaroons:serialize(Macaroon),
                    Token
                end]
            },
            bad_values = [
                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"token">>)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, <<"123qwe">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)}
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
            forbidden = [{user, U2}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = join_group,
            args = [client, U1, data],
            expected_result = ?OK_BINARY(G1)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, undefined, undefined, VerifyEndFun
    )).


get_group_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1),
        #{<<"name">> => ?GROUP_NAME1, <<"type">> => ?GROUP_TYPE1}
    ),
    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/groups/">>, G1],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"groupId">> => G1,
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1_BIN
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

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
            function = get_group,
            args = [client, U1, G1],
            expected_result = ?OK_MAP(#{
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = G1, aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_USER(U1),
            expected_result = ?OK_MAP(#{
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1_BIN,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, G1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


leave_group_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
        {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
        #{groupId => G1}
    end,
    DeleteEntityFun = fun(#{groupId := GroupId} = _Env) ->
        oz_test_utils:group_remove_user(Config, GroupId, U1)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, _) ->
        {ok, ChildrenGroups} = oz_test_utils:user_get_groups(Config, U1),
        ?assertEqual(lists:member(GroupId, ChildrenGroups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/groups/">>, groupId],
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
            function = leave_group,
            args = [client, U1, groupId],
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_eff_groups_test(Config) ->
    {
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_parent_groups_env(Config),

    ExpGroups = [G1, G2, G3, G4, G5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/effective_groups">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

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
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_groups,
            args = [client, U1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % check also user_logic:has_eff_group function
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:call_oz(
                Config, user_logic, has_eff_group, [U2, GroupId])
            )
        end, ExpGroups
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, user_logic, has_eff_group, [U2, <<"asdiucyaie827346w">>])
    ).


get_eff_group_test(Config) ->
    {
        EffGroups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_parent_groups_env(Config),

    lists:foreach(
        fun({GroupId, GroupDetails}) ->
            ExpType = maps:get(<<"type">>, GroupDetails, role),

            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{correct = [{user, U1}]},
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/user/effective_groups/">>, GroupId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = GroupDetails#{
                        <<"groupId">> => GroupId,
                        <<"type">> => atom_to_binary(ExpType, utf8)
                    }
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

            % Check that regular client can't make request
            % on behalf of other client
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
                    function = get_eff_group,
                    args = [client, U1, GroupId],
                    expected_result = ?OK_MAP(GroupDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_group, id = GroupId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_USER(U1),
                    expected_result = ?OK_MAP(GroupDetails#{
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
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, EffGroups
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
