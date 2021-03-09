%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning group clusters API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_clusters_api_test_SUITE).
-author("Lukasz Opiola").

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
    list_clusters_test/1,
    join_cluster_test/1,
    get_cluster_test/1,
    leave_cluster_test/1,

    list_eff_clusters_test/1,
    get_eff_cluster_details_test/1
]).

all() ->
    ?ALL([
        list_clusters_test,
        join_cluster_test,
        get_cluster_test,
        leave_cluster_test,

        list_eff_clusters_test,
        get_eff_cluster_details_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_clusters_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ExpClusters = lists:map(
        fun(_) ->
            {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, undefined, ?PROVIDER_NAME1),
            ClusterId = ProviderId,
            {ok, G1} = oz_test_utils:cluster_add_group(Config, ClusterId, G1),
            ClusterId
        end, lists:seq(1, 5)
    ),

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
            path = [<<"/groups/">>, G1, <<"/clusters">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"clusters">> => ExpClusters}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_clusters,
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpClusters)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


join_cluster_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_ADD_CLUSTER privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_CLUSTER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, Creator} = oz_test_utils:create_user(Config),
        {ok, {P1, _Token1}} = oz_test_utils:create_provider(Config, Creator, ?PROVIDER_NAME1),
        ClusterId = P1,
        {ok, Token} = oz_test_utils:cluster_invite_group_token(
            Config, ?USER(Creator), ClusterId
        ),
        {ok, Serialized} = tokens:serialize(Token),
        #{
            clusterId => ClusterId,
            token => Serialized,
            tokenNonce => Token#token.id
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{clusterId := ClusterId, tokenNonce := TokenId} = _Env, _) ->
        {ok, Clusters} = oz_test_utils:group_get_clusters(Config, G1),
        ?assertEqual(lists:member(ClusterId, Clusters), ShouldSucceed),
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
            path = [<<"/groups/">>, G1, <<"/clusters/join">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{clusterId := ClusterId} = _Env, _) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config,
                        [<<"/groups/">>, G1, <<"/clusters/">>, ClusterId]
                    ),
                    ?assertMatch(ExpLocation, Location),
                    true
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_cluster,
            args = [auth, G1, data],
            expected_result = ?OK_ENV(fun(#{clusterId := ClusterId} = _Env, _) ->
                ?OK_BINARY(ClusterId)
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
    {ok, {P2, _}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
    NewCluster = P2,
    oz_test_utils:cluster_add_group(Config, NewCluster, G1),

    {ok, Token} = oz_test_utils:cluster_invite_group_token(
        Config, ?USER(U1), NewCluster
    ),
    {ok, Serialized} = tokens:serialize(Token),

    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/clusters/join">>],
            expected_code = ?HTTP_409_CONFLICT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_cluster,
            args = [auth, G1, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_group, G1, od_cluster, NewCluster))
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [Serialized]}
        }
    },
    VerifyEndFun1 = fun(_ShouldSucceed, _Env, _) ->
        oz_test_utils:assert_invite_token_usage_limit_reached(Config, false, Token#token.id)
    end,
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec1, undefined, undefined, VerifyEndFun1
    )).


get_cluster_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?PROVIDER_NAME1),
    ClusterId = ProviderId,
    {ok, G1} = oz_test_utils:cluster_add_group(Config, ClusterId, G1),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_CLUSTERS_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/clusters/">>, ClusterId],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:protected_cluster(rest, ClusterId, ?ONEPROVIDER, ?SUB(user, ProviderAdmin))
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_cluster,
            args = [auth, G1, ClusterId],
            expected_result = api_test_expect:protected_cluster(logic, ClusterId, ?ONEPROVIDER, ?SUB(user, ProviderAdmin))
        }
        % @TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


leave_cluster_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_LEAVE_CLUSTER privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_LEAVE_CLUSTER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
        ClusterId = ProviderId,
        {ok, G1} = oz_test_utils:cluster_add_group(Config, ClusterId, G1),
        #{clusterId => ClusterId}
    end,
    DeleteEntityFun = fun(#{clusterId := ClusterId} = _Env) ->
        oz_test_utils:cluster_remove_group(Config, ClusterId, G1)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{clusterId := ClusterId} = _Env, _) ->
        {ok, Clusters} = oz_test_utils:group_get_clusters(Config, G1),
        ?assertEqual(lists:member(ClusterId, Clusters), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_CLUSTERS_REMOVE_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/clusters/">>, clusterId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_cluster,
            args = [auth, G1, clusterId],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_eff_clusters_test(Config) ->
    {
        [{P1, _}, {P2, _}, {P3, _}, {P4, _}],
        _Spaces,
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _} | _] = _Groups,
        {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),
    {ok, {P5, _Token5}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),

    C1 = P1,
    C2 = P2,
    C3 = P3,
    C4 = P4,
    C5 = P5,

    % G1 is a child of all groups, so it inherits effective memberships from them
    oz_test_utils:cluster_add_group(Config, C1, G1),
    oz_test_utils:cluster_add_group(Config, C2, G2),
    oz_test_utils:cluster_add_group(Config, C3, G3),
    oz_test_utils:cluster_add_group(Config, C4, G4),
    oz_test_utils:cluster_add_group(Config, C5, G5),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ExpClusters = [C1, C2, C3, C4, C5],
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
            path = [<<"/groups/">>, G1, <<"/effective_clusters">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"clusters">> => ExpClusters}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_clusters,
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpClusters)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_cluster function
    lists:foreach(
        fun(ClustersId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_cluster, [G1, ClustersId])
            )
        end, ExpClusters
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_cluster,
        [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_cluster_details_test(Config) ->
    {
        EffProvidersList, _Spaces,
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _} | _] = _Groups,
        {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    EffProviderClusters = lists:map(fun({ProviderId, _}) ->
        ClusterId = ProviderId,
        % G1 is a child of all groups, so it inherits effective memberships from them
        oz_test_utils:cluster_add_group(Config, ClusterId, lists_utils:random_element([G1, G2, G3, G4, G5])),
        {ClusterId, ?ONEPROVIDER}
    end, EffProvidersList),

    oz_test_utils:cluster_add_group(Config, ?ONEZONE_CLUSTER_ID, lists_utils:random_element([G1, G2, G3, G4, G5])),
    EffClustersList = [{?ONEZONE_CLUSTER_ID, ?ONEZONE} | EffProviderClusters],

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    lists:foreach(fun({ClusterId, Type}) ->
        Creator = case Type of
            ?ONEZONE -> ?SUB(nobody);
            _ -> ?SUB(user, U1)
        end,
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_CLUSTERS_VIEW]},
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
                path = [<<"/groups/">>, G1, <<"/effective_clusters/">>, ClusterId],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:protected_cluster(rest, ClusterId, Type, Creator)
            },
            logic_spec = #logic_spec{
                module = group_logic,
                function = get_eff_cluster,
                args = [auth, G1, ClusterId],
                expected_result = api_test_expect:protected_cluster(logic, ClusterId, Type, Creator)
            }
            % @TODO VFS-4520 Tests for GraphSync API
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffClustersList).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    ozt:init_per_suite(Config).

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time().
