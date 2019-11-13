%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user clusters API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(user_clusters_api_test_SUITE).
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
    create_provider_registration_token_for_self_test/1,
    create_provider_registration_token_test/1,
    join_cluster_test/1,
    get_cluster_test/1,
    leave_cluster_test/1,

    list_eff_clusters_test/1,
    get_eff_cluster_test/1
]).

all() ->
    ?ALL([
        list_clusters_test,
        create_provider_registration_token_for_self_test,
        create_provider_registration_token_test,
        join_cluster_test,
        get_cluster_test,
        leave_cluster_test,

        list_eff_clusters_test,
        get_eff_cluster_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_clusters_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ExpClusters = lists:map(
        fun(_) ->
            {ok, {P1, _}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
            ClusterId = P1,
            {ok, U2} = oz_test_utils:cluster_add_user(Config, ClusterId, U2),
            ClusterId
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
            path = <<"/user/clusters">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"clusters">> => ExpClusters}
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
            function = get_clusters,
            args = [auth, U1],
            expected_result = ?OK_LIST(ExpClusters)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


create_provider_registration_token_for_self_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, NonAdmin}
            ],
            unauthorized = [],
            forbidden = []
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/clusters/provider_registration_token">>,
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Make sure that when policy is restricted, only privileged users can create
    % new provider registration tokens.
    oz_test_utils:set_env(Config, provider_registration_policy, restricted),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_PROVIDERS_INVITE], []),

    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1}
            ],
            unauthorized = [],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/clusters/provider_registration_token">>,
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


create_provider_registration_token_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_PROVIDERS_INVITE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/users/">>, U1, <<"/clusters/provider_registration_token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_provider_registration_token,
            args = [auth, U1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


join_cluster_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, Creator} = oz_test_utils:create_user(Config),
        {ok, {P1, _}} = oz_test_utils:create_provider(Config, Creator, ?PROVIDER_NAME1),
        ClusterId = P1,

        {ok, Token} = oz_test_utils:cluster_invite_user_token(
            Config, ?USER(Creator), ClusterId
        ),
        {ok, Serialized} = tokens:serialize(Token),
        #{
            clusterId => ClusterId,
            token => Serialized,
            tokenId => Token#token.id
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{clusterId := ClusterId, tokenId := TokenId} = _Env, _) ->
        {ok, Clusters} = oz_test_utils:user_get_clusters(Config, U1),
        ?assertEqual(lists:member(ClusterId, Clusters), ShouldSucceed),
        case ShouldSucceed of
            true ->
                oz_test_utils:assert_invite_token_usage_limit_reached(Config, true, TokenId);
            false -> ok
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/clusters/join">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{clusterId := ClusterId} = _Env, _) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config, [<<"/user/clusters/">>, ClusterId]),
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
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, <<"123qwe">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)}
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
                {admin, [?OZ_USERS_ADD_RELATIONSHIPS]},
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [{user, U2}]
        },
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = join_cluster,
            args = [auth, U1, data],
            expected_result = ?OK_ENV(fun(#{clusterId := ClusterId} = _Env, _) ->
                ?OK_BINARY(ClusterId)
            end)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check that token is not consumed upon failed operation
    {ok, {P2, _}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
    NewCluster = P2,
    {ok, Token2} = oz_test_utils:cluster_invite_user_token(
        Config, ?USER(U1), NewCluster
    ),
    {ok, Serialized2} = tokens:serialize(Token2),

    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/clusters/join">>,
            expected_code = ?HTTP_409_CONFLICT
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = join_cluster,
            args = [auth, U1, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_user, U1, od_cluster, NewCluster))
        },
        % TODO gs
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


get_cluster_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
    ClusterId = ProviderId,
    {ok, U2} = oz_test_utils:cluster_add_user(Config, ClusterId, U2),

    DefaultVersionInfo = #{
        <<"release">> => ?DEFAULT_RELEASE_VERSION,
        <<"build">> => ?DEFAULT_BUILD_VERSION,
        <<"gui">> => ?EMPTY_GUI_HASH
    },

    ExpDetails = #{
        <<"type">> => ?ONEPROVIDER,
        <<"workerVersion">> => DefaultVersionInfo,
        <<"onepanelVersion">> => DefaultVersionInfo,
        <<"onepanelProxy">> => true
    },

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/clusters/">>, ClusterId],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpDetails#{
                <<"clusterId">> => ClusterId,
                <<"type">> => <<"oneprovider">>
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
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
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_cluster,
            args = [auth, U1, ClusterId],
            expected_result = ?OK_MAP_CONTAINS(ExpDetails)
        }
        % @todo gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


leave_cluster_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, U1, ?PROVIDER_NAME1),
        ClusterId = ProviderId,
        {ok, U2} = oz_test_utils:cluster_add_user(Config, ClusterId, U2),
        #{clusterId => ClusterId}
    end,
    DeleteEntityFun = fun(#{clusterId := ClusterId} = _Env) ->
        oz_test_utils:user_leave_cluster(Config, U1, ClusterId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{clusterId := ClusterId} = _Env, _) ->
        {ok, Clusters} = oz_test_utils:user_get_clusters(Config, U1),
        ?assertEqual(lists:member(ClusterId, Clusters), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, U1}]},
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/clusters/">>, clusterId],
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
                {admin, [?OZ_USERS_REMOVE_RELATIONSHIPS, ?OZ_CLUSTERS_REMOVE_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = leave_cluster,
            args = [auth, U1, clusterId],
            expected_result = ?OK_RES
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])).


list_eff_clusters_test(Config) ->
    {
        [{P1, _}, {P2, _}, {P3, _}, {P4, _}],
        _Spaces, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),
    {ok, {P5, _}} = oz_test_utils:create_provider(Config, U2, ?PROVIDER_NAME1),

    C1 = P1,
    C2 = P2,
    C3 = P3,
    C4 = P4,
    C5 = P5,

    oz_test_utils:cluster_add_user(Config, C1, U2),
    oz_test_utils:cluster_add_user(Config, C2, U2),
    oz_test_utils:cluster_add_user(Config, C3, U2),
    oz_test_utils:cluster_add_user(Config, C4, U2),

    oz_test_utils:cluster_add_user(Config, C1, U1),
    oz_test_utils:cluster_add_user(Config, C2, U1),
    oz_test_utils:cluster_add_user(Config, C3, U1),
    oz_test_utils:cluster_add_user(Config, C4, U1),
    oz_test_utils:cluster_add_user(Config, C5, U1),

    ExpClusters = [C1, C2, C3, C4, C5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/effective_clusters">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"clusters">> => ExpClusters}
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
            function = get_eff_clusters,
            args = [auth, U2],
            expected_result = ?OK_LIST(ExpClusters)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % check also user_logic:has_eff_cluster function
    lists:foreach(
        fun(ClustersId) ->
            ?assert(oz_test_utils:call_oz(
                Config, user_logic, has_eff_cluster, [U2, ClustersId])
            )
        end, ExpClusters
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, user_logic, has_eff_cluster, [U2, <<"asdiucyaie827346w">>])
    ).


get_eff_cluster_test(Config) ->
    {
        EffProvidersList,
        _Spaces, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    EffClustersList = lists:map(fun({ProviderId, _}) ->
        ClusterId = ProviderId,
        oz_test_utils:cluster_add_user(Config, ClusterId, U1),
        oz_test_utils:cluster_add_user(Config, ClusterId, U2),

        {ok, #od_cluster{
            type = Type,
            worker_version = {WRelease, WBuild, WGui},
            onepanel_version = {ORelease, OBuild, OGui},
            onepanel_proxy = OnepanelProxy
        }} = oz_test_utils:get_cluster(Config, ClusterId),
        {ClusterId, #{
            <<"type">> => Type,
            <<"workerVersion">> => #{
                <<"release">> => WRelease,
                <<"build">> => WBuild,
                <<"gui">> => WGui
            },
            <<"onepanelVersion">> => #{
                <<"release">> => ORelease,
                <<"build">> => OBuild,
                <<"gui">> => OGui
            },
            <<"onepanelProxy">> => OnepanelProxy
        }}
    end, EffProvidersList),

    lists:foreach(
        fun({ClusterId, #{<<"type">> := ExpectedType} = ClusterDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        {user, U1},
                        {user, U2}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/user/effective_clusters/">>, ClusterId],
                    expected_code = ?HTTP_200_OK,
                    expected_body = ClusterDetails#{
                        <<"clusterId">> => ClusterId,
                        <<"type">> => atom_to_binary(ExpectedType, utf8)
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
                        {admin, [?OZ_CLUSTERS_VIEW]},
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
                    function = get_eff_cluster,
                    args = [auth, U1, ClusterId],
                    expected_result = ?OK_MAP_CONTAINS(ClusterDetails)
                }
                % @todo gs
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, EffClustersList
    ).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_, Config) ->
    oz_test_utils:set_env(Config, provider_registration_policy, open).


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().
