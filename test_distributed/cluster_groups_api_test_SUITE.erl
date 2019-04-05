%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning cluster groups API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_groups_api_test_SUITE).
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
-include_lib("ctool/include/api_errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    add_group_test/1,
    add_group_with_privileges_test/1,
    create_group_test/1,
    create_group_invite_token_test/1,
    remove_group_test/1,
    list_groups_test/1,
    get_group_test/1,

    get_group_privileges_test/1,
    update_group_privileges_test/1,

    list_eff_groups_test/1,
    get_eff_group_test/1,
    get_eff_group_privileges_test/1,
    get_eff_group_membership_intermediaries/1
]).

all() ->
    ?ALL([
        add_group_test,
        add_group_with_privileges_test,
        create_group_test,
        create_group_invite_token_test,
        remove_group_test,
        list_groups_test,
        get_group_test,

        get_group_privileges_test,
        update_group_privileges_test,

        list_eff_groups_test,
        get_eff_group_test,
        get_eff_group_privileges_test,
        get_eff_group_membership_intermediaries
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


add_group_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, UserNoAddGroupPriv} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, UserNoAddClusterPriv} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User), ?GROUP_NAME1),

    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, User, ?PROVIDER_NAME1),
    ClusterId = ProviderId,

    oz_test_utils:cluster_add_user(Config, ClusterId, UserNoAddGroupPriv),
    oz_test_utils:group_add_user(Config, G1, UserNoAddGroupPriv),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, UserNoAddGroupPriv,
        privileges:cluster_privileges() -- [?CLUSTER_ADD_GROUP], [?CLUSTER_ADD_GROUP]
    ),
    oz_test_utils:group_set_user_privileges(Config, G1, UserNoAddGroupPriv,
        privileges:group_privileges(), []
    ),

    oz_test_utils:cluster_add_user(Config, ClusterId, UserNoAddClusterPriv),
    oz_test_utils:group_add_user(Config, G1, UserNoAddClusterPriv),
    oz_test_utils:group_set_user_privileges(Config, G1, UserNoAddClusterPriv,
        privileges:group_privileges() -- [?GROUP_ADD_CLUSTER], [?GROUP_ADD_CLUSTER]
    ),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, UserNoAddClusterPriv,
        privileges:cluster_privileges(), []
    ),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, _) ->
            {ok, Groups} = oz_test_utils:cluster_get_groups(Config, ClusterId),
            ?assert(lists:member(G1, Groups)),
            oz_test_utils:cluster_remove_group(Config, ClusterId, G1);
        (false = _ShouldSucceed, _, _) ->
            {ok, Groups} = oz_test_utils:cluster_get_groups(Config, ClusterId),
            ?assertNot(lists:member(G1, Groups))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, User},
                root,
                {admin, [?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_GROUPS_ADD_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserNoAddGroupPriv},
                {user, UserNoAddClusterPriv}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/clusters/">>, ClusterId, <<"/groups/">>, G1],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/clusters/">>, ClusterId, <<"/groups/">>, G1]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = add_group,
            args = [client, ClusterId, G1, data],
            expected_result = ?OK_BINARY(G1)
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


add_group_with_privileges_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, UserNoSetPrivsPriv} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(User), ?GROUP_NAME1),

    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, User, ?PROVIDER_NAME1),
    ClusterId = ProviderId,

    oz_test_utils:cluster_add_user(Config, ClusterId, UserNoSetPrivsPriv),
    oz_test_utils:group_add_user(Config, G1, UserNoSetPrivsPriv),
    oz_test_utils:group_set_user_privileges(Config, G1, UserNoSetPrivsPriv,
        privileges:group_privileges(), []
    ),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, UserNoSetPrivsPriv,
        privileges:cluster_privileges() -- [?CLUSTER_SET_PRIVILEGES], [?CLUSTER_SET_PRIVILEGES]
    ),


    VerifyEndFun = fun
        (true = _ShouldSucceed, _, Data) ->
            ExpPrivs = lists:sort(maps:get(<<"privileges">>, Data)),
            {ok, Privs} = oz_test_utils:cluster_get_group_privileges(
                Config, ClusterId, G1
            ),
            ?assertEqual(ExpPrivs, lists:sort(Privs)),
            oz_test_utils:cluster_remove_group(Config, ClusterId, G1);
        (false = ShouldSucceed, _, _) ->
            {ok, Groups} = oz_test_utils:cluster_get_groups(Config, ClusterId),
            ?assertEqual(lists:member(G1, Groups), ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, User},
                root,
                {admin, [?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_CLUSTERS_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserNoSetPrivsPriv}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/clusters/">>, ClusterId, <<"/groups/">>, G1],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/clusters/">>, ClusterId, <<"/groups/">>, G1]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = add_group,
            args = [client, ClusterId, G1, data],
            expected_result = ?OK_BINARY(G1)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [?CLUSTER_UPDATE, ?CLUSTER_REMOVE_GROUP],
                    [?CLUSTER_ADD_USER, ?CLUSTER_VIEW]
                ]
            },
            bad_values = [
                {<<"privileges">>, <<"">>,
                    ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>)}
            ]
        }
    },

    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )).


create_group_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_ADD_GROUP privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, _} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_ADD_GROUP
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    VerifyFun = fun(GroupId, ExpType) ->
        {ok, Group} = oz_test_utils:get_group(Config, GroupId),
        ?assertEqual(?CORRECT_NAME, Group#od_group.name),
        ?assertEqual(ExpType, Group#od_group.type),
        {ok, Groups} = oz_test_utils:cluster_get_groups(Config, C1),
        ?assert(lists:member(GroupId, Groups)),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_GROUPS_CREATE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/clusters/">>, C1, <<"/groups">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                BaseURL = ?URL(Config, [<<"/clusters/">>, C1, <<"/groups/">>]),

                fun(#{<<"Location">> := Location} = _Headers) ->
                    [GroupId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(GroupId, ExpType)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = create_group,
            args = [client, C1, data],
            expected_result = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                ?OK_TERM(fun(ClusterId) -> VerifyFun(ClusterId, ExpType) end)
            end)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"name">>],
            optional = [<<"type">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_group_invite_token_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_ADD_GROUP privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_ADD_GROUP
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_ADD_RELATIONSHIPS]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/clusters/">>, C1, <<"/groups/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = create_group_invite_token,
            args = [client, C1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


remove_group_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_REMOVE_GROUP privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_REMOVE_GROUP
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
        {ok, G1} = oz_test_utils:cluster_add_group(Config, C1, G1),
        #{groupId => G1}
    end,
    DeleteEntityFun = fun(#{groupId := GroupId} = _Env) ->
        oz_test_utils:cluster_remove_group(Config, C1, GroupId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, _) ->
        {ok, Groups} = oz_test_utils:cluster_get_groups(Config, C1),
        ?assertEqual(lists:member(GroupId, Groups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/clusters/">>, C1, <<"/groups/">>, groupId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = remove_group,
            args = [client, C1, groupId],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_groups_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_VIEW privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    ExpGroups = lists:map(
        fun(_) ->
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?ROOT, ?GROUP_NAME1
            ),
            oz_test_utils:cluster_add_group(Config, C1, GroupId),
            GroupId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_LIST_RELATIONSHIPS]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/clusters/">>, C1, <<"/groups">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_groups,
            args = [client, C1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_group_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_VIEW privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(
        Config, ?ROOT,
        #{<<"name">> => ?GROUP_NAME1, <<"type">> => ?GROUP_TYPE1}
    ),
    oz_test_utils:cluster_add_group(Config, C1, G1),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_VIEW]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/clusters/">>, C1, <<"/groups/">>, G1],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"groupId">> => G1,
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1_BIN
            }
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_group,
            args = [client, C1, G1],
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = G1, aspect = instance, scope = shared
            },
            auth_hint = ?THROUGH_CLUSTER(C1),
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_group_privileges_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_VIEW privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get group privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U3), ?GROUP_NAME1),
    {ok, G1} = oz_test_utils:cluster_add_group(Config, C1, G1),

    AllPrivs = privileges:cluster_privileges(),
    InitialPrivs = privileges:cluster_user(),
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:cluster_set_group_privileges(
            Config, C1, G1, PrivsToGrant, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_VIEW_PRIVILEGES]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [
                <<"/clusters/">>, C1, <<"/groups/">>, G1, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_group_privileges,
            args = [client, C1, G1],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?CLUSTER_VIEW_PRIVILEGES
    ])).


update_group_privileges_test(Config) ->
    % create cluster with 2 users:
    %   U2 gets the CLUSTER_SET_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_SET_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to update group privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U3), ?GROUP_NAME1),
    {ok, G1} = oz_test_utils:cluster_add_group(Config, C1, G1),

    AllPrivs = privileges:cluster_privileges(),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:cluster_set_group_privileges(
            Config, C1, G1, PrivsToGrant, PrivsToRevoke
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:cluster_get_group_privileges(Config, C1, G1),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_SET_PRIVILEGES]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [
                <<"/clusters/">>, C1, <<"/groups/">>, G1, <<"/privileges">>
            ],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = update_group_privileges,
            args = [client, C1, G1, data],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?CLUSTER_SET_PRIVILEGES
    ])).


list_eff_groups_test(Config) ->
    {C1,
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}, {G6, _}],
        _EffUsers, {U1, U2, NonAdmin}, {P1, P1Macaroon}
    } = api_test_scenarios:create_cluster_eff_users_env(Config),


    ExpGroups = [G1, G2, G3, G4, G5, G6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_LIST_RELATIONSHIPS]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/clusters/">>, C1, <<"/effective_groups">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_eff_groups,
            args = [client, C1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also cluster_logic:has_eff_group function
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:call_oz(
                Config, cluster_logic, has_eff_group, [C1, GroupId])
            )
        end, ExpGroups
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, cluster_logic, has_eff_group, [C1, <<"asdiucyaie827346w">>])
    ).


get_eff_group_test(Config) ->
    {
        C1, EffGroups, _EffUsers, {U1, U2, NonAdmin}, {P1, P1Macaroon}
    } = api_test_scenarios:create_cluster_eff_users_env(Config),

    lists:foreach(
        fun({GroupId, GroupDetails}) ->
            GroupDetailsBinary = GroupDetails#{
                <<"type">> => atom_to_binary(
                    maps:get(<<"type">>, GroupDetails), utf8
                )
            },
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {admin, [?OZ_GROUPS_VIEW]},
                        {user, U2},
                        {provider, P1, P1Macaroon}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, U1},
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/clusters/">>, C1, <<"/effective_groups/">>, GroupId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = GroupDetailsBinary#{
                        <<"groupId">> => GroupId
                    }
                },
                logic_spec = #logic_spec{
                    module = cluster_logic,
                    function = get_eff_group,
                    args = [client, C1, GroupId],
                    expected_result = ?OK_MAP_CONTAINS(GroupDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_group, id = GroupId,
                        aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_CLUSTER(C1),
                    expected_result = ?OK_MAP(GroupDetailsBinary#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Id} = oz_test_utils:decode_gri(
                                Config, EncodedGri
                            ),
                            ?assertEqual(Id, GroupId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffGroups
    ).


get_eff_group_privileges_test(Config) ->
    %% Create environment with the following relations:
    %%
    %%                  Cluster
    %%                 /  ||  \
    %%                /   ||   \
    %%     [~cluster_view]  ||  [cluster_view]
    %%           /        ||        \
    %%        User1      /  \      User2
    %%                  /    \
    %%                 /      \
    %%             Group1    Group2
    %%                |         |
    %%                |         |
    %%             Group3       |
    %%                  \       |
    %%                   \      |
    %%                    Group4
    %%                      |
    %%                    User3
    %%      <<user>>
    %%      NonAdmin

    % create cluster with 2 users:
    %   U2 gets the CLUSTER_VIEW privilege
    %   U1 gets all remaining privileges
    {C1, U1, U2, {P1, P1Macaroon}} = api_test_scenarios:create_basic_cluster_env(
        Config, ?CLUSTER_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    % User whose eff privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G4} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:cluster_add_group(Config, C1, G1),
    {ok, G2} = oz_test_utils:cluster_add_group(Config, C1, G2),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),
    {ok, G4} = oz_test_utils:group_add_group(Config, G2, G4),
    {ok, G4} = oz_test_utils:group_add_group(Config, G3, G4),
    {ok, U3} = oz_test_utils:group_add_user(Config, G4, U3),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:cluster_privileges(),
    InitialPrivs = privileges:cluster_user(),
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],

    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        % In case of GRANT, randomly split privileges into four
        % parts and update groups with the privileges. G3 eff_privileges
        % should contain the sum of those. In case of revoke, the
        % privileges must be revoked for all 3 entities.
        #{1 := PrivsToGrant1, 2 := PrivsToGrant2} = lists:foldl(
            fun(Privilege, AccMap) ->
                Index = rand:uniform(2),
                AccMap#{
                    Index => [Privilege | maps:get(Index, AccMap)]
                }
            end, #{1 => [], 2 => []}, PrivsToGrant),

        oz_test_utils:cluster_set_group_privileges(Config, C1, G1, PrivsToGrant1, PrivsToRevoke),
        oz_test_utils:cluster_set_group_privileges(Config, C1, G2, PrivsToGrant2, PrivsToRevoke)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_VIEW_PRIVILEGES]},
                {user, U2},
                {provider, P1, P1Macaroon}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [
                <<"/clusters/">>, C1, <<"/effective_groups/">>, G4,
                <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_eff_group_privileges,
            args = [client, C1, G4],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?CLUSTER_VIEW_PRIVILEGES
    ])).


get_eff_group_membership_intermediaries(Config) ->
    %% Create environment with the following relations:
    %%
    %%      Cluster1       Cluster2     Cluster3
    %%       | |  \     /   |  \     /  | \
    %%       | |   \   /    |   \   /   |  \
    %%       |  \   Group2  |   Group3   \  Group4 (no view privs)
    %%       |   \   /      |   /  |      \  /
    %%        \   \ /       |  /   |      User2 (no view privs)
    %%         \  Group1----|-'    |
    %%          \     \     |     /
    %%           \     \    |    /
    %%            \     \   |   /
    %%             '------UserGroup
    %%                      |
    %%                  User1 (view privs)
    %%      <<user>>
    %%      NonAdmin

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, UserGroup} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G4} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),


    {ok, {P1, P1Macaroon}} = oz_test_utils:create_provider(Config, undefined, ?PROVIDER_NAME1),
    C1 = P1,
    {ok, {P2, P2Macaroon}} = oz_test_utils:create_provider(Config, undefined, ?PROVIDER_NAME1),
    C2 = P2,
    {ok, {P3, P3Macaroon}} = oz_test_utils:create_provider(Config, undefined, ?PROVIDER_NAME1),
    C3 = P3,

    oz_test_utils:group_add_user(Config, G4, U2),

    oz_test_utils:cluster_add_user(Config, C1, U1),
    oz_test_utils:cluster_add_user(Config, C3, U2),
    oz_test_utils:cluster_set_user_privileges(Config, C3, U2, [], [?CLUSTER_VIEW]),

    oz_test_utils:group_add_group(Config, G1, UserGroup),
    oz_test_utils:group_add_group(Config, G2, G1),
    oz_test_utils:group_add_group(Config, G3, G1),
    oz_test_utils:group_add_group(Config, G3, UserGroup),

    oz_test_utils:cluster_add_group(Config, C1, UserGroup),
    oz_test_utils:cluster_add_group(Config, C1, G1),
    oz_test_utils:cluster_add_group(Config, C1, G2),
    oz_test_utils:cluster_add_group(Config, C2, UserGroup),
    oz_test_utils:cluster_add_group(Config, C2, G2),
    oz_test_utils:cluster_add_group(Config, C2, G3),
    oz_test_utils:cluster_add_group(Config, C3, G3),
    oz_test_utils:cluster_add_group(Config, C3, G4),
    oz_test_utils:cluster_set_group_privileges(Config, C3, G4, [], [?CLUSTER_VIEW]),

    % {ClusterId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}
    ExpectedMembershipIntermediaries = [
        {C1, UserGroup, [{provider, P1, P1Macaroon}], [U1], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY},
            {od_group, G1},
            {od_group, G2}
        ])},
        {C1, G1, [{provider, P1, P1Macaroon}], [U1], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY},
            {od_group, G2}
        ])},
        {C1, G2, [{provider, P1, P1Macaroon}], [U1], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY}
        ])},

        {C2, UserGroup, [{provider, P2, P2Macaroon}], [U1], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY},
            {od_group, G2},
            {od_group, G3}
        ])},
        {C2, G1, [{provider, P2, P2Macaroon}], [U1], ordsets:from_list([
            {od_group, G2},
            {od_group, G3}
        ])},
        {C2, G2, [{provider, P2, P2Macaroon}], [U1], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY}
        ])},
        {C2, G3, [{provider, P2, P2Macaroon}], [U1], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY}
        ])},

        {C3, UserGroup, [{provider, P3, P3Macaroon}], [U1], ordsets:from_list([
            {od_group, G3}
        ])},
        {C3, G1, [{provider, P3, P3Macaroon}], [U1], ordsets:from_list([
            {od_group, G3}
        ])},
        {C3, G3, [{provider, P3, P3Macaroon}], [U1], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY}
        ])},
        {C3, G4, [{provider, P3, P3Macaroon}], [U1, U2], ordsets:from_list([
            {od_cluster, ?SELF_INTERMEDIARY}
        ])}
    ],

    lists:foreach(fun({ClusterId, GroupId, CorrectProviderClients, CorrectUsers, ExpIntermediariesRaw}) ->
        ExpIntermediaries = lists:map(fun({Type, Id}) ->
            #{<<"type">> => gs_protocol_plugin:encode_entity_type(Type), <<"id">> => Id}
        end, ExpIntermediariesRaw),
        CorrectUserClients = [{user, U} || U <- CorrectUsers],
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_CLUSTERS_VIEW]}
                ] ++ CorrectUserClients ++ CorrectProviderClients,
                unauthorized = [nobody],
                forbidden = [
                    {user, NonAdmin}, {user, U1}, {user, U2},
                    {provider, P1, P1Macaroon}, {provider, P2, P2Macaroon}, {provider, P3, P3Macaroon}
                ] -- (CorrectUserClients ++ CorrectProviderClients)
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/clusters/">>, ClusterId, <<"/effective_groups/">>, GroupId, <<"/membership">>],
                expected_code = ?HTTP_200_OK,
                expected_body = #{<<"intermediaries">> => ExpIntermediaries}
            },
            logic_spec = #logic_spec{
                module = cluster_logic,
                function = get_eff_group_membership_intermediaries,
                args = [client, ClusterId, GroupId],
                expected_result = ?OK_LIST(ExpIntermediariesRaw)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, ExpectedMembershipIntermediaries).


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
