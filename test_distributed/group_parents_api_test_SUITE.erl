%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C): 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning groups parents API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_parents_api_test_SUITE).
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
    list_parents_test/1,
    join_group_test/1,
    leave_group_test/1,
    get_parent_details_test/1,
    list_eff_parents_test/1,
    get_eff_parent_details_test/1
]).

all() ->
    ?ALL([
        list_parents_test,
        join_group_test,
        leave_group_test,
        get_parent_details_test,
        list_eff_parents_test,
        get_eff_parent_details_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_parents_test(Config) ->
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

    ExpGroups = lists:map(
        fun(Idx) ->
            IdxBin = integer_to_binary(Idx),
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?ROOT, <<"ParentGroup", IdxBin/binary>>
            ),
            oz_test_utils:add_group_to_group(Config, GroupId, G1),
            GroupId
        end, lists:seq(1, 5)
    ),

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
            path = [<<"/groups/">>, G1, <<"/parents">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"parent_groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_parents,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


join_group_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U1), <<"G2">>),
    oz_test_utils:group_set_user_privileges(Config, G2, U1, revoke, [
        ?GROUP_JOIN_GROUP
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G2, U2),
    oz_test_utils:group_set_user_privileges(Config, G2, U2, set, [
        ?GROUP_JOIN_GROUP
    ]),

    EnvSetUpFun = fun() ->
        {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, <<"G1">>),
        #{groupId => G1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := G1} = _Env, _) ->
        {ok, ChildrenGroups} = oz_test_utils:get_group_children(Config, G1),
        ?assertEqual(lists:member(G2, ChildrenGroups), ShouldSucceed)
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
            path = [<<"/groups/">>, G2, <<"/parents/join">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{groupId := G1} = _Env, _) ->
                fun(#{<<"location">> := Location} = _Headers) ->
                    [GroupId, ParentGroupId] = binary:split(
                        Location,
                        [<<"/groups/">>, <<"/nested/">>],
                        [trim_all, global]
                    ),
                    ?assertEqual(GroupId, G2),
                    ?assertEqual(ParentGroupId, G1),
                    true
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_group,
            args = [client, G2, data],
            expected_result = ?OK_ENV(fun(#{groupId := G1} = _Env, _) ->
                ?OK_BINARY(G1)
            end)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [fun(#{groupId := G1} = _Env) ->
                    {ok, Macaroon} = oz_test_utils:group_invite_group_token(
                        Config, ?ROOT, G1
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


leave_group_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, Group} = oz_test_utils:create_group(Config, ?USER(U1), <<"Group">>),
    oz_test_utils:group_set_user_privileges(Config, Group, U1, revoke, [
        % TODO VFS-3351 ?GROUP_LEAVE_GROUP
        ?GROUP_UPDATE
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, Group, U2),
    oz_test_utils:group_set_user_privileges(Config, Group, U2, set, [
        % TODO VFS-3351 ?GROUP_LEAVE_GROUP
        ?GROUP_UPDATE
    ]),

    EnvSetUpFun = fun() ->
        {ok, ParentGroup} = oz_test_utils:create_group(
            Config, ?ROOT, <<"PG">>
        ),
        {ok, Group} = oz_test_utils:add_group_to_group(
            Config, ParentGroup, Group
        ),
        #{parentGroupId => ParentGroup}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{parentGroupId := PGID} = _Env, _) ->
        {ok, ChildrenGroups} = oz_test_utils:get_group_children(Config, PGID),
        ?assertEqual(lists:member(Group, ChildrenGroups), not ShouldSucceed)
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
            path = [<<"/groups/">>, Group, <<"/parents/">>, parentGroupId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_group,
            args = [client, Group, parentGroupId],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun]
    )).


get_parent_details_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, Group} = oz_test_utils:create_group(Config, ?USER(U1), <<"G2">>),
    oz_test_utils:group_set_user_privileges(Config, Group, U1, revoke, [
        ?GROUP_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, Group, U2),
    oz_test_utils:group_set_user_privileges(Config, Group, U2, set, [
        ?GROUP_VIEW
    ]),

    ExpName = <<"G1">>,
    ExpType = organization,
    {ok, ParentGroup} = oz_test_utils:create_group(
        Config, ?ROOT, #{<<"name">> => ExpName, <<"type">> => ExpType}
    ),
    oz_test_utils:add_group_to_group(Config, ParentGroup, Group),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, Group, <<"/parents/">>, ParentGroup],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"groupId">> => ParentGroup,
                <<"name">> => ExpName,
                <<"type">> => atom_to_binary(ExpType, utf8)
            }
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_parent,
            args = [client, Group, ParentGroup],
            expected_result = ?OK_MAP(#{
                    <<"name">> => ExpName,
                    <<"type">> => ExpType
                })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = ParentGroup,
                aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_GROUP(Group),
            expected_result = ?OK_MAP(#{
                <<"name">> => ExpName,
                <<"type">> => atom_to_binary(ExpType, utf8),
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(ParentGroup, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_eff_parents_test(Config) ->
    {
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_groups_env(Config),

    ExpGroups = [G2, G3, G4, G5],
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
            path = [<<"/groups/">>, G1, <<"/effective_parents">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"parents">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_parents,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_parent function
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_parent, [G1, GroupId])
            )
        end, ExpGroups
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_parent, [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_parent_details_test(Config) ->
    {
        [{G1, _} | EffParentsList], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_groups_env(Config),

    lists:foreach(
        fun({GroupId, GroupDetails}) ->
            ExpType = maps:get(<<"type">>, GroupDetails, role),

            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, U1},
                        {user, U2}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/groups/">>, G1, <<"/effective_parents/">>, GroupId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = GroupDetails#{
                        <<"groupId">> => GroupId,
                        <<"type">> => atom_to_binary(
                            maps:get(<<"type">>, GroupDetails), utf8
                        )
                    }
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_parent,
                    args = [client, G1, GroupId],
                    expected_result = ?OK_MAP(GroupDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_group, id = GroupId,
                        aspect = instance, scope = protected
                    },
                    auth_hint = ?THROUGH_GROUP(G1),
                    expected_result = ?OK_MAP(
                        GroupDetails#{
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
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffParentsList
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
