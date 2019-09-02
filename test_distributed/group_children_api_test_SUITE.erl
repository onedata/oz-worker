%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning group children API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_children_api_test_SUITE).
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
    list_children_test/1,
    create_group_invite_token_test/1,
    create_child_test/1,
    get_child_details_test/1,
    add_child_test/1,
    add_child_with_privileges_test/1,
    remove_child_test/1,
    get_child_privileges_test/1,
    update_child_privileges_test/1,
    get_eff_children_test/1,
    get_eff_child_details_test/1,
    get_eff_child_privileges_test/1,
    get_eff_child_membership_intermediaries/1
]).

all() ->
    ?ALL([
        list_children_test,
        create_group_invite_token_test,
        create_child_test,
        get_child_details_test,
        add_child_test,
        add_child_with_privileges_test,
        remove_child_test,
        get_child_privileges_test,
        update_child_privileges_test,
        get_eff_children_test,
        get_eff_child_details_test,
        get_eff_child_privileges_test,
        get_eff_child_membership_intermediaries
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_children_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ExpChildren = lists:map(
        fun(_) ->
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?ROOT, ?GROUP_NAME2
            ),
            oz_test_utils:group_add_group(Config, G1, GroupId),
            GroupId
        end, lists:seq(1, 5)
    ),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

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
            path = [<<"/groups/">>, G1, <<"/children">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpChildren}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_children,
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpChildren)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_group_invite_token_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_ADD_CHILD privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_CHILD
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/children/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) ->
                VerifyFun(Token)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_group_invite_token,
            args = [auth, G1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_child_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_ADD_CHILD privilege
    %   U1 gets all remaining privileges
    {Parent, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_CHILD
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    VerifyFun = fun(GroupId, ExpType) ->
        {ok, Group} = oz_test_utils:get_group(Config, GroupId),
        ?assertEqual(?CORRECT_NAME, Group#od_group.name),
        ?assertEqual(ExpType, Group#od_group.type),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_CREATE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, Parent, <<"/children">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                BaseURL = ?URL(Config, [<<"/groups/">>, Parent, <<"/children/">>]),

                fun(#{<<"Location">> := Location} = _Headers) ->
                    [GroupId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(GroupId, ExpType)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_child_group,
            args = [auth, Parent, data],
            expected_result = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                ?OK_TERM(fun(GroupId) -> VerifyFun(GroupId, ExpType) end)
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


get_child_details_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT,
        #{<<"name">> => ?GROUP_NAME2, <<"type">> => ?GROUP_TYPE2}
    ),
    oz_test_utils:group_add_group(Config, G1, G2),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/children/">>, G2],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"groupId">> => G2,
                <<"name">> => ?GROUP_NAME2,
                <<"type">> => ?GROUP_TYPE2_BIN
            }
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_child,
            args = [auth, G1, G2],
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?GROUP_NAME2,
                <<"type">> => ?GROUP_TYPE2
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = G2, aspect = instance, scope = shared
            },
            auth_hint = ?THROUGH_GROUP(G1),
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?GROUP_NAME2,
                <<"type">> => ?GROUP_TYPE2_BIN,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(Id, G2)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


add_child_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, UserNoAddChildPriv} = oz_test_utils:create_user(Config),
    {ok, UserNoAddParentPriv} = oz_test_utils:create_user(Config),

    {ok, ParentGroup} = oz_test_utils:create_group(Config, ?USER(User), ?GROUP_NAME1),
    {ok, ChildGroup} = oz_test_utils:create_group(Config, ?USER(User), ?GROUP_NAME2),

    oz_test_utils:group_add_user(Config, ParentGroup, UserNoAddChildPriv),
    oz_test_utils:group_add_user(Config, ChildGroup, UserNoAddChildPriv),
    oz_test_utils:group_set_user_privileges(Config, ChildGroup, UserNoAddChildPriv,
        privileges:group_privileges(), []
    ),
    oz_test_utils:group_set_user_privileges(Config, ParentGroup, UserNoAddChildPriv,
        privileges:group_privileges() -- [?GROUP_ADD_CHILD], [?GROUP_ADD_CHILD]
    ),

    oz_test_utils:group_add_user(Config, ParentGroup, UserNoAddParentPriv),
    oz_test_utils:group_add_user(Config, ChildGroup, UserNoAddParentPriv),
    oz_test_utils:group_set_user_privileges(Config, ChildGroup, UserNoAddParentPriv,
        privileges:group_privileges() -- [?GROUP_ADD_PARENT], [?GROUP_ADD_PARENT]
    ),
    oz_test_utils:group_set_user_privileges(Config, ParentGroup, UserNoAddParentPriv,
        privileges:group_privileges(), []
    ),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, _) ->
            {ok, SubGroups} = oz_test_utils:group_get_children(Config, ParentGroup),
            ?assert(lists:member(ChildGroup, SubGroups)),
            oz_test_utils:group_remove_group(Config, ParentGroup, ChildGroup);
        (false = _ShouldSucceed, _, _) ->
            {ok, SubGroups} = oz_test_utils:group_get_children(Config, ParentGroup),
            ?assertNot(lists:member(ChildGroup, SubGroups))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, User}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserNoAddChildPriv},
                {user, UserNoAddParentPriv}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/groups/">>, ParentGroup, <<"/children/">>, ChildGroup],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/groups/">>, ParentGroup, <<"/children/">>, ChildGroup]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = add_group,
            args = [auth, ParentGroup, ChildGroup, data],
            expected_result = ?OK_BINARY(ChildGroup)
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


add_child_with_privileges_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, UserNoSetPrivsPriv} = oz_test_utils:create_user(Config),

    {ok, ParentGroup} = oz_test_utils:create_group(Config, ?USER(User), ?GROUP_NAME1),
    {ok, ChildGroup} = oz_test_utils:create_group(Config, ?USER(User), ?GROUP_NAME2),

    oz_test_utils:group_add_user(Config, ParentGroup, UserNoSetPrivsPriv),
    oz_test_utils:group_add_user(Config, ChildGroup, UserNoSetPrivsPriv),
    oz_test_utils:group_set_user_privileges(Config, ChildGroup, UserNoSetPrivsPriv,
        privileges:group_privileges(), []
    ),
    oz_test_utils:group_set_user_privileges(Config, ParentGroup, UserNoSetPrivsPriv,
        privileges:group_privileges() -- [?GROUP_SET_PRIVILEGES], [?GROUP_SET_PRIVILEGES]
    ),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, Data) ->
            ExpPrivs = lists:sort(maps:get(<<"privileges">>, Data)),
            {ok, Privs} = oz_test_utils:group_get_group_privileges(
                Config, ParentGroup, ChildGroup
            ),
            ?assertEqual(ExpPrivs, lists:sort(Privs)),
            oz_test_utils:group_remove_group(Config, ParentGroup, ChildGroup);
        (false = _ShouldSucceed, _, _) ->
            {ok, SubGroups} = oz_test_utils:group_get_children(Config, ParentGroup),
            ?assertNot(lists:member(ChildGroup, SubGroups))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_SET_PRIVILEGES, ?OZ_GROUPS_SET_PRIVILEGES]},
                {user, User}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserNoSetPrivsPriv}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/groups/">>, ParentGroup, <<"/children/">>, ChildGroup],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/groups/">>, ParentGroup, <<"/children/">>, ChildGroup]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = add_group,
            args = [auth, ParentGroup, ChildGroup, data],
            expected_result = ?OK_BINARY(ChildGroup)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [?GROUP_ADD_PARENT, ?GROUP_REMOVE_CHILD],
                    [?GROUP_ADD_USER, ?GROUP_VIEW]
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


remove_child_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_REMOVE_GROUP privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_REMOVE_CHILD
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME2),
        {ok, G2} = oz_test_utils:group_add_group(Config, G1, G2),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        #{groupId => G2}
    end,
    DeleteEntityFun = fun(#{groupId := GroupId} = _Env) ->
        oz_test_utils:group_remove_group(Config, G1, GroupId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, _) ->
        {ok, SubGroups} = oz_test_utils:group_get_children(Config, G1),
        ?assertEqual(lists:member(GroupId, SubGroups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_REMOVE_RELATIONSHIPS]},
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
            path = [<<"/groups/">>, G1, <<"/children/">>, groupId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = remove_group,
            args = [auth, G1, groupId],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


get_child_privileges_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get group privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U3), ?GROUP_NAME2),
    {ok, G2} = oz_test_utils:group_add_group(Config, G1, G2),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:group_privileges(),
    InitialPrivs = [?GROUP_VIEW],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:group_set_group_privileges(
            Config, G1, G2, PrivsToGrant, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_VIEW_PRIVILEGES]}
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
                <<"/groups/">>, G1, <<"/children/">>, G2, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_child_privileges,
            args = [auth, G1, G2],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?GROUP_VIEW_PRIVILEGES
    ])).


update_child_privileges_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_SET_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_SET_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to update group privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U3), ?GROUP_NAME2),
    {ok, G2} = oz_test_utils:group_add_group(Config, G1, G2),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:group_privileges(),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:group_set_group_privileges(
            Config, G1, G2, PrivsToGrant, PrivsToRevoke
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:group_get_group_privileges(
            Config, G1, G2
        ),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_SET_PRIVILEGES]}
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
                <<"/groups/">>, G1, <<"/children/">>, G2, <<"/privileges">>
            ],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = update_child_privileges,
            args = [auth, G1, G2, data],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?GROUP_SET_PRIVILEGES
    ])).


get_eff_children_test(Config) ->
    {
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}, {G6, _}], _Users
    } = api_test_scenarios:create_eff_child_groups_env(Config),

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllGroupPrivs = privileges:group_privileges(),
    {ok, U1} = oz_test_utils:group_add_user(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1,
        AllGroupPrivs -- [?GROUP_VIEW], [?GROUP_VIEW]
    ),
    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2,
        [?GROUP_VIEW], AllGroupPrivs -- [?GROUP_VIEW]
    ),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ExpGroups = [G2, G3, G4, G5, G6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/effective_children">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_children,
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_child function
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_child, [G1, GroupId])
            )
        end, ExpGroups
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_child, [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_child_details_test(Config) ->
    {
        [{G1, _} | EffChildren], _Users
    } = api_test_scenarios:create_eff_child_groups_env(Config),

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllGroupPrivs = privileges:group_privileges(),
    {ok, U1} = oz_test_utils:group_add_user(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1,
        AllGroupPrivs -- [?GROUP_VIEW], [?GROUP_VIEW]
    ),
    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2,
        [?GROUP_VIEW], AllGroupPrivs -- [?GROUP_VIEW]
    ),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

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
                        {user, U2}
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
                        <<"/groups/">>, G1, <<"/effective_children/">>, GroupId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = GroupDetailsBinary#{
                        <<"groupId">> => GroupId
                    }
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_child,
                    args = [auth, G1, GroupId],
                    expected_result = ?OK_MAP_CONTAINS(GroupDetails)
                },
                gs_spec = #gs_spec{
                    operation = get,
                    gri = #gri{
                        type = od_group, id = GroupId,
                        aspect = instance, scope = shared
                    },
                    auth_hint = ?THROUGH_GROUP(G1),
                    expected_result = ?OK_MAP_CONTAINS(GroupDetailsBinary#{
                        <<"gri">> => fun(EncodedGri) ->
                            #gri{id = Gid} = gri:deserialize(EncodedGri),
                            ?assertEqual(Gid, GroupId)
                        end
                    })
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffChildren
    ).


get_eff_child_privileges_test(Config) ->
    %% Create environment with the following relations:
    %%
    %%           User2          User3
    %%              \            /
    %%               \          /
    %%        [group_view]   [~group_view]
    %%                 \      /
    %%                  Group1
    %%                 /    \
    %%                /      \
    %%             Group2     \
    %%                   \    |
    %%                    \   |
    %%                    Group3
    %%                      |
    %%                    User1

    AllPrivs = privileges:group_privileges(),
    InitialPrivs = [?GROUP_VIEW],
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes has privilege
    % to get group privileges and sometimes not)
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {G3, G2, G1} = oz_test_utils:create_3_nested_groups(Config, U1),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),

    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2,
        [?GROUP_VIEW_PRIVILEGES], AllPrivs -- [?GROUP_VIEW_PRIVILEGES]
    ),
    {ok, U3} = oz_test_utils:group_add_user(Config, G1, U3),
    oz_test_utils:group_set_user_privileges(Config, G1, U3,
        AllPrivs -- [?GROUP_VIEW_PRIVILEGES], [?GROUP_VIEW_PRIVILEGES]
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

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

        oz_test_utils:group_set_group_privileges(
            Config, G1, G2, PrivsToGrant1, PrivsToRevoke
        ),
        oz_test_utils:group_set_group_privileges(
            Config, G1, G3, PrivsToGrant2, PrivsToRevoke
        ),

        oz_test_utils:ensure_entity_graph_is_up_to_date(Config)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_VIEW_PRIVILEGES]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U3},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [
                <<"/groups/">>, G1,
                <<"/effective_children/">>, G3, <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_child_privileges,
            args = [auth, G1, G3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U1}, ?GROUP_VIEW_PRIVILEGES
    ])).


get_eff_child_membership_intermediaries(Config) ->
    %% Create environment with the following relations:
    %%
    %%                   Group1    Group5
    %%                  /   |  \     /
    %%                 /    |   \   /
    %%              Group2  |    Group4
    %%               /      |   /  | | \
    %%              /       |  /   | |  \
    %%            Group3----|-'    | |  Group6 (no view privs)
    %%                \     |     /   \   /
    %%                 \    |    /     User2 (no view privs)
    %%                  \   |   /
    %%                  UserGroup
    %%                      |
    %%                    User1 (view privs)
    %%
    %%      <<user>>
    %%      NonAdmin

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, UserGroup} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G4} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G5} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G6} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    oz_test_utils:group_add_user(Config, G4, U2),
    oz_test_utils:group_set_user_privileges(Config, G4, U2, [], [?GROUP_VIEW]),
    oz_test_utils:group_add_user(Config, G6, U2),
    oz_test_utils:group_set_user_privileges(Config, G6, U2, [], [?GROUP_VIEW]),

    oz_test_utils:group_add_group(Config, G1, UserGroup),
    oz_test_utils:group_add_group(Config, G1, G2),
    oz_test_utils:group_add_group(Config, G1, G4),
    oz_test_utils:group_add_group(Config, G3, UserGroup),
    oz_test_utils:group_add_group(Config, G2, G3),
    oz_test_utils:group_add_group(Config, G4, G3),
    oz_test_utils:group_add_group(Config, G4, UserGroup),
    oz_test_utils:group_add_group(Config, G4, G6),
    oz_test_utils:group_set_group_privileges(Config, G4, G6, [], [?GROUP_VIEW]),
    oz_test_utils:group_add_group(Config, G5, G4),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % {GroupId, ChildId, CorrectUsers, ExpIntermediariesRaw}
    ExpectedMembershipIntermediaries = [
        {G1, UserGroup, [U1, U2], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY},
            {od_group, G2},
            {od_group, G4}
        ])},
        {G1, G2, [U1, U2], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY}
        ])},
        {G1, G3, [U1, U2], ordsets:from_list([
            {od_group, G2},
            {od_group, G4}
        ])},
        {G1, G4, [U1, U2], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY}
        ])},
        {G1, G6, [U1, U2], ordsets:from_list([
            {od_group, G4}
        ])},

        {G2, UserGroup, [U1], ordsets:from_list([
            {od_group, G3}
        ])},
        {G2, G3, [U1], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY}
        ])},

        {G3, UserGroup, [U1], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY}
        ])},

        {G4, UserGroup, [U1], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY},
            {od_group, G3}
        ])},
        {G4, G3, [U1], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY}
        ])},
        {G4, G6, [U1, U2], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY}
        ])},

        {G5, UserGroup, [U1, U2], ordsets:from_list([
            {od_group, G4}
        ])},
        {G5, G3, [U1, U2], ordsets:from_list([
            {od_group, G4}
        ])},
        {G5, G4, [U1, U2], ordsets:from_list([
            {od_group, ?SELF_INTERMEDIARY}
        ])},
        {G5, G6, [U1, U2], ordsets:from_list([
            {od_group, G4}
        ])}
    ],

    lists:foreach(fun({ParentId, ChildId, CorrectUsers, ExpIntermediariesRaw}) ->
        ExpIntermediaries = lists:map(fun({Type, Id}) ->
            #{<<"type">> => gri:serialize_type(Type, regular), <<"id">> => Id}
        end, ExpIntermediariesRaw),
        CorrectUserClients = [{user, U} || U <- CorrectUsers],
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_GROUPS_VIEW]}
                ] ++ CorrectUserClients,
                unauthorized = [nobody],
                forbidden = [{user, NonAdmin}, {user, U1}, {user, U2}] -- CorrectUserClients
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/groups/">>, ParentId, <<"/effective_children/">>, ChildId, <<"/membership">>],
                expected_code = ?HTTP_200_OK,
                expected_body = #{<<"intermediaries">> => ExpIntermediaries}
            },
            logic_spec = #logic_spec{
                module = group_logic,
                function = get_eff_child_membership_intermediaries,
                args = [auth, ParentId, ChildId],
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
