%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning group parents API (REST + logic + gs).
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
-include_lib("ctool/include/api_errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    list_parents_test/1,
    create_parent_test/1,
    join_parent_test/1,
    leave_parent_test/1,
    get_parent_details_test/1,
    list_eff_parents_test/1,
    get_eff_parent_details_test/1
]).

all() ->
    ?ALL([
        list_parents_test,
        create_parent_test,
        join_parent_test,
        leave_parent_test,
        get_parent_details_test,
        list_eff_parents_test,
        get_eff_parent_details_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_parents_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    ExpGroups = lists:map(
        fun(_) ->
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?ROOT, ?GROUP_NAME2
            ),
            oz_test_utils:group_add_group(Config, GroupId, G1),
            GroupId
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
            path = [<<"/groups/">>, G1, <<"/parents">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
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


create_parent_test(Config) ->
    % create group with 2 users:
    %   U2 gets ?GROUP_ADD_PARENT privilege
    %   U1 gets all remaining privileges
    {Child, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_PARENT
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    AllPrivs = oz_test_utils:all_group_privileges(Config),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    VerifyFun = fun(ParentId, Data) ->
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        {ok, Parent} = oz_test_utils:get_group(Config, ParentId),
        ExpName = maps:get(<<"name">>, Data),
        ExpType = maps:get(<<"type">>, Data, team),
        ?assertEqual(ExpName, Parent#od_group.name),
        ?assertEqual(ExpType, Parent#od_group.type),

        {ok, Children} = oz_test_utils:group_get_children(Config, ParentId),
        ?assert(lists:member(Child, Children)),

        ?assertEqual(#{Child => AllPrivs}, Parent#od_group.children),
        ?assertEqual(#{Child => {AllPrivs, [{od_group, <<"self">>}]}}, Parent#od_group.eff_children),

        ?assertEqual(#{}, Parent#od_group.users),
        ?assertEqual(
            #{
                U1 => {AllPrivs, [{od_group, Child}]},
                U2 => {AllPrivs, [{od_group, Child}]}
            },
            Parent#od_group.eff_users
        ),
        true
    end,


    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_GROUPS_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, U2}
            ],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, Child, <<"/parents/">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, Data) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/groups/">>, Child, <<"/parents/">>]),
                    [ParentId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(ParentId, Data)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_parent_group,
            args = [client, Child, data],
            expected_result = ?OK_ENV(fun(_, Data) ->
                ?OK_TERM(fun(ParentId) -> VerifyFun(ParentId, Data) end)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_group, aspect = instance},
            auth_hint = ?AS_GROUP(Child),
            expected_result = ?OK_ENV(fun(_, Data) ->
                ExpName = maps:get(<<"name">>, Data),
                ExpType = maps:get(<<"type">>, Data, team),
                ?OK_MAP_CONTAINS(#{
                    <<"children">> => #{Child => AllPrivsBin},
                    <<"name">> => ExpName,
                    <<"parents">> => [],
                    <<"spaces">> => [],
                    <<"type">> => atom_to_binary(ExpType, utf8),
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = oz_test_utils:decode_gri(
                            Config, EncodedGri
                        ),
                        VerifyFun(Id, Data)
                    end
                })
            end)
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


join_parent_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_ADD_PARENT privilege
    %   U1 gets all remaining privileges
    {Child, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_PARENT
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    CreateTokenForItselfFun = fun() ->
        {ok, Macaroon} = oz_test_utils:group_invite_group_token(
            Config, ?ROOT, Child
        ),
        {ok, Token} = onedata_macaroons:serialize(Macaroon),
        Token
    end,

    EnvSetUpFun = fun() ->
        {ok, Group} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME2),
        {ok, Macaroon} = oz_test_utils:group_invite_group_token(
            Config, ?ROOT, Group
        ),
        {ok, Token} = onedata_macaroons:serialize(Macaroon),
        #{
            groupId => Group,
            macaroonId => macaroon:identifier(Macaroon),
            token => Token
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId, macaroonId := MacaroonId} = _Env, _) ->
        {ok, ChildGroups} = oz_test_utils:group_get_children(Config, GroupId),
        ?assertEqual(lists:member(Child, ChildGroups), ShouldSucceed),
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
            path = [<<"/groups/">>, Child, <<"/parents/join">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{groupId := GroupId} = _Env, _) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config,
                        [<<"/groups/">>, Child, <<"/parents/">>, GroupId]
                    ),
                    ?assertMatch(ExpLocation, Location),
                    true
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_group,
            args = [client, Child, data],
            expected_result = ?OK_ENV(fun(#{groupId := GroupId} = _Env, _) ->
                ?OK_BINARY(GroupId)
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
                {<<"token">>, CreateTokenForItselfFun,
                    ?ERROR_CANNOT_JOIN_GROUP_TO_ITSELF},
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
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(U1),
        #{<<"name">> => ?GROUP_NAME1, <<"type">> => ?GROUP_TYPE1}
    ),
    {ok, Macaroon1} = oz_test_utils:group_invite_group_token(
        Config, ?ROOT, Group
    ),
    {ok, Token} = onedata_macaroons:serialize(Macaroon1),
    oz_test_utils:group_add_group(Config, Group, Child),
    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, Child, <<"/parents/join">>],
            expected_code = ?HTTP_400_BAD_REQUEST
        },
        logic_spec = LogicSpec = #logic_spec{
            module = group_logic,
            function = join_group,
            args = [client, Child, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_group, Child, od_group, Group))
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [Token]}
        }
    },
    VerifyEndFun1 = fun(MacaroonId) ->
        fun(_ShouldSucceed,_Env,_) ->
            oz_test_utils:assert_token_exists(Config, macaroon:identifier(MacaroonId))
        end
    end,
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec1, undefined, undefined, VerifyEndFun1(Macaroon1)
    )),
    
    {ok, Macaroon2} = oz_test_utils:group_invite_group_token(
        Config, ?ROOT, Child
    ),
    {ok, Token2} = onedata_macaroons:serialize(Macaroon2),
    ApiTestSpec2 = ApiTestSpec1#api_test_spec{
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?ERROR_REASON(?ERROR_CANNOT_JOIN_GROUP_TO_ITSELF)
        },
        data_spec = #data_spec{
        required = [<<"token">>],
        correct_values = #{<<"token">> => [Token2]}
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, undefined, undefined, VerifyEndFun1(Macaroon2)
    )). 


leave_parent_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_LEAVE_PARENT privilege
    %   U1 gets all remaining privileges
    {Child, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_LEAVE_PARENT
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, Group} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME2),
        {ok, Child} = oz_test_utils:group_add_group(Config, Group, Child),
        #{groupId => Group}
    end,
    DeleteEntityFun = fun(#{groupId := GroupId} = _Env) ->
        oz_test_utils:group_remove_group(Config, GroupId, Child)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, _) ->
        {ok, ChildGroups} = oz_test_utils:group_get_children(Config, GroupId),
        ?assertEqual(lists:member(Child, ChildGroups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_REMOVE_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, Child, <<"/parents/">>, groupId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_group,
            args = [client, Child, groupId],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


get_parent_details_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {Group, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, ParentGroup} = oz_test_utils:create_group(Config, ?ROOT,
        #{<<"name">> => ?GROUP_NAME2, <<"type">> => ?GROUP_TYPE2}
    ),
    oz_test_utils:group_add_group(Config, ParentGroup, Group),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {user, U2},
                {admin, [?OZ_GROUPS_VIEW]}
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
                <<"name">> => ?GROUP_NAME2,
                <<"type">> => ?GROUP_TYPE2_BIN
            }
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_parent,
            args = [client, Group, ParentGroup],
            expected_result = ?OK_MAP(#{
                <<"name">> => ?GROUP_NAME2,
                <<"type">> => ?GROUP_TYPE2
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
                <<"name">> => ?GROUP_NAME2,
                <<"type">> => ?GROUP_TYPE2_BIN,
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
    } = api_test_scenarios:create_eff_parent_groups_env(Config),

    ExpGroups = [G2, G3, G4, G5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]},
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
            expected_body = #{<<"groups">> => ExpGroups}
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
        [{G1, _} | EffParents], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_parent_groups_env(Config),

    lists:foreach(
        fun({GroupId, GroupDetails}) ->
            ExpType = maps:get(<<"type">>, GroupDetails, team),

            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, U1},
                        {user, U2},
                        {admin, [?OZ_GROUPS_VIEW]}
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

        end, EffParents
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
