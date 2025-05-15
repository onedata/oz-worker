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
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

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
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_parent_test(Config) ->
    % create group with 2 users:
    %   U2 gets ?GROUP_ADD_PARENT privilege
    %   U1 gets all remaining privileges
    {Child, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_ADD_PARENT
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:group_privileges(),

    VerifyFun = fun(ParentId, Data) ->
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        {ok, Parent} = oz_test_utils:get_group(Config, ParentId),
        ExpName = maps:get(<<"name">>, Data),
        ExpType = maps:get(<<"type">>, Data, ?DEFAULT_GROUP_TYPE),
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
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/groups/">>, Child, <<"/parents/">>]),
                    [ParentId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(ParentId, Data)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_parent_group,
            args = [auth, Child, data],
            expected_result = ?OK_ENV(fun(_, Data) ->
                ?OK_TERM(fun(ParentId) -> VerifyFun(ParentId, Data) end)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_group, aspect = instance},
            auth_hint = ?AS_GROUP(Child),
            expected_result_op = ?OK_ENV(fun(_, Data) ->
                ?OK_MAP_CONTAINS(#{
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
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
                    ?ERR_BAD_VALUE_NOT_ALLOWED(<<"type">>, ?GROUP_TYPES)},
                {<<"type">>, 1234, ?ERR_BAD_VALUE_STRING(<<"type">>)}
                | ?BAD_VALUES_NAME(?ERR_BAD_VALUE_NAME(undefined))
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


join_parent_test(Config) ->
    UserWithPrivilege = ozt_users:create(),
    UserWithoutPrivilege = ozt_users:create(),

    join_parent_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create/0
    ),
    join_parent_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create_protected/0, fun ozt_groups:create/0
    ),
    join_parent_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create_protected/0
    ),
    join_parent_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        blocked_by_protection, fun ozt_groups:create_protected/0, fun ozt_groups:create_protected/0
    ).


join_parent_test_base(Config, UserWithPrivilege, UserWithoutPrivilege, ExpOutcome, ChildCreateFun, ParentCreateFun) ->
    NonAdmin = ozt_users:create(),

    ChildId = ChildCreateFun(),
    ozt_groups:add_user(ChildId, UserWithPrivilege, [?GROUP_ADD_PARENT]),
    ozt_groups:add_user(ChildId, UserWithoutPrivilege, privileges:group_admin() -- [?GROUP_ADD_PARENT]),

    TokenCreatorInChildGroup = ozt_users:create(),
    ozt_groups:add_user(ChildId, TokenCreatorInChildGroup, [?GROUP_ADD_CHILD]),
    CreateTokenForItselfFun = fun() ->
        ozt_tokens:ensure_serialized(
            ozt_groups:create_one_shot_group_invite_token(ChildId, TokenCreatorInChildGroup)
        )
    end,

    EnvSetUpFun = fun() ->
        ParentId = ParentCreateFun(),
        TokenCreatorId = ozt_users:create(),
        ozt_groups:add_user(ParentId, TokenCreatorId, [?GROUP_ADD_CHILD]),
        Token = ozt_groups:create_one_shot_group_invite_token(ParentId, TokenCreatorId),
        #{
            parentId => ParentId,
            token => ozt_tokens:ensure_serialized(Token),
            tokenId => Token
        }
    end,

    VerifyEndFun = fun(ClientAuthorized, #{parentId := ParentId, token := Token} = _Env, _) ->
        ShouldSucceed = ClientAuthorized andalso ExpOutcome == allowed,
        ?assertEqual(ShouldSucceed, lists:member(ChildId, ozt_groups:get_children(ParentId))),
        ShouldSucceed andalso ozt_tokens:assert_invite_token_usage_limit_reached(Token, true)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, UserWithPrivilege}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutPrivilege},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, ChildId, <<"/parents/join">>],
            expected_code = case ExpOutcome of
                blocked_by_protection -> ?HTTP_403_FORBIDDEN;
                allowed -> ?HTTP_201_CREATED
            end,
            expected_headers = case ExpOutcome of
                blocked_by_protection ->
                    undefined;
                allowed ->
                    ?OK_ENV(fun(#{parentId := ParentId} = _Env, _) ->
                        fun(#{?HDR_LOCATION := Location} = _Headers) ->
                            ExpLocation = ?URL(Config,
                                [<<"/groups/">>, ChildId, <<"/parents/">>, ParentId]
                            ),
                            ?assertMatch(ExpLocation, Location),
                            true
                        end
                    end)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = join_group,
            args = [auth, ChildId, data],
            expected_result = case ExpOutcome of
                blocked_by_protection ->
                    ?ERROR_REASON(?ERR_PROTECTED_GROUP);
                allowed ->
                    ?OK_ENV(fun(#{parentId := ParentId} = _Env, _) ->
                        ?OK_BINARY(ParentId)
                    end)
            end
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
                {<<"token">>, <<"">>, ?ERR_BAD_VALUE_EMPTY(<<"token">>)},
                {<<"token">>, CreateTokenForItselfFun, case ozt_groups:is_protected(ChildId) of
                    true -> ?ERR_PROTECTED_GROUP;
                    false -> ?ERR_CANNOT_ADD_RELATION_TO_SELF
                end},
                {<<"token">>, 1234, ?ERR_BAD_VALUE_TOKEN(<<"token">>, ?ERR_BAD_TOKEN)},
                {<<"token">>, <<"123qwe">>, ?ERR_BAD_VALUE_TOKEN(<<"token">>, ?ERR_BAD_TOKEN)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check that token is not consumed upon failed operation
    AnotherParentId = ParentCreateFun(),
    AnotherTokenCreatorId = ozt_users:create(),
    ozt_groups:add_user(AnotherParentId, AnotherTokenCreatorId, [?GROUP_ADD_CHILD]),
    AnotherToken = ozt_groups:create_one_shot_group_invite_token(AnotherParentId, AnotherTokenCreatorId),

    ozt_groups:run_without_protection(ChildId, fun() -> ozt_groups:add_child(AnotherParentId, ChildId) end),

    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, UserWithPrivilege}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, ChildId, <<"/parents/join">>],
            expected_code = case ExpOutcome of
                blocked_by_protection -> ?HTTP_403_FORBIDDEN;
                allowed -> ?HTTP_409_CONFLICT
            end
        },
        logic_spec = LogicSpec = #logic_spec{
            module = group_logic,
            function = join_group,
            args = [auth, ChildId, data],
            expected_result = ?ERROR_REASON(case ExpOutcome of
                blocked_by_protection -> ?ERR_PROTECTED_GROUP;
                allowed -> ?ERR_RELATION_ALREADY_EXISTS(od_group, ChildId, od_group, AnotherParentId)
            end)
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [ozt_tokens:ensure_serialized(AnotherToken)]}
        }
    },
    VerifyEndFun1 = fun(Token) ->
        fun(_ShouldSucceed, _Env, _) ->
            ozt_tokens:assert_invite_token_usage_limit_reached(Token, false)
        end
    end,
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec1, undefined, undefined, VerifyEndFun1(AnotherToken)
    )),

    % check that it's not possible for a group to join itself
    TheLastToken = ozt_groups:create_one_shot_group_invite_token(ChildId, TokenCreatorInChildGroup),
    ApiTestSpec2 = ApiTestSpec1#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            expected_code = case ozt_groups:is_protected(ChildId) of
                true -> ?HTTP_403_FORBIDDEN;
                false -> ?HTTP_400_BAD_REQUEST
            end
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?ERROR_REASON(case ozt_groups:is_protected(ChildId) of
                true -> ?ERR_PROTECTED_GROUP;
                false -> ?ERR_CANNOT_ADD_RELATION_TO_SELF
            end)
        },
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [ozt_tokens:ensure_serialized(TheLastToken)]}
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, undefined, undefined, VerifyEndFun1(TheLastToken)
    )).


leave_parent_test(Config) ->
    UserWithPrivilege = ozt_users:create(),
    UserWithoutPrivilege = ozt_users:create(),

    leave_parent_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create/0
    ),
    leave_parent_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create_protected/0, fun ozt_groups:create/0
    ),
    leave_parent_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create_protected/0
    ),
    leave_parent_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        blocked_by_protection, fun ozt_groups:create_protected/0, fun ozt_groups:create_protected/0
    ).


leave_parent_test_base(Config, UserWithPrivilege, UserWithoutPrivilege, ExpOutcome, ChildCreateFun, ParentCreateFun) ->
    NonAdmin = ozt_users:create(),

    ChildId = ChildCreateFun(),
    ozt_groups:add_user(ChildId, UserWithPrivilege, [?GROUP_LEAVE_PARENT]),
    ozt_groups:add_user(ChildId, UserWithoutPrivilege, privileges:group_admin() -- [?GROUP_LEAVE_PARENT]),

    EnvSetUpFun = fun() ->
        ParentId = ParentCreateFun(),
        ozt_groups:run_without_protection(ParentId, fun() -> ozt_groups:add_child(ParentId, ChildId) end),
        #{parentId => ParentId}
    end,

    VerifyEndFun = fun(ClientAuthorized, #{parentId := ParentId} = _Env, _) ->
        ShouldSucceed = ClientAuthorized andalso ExpOutcome == allowed,
        ?assertEqual(not ShouldSucceed, lists:member(ChildId, ozt_groups:get_children(ParentId)))
    end,

    DeleteEntityFun = fun(#{parentId := ParentId} = _Env) ->
        ozt_groups:run_without_protection(ParentId, fun() -> ozt_groups:remove_child(ParentId, ChildId) end)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, UserWithPrivilege},
                {admin, [?OZ_GROUPS_REMOVE_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutPrivilege}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, ChildId, <<"/parents/">>, parentId],
            expected_code = case ExpOutcome of
                blocked_by_protection -> ?HTTP_403_FORBIDDEN;
                allowed -> ?HTTP_204_NO_CONTENT
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_group,
            args = [auth, ChildId, parentId],
            expected_result = case ExpOutcome of
                blocked_by_protection -> ?ERROR_REASON(?ERR_PROTECTED_GROUP);
                allowed -> ?OK_RES
            end
        }
        % TODO VFS-4520 Tests for GraphSync API
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
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    GroupData = #{<<"name">> => ?GROUP_NAME2, <<"type">> => ?GROUP_TYPE2},
    {ok, ParentGroup} = oz_test_utils:create_group(Config, ?ROOT, GroupData),
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
            expected_body = api_test_expect:protected_group(rest, ParentGroup, GroupData, ?SUB(nobody))
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_parent,
            args = [auth, Group, ParentGroup],
            expected_result = api_test_expect:protected_group(logic, ParentGroup, GroupData, ?SUB(nobody))
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = ParentGroup,
                aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_GROUP(Group),
            expected_result_op = api_test_expect:protected_group(gs, ParentGroup, GroupData, ?SUB(nobody))
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
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO VFS-4520 Tests for GraphSync API
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

    lists:foreach(fun({GroupId, GroupData}) ->
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
                path = [<<"/groups/">>, G1, <<"/effective_parents/">>, GroupId],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:protected_group(rest, GroupId, GroupData, ?SUB(nobody))
            },
            logic_spec = #logic_spec{
                module = group_logic,
                function = get_eff_parent,
                args = [auth, G1, GroupId],
                expected_result = api_test_expect:protected_group(logic, GroupId, GroupData, ?SUB(nobody))
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_group, id = GroupId,
                    aspect = instance, scope = protected
                },
                auth_hint = ?THROUGH_GROUP(G1),
                expected_result_op = api_test_expect:protected_group(gs, GroupId, GroupData, ?SUB(nobody))
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffParents).


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
