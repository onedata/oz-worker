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
-include_lib("ctool/include/errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
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
        % TODO VFS-4520 Tests for GraphSync API
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
                {user, U2},
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_GROUPS_ADD_RELATIONSHIPS]}
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
        % TODO VFS-4520 Tests for GraphSync API
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

                fun(#{?HDR_LOCATION := Location} = _Headers) ->
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
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [<<"name">>],
            optional = [<<"type">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
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


get_child_details_test(Config) ->
    % create group with 3 users:
    %   PrivilegedMember gets the GROUP_VIEW privilege
    %   UnprivilegedMember gets all remaining privileges
    %   UnprivilegedMemberFromTheGroup does not get the GROUP_VIEW privilege, but belongs to the child group
    {SubjectGroupId, UnprivilegedMember, PrivilegedMember} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    GroupData = #{<<"name">> => ?GROUP_NAME2, <<"type">> => ?GROUP_TYPE2},
    {ok, ChildGroupId} = oz_test_utils:create_group(Config, ?ROOT, GroupData),
    oz_test_utils:group_add_group(Config, SubjectGroupId, ChildGroupId),

    UnprivilegedMemberFromTheChildGroup = ozt_users:create(),
    ozt_groups:add_user(SubjectGroupId, UnprivilegedMemberFromTheChildGroup, ?RAND_SUBLIST(privileges:group_admin() -- [?SPACE_VIEW])),
    ozt_groups:add_user(ChildGroupId, UnprivilegedMemberFromTheChildGroup, ?RAND_SUBLIST(privileges:group_admin())),
    ozt:reconcile_entity_graph(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, PrivilegedMember},
                {user, UnprivilegedMemberFromTheChildGroup},
                {admin, [?OZ_GROUPS_VIEW]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UnprivilegedMember}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, SubjectGroupId, <<"/children/">>, ChildGroupId],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:shared_group(rest, ChildGroupId, GroupData)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_child,
            args = [auth, SubjectGroupId, ChildGroupId],
            expected_result = api_test_expect:shared_group(logic, ChildGroupId, GroupData)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = ChildGroupId, aspect = instance, scope = shared
            },
            auth_hint = ?THROUGH_GROUP(SubjectGroupId),
            expected_result_op = api_test_expect:shared_group(gs, ChildGroupId, GroupData)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


add_child_test(Config) ->
    UserWithPrivileges = ozt_users:create(),
    UserNoAddChildPriv = ozt_users:create(),
    UserNoAddParentPriv = ozt_users:create(),

    add_child_test_base(
        Config, UserWithPrivileges, UserNoAddChildPriv, UserNoAddParentPriv,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create/0
    ),
    add_child_test_base(
        Config, UserWithPrivileges, UserNoAddChildPriv, UserNoAddParentPriv,
        allowed, fun ozt_groups:create_protected/0, fun ozt_groups:create/0
    ),
    add_child_test_base(
        Config, UserWithPrivileges, UserNoAddChildPriv, UserNoAddParentPriv,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create_protected/0
    ),
    add_child_test_base(
        Config, UserWithPrivileges, UserNoAddChildPriv, UserNoAddParentPriv,
        blocked_by_protection, fun ozt_groups:create_protected/0, fun ozt_groups:create_protected/0
    ).


add_child_test_base(
    Config, UserWithPrivileges, UserNoAddChildPriv, UserNoAddParentPriv,
    ExpOutcome, ChildCreateFun, ParentCreateFun
) ->
    NonAdmin = ozt_users:create(),

    ParentId = ParentCreateFun(),
    ChildId = ChildCreateFun(),
    ozt_groups:add_user(ParentId, UserWithPrivileges, [?GROUP_ADD_CHILD]),
    ozt_groups:add_user(ChildId, UserWithPrivileges, [?GROUP_ADD_PARENT]),

    ozt_groups:add_user(ParentId, UserNoAddChildPriv, privileges:group_admin() -- [?GROUP_ADD_CHILD]),
    ozt_groups:add_user(ChildId, UserNoAddChildPriv, privileges:group_admin()),

    ozt_groups:add_user(ParentId, UserNoAddParentPriv, privileges:group_admin()),
    ozt_groups:add_user(ChildId, UserNoAddParentPriv, privileges:group_admin() -- [?GROUP_ADD_PARENT]),

    VerifyEndFun = fun(ClientAuthorized, _, _) ->
        ShouldSucceed = ClientAuthorized andalso ExpOutcome == allowed,
        ?assertEqual(ShouldSucceed, lists:member(ChildId, ozt_groups:get_children(ParentId)))
    end,

    EnvTearDownFun = fun(_) ->
        lists:member(ChildId, ozt_groups:get_children(ParentId)) andalso ozt_groups:remove_child(ParentId, ChildId)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS]},
                {user, UserWithPrivileges}
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
            path = [<<"/groups/">>, ParentId, <<"/children/">>, ChildId],
            expected_code = case ExpOutcome of
                blocked_by_protection -> ?HTTP_403_FORBIDDEN;
                allowed -> ?HTTP_201_CREATED
            end,
            expected_headers = case ExpOutcome of
                blocked_by_protection ->
                    undefined;
                allowed ->
                    fun(#{?HDR_LOCATION := Location} = _Headers) ->
                        ExpLocation = ?URL(Config, [<<"/groups/">>, ParentId, <<"/children/">>, ChildId]),
                        ?assertEqual(ExpLocation, Location),
                        true
                    end
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = add_group,
            args = [auth, ParentId, ChildId, data],
            expected_result = case ExpOutcome of
                blocked_by_protection -> ?ERROR_REASON(?ERR_PROTECTED_GROUP);
                allowed -> ?OK_BINARY(ChildId)
            end
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [],
            correct_values = #{},
            bad_values = []
        }
    },

    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, EnvTearDownFun, VerifyEndFun
    )).


add_child_with_privileges_test(Config) ->
    UserWithPrivileges = ozt_users:create(),
    UserNoAddChildPriv = ozt_users:create(),
    UserNoAddParentPriv = ozt_users:create(),
    UserNoSetPrivilegesPriv = ozt_users:create(),

    add_child_with_privileges_test_base(
        Config, UserWithPrivileges, UserNoAddChildPriv, UserNoAddParentPriv, UserNoSetPrivilegesPriv,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create/0
    ),
    add_child_with_privileges_test_base(
        Config, UserWithPrivileges, UserNoAddChildPriv, UserNoAddParentPriv, UserNoSetPrivilegesPriv,
        allowed, fun ozt_groups:create_protected/0, fun ozt_groups:create/0
    ),
    add_child_with_privileges_test_base(
        Config, UserWithPrivileges, UserNoAddChildPriv, UserNoAddParentPriv, UserNoSetPrivilegesPriv,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create_protected/0
    ),
    add_child_with_privileges_test_base(
        Config, UserWithPrivileges, UserNoAddChildPriv, UserNoAddParentPriv, UserNoSetPrivilegesPriv,
        blocked_by_protection, fun ozt_groups:create_protected/0, fun ozt_groups:create_protected/0
    ).


add_child_with_privileges_test_base(
    Config, UserWithPrivileges, UserNoAddChildPriv, UserNoAddParentPriv, UserNoSetPrivilegesPriv,
    ExpOutcome, ChildCreateFun, ParentCreateFun
) ->
    NonAdmin = ozt_users:create(),

    ParentId = ParentCreateFun(),
    ChildId = ChildCreateFun(),
    ozt_groups:add_user(ParentId, UserWithPrivileges, [?GROUP_ADD_CHILD, ?GROUP_SET_PRIVILEGES]),
    ozt_groups:add_user(ChildId, UserWithPrivileges, [?GROUP_ADD_PARENT]),

    ozt_groups:add_user(ParentId, UserNoAddChildPriv, privileges:group_admin() -- [?GROUP_ADD_CHILD]),
    ozt_groups:add_user(ChildId, UserNoAddChildPriv, privileges:group_admin()),

    ozt_groups:add_user(ParentId, UserNoAddParentPriv, privileges:group_admin()),
    ozt_groups:add_user(ChildId, UserNoAddParentPriv, privileges:group_admin() -- [?GROUP_ADD_PARENT]),

    ozt_groups:add_user(ParentId, UserNoSetPrivilegesPriv, privileges:group_admin() -- [?GROUP_SET_PRIVILEGES]),
    ozt_groups:add_user(ChildId, UserNoSetPrivilegesPriv, privileges:group_admin()),

    VerifyEndFun = fun(ClientAuthorized, _, Data) ->
        ShouldSucceed = ClientAuthorized andalso ExpOutcome == allowed,
        ?assertEqual(ShouldSucceed, lists:member(ChildId, ozt_groups:get_children(ParentId))),
        ShouldSucceed andalso ?assertEqual(
            lists:sort(maps:get(<<"privileges">>, Data)),
            ozt_groups:get_child_privileges(ParentId, ChildId)
        )
    end,

    EnvTearDownFun = fun(_) ->
        lists:member(ChildId, ozt_groups:get_children(ParentId)) andalso ozt_groups:remove_child(ParentId, ChildId)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_SET_PRIVILEGES, ?OZ_GROUPS_SET_PRIVILEGES]},
                {user, UserWithPrivileges}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserNoAddChildPriv},
                {user, UserNoAddParentPriv},
                {user, UserNoSetPrivilegesPriv}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/groups/">>, ParentId, <<"/children/">>, ChildId],
            expected_code = case ExpOutcome of
                blocked_by_protection -> ?HTTP_403_FORBIDDEN;
                allowed -> ?HTTP_201_CREATED
            end,
            expected_headers = case ExpOutcome of
                blocked_by_protection ->
                    undefined;
                allowed ->
                    fun(#{?HDR_LOCATION := Location} = _Headers) ->
                        ExpLocation = ?URL(Config, [<<"/groups/">>, ParentId, <<"/children/">>, ChildId]),
                        ?assertEqual(ExpLocation, Location),
                        true
                    end
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = add_group,
            args = [auth, ParentId, ChildId, data],
            expected_result = case ExpOutcome of
                blocked_by_protection -> ?ERROR_REASON(?ERR_PROTECTED_GROUP);
                allowed -> ?OK_BINARY(ChildId)
            end
        },
        % TODO VFS-4520 Tests for GraphSync API
        data_spec = #data_spec{
            required = [<<"privileges">>],
            correct_values = #{
                <<"privileges">> => [
                    [?GROUP_ADD_PARENT, ?GROUP_REMOVE_CHILD],
                    [?GROUP_ADD_USER, ?GROUP_VIEW]
                ]
            },
            bad_values = [
                {<<"privileges">>, <<"">>, ?ERR_BAD_VALUE_LIST_OF_STRINGS(<<"privileges">>)}
            ]
        }
    },

    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, EnvTearDownFun, VerifyEndFun
    )).


remove_child_test(Config) ->
    UserWithPrivilege = ozt_users:create(),
    UserWithoutPrivilege = ozt_users:create(),

    remove_child_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create/0
    ),
    remove_child_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create_protected/0, fun ozt_groups:create/0
    ),
    remove_child_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create_protected/0
    ),
    remove_child_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        blocked_by_protection, fun ozt_groups:create_protected/0, fun ozt_groups:create_protected/0
    ).


remove_child_test_base(Config, UserWithPrivilege, UserWithoutPrivilege, ExpOutcome, ChildCreateFun, ParentCreateFun) ->
    NonAdmin = ozt_users:create(),

    ParentId = ParentCreateFun(),
    ozt_groups:add_user(ParentId, UserWithPrivilege, [?GROUP_REMOVE_CHILD]),
    ozt_groups:add_user(ParentId, UserWithoutPrivilege, privileges:group_admin() -- [?GROUP_REMOVE_CHILD]),

    EnvSetUpFun = fun() ->
        ChildId = ChildCreateFun(),
        ozt_groups:run_without_protection(ParentId, fun() -> ozt_groups:add_child(ParentId, ChildId) end),
        #{childId => ChildId}
    end,

    DeleteEntityFun = fun(#{childId := ChildId} = _Env) ->
        ozt_groups:run_without_protection(ParentId, fun() -> ozt_groups:remove_child(ParentId, ChildId) end)
    end,

    VerifyEndFun = fun(ClientAuthorized, #{childId := ChildId} = _Env, _) ->
        ShouldSucceed = ClientAuthorized andalso ExpOutcome == allowed,
        ?assertEqual(not ShouldSucceed, lists:member(ChildId, ozt_groups:get_children(ParentId)))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_REMOVE_RELATIONSHIPS]},
                {user, UserWithPrivilege}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutPrivilege},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, ParentId, <<"/children/">>, childId],
            expected_code = case ExpOutcome of
                blocked_by_protection -> ?HTTP_403_FORBIDDEN;
                allowed -> ?HTTP_204_NO_CONTENT
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = remove_group,
            args = [auth, ParentId, childId],
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


get_child_privileges_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
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
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?GROUP_VIEW_PRIVILEGES
    ])).


update_child_privileges_test(Config) ->
    UserWithPrivilege = ozt_users:create(),
    UserWithoutPrivilege = ozt_users:create(),

    update_child_privileges_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create/0
    ),
    update_child_privileges_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create_protected/0, fun ozt_groups:create/0
    ),
    update_child_privileges_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        allowed, fun ozt_groups:create/0, fun ozt_groups:create_protected/0
    ),
    update_child_privileges_test_base(
        Config, UserWithPrivilege, UserWithoutPrivilege,
        blocked_by_protection, fun ozt_groups:create_protected/0, fun ozt_groups:create_protected/0
    ).


update_child_privileges_test_base(
    Config, UserWithPrivilege, UserWithoutPrivilege, ExpOutcome, ChildCreateFun, ParentCreateFun
) ->
    NonAdmin = ozt_users:create(),

    ParentId = ParentCreateFun(),
    ChildId = ChildCreateFun(),
    ozt_groups:run_without_protection(ParentId, fun() -> ozt_groups:add_child(ParentId, ChildId) end),

    ozt_groups:add_user(ParentId, UserWithPrivilege, [?GROUP_SET_PRIVILEGES]),
    ozt_groups:add_user(ParentId, UserWithoutPrivilege, privileges:group_admin() -- [?GROUP_SET_PRIVILEGES]),

    % User whose eff privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to update group privileges and sometimes not)
    AffectedUserId = ozt_users:create(),
    ozt_groups:add_user(ChildId, AffectedUserId),

    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        ozt_groups:run_without_protection(ParentId, fun() ->
            ozt_groups:update_child_privileges(ParentId, ChildId, PrivsToGrant, PrivsToRevoke)
        end)
    end,

    GetPrivsFun = fun() ->
        ozt_groups:get_child_privileges(ParentId, ChildId)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, UserWithPrivilege},
                {admin, [?OZ_GROUPS_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserWithoutPrivilege}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [
                <<"/groups/">>, ParentId, <<"/children/">>, ChildId, <<"/privileges">>
            ],
            expected_code = case ExpOutcome of
                blocked_by_protection -> ?HTTP_403_FORBIDDEN;
                allowed -> ?HTTP_204_NO_CONTENT
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = update_child_privileges,
            args = [auth, ParentId, ChildId, data],
            expected_result = case ExpOutcome of
                blocked_by_protection -> ?ERROR_REASON(?ERR_PROTECTED_GROUP);
                allowed -> ?OK_RES
            end
        },
        data_spec = #data_spec{
            at_least_one = [<<"grant">>, <<"revoke">>],
            correct_values = #{
                <<"grant">> => [?RAND_SUBLIST(privileges:group_admin())],
                <<"revoke">> => [?RAND_SUBLIST(privileges:group_admin())]
            }
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    case ExpOutcome of
        allowed ->
            ?assert(api_test_scenarios:run_scenario(update_privileges, [
                Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, privileges:group_admin(),
                {user, AffectedUserId}, ?GROUP_SET_PRIVILEGES
            ]));
        blocked_by_protection ->
            % cannot use the scenario here as the update always fails
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end.


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
        % TODO VFS-4520 Tests for GraphSync API
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
        [{SubjectGroupId, _} | EffChildren], _Users
    } = api_test_scenarios:create_eff_child_groups_env(Config),

    {ok, UnprivilegedMember} = oz_test_utils:create_user(Config),
    {ok, PrivilegedMember} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllGroupPrivs = privileges:group_privileges(),
    {ok, UnprivilegedMember} = oz_test_utils:group_add_user(Config, SubjectGroupId, UnprivilegedMember),
    oz_test_utils:group_set_user_privileges(Config, SubjectGroupId, UnprivilegedMember,
        AllGroupPrivs -- [?GROUP_VIEW], [?GROUP_VIEW]
    ),
    {ok, PrivilegedMember} = oz_test_utils:group_add_user(Config, SubjectGroupId, PrivilegedMember),
    oz_test_utils:group_set_user_privileges(Config, SubjectGroupId, PrivilegedMember,
        [?GROUP_VIEW], AllGroupPrivs -- [?GROUP_VIEW]
    ),

    UnprivilegedMemberFromTheChildGroup = ozt_users:create(),
    ozt_groups:add_user(SubjectGroupId, UnprivilegedMemberFromTheChildGroup, ?RAND_SUBLIST(privileges:group_admin() -- [?SPACE_VIEW])),
    lists:foreach(fun({GroupId, _}) ->
        ozt_groups:add_user(GroupId, UnprivilegedMemberFromTheChildGroup, ?RAND_SUBLIST(privileges:group_admin()))
    end, EffChildren),
    ozt:reconcile_entity_graph(),

    lists:foreach(fun({ChildGroupId, GroupData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_GROUPS_VIEW]},
                    {user, PrivilegedMember},
                    {user, UnprivilegedMemberFromTheChildGroup}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, UnprivilegedMember},
                    {user, NonAdmin}
                ]
            },
            rest_spec = #rest_spec{
                method = get,
                path = [
                    <<"/groups/">>, SubjectGroupId, <<"/effective_children/">>, ChildGroupId
                ],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:shared_group(rest, ChildGroupId, GroupData)
            },
            logic_spec = #logic_spec{
                module = group_logic,
                function = get_eff_child,
                args = [auth, SubjectGroupId, ChildGroupId],
                expected_result = api_test_expect:shared_group(logic, ChildGroupId, GroupData)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_group, id = ChildGroupId,
                    aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_GROUP(SubjectGroupId),
                expected_result_op = api_test_expect:shared_group(gs, ChildGroupId, GroupData)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffChildren).


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
    % should not be listed in client spec (he will sometimes have privilege
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
        % TODO VFS-4520 Tests for GraphSync API
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
            #{<<"type">> => gri:serialize_type(Type), <<"id">> => Id}
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
