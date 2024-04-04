%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning space users API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(space_users_api_test_SUITE).
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
    add_user_test/1,
    add_user_with_privileges_test/1,
    create_user_invite_token_test/1,
    remove_user_test/1,
    list_users_test/1,
    get_user_test/1,

    get_owners_test/1,
    add_owner_test/1,
    remove_owner_test/1,

    get_user_privileges_test/1,
    update_user_privileges_test/1,

    list_eff_users_test/1,
    get_eff_user_test/1,
    get_eff_user_privileges_test/1,
    get_eff_user_membership_intermediaries/1
]).

all() ->
    ?ALL([
        add_user_test,
        add_user_with_privileges_test,
        create_user_invite_token_test,
        remove_user_test,
        list_users_test,
        get_user_test,

        get_owners_test,
        add_owner_test,
        remove_owner_test,

        get_user_privileges_test,
        update_user_privileges_test,

        list_eff_users_test,
        get_eff_user_test,
        get_eff_user_privileges_test,
        get_eff_user_membership_intermediaries
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


add_user_test(Config) ->
    {ok, Creator} = oz_test_utils:create_user(Config),
    {ok, EffectiveUser} = oz_test_utils:create_user(Config),
    {ok, EffectiveUserWithoutAddUserPriv} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(Creator), ?SPACE_NAME1),

    % EffectiveUser belongs to space S1 effectively via SubGroup1, with the
    % effective privilege to ADD_USER, so he should be able to add himself to the space
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:space_add_group(Config, S1, SubGroup1),
    oz_test_utils:space_set_group_privileges(Config, S1, SubGroup1, [?SPACE_ADD_USER], []),

    % EffectiveUserWithoutAddUserPriv belongs to group S1 effectively via SubGroup2,
    % but without the effective privilege to ADD_USER, so he should NOT be able
    % to add himself to the space
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutAddUserPriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:space_add_group(Config, S1, SubGroup2),

    lists:foreach(fun({ClientClassification, SubjectUser}) ->
        VerifyEndFun = fun
            (true = _ShouldSucceed, _, _) ->
                {ok, Users} = oz_test_utils:space_get_users(Config, S1),
                ?assert(lists:member(SubjectUser, Users)),
                oz_test_utils:space_remove_user(Config, S1, SubjectUser);
            (false = _ShouldSucceed, _, _) ->
                {ok, Users} = oz_test_utils:space_get_users(Config, S1),
                ?assertNot(lists:member(SubjectUser, Users))
        end,

        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = lists:flatten([
                    root,
                    {admin, [?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS]},
                    case ClientClassification of
                        correct -> {user, SubjectUser};
                        forbidden -> []
                    end
                ]),
                unauthorized = [nobody],
                forbidden = lists:flatten([
                    {user, Creator},
                    {user, NonAdmin},
                    case ClientClassification of
                        correct -> [];
                        forbidden -> {user, SubjectUser}
                    end
                ])
            },
            rest_spec = #rest_spec{
                method = put,
                path = [<<"/spaces/">>, S1, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_201_CREATED,
                expected_headers = fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    ExpLocation = ?URL(Config, [<<"/spaces/">>, S1, <<"/users/">>, SubjectUser]),
                    ?assertEqual(ExpLocation, Location),
                    true
                end
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = add_user,
                args = [auth, S1, SubjectUser, data],
                expected_result = ?OK_BINARY(SubjectUser)
            },
            % TODO VFS-4520 Tests for GraphSync API
            data_spec = #data_spec{
                required = [],
                correct_values = #{},
                bad_values = []
            }
        },
        ?assert(api_test_utils:run_tests(
            Config, ApiTestSpec, undefined, undefined, VerifyEndFun
        ))
    end, [{correct, EffectiveUser}, {forbidden, EffectiveUserWithoutAddUserPriv}]).


add_user_with_privileges_test(Config) ->
    {ok, Creator} = oz_test_utils:create_user(Config),
    {ok, EffectiveUser} = oz_test_utils:create_user(Config),
    {ok, EffectiveUserWithoutAddUserPriv} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(Creator), ?SPACE_NAME1),

    AllPrivs = privileges:space_privileges(),

    % EffectiveUser belongs to space S1 effectively via SubGroup1, with the
    % effective privilege to ADD_USER, so he should be able to add himself to the space
    {ok, SubGroup1} = oz_test_utils:create_group(Config, ?USER(EffectiveUser), ?GROUP_NAME2),
    {ok, SubGroup1} = oz_test_utils:space_add_group(Config, S1, SubGroup1),
    oz_test_utils:space_set_group_privileges(Config, S1, SubGroup1, [?SPACE_ADD_USER, ?SPACE_SET_PRIVILEGES], []),

    % EffectiveUserWithoutAddUserPriv belongs to group S1 effectively via SubGroup2,
    % but without the effective privilege to ADD_USER, so he should NOT be able
    % to add himself to the space
    {ok, SubGroup2} = oz_test_utils:create_group(Config, ?USER(EffectiveUserWithoutAddUserPriv), ?GROUP_NAME2),
    {ok, SubGroup2} = oz_test_utils:space_add_group(Config, S1, SubGroup2),

    lists:foreach(fun({ClientClassification, SubjectUser}) ->
        VerifyEndFun = fun
            (true = _ShouldSucceed, _, Data) ->
                Privs = lists:sort(maps:get(<<"privileges">>, Data)),
                {ok, ActualPrivs} = oz_test_utils:space_get_user_privileges(
                    Config, S1, SubjectUser
                ),
                ?assertEqual(Privs, lists:sort(ActualPrivs)),
                oz_test_utils:space_remove_user(Config, S1, SubjectUser);
            (false = ShouldSucceed, _, _) ->
                {ok, Users} = oz_test_utils:space_get_users(Config, S1),
                ?assertEqual(lists:member(SubjectUser, Users), ShouldSucceed)
        end,

        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = lists:flatten([
                    root,
                    {admin, [?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS, ?OZ_SPACES_SET_PRIVILEGES]},
                    case ClientClassification of
                        correct -> {user, SubjectUser};
                        forbidden -> []
                    end
                ]),
                unauthorized = [nobody],
                forbidden = lists:flatten([
                    {user, Creator},
                    {user, NonAdmin},
                    case ClientClassification of
                        correct -> [];
                        forbidden -> {user, SubjectUser}
                    end
                ])
            },
            rest_spec = #rest_spec{
                method = put,
                path = [<<"/spaces/">>, S1, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_201_CREATED,
                expected_headers = fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    ExpLocation = ?URL(Config, [<<"/spaces/">>, S1, <<"/users/">>, SubjectUser]),
                    ?assertEqual(ExpLocation, Location),
                    true
                end
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = add_user,
                args = [auth, S1, SubjectUser, data],
                expected_result = ?OK_BINARY(SubjectUser)
            },
            % TODO VFS-4520 Tests for GraphSync API
            data_spec = #data_spec{
                required = [<<"privileges">>],
                correct_values = #{
                    <<"privileges">> => [
                        [?SPACE_UPDATE, ?SPACE_REMOVE_GROUP],
                        [?SPACE_WRITE_DATA, ?SPACE_VIEW]
                    ]
                },
                bad_values = [
                    {<<"privileges">>, <<"">>,
                        ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>)},
                    {<<"privileges">>, [?SPACE_VIEW, ?GROUP_VIEW],
                        ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"privileges">>, AllPrivs)}
                ]
            }
        },
        ?assert(api_test_utils:run_tests(
            Config, ApiTestSpec, undefined, undefined, VerifyEndFun
        ))
    end, [{correct, EffectiveUser}, {forbidden, EffectiveUserWithoutAddUserPriv}]).


create_user_invite_token_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_INVITE_USER_TOKEN privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_ADD_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_SPACES_ADD_RELATIONSHIPS]},
                {user, Owner},
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
            path = [<<"/spaces/">>, S1, <<"/users/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = create_user_invite_token,
            args = [auth, S1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


remove_user_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_REMOVE_USER privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_REMOVE_USER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, U3} = oz_test_utils:create_user(Config),
        {ok, U3} = oz_test_utils:space_add_user(Config, S1, U3),
        #{userId => U3}
    end,
    DeleteEntityFun = fun(#{userId := User} = _Env) ->
        oz_test_utils:space_remove_user(Config, S1, User)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := User} = _Env, _) ->
        {ok, Users} = oz_test_utils:space_get_users(Config, S1),
        ?assertEqual(lists:member(User, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS]},
                {user, Owner},
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
            path = [<<"/spaces/">>, S1, <<"/users/">>, userId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = remove_user,
            args = [auth, S1, userId],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_users_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_VIEW privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, U3} = oz_test_utils:space_add_user(Config, S1, U3),
    ExpUsers = [Owner, U1, U2, U3],

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    oz_test_utils:support_space_by_provider(Config, P1, S1),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_LIST_RELATIONSHIPS]},
                {user, Owner},
                {user, U2},
                {user, U3},
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_users,
            args = [auth, S1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_user_test(Config) ->
    CreatorData = #{<<"fullName">> => <<"creator">>, <<"username">> => <<"creator">>},
    {ok, Creator} = oz_test_utils:create_user(Config, CreatorData),
    {ok, MemberWithViewPrivs} = oz_test_utils:create_user(Config),
    {ok, MemberWithoutViewPrivs} = oz_test_utils:create_user(Config),
    MemberData = #{<<"fullName">> => <<"member">>, <<"username">> => <<"member">>},
    {ok, Member} = oz_test_utils:create_user(Config, MemberData),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, Space} = oz_test_utils:create_space(Config, ?USER(Creator), ?SPACE_NAME1),
    {ok, _} = oz_test_utils:space_add_user(Config, Space, MemberWithViewPrivs),
    {ok, _} = oz_test_utils:space_add_user(Config, Space, MemberWithoutViewPrivs),
    {ok, _} = oz_test_utils:space_add_user(Config, Space, Member),

    oz_test_utils:space_set_user_privileges(Config, Space, MemberWithViewPrivs, [?SPACE_VIEW], []),
    oz_test_utils:space_set_user_privileges(Config, Space, MemberWithoutViewPrivs, [], [?SPACE_VIEW]),

    % Shared data about creator should be available even if he is not longer in the space
    oz_test_utils:space_add_owner(Config, Space, Member),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    oz_test_utils:space_remove_user(Config, Space, Creator),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    oz_test_utils:support_space_by_provider(Config, P1, Space),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    lists:foreach(fun({SubjectUser, UserData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_VIEW]},
                    {user, SubjectUser},
                    {user, MemberWithViewPrivs},
                    {provider, P1, P1Token}
                ] ++ case SubjectUser of
                    % Every member of the space should be able to see the creator details
                    Creator -> [{user, MemberWithoutViewPrivs}];
                    _ -> []
                end,
                unauthorized = [nobody],
                forbidden = [
                    {user, NonAdmin}
                ] ++ case SubjectUser of
                    Creator -> [];
                    _ -> [{user, MemberWithoutViewPrivs}]
                end
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/spaces/">>, Space, <<"/users/">>, SubjectUser],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:shared_user(rest, SubjectUser, UserData)
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = get_user,
                args = [auth, Space, SubjectUser],
                expected_result = api_test_expect:shared_user(logic, SubjectUser, UserData)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = SubjectUser, aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_SPACE(Space),
                expected_result_op = api_test_expect:shared_user(gs, SubjectUser, UserData)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
    end, [{Creator, CreatorData}, {Member, MemberData}]).


get_owners_test(Config) ->
    {ok, Owner} = oz_test_utils:create_user(Config),
    {ok, UserWithView} = oz_test_utils:create_user(Config),
    {ok, UserWithoutView} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, Space} = oz_test_utils:create_space(Config, ?USER(Owner)),
    oz_test_utils:space_add_user(Config, Space, UserWithView),
    oz_test_utils:space_add_user(Config, Space, UserWithoutView),
    oz_test_utils:space_set_user_privileges(Config, Space, UserWithView, [?SPACE_VIEW], []),
    oz_test_utils:space_set_user_privileges(Config, Space, UserWithoutView, [], [?SPACE_VIEW]),

    {ok, OtherOwner} = oz_test_utils:create_space_owner(Config, Space),

    ExpOwners = [Owner, OtherOwner],

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    oz_test_utils:support_space_by_provider(Config, P1, Space),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW]},
                {user, Owner},
                {user, UserWithView},
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutView},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, Space, <<"/owners">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpOwners}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_owners,
            args = [auth, Space],
            expected_result = ?OK_LIST(ExpOwners)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


add_owner_test(Config) ->
    {ok, Owner} = oz_test_utils:create_user(Config),
    {ok, RegularMember} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, Space} = oz_test_utils:create_space(Config, ?USER(Owner)),
    oz_test_utils:space_add_user(Config, Space, RegularMember),

    EnvSetUpFun = fun() ->
        % randomly choose direct or effective member to as candidate to become a new owner
        Candidate = case rand:uniform(2) of
            1 ->
                {ok, DirectCandidate} = oz_test_utils:create_user(Config),
                oz_test_utils:space_add_user(Config, Space, DirectCandidate),
                DirectCandidate;
            2 ->
                {ok, EffectiveCandidate} = oz_test_utils:create_user(Config),
                {ok, SubGroup} = oz_test_utils:create_group(Config, ?USER(EffectiveCandidate)),
                oz_test_utils:space_add_group(Config, Space, SubGroup),
                EffectiveCandidate
        end,
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        #{userId => Candidate}
    end,
    EnvTearDownFun = fun(#{userId := Candidate}) ->
        oz_test_utils:call_oz(Config, space_logic, remove_owner, [?ROOT, Space, Candidate])
    end,
    VerifyEndFun = fun(ShouldSucceed, #{userId := Candidate} = _Env, _) ->
        {ok, #od_space{owners = Owners, users = DirectUsers}} = oz_test_utils:get_space(Config, Space),
        ?assertEqual(lists:member(Candidate, Owners), ShouldSucceed),
        case ShouldSucceed of
            false -> ok;
            % upon success, the new owner should be added as a direct member (if he wasn't one)
            true -> ?assert(maps:is_key(Candidate, DirectUsers))
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Owner},
                {admin, [?OZ_SPACES_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, RegularMember},
                {user, NonAdmin}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = put,
            path = [<<"/spaces/">>, Space, <<"/owners/">>, userId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = LogicSpec = #logic_spec{
            module = space_logic,
            function = add_owner,
            args = [auth, Space, userId],
            expected_result = ?OK
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, VerifyEndFun
    )),

    EnvSetUpFunAlreadyAnOwner = fun() ->
        #{userId := Candidate} = EnvSetUpFun(),
        oz_test_utils:space_add_owner(Config, Space, Candidate),
        #{userId => Candidate}
    end,
    % it should not be possible to add a user that is already an owner
    ApiTestSpecAlreadyExists = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            path = [<<"/spaces/">>, Space, <<"/owners/">>, userId],
            expected_code = ?HTTP_409_CONFLICT
        },
        logic_spec = LogicSpec#logic_spec{
            args = [auth, Space, userId],
            expected_result = ?ERROR_REASON(?ERROR_ALREADY_EXISTS)
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpecAlreadyExists, EnvSetUpFunAlreadyAnOwner, EnvTearDownFun, undefined
    )).


remove_owner_test(Config) ->
    {ok, Owner} = oz_test_utils:create_user(Config),
    {ok, RegularMember} = oz_test_utils:create_user(Config),
    {ok, SpaceAdminButNotOwner} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, Space} = oz_test_utils:create_space(Config, ?USER(Owner)),
    oz_test_utils:space_add_user(Config, Space, RegularMember),
    oz_test_utils:space_add_user(Config, Space, SpaceAdminButNotOwner),
    oz_test_utils:space_set_user_privileges(Config, Space, SpaceAdminButNotOwner, privileges:space_admin(), []),

    EnvSetUpFun = fun() ->
        {ok, NewOwner} = oz_test_utils:create_space_owner(Config, Space),
        #{ownerToRemove => NewOwner}
    end,
    EnvTearDownFun = fun(#{ownerToRemove := Candidate}) ->
        oz_test_utils:call_oz(Config, space_logic, remove_owner, [?ROOT, Space, Candidate])
    end,
    VerifyEndFun = fun(ShouldSucceed, #{ownerToRemove := OwnerToRemove} = _Env, _) ->
        {ok, #od_space{owners = Owners}} = oz_test_utils:get_space(Config, Space),
        ?assertEqual(lists:member(OwnerToRemove, Owners), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Owner},
                {admin, [?OZ_SPACES_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, SpaceAdminButNotOwner},
                {user, RegularMember},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/spaces/">>, Space, <<"/owners/">>, ownerToRemove],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = remove_owner,
            args = [auth, Space, ownerToRemove],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, VerifyEndFun
    )).


get_user_privileges_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_VIEW_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:space_add_user(Config, S1, U3),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    oz_test_utils:support_space_by_provider(Config, P1, S1),

    AllPrivs = privileges:space_privileges(),
    InitialPrivs = privileges:space_member(),
    InitialPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- InitialPrivs],
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:space_set_user_privileges(
            Config, S1, U3, PrivsToGrant, PrivsToRevoke
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW_PRIVILEGES]},
                {user, Owner},
                {user, U2},
                % user can always see his own privileges
                {user, U3},
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/users/">>, U3, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_user_privileges,
            args = [auth, S1, U3],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U3}, ?SPACE_VIEW_PRIVILEGES, false, U3
    ])).


update_user_privileges_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_SET_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_SET_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to update user privileges and sometimes not)
    {ok, U3} = oz_test_utils:create_user(Config),
    {ok, U3} = oz_test_utils:space_add_user(Config, S1, U3),

    AllPrivs = privileges:space_privileges(),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:space_set_user_privileges(
            Config, S1, U3, PrivsToGrant, PrivsToRevoke
        )
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:space_get_user_privileges(Config, S1, U3),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_SET_PRIVILEGES]},
                {user, Owner},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/spaces/">>, S1, <<"/users/">>, U3, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = update_user_privileges,
            args = [auth, S1, U3, data],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U3}, ?SPACE_SET_PRIVILEGES
    ])).


list_eff_users_test(Config) ->
    {
        S1, _Groups, [{U3, _}, {U4, _}, {U5, _}, {U6, _}], {Owner, U1, U2, NonAdmin}
    } = api_test_scenarios:create_space_eff_users_env(Config),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    oz_test_utils:support_space_by_provider(Config, P1, S1),

    ExpUsers = [Owner, U1, U2, U3, U4, U5, U6],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Owner},
                {user, U2},
                {admin, [?OZ_SPACES_LIST_RELATIONSHIPS]},
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/effective_users">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_eff_users,
            args = [auth, S1],
            expected_result = ?OK_LIST(ExpUsers)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check also space_logic:has_eff_user function
    lists:foreach(
        fun(UserId) ->
            ?assert(oz_test_utils:call_oz(
                Config, space_logic, has_eff_user, [S1, UserId])
            )
        end, ExpUsers
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, space_logic, has_eff_user, [S1, <<"asdiucyaie827346w">>])
    ).


get_eff_user_test(Config) ->
    {
        S1, _Groups, EffUsers, {Owner, U1, U2, NonAdmin}
    } = api_test_scenarios:create_space_eff_users_env(Config),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    oz_test_utils:support_space_by_provider(Config, P1, S1),

    lists:foreach(fun({UserId, UserData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_USERS_VIEW]},
                    {user, Owner},
                    {user, U2},
                    {provider, P1, P1Token}
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
                    <<"/spaces/">>, S1, <<"/effective_users/">>, UserId
                ],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:shared_user(rest, UserId, UserData)
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = get_eff_user,
                args = [auth, S1, UserId],
                expected_result = api_test_expect:shared_user(logic, UserId, UserData)
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_user, id = UserId,
                    aspect = instance, scope = shared
                },
                auth_hint = ?THROUGH_SPACE(S1),
                expected_result_op = api_test_expect:shared_user(gs, UserId, UserData)
            }
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffUsers).


get_eff_user_privileges_test(Config) ->
    %% Create environment with the following relations:
    %%
    %%                  Space
    %%                 /  ||  \
    %%                /   ||   \
    %%     [~tspace_view]  ||  [space_view]
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
    %%                     User3
    %%      <<user>>
    %%      NonAdmin

    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_VIEW_PRIVILEGES privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW_PRIVILEGES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, SubjectUser} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),

    {ok, G1} = oz_test_utils:space_add_group(Config, S1, G1),
    {ok, G2} = oz_test_utils:space_add_group(Config, S1, G2),
    {ok, G3} = oz_test_utils:group_add_group(Config, G1, G3),
    {ok, SubjectUser} = oz_test_utils:group_add_user(Config, G3, SubjectUser),
    {ok, SubjectUser} = oz_test_utils:group_add_user(Config, G2, SubjectUser),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    oz_test_utils:support_space_by_provider(Config, P1, S1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = privileges:space_privileges(),
    InitialPrivs = privileges:space_member(),
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

        oz_test_utils:space_set_group_privileges(Config, S1, G1, PrivsToGrant1, PrivsToRevoke),
        oz_test_utils:space_set_group_privileges(Config, S1, G2, PrivsToGrant2, PrivsToRevoke)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW_PRIVILEGES]},
                {user, Owner},
                {user, U2},
                % user can always see his own privileges
                {user, SubjectUser},
                {provider, P1, P1Token}
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
                <<"/spaces/">>, S1, <<"/effective_users/">>, SubjectUser,
                <<"/privileges">>
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivsBin}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_eff_user_privileges,
            args = [auth, S1, SubjectUser],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, SubjectUser}, ?SPACE_VIEW_PRIVILEGES, false, SubjectUser
    ])).


get_eff_user_membership_intermediaries(Config) ->
    %% Create environment with the following relations:
    %%
    %%      Space1       Space2     Space3
    %%       | |  \     /   |  \     /   \
    %%       | |   \   /    |   \   /     User2 (no view privs)
    %%       |  \   Group2  |   Group3
    %%       |   \   /      |   /  |
    %%        \   \ /       |  /   |
    %%         \  Group1----|-'    |
    %%          \     \     |     /
    %%           \     \    |    /
    %%            \     \   |   /
    %%             '------User1 (view privs)
    %%
    %%      <<user>>                          <<user>>
    %%      NonAdmin                        AllSpacesOwner

    {ok, AllSpacesOwner} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?ROOT, ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(AllSpacesOwner), ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(AllSpacesOwner), ?SPACE_NAME1),
    {ok, S3} = oz_test_utils:create_space(Config, ?USER(AllSpacesOwner), ?SPACE_NAME1),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    oz_test_utils:support_space_by_provider(Config, P1, S1),
    oz_test_utils:support_space_by_provider(Config, P1, S2),
    oz_test_utils:support_space_by_provider(Config, P1, S3),

    oz_test_utils:space_add_user(Config, S1, U1),
    oz_test_utils:space_add_user(Config, S2, U1),
    oz_test_utils:space_add_user(Config, S3, U2),
    oz_test_utils:space_set_user_privileges(Config, S3, U2, [], [?SPACE_VIEW]),

    oz_test_utils:group_add_group(Config, G2, G1),
    oz_test_utils:group_add_group(Config, G3, G1),

    oz_test_utils:space_add_group(Config, S3, G3),
    oz_test_utils:space_add_group(Config, S1, G1),
    oz_test_utils:space_add_group(Config, S1, G2),
    oz_test_utils:space_add_group(Config, S2, G2),
    oz_test_utils:space_add_group(Config, S2, G3),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % {SpaceId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}
    ExpectedMembershipIntermediaries = [
        {S1, U1, [U1], ordsets:from_list([
            {od_space, ?SELF_INTERMEDIARY},
            {od_group, G1},
            {od_group, G2}
        ])},

        {S2, U1, [U1], ordsets:from_list([
            {od_space, ?SELF_INTERMEDIARY},
            {od_group, G2},
            {od_group, G3}
        ])},

        {S3, U1, [U1], ordsets:from_list([
            {od_group, G3}
        ])},
        {S3, U2, [U1, U2], ordsets:from_list([
            {od_space, ?SELF_INTERMEDIARY}
        ])}
    ],

    lists:foreach(fun({SpaceId, SubjectUser, CorrectUsers, ExpIntermediariesRaw}) ->
        ExpIntermediaries = lists:map(fun({Type, Id}) ->
            #{<<"type">> => gri:serialize_type(Type), <<"id">> => Id}
        end, ExpIntermediariesRaw),
        CorrectUserClients = [{user, U} || U <- CorrectUsers],
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_SPACES_VIEW]},
                    {user, AllSpacesOwner},
                    {provider, P1, P1Token}
                ] ++ CorrectUserClients,
                unauthorized = [nobody],
                forbidden = [{user, NonAdmin}, {user, U1}, {user, U2}] -- CorrectUserClients
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/spaces/">>, SpaceId, <<"/effective_users/">>, SubjectUser, <<"/membership">>],
                expected_code = ?HTTP_200_OK,
                expected_body = #{<<"intermediaries">> => ExpIntermediaries}
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = get_eff_user_membership_intermediaries,
                args = [auth, SpaceId, SubjectUser],
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
