%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning space basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(space_misc_api_test_SUITE).
-author("Bartosz Walkowicz").

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-include("api_test_utils.hrl").
-include("ozt.hrl").


-export([
    groups/0, all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    end_per_testcase/2
]).
-export([
    create_test/1,
    create_with_custom_id_generator_seed_test/1,
    list_privileges_test/1,
    get_test/1,
    get_marketplace_data_test/1,
    update_space_not_in_marketplace_test/1,
    update_space_in_marketplace_test/1,
    delete_test/1,

    get_shares_test/1,
    get_share_test/1,

    list_storages_test/1,
    create_space_support_token/1,
    remove_storage_test/1,
    remove_provider_test/1,

    submit_membership_request_test/1,
    get_membership_requester_info_test/1,
    resolve_membership_request_test/1,

    list_effective_providers_test/1,
    get_eff_provider_test/1,

    update_support_parameters_test/1,

    % sequential tests
    get_marketplace_data_error_marketplace_disabled_test/1,
    list_test/1,
    list_marketplace_test/1,
    list_marketplace_error_marketplace_disabled_test/1
]).


groups() -> [
    {parallel_tests, [parallel], [
        create_test,
        create_with_custom_id_generator_seed_test,
        list_privileges_test,
        get_test,
        get_marketplace_data_test,
        update_space_not_in_marketplace_test,
        update_space_in_marketplace_test,
        delete_test,

        get_shares_test,
        get_share_test,

        list_storages_test,
        create_space_support_token,
        remove_storage_test,
        remove_provider_test,

        submit_membership_request_test,
        get_membership_requester_info_test,
        resolve_membership_request_test,

        list_effective_providers_test,
        get_eff_provider_test,

        update_support_parameters_test
    ]},
    {sequential_tests, [sequential], [
        get_marketplace_data_error_marketplace_disabled_test,
        list_test,
        list_marketplace_test,
        list_marketplace_error_marketplace_disabled_test
    ]}
].

all() -> [
    {group, sequential_tests},
    {group, parallel_tests}
].


-define(MARKETPLACE_RELATED_PARAMETERS_NAMES, [
    <<"description">>,
    <<"organizationName">>,
    <<"tags">>,
    <<"advertisedInMarketplace">>,
    <<"marketplaceContactEmail">>
]).

-define(CORRECT_PARAMETER_VALUES_FOR_NON_ADVERTISED_SPACE(__OTHER_VALUES), maps:merge(
    #{
        <<"name">> => [?RAND_STR()],
        <<"description">> => [<<>>, ?RAND_STR()],
        <<"organizationName">> => [<<>>, ?RAND_STR()],
        <<"tags">> => [[], ?RAND_SUBLIST(ozt_spaces:available_space_tags())],
        <<"marketplaceContactEmail">> => [<<>>, ?RAND_STR()]
    },
    __OTHER_VALUES
)).
-define(CORRECT_PARAMETER_VALUES_FOR_NON_ADVERTISED_SPACE,
    ?CORRECT_PARAMETER_VALUES_FOR_NON_ADVERTISED_SPACE(#{})
).
-define(CORRECT_PARAMETER_VALUES_FOR_ADVERTISED_SPACE(__OTHER_VALUES), maps:merge(
    #{
        <<"name">> => [?RAND_STR()],
        <<"description">> => [?RAND_STR()],
        <<"organizationName">> => [?RAND_STR()],
        <<"tags">> => [[], ?RAND_SUBLIST(ozt_spaces:available_space_tags())],
        <<"marketplaceContactEmail">> => [<<"a@a.a">>]
    },
    __OTHER_VALUES
)).
-define(CORRECT_PARAMETER_VALUES_FOR_ADVERTISED_SPACE,
    ?CORRECT_PARAMETER_VALUES_FOR_ADVERTISED_SPACE(#{})
).

-define(BAD_PARAMETER_VALUES_FOR_NON_ADVERTISED_SPACE, [
    {<<"description">>, 10, ?ERROR_BAD_VALUE_BINARY(<<"description">>)},
    {<<"organizationName">>, 10, ?ERROR_BAD_VALUE_BINARY(<<"organizationName">>)},
    {<<"tags">>, [<<"troll">>], ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"tags">>, ozt_spaces:available_space_tags())},
    {<<"advertisedInMarketplace">>, 10, ?ERROR_BAD_VALUE_BOOLEAN(<<"advertisedInMarketplace">>)},
    {<<"marketplaceContactEmail">>, 10, ?ERROR_BAD_VALUE_BINARY(<<"marketplaceContactEmail">>)}
]).
-define(BAD_PARAMETER_VALUES_FOR_ADVERTISED_SPACE, [
    {<<"description">>, <<>>, ?ERROR_BAD_VALUE_EMPTY(<<"description">>)},
    {<<"organizationName">>, <<>>, ?ERROR_BAD_VALUE_EMPTY(<<"organizationName">>)},
    {<<"marketplaceContactEmail">>, <<>>, ?ERROR_BAD_VALUE_EMPTY(<<"marketplaceContactEmail">>)}
]).

-define(CORRECT_DATA_FOR_ADVERTISED_SPACE(__VALUES), maps:merge(
    #{
        <<"name">> => ?RAND_STR(),
        <<"description">> => ?RAND_STR(),
        <<"organizationName">> => ?RAND_STR(),
        <<"tags">> => ?RAND_SUBLIST(ozt_spaces:available_space_tags()),
        <<"advertisedInMarketplace">> => true,
        <<"marketplaceContactEmail">> => <<"a@a.a">>
    },
    __VALUES
)).


%%%===================================================================
%%% Test functions
%%%===================================================================


create_test(Config) ->
    U1 = ozt_users:create(),

    VerifyFun = fun(SpaceId, Data) ->
        F = fun(Key, Default) -> maps:get(Key, Data, Default) end,

        Space = ozt_spaces:get(SpaceId),

        ?assertEqual(F(<<"name">>, ?CORRECT_NAME), Space#od_space.name),
        ?assertEqual(F(<<"description">>, <<>>), Space#od_space.description),
        ?assertEqual(F(<<"organizationName">>, <<>>), Space#od_space.organization_name),
        ?assertEqual(F(<<"tags">>, []), Space#od_space.tags),
        ?assertEqual(F(<<"advertisedInMarketplace">>, false), Space#od_space.advertised_in_marketplace),
        ?assertEqual(F(<<"marketplaceContactEmail">>, <<>>), Space#od_space.marketplace_contact_email),

        case F(<<"advertisedInMarketplace">>, false) of
            true -> assert_in_marketplace_with_tags(SpaceId, maps:get(<<"tags">>, Data));
            false -> ?assertNot(in_marketplace(SpaceId))
        end,

        true
    end,

    WithoutMarketplaceDataSpec = #data_spec{
        required = [<<"name">>],
        optional = ?MARKETPLACE_RELATED_PARAMETERS_NAMES,
        correct_values = ?CORRECT_PARAMETER_VALUES_FOR_NON_ADVERTISED_SPACE(#{
            <<"advertisedInMarketplace">> => [false]
        }),
        bad_values = lists:flatten([
            ?BAD_PARAMETER_VALUES_FOR_NON_ADVERTISED_SPACE,
            ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        ])
    },
    WithMarketplaceDataSpec = #data_spec{
        required = [<<"name">> | ?MARKETPLACE_RELATED_PARAMETERS_NAMES],
        correct_values = ?CORRECT_PARAMETER_VALUES_FOR_ADVERTISED_SPACE(#{
            <<"advertisedInMarketplace">> => [true]
        }),
        bad_values = ?BAD_PARAMETER_VALUES_FOR_ADVERTISED_SPACE
    },

    BuildApiTestSpecFun = fun(DataSpec) ->
        #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_SPACES_CREATE]}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, U1}
                ]
            },
            rest_spec = #rest_spec{
                method = post,
                path = <<"/spaces">>,
                expected_code = ?HTTP_201_CREATED,
                expected_headers = ?OK_ENV(fun(_Env, Data) ->
                    fun(#{?HDR_LOCATION := Location} = _Headers) ->
                        BaseURL = ?URL(Config, [<<"/spaces/">>]),
                        [SpaceId] = binary:split(Location, [BaseURL], [global, trim_all]),
                        VerifyFun(SpaceId, Data)
                    end
                end)
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = create,
                args = [auth, data],
                expected_result = ?OK_ENV(fun(_Env, Data) ->
                    ?OK_TERM(fun(SpaceId) -> VerifyFun(SpaceId, Data) end)
                end)
            },
            gs_spec = #gs_spec{
                operation = create,
                gri = #gri{type = od_space, aspect = instance},
                expected_result_op = ?OK_ENV(fun(_Env, Data) ->
                    ?OK_MAP_CONTAINS(#{<<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
                        VerifyFun(Id, Data)
                    end})
                end)
            },
            data_spec = DataSpec
        }
    end,
    ?assert(api_test_utils:run_tests(Config, BuildApiTestSpecFun(WithoutMarketplaceDataSpec))),
    ?assert(api_test_utils:run_tests(Config, BuildApiTestSpecFun(WithMarketplaceDataSpec))).


create_with_custom_id_generator_seed_test(Config) ->
    SpaceCount = 100,
    OzNodes = ?config(oz_worker_nodes, Config),
    IdGeneratorSeed = ?RAND_STR(),
    Data = #{
        <<"name">> => ?RAND_STR(),
        <<"idGeneratorSeed">> => IdGeneratorSeed
    },

    U1 = ozt_users:create(),
    U2 = ozt_users:create(),
    U3 = ozt_users:create(),
    U4 = ozt_users:create(),
    Auths = [?USER(U1), ?USER(U2), ?USER(U3), ?USER(U4), ?ROOT],

    Results = lists_utils:pmap(fun(_) ->
        case ?RAND_ELEMENT(Auths) of
            ?ROOT ->
                rpc:call(?RAND_ELEMENT(OzNodes), space_logic, create, [?ROOT, Data]);
            ?USER(UserId) ->
                rpc:call(?RAND_ELEMENT(OzNodes), user_logic, create_space, [?USER(UserId), UserId, Data])
        end
    end, lists:seq(1, SpaceCount)),

    {ok, {ok, SpaceId}} = lists_utils:find(fun
        ({ok, _}) -> true;
        (_) -> false
    end, Results),

    ?assertEqual(SpaceId, datastore_key:new_from_digest([<<"customSpaceIdGeneratorSeed">>, IdGeneratorSeed])),

    ErrorResults = lists:delete({ok, SpaceId}, Results),
    ?assertEqual(lists:duplicate(SpaceCount - 1, ?ERROR_ALREADY_EXISTS), ErrorResults).


list_privileges_test(Config) ->
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/spaces/privileges">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"member">> => [atom_to_binary(P, utf8) || P <- privileges:space_member()],
                <<"manager">> => [atom_to_binary(P, utf8) || P <- privileges:space_manager()],
                <<"admin">> => [atom_to_binary(P, utf8) || P <- privileges:space_admin()]
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_test(Config) ->
    Owner = ozt_users:create(),
    UserWithoutSpaceView = ozt_users:create(),
    UserWithSpaceViewOnly = ozt_users:create(),
    UserWithSpaceViewAndManageMarketplace = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    ExpectedMarketplaceContactEmail = ?RAND_ELEMENT([?RAND_EMAIL_ADDRESS(), <<"">>]),
    SpaceId = ozt_users:create_space_for(Owner, #{
        <<"name">> => ?SPACE_NAME1,
        <<"marketplaceContactEmail">> => ExpectedMarketplaceContactEmail
    }),

    AllPrivs = privileges:space_privileges(),
    ozt_spaces:add_user(SpaceId, UserWithoutSpaceView, AllPrivs -- [?SPACE_VIEW]),
    ozt_spaces:add_user(SpaceId, UserWithSpaceViewOnly, [?SPACE_VIEW]),
    ozt_spaces:add_user(SpaceId, UserWithSpaceViewAndManageMarketplace, [?SPACE_VIEW, ?SPACE_MANAGE_IN_MARKETPLACE]),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config),
    SupportSize = ozt_spaces:minimum_support_size(),
    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, SpaceId} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, SpaceId, SupportSize),

    ozt_providers:simulate_version(P1, ?LINE_21_02),
    ozt_spaces:set_support_parameters(SpaceId, P1, ozt_spaces:random_support_parameters()),
    ExpSupportParametersRegistry = #support_parameters_registry{
        registry = #{
            % the parameters may have been tweaked during update, fetch them to ensure latest value
            P1 => ozt_spaces:get_support_parameters(SpaceId, P1)
        }
    },
    ExpAdvertisedInMarketplace = false,
    ozt:reconcile_entity_graph(),

    ExpUsersField = #{
        Owner => AllPrivs,
        UserWithoutSpaceView => AllPrivs -- [?SPACE_VIEW],
        UserWithSpaceViewOnly => [?SPACE_VIEW],
        UserWithSpaceViewAndManageMarketplace => [?SPACE_VIEW, ?SPACE_MANAGE_IN_MARKETPLACE]
    },
    ExpEffUsersField = #{
        Owner => {AllPrivs, [{od_space, <<"self">>}]},
        UserWithoutSpaceView => {AllPrivs -- [?SPACE_VIEW], [{od_space, <<"self">>}]},
        UserWithSpaceViewOnly => {[?SPACE_VIEW], [{od_space, <<"self">>}]},
        UserWithSpaceViewAndManageMarketplace => {[?SPACE_VIEW, ?SPACE_MANAGE_IN_MARKETPLACE], [{od_space, <<"self">>}]}
    },
    ExpUsersWithBinaryPrivs = maps:map(fun(_UserId, Privileges) ->
        [atom_to_binary(P) || P <- Privileges]
    end, ExpUsersField),

    % The private data which differs depending on the auth
    % (in case of users, requires ?SPACE_MANAGE_IN_MARKETPLACE privilege)
    lists:foreach(fun({CorrectClient, CanViewMarketplaceContactEmail}) ->
        ?assert(api_test_utils:run_tests(Config, #api_test_spec{
            client_spec = #client_spec{
                correct = [CorrectClient],
                unauthorized = [nobody],
                forbidden = [
                    {admin, [?OZ_SPACES_VIEW]},
                    {user, NonAdmin},
                    {user, UserWithoutSpaceView}
                ]
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = get,
                args = [auth, SpaceId],
                expected_result = ?OK_TERM(
                    fun(#od_space{
                        name = Name,
                        advertised_in_marketplace = AdvertisedInMarketplace,
                        marketplace_contact_email = MarketplaceContactEmail,
                        users = Users, groups = #{},
                        storages = Storages, shares = [],
                        harvesters = [],
                        eff_users = EffUsers, eff_groups = #{},
                        eff_providers = EffProviders,
                        support_parameters_registry = SupportParametersRegistry,
                        top_down_dirty = false, bottom_up_dirty = false
                    }) ->
                        ?assertEqual(?SPACE_NAME1, Name),
                        ?assertEqual(ExpAdvertisedInMarketplace, AdvertisedInMarketplace),
                        ?assertEqual(MarketplaceContactEmail, case CanViewMarketplaceContactEmail of
                            true -> ExpectedMarketplaceContactEmail;
                            false -> <<"">>
                        end),
                        ?assertEqual(Users, ExpUsersField),
                        ?assertEqual(EffUsers, ExpEffUsersField),
                        ?assertEqual(Storages, #{St1 => SupportSize}),
                        ?assertEqual(SupportParametersRegistry, ExpSupportParametersRegistry),
                        ?assertEqual(EffProviders, #{P1 => {SupportSize, [{od_storage, St1}]}})
                    end
                )
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{type = od_space, id = SpaceId, aspect = instance},
                expected_result_op = ?OK_MAP_CONTAINS(#{
                    <<"name">> => ?SPACE_NAME1,
                    <<"owners">> => [Owner],
                    <<"users">> => ExpUsersWithBinaryPrivs,
                    <<"groups">> => #{},
                    <<"shares">> => [],
                    <<"providers">> => #{P1 => SupportSize},
                    <<"harvesters">> => [],
                    <<"effectiveUsers">> => ExpUsersWithBinaryPrivs,
                    <<"effectiveGroups">> => #{},
                    <<"supportParametersRegistry">> => jsonable_record:to_json(
                        ExpSupportParametersRegistry, support_parameters_registry
                    ),
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
                        ?assertEqual(SpaceId, Id)
                    end
                })
            }
        }))
    end, [
        {root, true},
        {{user, Owner}, true},
        {{user, UserWithSpaceViewOnly}, false},
        {{user, UserWithSpaceViewAndManageMarketplace}, true},
        {{provider, P1, P1Token}, false}
    ]),

    % The protected data which differs depending on the auth
    % (in case of users, requires ?SPACE_MANAGE_IN_MARKETPLACE privilege)
    lists:foreach(fun({CorrectClient, CanViewMarketplaceContactEmail}) ->
        ProtectedSpaceData = #{
            <<"name">> => ?SPACE_NAME1,
            <<"providers">> => #{P1 => SupportSize},
            <<"supportParametersRegistry">> => ExpSupportParametersRegistry,
            <<"advertisedInMarketplace">> => ExpAdvertisedInMarketplace,
            <<"marketplaceContactEmail">> => case CanViewMarketplaceContactEmail of
                true -> ExpectedMarketplaceContactEmail;
                false -> <<"">>
            end
        },
        ?assert(api_test_utils:run_tests(Config, #api_test_spec{
            client_spec = #client_spec{
                correct = [CorrectClient],
                unauthorized = [nobody],
                forbidden = [{user, NonAdmin}]
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/spaces/">>, SpaceId],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:protected_space(rest, SpaceId, ProtectedSpaceData, ?SUB(user, Owner))
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = get_protected_data,
                args = [auth, SpaceId],
                expected_result = api_test_expect:protected_space(logic, SpaceId, ProtectedSpaceData, ?SUB(user, Owner))
            },
            gs_spec = #gs_spec{
                operation = get,
                gri = #gri{
                    type = od_space, id = SpaceId, aspect = instance, scope = protected
                },
                expected_result_op = api_test_expect:protected_space(gs, SpaceId, ProtectedSpaceData, ?SUB(user, Owner))
            }
        }))
    end, [
        {root, true},
        {{admin, [?OZ_SPACES_VIEW]}, true},
        {{user, Owner}, true},
        {{user, UserWithSpaceViewAndManageMarketplace}, true},
        {{user, UserWithSpaceViewOnly}, false},
        {{user, UserWithoutSpaceView}, false},
        {{provider, P1, P1Token}, false}
    ]).


get_marketplace_data_test(Config) ->
    Owner = ozt_users:create(),
    U1 = ozt_users:create(),
    U2 = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    BasicS1Data = #{
        <<"name">> => ?SPACE_NAME1,
        <<"description">> => ?RAND_STR(),
        <<"organizationName">> => ?RAND_STR(),
        <<"tags">> => [<<"demo">>]
    },
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(Owner), BasicS1Data#{
        <<"advertisedInMarketplace">> => true,
        <<"marketplaceContactEmail">> => <<"a@a.a">>
    }),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(Owner), ?SPACE_NAME2),

    AllPrivs = privileges:space_privileges(),
    lists:foreach(fun(SpaceId) ->
        ozt_spaces:add_user(SpaceId, U1, AllPrivs -- [?SPACE_VIEW]),
        ozt_spaces:add_user(SpaceId, U2, [?SPACE_VIEW])
    end, [S1, S2]),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config),
    SupportSize = ozt_spaces:minimum_support_size(),
    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, S1, SupportSize),

    S1Data = BasicS1Data#{
        <<"spaceId">> => S1,
        <<"index">> => space_marketplace:index(?SPACE_NAME1, S1),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"totalSupportSize">> => SupportSize,
        <<"providerNames">> => [ozt_providers:get_name(P1)]
    },

    UsersWithAccounts = [
        root,
        {admin, [?OZ_SPACES_VIEW]},
        {user, Owner},
        {user, U1},
        {user, U2},
        {user, NonAdmin}
    ],
    LogicSpec = #logic_spec{
        module = space_logic,
        function = get_marketplace_data,
        args = [auth, S1],
        expected_result = ?OK_MAP(S1Data)
    },
    GSSpec = #gs_spec{
        operation = get,
        gri = #gri{type = od_space, id = S1, aspect = marketplace_data, scope = protected},
        expected_result_op = ?ERROR_REASON(?ERROR_FORBIDDEN),
        expected_result_gui = ?OK_MAP(S1Data#{
            <<"gri">> => gri:serialize(?GRI(od_space, S1, marketplace_data, protected))
        })
    },

    % Get and check protected data
    GetProtectedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = UsersWithAccounts,
            unauthorized = [nobody],
            forbidden = [{provider, P1, P1Token}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/marketplace/">>, S1],
            expected_code = ?HTTP_200_OK,
            expected_body = S1Data
        },
        logic_spec = LogicSpec,
        gs_spec = GSSpec
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)),

    % Assert it is not possible to get marketplace data from space not in marketplace
    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
        client_spec = #client_spec{
            correct = [nobody, {provider, P1, P1Token} | UsersWithAccounts],
            unauthorized = [],
            forbidden = []
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/marketplace/">>, S2],
            expected_code = ?HTTP_404_NOT_FOUND,
            expected_body = #{<<"error">> => errors:to_json(?ERROR_NOT_FOUND)}
        },
        logic_spec = LogicSpec#logic_spec{
            args = [auth, S2],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        },
        gs_spec = GSSpec#gs_spec{
            gri = #gri{type = od_space, id = S2, aspect = marketplace_data, scope = protected},
            expected_result_op = ?ERROR_REASON(?ERROR_NOT_FOUND),
            expected_result_gui = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
    })).


update_space_not_in_marketplace_test(Config) ->
    Owner = ozt_users:create(),
    % if marketplace advertisement or contact email is modified,
    % SPACE_MANAGE_IN_MARKETPLACE is required beside SPACE_UPDATE
    UserWithUpdatePrivileges = ozt_users:create(),
    UserWithManageMarketplacePrivileges = ozt_users:create(),
    UserWithoutPrivileges = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    RandomMarketplaceData = maps_utils:random_submap(#{
        <<"description">> => ?RAND_STR(),
        <<"organizationName">> => ?RAND_STR(),
        <<"tags">> => [<<"demo">>],
        <<"marketplaceContactEmail">> => <<"a@a.a">>
    }),
    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(Owner), maps:merge(
            RandomMarketplaceData,
            #{<<"name">> => ?CORRECT_NAME, <<"advertisedInMarketplace">> => false}
        )),
        ozt_spaces:add_user(S1, UserWithoutPrivileges, []),
        ozt_spaces:add_user(S1, UserWithUpdatePrivileges, [?SPACE_UPDATE]),
        ozt_spaces:add_user(S1, UserWithManageMarketplacePrivileges, [?SPACE_MANAGE_IN_MARKETPLACE]),
        #{spaceId => S1, initialData => get_space_marketplace_related_data_json(S1)}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId, initialData := PrevData} = _Env, Data) ->
        ExpData = case ShouldSucceed of
            false -> PrevData;
            true -> maps:merge(PrevData, Data)
        end,
        ?assertEqual(ExpData, get_space_marketplace_related_data_json(SpaceId)),

        case ShouldSucceed andalso maps:get(<<"advertisedInMarketplace">>, Data, false) of
            true -> assert_in_marketplace_with_tags(SpaceId, maps:get(<<"tags">>, ExpData));
            false -> ?assertNot(in_marketplace(SpaceId))
        end
    end,

    BuildDataSpec = fun
        (does_not_add_to_marketplace, ContactEmailModification) ->
            #data_spec{
                required = case ContactEmailModification of
                    does_not_modify_contact_email -> [];
                    modifies_contact_email -> [<<"marketplaceContactEmail">>]
                end,
                at_least_one = [<<"name">> | ?MARKETPLACE_RELATED_PARAMETERS_NAMES] -- [
                    <<"advertisedInMarketplace">>,
                    <<"marketplaceContactEmail">>
                ],
                correct_values = ?CORRECT_PARAMETER_VALUES_FOR_NON_ADVERTISED_SPACE,
                bad_values = lists:flatten([
                    ?BAD_PARAMETER_VALUES_FOR_NON_ADVERTISED_SPACE,
                    ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
                ])
            };
        (adds_to_marketplace, ContactEmailModification) ->
            #data_spec{
                required = ?MARKETPLACE_RELATED_PARAMETERS_NAMES -- lists:flatten([
                    maps:keys(RandomMarketplaceData),
                    case ContactEmailModification of
                        does_not_modify_contact_email -> <<"marketplaceContactEmail">>;
                        modifies_contact_email -> []
                    end
                ]),
                optional = [<<"name">> | maps:keys(RandomMarketplaceData)],
                correct_values = ?CORRECT_PARAMETER_VALUES_FOR_ADVERTISED_SPACE(#{
                    <<"advertisedInMarketplace">> => [true]
                }),
                bad_values = ?BAD_PARAMETER_VALUES_FOR_ADVERTISED_SPACE
            }
    end,

    RunTests = fun(AdvertisementModification, ContactEmailModification) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = lists:flatten([
                    root,
                    {admin, [?OZ_SPACES_UPDATE]},
                    {user, Owner},
                    case {AdvertisementModification, ContactEmailModification} of
                        {does_not_add_to_marketplace, does_not_modify_contact_email} ->
                            {user, UserWithUpdatePrivileges};
                        _ ->
                            []
                    end
                ]),
                unauthorized = [nobody],
                forbidden = lists:flatten([
                    {user, UserWithoutPrivileges},
                    {user, UserWithManageMarketplacePrivileges},
                    {user, NonAdmin},
                    case {AdvertisementModification, ContactEmailModification} of
                        {does_not_add_to_marketplace, does_not_modify_contact_email} ->
                            [];
                        _ ->
                            {user, UserWithUpdatePrivileges}
                    end
                ])
            },
            rest_spec = #rest_spec{
                method = patch,
                path = [<<"/spaces/">>, spaceId],
                expected_code = ?HTTP_204_NO_CONTENT
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = update,
                args = [auth, spaceId, data],
                expected_result = ?OK_RES
            },
            gs_spec = #gs_spec{
                operation = update,
                gri = #gri{type = od_space, id = spaceId, aspect = instance},
                expected_result_op = ?OK_RES
            },
            data_spec = BuildDataSpec(AdvertisementModification, ContactEmailModification)
        },
        api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)
    end,

    ?assert(RunTests(does_not_add_to_marketplace, does_not_modify_contact_email)),
    ?assert(RunTests(does_not_add_to_marketplace, modifies_contact_email)),
    ?assert(RunTests(adds_to_marketplace, modifies_contact_email)),
    % if the space is added to marketplace, marketplace contact email is required
    ?assertEqual(?ERROR_MISSING_REQUIRED_VALUE(<<"marketplaceContactEmail">>), ozt:rpc(space_logic, update, [
        ?USER(Owner), ozt_users:create_space_for(Owner), #{
            <<"advertisedInMarketplace">> => true,
            <<"description">> => ?RAND_UNICODE_STR(),
            <<"tags">> => []
        }
    ])).


update_space_in_marketplace_test(Config) ->
    Owner = ozt_users:create(),
    % if marketplace advertisement or contact email is modified,
    % SPACE_MANAGE_IN_MARKETPLACE is required beside SPACE_UPDATE
    UserWithUpdatePrivileges = ozt_users:create(),
    UserWithoutPrivileges = ozt_users:create(),
    UserWithManageMarketplacePrivileges = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(Owner), ?CORRECT_DATA_FOR_ADVERTISED_SPACE(#{
            <<"name">> => ?CORRECT_NAME
        })),
        ozt_spaces:add_user(S1, UserWithoutPrivileges, []),
        ozt_spaces:add_user(S1, UserWithUpdatePrivileges, [?SPACE_UPDATE]),
        ozt_spaces:add_user(S1, UserWithManageMarketplacePrivileges, [?SPACE_MANAGE_IN_MARKETPLACE]),
        #{spaceId => S1, initialData => get_space_marketplace_related_data_json(S1)}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId, initialData := PrevData} = _Env, Data) ->
        ExpData = case ShouldSucceed of
            false -> PrevData;
            true -> maps:merge(PrevData, Data)
        end,
        ?assertEqual(ExpData, get_space_marketplace_related_data_json(SpaceId)),

        case {ShouldSucceed, maps:get(<<"advertisedInMarketplace">>, Data, true)} of
            {true, false} -> ?assertNot(in_marketplace(SpaceId));
            _ -> assert_in_marketplace_with_tags(SpaceId, maps:get(<<"tags">>, ExpData))
        end
    end,

    Params = [<<"name">> | ?MARKETPLACE_RELATED_PARAMETERS_NAMES] -- [
        <<"advertisedInMarketplace">>, <<"marketplaceContactEmail">>
    ],

    BuildDataSpec = fun
        (does_not_remove_from_marketplace, ContactEmailModification) ->
            #data_spec{
                required = case ContactEmailModification of
                    does_not_modify_contact_email -> [];
                    modifies_contact_email -> [<<"marketplaceContactEmail">>]
                end,
                at_least_one = Params,
                correct_values = ?CORRECT_PARAMETER_VALUES_FOR_ADVERTISED_SPACE(#{
                    <<"marketplaceContactEmail">> => [<<"b@b.b">>]
                }),
                bad_values = lists:flatten([
                    ?BAD_PARAMETER_VALUES_FOR_ADVERTISED_SPACE,
                    ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
                ])
            };
        (removes_from_marketplace, ContactEmailModification) ->
            #data_spec{
                required = [<<"advertisedInMarketplace">>] ++ case ContactEmailModification of
                    does_not_modify_contact_email -> [];
                    modifies_contact_email -> [<<"marketplaceContactEmail">>]
                end,
                optional = Params,
                correct_values = ?CORRECT_PARAMETER_VALUES_FOR_NON_ADVERTISED_SPACE(#{
                    <<"advertisedInMarketplace">> => [false]
                })
            }
    end,

    RunTests = fun(AdvertisementModification, ContactEmailModification) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = lists:flatten([
                    root,
                    {admin, [?OZ_SPACES_UPDATE]},
                    {user, Owner},
                    case {AdvertisementModification, ContactEmailModification} of
                        {does_not_remove_from_marketplace, does_not_modify_contact_email} ->
                            {user, UserWithUpdatePrivileges};
                        _ ->
                            []
                    end
                ]),
                unauthorized = [nobody],
                forbidden = lists:flatten([
                    {user, UserWithoutPrivileges},
                    {user, UserWithManageMarketplacePrivileges},
                    {user, NonAdmin},
                    case {AdvertisementModification, ContactEmailModification} of
                        {does_not_remove_from_marketplace, does_not_modify_contact_email} ->
                            [];
                        _ ->
                            {user, UserWithUpdatePrivileges}
                    end
                ])
            },
            rest_spec = #rest_spec{
                method = patch,
                path = [<<"/spaces/">>, spaceId],
                expected_code = ?HTTP_204_NO_CONTENT
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = update,
                args = [auth, spaceId, data],
                expected_result = ?OK_RES
            },
            gs_spec = #gs_spec{
                operation = update,
                gri = #gri{type = od_space, id = spaceId, aspect = instance},
                expected_result_op = ?OK_RES
            },
            data_spec = BuildDataSpec(AdvertisementModification, ContactEmailModification)
        },
        api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)
    end,

    ?assert(RunTests(does_not_remove_from_marketplace, does_not_modify_contact_email)),
    ?assert(RunTests(does_not_remove_from_marketplace, modifies_contact_email)),
    ?assert(RunTests(removes_from_marketplace, does_not_modify_contact_email)),
    ?assert(RunTests(removes_from_marketplace, modifies_contact_email)).


delete_test(Config) ->
    Owner = ozt_users:create(),
    U1 = ozt_users:create(),
    U2 = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    EnvSetUpFun = fun() ->
        AdvertisedInMarketplace = ?RAND_BOOL(),
        SpaceData = case AdvertisedInMarketplace of
            true -> ?CORRECT_DATA_FOR_ADVERTISED_SPACE(#{<<"name">> => ?SPACE_NAME1});
            false -> ?SPACE_NAME1
        end,
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(Owner), SpaceData),
        ozt_spaces:add_user(S1, U1, []),
        ozt_spaces:add_user(S1, U2, [?SPACE_DELETE]),
        #{spaceId => S1, in_marketplace => AdvertisedInMarketplace}
    end,
    DeleteEntityFun = fun(#{spaceId := SpaceId} = _Env) ->
        ozt_spaces:delete(SpaceId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{
        spaceId := SpaceId,
        in_marketplace := InMarketplace
    } = _Env, _) ->
        {ok, Spaces} = oz_test_utils:list_spaces(Config),
        ?assertEqual(lists:member(SpaceId, Spaces), not ShouldSucceed),

        ?assertEqual(in_marketplace(SpaceId), InMarketplace andalso not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_DELETE]},
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
            path = [<<"/spaces/">>, spaceId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = delete,
            args = [auth, spaceId],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_space, id = spaceId, aspect = instance},
            expected_result_op = ?OK_RES
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


get_shares_test(Config) ->
    User = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, User, [?SPACE_VIEW], []),

    ExpShares = lists:map(
        fun(_) ->
            ShareId = ?UNIQUE_STRING,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, ShareId, ?ROOT_FILE_ID, S1
            ),
            ShareId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_LIST_RELATIONSHIPS]},
                {user, User}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/shares">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"shares">> => ExpShares}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_shares,
            args = [auth, S1],
            expected_result = ?OK_LIST(ExpShares)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_share_test(Config) ->
    User = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, User, [?SPACE_VIEW], []),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config),
    oz_test_utils:support_space_by_provider(Config, P1, S1),

    ozt:reconcile_entity_graph(),

    ShareId = ?UNIQUE_STRING,
    ShareData = #{
        <<"shareId">> => ShareId,
        <<"name">> => str_utils:rand_hex(12),
        <<"description">> => <<"## Share description">>,
        <<"spaceId">> => S1,
        <<"rootFileId">> => ?ROOT_FILE_ID,
        <<"fileType">> => lists_utils:random_element([dir, file])
    },
    {ok, ShareId} = oz_test_utils:create_share(Config, ?USER(User), ShareData),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SHARES_VIEW]},
                {user, User},
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/shares/">>, ShareId],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:private_share(rest, ShareId, ShareData, ?SUB(user, User))
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_share,
            args = [auth, S1, ShareId],
            expected_result = api_test_expect:private_share(logic, ShareId, ShareData, ?SUB(user, User))
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_share, id = ShareId,
                aspect = instance, scope = private
            },
            auth_hint = ?THROUGH_SPACE(S1),
            expected_result_op = api_test_expect:private_share(gs, ShareId, ShareData, ?SUB(user, User))
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_storages_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_VIEW privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    NonAdmin = ozt_users:create(),
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),

    ExpStorages = lists:map(
        fun(_) ->
            {ok, StorageId} = oz_test_utils:create_storage(
                Config, ?PROVIDER(ProviderId), ?STORAGE_NAME1
            ),
            {ok, S1} = oz_test_utils:support_space(
                Config, ?PROVIDER(ProviderId), StorageId, S1
            ),
            StorageId
        end, lists:seq(1, 5)
    ),

    ozt:reconcile_entity_graph(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_LIST_RELATIONSHIPS]},
                {user, Owner},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_storages,
            args = [auth, S1],
            expected_result = ?OK_LIST(ExpStorages)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:has_storage function
    lists:foreach(fun(StorageId) ->
        ?assert(ozt:rpc(space_logic, is_supported_by_storage, [S1, StorageId]))
    end, ExpStorages),
    ?assert(not ozt:rpc(space_logic, is_supported_by_storage, [S1, <<"asdiucyaie827346w">>])).


create_space_support_token(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_ADD_STORAGE privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_ADD_SUPPORT
    ),
    NonAdmin = ozt_users:create(),

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
            path = [<<"/spaces/">>, S1, <<"/providers/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = create_space_support_token,
            args = [auth, S1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


remove_storage_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_REMOVE_SUPPORT privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_REMOVE_SUPPORT
    ),
    NonAdmin = ozt_users:create(),
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),

    EnvSetUpFun = fun() ->
        {ok, StorageId} = oz_test_utils:create_storage(
            Config, ?PROVIDER(ProviderId), ?STORAGE_NAME1
        ),
        {ok, S1} = oz_test_utils:support_space(
            Config, ?PROVIDER(ProviderId), StorageId, S1
        ),
        #{storageId => StorageId}
    end,
    DeleteEntityFun = fun(#{storageId := StorageId} = _Env) ->
        oz_test_utils:space_remove_storage(Config, S1, StorageId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{storageId := StorageId} = _Env, _) ->
        {ok, Storages} = oz_test_utils:space_get_storages(Config, S1),
        ?assertEqual(lists:member(StorageId, Storages), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_REMOVE_RELATIONSHIPS]},
                {user, Owner},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = remove_storage,
            args = [auth, S1, storageId],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


remove_provider_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_REMOVE_SUPPORT privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_REMOVE_SUPPORT
    ),
    NonAdmin = ozt_users:create(),
    {ok, {P1, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, St3} = oz_test_utils:create_storage(Config, ?PROVIDER(P2), ?STORAGE_NAME1),
    {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P2), St3, S1),
    ozt:reconcile_entity_graph(),

    EnvSetUpFun = fun() ->
        {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
        {ok, St2} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
        {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, S1),
        {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St2, S1),
        ozt:reconcile_entity_graph(),
        #{storageId1 => St1, storageId2 => St2}
    end,
    DeleteEntityFun = fun(_Env) ->
        oz_test_utils:space_remove_provider(Config, S1, P1),
        ozt:reconcile_entity_graph()
    end,
    VerifyEndFun = fun(ShouldSucceed, #{storageId1 := St1, storageId2 := St2} = _Env, _) ->
        {ok, Storages} = oz_test_utils:space_get_storages(Config, S1),
        ?assertEqual(lists:member(St1, Storages), not ShouldSucceed),
        ?assertEqual(lists:member(St2, Storages), not ShouldSucceed),
        ?assertEqual(lists:member(St3, Storages), true)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_REMOVE_RELATIONSHIPS]},
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
            path = [<<"/spaces/">>, S1, <<"/providers/">>, P1],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = remove_provider,
            args = [auth, S1, P1],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


submit_membership_request_test(Config) ->
    ProviderId = ozt_providers:create(),
    RequesterId = ozt_users:create(),

    EnvSetUpFun = fun() ->
        #{space_id => ozt_spaces:create_advertised()}
    end,

    VerifyFun = fun(SpaceId, Data, RequestId) ->
        ContactEmail = maps:get(<<"contactEmail">>, Data),
        #od_user{space_membership_requests = Record} = ozt_users:get(RequesterId),
        ?assertEqual(ContactEmail, ozt:rpc(space_membership_requests, lookup_email_for_pending, [SpaceId, RequestId, Record]))
    end,

    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, RequesterId}
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, ProviderId}
            ]
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = submit_membership_request,
            args = [auth, space_id, data],
            expected_result = ?OK_ENV(fun(#{space_id := SpaceId}, Data) ->
                ?OK_TERM(fun(RequestId) -> VerifyFun(SpaceId, Data, RequestId) end)
            end)
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/spaces/marketplace/">>, space_id, <<"/request">>],
            expected_code = ?HTTP_200_OK,
            expected_body = ?OK_ENV(fun(#{space_id := SpaceId}, Data) ->
                fun(#{<<"requestId">> := RequestId}) ->
                    VerifyFun(SpaceId, Data, RequestId),
                    true
                end
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_space, id = space_id, aspect = membership_request},
            expected_result_gui = ?OK_ENV(fun(#{space_id := SpaceId}, Data) ->
                ?OK_TERM(fun(#{<<"requestId">> := RequestId}) -> VerifyFun(SpaceId, Data, RequestId) end)
            end)
        },
        data_spec = #data_spec{
            required = [<<"contactEmail">>],
            optional = [<<"message">>],
            correct_values = #{
                <<"contactEmail">> => [?RAND_EMAIL_ADDRESS()],
                <<"message">> => [<<"">>, ?RAND_STR(), ?RAND_STR(2000)]
            },
            bad_values = [
                {<<"contactEmail">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_BINARY(<<"contactEmail">>)},
                {<<"contactEmail">>, <<"emailwithoutdomain">>, ?ERROR_BAD_VALUE_EMAIL},
                {<<"message">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"message">>)},
                {<<"message">>, ?RAND_UNICODE_STR(2001), ?ERROR_BAD_VALUE_TEXT_TOO_LARGE(<<"message">>, 2000)}
            ]
        }
    }, EnvSetUpFun, undefined, undefined)).


get_membership_requester_info_test(Config) ->
    ProviderId = ozt_providers:create(),
    RequesterId = ozt_users:create(#{
        <<"fullName">> => FullName = ?RAND_STR(),
        <<"username">> => Username = ?RAND_STR()
    }),
    SpaceOperatorId = ozt_users:create(),
    SpaceMemberWithoutPrivsId = ozt_users:create(),
    SpaceId = ozt_users:create_advertised_space_for(SpaceOperatorId),
    ozt_spaces:add_user(SpaceId, SpaceMemberWithoutPrivsId, privileges:space_admin() -- [?SPACE_MANAGE_IN_MARKETPLACE]),

    ContactEmail = ?RAND_EMAIL_ADDRESS(),
    RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId, ContactEmail),

    ExpectedInfo = #{
        <<"userId">> => RequesterId,
        <<"fullName">> => FullName,
        <<"username">> => Username,
        <<"contactEmail">> => ContactEmail
    },

    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW]},
                {user, SpaceOperatorId}
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, ProviderId},
                {user, SpaceMemberWithoutPrivsId}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/marketplace/">>, SpaceId, <<"/request/">>, RequestId, <<"/requester_info">>],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpectedInfo
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_membership_requester_info,
            args = [auth, SpaceId, RequestId],
            expected_result = ?OK_MAP(ExpectedInfo)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_space, id = SpaceId, aspect = {membership_requester_info, RequestId}},
            expected_result_op = ?ERROR_REASON(?ERROR_FORBIDDEN),
            expected_result_gui = ?OK_MAP_CONTAINS(ExpectedInfo)
        }
    })),

    ?assertEqual(?ERROR_BAD_VALUE_IDENTIFIER(<<"requestId">>), ozt:rpc(
        space_logic, get_membership_requester_info, [?USER(SpaceOperatorId), SpaceId, <<"abc">>]
    )),
    ?assertEqual(?ERROR_NOT_FOUND, ozt:rpc(
        space_logic, get_membership_requester_info, [?USER(SpaceOperatorId), SpaceId, <<"baduserid-xyz">>]
    )).


resolve_membership_request_test(Config) ->
    ProviderId = ozt_providers:create(),
    RequesterId = ozt_users:create(),
    SpaceOperatorId = ozt_users:create(),
    SpaceMemberWithoutPrivsId = ozt_users:create(),

    lists:foreach(fun(Decision) ->
        EnvSetUpFun = fun() ->
            SpaceId = ozt_users:create_advertised_space_for(SpaceOperatorId),
            RequiredPrivs = case Decision of
                grant -> [?SPACE_MANAGE_IN_MARKETPLACE, ?SPACE_ADD_USER];
                reject -> [?SPACE_MANAGE_IN_MARKETPLACE]
            end,
            ozt_spaces:add_user(SpaceId, SpaceMemberWithoutPrivsId, privileges:space_admin() -- RequiredPrivs),
            RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId),
            #{space_id => SpaceId, request_id => RequestId}
        end,

        ?assert(api_test_utils:run_tests(Config, #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    {user, SpaceOperatorId}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, RequesterId},
                    {user, SpaceMemberWithoutPrivsId},
                    {provider, ProviderId}
                ]
            },
            rest_spec = #rest_spec{
                method = post,
                path = [<<"/spaces/marketplace/">>, space_id, <<"/request/">>, request_id, <<"/resolve">>],
                expected_code = ?HTTP_204_NO_CONTENT
            },
            logic_spec = #logic_spec{
                module = space_logic,
                function = resolve_membership_request,
                args = [auth, space_id, request_id, data],
                expected_result = ?OK
            },
            gs_spec = #gs_spec{
                operation = create,
                gri = #gri{type = od_space, id = space_id, aspect = {resolve_membership_request, request_id}},
                expected_result_gui = ?OK
            },
            data_spec = #data_spec{
                required = [
                    <<"decision">>
                ],
                correct_values = #{
                    <<"decision">> => [Decision]
                },
                bad_values = [
                    {<<"decision">>, not_sure, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"decision">>, [grant, reject])},
                    {<<"decision">>, 1234, ?ERROR_BAD_VALUE_ATOM(<<"decision">>)}
                ]
            }
        }, EnvSetUpFun, undefined, undefined))
    end, [grant, reject]),

    SpaceId = ozt_users:create_advertised_space_for(SpaceOperatorId),
    ?assertEqual(
        ?ERROR_BAD_VALUE_EMPTY(<<"requestId">>),
        ozt_spaces:try_resolve_membership_request(SpaceId, <<"">>, ?RAND_ELEMENT([grant, reject]))
    ),
    ?assertEqual(
        ?ERROR_BAD_VALUE_BINARY(<<"requestId">>),
        ozt_spaces:try_resolve_membership_request(SpaceId, [1, 2, 3], ?RAND_ELEMENT([grant, reject]))
    ).


list_effective_providers_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_VIEW privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    NonAdmin = ozt_users:create(),

    ExpProviders = lists:map(
        fun(_) ->
            {ok, {ProviderId, _}} = oz_test_utils:create_provider(
                Config, ?PROVIDER_NAME1
            ),
            {ok, S1} = oz_test_utils:support_space_by_provider(Config, ProviderId, S1),
            ProviderId
        end, lists:seq(1, 5)
    ),

    ozt:reconcile_entity_graph(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_LIST_RELATIONSHIPS]},
                {user, Owner},
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
            path = [<<"/spaces/">>, S1, <<"/providers">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"providers">> => ExpProviders}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_eff_providers,
            args = [auth, S1],
            expected_result = ?OK_LIST(ExpProviders)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:has_provider function
    lists:foreach(fun(ProviderId) ->
        ?assert(ozt:rpc(space_logic, is_supported_by_provider, [S1, ProviderId]))
    end, ExpProviders),
    ?assert(not ozt:rpc(space_logic, is_supported_by_provider, [S1, <<"asdiucyaie827346w">>])).


get_eff_provider_test(Config) ->
    User = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    ProviderDetails = ?PROVIDER_DETAILS(?PROVIDER_NAME1),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(
        Config, ProviderDetails#{<<"subdomainDelegation">> => false}
    ),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    {ok, {P3, P3Token}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),

    SpacePrivs = privileges:space_privileges(),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, User, [], SpacePrivs),

    {ok, S1} = oz_test_utils:support_space_by_provider(Config, P1, S1),
    {ok, S1} = oz_test_utils:support_space_by_provider(Config, P2, S1),

    ozt:reconcile_entity_graph(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_PROVIDERS_VIEW]},
                {user, User},
                {provider, P2, P2Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P3, P3Token}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/providers/">>, P1],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:protected_provider(rest, P1, ProviderDetails)
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_provider,
            args = [auth, S1, P1],
            expected_result = api_test_expect:protected_provider(logic, P1, ProviderDetails)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_provider, id = P1,
                aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_SPACE(S1),
            expected_result_op = api_test_expect:protected_provider(gs, P1, ProviderDetails)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % When making connection with gs provider becomes online
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, P1Token}]
        },
        gs_spec = GsSpec#gs_spec{
            expected_result_op = api_test_expect:protected_provider(gs, P1, ProviderDetails#{<<"online">> => true})
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


update_support_parameters_test(Config) ->
    ClusterMemberWithPrivs = ozt_users:create(),
    ClusterMemberWithoutPrivs = ozt_users:create(),
    SpaceMemberWithPrivs = ozt_users:create(),
    SpaceMemberWithoutPrivs = ozt_users:create(),
    UnrelatedUser = ozt_users:create(),

    SubjectProvider = ozt_providers:create_for_admin_user(ClusterMemberWithPrivs),
    ozt_clusters:add_user(SubjectProvider, ClusterMemberWithoutPrivs, privileges:cluster_admin() -- [?CLUSTER_UPDATE]),
    OtherProvider = ozt_providers:create(),

    SubjectSpace = ozt_users:create_space_for(SpaceMemberWithPrivs),
    ozt_spaces:add_user(SubjectSpace, SpaceMemberWithoutPrivs, privileges:space_admin() -- [?SPACE_UPDATE]),
    ozt_providers:support_space(SubjectProvider, SubjectSpace),
    ozt_providers:support_space(OtherProvider, SubjectSpace),

    ozt_providers:simulate_version(SubjectProvider, ?LINE_21_02),
    ozt_spaces:set_support_parameters(SubjectSpace, SubjectProvider, ozt_spaces:random_support_parameters()),

    EnvSetUpFun = fun() ->
        #{
            previous_support_parameters => ozt_spaces:get_support_parameters(SubjectSpace, SubjectProvider)
        }
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            % the invalid clients are tested first, the correct clients are tested further on
            correct = [],
            unauthorized = [nobody],
            forbidden = [
                {user, ClusterMemberWithoutPrivs},
                {user, SpaceMemberWithoutPrivs},
                {user, UnrelatedUser},
                {provider, OtherProvider}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = patch,
            path = [
                <<"/spaces/">>, SubjectSpace, <<"/providers/">>, SubjectProvider, <<"/support_parameters">>
            ],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = LogicSpec = #logic_spec{
            module = space_logic,
            function = update_support_parameters,
            args = [auth, SubjectSpace, SubjectProvider, data],
            expected_result = ?OK_RES
        },
        gs_spec = GsSpec = #gs_spec{
            operation = update,
            gri = #gri{type = od_space, id = SubjectSpace, aspect = {support_parameters, SubjectProvider}},
            expected_result_op = ?OK_RES
        },
        data_spec = DataSpec = #data_spec{
            at_least_one = [<<"accountingEnabled">>, <<"dirStatsServiceEnabled">>, <<"dirStatsServiceStatus">>],
            correct_values = #{
                <<"accountingEnabled">> => [true, false],
                <<"dirStatsServiceEnabled">> => [true, false],
                <<"dirStatsServiceStatus">> => [atom_to_binary(S) || S <- support_parameters:all_dir_stats_service_statuses()]
            },
            bad_values = BadValues = [
                {<<"accountingEnabled">>, atom, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"accountingEnabled">>, [true, false, null])},
                {<<"dirStatsServiceEnabled">>, [1, 2, 3], ?ERROR_BAD_VALUE_ATOM(<<"dirStatsServiceEnabled">>)},
                {<<"dirStatsServiceStatus">>, <<"text">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(
                    <<"dirStatsServiceStatus">>, [null] ++ [atom_to_binary(S) || S <- support_parameters:all_dir_stats_service_statuses()]
                )}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, undefined)),

    CorrectClients = [
        root,
        {admin, [?OZ_SPACES_UPDATE]},
        {user, ClusterMemberWithPrivs},
        {user, SpaceMemberWithPrivs},
        {provider, SubjectProvider}
    ],
    ShouldBeAuthorized = fun(Client, Data) ->
        case {Client, Data} of
            {root, _} -> true;
            {{admin, [?OZ_SPACES_UPDATE]}, #{<<"dirStatsServiceStatus">> := V}} when V /= null -> false;
            {{admin, [?OZ_SPACES_UPDATE]}, _} -> true;
            {{user, ClusterMemberWithPrivs}, #{<<"dirStatsServiceStatus">> := V}} when V /= null -> false;
            {{user, ClusterMemberWithPrivs}, _} -> true;
            {{user, SpaceMemberWithPrivs}, #{<<"accountingEnabled">> := V}} when V /= null -> false;
            {{user, SpaceMemberWithPrivs}, #{<<"dirStatsServiceStatus">> := V}} when V /= null -> false;
            {{user, SpaceMemberWithPrivs}, _} -> true;
            {{provider, SubjectProvider}, _} -> true
        end
    end,
    InferExpectedOutcome = fun(Client, PreviousSupportParameters, Data, <<MajorProviderVersion:2/binary, _/binary>>) ->
        case ShouldBeAuthorized(Client, Data) of
            false ->
                {failure, ?ERROR_FORBIDDEN};
            true ->
                case binary_to_integer(MajorProviderVersion) >= 21 of
                    true ->
                        FinalAccountingEnabled = maps:get(
                            <<"accountingEnabled">>, Data,
                            PreviousSupportParameters#support_parameters.accounting_enabled
                        ),
                        FinalDirStatsServiceEnabled = maps:get(
                            <<"dirStatsServiceEnabled">>, Data,
                            PreviousSupportParameters#support_parameters.dir_stats_service_enabled
                        ),
                        case FinalAccountingEnabled =:= true andalso FinalDirStatsServiceEnabled =:= false of
                            true ->
                                {failure, ?ERROR_BAD_DATA(
                                    <<"dirStatsServiceEnabled">>,
                                    <<"Dir stats service must be enabled if accounting is enabled">>
                                )};

                            false ->
                                success
                        end;
                    false ->
                        {failure, ?ERROR_NOT_SUPPORTED}
                end
        end
    end,

    lists:foreach(fun(ProviderVersion) ->
        ozt_providers:simulate_version(SubjectProvider, ProviderVersion),
        lists:foreach(fun(CorrectClient) ->
            VerifyEndFun = fun(ShouldSucceed, #{previous_support_parameters := PreviousSupportParameters}, Data) ->
                ActualSupportParameters = ozt_spaces:get_support_parameters(SubjectSpace, SubjectProvider),
                ExpectedOutcome = InferExpectedOutcome(CorrectClient, PreviousSupportParameters, Data, ProviderVersion),
                ExpectedSupportParameters = case {ExpectedOutcome, ShouldSucceed} of
                    {{failure, _}, _} ->
                        PreviousSupportParameters;
                    {_, false} ->
                        PreviousSupportParameters;
                    {success, true} ->
                        ozt_spaces:expected_tweaked_support_parameters(PreviousSupportParameters#support_parameters{
                            accounting_enabled = maps:get(
                                <<"accountingEnabled">>, Data, PreviousSupportParameters#support_parameters.accounting_enabled
                            ),
                            dir_stats_service_enabled = maps:get(
                                <<"dirStatsServiceEnabled">>, Data, PreviousSupportParameters#support_parameters.dir_stats_service_enabled
                            ),
                            dir_stats_service_status = case maps:find(<<"dirStatsServiceStatus">>, Data) of
                                {ok, Value} -> binary_to_atom(Value);
                                error -> PreviousSupportParameters#support_parameters.dir_stats_service_status
                            end
                        })
                end,
                ?assertEqual(ActualSupportParameters, ExpectedSupportParameters)
            end,

            ?assert(api_test_utils:run_tests(Config, ApiTestSpec#api_test_spec{
                client_spec = #client_spec{
                    correct = [CorrectClient]
                },
                rest_spec = RestSpec#rest_spec{
                    expected_code = ?OK_ENV(fun(#{previous_support_parameters := PreviousSupportParameters}, Data) ->
                        case InferExpectedOutcome(CorrectClient, PreviousSupportParameters, Data, ProviderVersion) of
                            success -> ?HTTP_204_NO_CONTENT;
                            {failure, Error} -> errors:to_http_code(Error)
                        end
                    end)
                },
                logic_spec = LogicSpec#logic_spec{
                    expected_result = ?OK_ENV(fun(#{previous_support_parameters := PreviousSupportParameters}, Data) ->
                        case InferExpectedOutcome(CorrectClient, PreviousSupportParameters, Data, ProviderVersion) of
                            success -> ?OK_RES;
                            {failure, Error} -> ?ERROR_REASON(Error)
                        end
                    end)
                },
                gs_spec = GsSpec#gs_spec{
                    expected_result_op = ?OK_ENV(fun(#{previous_support_parameters := PreviousSupportParameters}, Data) ->
                        case InferExpectedOutcome(CorrectClient, PreviousSupportParameters, Data, ProviderVersion) of
                            success -> ?OK_RES;
                            {failure, Error} -> ?ERROR_REASON(Error)
                        end
                    end)
                },
                data_spec = DataSpec#data_spec{
                    bad_values = case CorrectClient of
                        % test bad values only for root, for other clients authorization depends on the values and
                        % it is impossible to handle these combinations using the test framework
                        root -> BadValues;
                        _ -> []
                    end
                }
            }, EnvSetUpFun, undefined, VerifyEndFun))
        end, CorrectClients)
    end, [?LINE_19_02, ?LINE_20_02, ?LINE_21_02]).

% ----------------
% sequential tests

get_marketplace_data_error_marketplace_disabled_test(Config) ->
    Owner = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    SubjectSpace = ozt_users:create_advertised_space_for(Owner),
    SupportingProvider = ozt_providers:create_as_support_for_space(SubjectSpace),

    ozt:set_env(space_marketplace_enabled, false),

    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {admin, [?OZ_SPACES_VIEW]},
                {user, Owner},
                {user, NonAdmin},
                {provider, SupportingProvider}
            ],
            unauthorized = [],
            forbidden = []
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/marketplace/">>, SubjectSpace],
            expected_code = ?HTTP_400_BAD_REQUEST,
            expected_body = #{<<"error">> => errors:to_json(?ERROR_SPACE_MARKETPLACE_DISABLED)}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_marketplace_data,
            args = [auth, SubjectSpace],
            expected_result = ?ERROR_REASON(?ERROR_SPACE_MARKETPLACE_DISABLED)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_space, id = SubjectSpace, aspect = marketplace_data, scope = protected},
            expected_result_op = ?ERROR_REASON(?ERROR_SPACE_MARKETPLACE_DISABLED),
            expected_result_gui = ?ERROR_REASON(?ERROR_SPACE_MARKETPLACE_DISABLED)
        }
    })).


list_test(Config) ->
    % Make sure that spaces created in other tests are deleted.
    ozt:delete_all_entities(),

    U1 = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S3} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S4} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S5} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    ExpSpaces = [S1, S2, S3, S4, S5],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_LIST]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/spaces">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = list,
            args = [auth],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:exist function
    lists:foreach(
        fun(SpaceId) -> ?assert(ozt:rpc(space_logic, exists, [SpaceId])) end,
        ExpSpaces
    ),
    ?assert(not ozt:rpc(space_logic, exists, [<<"asdiucyaie827346w">>])).


list_marketplace_test(Config) ->
    % Make sure that spaces created in other tests are deleted.
    ozt:delete_all_entities(),

    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    MarketplaceSpaces = create_advertised_spaces(200, Creator) ++ create_non_advertised_spaces(200, Creator),

    FilterMarketplaceSpacesFun = fun(Data, Type) ->
        ExpEntries0 = case maps:get(<<"tags">>, Data, all) of
            all ->
                MarketplaceSpaces;
            [] ->
                [];
            Tags ->
                lists:filter(fun(ExpectedEntry) ->
                    lists_utils:intersect(Tags, maps:get(<<"tags">>, ExpectedEntry)) /= []
                end, MarketplaceSpaces)
        end,

        Offset0 = maps:get(<<"offset">>, Data, 0),
        {Index, Offset1} = case maps:get(<<"token">>, Data, undefined) of
            undefined ->
                {maps:get(<<"index">>, Data, <<>>), Offset0};
            Token when is_binary(Token) ->
                {http_utils:base64url_decode(Token), Offset0 + 1}
        end,
        ExpEntries1 = case Offset1 >= 0 of
            true ->
                lists:nthtail(Offset1, lists:dropwhile(fun(ExpectedEntry) ->
                    maps:get(<<"name">>, ExpectedEntry) < Index
                end, ExpEntries0));
            false ->
                {LtIndexEntries0, GteIndexEntries} = lists:partition(fun(ExpectedEntry) ->
                    maps:get(<<"name">>, ExpectedEntry) < Index
                end, ExpEntries0),
                LtIndexEntries1 = lists:reverse(lists:sublist(
                    lists:reverse(LtIndexEntries0), abs(Offset1)
                )),
                LtIndexEntries1 ++ GteIndexEntries
        end,

        Limit = maps:get(<<"limit">>, Data, 1000),
        ExpEntries2 = lists:sublist(ExpEntries1, Limit),

        ExpEntries3 = case Type of
            basic ->
                lists:map(fun(ExpectedEntry) ->
                    SpaceName = maps:get(<<"name">>, ExpectedEntry),
                    SpaceId = maps:get(<<"spaceId">>, ExpectedEntry),
                    #{
                        <<"index">> => space_marketplace:index(SpaceName, SpaceId),
                        <<"spaceId">> => SpaceId
                    }
                end, ExpEntries2);
            extended ->
                lists:map(fun(ExpectedEntry) ->
                    SpaceName = maps:get(<<"name">>, ExpectedEntry),
                    SpaceId = maps:get(<<"spaceId">>, ExpectedEntry),
                    #{
                        <<"spaceId">> => SpaceId,
                        <<"name">> => SpaceName,
                        <<"description">> => maps:get(<<"description">>, ExpectedEntry),
                        <<"organizationName">> => maps:get(<<"organizationName">>, ExpectedEntry),
                        <<"tags">> => maps:get(<<"tags">>, ExpectedEntry),
                        <<"index">> => space_marketplace:index(SpaceName, SpaceId),
                        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
                        <<"totalSupportSize">> => 0,
                        <<"providerNames">> => []
                    }
                end, ExpEntries2)
        end,
        IsLast = length(ExpEntries3) < Limit,
        NextPageToken = case IsLast of
            true -> undefined;
            false -> http_utils:base64url_encode(maps:get(<<"index">>, lists:last(ExpEntries3)))
        end,

        {ExpEntries3, IsLast, NextPageToken}
    end,

    ApiTestSpecForBasicListing = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_LIST]},
                {user, NonAdmin},
                {user, Creator}
            ],
            unauthorized = [nobody],
            forbidden = []
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/spaces/marketplace/list">>,
            expected_code = ?HTTP_200_OK,
            expected_body = ?OK_ENV(fun(_Env, Data) ->
                {ExpEntries, IsLast, NextPageToken} = FilterMarketplaceSpacesFun(Data, basic),
                #{
                    <<"spaces">> => ExpEntries,
                    <<"isLast">> => IsLast,
                    <<"nextPageToken">> => utils:undefined_to_null(NextPageToken)
                }
            end)
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = list_marketplace,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(_, Data) ->
                ExpResult = FilterMarketplaceSpacesFun(Data, basic),
                ?OK_TERM(fun(Result) -> ?assertEqual(ExpResult, Result) end)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_space, aspect = list_marketplace, scope = protected},
            expected_result_gui = ?OK_ENV(fun(_Env, Data) ->
                {ExpEntries, IsLast, _NextPageToken} = FilterMarketplaceSpacesFun(Data, basic),
                ?OK_MAP(#{<<"list">> => ExpEntries, <<"isLast">> => IsLast})
            end)
        },
        data_spec = #data_spec{
            optional = [
                <<"index">>,
                <<"token">>,
                <<"offset">>,
                <<"limit">>,
                <<"tags">>
            ],
            correct_values = #{
                <<"index">> => [<<"space_1100">>, <<>>, null],
                <<"token">> => [http_utils:base64url_encode(<<"space_1200">>)],
                <<"offset">> => [-100, -10, 0, 10],
                <<"limit">> => [150, 50, 10, 2],
                <<"tags">> => [?RAND_SUBLIST(ozt_spaces:available_space_tags()), []]
            },
            bad_values = [
                {<<"index">>, 10, ?ERROR_BAD_VALUE_BINARY(<<"index">>)},
                {<<"token">>, 10, ?ERROR_BAD_VALUE_BINARY(<<"token">>)},
                {<<"token">>, <<>>, ?ERROR_BAD_VALUE_EMPTY(<<"token">>)},
                {<<"offset">>, <<"a">>, ?ERROR_BAD_VALUE_INTEGER(<<"offset">>)},
                {<<"limit">>, <<"a">>, ?ERROR_BAD_VALUE_INTEGER(<<"limit">>)},
                {<<"limit">>, 0, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"limit">>, 1, 1000)},
                {<<"tags">>, [<<"troll">>], ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"tags">>, ozt_spaces:available_space_tags())}
            ]
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpecForBasicListing)),

    ApiTestSpecForExtendedListing = ApiTestSpecForBasicListing#api_test_spec{
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = space_logic,
            function = list_marketplace_with_data,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(_, Data) ->
                ExpResult = FilterMarketplaceSpacesFun(Data, extended),
                ?OK_TERM(fun(Result) -> ?assertEqual(ExpResult, Result) end)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_space, aspect = list_marketplace_with_data, scope = protected},
            expected_result_gui = ?OK_ENV(fun(_Env, Data) ->
                {ExpEntries, IsLast, _NextPageToken} = FilterMarketplaceSpacesFun(Data, extended),
                ?OK_MAP(#{<<"list">> => ExpEntries, <<"isLast">> => IsLast})
            end)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpecForExtendedListing)).


list_marketplace_error_marketplace_disabled_test(Config) ->
    Creator = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    create_advertised_spaces(20, Creator),
    ozt:set_env(space_marketplace_enabled, false),

    ApiTestSpecForBasicListing = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {admin, [?OZ_SPACES_LIST]},
                {user, NonAdmin},
                {user, Creator}
            ],
            unauthorized = [],
            forbidden = []
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/spaces/marketplace/list">>,
            expected_code = ?HTTP_400_BAD_REQUEST,
            expected_body = #{<<"error">> => errors:to_json(?ERROR_SPACE_MARKETPLACE_DISABLED)}
        },
        logic_spec = LogicSpec = #logic_spec{
            module = space_logic,
            function = list_marketplace,
            args = [auth, data],
            expected_result = ?ERROR_REASON(?ERROR_SPACE_MARKETPLACE_DISABLED)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = create,
            gri = #gri{type = od_space, aspect = list_marketplace, scope = protected},
            expected_result_op = ?ERROR_REASON(?ERROR_SPACE_MARKETPLACE_DISABLED),
            expected_result_gui = ?ERROR_REASON(?ERROR_SPACE_MARKETPLACE_DISABLED)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpecForBasicListing)),

    ApiTestSpecForExtendedListing = ApiTestSpecForBasicListing#api_test_spec{
        rest_spec = undefined,
        logic_spec = LogicSpec#logic_spec{function = list_marketplace_with_data},
        gs_spec = GsSpec#gs_spec{
            gri = #gri{type = od_space, aspect = list_marketplace_with_data, scope = protected}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpecForExtendedListing)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec assert_in_marketplace_with_tags(od_space:id(), all | [od_space:tag()]) ->
    ok | no_return().
assert_in_marketplace_with_tags(SpaceId, []) ->
    % Even with no tags space will be at least in all spaces tree
    ?assert(in_marketplace(all, SpaceId)),
    ?assertNot(in_marketplace(ozt_spaces:available_space_tags(), SpaceId));
assert_in_marketplace_with_tags(SpaceId, NewTags) ->
    ?assert(in_marketplace(all, SpaceId)),
    ?assert(in_marketplace(NewTags, SpaceId)),
    ?assertNot(in_marketplace(ozt_spaces:available_space_tags() -- NewTags, SpaceId)).


%% @private
-spec in_marketplace(od_space:id()) -> boolean().
in_marketplace(SpaceId) ->
    in_marketplace(all, SpaceId).


%% @private
-spec in_marketplace(all | [od_space:tag()], od_space:id()) -> boolean().
in_marketplace(SpaceTags, SpaceId) ->
    lists:member(SpaceId, ozt_spaces:list_marketplace(SpaceTags)).


%% @private
-spec get_space_marketplace_related_data_json(od_space:id()) -> json_utils:json_map().
get_space_marketplace_related_data_json(SpaceId) ->
    Space = ozt_spaces:get(SpaceId),

    #{
        <<"name">> => Space#od_space.name,
        <<"description">> => Space#od_space.description,
        <<"organizationName">> => Space#od_space.organization_name,
        <<"tags">> => Space#od_space.tags,
        <<"advertisedInMarketplace">> => Space#od_space.advertised_in_marketplace,
        <<"marketplaceContactEmail">> => Space#od_space.marketplace_contact_email
    }.


%% @private
-spec create_advertised_spaces(number, creator) -> list().
create_advertised_spaces(Number, Creator) ->
    lists:reverse(lists:foldl(fun(Num, Acc) ->
        SpaceName = str_utils:format_bin("space_~B", [1000 + Num]),
        SpaceTags = ?RAND_SUBLIST(ozt_spaces:available_space_tags()),
        Description = ?RAND_STR(),
        OrganizationName = ?RAND_STR(),
        SpaceId = ozt_users:create_advertised_space_for(Creator, #{
            <<"name">> => SpaceName,
            <<"description">> => Description,
            <<"organizationName">> => OrganizationName,
            <<"tags">> => SpaceTags
        }),
        [#{
            <<"spaceId">> => SpaceId,
            <<"name">> => SpaceName,
            <<"description">> => Description,
            <<"organizationName">> => OrganizationName,
            <<"tags">> => SpaceTags
        } | Acc]

  end, [], lists:seq(1, Number))).


%% @private
-spec create_non_advertised_spaces(number, creator) -> list().
create_non_advertised_spaces(Number, Creator) ->
    lists:reverse(lists:foldl(fun(Num, Acc) ->
        SpaceName = str_utils:format_bin("space_~B", [1000 + Num]),
        ozt_users:create_space_for(Creator, SpaceName),
        Acc
    end, [],   lists:seq(1, Number))).

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

init_per_group(_Group, Config) ->
    ozt_mailer:mock(),
    ozt_mocks:freeze_time(),
    Config.

end_per_group(_Group, Config) ->
    ozt_mailer:unmock(),
    ozt_mocks:unfreeze_time(),
    Config.

end_per_testcase(_, Config) ->
    ozt:set_env(space_marketplace_enabled, true),
    Config.

