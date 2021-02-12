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
-include_lib("ctool/include/space_support/support_stage.hrl").
-include_lib("ctool/include/space_support/provider_sync_progress.hrl").
-include_lib("ctool/include/space_support/provider_capacity_usage.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    create_test/1,
    list_test/1,
    list_privileges_test/1,
    get_test/1,
    update_test/1,
    delete_test/1,

    list_shares_test/1,
    get_share_test/1,

    list_storages_test/1,
    create_space_support_token/1,
    remove_storage_test/1,
    remove_provider_test/1,

    list_effective_providers_test/1,
    get_eff_provider_test/1,

    update_support_parameters_test/1,
    update_provider_capacity_usage_test/1,
    update_provider_sync_progress_test/1,
    get_latest_emitted_seq_test/1,

    get_stats_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        list_privileges_test,
        get_test,
        update_test,
        delete_test,

        list_shares_test,
        get_share_test,

        list_storages_test,
        create_space_support_token,
        remove_storage_test,
        remove_provider_test,

        list_effective_providers_test,
        get_eff_provider_test,

        update_support_parameters_test,
        update_provider_capacity_usage_test,
        update_provider_sync_progress_test,
        get_latest_emitted_seq_test,

        get_stats_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


create_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),

    VerifyFun = fun(SpaceId) ->
        {ok, Space} = oz_test_utils:get_space(Config, SpaceId),
        ?assertEqual(?CORRECT_NAME, Space#od_space.name),
        true
    end,

    ApiTestSpec = #api_test_spec{
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
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/spaces/">>]),
                [SpaceId] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(SpaceId)
            end
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_space, aspect = instance},
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?CORRECT_NAME,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    VerifyFun(Id)
                end
            })
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            correct_values = #{<<"name">> => [?CORRECT_NAME]},
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_test(Config) ->
    % Make sure that spaces created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

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
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:exist function
    lists:foreach(
        fun(SpaceId) ->
            ?assert(oz_test_utils:call_oz(
                Config, space_logic, exists, [SpaceId])
            )
        end, ExpSpaces
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, space_logic, exists, [<<"asdiucyaie827346w">>])
    ).


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
    {ok, Owner} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:space_privileges(),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(Owner), ?SPACE_NAME1),
    oz_test_utils:space_add_user(Config, S1, U1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1,
        AllPrivs -- [?SPACE_VIEW], [?SPACE_VIEW]
    ),
    oz_test_utils:space_add_user(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2,
        [?SPACE_VIEW], AllPrivs -- [?SPACE_VIEW]
    ),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config),
    SupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
    {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, S1, SupportSize),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    ExpSupportParametersRegistry = #{
        P1 => support_parameters:build(global, eager)
    },
    ExpSupportParametersRegistryJson = #{
        P1 => #{<<"dataWrite">> => <<"global">>, <<"metadataReplication">> => <<"eager">>}
    },
    ExpSupportStageRegistry = #{
        P1 => #support_stage_details{
            provider_stage = active,
            per_storage = #{
                St1 => active
            }
        }},
    ExpSupportStageRegistryJson = #{
        P1 => #{
            <<"providerStage">> => <<"active">>,
            <<"perStorage">> => #{
                St1 => <<"active">>
            }
        }
    },

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Owner},
                {user, U2},
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {admin, [?OZ_SPACES_VIEW]},
                {user, NonAdmin},
                {user, U1}
            ]
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get,
            args = [auth, S1],
            expected_result = ?OK_TERM(
                fun(#od_space{
                    name = Name, users = Users, groups = #{},
                    storages = Storages, shares = [],
                    harvesters = [],
                    eff_users = EffUsers, eff_groups = #{},
                    eff_providers = EffProviders,
                    support_parameters_registry = SupportParameters,
                    support_stage_registry = SupportStage,
                    top_down_dirty = false, bottom_up_dirty = false
                }) ->
                    ?assertEqual(?SPACE_NAME1, Name),
                    ?assertEqual(Users, #{
                        Owner => AllPrivs,
                        U1 => AllPrivs -- [?SPACE_VIEW],
                        U2 => [?SPACE_VIEW]}
                    ),
                    ?assertEqual(EffUsers, #{
                        Owner => {AllPrivs, [{od_space, <<"self">>}]},
                        U1 => {AllPrivs -- [?SPACE_VIEW], [{od_space, <<"self">>}]},
                        U2 => {[?SPACE_VIEW], [{od_space, <<"self">>}]}
                    }),
                    ?assertEqual(Storages, #{St1 => SupportSize}),
                    ?assertEqual(EffProviders, #{P1 => {SupportSize, [{od_storage, St1}]}}),
                    ?assertEqual(SupportParameters, ExpSupportParametersRegistry),
                    ?assertEqual(SupportStage, ExpSupportStageRegistry)
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_space, id = S1, aspect = instance},
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?SPACE_NAME1,
                <<"owners">> => [Owner],
                <<"users">> => #{
                    Owner => AllPrivsBin,
                    U1 => AllPrivsBin -- [<<"space_view">>],
                    U2 => [<<"space_view">>]
                },
                <<"groups">> => #{},
                <<"shares">> => [],
                <<"providers">> => #{P1 => SupportSize},
                <<"harvesters">> => [],
                <<"effectiveUsers">> => #{
                    Owner => AllPrivsBin,
                    U1 => AllPrivsBin -- [<<"space_view">>],
                    U2 => [<<"space_view">>]
                },
                <<"effectiveGroups">> => #{},
                <<"supportParametersRegistry">> => ExpSupportParametersRegistryJson,
                <<"supportStageRegistry">> => ExpSupportStageRegistryJson,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(S1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check protected data
    SpaceData = #{<<"name">> => ?SPACE_NAME1, <<"providers">> => #{P1 => #{St1 => SupportSize}}},
    GetProtectedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW]},
                {user, Owner},
                {user, U1},
                {user, U2},
                {provider, P1, P1Token}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:protected_space(rest, S1, SpaceData, ?SUB(user, Owner))
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_protected_data,
            args = [auth, S1],
            expected_result = api_test_expect:protected_space(logic, S1, SpaceData, ?SUB(user, Owner))
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_space, id = S1, aspect = instance, scope = protected
            },
            expected_result = api_test_expect:protected_space(gs, S1, SpaceData, ?SUB(user, Owner))
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)).


update_test(Config) ->
    {ok, Owner} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(Owner), ?CORRECT_NAME),
        oz_test_utils:space_add_user(Config, S1, U1),
        oz_test_utils:space_set_user_privileges(Config, S1, U1, [], [
            ?SPACE_UPDATE
        ]),
        oz_test_utils:space_add_user(Config, S1, U2),
        oz_test_utils:space_set_user_privileges(Config, S1, U2, [
            ?SPACE_UPDATE
        ], []),
        #{spaceId => S1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, Data) ->
        {ok, Space} = oz_test_utils:get_space(Config, SpaceId),
        ExpName = case ShouldSucceed of
            false -> ?CORRECT_NAME;
            true -> maps:get(<<"name">>, Data, ?CORRECT_NAME)
        end,
        ?assertEqual(ExpName, Space#od_space.name)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_UPDATE]},
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
            expected_result = ?OK_RES
        },
        data_spec = #data_spec{
            at_least_one = [<<"name">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME]
            },
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, Owner} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(Owner), ?SPACE_NAME1),
        oz_test_utils:space_add_user(Config, S1, U1),
        oz_test_utils:space_set_user_privileges(
            Config, S1, U1, [], [?SPACE_DELETE]
        ),
        oz_test_utils:space_add_user(Config, S1, U2),
        oz_test_utils:space_set_user_privileges(
            Config, S1, U2, [?SPACE_DELETE], []
        ),
        #{spaceId => S1}
    end,
    DeleteEntityFun = fun(#{spaceId := SpaceId} = _Env) ->
        oz_test_utils:delete_space(Config, SpaceId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:list_spaces(Config),
        ?assertEqual(lists:member(SpaceId, Spaces), not ShouldSucceed)
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
            expected_result = ?OK_RES
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_shares_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

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
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_share_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, User, [?SPACE_VIEW], []),

    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config),
    oz_test_utils:support_space_by_provider(Config, P1, S1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

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
            expected_result = api_test_expect:private_share(gs, ShareId, ShareData, ?SUB(user, User))
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
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
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

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

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
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:has_storage function
    lists:foreach(
        fun(StorageId) ->
            ?assert(oz_test_utils:call_oz(
                Config, space_logic, is_supported_by_storage, [S1, StorageId])
            )
        end, ExpStorages
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, space_logic, is_supported_by_storage, [S1, <<"asdiucyaie827346w">>])
    ).


create_space_support_token(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_ADD_STORAGE privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_ADD_SUPPORT
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
        % TODO gs
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
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
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
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


remove_provider_test(Config) ->
    % for each test run, a space with 3 users is created:
    %   Owner effectively has all the privileges
    %   UserWithRemovePrivs gets the SPACE_REMOVE_SUPPORT privilege
    %   UserWithoutRemovePrivs gets all remaining privileges
    {ok, Owner} = oz_test_utils:create_user(Config),
    {ok, UserWithRemovePrivs} = oz_test_utils:create_user(Config),
    {ok, UserWithoutRemovePrivs} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, {SubjectProvider, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {OtherProvider, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, OtherStorage} = oz_test_utils:create_storage(Config, ?PROVIDER(OtherProvider), ?STORAGE_NAME1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    EnvSetUpFun = fun() ->
        {ok, Space} = oz_test_utils:create_space(Config, ?USER(Owner)),
        {ok, SubjectStorage1} = oz_test_utils:create_storage(Config, ?PROVIDER(SubjectProvider), ?STORAGE_NAME1),
        {ok, SubjectStorage2} = oz_test_utils:create_storage(Config, ?PROVIDER(SubjectProvider), ?STORAGE_NAME1),
        oz_test_utils:support_space(Config, ?PROVIDER(SubjectProvider), SubjectStorage1, Space),
        oz_test_utils:support_space(Config, ?PROVIDER(SubjectProvider), SubjectStorage2, Space),

        oz_test_utils:support_space(Config, ?PROVIDER(OtherProvider), OtherStorage, Space),

        oz_test_utils:space_add_user(Config, Space, UserWithRemovePrivs),
        oz_test_utils:space_add_user(Config, Space, UserWithoutRemovePrivs),
        AllPrivs = privileges:space_privileges(),
        oz_test_utils:space_set_user_privileges(
            Config, Space, UserWithRemovePrivs, [?SPACE_REMOVE_SUPPORT], AllPrivs -- [?SPACE_REMOVE_SUPPORT]
        ),
        oz_test_utils:space_set_user_privileges(
            Config, Space, UserWithoutRemovePrivs, AllPrivs -- [?SPACE_REMOVE_SUPPORT], [?SPACE_REMOVE_SUPPORT]
        ),

        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        #{spaceId => Space, storageId1 => SubjectStorage1, storageId2 => SubjectStorage2}
    end,
    DeleteEntityFun = fun(#{spaceId := Space}) ->
        oz_test_utils:space_remove_provider(Config, Space, SubjectProvider),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := Space, storageId1 := St1, storageId2 := St2} = _Env, _) ->
        {ok, Storages} = oz_test_utils:space_get_storages(Config, Space),
        ?assertEqual(lists:member(St1, Storages), not ShouldSucceed),
        ?assertEqual(lists:member(St2, Storages), not ShouldSucceed),
        ?assertEqual(lists:member(OtherStorage, Storages), true)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_REMOVE_RELATIONSHIPS]},
                {user, Owner},
                {user, UserWithRemovePrivs}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutRemovePrivs},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/spaces/">>, spaceId, <<"/providers/">>, SubjectProvider],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = remove_provider,
            args = [auth, spaceId, SubjectProvider],
            expected_result = ?OK_RES
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_effective_providers_test(Config) ->
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_VIEW privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ExpProviders = lists:map(
        fun(_) ->
            {ok, {ProviderId, _}} = oz_test_utils:create_provider(
                Config, ?PROVIDER_NAME1
            ),
            {ok, S1} = oz_test_utils:support_space_by_provider(Config, ProviderId, S1),
            ProviderId
        end, lists:seq(1, 5)
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

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
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:has_provider function
    lists:foreach(
        fun(ProviderId) ->
            ?assert(oz_test_utils:call_oz(
                Config, space_logic, is_supported_by_provider, [S1, ProviderId])
            )
        end, ExpProviders
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, space_logic, is_supported_by_provider, [S1, <<"asdiucyaie827346w">>])
    ).


get_eff_provider_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

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

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

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
            expected_body = api_test_expect:protected_provider(rest, P1, ProviderDetails, offline)
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_provider,
            args = [auth, S1, P1],
            expected_result = api_test_expect:protected_provider(logic, P1, ProviderDetails, offline)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_provider, id = P1,
                aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_SPACE(S1),
            expected_result = api_test_expect:protected_provider(gs, P1, ProviderDetails, offline)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % When making connection with gs provider becomes online
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, P1Token}]
        },
        gs_spec = GsSpec#gs_spec{
            expected_result = api_test_expect:protected_provider(gs, P1, ProviderDetails, online)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


update_support_parameters_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config),
    {ok, {P3, P3Token}} = oz_test_utils:create_provider(Config),
    oz_test_utils:support_space_by_provider(Config, P1, Space),
    oz_test_utils:support_space_by_provider(Config, P2, Space),
    oz_test_utils:support_space_by_provider(Config, P3, Space),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ForbiddenClients = [
        {admin, privileges:oz_privileges()},
        {user, NonAdmin},
        {provider, P1, P1Token},
        {provider, P2, P2Token},
        {provider, P3, P3Token}
    ],
    update_support_parameters_test(Config, U1, Space, P1, ForbiddenClients),
    update_support_parameters_test(Config, U1, Space, P2, ForbiddenClients),
    update_support_parameters_test(Config, U1, Space, P3, ForbiddenClients).

update_support_parameters_test(Config, SpaceAdmin, Space, Provider, ForbiddenClients) ->
    EnvSetUpFun = fun() ->
        {ok, #od_space{support_parameters_registry = SupportParameters}} = oz_test_utils:get_space(Config, Space),
        #{previous => SupportParameters}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{previous := PreviousSupportParameters}, Data) ->
        {ok, #od_space{support_parameters_registry = SupportParameters}} = oz_test_utils:get_space(Config, Space),
        case ShouldSucceed of
            false ->
                ?assertEqual(SupportParameters, PreviousSupportParameters);
            true ->
                PreviousForProvider = maps:get(Provider, PreviousSupportParameters),
                ExpParametersForProvider = support_parameters:build(
                    case maps:find(<<"dataWrite">>, Data) of
                        {ok, DW} -> binary_to_atom(DW, utf8);
                        error -> support_parameters:get_data_write(PreviousForProvider)
                    end,
                    case maps:find(<<"metadataReplication">>, Data) of
                        {ok, MR} -> binary_to_atom(MR, utf8);
                        error -> support_parameters:get_metadata_replication(PreviousForProvider)
                    end
                ),
                ?assertEqual(SupportParameters, PreviousSupportParameters#{
                    Provider => ExpParametersForProvider
                })
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, SpaceAdmin}
            ],
            unauthorized = [nobody],
            forbidden = ForbiddenClients
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/spaces/">>, Space, <<"/providers/">>, Provider, <<"/support_parameters">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = update_support_parameters,
            args = [auth, Space, Provider, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_space, id = Space, aspect = {support_parameters, Provider}},
            expected_result = ?OK_RES
        },
        data_spec = #data_spec{
            at_least_one = [<<"dataWrite">>, <<"metadataReplication">>],
            correct_values = #{
                <<"dataWrite">> => [<<"global">>, <<"none">>],
                <<"metadataReplication">> => [<<"eager">>, <<"lazy">>, <<"none">>]
            },
            bad_values = [
                {<<"dataWrite">>, <<"binary">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"dataWrite">>, [global, none])},
                {<<"dataWrite">>, 1234,
                    ?ERROR_BAD_VALUE_ATOM(<<"dataWrite">>)},
                {<<"metadataReplication">>, <<"binary">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"metadataReplication">>, [eager, lazy, none])},
                {<<"metadataReplication">>, 1234,
                    ?ERROR_BAD_VALUE_ATOM(<<"metadataReplication">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


update_provider_capacity_usage_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config),
    {ok, {P3, P3Token}} = oz_test_utils:create_provider(Config),
    {ok, {NonSupporter, NonSupporterToken}} = oz_test_utils:create_provider(Config),
    % P1 supports the space with two storages
    oz_test_utils:support_space_by_provider(Config, P1, Space),
    oz_test_utils:support_space_by_provider(Config, P1, Space),
    oz_test_utils:support_space_by_provider(Config, P2, Space),
    oz_test_utils:support_space_by_provider(Config, P3, Space),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllClients = [
        {admin, privileges:oz_privileges()},
        {user, U1},
        {user, NonAdmin},
        {provider, P1, P1Token},
        {provider, P2, P2Token},
        {provider, P3, P3Token},
        {provider, NonSupporter, NonSupporterToken}
    ],
    update_provider_capacity_usage_test_base(Config, Space, P1, P1Token, AllClients),
    update_provider_capacity_usage_test_base(Config, Space, P2, P2Token, AllClients),
    update_provider_capacity_usage_test_base(Config, Space, P3, P3Token, AllClients).


%% @private
update_provider_capacity_usage_test_base(Config, Space, SubjectProvider, SubjectProviderToken, AllClients) ->
    {ok, SpaceRecord} = oz_test_utils:get_space(Config, Space),
    SpaceStorages = SpaceRecord#od_space.storages,
    {ok, #od_provider{storages = ProviderStorages}} = oz_test_utils:get_provider(Config, SubjectProvider),
    SupportingStorages = lists_utils:intersect(ProviderStorages, maps:keys(SpaceStorages)),
    RandomizeCapacityUsageReport = fun() ->
        lists:foldl(fun(StorageId, Acc) ->
            SupportSize = maps:get(StorageId, SpaceStorages),
            case rand:uniform(3) of
                1 ->
                    % incomplete report
                    Acc;
                2 ->
                    % overfull storage
                    Acc#{StorageId => SupportSize - 1000 + rand:uniform(2000)};
                3 ->
                    % random usage
                    Acc#{StorageId => rand:uniform(SupportSize)}
            end
        end, #{}, SupportingStorages)
    end,

    EnvSetUpFun = fun() ->
        #{previousRegistry => get_capacity_usage_registry(Config, Space)}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{previousRegistry := PreviousRegistry}, Data) ->
        CurrentRegistry = get_capacity_usage_registry(Config, Space),
        case ShouldSucceed of
            false ->
                ?assertEqual(CurrentRegistry, PreviousRegistry);
            true ->
                {ok, CurrentProviderUsage} = provider_capacity_usage:lookup_by_provider(CurrentRegistry, SubjectProvider),
                {ok, PreviousProviderUsage} = provider_capacity_usage:lookup_by_provider(PreviousRegistry, SubjectProvider),
                Report = maps:get(<<"providerCapacityUsageReport">>, Data),
                ExpectedProviderOverfull = lists:foldl(fun(StorageId, Acc) ->
                    CurrentStorageUsage = maps:get(StorageId, CurrentProviderUsage#provider_capacity_usage.per_storage),
                    PreviousStorageUsage = maps:get(StorageId, PreviousProviderUsage#provider_capacity_usage.per_storage),
                    SupportSize = maps:get(StorageId, SpaceStorages),
                    ?assertEqual(SupportSize, CurrentStorageUsage#storage_capacity_usage.total),

                    ExpectedUsed = maps:get(StorageId, Report, PreviousStorageUsage#storage_capacity_usage.used),
                    ?assertEqual(CurrentStorageUsage#storage_capacity_usage.used, ExpectedUsed),

                    OverfullThreshold = oz_test_utils:call_oz(Config, ctool, get_env, [
                        provider_capacity_usage_overfull_threshold_percent, 98
                    ]),
                    ?assertEqual(
                        CurrentStorageUsage#storage_capacity_usage.overfull,
                        ExpectedUsed * 100 / SupportSize > OverfullThreshold
                    ),
                    Acc orelse CurrentStorageUsage#storage_capacity_usage.overfull
                end, false, SupportingStorages),
                ?assertEqual(CurrentProviderUsage#provider_capacity_usage.overfull, ExpectedProviderOverfull)
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, SubjectProvider, SubjectProviderToken}
            ],
            unauthorized = [nobody],
            forbidden = AllClients -- [{provider, SubjectProvider, SubjectProviderToken}]
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = update_provider_capacity_usage,
            args = [auth, Space, SubjectProvider, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = space_stats, id = Space, aspect = {provider_capacity_usage, SubjectProvider}},
            expected_result = ?OK_RES
        },
        data_spec = #data_spec{
            required = [<<"providerCapacityUsageReport">>],
            correct_values = #{<<"providerCapacityUsageReport">> => [RandomizeCapacityUsageReport]},
            bad_values = [
                {<<"providerCapacityUsageReport">>, <<"binary">>, ?ERROR_BAD_VALUE_JSON(<<"providerCapacityUsageReport">>)},
                {<<"providerCapacityUsageReport">>, 1234, ?ERROR_BAD_VALUE_JSON(<<"providerCapacityUsageReport">>)},
                {<<"providerCapacityUsageReport">>, #{<<"providerid">> => [123, <<"value">>]}, ?ERROR_BAD_DATA(<<"providerCapacityUsageReport">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


update_provider_sync_progress_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, {P1, P1Token}} = oz_test_utils:create_provider(Config),
    {ok, {P2, P2Token}} = oz_test_utils:create_provider(Config),
    {ok, {P3, P3Token}} = oz_test_utils:create_provider(Config),
    {ok, {NonSupporter, NonSupporterToken}} = oz_test_utils:create_provider(Config),
    oz_test_utils:support_space_by_provider(Config, P1, Space),
    oz_test_utils:support_space_by_provider(Config, P2, Space),
    oz_test_utils:support_space_by_provider(Config, P3, Space),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllClients = [
        {admin, privileges:oz_privileges()},
        {user, U1},
        {user, NonAdmin},
        {provider, P1, P1Token},
        {provider, P2, P2Token},
        {provider, P3, P3Token},
        {provider, NonSupporter, NonSupporterToken}
    ],
    update_provider_sync_progress_test_base(Config, Space, P1, P1Token, AllClients),
    update_provider_sync_progress_test_base(Config, Space, P2, P2Token, AllClients),
    update_provider_sync_progress_test_base(Config, Space, P3, P3Token, AllClients).


%% @private
update_provider_sync_progress_test_base(Config, Space, SubjectProvider, SubjectProviderToken, AllClients) ->
    {ok, SpaceRecord} = oz_test_utils:get_space(Config, Space),
    SupportingProviders = maps:keys(SpaceRecord#od_space.eff_providers),
    oz_test_utils:simulate_seconds_passing(50000 + rand:uniform(50000)),
    Now = oz_test_utils:get_frozen_time_seconds(),
    RandomizeSyncProgressReport = fun() ->
        lists:foldl(fun(Provider, Acc) ->
            % Due to delays in GraphSync, providers might not know all the other
            % providers for a while after a new support - simulate by randomly
            % dropping them out of the sequence map. They should default to
            % seq=1 and timestamp=0. However, the reporting provider must always
            % be in the report.
            case SubjectProvider /= Provider andalso rand:uniform(3) == 1 of
                true ->
                    Acc;
                false ->
                    Acc#{Provider => #{<<"seenSeq">> => rand:uniform(1000), <<"seqTimestamp">> => Now - rand:uniform(50000)}}
            end
        end, #{}, SupportingProviders)
    end,

    EnvSetUpFun = fun() ->
        oz_test_utils:simulate_seconds_passing(rand:uniform(100)),
        #{previousRegistry => get_sync_progress_registry(Config, Space)}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{previousRegistry := PreviousRegistry}, Data) ->
        CurrentRegistry = get_sync_progress_registry(Config, Space),
        case ShouldSucceed of
            false ->
                ?assertEqual(CurrentRegistry, PreviousRegistry);
            true ->
                {ok, CurrentProviderSummary} = provider_sync_progress:lookup(CurrentRegistry, SubjectProvider),
                ?assertEqual(oz_test_utils:get_frozen_time_seconds(), CurrentProviderSummary#provider_sync_progress.last_report),
                {ok, PreviousProviderSummary} = provider_sync_progress:lookup(PreviousRegistry, SubjectProvider),
                Report = maps:get(<<"providerSyncProgressReport">>, Data),
                lists:foreach(fun(PeerId) ->
                    CurrentPeerSummary = maps:get(PeerId, CurrentProviderSummary#provider_sync_progress.per_peer),
                    PreviousPeerSummary = maps:get(PeerId, PreviousProviderSummary#provider_sync_progress.per_peer),
                    ReportForPeer = maps:get(PeerId, Report, #{}),
                    ?assertEqual(
                        CurrentPeerSummary#sync_progress_with_peer.seen_seq,
                        maps:get(<<"seenSeq">>, ReportForPeer, PreviousPeerSummary#sync_progress_with_peer.seen_seq)
                    ),
                    ?assertEqual(
                        CurrentPeerSummary#sync_progress_with_peer.seq_timestamp,
                        maps:get(<<"seqTimestamp">>, ReportForPeer, PreviousPeerSummary#sync_progress_with_peer.seq_timestamp)
                    )
                end, SupportingProviders)
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, SubjectProvider, SubjectProviderToken}
            ],
            unauthorized = [nobody],
            forbidden = AllClients -- [{provider, SubjectProvider, SubjectProviderToken}]
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = update_provider_sync_progress,
            args = [auth, Space, SubjectProvider, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = space_stats, id = Space, aspect = {provider_sync_progress, SubjectProvider}},
            expected_result = ?OK_RES
        },
        data_spec = #data_spec{
            required = [<<"providerSyncProgressReport">>],
            correct_values = #{<<"providerSyncProgressReport">> => [RandomizeSyncProgressReport]},
            bad_values = [
                {<<"providerSyncProgressReport">>, <<"binary">>, ?ERROR_BAD_VALUE_JSON(<<"providerSyncProgressReport">>)},
                {<<"providerSyncProgressReport">>, 1234, ?ERROR_BAD_VALUE_JSON(<<"providerSyncProgressReport">>)},
                {<<"providerSyncProgressReport">>, #{<<"providerid">> => [123, <<"value">>]}, ?ERROR_BAD_DATA(<<"providerSyncProgressReport">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


get_latest_emitted_seq_test(Config) ->
    {Space, Owner, UserWithoutViewPrivs, UserWithViewPrivs} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, {SubjectProvider, _}} = oz_test_utils:create_provider(Config),
    {ok, {OtherProvider, _}} = oz_test_utils:create_provider(Config),
    {ok, {NonSupporter, _}} = oz_test_utils:create_provider(Config),
    oz_test_utils:support_space_by_provider(Config, SubjectProvider, Space),
    oz_test_utils:support_space_by_provider(Config, OtherProvider, Space),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % simulate some time passing after the space is created
    oz_test_utils:simulate_seconds_passing(1000),
    Now = oz_test_utils:get_frozen_time_seconds(),
    ExpLatestEmittedSeq = rand:uniform(500000),
    ExpLatestEmittedSeqTimestamp = Now - 15,
    ?assertEqual(ok, oz_test_utils:call_oz(Config, space_logic, update_provider_sync_progress, [
        ?PROVIDER(SubjectProvider), Space, SubjectProvider, provider_sync_progress:collective_report_to_json(
            provider_sync_progress:build_collective_report([SubjectProvider, OtherProvider], fun
                (P) when P == SubjectProvider -> {ExpLatestEmittedSeq, ExpLatestEmittedSeqTimestamp};
                (P) when P == OtherProvider -> {1, Now - 500}
            end)
        )
    ])),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW]},
                {user, Owner},
                {user, UserWithViewPrivs},
                {provider, SubjectProvider},
                {provider, OtherProvider}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutViewPrivs},
                {user, NonAdmin},
                {provider, NonSupporter}
            ]
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_latest_emitted_seq,
            args = [auth, Space, SubjectProvider],
            expected_result = ?OK_TERM(fun({LatestEmittedSeq, LatestEmittedSeqTimestamp}) ->
                ?assertEqual(LatestEmittedSeq, ExpLatestEmittedSeq),
                ?assertEqual(LatestEmittedSeqTimestamp, ExpLatestEmittedSeqTimestamp)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = space_stats, id = Space, aspect = {latest_emitted_seq, SubjectProvider}, scope = private},
            expected_result = ?OK_MAP(#{
                <<"seq">> => ExpLatestEmittedSeq,
                <<"seqTimestamp">> => ExpLatestEmittedSeqTimestamp
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_stats_test(Config) ->
    {Space, Owner, UserWithoutViewPrivs, UserWithViewPrivs} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, {ModernProvA, _}} = oz_test_utils:create_provider(Config),
    {ok, ModernProvAStorage} = oz_test_utils:create_storage(Config, ?PROVIDER(ModernProvA)),
    {ok, {ModernProvB, _}} = oz_test_utils:create_provider(Config),
    {ok, ModernProvBStoragePrim} = oz_test_utils:create_storage(Config, ?PROVIDER(ModernProvB)),
    {ok, ModernProvBStorageBis} = oz_test_utils:create_storage(Config, ?PROVIDER(ModernProvB)),
    {ok, {LegacyProv, _}} = oz_test_utils:create_provider(Config),
    {ok, LegacyProvStorage} = oz_test_utils:create_storage(Config, ?PROVIDER(LegacyProv)),
    oz_test_utils:simulate_provider_version(Config, LegacyProv, ?LINE_20_02(<<"1">>)),
    {ok, {NonSupporter, _}} = oz_test_utils:create_provider(Config),

    GenTotalSize = fun
        (S) when S == ModernProvAStorage -> 1234567890;
        (S) when S == ModernProvBStoragePrim -> 999999999;
        (S) when S == ModernProvBStorageBis -> 50000000000;
        (S) when S == LegacyProvStorage -> 3456128978
    end,
    GenExpUsedSize = fun
        (S) when S == ModernProvAStorage -> 987654321;
        (S) when S == ModernProvBStoragePrim -> 999999998;
        (S) when S == ModernProvBStorageBis -> 0;
        (S) when S == LegacyProvStorage -> -1  % the value of -1 is used when there has been no report
    end,
    oz_test_utils:support_space(Config, ?PROVIDER(ModernProvA), ModernProvAStorage, Space, GenTotalSize(ModernProvAStorage)),
    oz_test_utils:support_space(Config, ?PROVIDER(ModernProvB), ModernProvBStoragePrim, Space, GenTotalSize(ModernProvBStoragePrim)),
    oz_test_utils:support_space(Config, ?PROVIDER(ModernProvB), ModernProvBStorageBis, Space, GenTotalSize(ModernProvBStorageBis)),
    oz_test_utils:support_space(Config, ?PROVIDER(LegacyProv), LegacyProvStorage, Space, GenTotalSize(LegacyProvStorage)),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    TimeZero = oz_test_utils:get_frozen_time_seconds(),
    oz_test_utils:simulate_seconds_passing(5000),
    UpdateSyncProgress = fun(Provider, BuildReportFun) ->
        oz_test_utils:simulate_seconds_passing(rand:uniform(5000)),
        ?assertEqual(ok, oz_test_utils:call_oz(Config, space_logic, update_provider_sync_progress, [
            ?PROVIDER(Provider), Space, Provider, provider_sync_progress:collective_report_to_json(
                provider_sync_progress:build_collective_report([ModernProvA, ModernProvB, LegacyProv], BuildReportFun)
            )
        ])),
        oz_test_utils:get_frozen_time_seconds()
    end,

    ModernProvALastUpdate = UpdateSyncProgress(ModernProvA, fun
        (P) when P == ModernProvA -> {10, TimeZero + 10};
        (P) when P == ModernProvB -> {20, TimeZero + 20};
        (P) when P == LegacyProv -> {1, TimeZero}
    end),
    ModernProvBLastUpdate = UpdateSyncProgress(ModernProvB, fun
        (P) when P == ModernProvA -> {8, TimeZero + 8};
        (P) when P == ModernProvB -> {400, TimeZero + 400};
        (P) when P == LegacyProv -> {1, TimeZero}
    end),

    ExpectedProvSyncProgressRegistry = #sync_progress_registry{space_creation_time = TimeZero, per_provider = #{
        ModernProvA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = true,
            last_report = ModernProvALastUpdate,
            per_peer = #{
                ModernProvA => #sync_progress_with_peer{seen_seq = 10, seq_timestamp = TimeZero + 10,
                    diff = 0, delay = 0, desync = false},
                ModernProvB => #sync_progress_with_peer{seen_seq = 20, seq_timestamp = TimeZero + 20,
                    diff = 380, delay = 380, desync = true},
                LegacyProv => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = TimeZero,
                    diff = 0, delay = 0, desync = false}
            }
        },
        ModernProvB => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
            last_report = ModernProvBLastUpdate,
            per_peer = #{
                ModernProvA => #sync_progress_with_peer{seen_seq = 8, seq_timestamp = TimeZero + 8,
                    diff = 2, delay = 2, desync = false},
                ModernProvB => #sync_progress_with_peer{seen_seq = 400, seq_timestamp = TimeZero + 400,
                    diff = 0, delay = 0, desync = false},
                LegacyProv => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = TimeZero,
                    diff = 0, delay = 0, desync = false}
            }
        },
        LegacyProv => #provider_sync_progress{legacy = true, joining = true, archival = false, desync = true,
            % legacy providers do not report the progress and their lastReport is always set to -1
            last_report = -1,
            per_peer = #{
                ModernProvA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = TimeZero,
                    diff = 9, delay = 10, desync = false},
                ModernProvB => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = TimeZero,
                    diff = 399, delay = 400, desync = true},
                LegacyProv => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = TimeZero,
                    diff = 0, delay = 0, desync = false}
            }
        }
    }},

    UpdateCapacityUsage = fun(Provider, Storages) ->
        ?assertEqual(ok, oz_test_utils:call_oz(Config, space_logic, update_provider_capacity_usage, [
            ?PROVIDER(Provider), Space, Provider, provider_capacity_usage:report_to_json(
                provider_capacity_usage:build_report(Storages, GenExpUsedSize)
            )
        ])),
        oz_test_utils:get_frozen_time_seconds()
    end,
    UpdateCapacityUsage(ModernProvA, [ModernProvAStorage]),
    UpdateCapacityUsage(ModernProvB, [ModernProvBStoragePrim, ModernProvBStorageBis]),

    ExpectedProvCapacityUsageRegistry = #{
        ModernProvA => #provider_capacity_usage{overfull = false, per_storage = #{
            ModernProvAStorage => #storage_capacity_usage{
                used = GenExpUsedSize(ModernProvAStorage),
                total = GenTotalSize(ModernProvAStorage),
                overfull = false
            }
        }},
        ModernProvB => #provider_capacity_usage{overfull = true, per_storage = #{
            ModernProvBStoragePrim => #storage_capacity_usage{
                used = GenExpUsedSize(ModernProvBStoragePrim),
                total = GenTotalSize(ModernProvBStoragePrim),
                overfull = true
            },
            ModernProvBStorageBis => #storage_capacity_usage{
                used = GenExpUsedSize(ModernProvBStorageBis),
                total = GenTotalSize(ModernProvBStorageBis),
                overfull = false
            }
        }},
        LegacyProv => #provider_capacity_usage{overfull = false, per_storage = #{
            LegacyProvStorage => #storage_capacity_usage{
                used = GenExpUsedSize(LegacyProvStorage),
                total = GenTotalSize(LegacyProvStorage),
                overfull = false
            }
        }}
    },

    ExpectedStatsJson = #{
        <<"capacityUsageRegistry">> => provider_capacity_usage:registry_to_json(ExpectedProvCapacityUsageRegistry),
        <<"syncProgressRegistry">> => provider_sync_progress:registry_to_json(ExpectedProvSyncProgressRegistry)
    },

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW]},
                {user, Owner},
                {user, UserWithViewPrivs},
                {provider, ModernProvA},
                {provider, ModernProvB},
                {provider, LegacyProv}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UserWithoutViewPrivs},
                {user, NonAdmin},
                {provider, NonSupporter}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, Space, <<"/stats">>],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpectedStatsJson
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_stats,
            args = [auth, Space],
            expected_result = ?OK_TERM(fun(SpaceStats) ->
                ?assertEqual(ExpectedProvCapacityUsageRegistry, SpaceStats#space_stats.capacity_usage_registry),
                ?assertEqual(ExpectedProvSyncProgressRegistry, SpaceStats#space_stats.sync_progress_registry)
            end)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = space_stats, id = Space, aspect = instance, scope = private},
            expected_result = ?OK_MAP(ExpectedStatsJson#{
                <<"gri">> => gri:serialize(?GRI(space_stats, Space, instance, private))
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_sync_progress_registry(Config, SpaceId) ->
    {ok, #space_stats{sync_progress_registry = Registry}} = oz_test_utils:call_oz(
        Config, space_logic, get_stats, [?ROOT, SpaceId]
    ),
    Registry.


get_capacity_usage_registry(Config, SpaceId) ->
    {ok, #space_stats{capacity_usage_registry = Registry}} = oz_test_utils:call_oz(
        Config, space_logic, get_stats, [?ROOT, SpaceId]
    ),
    Registry.

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
