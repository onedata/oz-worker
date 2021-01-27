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
    get_eff_provider_test/1
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
        get_eff_provider_test
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
            function = get_spaces,
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
                    ?assertEqual(EffProviders, #{P1 => {SupportSize, [{od_storage, St1}]}})
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
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(S1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check protected data
    SpaceData = #{<<"name">> => ?SPACE_NAME1, <<"providers">> => #{P1 => SupportSize}},
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
        {ok, Spaces} = oz_test_utils:get_spaces(Config),
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
    % create space with 3 users:
    %   Owner effectively has all the privileges
    %   U2 gets the SPACE_REMOVE_SUPPORT privilege
    %   U1 gets all remaining privileges
    {S1, Owner, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_REMOVE_SUPPORT
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, {P1, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, St3} = oz_test_utils:create_storage(Config, ?PROVIDER(P2), ?STORAGE_NAME1),
    {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P2), St3, S1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    EnvSetUpFun = fun() ->
        {ok, St1} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
        {ok, St2} = oz_test_utils:create_storage(Config, ?PROVIDER(P1), ?STORAGE_NAME1),
        {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St1, S1),
        {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), St2, S1),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        #{storageId1 => St1, storageId2 => St2}
    end,
    DeleteEntityFun = fun(_Env) ->
        oz_test_utils:space_remove_provider(Config, S1, P1),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config)
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
            expected_result = api_test_expect:protected_provider(gs, P1, ProviderDetails)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % When making connection with gs provider becomes online
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, P1Token}]
        },
        gs_spec = GsSpec#gs_spec{
            expected_result = api_test_expect:protected_provider(gs, P1, ProviderDetails#{<<"online">> => true})
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).

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
