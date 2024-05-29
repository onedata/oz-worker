%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning groups basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_misc_api_test_SUITE).
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
    create_test/1,
    list_test/1,
    list_privileges_test/1,
    get_test/1,
    update_test/1,
    delete_test/1,
    protected_group_test/1,
    get_oz_privileges_test/1,
    update_oz_privileges_test/1,
    delete_oz_privileges_test/1,
    get_eff_oz_privileges_test/1,

    list_eff_providers_test/1,
    get_eff_provider_test/1,
    get_spaces_in_eff_provider_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        list_privileges_test,
        get_test,
        update_test,
        delete_test,
        protected_group_test,
        get_oz_privileges_test,
        update_oz_privileges_test,
        delete_oz_privileges_test,
        get_eff_oz_privileges_test,

        list_eff_providers_test,
        get_eff_provider_test,
        get_spaces_in_eff_provider_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


create_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),

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
                {admin, [?OZ_GROUPS_CREATE]}
            ],
            unauthorized = [nobody],
            forbidden = [{user, U1}]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/groups">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                BaseURL = ?URL(Config, [<<"/groups/">>]),
                fun(#{?HDR_LOCATION := Location} = _Headers) ->
                    [GroupId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(GroupId, ExpType)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create,
            args = [auth, data],
            expected_result = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                ?OK_TERM(fun(GroupId) -> VerifyFun(GroupId, ExpType) end)
            end)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_group, aspect = instance},
            expected_result_op = ?OK_ENV(fun(_, DataSet) ->
                ExpType = maps:get(<<"type">>, DataSet, ?DEFAULT_GROUP_TYPE),
                ?OK_MAP_CONTAINS(#{
                    <<"name">> => ?CORRECT_NAME,
                    <<"type">> => atom_to_binary(ExpType, utf8),
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = gri:deserialize(EncodedGri),
                        VerifyFun(Id, ExpType)
                    end
                })
            end)
        },
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


list_test(Config) ->
    % Make sure that groups created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G4} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    {ok, G5} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    ExpGroups = [G1, G2, G3, G4, G5, <<"all_users">>, <<"admins">>],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_LIST]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/groups">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"groups">> => ExpGroups}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = list,
            args = [auth],
            expected_result = ?OK_LIST(ExpGroups)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:exist function
    lists:foreach(
        fun(GroupId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, exists, [GroupId])
            )
        end, ExpGroups
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, exists, [<<"asdiucyaie827346w">>])
    ).


list_privileges_test(Config) ->

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/groups/privileges">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"member">> => [atom_to_binary(P, utf8) || P <- privileges:group_member()],
                <<"manager">> => [atom_to_binary(P, utf8) || P <- privileges:group_manager()],
                <<"admin">> => [atom_to_binary(P, utf8) || P <- privileges:group_admin()]
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_test(Config) ->
    MemberWithView = ozt_users:create(),
    MemberWithoutView = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    AllPrivs = privileges:group_privileges(),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    GroupData = #{<<"name">> => ?GROUP_NAME1, <<"type">> => ?GROUP_TYPE1},
    GroupId = ozt_users:create_group_for(MemberWithView, GroupData),
    ozt_groups:set_user_privileges(GroupId, MemberWithView, [?GROUP_VIEW]),
    ozt_groups:add_user(GroupId, MemberWithoutView, AllPrivs -- [?GROUP_VIEW]),

    ChildGroupId = ozt_groups:create(),
    ozt_groups:add_child(GroupId, ChildGroupId, []),
    ozt_groups:add_user(ChildGroupId, MemberWithView, [?GROUP_VIEW]),
    ozt_groups:add_user(ChildGroupId, MemberWithoutView, []),

    ParentGroupId = ozt_groups:create(),
    ozt_groups:add_child(ParentGroupId, GroupId, []),
    ozt_groups:add_user(ParentGroupId, MemberWithView, [?GROUP_VIEW]),
    ozt_groups:add_user(ParentGroupId, MemberWithoutView, []),

    SpaceId = ozt_users:create_space_for(MemberWithView),
    ozt_spaces:add_group(SpaceId, GroupId),
    ProviderId = ozt_providers:create_for_admin_user(MemberWithView),
    ozt_clusters:add_user(ProviderId, MemberWithoutView, []),
    ozt_clusters:add_group(ProviderId, GroupId, []),
    ozt_providers:support_space(ProviderId, SpaceId),

    HandleServiceId = ozt_handle_services:create(),
    ozt_handle_services:add_group(HandleServiceId, GroupId, []),
    ozt_handle_services:add_user(HandleServiceId, MemberWithView, [?HANDLE_SERVICE_VIEW]),
    ozt_handle_services:add_user(HandleServiceId, MemberWithoutView, []),

    HandleId = ozt_handle_services:create_handle(HandleServiceId, ozt_spaces:create_share(SpaceId, datastore_key:new())),
    oz_test_utils:handle_add_group(Config, HandleId, GroupId),
    oz_test_utils:handle_add_user(Config, HandleId, MemberWithView),
    oz_test_utils:handle_set_user_privileges(Config, HandleId, MemberWithView, [?HANDLE_VIEW], []),
    oz_test_utils:handle_add_user(Config, HandleId, MemberWithoutView),
    oz_test_utils:handle_set_user_privileges(Config, HandleId, MemberWithoutView, [], [?HANDLE_VIEW]),

    HarvesterId = ozt_harvesters:create(),
    ozt_harvesters:add_group(HarvesterId, GroupId, []),
    ozt_harvesters:add_user(HarvesterId, MemberWithView, [?HARVESTER_VIEW]),
    ozt_harvesters:add_user(HarvesterId, MemberWithoutView, []),

    AtmInventoryId = ozt_atm_inventories:create(),
    ozt_atm_inventories:add_group(AtmInventoryId, GroupId, []),
    ozt_atm_inventories:add_user(AtmInventoryId, MemberWithView, [?ATM_INVENTORY_VIEW]),
    ozt_atm_inventories:add_user(AtmInventoryId, MemberWithoutView, []),

    ozt:reconcile_entity_graph(),

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, MemberWithView}
            ],
            unauthorized = [nobody],
            forbidden = [
                {admin, [?OZ_GROUPS_VIEW]},
                {user, NonAdmin},
                {user, MemberWithoutView},
                {provider, ProviderId}
            ]
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get,
            args = [auth, GroupId],
            expected_result = ?OK_TERM(
                fun(#od_group{
                    name = Name, type = Type,
                    oz_privileges = [], eff_oz_privileges = [],

                    users = Users, eff_users = EffUsers,
                    parents = Parents, eff_parents = EffParents,
                    children = Children, eff_children = EffChildren,
                    spaces = Spaces, eff_spaces = EffSpaces,
                    handle_services = HandleServices, eff_handle_services = EffHandleServices,
                    handles = Handles, eff_handles = EffHandles,
                    harvesters = Harvesters, eff_harvesters = EffHarvesters,
                    clusters = Clusters, eff_clusters = EffClusters,
                    atm_inventories = AtmInventories, eff_atm_inventories = EffAtmInventories,
                    eff_providers = EffProviders
                }) ->
                    ?assertEqual(?GROUP_NAME1, Name),
                    ?assertEqual(?GROUP_TYPE1, Type),
                    ?assertEqual(Users, #{
                        MemberWithoutView => AllPrivs -- [?GROUP_VIEW],
                        MemberWithView => [?GROUP_VIEW]}
                    ),
                    ?assertEqual(EffUsers, #{
                        MemberWithoutView => {AllPrivs -- [?GROUP_VIEW], [{od_group, ChildGroupId}, {od_group, <<"self">>}]},
                        MemberWithView => {[?GROUP_VIEW], [{od_group, ChildGroupId}, {od_group, <<"self">>}]}
                    }),
                    ?assertEqual(Parents, [ParentGroupId]),
                    ?assertEqual(EffParents, #{ParentGroupId => [{od_group, <<"self">>}]}),
                    ?assertEqual(Children, #{ChildGroupId => []}),
                    ?assertEqual(EffChildren, #{ChildGroupId => {[], [{od_group, <<"self">>}]}}),
                    ?assertEqual(Spaces, [SpaceId]),
                    ?assertEqual(EffSpaces, #{SpaceId => [{od_group, <<"self">>}]}),
                    ?assertEqual(HandleServices, [HandleServiceId]),
                    ?assertEqual(EffHandleServices, #{HandleServiceId => [{od_group, <<"self">>}]}),
                    ?assertEqual(Handles, [HandleId]),
                    ?assertEqual(EffHandles, #{HandleId => [{od_group, <<"self">>}]}),
                    ?assertEqual(Harvesters, [HarvesterId]),
                    ?assertEqual(EffHarvesters, #{HarvesterId => [{od_group, <<"self">>}]}),
                    ?assertEqual(Clusters, [ProviderId]),
                    ?assertEqual(EffClusters, #{ProviderId => [{od_group, <<"self">>}]}),
                    ?assertEqual(AtmInventories, [AtmInventoryId]),
                    ?assertEqual(EffAtmInventories, #{AtmInventoryId => [{od_group, <<"self">>}]}),
                    ?assertEqual(EffProviders, #{ProviderId => [{od_space, SpaceId}]})
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_group, id = GroupId, aspect = instance},
            expected_result_op = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?GROUP_NAME1,
                <<"type">> => ?GROUP_TYPE1_BIN,
                <<"parents">> => [ParentGroupId],
                <<"children">> => #{
                    ChildGroupId => []
                },
                <<"effectiveChildren">> => #{
                    ChildGroupId => []
                },
                <<"users">> => #{
                    MemberWithoutView => AllPrivsBin -- [<<"group_view">>],
                    MemberWithView => [<<"group_view">>]
                },
                <<"effectiveUsers">> => #{
                    MemberWithoutView => AllPrivsBin -- [<<"group_view">>],
                    MemberWithView => [<<"group_view">>]
                },
                <<"spaces">> => [SpaceId],
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    ?assertEqual(GroupId, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check shared data
    GetSharedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_GROUPS_VIEW]},
                {provider, ProviderId},
                {user, MemberWithoutView},
                {user, MemberWithView}
            ],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        logic_spec = LogicSpec = #logic_spec{
            module = group_logic,
            function = get_shared_data,
            args = [auth, GroupId],
            expected_result = api_test_expect:shared_group(logic, GroupId, GroupData)
        },
        gs_spec = GsSpec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_group, id = GroupId, aspect = instance, scope = shared
            },
            expected_result_op = api_test_expect:shared_group(gs, GroupId, GroupData)
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetSharedDataApiTestSpec)),

    % Get and check protected data
    GetProtectedDataApiTestSpec = GetSharedDataApiTestSpec#api_test_spec{
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, GroupId],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:protected_group(rest, GroupId, GroupData, ?SUB(user, MemberWithView))
        },
        logic_spec = LogicSpec#logic_spec{
            function = get_protected_data,
            expected_result = api_test_expect:protected_group(logic, GroupId, GroupData, ?SUB(user, MemberWithView))
        },
        gs_spec = GsSpec#gs_spec{
            gri = #gri{
                type = od_group, id = GroupId, aspect = instance, scope = protected
            },
            expected_result_op = api_test_expect:protected_group(gs, GroupId, GroupData, ?SUB(user, MemberWithView))
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetProtectedDataApiTestSpec)),

    GetWithAuthHint = fun(Scope, Auth, AuthHint) ->
        ozt:rpc(entity_logic, handle, [#el_req{
            operation = get,
            auth = Auth,
            auth_hint = AuthHint,
            gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = Scope}
        }])
    end,

    AllClients = [
        ?USER(MemberWithView),
        ?USER(MemberWithoutView),
        ?USER(NonAdmin),
        ?PROVIDER(ProviderId),
        ?PROVIDER(ozt_providers:create())
    ],

    lists:foreach(fun({Scope, AuthHint, AuthorizedClients}) ->
        lists:foreach(fun(Auth) ->
            case AuthorizedClients of
                {none, ExpError} ->
                    ?assertMatch(ExpError, GetWithAuthHint(Scope, Auth, AuthHint));
                [_ | _] ->
                    case lists:member(Auth, AuthorizedClients) of
                        true ->
                            ?assertMatch({ok, _}, GetWithAuthHint(Scope, Auth, AuthHint));
                        false ->
                            ?assertMatch(?ERROR_FORBIDDEN, GetWithAuthHint(Scope, Auth, AuthHint))
                    end
            end
        end, AllClients)
    end, [
        {private, undefined, [?USER(MemberWithView)]},
        {private, ?THROUGH_USER(MemberWithView), {none, ?ERROR_FORBIDDEN}},
        {private, ?THROUGH_USER(MemberWithoutView), {none, ?ERROR_FORBIDDEN}},
        {private, ?THROUGH_GROUP(ChildGroupId), {none, ?ERROR_FORBIDDEN}},
        {private, ?THROUGH_GROUP(ParentGroupId), {none, ?ERROR_FORBIDDEN}},
        {private, ?THROUGH_SPACE(SpaceId), {none, ?ERROR_FORBIDDEN}},
        {private, ?THROUGH_PROVIDER(ProviderId), {none, ?ERROR_FORBIDDEN}},
        {private, ?THROUGH_CLUSTER(ProviderId), {none, ?ERROR_FORBIDDEN}},
        {private, ?THROUGH_HANDLE_SERVICE(HandleServiceId), {none, ?ERROR_FORBIDDEN}},
        {private, ?THROUGH_HANDLE(HandleId), {none, ?ERROR_FORBIDDEN}},
        {private, ?THROUGH_HARVESTER(HarvesterId), {none, ?ERROR_FORBIDDEN}},
        {private, ?THROUGH_ATM_INVENTORY(AtmInventoryId), {none, ?ERROR_FORBIDDEN}},

        {protected, undefined, [?USER(MemberWithView), ?USER(MemberWithoutView), ?PROVIDER(ProviderId)]},
        {protected, ?THROUGH_USER(MemberWithView), [?USER(MemberWithView)]},
        {protected, ?THROUGH_USER(MemberWithoutView), [?USER(MemberWithoutView)]},
        {protected, ?THROUGH_GROUP(ChildGroupId), [?USER(MemberWithView), ?USER(MemberWithoutView)]},
        {protected, ?THROUGH_GROUP(ParentGroupId), {none, ?ERROR_NOT_FOUND}},
        {protected, ?THROUGH_SPACE(SpaceId), {none, ?ERROR_NOT_FOUND}},
        {protected, ?THROUGH_PROVIDER(ProviderId), [?PROVIDER(ProviderId), ?USER(MemberWithView)]},
        {protected, ?THROUGH_CLUSTER(ProviderId), {none, ?ERROR_NOT_FOUND}},
        {protected, ?THROUGH_HANDLE_SERVICE(HandleServiceId), {none, ?ERROR_NOT_FOUND}},
        {protected, ?THROUGH_HANDLE(HandleId), {none, ?ERROR_NOT_FOUND}},
        {protected, ?THROUGH_HARVESTER(HarvesterId), {none, ?ERROR_NOT_FOUND}},
        {protected, ?THROUGH_ATM_INVENTORY(AtmInventoryId), {none, ?ERROR_NOT_FOUND}},

        {shared, undefined, [?USER(MemberWithView), ?USER(MemberWithoutView), ?PROVIDER(ProviderId)]},
        {shared, ?THROUGH_USER(MemberWithView), [?USER(MemberWithView)]},
        {shared, ?THROUGH_USER(MemberWithoutView), [?USER(MemberWithoutView)]},
        {shared, ?THROUGH_USER(NonAdmin), {none, ?ERROR_NOT_FOUND}},
        {shared, ?THROUGH_GROUP(ParentGroupId), [?USER(MemberWithView), ?USER(MemberWithoutView)]},
        {shared, ?THROUGH_GROUP(ChildGroupId), {none, ?ERROR_NOT_FOUND}},
        {shared, ?THROUGH_SPACE(SpaceId), [?PROVIDER(ProviderId), ?USER(MemberWithView), ?USER(MemberWithoutView)]},
        {shared, ?THROUGH_PROVIDER(ProviderId), [?PROVIDER(ProviderId), ?USER(MemberWithView), ?USER(MemberWithoutView)]},
        {shared, ?THROUGH_CLUSTER(ProviderId), [?PROVIDER(ProviderId), ?USER(MemberWithView), ?USER(MemberWithoutView)]},
        {shared, ?THROUGH_HANDLE_SERVICE(HandleServiceId), [?USER(MemberWithView), ?USER(MemberWithoutView)]},
        {shared, ?THROUGH_HANDLE(HandleId), [?USER(MemberWithView), ?USER(MemberWithoutView)]},
        {shared, ?THROUGH_HARVESTER(HarvesterId), [?USER(MemberWithView), ?USER(MemberWithoutView)]},
        {shared, ?THROUGH_ATM_INVENTORY(AtmInventoryId), [?USER(MemberWithView), ?USER(MemberWithoutView)]}
    ]).


update_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
        oz_test_utils:group_set_user_privileges(Config, G1, U1, [], [
            ?GROUP_UPDATE
        ]),
        oz_test_utils:group_add_user(Config, G1, U2),
        oz_test_utils:group_set_user_privileges(Config, G1, U2, [
            ?GROUP_UPDATE
        ], []),
        #{groupId => G1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, Data) ->
        {ok, Group} = oz_test_utils:get_group(Config, GroupId),
        {ExpType, ExpName} = case ShouldSucceed of
            false ->
                {?DEFAULT_GROUP_TYPE, ?GROUP_NAME1};
            true ->
                {
                    maps:get(<<"type">>, Data, ?DEFAULT_GROUP_TYPE),
                    maps:get(<<"name">>, Data, ?GROUP_NAME1)
                }
        end,
        ?assertEqual(ExpName, Group#od_group.name),
        ?assertEqual(ExpType, Group#od_group.type)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_UPDATE]}
            ],
            unauthorized = [nobody],
            forbidden = [{user, U1}]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/groups/">>, groupId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = update,
            args = [auth, groupId, data],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_group, id = groupId, aspect = instance},
            expected_result_op = ?OK_RES
        },
        data_spec = #data_spec{
            at_least_one = [<<"name">>, <<"type">>],
            correct_values = #{
                <<"name">> => [?CORRECT_NAME],
                <<"type">> => [?GROUP_TYPE2]
            },
            bad_values = [
                {<<"type">>, kingdom,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>, ?GROUP_TYPES)},
                {<<"type">>, 1234, ?ERROR_BAD_VALUE_ATOM(<<"type">>)}
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
        oz_test_utils:group_set_user_privileges(Config, G1, U1, [], [
            ?GROUP_DELETE
        ]),
        oz_test_utils:group_add_user(Config, G1, U2),
        oz_test_utils:group_set_user_privileges(Config, G1, U2, [
            ?GROUP_DELETE
        ], []),
        #{groupId => G1}
    end,
    DeleteEntityFun = fun(#{groupId := GroupId} = _Env) ->
        oz_test_utils:delete_group(Config, GroupId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{groupId := GroupId} = _Env, _) ->
        {ok, Groups} = oz_test_utils:list_groups(Config),
        ?assertEqual(lists:member(GroupId, Groups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_DELETE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, groupId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = delete,
            args = [auth, groupId],
            expected_result = ?OK_RES
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_group, id = groupId, aspect = instance},
            expected_result_op = ?OK_RES
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


protected_group_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, GroupId} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    oz_test_utils:group_set_user_privileges(Config, GroupId, U1, [
        ?GROUP_DELETE
    ], []),
    oz_test_utils:mark_group_protected(Config, GroupId, true),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_GROUPS_DELETE]}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, GroupId],
            expected_code = ?HTTP_403_FORBIDDEN
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = delete,
            args = [auth, GroupId],
            expected_result = ?ERROR_REASON(?ERROR_PROTECTED_GROUP)
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_group, id = GroupId, aspect = instance},
            expected_result_op = ?ERROR_REASON(?ERROR_PROTECTED_GROUP)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Verify that group exists
    {ok, Groups} = oz_test_utils:list_groups(Config),
    ?assertEqual(lists:member(GroupId, Groups), true).


get_oz_privileges_test(Config) ->
    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get group privileges and sometimes not)
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),

    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    InitialPrivs = [],
    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:group_set_oz_privileges(Config, G1, PrivsToGrant, PrivsToRevoke)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_VIEW_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_oz_privileges,
            args = [auth, G1],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U1}, ?OZ_VIEW_PRIVILEGES
    ])).


update_oz_privileges_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to update group privileges and sometimes not)
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:group_set_oz_privileges(Config, G1, PrivsToGrant, PrivsToRevoke),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config)
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:group_get_oz_privileges(Config, G1),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/groups/">>, G1, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = update_oz_privileges,
            args = [auth, G1, data],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U1}, ?OZ_SET_PRIVILEGES
    ])).


delete_oz_privileges_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:group_set_oz_privileges(Config, G1, PrivsToGrant, PrivsToRevoke),
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config)
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:group_get_oz_privileges(Config, G1),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = delete_oz_privileges,
            args = [auth, G1],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(delete_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, U1}, ?OZ_SET_PRIVILEGES
    ])).


get_eff_oz_privileges_test(Config) ->
    % User whose privileges will be changing during test run and as such
    % should not be listed in client spec (he will sometimes have privilege
    % to get group privileges and sometimes not)
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {Bottom, _Mid, _Top} = oz_test_utils:create_3_nested_groups(Config, U1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    InitialPrivs = [],
    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        % In case of GRANT, randomly split privileges into four
        % parts and update groups with the privileges. G3 eff_privileges
        % should contain the sum of those. In case of revoke, the
        % privileges must be revoked for all 3 entities.
        #{1 := PrivsToGrant1, 2 := PrivsToGrant2, 3 := PrivsToGrant3} = lists:foldl(
            fun(Privilege, AccMap) ->
                Index = rand:uniform(3),
                AccMap#{
                    Index => [Privilege | maps:get(Index, AccMap)]
                }
            end, #{1 => [], 2 => [], 3 => []}, PrivsToGrant),

        oz_test_utils:group_set_oz_privileges(Config, Bottom, PrivsToGrant1, PrivsToRevoke),
        oz_test_utils:group_set_oz_privileges(Config, Bottom, PrivsToGrant2, PrivsToRevoke),
        oz_test_utils:group_set_oz_privileges(Config, Bottom, PrivsToGrant3, PrivsToRevoke),

        oz_test_utils:ensure_entity_graph_is_up_to_date(Config)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_VIEW_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, Bottom, <<"/effective_privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_oz_privileges,
            args = [auth, Bottom],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },

    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, U1}, ?OZ_VIEW_PRIVILEGES
    ])).


list_eff_providers_test(Config) ->
    {
        [{P1, _}, {P2, _}, {P3, _}, {P4, _}],
        _Spaces, [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    ExpProviders = [P1, P2, P3, P4],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                % U2 does not have the GROUP_VIEW privilege
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/effective_providers">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"providers">> => ExpProviders}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_providers,
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpProviders)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_provider function
    lists:foreach(
        fun(ProviderId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_provider, [G1, ProviderId])
            )
        end, ExpProviders
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_provider, [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_provider_test(Config) ->
    {
        EffProvidersList, _Spaces, [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_providers_env(Config),

    lists:foreach(fun({ProvId, ProviderData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_PROVIDERS_VIEW]},
                    {user, U1}
                ],
                unauthorized = [nobody],
                forbidden = [
                    % U2 does not have the GROUP_VIEW privilege
                    {user, U2},
                    {user, NonAdmin}
                ]
            },
            rest_spec = #rest_spec{
                method = get,
                path = [
                    <<"/groups/">>, G1, <<"/effective_providers/">>, ProvId
                ],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:protected_provider(rest, ProvId, ProviderData)
            },
            logic_spec = #logic_spec{
                module = group_logic,
                function = get_eff_provider,
                args = [auth, G1, ProvId],
                expected_result = api_test_expect:protected_provider(logic, ProvId, ProviderData)
            }
            % @TODO VFS-4520 Tests for GraphSync API
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffProvidersList).


get_spaces_in_eff_provider_test(Config) ->
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_DETAILS(?UNIQUE_STRING)),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), ?UNIQUE_STRING),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U2), ?UNIQUE_STRING),

    {ok, S1_1} = oz_test_utils:create_space(Config, ?USER(U1), ?UNIQUE_STRING),
    oz_test_utils:space_add_group(Config, S1_1, G1),
    {ok, S1_2} = oz_test_utils:create_space(Config, ?USER(U1), ?UNIQUE_STRING),
    oz_test_utils:space_add_group(Config, S1_2, G1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U2), ?UNIQUE_STRING),
    oz_test_utils:space_add_group(Config, S2, G2),

    oz_test_utils:support_space_by_provider(Config, ProviderId, S1_1),
    oz_test_utils:support_space_by_provider(Config, ProviderId, S1_2),
    oz_test_utils:support_space_by_provider(Config, ProviderId, S2),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AnotherUser = fun(User) -> case User of
        U1 -> U2;
        U2 -> U1
    end end,

    lists:foreach(
        fun({ClientUserId, GroupId, GroupSpaces}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, ClientUserId}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, AnotherUser(ClientUserId)},
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/groups/">>, GroupId, <<"/effective_providers/">>, ProviderId, <<"/spaces">>],
                    expected_code = ?HTTP_200_OK,
                    expected_body = #{<<"spaces">> => GroupSpaces}
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_spaces_in_eff_provider,
                    args = [auth, GroupId, ProviderId],
                    expected_result = ?OK_LIST(GroupSpaces)
                }
                % @TODO VFS-4520 Tests for GraphSync API
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
        end, [{U1, G1, [S1_1, S1_2]}, {U2, G2, [S2]}]
    ).


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
    ozt_mocks:mock_handle_proxy(),
    ozt_mocks:mock_harvesting_backends(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unmock_harvesting_backends(),
    ozt_mocks:unmock_handle_proxy(),
    ozt_mocks:unfreeze_time().
