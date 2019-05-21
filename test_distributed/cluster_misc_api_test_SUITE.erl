%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning cluster basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_misc_api_test_SUITE).
-author("Lukasz Opiola").

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
-include_lib("ctool/include/onedata.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    list_test/1,
    get_oneprovider_cluster_test/1,
    get_onezone_cluster_test/1,
    update_onepanel_proxy_test/1,
    update_op_worker_version_info_test/1,
    update_op_panel_version_info_test/1,
    update_oz_worker_version_info_test/1,
    update_oz_panel_version_info_test/1
]).

all() ->
    ?ALL([
        list_test,
        get_oneprovider_cluster_test,
        get_onezone_cluster_test,
        update_onepanel_proxy_test,
        update_op_worker_version_info_test,
        update_op_panel_version_info_test,
        update_oz_worker_version_info_test,
        update_oz_panel_version_info_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_test(Config) ->
    % Make sure that clusters created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ExpClusters = [?ONEZONE_CLUSTER_ID] ++ lists:map(
        fun(_) ->
            {ok, {ProviderId, _MacaroonBin}} = oz_test_utils:create_provider(
                Config, U1, ?PROVIDER_NAME1
            ),
            _ClusterId = ProviderId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_LIST]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/clusters">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"clusters">> => ExpClusters}
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpClusters)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also cluster_logic:exist function
    lists:foreach(
        fun(Cluster) ->
            ?assert(oz_test_utils:call_oz(
                Config, cluster_logic, exists, [Cluster])
            )
        end, ExpClusters
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, cluster_logic, exists, [<<"asdiucyaie827346w">>])
    ).


get_onezone_cluster_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    GuiPackagePath = oz_test_utils:get_env(Config, gui_package_path),
    {ok, GuiHash} = oz_test_utils:call_oz(Config, gui, package_hash, [GuiPackagePath]),
    Release = oz_test_utils:call_oz(Config, oz_worker, get_version, []),
    Build = oz_test_utils:call_oz(Config, oz_worker, get_build_version, []),
    VersionInfo = {Release, Build, GuiHash},

    get_private_data_test_base(
        Config, ?ONEZONE_CLUSTER_ID, ?ONEZONE, VersionInfo,
        [],
        [{user, NonAdmin}]
    ),
    get_protected_data_test_base(
        Config, ?ONEZONE_CLUSTER_ID, ?ONEZONE, VersionInfo,
        [],
        [{user, NonAdmin}]
    ),
    get_public_data_test_base(
        Config, ?ONEZONE_CLUSTER_ID, ?ONEZONE, VersionInfo,
        [{user, NonAdmin}], % Every user of onezone is allowed to view public data
        []
    ).


get_oneprovider_cluster_test(Config) ->
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {ProviderId, Macaroon}} = oz_test_utils:create_provider(
        Config, ProviderAdmin, ?PROVIDER_NAME1
    ),
    ClusterId = ProviderId,
    {ok, EffUserOfProvider} = oz_test_utils:create_user(Config),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(EffUserOfProvider), ?UNIQUE_STRING),
    oz_test_utils:support_space(Config, ProviderId, Space),
    VersionInfo = {?DEFAULT_RELEASE_VERSION, ?DEFAULT_BUILD_VERSION, ?EMPTY_GUI_HASH},

    get_private_data_test_base(
        Config, ClusterId, ?ONEPROVIDER, VersionInfo,
        [{provider, ProviderId, Macaroon}],
        [{user, NonAdmin}, {user, EffUserOfProvider}]
    ),
    get_protected_data_test_base(
        Config, ClusterId, ?ONEPROVIDER, VersionInfo,
        [{provider, ProviderId, Macaroon}],
        [{user, NonAdmin}, {user, EffUserOfProvider}]
    ),
    get_public_data_test_base(
        Config, ClusterId, ?ONEPROVIDER, VersionInfo,
        % Every user of onezone is allowed to view public data
        [{provider, ProviderId, Macaroon}, {user, EffUserOfProvider}, {user, NonAdmin}],
        []
    ).


get_private_data_test_base(Config, ClusterId, ClusterType, VersionInfo, CorrectClients, ForbiddenClients) ->
    AllPrivsWithoutView = privileges:cluster_privileges() -- [?CLUSTER_VIEW],
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:cluster_add_user(Config, ClusterId, U1),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U1, AllPrivsWithoutView, [?CLUSTER_VIEW]),
    {ok, U2} = oz_test_utils:cluster_add_user(Config, ClusterId, U2),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U2, [?CLUSTER_VIEW], AllPrivsWithoutView),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ExpectedOnepanelProxy = false,

    % Get and check private data
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ] ++ CorrectClients,
            unauthorized = [nobody],
            forbidden = [
                {admin, [?OZ_CLUSTERS_VIEW]},
                {user, U1}
            ] ++ ForbiddenClients
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get,
            args = [client, ClusterId],
            expected_result = ?OK_TERM(
                fun(Cluster) ->
                    ?assertMatch(
                        #od_cluster{
                            type = ClusterType,

                            worker_version = VersionInfo,
                            onepanel_version = {?DEFAULT_RELEASE_VERSION, ?DEFAULT_BUILD_VERSION, ?EMPTY_GUI_HASH},
                            onepanel_proxy = ExpectedOnepanelProxy,

                            users = #{
                                U1 := AllPrivsWithoutView, U2 := [?CLUSTER_VIEW]
                            },
                            groups = #{},

                            eff_users = #{
                                U1 := {AllPrivsWithoutView, [{od_cluster, <<"self">>}]},
                                U2 := {[?CLUSTER_VIEW], [{od_cluster, <<"self">>}]}
                            },
                            eff_groups = #{},
                            bottom_up_dirty = false
                        },
                        Cluster
                    )
                end
            )
        }
        % @todo gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_protected_data_test_base(Config, ClusterId, ClusterType, VersionInfo, CorrectClients, ForbiddenClients) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:cluster_add_user(Config, ClusterId, U1),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U1, [], [?CLUSTER_VIEW]),
    {ok, U2} = oz_test_utils:cluster_add_user(Config, ClusterId, U2),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U2, [?CLUSTER_VIEW], []),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {Release, Build, Gui} = VersionInfo,
    ExpectedWorkerVersionInfoJson = #{
        <<"release">> => Release,
        <<"build">> => Build,
        <<"gui">> => Gui
    },
    ExpectedOnepanelVersionInfoJson = #{
        <<"release">> => ?DEFAULT_RELEASE_VERSION,
        <<"build">> => ?DEFAULT_BUILD_VERSION,
        <<"gui">> => ?EMPTY_GUI_HASH
    },
    ExpectedOnepanelProxy = false,

    ClusterDetails = #{
        <<"type">> => ClusterType,
        <<"workerVersion">> => ExpectedWorkerVersionInfoJson,
        <<"onepanelVersion">> => ExpectedOnepanelVersionInfoJson,
        <<"onepanelProxy">> => ExpectedOnepanelProxy
    },

    % Get and check protected data
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_VIEW]},
                {user, U1},
                {user, U2}
            ] ++ CorrectClients,
            unauthorized = [nobody],
            forbidden = ForbiddenClients
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/clusters/">>, ClusterId],
            expected_code = ?HTTP_200_OK,
            expected_body = {contains, ClusterDetails#{
                <<"clusterId">> => ClusterId,
                <<"type">> => atom_to_binary(ClusterType, utf8)
            }}
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_protected_data,
            args = [client, ClusterId],
            expected_result = ?OK_MAP_CONTAINS(ClusterDetails)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_public_data_test_base(Config, ClusterId, ClusterType, VersionInfo, CorrectClients, ForbiddenClients) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, AnyUser} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:cluster_add_user(Config, ClusterId, U1),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U1, [], [?CLUSTER_VIEW]),
    {ok, U2} = oz_test_utils:cluster_add_user(Config, ClusterId, U2),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U2, [?CLUSTER_VIEW], []),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {Release, Build, Gui} = VersionInfo,
    ExpectedWorkerVersionInfoJson = #{
        <<"release">> => Release,
        <<"build">> => Build,
        <<"gui">> => Gui
    },
    ExpectedOnepanelVersionInfoJson = #{
        <<"release">> => ?DEFAULT_RELEASE_VERSION,
        <<"build">> => ?DEFAULT_BUILD_VERSION,
        <<"gui">> => ?EMPTY_GUI_HASH
    },

    ClusterDetails = #{
        <<"type">> => ClusterType,
        <<"workerVersion">> => ExpectedWorkerVersionInfoJson,
        <<"onepanelVersion">> => ExpectedOnepanelVersionInfoJson
    },

    % Get and check public data
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_VIEW]},
                {user, U1},
                {user, U2},
                {user, AnyUser},
                nobody
            ] ++ CorrectClients,
            unauthorized = [],
            forbidden = ForbiddenClients
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = get_public_data,
            args = [client, ClusterId],
            expected_result = ?OK_MAP_CONTAINS(ClusterDetails)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_onepanel_proxy_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:cluster_privileges(),

    EnvSetUpFun = fun() ->
        {ok, {ProviderId, Macaroon}} = oz_test_utils:create_provider(
            Config, U1, ?PROVIDER_NAME1
        ),
        ClusterId = ProviderId,

        oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U1,
            AllPrivs -- [?CLUSTER_UPDATE], [?CLUSTER_UPDATE]
        ),
        {ok, U2} = oz_test_utils:cluster_add_user(Config, ClusterId, U2),
        oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U2,
            [?CLUSTER_UPDATE], AllPrivs -- [?CLUSTER_UPDATE]
        ),
        #{
            clusterId => ClusterId,
            providerClient => {provider, ProviderId, Macaroon}
        }
    end,

    VerifyEndFun = fun(ShouldSucceed, #{clusterId := ClusterId}, Data) ->
        {ok, Cluster} = oz_test_utils:get_cluster(Config, ClusterId),
        ExpOnepanelProxy = case ShouldSucceed of
            false -> false;
            true -> maps:get(<<"onepanelProxy">>, Data)
        end,
        ?assertEqual(ExpOnepanelProxy, Cluster#od_cluster.onepanel_proxy)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_UPDATE]},
                {user, U2},
                providerClient
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/clusters/">>, clusterId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = update,
            args = [client, clusterId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_cluster, id = clusterId, aspect = instance},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [<<"onepanelProxy">>],
            correct_values = #{<<"onepanelProxy">> => [true, false]},
            bad_values = [
                {<<"onepanelProxy">>, 1234, ?ERROR_BAD_VALUE_BOOLEAN(<<"onepanelProxy">>)},
                {<<"onepanelProxy">>, <<"abc">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"onepanelProxy">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


update_op_worker_version_info_test(Config) ->
    update_version_info_test_base(Config, ?ONEPROVIDER, ?WORKER).


update_op_panel_version_info_test(Config) ->
    update_version_info_test_base(Config, ?ONEPROVIDER, ?ONEPANEL).


update_oz_worker_version_info_test(Config) ->
    update_version_info_test_base(Config, ?ONEZONE, ?WORKER).


update_oz_panel_version_info_test(Config) ->
    update_version_info_test_base(Config, ?ONEZONE, ?ONEPANEL).


update_version_info_test_base(Config, ClusterType, ServiceType) ->
    Service = onedata:service_by_type(ClusterType, ServiceType),
    DataKey = case ServiceType of
        ?WORKER -> <<"workerVersion">>;
        ?ONEPANEL -> <<"onepanelVersion">>
    end,

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllPrivs = privileges:cluster_privileges(),

    EnvSetUpFun = fun() ->
        {ok, {ProviderId, Macaroon}} = oz_test_utils:create_provider(
            Config, ?PROVIDER_NAME1
        ),
        ClusterId = case ClusterType of
            ?ONEPROVIDER -> ProviderId;
            ?ONEZONE -> ?ONEZONE_CLUSTER_ID
        end,

        {ok, Cluster} = oz_test_utils:get_cluster(Config, ClusterId),
        oz_test_utils:call_oz(Config, cluster_logic, add_user, [?ROOT, ClusterId, U1]),
        oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U1,
            AllPrivs -- [?CLUSTER_UPDATE], [?CLUSTER_UPDATE]
        ),
        oz_test_utils:call_oz(Config, cluster_logic, add_user, [?ROOT, ClusterId, U2]),
        oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U2,
            [?CLUSTER_UPDATE], AllPrivs -- [?CLUSTER_UPDATE]
        ),
        #{
            clusterId => ClusterId,
            providerClient => {provider, ProviderId, Macaroon},
            previousVersionInfo => get_version_info(ServiceType, Cluster)
        }
    end,

    {ExistingGuiHash, _IndexContent} = oz_test_utils:deploy_dummy_gui(Config, onedata:service_shortname(Service)),
    BadGuiHash = <<"abcdefg">>,

    ExpRelease = <<"20.07.3">>,
    ExpBuild = <<"137-g6343ed1">>,
    ExpGui = ExistingGuiHash,

    DataWithCorrectHash = #{
        <<"release">> => ExpRelease,
        <<"build">> => ExpBuild,
        <<"gui">> => ExpGui
    },
    DataWithBadHash = #{
        <<"release">> => ExpRelease,
        <<"build">> => ExpBuild,
        <<"gui">> => BadGuiHash
    },

    VerifyEndFun = fun(ShouldSucceed, #{clusterId := ClusterId, previousVersionInfo := PreviousVersionInfo}, Data) ->
        {ok, Cluster = #od_cluster{type = ClusterType}} = oz_test_utils:get_cluster(Config, ClusterId),
        ExpVersionInfo = case {ClusterType, ShouldSucceed, maps:get(DataKey, Data)} of
            % 1) Correct client and data
            {_, true, DataWithCorrectHash} -> {ExpRelease, ExpBuild, ExpGui};
            % 2) Incorrect client, update should not be performed
            % (incorrect clients are always tested with correct data)
            {_, false, DataWithCorrectHash} -> PreviousVersionInfo;
            % 3) Correct client and bad (inexistent) GUI hash
            %   a) Oneprovider - version and build are still updated and GUI is set to empty
            %   b) Onezone - the operation fails
            {?ONEPROVIDER, false, DataWithBadHash} -> {ExpRelease, ExpBuild, ?EMPTY_GUI_HASH};
            {?ONEZONE, false, DataWithBadHash} -> PreviousVersionInfo;
            % 4) Correct client and invalid data
            {_, false, _} -> PreviousVersionInfo
        end,
        ?assertEqual(ExpVersionInfo, get_version_info(ServiceType, Cluster)),
        % Bring back the previous version info for next test cases
        oz_test_utils:call_oz(Config, cluster_logic, update_version_info, [
            ?ROOT, ClusterId, ServiceType, PreviousVersionInfo
        ])
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_CLUSTERS_UPDATE]},
                {user, U2}
            ] ++ case ClusterType of ?ONEPROVIDER -> [providerClient]; _ -> [] end,
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ] ++ case ClusterType of ?ONEZONE -> [providerClient]; _ -> [] end
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/clusters/">>, clusterId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = cluster_logic,
            function = update,
            args = [client, clusterId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_cluster, id = clusterId, aspect = instance},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [DataKey],
            correct_values = #{DataKey => [DataWithCorrectHash]},
            bad_values = [
                {DataKey, 1234, ?ERROR_BAD_VALUE_JSON(DataKey)},
                {DataKey, <<"abc">>, ?ERROR_BAD_VALUE_JSON(DataKey)},
                {DataKey, DataWithBadHash, ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<DataKey/binary, ".gui">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


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


init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_, Config) ->
    oz_test_utils:delete_all_entities(Config).


%%%===================================================================
%%% Helper functions
%%%===================================================================

get_version_info(?WORKER, #od_cluster{worker_version = VersionInfo}) ->
    VersionInfo;
get_version_info(?ONEPANEL, #od_cluster{onepanel_version = VersionInfo}) ->
    VersionInfo.
