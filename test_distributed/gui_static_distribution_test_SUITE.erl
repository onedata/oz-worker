%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning the mechanisms of uploading and linking
%%% static GUI files of different services.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_static_distribution_test_SUITE).
-author("Lukasz Opiola").

-include("http/rest.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-include("api_test_utils.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    oz_worker_gui_is_set_up_after_startup/1,
    oz_panel_gui_setup_works/1,
    default_harvester_gui_is_automatically_linked/1,
    default_harvester_gui_is_updated_on_deployment/1,
    empty_gui_is_linked_after_provider_registration/1,
    op_worker_and_panel_gui_is_linked_upon_version_info_update/1,
    gui_is_unlinked_after_provider_deletion/1,
    gui_upload_requires_provider_auth/1,
    gui_upload_is_not_possible_for_onezone_services/1,
    gui_upload_for_inexistent_service_returns_not_found/1,
    gui_upload_for_inexistent_cluster_returns_not_found/1,
    gui_upload_with_invalid_package_returns_proper_error/1,
    gui_upload_with_too_large_package_returns_proper_error/1,
    gui_upload_with_confined_or_no_token_returns_proper_error/1,
    gui_upload_page_deploys_op_worker_gui_on_all_nodes/1,
    gui_upload_page_deploys_op_panel_gui_on_all_nodes/1,
    gui_upload_page_deploys_harvester_gui_on_all_nodes/1,
    gui_package_verification_works/1,
    unused_packages_are_cleaned/1,
    empty_gui_is_linked_after_failed_op_worker_version_update/1,
    empty_gui_is_linked_after_failed_op_panel_version_update/1,
    custom_static_files_are_served/1,
    custom_static_files_are_served_from_legacy_location/1
]).

all() ->
    ?ALL([
        oz_worker_gui_is_set_up_after_startup,
        oz_panel_gui_setup_works,
        default_harvester_gui_is_automatically_linked,
        default_harvester_gui_is_updated_on_deployment,
        empty_gui_is_linked_after_provider_registration,
        op_worker_and_panel_gui_is_linked_upon_version_info_update,
        gui_is_unlinked_after_provider_deletion,
        gui_upload_requires_provider_auth,
        gui_upload_is_not_possible_for_onezone_services,
        gui_upload_for_inexistent_service_returns_not_found,
        gui_upload_for_inexistent_cluster_returns_not_found,
        gui_upload_with_invalid_package_returns_proper_error,
        gui_upload_with_too_large_package_returns_proper_error,
        gui_upload_with_confined_or_no_token_returns_proper_error,
        gui_upload_page_deploys_op_worker_gui_on_all_nodes,
        gui_upload_page_deploys_op_panel_gui_on_all_nodes,
        gui_upload_page_deploys_harvester_gui_on_all_nodes,
        gui_package_verification_works,
        unused_packages_are_cleaned,
        empty_gui_is_linked_after_failed_op_worker_version_update,
        empty_gui_is_linked_after_failed_op_panel_version_update,
        custom_static_files_are_served,
        custom_static_files_are_served_from_legacy_location
    ]).

-define(SSL_OPTS(Config), {ssl_options, [
    {hostname, oz_test_utils:oz_domain(Config)},
    {cacerts, oz_test_utils:gui_ca_certs(Config)}
]}).

-define(EMPTY_INDEX_CONTENT(Config, GuiType), read_content(
    Config, [<<"./">>, onedata:gui_prefix(GuiType), <<"/empty">>, <<"/i">>]
)).

%%%===================================================================
%%% Test functions
%%%===================================================================

oz_worker_gui_is_set_up_after_startup(Config) ->
    GuiPackagePath = oz_test_utils:get_env(Config, ozw_gui_package_path),
    {ok, GuiHash} = oz_test_utils:call_oz(Config, gui, package_hash, [GuiPackagePath]),
    Release = oz_test_utils:call_oz(Config, oz_worker, get_release_version, []),
    Build = oz_test_utils:call_oz(Config, oz_worker, get_build_version, []),

    OzIndexContent = read_content(Config, [<<"./ozw/">>, GuiHash, <<"/index.html">>]),

    ?assert(static_directory_exists(Config, [<<"./ozw/">>, GuiHash])),
    ?assert(link_exists(Config, <<"./ozw/onezone">>, GuiHash)),
    ?assert(file_is_served(Config, OzIndexContent, <<"/">>)),
    ?assert(file_is_served(Config, OzIndexContent, [<<"/ozw/">>, ?ONEZONE_CLUSTER_ID, <<"/i">>])),
    ?assert(file_is_served(Config, OzIndexContent, [<<"/ozw/">>, ?ONEZONE_CLUSTER_ID, <<"/index.html">>])),
    ?assert(version_info_is_set(Config, ?ONEZONE_CLUSTER_ID, ?WORKER, {Release, Build, GuiHash})).

oz_panel_gui_setup_works(Config) ->
    % Simulate cluster setup procedure performed by oz_panel
    Release = <<"mock-release">>,
    Build = <<"mock-build">>,
    {GuiPackage, IndexContent} = oz_test_utils:create_dummy_gui_package(),
    oz_test_utils:copy_file_to_onezone_nodes(Config, GuiPackage),
    ReleaseVersion = oz_test_utils:call_oz(Config, oz_worker, get_release_version, []),

    {ok, GuiHash} = oz_test_utils:call_oz(Config, gui_static, deploy_package, [
        ?ONEPANEL_GUI, ReleaseVersion, GuiPackage
    ]),
    ok = oz_test_utils:call_oz(Config, cluster_logic, update_version_info, [
        ?ROOT, ?ONEZONE_CLUSTER_ID, ?ONEPANEL, {Release, Build, GuiHash}
    ]),

    ?assert(static_directory_exists(Config, [<<"./onp/">>, GuiHash])),
    ?assert(link_exists(Config, <<"./onp/onezone">>, GuiHash)),
    ?assert(file_is_served(Config, IndexContent, [<<"/onp/">>, ?ONEZONE_CLUSTER_ID, <<"/i">>])),
    ?assert(file_is_served(Config, IndexContent, [<<"/onp/">>, ?ONEZONE_CLUSTER_ID, <<"/index.html">>])),
    ?assert(version_info_is_set(Config, ?ONEZONE_CLUSTER_ID, ?ONEPANEL, {Release, Build, GuiHash})).

default_harvester_gui_is_automatically_linked(Config) ->
    HarvesterId = ozt_harvesters:create(),
    HrvIndexContent = read_content(Config, [<<"./hrv/default/index.html">>]),

    ?assert(static_directory_exists(Config, [<<"./hrv/default">>])),
    ?assert(link_exists(Config, <<"./hrv/", HarvesterId/binary>>, <<"default">>)),
    ?assert(file_is_served(Config, HrvIndexContent, [<<"/hrv/">>, HarvesterId, <<"/i">>])),
    ?assert(file_is_served(Config, HrvIndexContent, [<<"/hrv/">>, HarvesterId, <<"/index.html">>])).


default_harvester_gui_is_updated_on_deployment(Config) ->
    {TmpPath, NewHrvIndexContent} = oz_test_utils:create_dummy_gui_package(),
    ?assertNotEqual(NewHrvIndexContent, read_content(Config, [<<"./hrv/default/index.html">>])),

    % replace harvester gui package on all nodes
    Path = oz_test_utils:get_env(Config, default_hrv_gui_package_path),
    AbsPath = oz_test_utils:call_oz(Config, filename, absname, [Path]),
    {ok, Package} = file:read_file(TmpPath),
    Nodes = ?config(oz_worker_nodes, Config),
    lists:foreach(fun(Node) ->
        ok = rpc:call(Node, filelib, ensure_dir, [AbsPath]),
        ok = rpc:call(Node, file, write_file, [AbsPath, Package])
    end, Nodes),

    oz_test_utils:call_oz(Config, harvester_logic, deploy_default_gui_package, []),
    ?assertEqual(NewHrvIndexContent, read_content(Config, [<<"./hrv/default/index.html">>])).


empty_gui_is_linked_after_provider_registration(Config) ->
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ClusterId = ProviderId,

    ?assert(static_directory_exists(Config, [<<"./opw/">>, <<"empty">>])),
    ?assert(link_exists(Config, [<<"./opw/">>, ClusterId], <<"empty">>)),
    ?assert(file_is_served(Config, ?EMPTY_INDEX_CONTENT(Config, ?OP_WORKER_GUI), [<<"/opw/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(Config, ?EMPTY_INDEX_CONTENT(Config, ?OP_WORKER_GUI), [<<"/opw/">>, ClusterId, <<"/index.html">>])),
    ?assert(version_info_is_set(Config, ClusterId, ?WORKER, {?DEFAULT_RELEASE_VERSION, ?DEFAULT_BUILD_VERSION, ?EMPTY_GUI_HASH})),

    ?assert(static_directory_exists(Config, [<<"./onp/">>, <<"empty">>])),
    ?assert(link_exists(Config, [<<"./onp/">>, ClusterId], <<"empty">>)),
    ?assert(file_is_served(Config, ?EMPTY_INDEX_CONTENT(Config, ?ONEPANEL_GUI), [<<"/onp/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(Config, ?EMPTY_INDEX_CONTENT(Config, ?ONEPANEL_GUI), [<<"/onp/">>, ClusterId, <<"/index.html">>])),
    ?assert(version_info_is_set(Config, ClusterId, ?ONEPANEL, {?DEFAULT_RELEASE_VERSION, ?DEFAULT_BUILD_VERSION, ?EMPTY_GUI_HASH})).


op_worker_and_panel_gui_is_linked_upon_version_info_update(Config) ->
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ClusterId = ProviderId,

    {OpGuiHash, OpIndexContent} = oz_test_utils:deploy_dummy_gui(Config, ?OP_WORKER_GUI),

    oz_test_utils:call_oz(Config, cluster_logic, update_version_info, [
        ?PROVIDER(ProviderId), ClusterId, ?WORKER, {<<"18.07.1-WORKER">>, <<"build-WORKER">>, OpGuiHash}
    ]),

    ?assert(static_directory_exists(Config, [<<"./opw/">>, OpGuiHash])),
    ?assert(link_exists(Config, [<<"./opw/">>, ClusterId], OpGuiHash)),
    ?assert(file_is_served(Config, OpIndexContent, [<<"/opw/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(Config, OpIndexContent, [<<"/opw/">>, ClusterId, <<"/index.html">>])),
    ?assert(version_info_is_set(Config, ClusterId, ?WORKER, {<<"18.07.1-WORKER">>, <<"build-WORKER">>, OpGuiHash})),

    ?assert(static_directory_exists(Config, [<<"./onp/">>, <<"empty">>])),
    ?assert(link_exists(Config, [<<"./onp/">>, ClusterId], <<"empty">>)),
    ?assert(file_is_served(Config, ?EMPTY_INDEX_CONTENT(Config, ?ONEPANEL_GUI), [<<"/onp/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(Config, ?EMPTY_INDEX_CONTENT(Config, ?ONEPANEL_GUI), [<<"/onp/">>, ClusterId, <<"/index.html">>])),
    ?assert(version_info_is_set(Config, ClusterId, ?ONEPANEL, {?DEFAULT_RELEASE_VERSION, ?DEFAULT_BUILD_VERSION, ?EMPTY_GUI_HASH})),

    {OppGuiHash, OppIndexContent} = oz_test_utils:deploy_dummy_gui(Config, ?ONEPANEL_GUI),
    oz_test_utils:call_oz(Config, cluster_logic, update_version_info, [
        ?PROVIDER(ProviderId), ClusterId, ?ONEPANEL, {<<"18.07.1-PANEL">>, <<"build-PANEL">>, OppGuiHash}
    ]),

    ?assert(static_directory_exists(Config, [<<"./opw/">>, OpGuiHash])),
    ?assert(link_exists(Config, [<<"./opw/">>, ClusterId], OpGuiHash)),
    ?assert(file_is_served(Config, OpIndexContent, [<<"/opw/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(Config, OpIndexContent, [<<"/opw/">>, ClusterId, <<"/index.html">>])),
    ?assert(version_info_is_set(Config, ClusterId, ?WORKER, {<<"18.07.1-WORKER">>, <<"build-WORKER">>, OpGuiHash})),

    ?assert(static_directory_exists(Config, [<<"./onp/">>, OppGuiHash])),
    ?assert(link_exists(Config, [<<"./onp/">>, ClusterId], OppGuiHash)),
    ?assert(file_is_served(Config, OppIndexContent, [<<"/onp/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(Config, OppIndexContent, [<<"/onp/">>, ClusterId, <<"/index.html">>])),
    ?assert(version_info_is_set(Config, ClusterId, ?ONEPANEL, {<<"18.07.1-PANEL">>, <<"build-PANEL">>, OppGuiHash})).


gui_is_unlinked_after_provider_deletion(Config) ->
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ClusterId = ProviderId,

    {OpGuiHash, OpIndexContent} = oz_test_utils:deploy_dummy_gui(Config, ?OP_WORKER_GUI),
    oz_test_utils:call_oz(Config, cluster_logic, update_version_info, [
        ?PROVIDER(ProviderId), ClusterId, ?WORKER, {<<"18.07.1-WORKER">>, <<"build-WORKER">>, OpGuiHash}
    ]),
    {OppGuiHash, OppIndexContent} = oz_test_utils:deploy_dummy_gui(Config, ?ONEPANEL_GUI),
    oz_test_utils:call_oz(Config, cluster_logic, update_version_info, [
        ?PROVIDER(ProviderId), ClusterId, ?ONEPANEL, {<<"18.07.1-PANEL">>, <<"build-PANEL">>, OppGuiHash}
    ]),

    oz_test_utils:delete_provider(Config, ProviderId),
    ?assertMatch({ok, false}, oz_test_utils:call_oz(Config, od_cluster, exists, [ClusterId])),

    % Gui package should be left on the disk, but the link to it for given cluster removed
    ?assert(static_directory_exists(true, Config, [<<"./opw/">>, OpGuiHash])),
    ?assert(link_exists(false, Config, [<<"./opw/">>, ClusterId], OpGuiHash)),
    ?assert(file_is_served(false, Config, OpIndexContent, [<<"/opw/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(false, Config, OpIndexContent, [<<"/opw/">>, ClusterId, <<"/index.html">>])),

    ?assert(static_directory_exists(true, Config, [<<"./onp/">>, OppGuiHash])),
    ?assert(link_exists(false, Config, [<<"./onp/">>, ClusterId], OppGuiHash)),
    ?assert(file_is_served(false, Config, OppIndexContent, [<<"/onp/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(false, Config, OppIndexContent, [<<"/onp/">>, ClusterId, <<"/index.html">>])).


gui_upload_requires_provider_auth(Config) ->
    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ClusterId = ProviderId,
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),

    % No auth
    ?assertMatch(?ERROR_UNAUTHORIZED, perform_upload(
        Config, <<"opw">>, ClusterId, GuiPackage, #{}
    )),
    ?assertMatch(?ERROR_UNAUTHORIZED, perform_upload(
        Config, <<"onp">>, ClusterId, GuiPackage, #{}
    )),

    % With auth
    ?assertMatch(ok, perform_upload(
        Config, <<"opw">>, ClusterId, GuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),
    ?assertMatch(ok, perform_upload(
        Config, <<"onp">>, ClusterId, GuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),

    % Upload is only possible for own cluster
    {ok, {_AnotherProvider, AnotherProviderToken}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ?assertMatch(?ERROR_FORBIDDEN, perform_upload(
        Config, <<"opw">>, ClusterId, GuiPackage, #{?HDR_X_AUTH_TOKEN => AnotherProviderToken}
    )),
    ?assertMatch(?ERROR_FORBIDDEN, perform_upload(
        Config, <<"onp">>, ClusterId, GuiPackage, #{?HDR_X_AUTH_TOKEN => AnotherProviderToken}
    )).


gui_upload_is_not_possible_for_onezone_services(Config) ->
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),

    % With no auth
    ?assertMatch(?ERROR_NOT_FOUND, perform_upload(
        Config, <<"ozw">>, ?ONEZONE_CLUSTER_ID, GuiPackage, #{}
    )),
    ?assertMatch(?ERROR_NOT_FOUND, perform_upload(
        Config, <<"onp">>, ?ONEZONE_CLUSTER_ID, GuiPackage, #{}
    )),

    % With provider auth
    {ok, {_Provider, ProviderToken}} = oz_test_utils:create_provider(Config),
    ?assertMatch(?ERROR_NOT_FOUND, perform_upload(
        Config, <<"ozw">>, ?ONEZONE_CLUSTER_ID, GuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),
    ?assertMatch(?ERROR_NOT_FOUND, perform_upload(
        Config, <<"onp">>, ?ONEZONE_CLUSTER_ID, GuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )).


gui_upload_for_inexistent_service_returns_not_found(Config) ->
    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ClusterId = ProviderId,
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),

    ?assertMatch(?ERROR_NOT_FOUND, perform_upload(
        Config, <<"abc">>, ClusterId, GuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),
    ?assertMatch(?ERROR_NOT_FOUND, perform_upload(
        Config, <<"xyz">>, ClusterId, GuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )).


gui_upload_for_inexistent_cluster_returns_not_found(Config) ->
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),

    ?assertMatch(?ERROR_NOT_FOUND, perform_upload(
        Config, <<"opw">>, <<"bad-cluster-id">>, GuiPackage, #{}
    )),
    ?assertMatch(?ERROR_NOT_FOUND, perform_upload(
        Config, <<"onp">>, <<"bad-cluster-id">>, GuiPackage, #{}
    )).


gui_upload_with_invalid_package_returns_proper_error(Config) ->
    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ClusterId = ProviderId,
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),
    % Break the package a little bit
    {ok, Contents} = file:read_file(GuiPackage),
    ok = file:write_file(GuiPackage, <<"lalalalala-bad-bytes", Contents/binary>>),

    ?assertMatch(?ERROR_BAD_GUI_PACKAGE, perform_upload(
        Config, <<"opw">>, ClusterId, GuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),
    ?assertMatch(?ERROR_BAD_GUI_PACKAGE, perform_upload(
        Config, <<"onp">>, ClusterId, GuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )).


gui_upload_with_too_large_package_returns_proper_error(Config) ->
    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ClusterId = ProviderId,
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),

    % Set the package limit to one byte
    oz_test_utils:set_app_env(Config, gui, max_gui_package_size_mb, 1 / 1048576),

    ?assertMatch(?ERROR_GUI_PACKAGE_TOO_LARGE, perform_upload(
        Config, <<"opw">>, ClusterId, GuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),
    ?assertMatch(?ERROR_GUI_PACKAGE_TOO_LARGE, perform_upload(
        Config, <<"onp">>, ClusterId, GuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )).


gui_upload_with_confined_or_no_token_returns_proper_error(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, HarvesterId} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    oz_test_utils:harvester_add_user(Config, HarvesterId, U1),
    oz_test_utils:harvester_set_user_privileges(Config, HarvesterId, U1, [?HARVESTER_UPDATE], []),
    {ok, {_SessionId, SessionCookie}} = oz_test_utils:log_in(Config, U1),
    {ok, GuiToken} = oz_test_utils:request_gui_token(Config, SessionCookie),
    {HrvGuiPackage, _} = oz_test_utils:create_dummy_gui_package(),

    UserCaveat = #cv_data_path{whitelist = [<<"/abc">>]},
    ConfinedGuiToken = tokens:confine(GuiToken, UserCaveat),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(UserCaveat)), perform_upload(
        Config, <<"hrv">>, HarvesterId, HrvGuiPackage, #{?HDR_X_AUTH_TOKEN => ConfinedGuiToken}
    )),

    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config),
    {OpGuiPackage, _} = oz_test_utils:create_dummy_gui_package(),
    ProviderCaveat = #cv_api{whitelist = [{?OZ_WORKER, get, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')}]},
    ConfinedProviderToken = tokens:confine(ProviderToken, ProviderCaveat),
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(ProviderCaveat)), perform_upload(
        Config, <<"opw">>, ProviderId, OpGuiPackage, #{?HDR_X_AUTH_TOKEN => ConfinedProviderToken}
    )),

    % Harvester GUI upload with no auth should cause UNAUTHORIZED error
    ?assertMatch(?ERROR_UNAUTHORIZED, perform_upload(
        Config, <<"hrv">>, HarvesterId, HrvGuiPackage, #{}
    )).


gui_upload_page_deploys_op_worker_gui_on_all_nodes(Config) ->
    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config),
    ClusterId = ProviderId,

    {OpGuiPackage, OpIndexContent} = oz_test_utils:create_dummy_gui_package(),
    {ok, OpGuiHash} = gui:package_hash(OpGuiPackage),

    ?assertNot(oz_test_utils:call_oz(Config, gui_static, gui_exists, [?OP_WORKER_GUI, OpGuiHash])),
    ?assertMatch(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"workerVersion.gui">>), oz_test_utils:call_oz(
        Config, cluster_logic, update_version_info, [
            ?PROVIDER(ProviderId), ClusterId, ?WORKER, {<<"18.07.1">>, <<"build">>, OpGuiHash}
        ]
    )),
    % Failed version update still sets the release/build version
    ?assert(version_info_is_set(Config, ClusterId, ?WORKER, {<<"18.07.1">>, <<"build">>, ?EMPTY_GUI_HASH})),

    ?assertMatch(ok, perform_upload(
        Config, <<"opw">>, ClusterId, OpGuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),

    ?assert(oz_test_utils:call_oz(Config, gui_static, gui_exists, [?OP_WORKER_GUI, OpGuiHash])),
    ?assertMatch(ok, oz_test_utils:call_oz(
        Config, cluster_logic, update_version_info, [
            ?PROVIDER(ProviderId), ClusterId, ?WORKER, {<<"18.07.1">>, <<"build">>, OpGuiHash}
        ]
    )),

    ?assert(static_directory_exists(Config, [<<"./opw/">>, OpGuiHash])),
    ?assert(link_exists(Config, [<<"./opw/">>, ClusterId], OpGuiHash)),
    ?assert(file_is_served(Config, OpIndexContent, [<<"/opw/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(Config, OpIndexContent, [<<"/opw/">>, ClusterId, <<"/index.html">>])),
    ?assert(version_info_is_set(Config, ClusterId, ?WORKER, {<<"18.07.1">>, <<"build">>, OpGuiHash})).


gui_upload_page_deploys_op_panel_gui_on_all_nodes(Config) ->
    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config),
    ClusterId = ProviderId,

    {OppGuiPackage, OppIndexContent} = oz_test_utils:create_dummy_gui_package(),
    {ok, OppGuiHash} = gui:package_hash(OppGuiPackage),

    ?assertNot(oz_test_utils:call_oz(Config, gui_static, gui_exists, [?ONEPANEL_GUI, OppGuiHash])),
    ?assertMatch(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"onepanelVersion.gui">>), oz_test_utils:call_oz(
        Config, cluster_logic, update_version_info, [
            ?PROVIDER(ProviderId), ClusterId, ?ONEPANEL, {<<"18.07.1">>, <<"build">>, OppGuiHash}
        ]
    )),
    % Failed version update still sets the release/build version
    ?assert(version_info_is_set(Config, ClusterId, ?ONEPANEL, {<<"18.07.1">>, <<"build">>, ?EMPTY_GUI_HASH})),

    ?assertMatch(ok, perform_upload(
        Config, <<"onp">>, ClusterId, OppGuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),

    ?assert(oz_test_utils:call_oz(Config, gui_static, gui_exists, [?ONEPANEL_GUI, OppGuiHash])),
    ?assertMatch(ok, oz_test_utils:call_oz(
        Config, cluster_logic, update_version_info, [
            ?PROVIDER(ProviderId), ClusterId, ?ONEPANEL, {<<"18.07.1">>, <<"build">>, OppGuiHash}
        ]
    )),

    ?assert(static_directory_exists(Config, [<<"./onp/">>, OppGuiHash])),
    ?assert(link_exists(Config, [<<"./onp/">>, ClusterId], OppGuiHash)),
    ?assert(file_is_served(Config, OppIndexContent, [<<"/onp/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(Config, OppIndexContent, [<<"/onp/">>, ClusterId, <<"/index.html">>])),
    ?assert(version_info_is_set(Config, ClusterId, ?ONEPANEL, {<<"18.07.1">>, <<"build">>, OppGuiHash})).


gui_upload_page_deploys_harvester_gui_on_all_nodes(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, HarvesterId} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    oz_test_utils:harvester_add_user(Config, HarvesterId, U1),
    oz_test_utils:harvester_set_user_privileges(Config, HarvesterId, U1, [?HARVESTER_UPDATE], []),

    {ok, {_SessionId, SessionCookie}} = oz_test_utils:log_in(Config, U1),
    {ok, GuiToken} = oz_test_utils:request_gui_token(Config, SessionCookie),

    {HrvGuiPackage, HrvIndexContent} = oz_test_utils:create_dummy_gui_package(),
    {ok, HrvGuiHash} = gui:package_hash(HrvGuiPackage),

    ?assertNot(oz_test_utils:call_oz(Config, gui_static, gui_exists, [?HARVESTER_GUI, HrvGuiHash])),

    ?assertMatch(ok, perform_upload(
        Config, <<"hrv">>, HarvesterId, HrvGuiPackage, #{
            ?HDR_X_AUTH_TOKEN => GuiToken, ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", SessionCookie/binary>>
        }
    )),

    % without the session cookie, upload fails (as the GUI token is bound to the cookie)
    ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SESSION_INVALID), perform_upload(
        Config, <<"hrv">>, HarvesterId, HrvGuiPackage, #{?HDR_X_AUTH_TOKEN => GuiToken}
    )),

    ?assert(oz_test_utils:call_oz(Config, gui_static, gui_exists, [?HARVESTER_GUI, HrvGuiHash])),

    ?assert(static_directory_exists(Config, [<<"./hrv/">>, HrvGuiHash])),
    ?assert(link_exists(Config, [<<"./hrv/">>, HarvesterId], HrvGuiHash)),
    ?assert(file_is_served(Config, HrvIndexContent, [<<"/hrv/">>, HarvesterId, <<"/i">>])),
    ?assert(file_is_served(Config, HrvIndexContent, [<<"/hrv/">>, HarvesterId, <<"/index.html">>])).


gui_package_verification_works(Config) ->
    oz_test_utils:set_env(Config, gui_package_verification, true),
    oz_test_utils:set_app_env(Config, ctool, compatibility_registry_mirrors, []),
    OnezoneVersion = oz_test_utils:call_oz(Config, oz_worker, get_release_version, []),

    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(Config),
    ClusterId = ProviderId,

    {OpGuiPackage, _OpIndexContent} = oz_test_utils:create_dummy_gui_package(),
    {ok, OpGuiHash} = gui:package_hash(OpGuiPackage),

    % GUI hash is not whitelisted
    ?assertMatch(?ERROR_GUI_PACKAGE_UNVERIFIED(OpGuiHash), perform_upload(
        Config, <<"opw">>, ClusterId, OpGuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),

    % Whitelist the GUI hash:
    oz_test_utils:overwrite_compatibility_registry(Config, #{
        <<"revision">> => 2019010100,
        <<"gui-sha256">> => #{
            <<"op-worker">> => #{
                ?DEFAULT_RELEASE_VERSION => [OpGuiHash]
            }
        }
    }),

    % Now, upload should work
    ?assertMatch(ok, perform_upload(
        Config, <<"opw">>, ClusterId, OpGuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),

    % Make sure that only packages for op-worker with such hash are accepted
    ?assertMatch(?ERROR_GUI_PACKAGE_UNVERIFIED(OpGuiHash), perform_upload(
        Config, <<"onp">>, ClusterId, OpGuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),

    % When verification is disabled, any package should be accepted
    oz_test_utils:set_env(Config, gui_package_verification, false),
    ?assertMatch(ok, perform_upload(
        Config, <<"opw">>, ClusterId, OpGuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),
    ?assertMatch(ok, perform_upload(
        Config, <<"onp">>, ClusterId, OpGuiPackage, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),

    % Harvester GUI package verification can be turned off using a separate env
    oz_test_utils:set_env(Config, gui_package_verification, true),
    oz_test_utils:set_env(Config, harvester_gui_package_verification, true),

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, HarvesterId} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    oz_test_utils:harvester_add_user(Config, HarvesterId, U1),
    oz_test_utils:harvester_set_user_privileges(Config, HarvesterId, U1, [?HARVESTER_UPDATE], []),
    {ok, {_SessionId, SessionCookie}} = oz_test_utils:log_in(Config, U1),
    {ok, GuiToken} = oz_test_utils:request_gui_token(Config, SessionCookie),

    {HrvGuiPackage1, _HrvIndexContent1} = oz_test_utils:create_dummy_gui_package(),
    {ok, HrvGuiHash1} = gui:package_hash(HrvGuiPackage1),

    % GUI hash is not whitelisted
    ?assertMatch(?ERROR_GUI_PACKAGE_UNVERIFIED(HrvGuiHash1), perform_upload(
        Config, <<"hrv">>, HarvesterId, HrvGuiPackage1, #{
            ?HDR_X_AUTH_TOKEN => GuiToken, ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", SessionCookie/binary>>
        }
    )),

    % Whitelist the GUI hash:
    oz_test_utils:overwrite_compatibility_registry(Config, #{
        <<"revision">> => 2019010100,
        <<"gui-sha256">> => #{
            %% Harvester GUI entries have another nesting level with human-readable labels
            <<"harvester">> => #{
                OnezoneVersion => #{
                    <<"ecrin">> => [HrvGuiHash1]
                }
            }
        }
    }),
    ?assertMatch(ok, perform_upload(
        Config, <<"hrv">>, HarvesterId, HrvGuiPackage1, #{
            ?HDR_X_AUTH_TOKEN => GuiToken, ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", SessionCookie/binary>>
        }
    )),

    % Unknown hash should not be accepted
    {HrvGuiPackage2, _HrvIndexContent2} = oz_test_utils:create_dummy_gui_package(),
    {ok, HrvGuiHash2} = gui:package_hash(HrvGuiPackage2),

    ?assertMatch(?ERROR_GUI_PACKAGE_UNVERIFIED(HrvGuiHash2), perform_upload(
        Config, <<"hrv">>, HarvesterId, HrvGuiPackage2, #{
            ?HDR_X_AUTH_TOKEN => GuiToken, ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", SessionCookie/binary>>
        }
    )),

    % Disable GUI package verification for harvesters only
    oz_test_utils:set_env(Config, harvester_gui_package_verification, false),
    ?assertMatch(ok, perform_upload(
        Config, <<"hrv">>, HarvesterId, HrvGuiPackage2, #{
            ?HDR_X_AUTH_TOKEN => GuiToken, ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", SessionCookie/binary>>
        }
    )),

    % It should not work for other services
    ?assertMatch(?ERROR_GUI_PACKAGE_UNVERIFIED(HrvGuiHash2), perform_upload(
        Config, <<"onp">>, ClusterId, HrvGuiPackage2, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),

    % When verification is disabled, any package for any service should be accepted
    oz_test_utils:set_env(Config, gui_package_verification, false),
    ?assertMatch(ok, perform_upload(
        Config, <<"onp">>, ClusterId, HrvGuiPackage2, #{?HDR_X_AUTH_TOKEN => ProviderToken}
    )),

    {HrvGuiPackage3, _HrvIndexContent3} = oz_test_utils:create_dummy_gui_package(),
    ?assertMatch(ok, perform_upload(
        Config, <<"hrv">>, HarvesterId, HrvGuiPackage3, #{
            ?HDR_X_AUTH_TOKEN => GuiToken, ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", SessionCookie/binary>>
        }
    )).


unused_packages_are_cleaned(Config) ->
    % Package verification is disabled, hence version can be anything
    LinkGui = fun(GuiType, GuiId, GuiHash) ->
        ?assertMatch(ok, oz_test_utils:call_oz(
            Config, gui_static, link_gui, [GuiType, GuiId, GuiHash]
        ))
    end,
    UnlinkGui = fun(GuiType, GuiId) ->
        ?assertMatch(ok, oz_test_utils:call_oz(
            Config, gui_static, unlink_gui, [GuiType, GuiId]
        ))
    end,

    {ok, {P1, _}} = oz_test_utils:create_provider(Config),
    {ok, {P2, _}} = oz_test_utils:create_provider(Config),

    {HashAlpha, _} = oz_test_utils:deploy_dummy_gui(Config, ?OP_WORKER_GUI),
    {HashBeta, _} = oz_test_utils:deploy_dummy_gui(Config, ?OP_WORKER_GUI),
    {HashGamma, _} = oz_test_utils:deploy_dummy_gui(Config, ?OZ_WORKER_GUI),
    {HashDelta, _} = oz_test_utils:deploy_dummy_gui(Config, ?ONEPANEL_GUI),
    {HashTheta, _} = oz_test_utils:deploy_dummy_gui(Config, ?ONEPANEL_GUI),
    {HashSigma, _} = oz_test_utils:deploy_dummy_gui(Config, ?ONEPANEL_GUI),
    {HashOmega, _} = oz_test_utils:deploy_dummy_gui(Config, ?HARVESTER_GUI),

    LinkGui(?OP_WORKER_GUI, P1, HashAlpha),
    LinkGui(?OP_WORKER_GUI, P2, HashAlpha),
    LinkGui(?OZ_WORKER_GUI, ?ONEZONE_CLUSTER_ID, HashGamma),
    LinkGui(?ONEPANEL_GUI, ?ONEZONE_CLUSTER_ID, HashDelta),
    LinkGui(?ONEPANEL_GUI, P1, HashTheta),
    LinkGui(?ONEPANEL_GUI, P2, HashTheta),
    LinkGui(?HARVESTER_GUI, <<"hrv1">>, HashOmega),
    LinkGui(?HARVESTER_GUI, <<"hrv2">>, HashOmega),

    % Currently, Beta and Sigma are not used, but they should not be removed
    % sooner than a day after last modification.
    ?assert(static_directory_exists(true, Config, [<<"./opw/">>, HashBeta])),
    ?assert(static_directory_exists(true, Config, [<<"./onp/">>, HashSigma])),

    % Simulate that a day has passed
    Day = 3600 * 24,
    set_mocked_seconds_since_modification(Config, Day + 1),

    % Any linking/unlinking should trigger a cleanup
    UnlinkGui(?OZ_WORKER_GUI, ?ONEZONE_CLUSTER_ID),  % Now, Gamma is also unused
    ?assert(static_directory_exists(false, 60, Config, [<<"./opw/">>, HashBeta])),
    ?assert(static_directory_exists(false, 60, Config, [<<"./onp/">>, HashSigma])),
    ?assert(static_directory_exists(false, 60, Config, [<<"./ozw/">>, HashGamma])),

    ?assert(static_directory_exists(true, Config, [<<"./opw/">>, HashAlpha])),
    ?assert(static_directory_exists(true, Config, [<<"./onp/">>, HashDelta])),
    ?assert(static_directory_exists(true, Config, [<<"./onp/">>, HashTheta])),
    ?assert(static_directory_exists(true, Config, [<<"./hrv/">>, HashOmega])),

    UnlinkGui(?HARVESTER_GUI, <<"hrv1">>),
    % Omega is still used by another harvester
    ?assert(static_directory_exists(true, Config, [<<"./hrv/">>, HashOmega])),
    UnlinkGui(?HARVESTER_GUI, <<"hrv2">>),
    % But now it should be cleaned
    ?assert(static_directory_exists(false, 60, Config, [<<"./hrv/">>, HashOmega])),

    % Empty GUI placeholders and default harvester GUI should be exempt from
    % periodic cleaning and still be around, despite nothing being linked to them
    ?assert(static_directory_exists(true, Config, [<<"./ozw/">>, ?EMPTY_GUI_HASH])),
    ?assert(static_directory_exists(true, Config, [<<"./opw/">>, ?EMPTY_GUI_HASH])),
    ?assert(static_directory_exists(true, Config, [<<"./onp/">>, ?EMPTY_GUI_HASH])),
    ?assert(static_directory_exists(true, Config, [<<"./hrv/">>, <<"default">>])).


empty_gui_is_linked_after_failed_op_worker_version_update(Config) ->
    {ok, {ProviderId, _ProviderToken}} = oz_test_utils:create_provider(Config),
    ClusterId = ProviderId,

    % First link correct GUI to provider
    {OpGuiHash, _OpIndexContent} = oz_test_utils:deploy_dummy_gui(Config, ?OP_WORKER_GUI),
    ?assertMatch(ok, oz_test_utils:call_oz(
        Config, cluster_logic, update_version_info, [
            ?PROVIDER(ProviderId), ClusterId, ?WORKER, {<<"18.07.1">>, <<"build-1">>, OpGuiHash}
        ]
    )),
    ?assert(version_info_is_set(Config, ClusterId, ?WORKER, {<<"18.07.1">>, <<"build-1">>, OpGuiHash})),

    % Trying to update to an inexistent gui version should link service's GUI to the empty page
    ?assertMatch(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"workerVersion.gui">>), oz_test_utils:call_oz(
        Config, cluster_logic, update_version_info, [
            ?PROVIDER(ProviderId), ClusterId, ?WORKER, {<<"18.07.2">>, <<"build-2">>, <<"bad-gui-hash">>}
        ]
    )),

    ?assert(static_directory_exists(Config, [<<"./opw/">>, <<"empty">>])),
    ?assert(link_exists(Config, [<<"./opw/">>, ClusterId], <<"empty">>)),
    ?assert(file_is_served(Config, ?EMPTY_INDEX_CONTENT(Config, ?OP_WORKER_GUI), [<<"/opw/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(Config, ?EMPTY_INDEX_CONTENT(Config, ?OP_WORKER_GUI), [<<"/opw/">>, ClusterId, <<"/index.html">>])),
    % Failed version update still sets the release/build version, but sets GUI hash to empty
    ?assert(version_info_is_set(Config, ClusterId, ?WORKER, {<<"18.07.2">>, <<"build-2">>, ?EMPTY_GUI_HASH})).


empty_gui_is_linked_after_failed_op_panel_version_update(Config) ->
    {ok, {ProviderId, _ProviderToken}} = oz_test_utils:create_provider(Config),
    ClusterId = ProviderId,

    % First link correct GUI to onepanel
    {OppGuiHash, _OpIndexContent} = oz_test_utils:deploy_dummy_gui(Config, ?ONEPANEL_GUI),
    ?assertMatch(ok, oz_test_utils:call_oz(
        Config, cluster_logic, update_version_info, [
            ?PROVIDER(ProviderId), ClusterId, ?ONEPANEL, {<<"18.07.1">>, <<"build-1">>, OppGuiHash}
        ]
    )),
    ?assert(version_info_is_set(Config, ClusterId, ?ONEPANEL, {<<"18.07.1">>, <<"build-1">>, OppGuiHash})),

    % Trying to update to an inexistent gui version should link service's GUI to the empty page
    ?assertMatch(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"onepanelVersion.gui">>), oz_test_utils:call_oz(
        Config, cluster_logic, update_version_info, [
            ?PROVIDER(ProviderId), ClusterId, ?ONEPANEL, {<<"18.07.2">>, <<"build-2">>, <<"bad-gui-hash">>}
        ]
    )),

    ?assert(static_directory_exists(Config, [<<"./onp/">>, <<"empty">>])),
    ?assert(link_exists(Config, [<<"./onp/">>, ClusterId], <<"empty">>)),
    ?assert(file_is_served(Config, ?EMPTY_INDEX_CONTENT(Config, ?ONEPANEL_GUI), [<<"/onp/">>, ClusterId, <<"/i">>])),
    ?assert(file_is_served(Config, ?EMPTY_INDEX_CONTENT(Config, ?ONEPANEL_GUI), [<<"/onp/">>, ClusterId, <<"/index.html">>])),
    % Failed version update still sets the release/build version, but sets GUI hash to empty
    ?assert(version_info_is_set(Config, ClusterId, ?ONEPANEL, {<<"18.07.2">>, <<"build-2">>, ?EMPTY_GUI_HASH})).


custom_static_files_are_served(Config) ->
    Data = <<"123123123">>,
    Filename = <<"file.txt">>,
    CustomStaticRoot = oz_test_utils:get_env(Config, gui_custom_static_root),
    check_on_all_nodes(Config, fun(Node) ->
        rpc:call(Node, filelib, ensure_dir, [CustomStaticRoot]),
        rpc:call(Node, file, make_dir, [CustomStaticRoot]),
        rpc:call(Node, file, write_file, [filename:join(CustomStaticRoot, Filename), Data]),
        true
    end),
    ?assert(file_is_served(true, Config, Data, <<"text/plain">>, [<<"/ozw/onezone/custom/">>, Filename])),
    ok.


custom_static_files_are_served_from_legacy_location(Config) ->
    Data = <<"abcabcabc">>,
    Filename = <<"data.txt">>,
    CustomStaticRoot = oz_test_utils:get_env(Config, gui_custom_static_root),
    LegacyCustomStaticRoot = oz_test_utils:get_env(Config, legacy_gui_custom_static_root),
    check_on_all_nodes(Config, fun(Node) ->
        % Fallback to legacy is performed if the correct path is empty
        rpc:call(Node, file_utils, recursive_del, [CustomStaticRoot]),
        rpc:call(Node, filelib, ensure_dir, [LegacyCustomStaticRoot]),
        rpc:call(Node, file, make_dir, [LegacyCustomStaticRoot]),
        rpc:call(Node, file, write_file, [filename:join(LegacyCustomStaticRoot, Filename), Data]),
        rpc:call(Node, https_listener, stop, []),
        rpc:call(Node, https_listener, start, []),
        true
    end),
    ?assert(file_is_served(true, Config, Data, <<"text/plain">>, [<<"/ozw/onezone/custom/">>, Filename])),
    ok.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    ozt:init_per_suite([{?LOAD_MODULES, [oz_test_utils]} | Config]).


end_per_suite(_Config) ->
    application:stop(hackney),
    ssl:stop().


init_per_testcase(unused_packages_are_cleaned, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, file_utils, [passthrough]),
    ok = test_utils:mock_expect(Nodes, file_utils, seconds_since_modification, fun(_) ->
        {ok, oz_worker:get_env(mocked_seconds, 0)}
    end),
    init_per_testcase(default, Config);
init_per_testcase(_, Config) ->
    oz_test_utils:set_env(Config, gui_package_verification, false),
    oz_test_utils:set_app_env(Config, gui, max_gui_package_size_mb, 50),
    oz_test_utils:mock_harvesting_backends(Config, ?HARVESTER_MOCK_BACKEND),
    Config.


end_per_testcase(unused_packages_are_cleaned, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, file_utils),
    end_per_testcase(default, Config);
end_per_testcase(_, Config) ->
    oz_test_utils:unmock_harvesting_backends(Config, ?HARVESTER_MOCK_BACKEND),
    ok.


% Used in 'unused_packages_are_cleaned' testcase
set_mocked_seconds_since_modification(Config, Seconds) ->
    oz_test_utils:set_env(Config, mocked_seconds, Seconds).


%%%===================================================================
%%% Helper functions
%%%===================================================================

perform_upload(Config, Prefix, ClusterId, GuiPackagePath, Headers) ->
    Result = http_client:post(
        oz_test_utils:oz_url(Config, [str_utils:format_bin("/~ts/~ts/gui-upload", [Prefix, ClusterId])]),
        Headers,
        {multipart, [{file, str_utils:to_binary(GuiPackagePath)}]},
        [?SSL_OPTS(Config)]
    ),
    case Result of
        {ok, 200, _, <<"">>} ->
            ok;
        {ok, Code, _, Body} when Code >= 400 andalso Code < 500 ->
            Error = errors:from_json(json_utils:decode(Body)),
            % @TODO VFS-6977 providers and panels up to 20.02.6 expect the error
            % object in the top level of the JSON, while for versions 20.02.7
            % they expect it to be nested in the "error" field. For now, send
            % both versions at once and switch to newer way when possible
            % (backward compatibility can be dropped).
            ?assertEqual(Error, errors:from_json(maps:get(<<"error">>, json_utils:decode(Body)))),
            Error
    end.


static_directory_exists(Config, PathTokens) ->
    static_directory_exists(true, Config, PathTokens).

static_directory_exists(ExpState, Config, PathTokens) ->
    static_directory_exists(ExpState, 1, Config, PathTokens).

static_directory_exists(ExpState, Retries, Config, PathTokens) ->
    Path = to_path_in_onezone_gui_static(Config, PathTokens),
    check_on_all_nodes(Config, fun(Node) ->
        case rpc:call(Node, filelib, is_dir, [Path]) of
            ExpState ->
                true;
            Got ->
                case Retries of
                    1 ->
                        ct:pal("static_directory_exists(~tp, Config, ~tp) failed on node ~tp~ngot: ~tp", [
                            ExpState, Path, Node, Got
                        ]),
                        false;
                    _ ->
                        timer:sleep(timer:seconds(1)),
                        static_directory_exists(ExpState, Retries - 1, Config, PathTokens)
                end
        end
    end).


link_exists(Config, PathTokens, LinkValue) ->
    link_exists(true, Config, PathTokens, LinkValue).

link_exists(ExpState, Config, PathTokens, LinkValue) when is_binary(LinkValue) ->
    link_exists(ExpState, Config, PathTokens, binary_to_list(LinkValue));
link_exists(ExpState, Config, PathTokens, LinkValue) when is_list(LinkValue) ->
    Path = to_path_in_onezone_gui_static(Config, PathTokens),
    check_on_all_nodes(Config, fun(Node) ->
        case {ExpState, rpc:call(Node, file, read_link, [Path])} of
            {true, {ok, LinkValue}} ->
                true;
            {false, {ok, LinkValue} = Got} ->
                ct:pal("link_exists(~tp, Config, ~tp, ~tp) failed on node ~tp~ngot: ~tp", [
                    ExpState, Path, LinkValue, Node, Got
                ]),
                false;
            {true, Got} ->
                ct:pal("link_exists(~tp, Config, ~tp, ~tp) failed on node ~tp~ngot: ~tp", [
                    ExpState, Path, LinkValue, Node, Got
                ]),
                false;
            {false, _} ->
                true
        end
    end).


file_is_served(Config, ExpectedContent, PathTokens) ->
    file_is_served(true, Config, ExpectedContent, PathTokens).

file_is_served(ExpState, Config, ExpectedContent, PathTokens) ->
    file_is_served(ExpState, Config, ExpectedContent, <<"text/html">>, PathTokens).

file_is_served(ExpState, Config, ExpectedContent, ExpectedContentType, PathTokens) when is_list(PathTokens) ->
    file_is_served(ExpState, Config, ExpectedContent, ExpectedContentType, str_utils:join_binary(PathTokens));
file_is_served(ExpState, Config, ExpectedContent, ExpectedContentType, Path) ->
    Opts = [
        {follow_redirect, true},
        ?SSL_OPTS(Config)
    ],
    check_on_all_nodes(Config, fun(Node) ->
        Ip = test_utils:get_docker_ip(Node),
        Url = str_utils:format("https://~ts~ts", [Ip, Path]),
        {ok, Code, Headers, Body} = http_client:get(Url, #{}, <<>>, Opts),
        Result = Code =:= 200 andalso
            maps:get(?HDR_CONTENT_TYPE, Headers) =:= ExpectedContentType andalso
            Body =:= ExpectedContent,
        case Result of
            ExpState ->
                true;
            _ ->
                ct:pal("file_is_served(~tp, Config, <~tp bytes>, ~tp) failed on node ~tp~ngot: ~tp", [
                    ExpState, byte_size(ExpectedContent), Path, Node, {Code, Headers, Body}
                ]),
                false
        end
    end).


version_info_is_set(Config, ClusterId, ServiceType, {Release, Build, GuiHash}) ->
    case get_version_info(Config, ClusterId, ServiceType) of
        {Release, Build, GuiHash} ->
            true;
        Other ->
            ct:pal("Version info for service ~ts:~tp is different than expected:~nGot: ~tp~nExpected: ~tp", [
                ClusterId, ServiceType, Other, {Release, Build, GuiHash}
            ]),
            false
    end.


check_on_all_nodes(Config, Fun) ->
    Nodes = ?config(oz_worker_nodes, Config),
    lists:all(Fun, Nodes).


to_path_in_onezone_gui_static(Config, PathTokens) when is_list(PathTokens) ->
    to_path_in_onezone_gui_static(Config, str_utils:join_binary(PathTokens));
to_path_in_onezone_gui_static(Config, Path) ->
    GuiStaticRoot = oz_test_utils:get_env(Config, gui_static_root),
    AbsGuiStaticRoot = oz_test_utils:call_oz(Config, filename, absname, [GuiStaticRoot]),
    filename:join(AbsGuiStaticRoot, Path).


read_content(Config, GuiPathTokens) ->
    IndexPagePath = to_path_in_onezone_gui_static(Config, GuiPathTokens),
    {ok, Content} = oz_test_utils:call_oz(Config, file, read_file, [IndexPagePath]),
    Content.


get_version_info(Config, ClusterId, ServiceType) ->
    {ok, #od_cluster{
        worker_version = WorkerVersion,
        onepanel_version = OnepanelVersion
    }} = oz_test_utils:get_cluster(Config, ClusterId),
    case ServiceType of
        ?WORKER -> WorkerVersion;
        ?ONEPANEL -> OnepanelVersion
    end.