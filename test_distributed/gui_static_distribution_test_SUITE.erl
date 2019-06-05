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
-include_lib("ctool/include/api_errors.hrl").
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
    empty_gui_is_linked_after_provider_registration/1,
    op_worker_and_panel_gui_is_linked_upon_version_info_update/1,
    gui_is_unlinked_after_provider_deletion/1,
    gui_upload_requires_provider_auth/1,
    gui_upload_is_not_possible_for_onezone_services/1,
    gui_upload_for_inexistent_service_returns_not_found/1,
    gui_upload_for_inexistent_cluster_returns_not_found/1,
    gui_upload_with_invalid_package_returns_proper_error/1,
    gui_upload_with_too_large_package_returns_proper_error/1,
    gui_upload_page_deploys_op_worker_gui_on_all_nodes/1,
    gui_upload_page_deploys_op_panel_gui_on_all_nodes/1,
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
        empty_gui_is_linked_after_provider_registration,
        op_worker_and_panel_gui_is_linked_upon_version_info_update,
        gui_is_unlinked_after_provider_deletion,
        gui_upload_requires_provider_auth,
        gui_upload_is_not_possible_for_onezone_services,
        gui_upload_for_inexistent_service_returns_not_found,
        gui_upload_for_inexistent_cluster_returns_not_found,
        gui_upload_with_invalid_package_returns_proper_error,
        gui_upload_with_too_large_package_returns_proper_error,
        gui_upload_page_deploys_op_worker_gui_on_all_nodes,
        gui_upload_page_deploys_op_panel_gui_on_all_nodes,
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
    GuiPackagePath = oz_test_utils:get_env(Config, gui_package_path),
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
    {ok, {ProviderId, ProviderMacaroon}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ClusterId = ProviderId,
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),

    % No auth
    ?assertMatch({ok, 401, _, _}, perform_upload(
        Config, <<"opw">>, ClusterId, GuiPackage, #{}
    )),
    ?assertMatch({ok, 401, _, _}, perform_upload(
        Config, <<"onp">>, ClusterId, GuiPackage, #{}
    )),

    % With auth
    ?assertMatch({ok, 200, _, _}, perform_upload(
        Config, <<"opw">>, ClusterId, GuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )),
    ?assertMatch({ok, 200, _, _}, perform_upload(
        Config, <<"onp">>, ClusterId, GuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )),

    % Upload is only possible for own cluster
    {ok, {_AnotherProvider, AnotherProviderMacaroon}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ?assertMatch({ok, 403, _, _}, perform_upload(
        Config, <<"opw">>, ClusterId, GuiPackage, #{<<"macaroon">> => AnotherProviderMacaroon}
    )),
    ?assertMatch({ok, 403, _, _}, perform_upload(
        Config, <<"onp">>, ClusterId, GuiPackage, #{<<"macaroon">> => AnotherProviderMacaroon}
    )).


gui_upload_is_not_possible_for_onezone_services(Config) ->
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),

    % With no auth
    ?assertMatch({ok, 404, _, _}, perform_upload(
        Config, <<"ozw">>, ?ONEZONE_CLUSTER_ID, GuiPackage, #{}
    )),
    ?assertMatch({ok, 404, _, _}, perform_upload(
        Config, <<"onp">>, ?ONEZONE_CLUSTER_ID, GuiPackage, #{}
    )),

    % With provider auth
    {ok, {_Provider, ProviderMacaroon}} = oz_test_utils:create_provider(Config, ?UNIQUE_STRING),
    ?assertMatch({ok, 404, _, _}, perform_upload(
        Config, <<"ozw">>, ?ONEZONE_CLUSTER_ID, GuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )),
    ?assertMatch({ok, 404, _, _}, perform_upload(
        Config, <<"onp">>, ?ONEZONE_CLUSTER_ID, GuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )).


gui_upload_for_inexistent_service_returns_not_found(Config) ->
    {ok, {ProviderId, ProviderMacaroon}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ClusterId = ProviderId,
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),

    ?assertMatch({ok, 404, _, _}, perform_upload(
        Config, <<"abc">>, ClusterId, GuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )),
    ?assertMatch({ok, 404, _, _}, perform_upload(
        Config, <<"xyz">>, ClusterId, GuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )).


gui_upload_for_inexistent_cluster_returns_not_found(Config) ->
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),

    ?assertMatch({ok, 404, _, _}, perform_upload(
        Config, <<"opw">>, <<"bad-cluster-id">>, GuiPackage, #{}
    )),
    ?assertMatch({ok, 404, _, _}, perform_upload(
        Config, <<"onp">>, <<"bad-cluster-id">>, GuiPackage, #{}
    )).


gui_upload_with_invalid_package_returns_proper_error(Config) ->
    {ok, {ProviderId, ProviderMacaroon}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ClusterId = ProviderId,
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),
    % Break the package a little bit
    {ok, Contents} = file:read_file(GuiPackage),
    ok = file:write_file(GuiPackage, <<"lalalalala-bad-bytes", Contents/binary>>),

    ExpError = json_utils:encode(gs_protocol_errors:error_to_json(1, ?ERROR_BAD_GUI_PACKAGE)),

    ?assertMatch({ok, 400, _, ExpError}, perform_upload(
        Config, <<"opw">>, ClusterId, GuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )),
    ?assertMatch({ok, 400, _, ExpError}, perform_upload(
        Config, <<"onp">>, ClusterId, GuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )).


gui_upload_with_too_large_package_returns_proper_error(Config) ->
    {ok, {ProviderId, ProviderMacaroon}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    ClusterId = ProviderId,
    {GuiPackage, _} = oz_test_utils:create_dummy_gui_package(),

    % Set the package limit to one byte
    oz_test_utils:set_app_env(Config, gui, max_gui_package_size_mb, 1 / 1048576),

    ExpError = json_utils:encode(gs_protocol_errors:error_to_json(1, ?ERROR_GUI_PACKAGE_TOO_LARGE)),

    ?assertMatch({ok, 400, _, ExpError}, perform_upload(
        Config, <<"opw">>, ClusterId, GuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )),
    ?assertMatch({ok, 400, _, ExpError}, perform_upload(
        Config, <<"onp">>, ClusterId, GuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )).


gui_upload_page_deploys_op_worker_gui_on_all_nodes(Config) ->
    {ok, {ProviderId, ProviderMacaroon}} = oz_test_utils:create_provider(Config, ?UNIQUE_STRING),
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

    ?assertMatch({ok, 200, _, _}, perform_upload(
        Config, <<"opw">>, ClusterId, OpGuiPackage, #{<<"macaroon">> => ProviderMacaroon}
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
    {ok, {ProviderId, ProviderMacaroon}} = oz_test_utils:create_provider(Config, ?UNIQUE_STRING),
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

    ?assertMatch({ok, 200, _, _}, perform_upload(
        Config, <<"onp">>, ClusterId, OppGuiPackage, #{<<"macaroon">> => ProviderMacaroon}
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


gui_package_verification_works(Config) ->
    oz_test_utils:set_env(Config, disable_gui_package_verification, false),
    oz_test_utils:set_app_env(Config, ctool, compatibility_registry_mirrors, []),

    {ok, {ProviderId, ProviderMacaroon}} = oz_test_utils:create_provider(Config, ?UNIQUE_STRING),
    ClusterId = ProviderId,

    {OpGuiPackage, _OpIndexContent} = oz_test_utils:create_dummy_gui_package(),
    {ok, OpGuiHash} = gui:package_hash(OpGuiPackage),

    ExpError = json_utils:encode(gs_protocol_errors:error_to_json(1, ?ERROR_GUI_PACKAGE_UNVERIFIED)),

    % GUI hash is not whitelisted
    ?assertMatch({ok, 400, _, ExpError}, perform_upload(
        Config, <<"opw">>, ClusterId, OpGuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )),

    % Whitelist the GUI hash:
    oz_test_utils:overwrite_compatibility_registry(Config, #{
        <<"revision">> => 1,
        <<"gui-sha256">> => #{
            <<"op-worker">> => #{
                ?DEFAULT_RELEASE_VERSION => [OpGuiHash]
            }
        }
    }),

    % Now, upload should work
    ?assertMatch({ok, 200, _, _}, perform_upload(
        Config, <<"opw">>, ClusterId, OpGuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )),

    % Make sure that only packages for op-worker with such hash are accepted
    ?assertMatch({ok, 400, _, ExpError}, perform_upload(
        Config, <<"onp">>, ClusterId, OpGuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )),

    % When verification is disabled, any package should be accepted
    oz_test_utils:set_env(Config, disable_gui_package_verification, true),
    ?assertMatch({ok, 200, _, _}, perform_upload(
        Config, <<"opw">>, ClusterId, OpGuiPackage, #{<<"macaroon">> => ProviderMacaroon}
    )),
    ?assertMatch({ok, 200, _, _}, perform_upload(
        Config, <<"onp">>, ClusterId, OpGuiPackage, #{<<"macaroon">> => ProviderMacaroon}
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

    {ok, {P1, _}} = oz_test_utils:create_provider(Config, ?UNIQUE_STRING),
    {ok, {P2, _}} = oz_test_utils:create_provider(Config, ?UNIQUE_STRING),

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
    ?assert(static_directory_exists(false, 60, Config, [<<"./hrv/">>, HashOmega])).


empty_gui_is_linked_after_failed_op_worker_version_update(Config) ->
    {ok, {ProviderId, _ProviderMacaroon}} = oz_test_utils:create_provider(Config, ?UNIQUE_STRING),
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
    {ok, {ProviderId, _ProviderMacaroon}} = oz_test_utils:create_provider(Config, ?UNIQUE_STRING),
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
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


init_per_testcase(unused_packages_are_cleaned, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, file_utils, [passthrough]),
    ok = test_utils:mock_expect(Nodes, file_utils, seconds_since_modification, fun(_) ->
        {ok, oz_worker:get_env(mocked_seconds, 0)}
    end),
    init_per_testcase(default, Config);
init_per_testcase(_, Config) ->
    oz_test_utils:set_env(Config, disable_gui_package_verification, true),
    oz_test_utils:set_app_env(Config, gui, max_gui_package_size_mb, 50),
    Config.


end_per_testcase(unused_packages_are_cleaned, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, file_utils),
    end_per_testcase(default, Config);
end_per_testcase(_, _Config) ->
    ok.


% Used in 'unused_packages_are_cleaned' testcase
set_mocked_seconds_since_modification(Config, Seconds) ->
    oz_test_utils:set_env(Config, mocked_seconds, Seconds).


%%%===================================================================
%%% Helper functions
%%%===================================================================

perform_upload(Config, Prefix, ClusterId, GuiPackagePath, Headers) ->
    http_client:post(
        oz_test_utils:oz_url(Config, [str_utils:format_bin("/~s/~s/gui-upload", [Prefix, ClusterId])]),
        Headers,
        {multipart, [{file, str_utils:to_binary(GuiPackagePath)}]},
        [?SSL_OPTS(Config)]
    ).


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
                        ct:pal("static_directory_exists(~p, Config, ~p) failed on node ~p~ngot: ~p", [
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
                ct:pal("link_exists(~p, Config, ~p, ~p) failed on node ~p~ngot: ~p", [
                    ExpState, Path, LinkValue, Node, Got
                ]),
                false;
            {true, Got} ->
                ct:pal("link_exists(~p, Config, ~p, ~p) failed on node ~p~ngot: ~p", [
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
        Url = str_utils:format("https://~s~s", [Ip, Path]),
        {ok, Code, Headers, Body} = http_client:get(Url, #{}, <<>>, Opts),
        Result = Code =:= 200 andalso
            maps:get(<<"content-type">>, Headers) =:= ExpectedContentType andalso
            Body =:= ExpectedContent,
        case Result of
            ExpState ->
                true;
            _ ->
                ct:pal("file_is_served(~p, Config, <~p bytes>, ~p) failed on node ~p~ngot: ~p", [
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
            ct:pal("Version info for service ~s:~p is different than expected:~nGot: ~p~nExpected: ~p", [
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