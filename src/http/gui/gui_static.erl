%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for storing and distribution of static GUI files.
%%%
%%% GUI path under which files are stored consists of GUI prefix and GUI id.
%%% GUI prefix is shortname of service or harvester:
%%%     * ozw
%%%     * ozp
%%%     * opw
%%%     * opp
%%%     * hrv
%%% GUI id is equal to id of corresponding service or harvester.
%%%
%%% In order to reuse the existing GUI packages, each GUI path is linked to the
%%% proper GUI version, denoted by package hash (SHA256), using symbolic links e.g.
%%%
%%%     /opw/9ba150771ad7b01366d17d89f19b56af  ->
%%%         /opw/4fc98577cee89c3dfb7817b11c0027ec3084f8ba455a162c9038ed568b9d3b7d
%%%
%%%     /opw/84b7331f7c230541d4177c23dd901fd1  ->
%%%         /opw/4fc98577cee89c3dfb7817b11c0027ec3084f8ba455a162c9038ed568b9d3b7d
%%%
%%% Special ./i file in every GUI path is an alias (symbolic link) for index.html:
%%%
%%%     /ozw/74afc09f584276186894b82caf466886/i  ->
%%%         /ozw/74afc09f584276186894b82caf466886/index.html
%%% @end
%%%--------------------------------------------------------------------
-module(gui_static).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include("http/gui_paths.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/global_definitions.hrl").

-type gui_prefix() :: binary().
-type gui_id() :: od_cluster:id() | od_harvester:id().

%% API
-export([deploy_package/2, deploy_package/4]).
-export([link_gui/3, link_gui/4]).
-export([unlink_gui/2, unlink_gui/3]).
-export([gui_exists/2]).
-export([routes/0]).
-export([oz_worker_gui_path/1]).
-export([mimetype/1]).

-define(GUI_STATIC_ROOT, oz_worker:get_env(gui_static_root)).
-define(CUSTOM_STATIC_ROOT, oz_worker:get_env(gui_custom_static_root)).
-define(LEGACY_CUSTOM_STATIC_ROOT, oz_worker:get_env(legacy_gui_custom_static_root)).

-define(CLUSTER_NODES, begin {ok, __Nodes} = node_manager:get_cluster_nodes(), __Nodes end).

-define(CRITICAL_SECTION(GuiHash, Fun), critical_section:run({gui_static, GuiHash}, Fun)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Reads given GUI package, and upon success, deploys the GUI package under given 
%% GUI prefix on all cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec deploy_package(gui_prefix(), file:name_all()) -> 
    ok | {error, term()}.
deploy_package(GuiPrefix, PackagePath) ->
    case gui:read_package(PackagePath) of
        {ok, _, PackageBin} ->
            {ok, GuiHash} = gui:package_hash({binary, PackageBin}),
            deploy_package(GuiPrefix, PackageBin, GuiHash);
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deploys a GUI package under given  GUI prefix on all cluster nodes. GuiHash must be
%% provided manually. The operation is skipped if the GUI package already exists on all 
%% cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec deploy_package(gui_prefix(), PackageBin :: binary(), gui:package_hash()) -> ok.
deploy_package(GuiPrefix, PackageBin, GuiHash) ->
    ?CRITICAL_SECTION(GuiHash, fun() ->
        case gui_exists_unsafe(GuiPrefix, GuiHash) of
            true -> ok;
            false -> deploy_package(on_cluster, GuiPrefix, PackageBin, GuiHash)
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Deploys a GUI package under given  GUI prefix.
%%
%% NOTE: This function must be run in a critical section to avoid race conditions.
%%
%% Has two modes:
%%  on_cluster - performs the operation on all cluster nodes
%%  on_node - performs the operation only on the local node
%% @end
%%--------------------------------------------------------------------
-spec deploy_package(on_cluster | on_node, gui_prefix(), PackageBin :: binary(),
    gui:package_hash()) -> ok.
deploy_package(on_cluster, GuiPrefix, PackageBin, GuiHash) ->
    lists:foreach(fun(Node) ->
        ok = rpc:call(Node, ?MODULE, deploy_package, [on_node, GuiPrefix, PackageBin, GuiHash])
    end, ?CLUSTER_NODES);

deploy_package(on_node, GuiPrefix, PackageBin, GuiHash) ->
    ?info("Deploying GUI package for ~s: ~s", [GuiPrefix, GuiHash]),
    TempDir = mochitemp:mkdtemp(),
    {ok, ExtractedPackagePath} = gui:extract_package({binary, PackageBin}, TempDir),
    PackageStaticRoot = static_root(GuiPrefix, GuiHash),
    ok = file_utils:recursive_del(PackageStaticRoot),
    ok = file_utils:move(ExtractedPackagePath, PackageStaticRoot),
    mochitemp:rmtempdir(TempDir).


%%--------------------------------------------------------------------
%% @doc
%% @equiv link_gui(on_cluster, GuiPrefix, GuiId, GuiHash).
%% @end
%%--------------------------------------------------------------------
-spec link_gui(gui_prefix(), gui_id(), gui:package_hash()) -> ok.
link_gui(GuiPrefix, GuiId, GuiHash) ->
    link_gui(on_cluster, GuiPrefix, GuiId, GuiHash).


%%--------------------------------------------------------------------
%% @doc
%% Links a GUI path to given GUI, defined by its hash. Under the hood, creates
%% a symbolic link on the filesystem to reuse the same GUI packages for multiple
%% GUI paths.
%%
%% NOTE: This operation assumes that the GUI package exists.
%%
%% Has two modes:
%%  on_cluster - performs the operation on all cluster nodes
%%  on_node - performs the operation only on the local node
%% @end
%%--------------------------------------------------------------------
-spec link_gui(on_cluster | on_node, gui_prefix(), gui_id(),
    gui:package_hash()) -> ok.
link_gui(on_cluster, GuiPrefix, GuiId, GuiHash) ->
    lists:foreach(fun(Node) ->
        ok = rpc:call(Node, ?MODULE, link_gui, [on_node, GuiPrefix, GuiId, GuiHash])
    end, ?CLUSTER_NODES);

link_gui(on_node, GuiPrefix, GuiId, GuiHash) ->
    case GuiHash of
        ?EMPTY_GUI_HASH ->
            ?debug("Linking empty GUI for ~s", [gui_path(GuiPrefix, GuiId)]);
        _ ->
            ?info("Linking gui for ~s -> ~s", [gui_path(GuiPrefix, GuiId), GuiHash])
    end,
    StaticRoot = static_root(GuiPrefix, GuiId),
    link_exists(StaticRoot) andalso (ok = file:delete(StaticRoot)),
    file:make_symlink(GuiHash, StaticRoot),
    ensure_link_to_index_html(StaticRoot).


%%--------------------------------------------------------------------
%% @doc
%% @equiv unlink_gui(on_cluster, GuiPrefix, GuiId).
%% @end
%%--------------------------------------------------------------------
-spec unlink_gui(gui_prefix(), gui_id()) -> ok.
unlink_gui(GuiPrefix, GuiId) ->
    unlink_gui(on_cluster, GuiPrefix, GuiId).


%%--------------------------------------------------------------------
%% @doc
%% Unlinks a  GUI prefix from its GUI. Under the hood, removes the symbolic link
%% from the filesystem.
%%
%% Has two modes:
%%  on_cluster - performs the operation on all cluster nodes
%%  on_node - performs the operation only on the local node
%% @end
%%--------------------------------------------------------------------
-spec unlink_gui(on_cluster | on_node, gui_prefix(), gui_id()) -> ok.
unlink_gui(on_cluster, GuiPrefix, GuiId) ->
    lists:foreach(fun(Node) ->
        ok = rpc:call(Node, ?MODULE, unlink_gui, [on_node, GuiPrefix, GuiId])
    end, ?CLUSTER_NODES);

unlink_gui(on_node, GuiPrefix, GuiId) ->
    ?info("Unlinking gui for ~s", [gui_path(GuiPrefix, GuiId)]),
    StaticRoot = static_root(GuiPrefix, GuiId),
    link_exists(StaticRoot) andalso (ok = file:delete(StaticRoot)).


%%--------------------------------------------------------------------
%% @doc
%% Checks on all cluster nodes if given GUI hash exists.
%% @end
%%--------------------------------------------------------------------
-spec gui_exists(gui_prefix(), gui:package_hash()) -> boolean().
gui_exists(GuiPrefix, GuiHash) ->
    ?CRITICAL_SECTION(GuiHash, fun() ->
        gui_exists_unsafe(GuiPrefix, GuiHash)
    end).


%%--------------------------------------------------------------------
%% @doc
%% Returns cowboy routes related to the static GUI server.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{Path :: string() | binary(), module(), State :: term()}].
routes() ->
    % Try to guess if new or legacy custom static root is being used
    CustomRootDir = case {filelib:is_dir(?CUSTOM_STATIC_ROOT), filelib:is_dir(?LEGACY_CUSTOM_STATIC_ROOT)} of
        {false, true} ->
            ?alert(
                "Detected custom static files in deprecated location, please "
                "use the new one:~ndeprecated path: ~s~ncorrect path:    ~s", [
                    ?LEGACY_CUSTOM_STATIC_ROOT, ?CUSTOM_STATIC_ROOT
                ]
            ),
            ?LEGACY_CUSTOM_STATIC_ROOT;
        _ ->
            ?CUSTOM_STATIC_ROOT
    end,


    CustomPath = binary_to_list(oz_worker_gui_path(<<?CUSTOM_STATIC_GUI_PATH>>)),
    ?info("Serving custom static files:~nURN:       ~s~nhost path: ~s/[...]", [
        CustomPath,
        CustomRootDir
    ]),

    [
        {CustomPath, cowboy_static, {dir, CustomRootDir, [{mimetypes, ?MODULE, mimetype}]}},
        {"/[...]", cowboy_static, {dir, ?GUI_STATIC_ROOT, [{mimetypes, ?MODULE, mimetype}]}}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Returns the full absolute path to given resource within oz worker GUI, e.g.:
%% oz_worker_gui_path(<<"/custom/image.png">>) -> <<"/ozw/onezone/custom/image.png">>
%% @end
%%--------------------------------------------------------------------
-spec oz_worker_gui_path(binary()) -> binary().
oz_worker_gui_path(<<"/", _/binary>> = AbsPath) ->
    <<(gui_path(onedata:service_shortname(?OZ_WORKER), ?ONEZONE_CLUSTER_ID))/binary, AbsPath/binary>>;
oz_worker_gui_path(RelPath) ->
    oz_worker_gui_path(<<"/", RelPath/binary>>).


%%--------------------------------------------------------------------
%% @doc
%% Cowboy callback for resolving file mimetype. Compared to default cowboy
%% behaviour, recognizes json files and returns text/html mimetype for the
%% ./i file (alias for ./index.html).
%% Used in cowboy_static handler opts (routes/0).
%% @end
%%--------------------------------------------------------------------
-spec mimetype(binary()) -> {binary(), binary(), []}.
mimetype(Path) ->
    case filename:extension(Path) of
        <<>> ->
            % Special ./i file in every location is an alias
            % (symbolic link) for index.html
            case filename:basename(Path) of
                <<"i">> -> {<<"text">>, <<"html">>, []};
                _ -> {<<"application">>, <<"octet-stream">>, []}
            end;
        <<$., Ext/binary>> ->
            mimetype_by_ext(Ext)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks on all cluster nodes if given GUI hash exists.
%% NOTE: This function must be run in a critical section to avoid race conditions.
%% @end
%%--------------------------------------------------------------------
-spec gui_exists_unsafe(gui_prefix(), gui:package_hash()) -> boolean().
gui_exists_unsafe(GuiPrefix, GuiHash) ->
    PackageStaticRoot = static_root(GuiPrefix, GuiHash),
    lists:all(fun(Node) ->
        rpc:call(Node, filelib, is_file, [PackageStaticRoot])
    end, ?CLUSTER_NODES).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the path (on the filesystem) to the static GUI root of given  GUI prefix
%% or gui package.
%% Examples:
%%      /etc/oz_worker/gui_static/ozw/74afc09f584276186894b82caf466886
%%      /etc/oz_worker/gui_static/opp/4fc98577cee89c3dfb7817b11c0027ec3084f8ba455a162c9038ed568b9d3b7d
%% Identifier can be a GuiId or a GuiHash.
%% @end
%%--------------------------------------------------------------------
-spec static_root(gui_prefix(), gui_id() | gui:package_hash()) -> binary().
static_root(GuiPrefix, Identifier) ->
    GuiStaticRoot = list_to_binary(?GUI_STATIC_ROOT),
    <<GuiStaticRoot/binary, (gui_path(GuiPrefix, Identifier))/binary>>.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the path (URN) to the web GUI root of given  GUI prefix or gui package.
%% Examples:
%%      /ozw/74afc09f584276186894b82caf466886
%%      /opp/4fc98577cee89c3dfb7817b11c0027ec3084f8ba455a162c9038ed568b9d3b7d
%% @end
%%--------------------------------------------------------------------
-spec gui_path(gui_prefix(), gui_id() | gui:package_hash()) -> binary().
gui_path(GuiPrefix, Identifier) ->
    <<"/", GuiPrefix/binary, "/", Identifier/binary>>.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% If not existent, creates an alias (symbolic link) for index.html, like this:
%%     /etc/oz_worker/gui_static/ozw/74afc09f584276186894b82caf466886/i  ->
%%         /etc/oz_worker/gui_static/ozw/74afc09f584276186894b82caf466886/index.html
%% @end
%%--------------------------------------------------------------------
-spec ensure_link_to_index_html(StaticRoot :: binary()) -> ok.
ensure_link_to_index_html(StaticRoot) ->
    IndexLink = filename:join([StaticRoot, "i"]),
    link_exists(IndexLink) orelse file:make_symlink("index.html", IndexLink),
    ok.


%% @private
-spec link_exists(file:name_all()) -> boolean().
link_exists(Path) ->
    case file:read_link(Path) of
        {ok, _} -> true;
        _ -> false
    end.


%% @private
-spec mimetype_by_ext(binary()) -> {binary(), binary(), []}.
mimetype_by_ext(<<"css">>) -> {<<"text">>, <<"css">>, []};
mimetype_by_ext(<<"gif">>) -> {<<"image">>, <<"gif">>, []};
mimetype_by_ext(<<"html">>) -> {<<"text">>, <<"html">>, []};
mimetype_by_ext(<<"htm">>) -> {<<"text">>, <<"html">>, []};
mimetype_by_ext(<<"ico">>) -> {<<"image">>, <<"x-icon">>, []};
mimetype_by_ext(<<"jpeg">>) -> {<<"image">>, <<"jpeg">>, []};
mimetype_by_ext(<<"jpg">>) -> {<<"image">>, <<"jpeg">>, []};
mimetype_by_ext(<<"js">>) -> {<<"application">>, <<"javascript">>, []};
mimetype_by_ext(<<"json">>) -> {<<"application">>, <<"json">>, []};
mimetype_by_ext(<<"mp3">>) -> {<<"audio">>, <<"mpeg">>, []};
mimetype_by_ext(<<"mp4">>) -> {<<"video">>, <<"mp4">>, []};
mimetype_by_ext(<<"ogg">>) -> {<<"audio">>, <<"ogg">>, []};
mimetype_by_ext(<<"ogv">>) -> {<<"video">>, <<"ogg">>, []};
mimetype_by_ext(<<"png">>) -> {<<"image">>, <<"png">>, []};
mimetype_by_ext(<<"svg">>) -> {<<"image">>, <<"svg+xml">>, []};
mimetype_by_ext(<<"txt">>) -> {<<"text">>, <<"plain">>, []};
mimetype_by_ext(<<"wav">>) -> {<<"audio">>, <<"x-wav">>, []};
mimetype_by_ext(<<"webm">>) -> {<<"video">>, <<"webm">>, []};
mimetype_by_ext(_) -> {<<"application">>, <<"octet-stream">>, []}.
