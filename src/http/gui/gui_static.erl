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
%%% In order to reuse the existing GUI packages, each location is linked to the
%%% proper GUI version, denoted by package hash (SHA256), using symbolic links e.g.
%%%
%%%     /opw/9ba150771ad7b01366d17d89f19b56af  ->
%%%         /opw/4fc98577cee89c3dfb7817b11c0027ec3084f8ba455a162c9038ed568b9d3b7d
%%%
%%%     /opw/84b7331f7c230541d4177c23dd901fd1  ->
%%%         /opw/4fc98577cee89c3dfb7817b11c0027ec3084f8ba455a162c9038ed568b9d3b7d
%%%
%%% Special ./i file in every location is an alias (symbolic link) for index.html:
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

-type location_key() :: binary().
-type package_id() :: od_cluster:id() | od_harvester:id() | gui:package_hash().

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
%% location key on all cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec deploy_package(location_key(), file:name_all()) -> 
    {ok, gui:package_hash()} | {error, term()}.
deploy_package(LocationKey, PackagePath) ->
    case gui:read_package(PackagePath) of
        {ok, _, PackageBin} ->
            {ok, GuiHash} = gui:package_hash({binary, PackageBin}),
            deploy_package(LocationKey, PackageBin, GuiHash);
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deploys a GUI package under given location key on all cluster nodes. GuiHash must be
%% provided manually. The operation is skipped if the GUI package already exists on all 
%% cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec deploy_package(location_key(), PackageBin :: binary(), gui:package_hash()) -> ok.
deploy_package(LocationKey, PackageBin, GuiHash) ->
    ?CRITICAL_SECTION(GuiHash, fun() ->
        case gui_exists_unsafe(LocationKey, GuiHash) of
            true -> ok;
            false -> deploy_package(on_cluster, LocationKey, PackageBin, GuiHash)
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Deploys a GUI package under given location key.
%%
%% NOTE: This function must be run in a critical section to avoid race conditions.
%%
%% Has two modes:
%%  on_cluster - performs the operation on all cluster nodes
%%  on_node - performs the operation only on the local node
%% @end
%%--------------------------------------------------------------------
-spec deploy_package(on_cluster | on_node, location_key(), PackageBin :: binary(),
    gui:package_hash()) -> ok.
deploy_package(on_cluster, LocationKey, PackageBin, GuiHash) ->
    lists:foreach(fun(Node) ->
        ok = rpc:call(Node, ?MODULE, deploy_package, [on_node, LocationKey, PackageBin, GuiHash])
    end, ?CLUSTER_NODES);

deploy_package(on_node, LocationKey, PackageBin, GuiHash) ->
    ?info("Deploying GUI package for ~s: ~s", [LocationKey, GuiHash]),
    TempDir = mochitemp:mkdtemp(),
    {ok, ExtractedPackageLocationKey} = gui:extract_package({binary, PackageBin}, TempDir),
    PackageStaticRoot = static_root(LocationKey, GuiHash),
    ok = file_utils:recursive_del(PackageStaticRoot),
    ok = file_utils:move(ExtractedPackageLocationKey, PackageStaticRoot),
    mochitemp:rmtempdir(TempDir).


%%--------------------------------------------------------------------
%% @doc
%% @equiv link_gui(on_cluster, LocationKey, ClusterId, GuiHash).
%% @end
%%--------------------------------------------------------------------
-spec link_gui(location_key(), od_cluster:id(), gui:package_hash()) -> ok.
link_gui(LocationKey, ClusterId, GuiHash) ->
    link_gui(on_cluster, LocationKey, ClusterId, GuiHash).


%%--------------------------------------------------------------------
%% @doc
%% Links a location to given GUI, defined by its hash. Under the hood, creates
%% a symbolic link on the filesystem to reuse the same GUI packages for multiple
%% locations.
%%
%% NOTE: This operation assumes that the GUI package exists.
%%
%% Has two modes:
%%  on_cluster - performs the operation on all cluster nodes
%%  on_node - performs the operation only on the local node
%% @end
%%--------------------------------------------------------------------
-spec link_gui(on_cluster | on_node, location_key(), od_cluster:id(),
    gui:package_hash()) -> ok.
link_gui(on_cluster, LocationKey, ClusterId, GuiHash) ->
    lists:foreach(fun(Node) ->
        ok = rpc:call(Node, ?MODULE, link_gui, [on_node, LocationKey, ClusterId, GuiHash])
    end, ?CLUSTER_NODES);

link_gui(on_node, LocationKey, ClusterId, GuiHash) ->
    case GuiHash of
        ?EMPTY_GUI_HASH ->
            ?debug("Linking empty GUI for ~s", [gui_path(LocationKey, ClusterId)]);
        _ ->
            ?info("Linking gui for ~s -> ~s", [gui_path(LocationKey, ClusterId), GuiHash])
    end,
    StaticRoot = static_root(LocationKey, ClusterId),
    link_exists(StaticRoot) andalso (ok = file:delete(StaticRoot)),
    file:make_symlink(GuiHash, StaticRoot),
    ensure_link_to_index_html(StaticRoot).


%%--------------------------------------------------------------------
%% @doc
%% @equiv unlink_gui(on_cluster, LocationKey, ClusterId).
%% @end
%%--------------------------------------------------------------------
-spec unlink_gui(location_key(), od_cluster:id()) -> ok.
unlink_gui(LocationKey, ClusterId) ->
    unlink_gui(on_cluster, LocationKey, ClusterId).


%%--------------------------------------------------------------------
%% @doc
%% Unlinks a location key from its GUI. Under the hood, removes the symbolic link
%% from the filesystem.
%%
%% Has two modes:
%%  on_cluster - performs the operation on all cluster nodes
%%  on_node - performs the operation only on the local node
%% @end
%%--------------------------------------------------------------------
-spec unlink_gui(on_cluster | on_node, location_key(), od_cluster:id()) -> ok.
unlink_gui(on_cluster, LocationKey, ClusterId) ->
    lists:foreach(fun(Node) ->
        ok = rpc:call(Node, ?MODULE, unlink_gui, [on_node, LocationKey, ClusterId])
    end, ?CLUSTER_NODES);

unlink_gui(on_node, LocationKey, ClusterId) ->
    ?info("Unlinking gui for ~s", [gui_path(LocationKey, ClusterId)]),
    StaticRoot = static_root(LocationKey, ClusterId),
    link_exists(StaticRoot) andalso (ok = file:delete(StaticRoot)).


%%--------------------------------------------------------------------
%% @doc
%% Checks on all cluster nodes if given GUI hash exists.
%% @end
%%--------------------------------------------------------------------
-spec gui_exists(location_key(), gui:package_hash()) -> boolean().
gui_exists(LocationKey, GuiHash) ->
    ?CRITICAL_SECTION(GuiHash, fun() ->
        gui_exists_unsafe(LocationKey, GuiHash)
    end).


%%--------------------------------------------------------------------
%% @doc
%% Returns cowboy routes related to the static GUI server.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{Path :: string() | binary(), module(), State :: term()}].
routes() ->
    CustomPath = binary_to_list(oz_worker_gui_path(<<?CUSTOM_STATIC_GUI_PATH>>)),
    [
        {CustomPath, cowboy_static, {dir, ?CUSTOM_STATIC_ROOT}},
        {CustomPath, cowboy_static, {dir, ?LEGACY_CUSTOM_STATIC_ROOT}},
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
-spec gui_exists_unsafe(location_key(), gui:package_hash()) -> boolean().
gui_exists_unsafe(LocationKey, GuiHash) ->
    PackageStaticRoot = static_root(LocationKey, GuiHash),
    lists:all(fun(Node) ->
        rpc:call(Node, filelib, is_file, [PackageStaticRoot])
    end, ?CLUSTER_NODES).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the path (on the filesystem) to the static GUI root of given location key
%% or gui package.
%% Examples:
%%      /etc/oz_worker/gui_static/ozw/74afc09f584276186894b82caf466886
%%      /etc/oz_worker/gui_static/opp/4fc98577cee89c3dfb7817b11c0027ec3084f8ba455a162c9038ed568b9d3b7d
%% Identifier can be a ClusterId or a GuiHash.
%% @end
%%--------------------------------------------------------------------
-spec static_root(location_key(), package_id()) -> binary().
static_root(LocationKey, Identifier) ->
    GuiStaticRoot = list_to_binary(?GUI_STATIC_ROOT),
    <<GuiStaticRoot/binary, (gui_path(LocationKey, Identifier))/binary>>.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the path (URN) to the web GUI root of given location key or gui package.
%% Examples:
%%      /ozw/74afc09f584276186894b82caf466886
%%      /opp/4fc98577cee89c3dfb7817b11c0027ec3084f8ba455a162c9038ed568b9d3b7d
%% @end
%%--------------------------------------------------------------------
-spec gui_path(location_key(), package_id()) -> binary().
gui_path(LocationKey, Identifier) ->
    <<"/", LocationKey/binary, "/", Identifier/binary>>.


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
-spec link_exists(Path :: file:name_all()) -> boolean().
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
mimetype_by_ext(<<"wav">>) -> {<<"audio">>, <<"x-wav">>, []};
mimetype_by_ext(<<"webm">>) -> {<<"video">>, <<"webm">>, []};
mimetype_by_ext(_) -> {<<"application">>, <<"octet-stream">>, []}.
