%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for cluster record - representing a cluster (Oneprovider or Onezone)
%%% in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(od_cluster).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([create/1, get/1, get_name/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).
-export([ensure_onezone_cluster/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

% The Id of corresponding service:
%   <<"onezone">> for onezone cluster
%   ProviderId for oneprovider cluster
-type id() :: binary().
-type record() :: #od_cluster{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-type version_info() :: {
    Release :: onedata:release_version(),
    Build :: binary(),
    GuiHash :: onedata:gui_hash()
}.
-export_type([version_info/0]).

-define(CTX, #{
    model => ?MODULE,
    secure_fold_enabled => true,
    sync_enabled => true,
    memory_copies => all
}).

%%%===================================================================
%%% API
%%%===================================================================


-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).


-spec get(id()) -> {ok, doc()} | {error, term()}.
get(ClusterId) ->
    case datastore_model:get(?CTX, ClusterId) of
        {error, not_found} ->
            %% @TODO VFS-5207 remove when no longer needed for compatibility
            ensure_cluster_for_legacy_provider(ClusterId),
            datastore_model:get(?CTX, ClusterId);
        OtherResult ->
            OtherResult
    end.


-spec get_name(id()) -> {ok, binary()} | {error, term()}.
get_name(?ONEZONE_CLUSTER_ID) ->
    {ok, <<"Onezone">>};
get_name(ClusterId) ->
    od_provider:get_name(ClusterId).


-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(ClusterId) ->
    case datastore_model:exists(?CTX, ClusterId) of
        {ok, false} ->
            %% @TODO VFS-5207 remove when no longer needed for compatibility
            ensure_cluster_for_legacy_provider(ClusterId),
            datastore_model:exists(?CTX, ClusterId);
        OtherResult ->
            OtherResult
    end.


-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(ClusterId, Diff) ->
    datastore_model:update(?CTX, ClusterId, Diff).


%%--------------------------------------------------------------------
%% @doc
%% Deletes cluster by ID.
%% WARNING: Must not be used directly, as deleting a cluster that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a cluster, delete the corresponding provider with
%% provider_logic:delete/2.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(ClusterId) ->
    datastore_model:delete(?CTX, ClusterId).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all clusters.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).


%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the cluster with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(ClusterId :: id()) -> binary().
to_string(ClusterId) ->
    <<"cluster:", ClusterId/binary>>.


%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    cluster_logic_plugin.


%%--------------------------------------------------------------------
%% @doc
%% Creates a new onezone cluster singleton if it does not exist.
%% @end
%%--------------------------------------------------------------------
-spec ensure_onezone_cluster() -> ok.
ensure_onezone_cluster() ->
    NewClusterDoc = #document{key = ?ONEZONE_CLUSTER_ID, value = #od_cluster{
        type = ?ONEZONE,
        creator = ?SUB(nobody)
    }},
    case create(NewClusterDoc) of
        {ok, _} -> ok;
        {error, already_exists} -> ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% Creates a new cluster for given provider if the provider exists and doesn't
%% have a cluster - dedicated for legacy providers after upgrade.
%% @end
%%--------------------------------------------------------------------
-spec ensure_cluster_for_legacy_provider(id()) -> ok.
ensure_cluster_for_legacy_provider(ClusterId) ->
    ProviderId = ClusterId,
    case od_provider:exists(ProviderId) of
        {ok, false} ->
            ok;
        {ok, true} ->
            % Avoid race conditions
            critical_section:run({generate_cluster, ClusterId}, fun() ->
                case datastore_model:get(?CTX, ClusterId) of
                    {error, not_found} ->
                        cluster_logic:create_oneprovider_cluster(undefined, ProviderId);
                    _ ->
                        % Cluster might have been generated by another process in the meantime
                        ok
                end
            end)
    end.

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_version() -> datastore_model:record_version().
get_record_version() ->
    3.

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {type, atom},

        {worker_version, {string, string, string}},
        {onepanel_version, {string, string, string}},
        {onepanel_proxy, boolean},

        {creation_time, integer},
        {creator, {record, [
            {type, atom},
            {id, string}
        ]}},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},

        {bottom_up_dirty, boolean}
    ]};
get_record_struct(2) ->
    {record, [
        {type, atom},

        {worker_version, {string, string, string}},
        {onepanel_version, {string, string, string}},
        {onepanel_proxy, boolean},

        {creation_time, integer},
        {creator, {record, [ % nested record changed from #client{} to #subject{}
            {type, atom},
            {id, string}
        ]}},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},

        {bottom_up_dirty, boolean}
    ]};
get_record_struct(3) ->
    % creator field - nested #subject{} record and encoding changed
    {record, [
        {type, atom},

        {worker_version, {string, string, string}},
        {onepanel_version, {string, string, string}},
        {onepanel_proxy, boolean},

        {creation_time, integer},
        % nested #subject{} record was extended and is now encoded as string
        % rather than record tuple
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},

        {bottom_up_dirty, boolean}
    ]}.


%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, Cluster) ->
    {
        od_cluster,
        Type,

        WorkerVersion,
        OnepanelVersion,
        OnepanelProxy,

        CreationTime,
        Creator,

        Users,
        Groups,

        EffUsers,
        EffGroups,

        BottomUpDirty
    } = Cluster,

    {2, {od_cluster,
        Type,

        WorkerVersion,
        OnepanelVersion,
        OnepanelProxy,

        CreationTime,
        upgrade_common:client_to_subject(Creator),

        Users,
        Groups,

        EffUsers,
        EffGroups,

        BottomUpDirty
    }};
upgrade_record(2, Cluster) ->
    {od_cluster,
        Type,

        WorkerVersion,
        OnepanelVersion,
        OnepanelProxy,

        CreationTime,
        Creator,

        Users,
        Groups,

        EffUsers,
        EffGroups,

        BottomUpDirty
    } = Cluster,
    {3, #od_cluster{
        type = Type,

        worker_version = WorkerVersion,
        onepanel_version = OnepanelVersion,
        onepanel_proxy = OnepanelProxy,

        creation_time = CreationTime,
        creator = upgrade_common:upgrade_subject_record(Creator),

        users = Users,
        groups = Groups,

        eff_users = EffUsers,
        eff_groups = EffGroups,

        bottom_up_dirty = BottomUpDirty
    }}.