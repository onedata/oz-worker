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
-export([create/1, save/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).
-export([ensure_onezone_cluster/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1]).

-type id() :: binary().
-type record() :: #od_cluster{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

% The Id of corresponding service:
%   <<"onezone">> for onezone cluster
%   ProviderId for oneprovider cluster
-type service_id() :: binary() | od_provider:id().
-type version_info() :: {
    Release :: binary(),
    Build :: binary(),
    GuiHash :: binary()
}.
-export_type([service_id/0, version_info/0]).

-define(CTX, #{
    model => ?MODULE,
    fold_enabled => true,
    sync_enabled => true
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates cluster.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Saves cluster.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:save(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns cluster by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(ClusterId) ->
    datastore_model:get(?CTX, ClusterId).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether cluster given by ID exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(ClusterId) ->
    datastore_model:exists(?CTX, ClusterId).

%%--------------------------------------------------------------------
%% @doc
%% Updates cluster by ID.
%% @end
%%--------------------------------------------------------------------
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
    {ok, _} = datastore_model:update(?CTX, ?ONEZONE_CLUSTER_ID,
        fun(Cluster) -> {ok, Cluster} end,
        #document{key = ?ONEZONE_CLUSTER_ID, value = #od_cluster{
            type = ?ONEZONE,
            service_id = ?ONEZONE_SERVICE_ID,
            creator = ?ROOT
        }}
    ),
    ok.

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
    1.

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
        {service_id, string},

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
    ]}.
