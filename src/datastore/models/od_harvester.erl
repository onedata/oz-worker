%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for harvester record - representing a harvester in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(od_harvester).
-author("Michal Stanisz").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([create/1, save/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_harvester{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-type name() :: binary().
-type plugin() :: module().
-type endpoint() :: binary().
% Schema is stored as binary and it can contain e.g. encoded json.
-type schema() :: binary() | undefined.
-type entry_id() :: binary().

-type index_id() :: binary().
-type index() :: #harvester_index{}.
-type indices() :: #{index_id() => #harvester_index{}}.
% Index harvesting stats are stored per space per provider.
-type indices_stats() :: #{od_space:id() => #{od_provider:id() => #index_stats{}}}.

%% Batch entry is a map in a following format:
%% #{
%%    <<"fileId">> :: binary()
%%    <<"operation">> :: binary(), %% <<"submit">> | <<"delete">>
%%    <<"seq">> :: integer(),
%%    <<"payload">> :: #{
%%        json :: binary(),
%%        rdf :: binary(),
%%        xattrs :: json_map()
%%    }
%%  }
-type batch_entry() :: #{binary() => binary() | integer() | map()}.
-type batch() :: [batch_entry()].


-export_type([name/0, plugin/0, endpoint/0, schema/0, entry_id/0,
    index_id/0, index/0, indices/0, indices_stats/0, batch/0, batch_entry/0]).

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
%% Creates harvester.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Save harvester.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:save(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns harvester by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(HarvesterId) ->
    datastore_model:get(?CTX, HarvesterId).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether harvester given by ID exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(HarvesterId) ->
    datastore_model:exists(?CTX, HarvesterId).

%%--------------------------------------------------------------------
%% @doc
%% Updates harvester by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(HarvesterId, Diff) ->
    datastore_model:update(?CTX, HarvesterId, Diff).

%%--------------------------------------------------------------------
%% @doc
%% Deletes harvester by ID.
%% WARNING: Must not be used directly, as deleting a harvester that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a harvester use harvester_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(HarvesterId) ->
    datastore_model:delete(?CTX, HarvesterId).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all providers.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the harvester with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(HarvesterId :: id()) -> binary().
to_string(HarvesterId) ->
    <<"harvester:", HarvesterId/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    harvester_logic_plugin.

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
    2.

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {name, string},
        {plugin, atom},
        {endpoint, string},

        {gui_plugin_config, {custom, {json_utils, encode, decode}}},
        {public, boolean},

        {indices, #{string => {record, [
            {name, string},
            {schema, string},
            {guiPluginName, string},
            {stats, #{string => #{string => {record, [
                {current_seq, integer},
                {max_seq, integer},
                {last_update, integer},
                {error, string},
                {archival, boolean}
            ]}}}}
        ]}}},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {spaces, [string]},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {eff_providers, #{string => [{atom, string}]}},

        {creation_time, integer},
        {creator, {record, [
            {type, atom},
            {id, string}
        ]}},

        {bottom_up_dirty, boolean},
        {top_down_dirty, boolean}
    ]};
get_record_struct(2) ->
    % creator field - nested record changed from #client{} to #subject{}
    {record, [
        {name, string},
        {plugin, atom},
        {endpoint, string},

        {gui_plugin_config, {custom, {json_utils, encode, decode}}},
        {public, boolean},

        {indices, #{string => {record, [
            {name, string},
            {schema, string},
            {guiPluginName, string},
            {stats, #{string => #{string => {record, [
                {current_seq, integer},
                {max_seq, integer},
                {last_update, integer},
                {error, string},
                {archival, boolean}
            ]}}}}
        ]}}},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {spaces, [string]},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {eff_providers, #{string => [{atom, string}]}},

        {creation_time, integer},
        {creator, {record, [ % nested record changed from #client{} to #subject{}
            {type, atom},
            {id, string}
        ]}},

        {bottom_up_dirty, boolean},
        {top_down_dirty, boolean}
    ]}.


%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, Harvester) ->
    {
        od_harvester,
        Name,
        Plugin,
        Endpoint,

        GuiPluginConfig,
        Public,

        Indices,

        Users,
        Groups,
        Spaces,

        EffUsers,
        EffGroups,
        EffProviders,

        CreationTime,
        Creator,

        BottomUpDirty,
        TopDownDirty
    } = Harvester,
    {2, #od_harvester{
        name = Name,
        plugin = Plugin,
        endpoint = Endpoint,

        gui_plugin_config = GuiPluginConfig,
        public = Public,

        indices = Indices,

        users = Users,
        groups = Groups,
        spaces = Spaces,

        eff_users = EffUsers,
        eff_groups = EffGroups,
        eff_providers = EffProviders,

        creation_time = CreationTime,
        creator = upgrade_common:client_to_subject(Creator),

        bottom_up_dirty = BottomUpDirty,
        top_down_dirty = TopDownDirty
    }}.

