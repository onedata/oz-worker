%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Database model representing file handles.
%%% @end
%%%-------------------------------------------------------------------
-module(od_handle).
-author("Tomasz Lichon").

-include("datastore/oz_datastore_models.hrl").
-include("http/handlers/oai.hrl").

%% API
-export([create/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).
-export([get_ctx/0]).
-export([current_timestamp/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_handle{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-type resource_type() :: binary().
-type resource_id() :: binary().
-type public_handle() :: binary().
-type metadata_prefix() :: binary().  % ?OAI_DC_METADATA_PREFIX | ?EDM_METADATA_PREFIX   - @see oai.hrl
-type metadata() :: binary().
-type timestamp_seconds() :: time:seconds().

-export_type([id/0, record/0]).
-export_type([resource_type/0, resource_id/0, public_handle/0,
    metadata_prefix/0, metadata/0, timestamp_seconds/0]).

-define(CTX, #{
    model => od_handle,
    secure_fold_enabled => true,
    sync_enabled => true,
    memory_copies => all
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates handle.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns handle by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(HandleId) ->
    datastore_model:get(?CTX, HandleId).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether handle given by ID exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(HandleId) ->
    datastore_model:exists(?CTX, HandleId).

%%--------------------------------------------------------------------
%% @doc
%% Updates handle by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(HandleId, Diff) ->
    datastore_model:update(?CTX, HandleId, Diff).

%%--------------------------------------------------------------------
%% @doc
%% Deletes handle by ID.
%% WARNING: Must not be used directly, as deleting a handle that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a handle use handle_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(HandleId) ->
    datastore_model:delete(?CTX, HandleId).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all handles.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the handle with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(HandleId :: id()) -> binary().
to_string(HandleId) ->
    <<"handle:", HandleId/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    handle_logic_plugin.

-spec get_ctx() -> datastore:ctx().
get_ctx() ->
    ?CTX.

%%--------------------------------------------------------------------
%% @equiv global_clock:timestamp_seconds().
%% @end
%%--------------------------------------------------------------------
-spec current_timestamp() -> timestamp_seconds().
current_timestamp() ->
    global_clock:timestamp_seconds().

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
    8.

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {public_handle, string},
        {resource_type, string},
        {resource_id, string},
        {metadata, string},
        {timestamp, {{integer, integer, integer}, {integer, integer, integer}}},
        {handle_service, string},
        {users, [{string, [atom]}]},
        {groups, [{string, [atom]}]},
        {eff_users, [{string, [atom]}]},
        {eff_groups, [{string, [atom]}]},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(2) ->
    {record, [
        {public_handle, string},
        {resource_type, string},
        {metadata, string},
        {timestamp, {{integer, integer, integer}, {integer, integer, integer}}},
        {resource_id, string},
        {handle_service, string},
        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(3) ->
    % There are no changes, but all records must be marked dirty to recalculate
    % effective relations (as intermediaries computing logic has changed).
    get_record_struct(2);
get_record_struct(4) ->
    % * new field - creation_time
    % * new field - creator
    {record, [
        {public_handle, string},
        {resource_type, string},
        {metadata, string},
        {timestamp, {{integer, integer, integer}, {integer, integer, integer}}},
        {resource_id, string},

        {handle_service, string},
        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},

        {creation_time, integer}, % New field
        {creator, {record, [ % New field
            {type, atom},
            {id, string}
        ]}},

        {bottom_up_dirty, boolean}
    ]};
get_record_struct(5) ->
    % creator field - nested record changed from #client{} to #subject{}
    {record, [
        {public_handle, string},
        {resource_type, string},
        {metadata, string},
        {timestamp, {{integer, integer, integer}, {integer, integer, integer}}},
        {resource_id, string},

        {handle_service, string},
        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},

        {creation_time, integer},
        {creator, {record, [ % nested record changed from #client{} to #subject{}
            {type, atom},
            {id, string}
        ]}},

        {bottom_up_dirty, boolean}
    ]};
get_record_struct(6) ->
    % creator field - nested #subject{} record and encoding changed
    {record, [
        {public_handle, string},
        {resource_type, string},
        {metadata, string},
        {timestamp, {{integer, integer, integer}, {integer, integer, integer}}},
        {resource_id, string},

        {handle_service, string},
        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},

        {creation_time, integer},
        % nested #subject{} record was extended and is now encoded as string
        % rather than record tuple
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},

        {bottom_up_dirty, boolean}
    ]};
get_record_struct(7) ->
    % timestamp field - changed format from datetime to timestamp in seconds
    {record, [
        {public_handle, string},
        {resource_type, string},
        {metadata, string},
        {timestamp, integer},  % changed field
        {resource_id, string},

        {handle_service, string},
        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},

        {bottom_up_dirty, boolean}
    ]};
get_record_struct(8) ->
    % added new field metadata_prefix
    {record, [
        {public_handle, string},
        {resource_type, string},
        {metadata_prefix, string}, % new field
        {metadata, string},
        {timestamp, integer},
        {resource_id, string},

        {handle_service, string},
        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},

        {bottom_up_dirty, boolean}
    ]}.


%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, Handle) ->
    {
        od_handle,
        PublicHandle,
        ResourceType,
        ResourceId,
        Metadata,
        Timestamp,

        HandleService,
        Users,
        Groups,

        _EffUsers,
        _EffGroups,

        _BottomUpDirty
    } = Handle,
    {2, {od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        Timestamp,

        ResourceId,
        HandleService,
        maps:from_list(Users),
        maps:from_list(Groups),

        #{},
        #{},

        true
    }};
upgrade_record(2, Handle) ->
    {od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        Timestamp,

        ResourceId,
        HandleService,
        Users,
        Groups,

        _EffUsers,
        _EffGroups,

        _BottomUpDirty
    } = Handle,
    {3, {od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        Timestamp,

        ResourceId,
        HandleService,
        Users,
        Groups,

        #{},
        #{},

        true
    }};
upgrade_record(3, Handle) ->
    {od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        Timestamp,

        ResourceId,
        HandleService,
        Users,
        Groups,

        EffUsers,
        EffGroups,

        BottomUpDirty
    } = Handle,
    {4, {
        od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        Timestamp,

        ResourceId,
        HandleService,
        Users,
        Groups,

        EffUsers,
        EffGroups,

        global_clock:timestamp_seconds(),
        undefined,

        BottomUpDirty
    }};
upgrade_record(4, Handle) ->
    {
        od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        Timestamp,

        ResourceId,
        HandleService,
        Users,
        Groups,

        EffUsers,
        EffGroups,

        CreationTime,
        Creator,

        BottomUpDirty
    } = Handle,
    {5, {
        od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        Timestamp,

        ResourceId,
        HandleService,
        Users,
        Groups,

        EffUsers,
        EffGroups,

        CreationTime,
        upgrade_common:client_to_subject(Creator),

        BottomUpDirty
    }};
upgrade_record(5, Handle) ->
    {
        od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        Timestamp,

        ResourceId,
        HandleService,
        Users,
        Groups,

        EffUsers,
        EffGroups,

        CreationTime,
        Creator,

        BottomUpDirty
    } = Handle,
    {6, {
        od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        Timestamp,

        ResourceId,
        HandleService,
        Users,
        Groups,

        EffUsers,
        EffGroups,

        CreationTime,
        upgrade_common:upgrade_subject_record(Creator),

        BottomUpDirty
    }};
upgrade_record(6, Handle) ->
    {
        od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        Timestamp,

        ResourceId,
        HandleService,
        Users,
        Groups,

        EffUsers,
        EffGroups,

        CreationTime,
        Creator,

        BottomUpDirty
    } = Handle,
    {7, {
        od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        time:datetime_to_seconds(Timestamp),

        ResourceId,
        HandleService,
        Users,
        Groups,

        EffUsers,
        EffGroups,

        CreationTime,
        Creator,

        BottomUpDirty
    }};
upgrade_record(7, Handle) ->
    {
        od_handle,
        PublicHandle,
        ResourceType,
        Metadata,
        Timestamp,

        ResourceId,
        HandleService,
        Users,
        Groups,

        EffUsers,
        EffGroups,

        CreationTime,
        Creator,

        BottomUpDirty
    } = Handle,
    {8, #od_handle{
        public_handle = PublicHandle,
        resource_type = ResourceType,
        metadata_prefix = ?OAI_DC_METADATA_PREFIX,
        metadata = Metadata,
        timestamp = Timestamp,

        resource_id = ResourceId,
        handle_service = HandleService,
        users = Users,
        groups = Groups,

        eff_users = EffUsers,
        eff_groups = EffGroups,

        creation_time = CreationTime,
        creator = Creator,

        bottom_up_dirty = BottomUpDirty
    }}.