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
-export([critical_section_for/2]).
-export([get_ctx/0]).
-export([current_timestamp/0]).
-export([migrate_legacy_handles/0, migrate_legacy_handle/2]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_handle{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-type resource_type() :: binary().
-type resource_id() :: binary().
-type public_handle() :: binary().
% short literal (e.g. <<"oai_dc">>) that identifies a metadata format, allowed values depend on
% loaded handle_metadata_plugins
-type metadata_prefix() :: binary().
% metadata encoded to XML - used in the APIs and stored like this in the DB
-type raw_metadata() :: binary().
% parsed metadata expressed as nested xmerl records
-type parsed_metadata() :: #xmlElement{}.
-type timestamp_seconds() :: time:seconds().

-export_type([id/0, record/0]).
-export_type([resource_type/0, resource_id/0, public_handle/0,
    metadata_prefix/0, raw_metadata/0, parsed_metadata/0, timestamp_seconds/0]).

-define(CTX, #{
    model => od_handle,
    secure_fold_enabled => true,
    sync_enabled => true,
    memory_copies => all
}).

% the value for metadata_prefix field of handles that have been upgraded on the DB level,
% but not yet migrated to the new handle registry (@see migrate_legacy_handles/0)
-define(LEGACY_METADATA_PREFIX_INDICATOR, <<"legacy">>).

-compile([{no_auto_import, [get/1]}]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).


-spec get(id()) -> {ok, doc()} | {error, term()}.
get(HandleId) ->
    datastore_model:get(?CTX, HandleId).


-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(HandleId) ->
    datastore_model:exists(?CTX, HandleId).


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


-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold_keys(?CTX, fun(Id, Acc) -> {ok, [Id | Acc]} end, []).


-spec to_string(HandleId :: id()) -> binary().
to_string(HandleId) ->
    <<"handle:", HandleId/binary>>.


-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    handle_logic_plugin.


%% @doc Wraps the function in a critical section that locks on the specific handle.
-spec critical_section_for(id(), fun(() -> X)) -> X.
critical_section_for(HandleId, Fun) ->
    critical_section:run({?MODULE, HandleId}, Fun).


-spec get_ctx() -> datastore:ctx().
get_ctx() ->
    ?CTX.


-spec current_timestamp() -> timestamp_seconds().
current_timestamp() ->
    global_clock:timestamp_seconds().


%%--------------------------------------------------------------------
%% @doc
%% Migrates all handles to the handle registry - previously, they were stored as a
%% flat list per handle service in their docs. Firstly, the handle is added to the registry.
%% Secondly, its metadata is transformed using the same procedures as are applied in the
%% current software version.
%% The procedure is idempotent.
%% @end
%%--------------------------------------------------------------------
-spec migrate_legacy_handles() -> ok.
migrate_legacy_handles() ->
    lists:foreach(fun(#document{
        key = HServiceId,
        value = #od_handle_service{
            name = HServiceName,
            handles = Handles
        }
    }) ->
        ?info("Migrating ~B legacy handles from '~ts' (~ts)...", [length(Handles), HServiceName, HServiceId]),
        lists:foreach(fun(HandleId) -> migrate_legacy_handle(HServiceId, HandleId) end, Handles),
        ok = ?extract_ok(od_handle_service:update(HServiceId, fun(HService) ->
            {ok, HService#od_handle_service{handles = []}}
        end)),
        ?info("Successfully migrated legacy handles from '~ts' (~ts)", [HServiceName, HServiceId])
    end, element(2, {ok, _} = od_handle_service:list())).


% exported for tests
-spec migrate_legacy_handle(od_handle_service:id(), id()) -> ok.
migrate_legacy_handle(HServiceId, HandleId) ->
    case get(HandleId) of
        {error, not_found} ->
            ?error("The handle ~ts was not found in DB, skipping its migration", [HandleId]);
        {ok, #document{value = Handle}} ->
            migrate_legacy_handle(HServiceId, HandleId, Handle)
    end.

%% @private
-spec migrate_legacy_handle(od_handle_service:id(), id(), record()) -> ok.
migrate_legacy_handle(HServiceId, HandleId, #od_handle{
    resource_id = ShareId,
    metadata = Metadata,
    public_handle = PublicHandle,
    timestamp = Timestamp
}) ->
    % all legacy handles were created with Dublin Core metadata
    case ?catch_exceptions(handle_registry:report_created(?OAI_DC_METADATA_PREFIX, HServiceId, HandleId, Timestamp)) of
        ok ->
            ok;
        ?ERROR_ALREADY_EXISTS ->
            ?info("The handle ~ts appears to already be registered (during a previous run?)", [HandleId])
    end,
    % legacy code allowed invalid XML DC metadata - in such a case, use empty metadata
    MetadataXml = case oai_xml:parse(Metadata) of
        error ->
            #xmlElement{name = metadata, content = []};
        {ok, #xmlElement{name = metadata} = Parsed} ->
            Parsed;
        {ok, _} ->
            #xmlElement{name = metadata, content = []}
    end,
    {ok, RevisedMetadata} = oai_metadata:revise_for_publication(
        ?OAI_DC_METADATA_PREFIX,
        MetadataXml,
        ShareId,
        #od_share{}  % this argument can be whatever as dublin core metadata plugin ignores it
    ),
    FinalMetadata = oai_metadata:insert_public_handle(?OAI_DC_METADATA_PREFIX, RevisedMetadata, PublicHandle),
    FinalRawMetadata = oai_metadata:encode_xml(?OAI_DC_METADATA_PREFIX, FinalMetadata),
    ok = ?extract_ok(update(HandleId, fun
        (#od_handle{metadata_prefix = ?OAI_DC_METADATA_PREFIX} = Handle) ->
            % updated metadata prefix indicates that this handle has already been upgraded
            % (this procedure must be idempotent; it might have crashed mid-way before)
            ?info("The handle ~ts appears to already be migrated (during a previous run?)", [HandleId]),
            {ok, Handle};
        (#od_handle{metadata_prefix = ?LEGACY_METADATA_PREFIX_INDICATOR} = Handle) ->
            {ok, Handle#od_handle{metadata_prefix = ?OAI_DC_METADATA_PREFIX, metadata = FinalRawMetadata}}
    end)).

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
        metadata_prefix = ?LEGACY_METADATA_PREFIX_INDICATOR,
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