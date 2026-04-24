%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for share record - representing a file share in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(od_share).
-author("Lukasz Opiola").

-include("http/gui_paths.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata_file.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([create/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).
-export([critical_section_for/2]).
-export([get_ctx/0]).
-export([build_root_file/3]).
-export([build_public_url/1]).
-export([build_public_rest_url/1]).
-export([choose_provider_for_public_share_handling/1]).
-export([increment_visit_count/1]).
-export([migrate_legacy_shares_21_02_8/0, migrate_legacy_share_21_02_8/2]).
-export([reorganize_shares_to_inline_registries_25_1/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_share{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-type name() :: binary().
% publicly visible share description in markdown (.md) format
-type description() :: binary().
-type file_type() :: ?REGULAR_FILE_TYPE | ?DIRECTORY_TYPE.

-export_type([name/0, description/0, file_type/0]).

-define(CTX, #{
    model => ?MODULE,
    secure_fold_enabled => true,
    sync_enabled => true,
    memory_copies => all
}).

-compile([{no_auto_import, [get/1]}]).

%%%===================================================================
%%% API
%%%===================================================================


-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).


-spec get(id()) -> {ok, doc()} | {error, term()}.
get(ShareId) ->
    datastore_model:get(?CTX, ShareId).


-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(ShareId) ->
    datastore_model:exists(?CTX, ShareId).


-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(ShareId, Diff) ->
    datastore_model:update(?CTX, ShareId, Diff).


%%--------------------------------------------------------------------
%% @doc
%% Deletes share by ID.
%% WARNING: Must not be used directly, as deleting a share that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a share use share_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(ShareId) ->
    datastore_model:delete(?CTX, ShareId).


-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).


%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the share with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(ShareId :: id()) -> binary().
to_string(ShareId) ->
    <<"share:", ShareId/binary>>.


%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    share_logic_plugin.


%% @doc Wraps the function in a critical section that locks on the specific share.
-spec critical_section_for(id(), fun(() -> X)) -> X.
critical_section_for(ShareId, Fun) ->
    critical_section:run({?MODULE, ShareId}, Fun).


-spec get_ctx() -> datastore:ctx().
get_ctx() ->
    ?CTX.


-spec build_root_file
    (guid, od_space:id(), od_share:record()) -> file_id:file_guid();
    (objectid, od_space:id(), od_share:record()) -> file_id:objectid().
build_root_file(guid, ShareId, #od_share{root_file_uuid = RootFileUuid, space = SpaceId}) ->
    file_id:pack_share_guid(RootFileUuid, SpaceId, ShareId);
build_root_file(objectid, ShareId, ShareRecord) ->
    ?check(file_id:guid_to_objectid(build_root_file(guid, ShareId, ShareRecord))).


%%--------------------------------------------------------------------
%% @doc
%% Public access URL for a share that points to Onezone. Onezone
%% will then redirect clients to one of providers that support the
%% parent space of the share.
%% @end
%%--------------------------------------------------------------------
-spec build_public_url(od_share:id()) -> binary().
build_public_url(ShareId) ->
    oz_worker:get_uri(?PUBLIC_SHARE_URN(ShareId)).


%% @doc Public access REST URL to get share details.
-spec build_public_rest_url(od_share:id()) -> binary().
build_public_rest_url(ShareId) ->
    oz_worker:get_rest_uri(?PUBLIC_SHARE_REST_PATH(ShareId)).


%%--------------------------------------------------------------------
%% @doc
%% Chooses a provider to handle requests concerning public share information
%% and data (for all interfaces).
%%
%% @see od_space:choose_provider_for_request_handling/1 for details about the
%% provider selection logic.
%% @end
%%--------------------------------------------------------------------
-spec choose_provider_for_public_share_handling(od_share:id()) ->
    {ok, {od_provider:id(), onedata:release_version()}} | od_error:error().
choose_provider_for_public_share_handling(ShareId) ->
    case get(ShareId) of
        {ok, #document{value = #od_share{space = SpaceId}}} ->
            od_space:choose_provider_for_request_handling(SpaceId);
        {error, not_found} ->
            ?ERROR_NOT_FOUND
    end.


-spec increment_visit_count(od_share:id()) -> ok.
increment_visit_count(ShareId) ->
    ?check(od_share:update(ShareId, fun(ShareRecord = #od_share{visit_count = VC}) ->
        {ok, ShareRecord#od_share{visit_count = VC + 1}}
    end)).


%%--------------------------------------------------------------------
%% @doc
%% Migrates all shares to the share registry - previously, they were stored as a
%% flat list per space in their docs.
%% The procedure is idempotent.
%% Introduced in version 21.02.8.
%% @end
%%--------------------------------------------------------------------
migrate_legacy_shares_21_02_8() ->
    lists:foreach(fun
        (#document{key = SpaceId, value = #od_space{name = SpaceName, shares = []}}) ->
            ?info("No migration needed for space '~ts' (~ts) - no shares found", [SpaceName, SpaceId]);
        (#document{key = SpaceId, value = #od_space{name = SpaceName, shares = Shares}}) ->
            ?info("Found ~B legacy share(s) in space '~ts' (~ts), migrating...", [length(Shares), SpaceName, SpaceId]),

            {Ok, AlreadyMigrated, ShareMissing, HandleMissing} = lists:foldl(
                fun(ShareId, {AccOk, AccAM, AccSM, AccHM}) ->
                case migrate_legacy_share_21_02_8(ShareId) of
                    ok -> {AccOk + 1, AccAM, AccSM, AccHM};
                    already_migrated -> {AccOk, AccAM + 1, AccSM, AccHM};
                    share_missing -> {AccOk, AccAM, AccSM + 1, AccHM};
                    handle_missing -> {AccOk, AccAM, AccSM, AccHM + 1}
                end
            end,
                {0, 0, 0, 0},
                Shares
            ),

            ok = ?extract_ok(od_space:update(SpaceId, fun(SpaceRecord) ->
                {ok, SpaceRecord#od_space{shares = []}}
            end)),

            ?info(
                "Finished migration of legacy share(s) from space '~ts' (~ts):~n"
                "> successful: ~4B~n"
                "> not needed: ~4B  (already migrated during previous runs)~n"
                "> partial:    ~4B  (the corresponding handle could not be found and was discarded)~n"
                "> discarded:  ~4B  (could not be found in the database and cannot be recovered)", [
                    SpaceName, SpaceId,
                    Ok,
                    AlreadyMigrated,
                    HandleMissing,
                    ShareMissing
                ])
    end, ?check(od_space:list())).


% exported for tests
-spec migrate_legacy_share_21_02_8(id()) -> ok | already_migrated | share_missing | handle_missing.
migrate_legacy_share_21_02_8(ShareId) ->
    case get(ShareId) of
        {error, not_found} ->
            ?error("The share ~ts was not found in DB - skipping its migration", [ShareId]),
            share_missing;
        {ok, #document{value = ShareRecord}} ->
            case share_registry:find_entry(ShareId, ShareRecord) of
                {ok, _} ->
                    % must be a consecutive run of the upgrade procedure
                    already_migrated;
                error ->
                    migrate_legacy_share_21_02_8(ShareId, ShareRecord)
            end
    end.

%% @private
-spec migrate_legacy_share_21_02_8(id(), record()) -> ok | handle_missing.
migrate_legacy_share_21_02_8(ShareId, ShareRecord = #od_share{handle = HandleId}) ->
    % The share registry assumes that each handle goes through a lifecycle
    % (created -> name/handle updated -> deleted) and it's not possible
    % to add an entry with a handle in one go; it must be split into
    % two share_registry calls. The first one requires the handle to be
    % not yet defined.
    ShareRecordWithoutHandle = ShareRecord#od_share{handle = undefined},
    try
        % ensure idempotency in case of multiple re-runs
        share_registry:report_created(ShareId, ShareRecordWithoutHandle)
    catch throw:?ERROR_ALREADY_EXISTS ->
        ok
    end,
    case HandleId of
        undefined ->
            ok;
        _ ->
            case od_handle:get(HandleId) of
                {error, not_found} ->
                    ?error(?autoformat_with_msg(
                        "Stumbled upon a share that had a handle, but the handle doc was not found - "
                        "registering the share as one without a handle",
                        [ShareId, HandleId]
                    )),
                    {ok, _} = od_share:update(ShareId, fun(S) -> {ok, S#od_share{handle = undefined}} end),
                    handle_missing;
                {ok, #document{value = #od_handle{public_handle = PublicHandle}}} ->
                    try
                        % ensure idempotency in case of multiple re-runs
                        share_registry:report_handle_created_for(
                            ShareId, ShareRecordWithoutHandle, HandleId, PublicHandle
                        )
                    catch throw:?ERROR_ALREADY_EXISTS ->
                        ok
                    end
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% For space with a lower share count, migrates the shares from the
%% share registry to the inline registry stored in the space doc.
%% This is done for performance reasons - link listing is very expensive
%% for spaces with a low share count (e.g. 1000x slower).
%%
%% The procedure is idempotent.
%% Introduced in version 25.1.
%% @end
%%--------------------------------------------------------------------
-spec reorganize_shares_to_inline_registries_25_1() -> ok.
reorganize_shares_to_inline_registries_25_1() ->
    ?notice("Reorganizing the share registries..."),
    {ok, SpaceDocs} = od_space:list(),
    lists:foreach(fun(#document{key = SpaceId, value = #od_space{name = SpaceName}}) ->
        try
            UpdatedInlineRegistry = share_registry:ensure_reorganized(SpaceId),
            ShareCount = inline_share_registry:get_share_count(UpdatedInlineRegistry),
            InlineRegistryInfo = case inline_share_registry:is_active(UpdatedInlineRegistry) of
                true -> <<"inline registry">>;
                false -> <<"link-based registry">>
            end,
            ?info("* ~ts\t~ts:\t~B share(s) - ~ts", [
                SpaceId, SpaceName, ShareCount, InlineRegistryInfo
            ])
        catch Class:Reason:Stacktrace ->
            ?critical_exception(
                ?autoformat_with_msg(
                    "Unexpected error - failed to reorganize shares, cannot recover. "
                    "Consider retrying the upgrade procedure by restarting the service.",
                    [SpaceId, SpaceName]
                ),
                Class, Reason, Stacktrace
            ),
            error(share_reorganization_failed)
        end
    end, SpaceDocs).


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
    9.

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
        {public_url, string},
        {space, string},
        {handle, string},
        {root_file, string},
        {eff_users, [string]},
        {eff_groups, [string]},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(2) ->
    {record, [
        {name, string},
        {public_url, string},
        {space, string},
        {handle, string},
        {root_file, string}
    ]};
get_record_struct(3) ->
    % * new field - creation_time
    % * new field - creator
    {record, [
        {name, string},
        {public_url, string},
        {space, string},
        {handle, string},
        {root_file, string},

        {creation_time, integer}, % New field
        {creator, {record, [ % New field
            {type, atom},
            {id, string}
        ]}}
    ]};
get_record_struct(4) ->
    % creator field - nested record changed from #client{} to #subject{}
    {record, [
        {name, string},
        {public_url, string},
        {space, string},
        {handle, string},
        {root_file, string},

        {creation_time, integer},
        {creator, {record, [ % nested record changed from #client{} to #subject{}
            {type, atom},
            {id, string}
        ]}}
    ]};
get_record_struct(5) ->
    % new field - file_type
    % creator field - nested #subject{} record and encoding changed
    {record, [
        {name, string},
        {public_url, string},
        {space, string},
        {handle, string},

        {root_file, string},
        {file_type, atom},

        {creation_time, integer},
        % nested #subject{} record was extended and is now encoded as string
        % rather than record tuple
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}}
    ]};
get_record_struct(6) ->
    % new field - description
    {record, [
        {name, string},
        {description, string}, % new field
        {public_url, string},
        {space, string},
        {handle, string},

        {root_file, string},
        {file_type, atom},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}}
    ]};
get_record_struct(7) ->
    % removed field - public_url
    {record, [
        {name, string},
        {description, string},
        {space, string},
        {handle, string},

        {root_file, string},
        {file_type, atom},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}}
    ]};
get_record_struct(8) ->
    % reworked field - root_file (share guid) to root_file_uuid
    % reworked field - file_type from [file, dir] to [?REGULAR_FILE_TYPE, ?DIRECTORY_TYPE]
    {record, [
        {name, string},
        {description, string},
        {space, string},
        {handle, string},

        {root_file_uuid, string},
        {file_type, atom},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}}
    ]};
get_record_struct(9) ->
    % new field - visit_count
    {record, [
        {name, string},
        {description, string},
        {space, string},
        {handle, string},

        {root_file_uuid, string},
        {file_type, atom},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},

        {visit_count, integer}
    ]}.

%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, Share) ->
    {
        od_share,
        Name,
        PublicUrl,
        SpaceId,
        HandleId,
        RootFileId,

        _EffUsers,
        _EffGroups,

        _BottomUpDirty
    } = Share,
    {2, {od_share,
        Name,
        PublicUrl,
        SpaceId,
        HandleId,
        RootFileId
    }};
upgrade_record(2, Share) ->
    {
        od_share,
        Name,
        PublicUrl,
        SpaceId,
        HandleId,
        RootFileId
    } = Share,
    {3, {
        od_share,
        Name,
        PublicUrl,
        SpaceId,
        HandleId,
        RootFileId,

        global_clock:timestamp_seconds(),
        undefined
    }};
upgrade_record(3, Share) ->
    {
        od_share,
        Name,
        PublicUrl,
        SpaceId,
        HandleId,
        RootFileId,

        CreationTime,
        Creator
    } = Share,
    {4, {
        od_share,
        Name,
        PublicUrl,
        SpaceId,
        HandleId,
        RootFileId,

        CreationTime,
        upgrade_common:client_to_subject(Creator)
    }};
upgrade_record(4, Share) ->
    {
        od_share,
        Name,
        PublicUrl,
        SpaceId,
        HandleId,
        RootFileId,

        CreationTime,
        Subject
    } = Share,
    {5, {od_share,
        Name,
        PublicUrl,

        SpaceId,
        HandleId,

        RootFileId,
        dir,

        CreationTime,
        upgrade_common:upgrade_subject_record(Subject)
    }};
upgrade_record(5, Share) ->
    {
        od_share,
        Name,
        PublicUrl,

        SpaceId,
        HandleId,

        RootFileId,
        FileType,

        CreationTime,
        Creator
    } = Share,
    {6, {od_share,
        Name,
        <<"">>,
        PublicUrl,

        SpaceId,
        HandleId,

        RootFileId,
        FileType,

        CreationTime,
        Creator
    }};
upgrade_record(6, Share) ->
    {
        od_share,
        Name,
        Description,
        _PublicUrl,

        SpaceId,
        HandleId,

        RootFileId,
        FileType,

        CreationTime,
        Creator
    } = Share,
    {7, {od_share,
        Name,
        Description,

        SpaceId,
        HandleId,

        RootFileId,
        FileType,

        CreationTime,
        Creator
    }};
upgrade_record(7, Share) ->
    {
        od_share,
        Name,
        Description,

        SpaceId,
        HandleId,

        RootFileId,
        OldFileType,

        CreationTime,
        Creator
    } = Share,
    RootFileUuid = try
        case file_id:unpack_share_guid(RootFileId) of
            {FileUuid, SpaceId, _ShareId} when is_binary(FileUuid) andalso byte_size(FileUuid) > 0 ->
                FileUuid
        end
    catch _:_ ->
        % This should not happen, but theoretically in older versions of the system
        % it was possible to create shares with an invalid root_file. In this case,
        % generate a dummy uuid so that the record is semantically correct; it will
        % show up as a share with deleted files in the system.
        GeneratedDummyUuid = datastore_key:new_from_digest(RootFileId),
        ?warning(?autoformat_with_msg("Invalid root_file found in a share, regenerating a dummy one", [
            Name,
            SpaceId,
            RootFileId,
            HandleId,
            GeneratedDummyUuid
        ])),
        GeneratedDummyUuid
    end,
    {8, {od_share,
        Name,
        Description,

        SpaceId,
        HandleId,

        RootFileUuid,
        case OldFileType of
            file -> ?REGULAR_FILE_TYPE;
            dir -> ?DIRECTORY_TYPE
        end,

        CreationTime,
        Creator
    }};
upgrade_record(8, Share) ->
    {
        od_share,
        Name,
        Description,

        SpaceId,
        HandleId,

        RootFileUuid,
        FileType,

        CreationTime,
        Creator
    } = Share,
    {9, #od_share{
        name = Name,
        description = Description,

        space = SpaceId,
        handle = HandleId,

        root_file_uuid = RootFileUuid,
        file_type = FileType,

        creation_time = CreationTime,
        creator = Creator,

        visit_count = 0
    }}.
