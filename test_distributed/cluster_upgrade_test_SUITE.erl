%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for the cluster upgrade procedures.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_upgrade_test_SUITE).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    upgrade_from_21_02_4_handles/1,
    upgrade_from_21_02_7_shares/1,
    upgrade_from_25_0_shares/1
]).

all() -> ?ALL([
    upgrade_from_21_02_4_handles,
    upgrade_from_21_02_7_shares,
    upgrade_from_25_0_shares
]).


-define(RAND_SHARE_ID(), datastore_key:new()).
-define(RAND_XML_SAFE_UNICODE_STR(), lists:foldl(fun(Char, Acc) ->
    binary:replace(Acc, <<Char>>, <<"">>, [global])
end, ?RAND_UNICODE_STR(), [$<, $>, $&, $', $"])).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ozt:init_per_suite(Config).


end_per_suite(_Config) ->
    ok.


init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    ozt_mocks:mock_handle_proxy(),

    AllHandles = [D#document.key || D <- element(2, ozt:rpc(od_handle, list, []))],
    lists_utils:pforeach(fun(HandleId) ->
        try
            ok = ozt:rpc(handle_logic, delete, [?ROOT, HandleId])
        catch _:_ ->
            ozt:rpc(od_handle, force_delete, [HandleId])
        end
    end, AllHandles),

    oz_test_utils:delete_all_entities(Config),
    % if the suite fails midway, there may be some remnants in the handle registry - clean it up
    % (delete_all_entities won't do it because some of the handles may still have the "legacy" metadata schema set)
    lists:foreach(fun(#handle_listing_entry{timestamp = Timestamp, handle_id = HandleId, service_id = HServiceId}) ->
        ozt:rpc(handle_registry, report_deleted, [?OAI_DC_METADATA_PREFIX, HServiceId, HandleId, Timestamp, Timestamp])
    end, list_handles_completely(#{metadata_schema => ?OAI_DC_METADATA_PREFIX})),
    Config.


end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time(),
    ozt_mocks:unmock_handle_proxy(),
    ok.

%%%===================================================================
%%% Test functions
%%%===================================================================

upgrade_from_21_02_4_handles(_Config) ->
    HServiceAlpha = ozt_handle_services:create(),
    HServiceBeta = ozt_handle_services:create(),
    HServiceGamma = ozt_handle_services:create(),

    Spaces = lists_utils:generate(fun ozt_spaces:create/0, 50),

    ?assertEqual([], list_handles_completely(#{metadata_schema => ?OAI_DC_METADATA_PREFIX})),
    ?assertEqual([], gather_handles_by_all_prefixes()),

    PreexistingHandleDocs = lists:sort(lists:flatmap(fun(HServiceId) ->
        HandleDocsForHService = lists_utils:generate(fun() ->
            #document{
                key = HandleId,
                value = #od_handle{
                    timestamp = Timestamp
                }
            } = Doc = create_legacy_handle(?RAND_ELEMENT(Spaces), HServiceId),
            % simulate a case when some handles have already been upgraded
            % (during a previous upgrade run that could have crashed or been interrupted)
            case ?RAND_INT(1, 5) of
                1 ->
                    % the handle was fully migrated
                    ?assertMatch(ok, ozt:rpc(od_handle, migrate_legacy_handle_21_02_5, [HServiceId, HandleId]));
                2 ->
                    % the handle was partially migrated
                    ?assertMatch(ok, ozt:rpc(handle_registry, report_created, [
                        ?OAI_DC_METADATA_PREFIX, HServiceId, HandleId, Timestamp
                    ]));
                _ ->
                    % the handle has not been migrated at all
                    ok
            end,
            Doc
        end, ?RAND_INT(100, 1000)),
        % invalid handle ids (for which documents cannot be found in the DB)
        % should be ignored by the upgrade procedure and not cause it to crash
        InvalidHandleIds = lists_utils:generate(fun datastore_key:new/0, ?RAND_INT(0, 10)),
        ozt:rpc(od_handle_service, update, [HServiceId, fun(HService) ->
            {ok, HService#od_handle_service{
                handles = ?SHUFFLED(docs_to_ids(HandleDocsForHService) ++ InvalidHandleIds)
            }}
        end]),
        HandleDocsForHService
    end, [HServiceAlpha, HServiceBeta, HServiceGamma])),

    ?assertEqual({ok, 4}, ozt:rpc(node_manager_plugin, upgrade_cluster, [3])),

    lists:foreach(fun(HServiceId) ->
        ?assertMatch(#od_handle_service{handles = []}, ozt_handle_services:get(HServiceId))
    end, [HServiceAlpha, HServiceBeta, HServiceGamma]),

    ?assertEqual(
        handle_docs_to_exp_listing_entries(PreexistingHandleDocs),
        list_handles_completely(#{metadata_schema => ?OAI_DC_METADATA_PREFIX})
    ),
    ?assertEqual(
        handle_docs_to_exp_listing_entries(PreexistingHandleDocs),
        gather_handles_by_all_prefixes()
    ),

    lists:foreach(fun(#document{key = HandleId, value = PreexistingHandleRecord}) ->
        MigratedHandleRecord = ozt_handles:get(HandleId),
        ?assertEqual(MigratedHandleRecord#od_handle.public_handle, PreexistingHandleRecord#od_handle.public_handle),
        ?assertEqual(MigratedHandleRecord#od_handle.resource_type, PreexistingHandleRecord#od_handle.resource_type),
        ?assertEqual(MigratedHandleRecord#od_handle.metadata_schema, ?OAI_DC_METADATA_PREFIX),
        ?assertEqual(MigratedHandleRecord#od_handle.metadata, exp_handle_metadata(PreexistingHandleRecord)),
        ?assertEqual(MigratedHandleRecord#od_handle.timestamp, PreexistingHandleRecord#od_handle.timestamp),
        ?assertEqual(MigratedHandleRecord#od_handle.resource_id, PreexistingHandleRecord#od_handle.resource_id),
        ?assertEqual(MigratedHandleRecord#od_handle.handle_service, PreexistingHandleRecord#od_handle.handle_service),
        ?assertEqual(MigratedHandleRecord#od_handle.users, PreexistingHandleRecord#od_handle.users),
        ?assertEqual(MigratedHandleRecord#od_handle.groups, PreexistingHandleRecord#od_handle.groups),
        ?assertEqual(MigratedHandleRecord#od_handle.eff_users, PreexistingHandleRecord#od_handle.eff_users),
        ?assertEqual(MigratedHandleRecord#od_handle.eff_groups, PreexistingHandleRecord#od_handle.eff_groups),
        ?assertEqual(MigratedHandleRecord#od_handle.creation_time, PreexistingHandleRecord#od_handle.creation_time),
        ?assertEqual(MigratedHandleRecord#od_handle.creator, PreexistingHandleRecord#od_handle.creator)
    end, PreexistingHandleDocs),

    % make sure that a consecutive upgrade to the next version works as expected
    lists:foreach(fun(SpaceId) ->
        ?assertEqual([], list_shares_completely(SpaceId))
    end, Spaces),

    ?assertEqual({ok, 5}, ozt:rpc(node_manager_plugin, upgrade_cluster, [4])),

    {ok, PreexistingShareDocs} = ozt:rpc(od_share, list, []),
    lists:foreach(fun(SpaceId) ->
        ?assertMatch(#od_space{shares = []}, ozt_spaces:get(SpaceId)),
        ?assertEqual(
            share_docs_to_exp_listing_entries(
                [D || D <- PreexistingShareDocs, D#document.value#od_share.space == SpaceId]
            ),
            list_shares_completely(SpaceId)
        )
    end, Spaces).


upgrade_from_21_02_7_shares(_Config) ->
    SpaceAlpha = ozt_spaces:create(),
    SpaceBeta = ozt_spaces:create(),
    SpaceGamma = ozt_spaces:create(),
    AllSpaces = [SpaceAlpha, SpaceBeta, SpaceGamma],

    HServices = lists_utils:generate(fun ozt_handle_services:create/0, 50),

    lists:foreach(fun(SpaceId) ->
        ?assertEqual([], list_shares_completely(SpaceId))
    end, AllSpaces),

    PreexistingShareDocs = lists:sort(lists:flatmap(fun(SpaceId) ->
        ShareDocsForSpace = lists_utils:generate(fun() ->
            ShareDoc = #document{
                key = ShareId,
                value = ShareRecord = #od_share{handle = HandleId}
            } = create_legacy_share(SpaceId, ?RAND_CHOICE(without_handle, {with_handle, ?RAND_ELEMENT(HServices)})),
            % simulate edge cases e.g. when some shares have already been upgraded
            % (during a previous upgrade run that could have crashed or been interrupted)
            case ?RAND_INT(1, 5) of
                1 ->
                    % the share was fully migrated
                    ?assertMatch(ok, ozt:rpc(od_share, migrate_legacy_share_21_02_8, [ShareId, ShareRecord]));
                2 ->
                    % the share was partially migrated
                    ShareRecordWithoutHandle = ShareRecord#od_share{handle = undefined},
                    ?assertMatch(ok, ozt:rpc(share_registry, report_created, [ShareId, ShareRecordWithoutHandle]));
                3 when HandleId /= undefined ->
                    % the handle doc is missing
                    ozt:rpc(od_handle, force_delete, [ShareRecord#od_share.handle]);
                _ ->
                    % the share has not been migrated at all
                    ok
            end,
            ShareDoc
        end, ?RAND_INT(100, 1000)),
        % invalid share ids (for which documents cannot be found in the DB)
        % should be ignored by the upgrade procedure and not cause it to crash
        InvalidShareIds = lists_utils:generate(fun datastore_key:new/0, ?RAND_INT(0, 10)),
        ozt:rpc(od_space, update, [SpaceId, fun(Space) ->
            {ok, Space#od_space{
                shares = ?SHUFFLED(docs_to_ids(ShareDocsForSpace) ++ InvalidShareIds)
            }}
        end]),
        ShareDocsForSpace
    end, AllSpaces)),

    ?assertEqual({ok, 5}, ozt:rpc(node_manager_plugin, upgrade_cluster, [4])),

    lists:foreach(fun(SpaceId) ->
        ?assertMatch(#od_space{shares = []}, ozt_spaces:get(SpaceId)),
        ?assertEqual(
            share_docs_to_exp_listing_entries(
                [D || D <- PreexistingShareDocs, D#document.value#od_share.space == SpaceId]
            ),
            list_shares_completely(SpaceId)
        )
    end, AllSpaces),

    lists:foreach(fun(#document{key = ShareId, value = #od_share{handle = HandleId} = PreexistingShareRecord}) ->
        MigratedShareRecord = ozt_shares:get(ShareId),
        ExpShareRecord = case ozt_handles:exists(utils:ensure_defined(HandleId, <<"undef">>)) of
            true -> PreexistingShareRecord;
            % share records pointing to a non-existing handle should have the field nulled
            false -> PreexistingShareRecord#od_share{handle = undefined}
        end,
        ?assertEqual(MigratedShareRecord, ExpShareRecord)
    end, PreexistingShareDocs).


upgrade_from_25_0_shares(_Config) ->
    % setting this value to 0 will force all shares to be stored in links,
    % hence simulating the approach from vsn <= 25.0
    ozt:set_env(max_inline_share_registry_size, 0),

    SpacesAndShares = lists_utils:generate(fun(_) ->
        SpaceId = ozt_spaces:create(),
        Shares = lists_utils:generate(fun(_) ->
            ozt_shares:create(SpaceId, datastore_key:new())
        end, ?RAND_ELEMENT([0, 1, 10, 50, 100, 200, 300])),
        {SpaceId, lists:sort(Shares)}
    end, 100),
    {Spaces, _} = lists:unzip(SpacesAndShares),

    lists:foreach(fun({SpaceId, SortedShares}) ->
        ?assertEqual(SortedShares, lists:sort(list_share_ids_completely(SpaceId)))
    end, SpacesAndShares),

    % simulate legacy od_space documents that did not have a properly initialized
    % inline_share_registry (should be set to "requires_reorganization" state after
    % doc upgrade on the DB level)
    lists:foreach(fun(SpaceId) ->
        ozt:rpc(od_space, update, [SpaceId, fun(SpaceRecord) ->
            {ok, SpaceRecord#od_space{
                inline_share_registry = inline_share_registry:post_upgrade_from_25_0()
            }}
        end])
    end, Spaces),

    % perform the upgrade

    MaxInlineRegSize = 100,
    ozt:set_env(max_inline_share_registry_size, MaxInlineRegSize),
    ?assertEqual({ok, 6}, ozt:rpc(node_manager_plugin, upgrade_cluster, [5])),

    lists:foreach(fun({SpaceId, SortedShares}) ->
        ShareCount = length(SortedShares),
        ?assertEqual(SortedShares, lists:sort(list_share_ids_completely(SpaceId))),
        #od_space{inline_share_registry = InlineRegistry} = ozt_spaces:get(SpaceId),
        ?assertEqual(ShareCount, inline_share_registry:get_share_count(ShareCount)),
        ?assertEqual(ShareCount =< MaxInlineRegSize, inline_share_registry:is_utilized(InlineRegistry))
    end, SpacesAndShares).

    % fixme test idempotency


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
create_legacy_share(SpaceId, without_handle) ->
    ozt_mocks:simulate_seconds_passing(?RAND_INT(3600)),
    ShareId = datastore_key:new(),
    {ok, ShareDoc} = ozt:rpc(od_share, create, [
        #document{
            key = ShareId,
            value = #od_share{
                name = ?RAND_SHARE_NAME(),
                description = ?RAND_UNICODE_STR(?RAND_INT(2, 1000)),
                root_file_uuid = datastore_key:new(),
                file_type = ?RAND_CHOICE(?REGULAR_FILE_TYPE, ?DIRECTORY_TYPE),
                space = SpaceId,
                creation_time = ozt_mocks:get_frozen_time_seconds()
            }
        }
    ]),
    {ok, _} = ozt:rpc(od_space, update, [SpaceId, fun(SpaceRecord = #od_space{shares = Shares}) ->
        {ok, SpaceRecord#od_space{shares = [ShareId | Shares]}}
    end]),
    ShareDoc;

%% @private
create_legacy_share(SpaceId, {with_handle, HServiceId}) ->
    #document{key = ShareId} = create_legacy_share(SpaceId, without_handle),
    MetadataSchema = ?RAND_ELEMENT(ozt_handles:supported_metadata_schemas()),
    Metadata = ozt_handles:example_input_metadata(MetadataSchema),
    PublicHandle = ozt:rpc(handle_proxy, register_handle, [HServiceId, <<"Share">>, ShareId, Metadata]),
    HandleId = datastore_key:new(),
    {ok, _} = ozt:rpc(od_handle, create, [#document{
        key = HandleId,
        value = #od_handle{
            handle_service = HServiceId,
            resource_type = <<"Share">>,
            resource_id = ShareId,
            public_handle = PublicHandle,
            metadata_schema = <<"legacy">>,
            metadata = Metadata,
            timestamp = ozt:timestamp_seconds()
        }
    }]),
    {ok, UpdatedShareDoc} = ozt:rpc(od_share, update, [ShareId, fun(ShareRecord) ->
        {ok, ShareRecord#od_share{handle = HandleId}}
    end]),
    UpdatedShareDoc.


%% @private
share_docs_to_exp_listing_entries(Docs) ->
    UnsortedEntries = lists:map(fun(#document{key = ShareId, value = #od_share{
        name = Name,
        space = SpaceId,
        file_type = RootFileType,
        root_file_uuid = RootFileUuid,
        handle = HandleIdFromTheDoc
    }}) ->
        % the handle may be undefined or point to a non-existing doc
        {HandleId, PublicHandle} = case ozt_handles:exists(utils:ensure_defined(HandleIdFromTheDoc, <<"undef">>)) of
            true ->
                {HandleIdFromTheDoc, (ozt_handles:get(HandleIdFromTheDoc))#od_handle.public_handle};
            false ->
                {null, null}
        end,
        #{
            <<"index">> => ozt_shares:expected_share_entry_index(ShareId, Name, HandleId),
            <<"shareId">> => ShareId,
            <<"name">> => Name,
            <<"rootFileType">> => atom_to_binary(RootFileType),
            <<"rootFilePrivateId">> => file_id:pack_guid(RootFileUuid, SpaceId),
            <<"rootFilePublicId">> => file_id:pack_share_guid(RootFileUuid, SpaceId, ShareId),
            <<"sharePublicUrl">> => api_test_expect:expected_public_share_url(ShareId),
            <<"handleId">> => HandleId,
            <<"handlePublicUrl">> => PublicHandle
        }
    end, Docs),
    lists:sort(fun(#{<<"index">> := IndexA}, #{<<"index">> := IndexB}) ->
        IndexA =< IndexB
    end, UnsortedEntries).


%% @private
list_shares_completely(SpaceId) ->
    ozt:rpc(share_registry, list_entries, [SpaceId, #{limit => infinity}]).


%% @private
list_share_ids_completely(SpaceId) ->
    ozt:rpc(share_registry, list_ids, [SpaceId, #{limit => infinity}]).


%% @private
create_legacy_handle(SpaceId, HServiceId) ->
    #document{key = ShareId} = create_legacy_share(SpaceId, without_handle),
    ozt_mocks:simulate_seconds_passing(?RAND_INT(3600)),
    {ok, Doc} = ozt:rpc(od_handle, create, [
        ozt_handles:gen_legacy_handle_doc(HServiceId, ShareId, gen_legacy_metadata())
    ]),
    Doc.


%% @private
handle_docs_to_exp_listing_entries(Docs) ->
    lists:sort(lists:map(fun(#document{
        key = HandleId,
        value = #od_handle{handle_service = HServiceId, timestamp = Timestamp}
    }) ->
        #handle_listing_entry{timestamp = Timestamp, handle_id = HandleId, service_id = HServiceId, status = present}
    end, Docs)).


%% @private
docs_to_ids(Docs) ->
    [Id || #document{key = Id} <- Docs].


%% @private
list_handles_completely(Opts) ->
    ozt:rpc(handle_registry, list_completely, [Opts]).


%% @private
gather_handles_by_all_prefixes() ->
    ozt:rpc(handle_registry, gather_by_all_schemas, []).


%% @private
%% In the previous version there was no validation, so the metadata can be invalid.
gen_legacy_metadata() ->
    case ?RAND_INT(1, 6) of
        1 ->
            <<"">>;
        2 ->
            <<"garbage-invalid-xml">>;
        3 ->
            <<"<not-a-metadata-tag></not-a-metadata-tag>">>;
        4 ->
            <<"<metadata></metadata>">>;
        _ ->
            <<
                "<?xml version=\"1.0\" encoding=\"utf-8\" ?>",
                "<metadata xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\">"
                "<dc:title>", (?RAND_XML_SAFE_UNICODE_STR())/binary, "</dc:title>",
                "<dc:creator>John Doe</dc:creator>",
                "<dc:description>", (?RAND_XML_SAFE_UNICODE_STR())/binary, "</dc:description>",
                "<dc:publisher>Onedata</dc:publisher>",
                "<dc:date>2024</dc:date>",
                "<dc:identifier>onedata:12345</dc:identifier>",
                "<dc:language>eng</dc:language>",
                "<dc:rights>CC-0</dc:rights>",
                "</metadata>"
            >>
    end.


%% @private
%% Invalid metadata should be treated the same as empty metadata.
exp_handle_metadata(H = #od_handle{metadata = <<"">>}) ->
    exp_handle_metadata(H#od_handle{metadata = <<"<metadata></metadata>">>});

exp_handle_metadata(H = #od_handle{metadata = <<"garbage-invalid-xml">>}) ->
    exp_handle_metadata(H#od_handle{metadata = <<"<metadata></metadata>">>});

exp_handle_metadata(H = #od_handle{metadata = <<"<not-a-metadata-tag></not-a-metadata-tag>">>}) ->
    exp_handle_metadata(H#od_handle{metadata = <<"<metadata></metadata>">>});

exp_handle_metadata(#od_handle{metadata = InputRawMetadata, resource_id = ResourceId, public_handle = PublicHandle}) ->
    {ok, MetadataXml} = oai_xml:parse(InputRawMetadata),
    {ok, RevisedMetadata} = ozt:rpc(oai_metadata, revise_for_publication, [
        ?OAI_DC_METADATA_PREFIX,
        MetadataXml,
        ResourceId,
        #od_share{}  % this argument can be whatever as dublin core metadata plugin ignores it
    ]),
    FinalMetadata = ozt:rpc(oai_metadata, insert_public_handle, [
        ?OAI_DC_METADATA_PREFIX,
        RevisedMetadata,
        PublicHandle
    ]),
    ozt:rpc(oai_metadata, encode_xml, [
        ?OAI_DC_METADATA_PREFIX,
        FinalMetadata
    ]).
