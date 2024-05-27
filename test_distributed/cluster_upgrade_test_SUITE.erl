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
    upgrade_from_21_02_4_handles/1
]).

all() -> ?ALL([
    upgrade_from_21_02_4_handles
]).


-define(RAND_SHARE_ID(), datastore_key:new()).
-define(RAND_XML_SAFE_UNICODE_STR(), lists:foldl(fun(Char, Acc) ->
    binary:replace(Acc, <<Char>>, <<"">>)
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
    oz_test_utils:delete_all_entities(Config),
    % if the suite fails midway, there may be some remnants in the handle registry - clean it up
    % (delete_all_entities won't do it because some of the handles may still have the "legacy" metadata prefix set)
    lists:foreach(fun(#handle_listing_entry{timestamp = Timestamp, handle_id = HandleId, service_id = HServiceId}) ->
        ozt:rpc(handles, report_deleted, [?OAI_DC_METADATA_PREFIX, HServiceId, HandleId, Timestamp])
    end, list_completely(#{metadata_prefix => ?OAI_DC_METADATA_PREFIX})),
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

    ?assertEqual([], list_completely(#{metadata_prefix => ?OAI_DC_METADATA_PREFIX})),
    ?assertEqual([], gather_by_all_prefixes()),

    PreexistingHandleDocs = lists:sort(lists:flatmap(fun(HServiceId) ->
        HandleDocsForHService = lists_utils:generate(fun() ->
            #document{
                key = HandleId,
                value = #od_handle{
                    timestamp = Timestamp
                }
            } = Doc = create_legacy_handle(HServiceId),
            % simulate a case when some handles have already been upgraded
            % (during a previous upgrade run that could have crashed or been interrupted)
            case ?RAND_INT(1, 5) of
                1 ->
                    % the handle was fully migrated
                    ?assertMatch(ok, ozt:rpc(od_handle, migrate_legacy_handle, [HServiceId, HandleId]));
                2 ->
                    % the handle was partially migrated
                    ?assertMatch(ok, ozt:rpc(handles, report_created, [
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
        list_completely(#{metadata_prefix => ?OAI_DC_METADATA_PREFIX})
    ),
    ?assertEqual(
        handle_docs_to_exp_listing_entries(PreexistingHandleDocs),
        gather_by_all_prefixes()
    ),

    lists:foreach(fun(#document{key = HandleId, value = PreexistingHandleRecord}) ->
        MigratedHandleRecord = ozt_handles:get(HandleId),
        ?assertEqual(MigratedHandleRecord#od_handle.public_handle, PreexistingHandleRecord#od_handle.public_handle),
        ?assertEqual(MigratedHandleRecord#od_handle.resource_type, PreexistingHandleRecord#od_handle.resource_type),
        ?assertEqual(MigratedHandleRecord#od_handle.metadata_prefix, ?OAI_DC_METADATA_PREFIX),
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
    end, PreexistingHandleDocs).


%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
create_legacy_handle(HServiceId) ->
    ozt_mocks:simulate_seconds_passing(?RAND_INT(3600)),
    {ok, Doc} = ozt:rpc(od_handle, create, [
        ozt_handles:gen_legacy_handle_doc(HServiceId, ?RAND_SHARE_ID(), gen_legacy_metadata())
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
list_completely(Opts) ->
    ozt:rpc(handles, list_completely, [Opts]).


%% @private
gather_by_all_prefixes() ->
    ozt:rpc(handles, gather_by_all_prefixes, []).


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
