%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia, Lukasz Opiola
%%% @copyright (C) 2016-2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Behaviour for onezone plugins that handle specific types of metadata
%%% for Open Data handles (e.g. Dublin Core).
%%%
%%% The process of publishing Open Data from the PoV of metadata is as follows:
%%%
%%%   1) A user submits a request to create a new Open Data handle, providing
%%%      some metadata (in XML format) along with it.
%%%   2) The metadata is revised (see revise_for_publication/3):
%%%      * sanitized as far as format (XML) and schema is concerned
%%%      * modified by adding/modifying properties in an automatic way (if needed)
%%%   3) The Open Data record with revised metadata is published in a handle service,
%%%      the handle service assigns a public handle (PID, DOI) for the record.
%%%   4) The metadata is enriched with the public handle (see insert_public_handle/2)
%%%      and in this "final" version saved to Onezone's DB - whenever the record
%%%      is viewed through the UI or REST API, this version is served.
%%%   5) The Open Data record is advertised through the OAI-PMH protocol along with
%%%      the "final" metadata. OAI-PMH is based on XML; when responses are formed, the
%%%      metadata has to be slightly adopted (see adapt_for_oai_pmh/1), but this does
%%%      not impact the carried information, just the surrounding structure of the XML.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_metadata_plugin_behaviour).
-author("Jakub Kudzia").
-author("Lukasz Opiola").

-include("plugins/onezone_plugins.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").


-export([validate_example/2]).


-type validation_example() :: #handle_metadata_plugin_validation_example{}.
-export_type([validation_example/0]).


%%-------------------------------------------------------------------
%% @doc
%% Returns metadata prefix for given metadata format.
%% @end
%%-------------------------------------------------------------------
-callback metadata_prefix() -> binary().


%%-------------------------------------------------------------------
%% @doc
%% Returns URL of XML schema for given metadata format.
%% @end
%%-------------------------------------------------------------------
-callback schema_URL() -> binary().


%%-------------------------------------------------------------------
%% @doc
%% Returns main XML namespace for given metadata format.
%% @end
%%-------------------------------------------------------------------
-callback main_namespace() -> {atom(), binary()}.


%%-------------------------------------------------------------------
%% @doc
%% Sanitizes and transforms (if needed) the provided metadata for publication in
%% a handle service. This can include adding auto-generated information to the metadata.
%% If the input metadata is not suitable for publication, should return an error.
%% @end
%%-------------------------------------------------------------------
-callback revise_for_publication(od_handle:parsed_metadata(), od_share:id(), od_share:record()) ->
    {ok, od_handle:parsed_metadata()} | error.


%%-------------------------------------------------------------------
%% @doc
%% Inserts the public handle (if applicable) into the metadata content.
%% @end
%%-------------------------------------------------------------------
-callback insert_public_handle(od_handle:parsed_metadata(), od_handle:public_handle()) ->
    od_handle:parsed_metadata().


%%-------------------------------------------------------------------
%% @doc
%% Transforms (if needed) the metadata to be conformant to OAI-PMH spec.
%% @end
%%-------------------------------------------------------------------
-callback adapt_for_oai_pmh(od_handle:parsed_metadata()) ->
    od_handle:parsed_metadata().


%%-------------------------------------------------------------------
%% @doc
%% Returns validation examples that will be tested when the plugin is loaded.
%% They serve as unit tests for the plugin.
%% @end
%%-------------------------------------------------------------------
-callback validation_examples() -> [validation_example()].


%%%===================================================================
%%% API
%%%===================================================================


-spec validate_example(module(), validation_example()) -> ok | no_return().
validate_example(Module, ValidationExample) ->
    try
        validate_handle_metadata_plugin_example_unsafe(Module, ValidationExample)
    catch Class:Reason:Stacktrace ->
        ?error_exception("Validation of an example for ~s crashed", [Module], Class, Reason, Stacktrace),
        throw(validation_failed)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec validate_handle_metadata_plugin_example_unsafe(module(), validation_example()) -> ok | no_return().
validate_handle_metadata_plugin_example_unsafe(Module, #handle_metadata_plugin_validation_example{
    input_raw_xml = InputRawXml,
    input_qualifies_for_publication = InputQualifiesForPublication,
    exp_revised_metadata_generator = ExpRevisedMetadataGenerator,
    exp_final_metadata_generator = ExpFinalMetadataGenerator,
    exp_oai_pmh_metadata_generator = ExpOaiPmhMetadataGenerator
}) ->
    DummyShareId = datastore_key:new(),
    DummyShareRecord = #od_share{
        name = str_utils:rand_hex(5),
        description = str_utils:rand_hex(50),
        space = datastore_key:new(),
        handle = datastore_key:new(),
        root_file = ?check(file_id:guid_to_objectid(file_id:pack_guid(datastore_key:new(), datastore_key:new()))),
        file_type = case rand:uniform(2) of 1 -> file; 2 -> dir end,
        creation_time = global_clock:timestamp_seconds(),
        creator = ?SUB(user, datastore_key:new())
    },
    DummyPublicHandle = str_utils:format_bin("http://hdl.handle.net/~s/~s", [datastore_key:new(), datastore_key:new()]),

    {ok, ParsedMetadata} = oai_metadata:parse_xml(InputRawXml),

    case Module:revise_for_publication(ParsedMetadata, DummyShareId, DummyShareRecord) of
        error when InputQualifiesForPublication == false ->
            ok;

        {ok, RevisedMetadata} when InputQualifiesForPublication == true ->
            ExpRevisedMetadata = ExpRevisedMetadataGenerator(DummyShareId, DummyShareRecord),
            assert_expectation_equal_to_result(
                revised, InputRawXml, oai_utils:encode_xml(RevisedMetadata), ExpRevisedMetadata
            ),

            FinalMetadata = Module:insert_public_handle(RevisedMetadata, DummyPublicHandle),
            ExpFinalMetadata = ExpFinalMetadataGenerator(DummyShareId, DummyShareRecord, DummyPublicHandle),
            assert_expectation_equal_to_result(
                final, InputRawXml, oai_utils:encode_xml(FinalMetadata), ExpFinalMetadata
            ),

            OaiPmhMetadata = Module:adapt_for_oai_pmh(FinalMetadata),
            ExpOaiPmhMetadata = ExpOaiPmhMetadataGenerator(DummyShareId, DummyShareRecord, DummyPublicHandle),
            assert_expectation_equal_to_result(
                final, InputRawXml, oai_utils:encode_xml(OaiPmhMetadata), ExpOaiPmhMetadata
            );

        RevisionResult ->
            ?error("Unmet expectation~ts", [?autoformat(InputQualifiesForPublication, RevisionResult)]),
            error(unmet_expectation)
    end.


%% @private
-spec assert_expectation_equal_to_result(revised | final, binary(), binary(), binary()) -> ok | no_return().
assert_expectation_equal_to_result(_MetadataType, _RawInput, Identical, Identical) ->
    ok;
assert_expectation_equal_to_result(MetadataType, RawInput, Got, Exp) ->
    ?error("Unmet expectation: obtained ~s metadata different than expected", [MetadataType]),
    ?error("~n  RawInput: ~ts", [RawInput]),
    ?error("~n  Got:      ~ts", [Got]),
    ?error("~n  Expected: ~ts", [Exp]),
    error(unmet_expectation).
