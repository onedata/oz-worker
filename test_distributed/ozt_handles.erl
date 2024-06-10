%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating handles of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_handles).
-author("Lukasz Opiola").

-include("ozt.hrl").
-include("plugins/onezone_plugins.hrl").

%% API
-export([create/1, create/2, create/4, get/1, exists/1, list/0]).
-export([supported_metadata_prefixes/0]).
-export([example_input_metadata/1, example_input_metadata/2]).
-export([expected_final_metadata/1, expected_final_metadata/2]).
-export([gen_legacy_handle_doc/3]).

-compile({no_auto_import, [get/1]}).


%%%===================================================================
%%% API
%%%===================================================================

-spec create(od_handle_service:id()) -> od_handle:id().
create(HandleServiceId) ->
    ShareId = ozt_spaces:create_share(ozt_spaces:create()),
    create(HandleServiceId, ShareId).

-spec create(od_handle_service:id(), od_share:id()) -> od_handle:id().
create(HandleServiceId, ShareId) ->
    MetadataPrefix = ?RAND_ELEMENT(supported_metadata_prefixes()),
    RawMetadata = example_input_metadata(MetadataPrefix),
    create(HandleServiceId, ShareId, MetadataPrefix, RawMetadata).

-spec create(od_handle_service:id(), od_share:id(), od_handle:metadata_prefix(), od_handle:raw_metadata()) ->
    od_handle:id().
create(HandleServiceId, ShareId, MetadataPrefix, RawMetadata) ->
    {ok, HandleId} = ?assertMatch({ok, _}, ozt:rpc(handle_logic, create, [?ROOT, #{
        <<"handleServiceId">> => HandleServiceId,
        <<"resourceType">> => <<"Share">>,
        <<"resourceId">> => ShareId,
        <<"metadataPrefix">> => MetadataPrefix,
        <<"metadata">> => RawMetadata
    }])),
    HandleId.


-spec get(od_handle:id()) -> od_handle:record().
get(HandleId) ->
    {ok, HandleRecord} = ?assertMatch({ok, _}, ozt:rpc(handle_logic, get, [?ROOT, HandleId])),
    HandleRecord.


-spec exists(od_handle:id()) -> boolean().
exists(HandleId) ->
    ozt:rpc(handle_logic, exists, [HandleId]).


-spec list() -> [od_handle:id()].
list() ->
    {ok, HandleIds} = ?assertMatch({ok, _}, ozt:rpc(handle_logic, list, [?ROOT])),
    HandleIds.


-spec supported_metadata_prefixes() -> [od_handle:metadata_prefix()].
supported_metadata_prefixes() ->
    ozt:rpc(oai_metadata, supported_formats, []).


-spec example_input_metadata(od_handle:metadata_prefix()) -> od_handle:raw_metadata().
example_input_metadata(MetadataPrefix) ->
    example_input_metadata(MetadataPrefix, 1).

-spec example_input_metadata(od_handle:metadata_prefix(), pos_integer()) -> od_handle:raw_metadata().
example_input_metadata(MetadataPrefix, ExampleNumber) ->
    ValidationExample = acquire_constant_validation_example(MetadataPrefix, ExampleNumber),
    ValidationExample#handle_metadata_plugin_validation_example.input_raw_xml.


-spec expected_final_metadata(od_handle:record() | od_handle:id()) ->
    od_handle:raw_metadata().
expected_final_metadata(HandleOrId) ->
    expected_final_metadata(HandleOrId, 1).

-spec expected_final_metadata(od_handle:record() | od_handle:id(), pos_integer()) ->
    od_handle:raw_metadata().
expected_final_metadata(HandleId, ExampleNumber) when is_binary(HandleId) ->
    expected_final_metadata(get(HandleId), ExampleNumber);
expected_final_metadata(#od_handle{
    resource_id = ShareId,
    metadata_prefix = MetadataPrefix,
    public_handle = PublicHandle
}, ExampleNumber) ->
    ShareRecord = ?check(ozt:rpc(share_logic, get, [?ROOT, ShareId])),
    ValidationExample = acquire_constant_validation_example(MetadataPrefix, ExampleNumber),
    % this must be evaluated on a onezone node as this is where the plugin is loaded
    GenExpFinalMetadata = fun() ->
        ExpFinalMetadataGenerator = ValidationExample#handle_metadata_plugin_validation_example.exp_final_metadata_generator,
        ExpFinalMetadataGenerator(ShareId, ShareRecord, PublicHandle)
    end,
    ozt:rpc(erlang, apply, [GenExpFinalMetadata, []]).


% NOTE: requires a mocked handle proxy
-spec gen_legacy_handle_doc(od_handle_service:id(), od_share:id(), od_handle:raw_metadata()) ->
    datastore_doc:doc(od_handle:record()).
gen_legacy_handle_doc(HServiceId, ShareId, Metadata) ->
    {ok, PublicHandle} = ozt:rpc(handle_proxy, register_handle, [HServiceId, <<"Share">>, ShareId, Metadata]),
    #document{
        key = datastore_key:new(),
        value = #od_handle{
            handle_service = HServiceId,
            resource_type = <<"Share">>,
            resource_id = ShareId,
            public_handle = PublicHandle,
            metadata_prefix = <<"legacy">>,
            metadata = Metadata,
            timestamp = ozt:timestamp_seconds()
        }
    }.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec acquire_constant_validation_example(od_handle:metadata_prefix(), pos_integer()) ->
    handle_metadata_plugin_behaviour:validation_example().
acquire_constant_validation_example(MetadataPrefix, ExampleNumber) ->
    {ok, ValidationExample} = node_cache:acquire({?MODULE, ?FUNCTION_NAME, MetadataPrefix, ExampleNumber}, fun() ->
        AllExamples = ozt:rpc(oai_metadata, validation_examples, [MetadataPrefix]),
        ValidExamples = lists:filter(fun(Example) ->
            Example#handle_metadata_plugin_validation_example.input_qualifies_for_publication
        end, AllExamples),
        {ok, lists:nth(ExampleNumber, lists_utils:ensure_length(ExampleNumber, ValidExamples)), infinity}
    end),
    ValidationExample.
