%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This module is responsible for handling OAI-PMH "GetRecord" request.
%%% http://www.openarchives.org/OAI/2.0/openarchivesprotocol.htm#GetRecord
%%% @end
%%%-------------------------------------------------------------------
-module(get_record).
-author("Jakub Kudzia").

-include("http/public_data/oai.hrl").
-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models.hrl").

-behaviour(oai_verb_behaviour).

%% API
-export([required_arguments/0, optional_arguments/0, exclusive_arguments/0,
    required_response_elements/0, optional_response_elements/0, get_response/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback required_arguments/0
%%% @end
%%%-------------------------------------------------------------------
-spec required_arguments() -> [binary()].
required_arguments() -> [<<"identifier">>, <<"metadataPrefix">>].


%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback optional_arguments/0
%%% @end
%%%-------------------------------------------------------------------
-spec optional_arguments() -> [binary()].
optional_arguments() -> [].


%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback exclusive_arguments/0
%%% @end
%%%--------------------------------------------------------------------
-spec exclusive_arguments() -> [binary()].
exclusive_arguments() -> [].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback required_response_elements/0
%%% @end
%%%-------------------------------------------------------------------
-spec required_response_elements() -> [binary()].
required_response_elements() -> [<<"record">>].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback optional_response_elements/0
%%% @end
%%%-------------------------------------------------------------------
-spec optional_response_elements() -> [binary()].
optional_response_elements() -> [].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback get_response/2
%%% @end
%%%-------------------------------------------------------------------
-spec get_response(binary(), [proplists:property()]) -> oai_response().
get_response(<<"record">>, Args) ->
    OaiId = proplists:get_value(<<"identifier">>, Args),
    MetadataPrefix = proplists:get_value(<<"metadataPrefix">>, Args),
    HandleId = oai_utils:oai_identifier_decode(OaiId),
    lists:member(MetadataPrefix, oai_metadata:all_supported_oai_pmh_prefixes()) orelse
        throw({cannotDisseminateFormat, MetadataPrefix}),
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle}} ->
            assert_can_disseminate_format(Handle#od_handle.metadata_schema, MetadataPrefix),
            oai_utils:build_oai_record(
                MetadataPrefix,
                #handle_listing_entry{
                    timestamp = Handle#od_handle.timestamp,
                    handle_id = HandleId,
                    service_id = Handle#od_handle.handle_service,
                    status = present
                },
                Handle
            );
        {error, not_found} ->
            case handle_registry:lookup_deleted(HandleId) of
                {ok, {HandleMetadataSchema, HandleListingEntry}} ->
                    assert_can_disseminate_format(HandleMetadataSchema, MetadataPrefix),
                    oai_utils:build_oai_record(MetadataPrefix, HandleListingEntry);
                error ->
                    throw({idDoesNotExist, OaiId})
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec assert_can_disseminate_format(od_handle:metadata_schema(), oai_metadata:prefix()) -> ok | no_return().
assert_can_disseminate_format(HandleMetadataSchema, MetadataPrefix) ->
    SupportedPrefixesForHandle = oai_metadata:supported_oai_pmh_prefixes_by_schema(HandleMetadataSchema),
    lists:member(MetadataPrefix, SupportedPrefixesForHandle) orelse throw({cannotDisseminateFormat, MetadataPrefix}),
    ok.
