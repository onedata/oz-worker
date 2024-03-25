%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This file contains types, records and macros definitions used
%%% across modules handling OAI-PMH protocol.
%%% @end
%%%-------------------------------------------------------------------
-author("Jakub Kudzia").

-ifndef(OAI_HRL).
-define(OAI_HRL, 1).

-include("plugins/onezone_plugins.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").


-record(oai_header, {
    identifier :: oai_id(),
    datestamp :: binary(),
    set_spec :: oai_set_spec()
}).

-record(oai_metadata_format, {
    metadataPrefix :: binary() | undefined,
    schema :: binary() | undefined,
    metadataNamespace :: binary() | undefined
}).

-record(oai_metadata, {
    metadata_prefix :: od_handle:metadata_prefix(),
    raw_value :: od_handle:raw_metadata(),
    handle :: od_handle:record()   % @TODO VFS-11365 Temporary workaround, rework
}).

-record(oai_about, {
    value :: #xmlElement{}
}).
%%% TODO VFS-7454: record can have additional attribute "about" to hold data about the
%%% metadata part of the record, this field is supported by oai_utils:to_xml
%%% function, but we don't keep suitable information in handle

-record(oai_record, {
    header :: oai_header(),
    metadata :: oai_metadata(),
    about :: oai_about() | undefined
}).

% NOTE: handles in Onedata are grouped into sets, where every handle service is a separate set.
-record(oai_set, {
    set_spec :: oai_set_spec(),
    set_name :: binary()
}).

-record(oai_error, {
    code :: oai_error_code(),
    description :: binary()
}).

%% @formatter:off
-type oai_verb_module() :: identify | get_record | list_identifiers |
                           list_medatada_formats | list_records | list_sets.

-type oai_error_code() :: badArgument | badResumptionToken | badVerb |
                          cannotDisseminateFormat |idDoesNotExist |
                          noRecordsMatch | noMetadataFormats | noSetHierarchy.

-type oai_id() :: binary().
-type oai_header() :: #oai_header{}.
-type oai_metadata_format() :: #oai_metadata_format{}.
-type oai_metadata() :: #oai_metadata{}.
-type oai_about() :: #oai_about{}.
-type oai_record() :: #oai_record{}.
-type oai_set() :: #oai_set{}.
-type oai_set_spec() :: od_handle_service:id().
-type oai_error() :: #oai_error{}.
-type oai_response() :: binary() | [binary()] |
                        oai_record() | [oai_record()] |
                        [oai_set()] |
                        oai_header() | [oai_header()] |
                        oai_metadata_format() | [oai_metadata_format()].
%% @formatter:on

-type maybe_invalid_date() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type maybe_invalid_time() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type maybe_invalid_datetime() :: {maybe_invalid_date(), maybe_invalid_time()}.
-type maybe_invalid_datestamp() :: supported_datestamp() | maybe_invalid_date() | maybe_invalid_datetime().
-type supported_datestamp() :: calendar:datetime() | calendar:date() | undefined.
-type oai_date_granularity() :: day_granularity | seconds_granularity.


-define(OAI_DC_METADATA_PREFIX, <<"oai_dc">>).
-define(EDM_METADATA_PREFIX, <<"edm">>).


-define(OAI_XML_NAMESPACE, #xmlAttribute{
    name = xmlns,
    value = "http://www.openarchives.org/OAI/2.0/"}).

-define(OAI_XML_SCHEMA_NAMESPACE, #xmlAttribute{
    name = 'xmlns:xsi',
    value = "http://www.w3.org/2001/XMLSchema-instance"}).

-define(OAI_XSI_SCHEMA_LOCATION, #xmlAttribute{
    name = 'xsi:schemaLocation',
    value = "http://www.openarchives.org/OAI/2.0/ http://www.openarchives.org/OAI/2.0/OAI-PMH.xsd"}).


-define(ROOT_ELEMENT, #xmlElement{
    name = 'OAI-PMH',
    attributes = [
        ?OAI_XML_NAMESPACE,
        ?OAI_XML_SCHEMA_NAMESPACE,
        ?OAI_XSI_SCHEMA_LOCATION
    ]
}).


-define(XML_RESPONSE_CONTENT_TYPE, <<"text/xml">>).

-define(PROTOCOL_VERSION, <<"2.0">>).

-define(OAI_IDENTIFIER_PREFIX, <<"oai:", (oz_worker:get_domain())/binary, ":">>).


-endif.