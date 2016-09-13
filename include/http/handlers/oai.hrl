%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-author("Jakub Kudzia").


-ifndef(OAI_HRL).
-define(OAI_HRL, 1).
-include_lib("xmerl/include/xmerl.hrl").

%%TODO write docs
%%TODO ??? macro names with/without"OAI" prefix

-record(oai_header, {
    identifier :: oai_id(),
    datestamp,
    setSpec :: list(),
    status :: undefined | deleted
}).

-record(oai_metadata_format, {
    metadataPrefix :: atom(),
    schema,
    metadataNamespace
}).

-record(oai_metadata, {
    metadata_format :: oai_metadata_format(),
    value :: #{} | #xmlElement{}
}).


-record(oai_about, {

}).

-record(oai_record, {
    header :: oai_header(),
    metadata :: oai_metadata(),
    about :: oai_about()
}).

-record(oai_error, {
    code :: badArgument | badResumptionToken | badVerb | cannotDisseminateFormat |
            idDoesNotExist | noRecordsMatch | noMetadataFormats | noSetHierarchy,
    description :: list()
}).

-type oai_verb() :: identify | getRecord | listIdentifiers |
                    listMedatadaFormats | listRecords | listSets.
-type oai_id() :: binary(). % todo maybe it should be record
-type oai_header() :: #oai_header{}.
-type oai_metadata_format() :: #oai_metadata_format{}.
-type oai_metadata() :: #oai_metadata{}.
-type oai_about() :: #oai_about{}.
-type oai_record() :: #oai_record{}.
-type oai_error() :: #oai_error{}.


-define(OAI_XML_NAMESPACE, #xmlAttribute{
        name=xmlns,
        value= "http://www.openarchives.org/OAI/2.0/"}).

-define(OAI_XML_SCHEMA_NAMESPACE, #xmlAttribute{
    name='xmlns:xsi',
    value= "http://www.w3.org/2001/XMLSchema-instance"}).

-define(OAI_XSI_SCHEMA_LOCATION, #xmlAttribute{
    name='xsi:schemaLocation',
    value = "http://www.openarchives.org/OAI/2.0/ http://www.openarchives.org/OAI/2.0/OAI-PMH.xsd"}).


-define(ROOT_ELEMENT, #xmlElement{
    name = 'OAI-PMH',
    attributes = [
        ?OAI_XML_NAMESPACE,
        ?OAI_XML_SCHEMA_NAMESPACE,
        ?OAI_XSI_SCHEMA_LOCATION]
}).


-define(ALLOWED_METHODS, [<<"GET">>, <<"POST">>]).

-define(REQUEST_CONTENT_TYPE, <<"application/x-www-form-urlencoded">>).

-define(RESPONSE_CONTENT_TYPE, <<"text/xml">>).

-define(PROTOCOL_VERSION, <<"2.0">>).

-endif.