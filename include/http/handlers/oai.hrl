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

%%TODO write docs
%%TODO ??? macro names with/without"OAI" prefix 


-record(oai_header, {
    id :: oai_id(),
    datestamp,
    setSpec :: list(),
    status :: undefined | deleted
}).

-record(oai_metadata, {

}).

-record(oai_about, {

}).

-record(oai_record, {
    header :: oai_header(),
    metadata :: oai_metadata(),
    about :: oai_about()
}).


-type oai_verb() :: identify | getRecord | listIdentifiers |
                    listMedatadaFormats | listRecords | listSets.
-type oai_id() :: binary(). % todo maybe it should be record
-type oai_header() :: #oai_header{}.
-type oai_metadata() :: #oai_metadata{}.
-type oai_about() :: #oai_about{}.
-type oai_record() :: #oai_record{}.
-type oai_granularity() :: 'YYYY-MM-DD' | 'YYYY-MM-DDThh:mm:ssZ' .


-define(ALLOWED_METHODS, [<<"GET">>, <<"POST">>]).

-define(REQUEST_CONTENT_TYPE, <<"application/x-www-form-urlencoded">>).

-define(RESPONSE_CONTENT_TYPE, <<"text/xml">>).

-define(SUPPORTED_VERSION, <<"2.0">>).
