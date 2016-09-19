%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This module is responsible for handling OAI-PMH errors and exceptions.
%%% http://www.openarchives.org/OAI/2.0/openarchivesprotocol.htm#ErrorConditions
%%% @end
%%%-------------------------------------------------------------------
-module(oai_errors).
-author("Jakub Kudzia").

-include("http/handlers/oai_errors.hrl").

%% API
-export([handle/1]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Function responsible for handling OAI-PMH errors.
%% @end
%%--------------------------------------------------------------------
-spec handle({oai_error_code(), list()} | oai_error_code()) -> oai_error().
handle({ErrorCode, Description}) -> ?OAI_ERROR(ErrorCode, Description);
handle(badVerb) -> ?BAD_VERB;
handle(badArgument) -> ?BAD_ARGUMENT;
handle(badResumptionToken) -> ?BAD_RESUMPTION_TOKEN;
handle(cannotDisseminateFormat) -> ?CANNOT_DISSEMINATE_FORMAT;
handle(idDoesNotExist) -> ?ID_DOES_NOT_EXIST;
handle(noRecordsMatch) -> ?NO_RECORDS_MATCH;
handle(noMetadataFormats) -> ?NO_METADATA_FORMATS;
handle(noSetHierarchy) -> ?NO_SET_HIERARCHY.
