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
-spec handle({oai_error_code(), binary()} | oai_error_code() | term()) ->
    oai_error() | {halt, cowboy_req:req(), any()}.
%%handle({ErrorCode, Description}) -> ?OAI_ERROR(ErrorCode, Description);

handle({missing_key, <<"verb">>}) ->
    ?BAD_VERB(<<"The verb argument is missing.">>);
handle({missing_key, Keys}) ->
    KeysStr = [str_utils:to_list(K) || K <- Keys],
    ?BAD_ARGUMENT(str_utils:format_bin(
        "The request is missing required arguments: ~p.",[KeysStr]));
handle({repeated_key, <<"verb">>}) ->
    ?BAD_VERB(<<"The verb argument is repeated.">>);
handle({repeated_key, Key}) ->
    ?BAD_ARGUMENT(str_utils:format_bin(
        "The request includes repeated argument ~s.",[Key]));
handle({not_legal_verb, BadVerb}) ->
    ?BAD_VERB(str_utils:format_bin(
        "The verb argument ~s is not a legal OAI-PMH verb.",[BadVerb]));
handle({value_is_empty, Key}) ->
    ?BAD_ARGUMENT(str_utils:format_bin(
        "The request argument ~s has empty value.",[Key]));
handle({cannotDisseminateFormat, MetadataPrefix}) ->
    ?CANNOT_DISSEMINATE_FORMAT(str_utils:format_bin(
        "The metadata format identified by the value ~s"
        "given for the metadataPrefix argument is not "
        "supported by this repository.",[MetadataPrefix]));
handle(exclusive_argument) ->
    ?BAD_ARGUMENT(<<"Exclusive argument is not an only argument">>);
handle({illegal_argument, IllegalArgs}) ->
    IllegalArgsStr = [str_utils:to_list(A) || A <- IllegalArgs],
    ?BAD_ARGUMENT(str_utils:format_bin(
        "The request includes illegal arguments: ~p.", [IllegalArgsStr]));
handle({granularity_mismatch, From, Until}) ->
    ?BAD_ARGUMENT(str_utils:format_bin(
        "Datestamps from=~s and until=~s "
        "have different granularity.", [From, Until]));
handle({invalid_date_format, Date}) ->
    ?BAD_ARGUMENT(str_utils:format_bin(
        "Datestamp ~s has invalid format.", [Date]));
handle({wrong_datestamps_relation, From, Until}) ->
    ?BAD_ARGUMENT(str_utils:format_bin(
        "Datestamp from=~s is greater than until=~s: ", [From, Until]));
handle({noRecordsMatch, FromDatestamp, UntilDatestamp, MetadataPrefix}) ->
    ?NO_RECORDS_MATCH(str_utils:format_bin(
        "The combination of the values of the from= ~s, "
        "until= ~s and metadataPrefix= ~s arguments results "
        "in an empty list.", [FromDatestamp, UntilDatestamp, MetadataPrefix]));
handle({illegalId, Id}) ->
    ?ID_DOES_NOT_EXIST(str_utils:format_bin(
        "The value of the identifier argument \"~s\" "
        "is illegal in this repository. Identifier must "
        "be in form oai:onedata.org:<id>", [Id]));
handle({idDoesNotExist, Id}) ->
    ?ID_DOES_NOT_EXIST(str_utils:format_bin(
        "The value of the identifier argument \"~s\" "
        "is unknown in this repository.", [Id]));
handle({noMetadataFormats, Id}) ->
    ?NO_METADATA_FORMATS(str_utils:format_bin(
        "There are no metadata formats available for item ~s", [Id]));
handle(badVerb) -> ?BAD_VERB;
handle(badArgument) -> ?BAD_ARGUMENT;
handle(badResumptionToken) -> ?BAD_RESUMPTION_TOKEN;
handle(cannotDisseminateFormat) -> ?CANNOT_DISSEMINATE_FORMAT;
handle(idDoesNotExist) -> ?ID_DOES_NOT_EXIST;
handle(noRecordsMatch) -> ?NO_RECORDS_MATCH;
handle(noMetadataFormats) -> ?NO_METADATA_FORMATS;
handle(noSetHierarchy) -> ?NO_SET_HIERARCHY.