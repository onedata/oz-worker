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
    oai_error() | {stop, cowboy_req:req(), any()}.
handle({missing_key, <<"verb">>}) ->
    ?BAD_VERB(<<"The verb argument is missing.">>);
handle({missing_key, Keys}) ->
    KeysStr = [str_utils:to_list(K) || K <- Keys],
    ?BAD_ARGUMENT(str_utils:format_bin(
        "The request is missing required arguments: ~tp.",[KeysStr]));
handle({repeated_key, <<"verb">>}) ->
    ?BAD_VERB(<<"The verb argument is repeated.">>);
handle({repeated_key, Key}) ->
    ?BAD_ARGUMENT(str_utils:format_bin(
        "The request includes repeated argument ~ts.",[Key]));
handle({not_legal_verb, BadVerb}) ->
    ?BAD_VERB(str_utils:format_bin(
        "The verb argument ~ts is not a legal OAI-PMH verb.",[BadVerb]));
handle({value_is_empty, Key}) ->
    ?BAD_ARGUMENT(str_utils:format_bin(
        "The request argument ~ts has empty value.",[Key]));
handle({cannotDisseminateFormat, MetadataPrefix}) ->
    ?CANNOT_DISSEMINATE_FORMAT(str_utils:format_bin(
        "The metadata format identified by the value ~ts"
        "given for the metadataPrefix argument is not "
        "supported by this repository.",[MetadataPrefix]));
handle(exclusive_argument) ->
    ?BAD_ARGUMENT(<<"Exclusive argument is not an only argument">>);
handle({illegal_argument, IllegalArgs}) ->
    IllegalArgsStr = [str_utils:to_list(A) || A <- IllegalArgs],
    ?BAD_ARGUMENT(str_utils:format_bin(
        "The request includes illegal arguments: ~tp.", [IllegalArgsStr]));
handle({granularity_mismatch, From, Until}) ->
    ?BAD_ARGUMENT(str_utils:format_bin(
        "Datestamps from=~ts and until=~ts "
        "have different granularity.", [From, Until]));
handle({invalid_date_format, Date}) ->
    ?BAD_ARGUMENT(str_utils:format_bin(
        "Datestamp ~ts has invalid format.", [Date]));
handle({wrong_datestamps_relation, From, Until}) ->
    ?BAD_ARGUMENT(str_utils:format_bin(
        "Datestamp from=~ts is greater than until=~ts: ", [From, Until]));
handle({noRecordsMatch, FromDatestamp, UntilDatestamp, SetSpec, MetadataPrefix}) ->
    ?NO_RECORDS_MATCH(str_utils:format_bin(
        "The combination of the values of the from=~ts, "
        "until=~ts, set=~ts and metadataPrefix=~ts arguments results "
        "in an empty list.", [FromDatestamp, UntilDatestamp, SetSpec, MetadataPrefix]));
handle({illegalId, Id}) ->
    ?ID_DOES_NOT_EXIST(str_utils:format_bin(
        "The value of the identifier argument \"~ts\" "
        "is illegal in this repository. Identifier must "
        "be in form oai:~ts:<id>", [Id, oz_worker:get_domain()]));
handle({idDoesNotExist, Id}) ->
    ?ID_DOES_NOT_EXIST(str_utils:format_bin(
        "The value of the identifier argument \"~ts\" "
        "is unknown in this repository.", [Id]));
handle(badVerb) -> ?BAD_VERB;
handle(badArgument) -> ?BAD_ARGUMENT;
handle(badResumptionToken) -> ?BAD_RESUMPTION_TOKEN;
handle(cannotDisseminateFormat) -> ?CANNOT_DISSEMINATE_FORMAT;
handle(idDoesNotExist) -> ?ID_DOES_NOT_EXIST;
handle(noRecordsMatch) -> ?NO_RECORDS_MATCH;
handle(noSetHierarchy) -> ?NO_SET_HIERARCHY.