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

-include("oai.hrl").

-define(OAI_ERROR(Code, Description), #oai_error{
    code=Code,
    description=str_utils:to_list(Description)
}).

-define(BAD_ARGUMENT, ?BAD_ARGUMENT(
    "The request includes illegal arguments, "
    "is missing required arguments, includes "
    "a repeated argument, or values for "
    "arguments have an illegal syntax.")).
-define(BAD_ARGUMENT(Description),
    ?OAI_ERROR(badArgument, Description)).

-define(BAD_RESUMPTION_TOKEN, ?BAD_RESUMPTION_TOKEN(
    "The value of the resumptionToken argument is invalid or expired.")).
-define(BAD_RESUMPTION_TOKEN(Description),
    ?OAI_ERROR(badResumptionToken, Description)).

-define(BAD_VERB, ?BAD_VERB(
    "Value of the verb argument is not a legal "
    "OAI-PMH verb, the verb argument is missing, "
    "or the verb argument is repeated.")).
-define(BAD_VERB(Description),
    ?OAI_ERROR(badVerb, Description)).

-define(CANNOT_DISSEMINATE_FORMAT, ?CANNOT_DISSEMINATE_FORMAT(
    "The metadata format identified by the value "
    "given for the metadataPrefix argument is not "
    "supported by the item or by the repository.")).

-define(CANNOT_DISSEMINATE_FORMAT(Description),
    ?OAI_ERROR(cannotDisseminateFormat, Description)).

-define(ID_DOES_NOT_EXIST, ?ID_DOES_NOT_EXIST(
    "The value of the identifier argument is unknown "
    "or illegal in this repository.")).

-define(ID_DOES_NOT_EXIST(Description),
    ?OAI_ERROR(idDoesNotExist, Description)).

-define(NO_RECORDS_MATCH, ?NO_RECORDS_MATCH(
    "The combination of the values of the from, "
    "until, set and metadataPrefix arguments results "
    "in an empty list.")).
-define(NO_RECORDS_MATCH(Description),
    ?OAI_ERROR(noRecordsMatch, Description)).

-define(NO_METADATA_FORMATS, ?NO_METADATA_FORMATS(
    "There are no metadata formats available for the specified item.")).
-define(NO_METADATA_FORMATS(Description),
    ?OAI_ERROR(noMetadataFormats, Description)).

-define(NO_SET_HIERARCHY, ?NO_SET_HIERARCHY(
    "The repository does not support sets.")).
-define(NO_SET_HIERARCHY(Description),
    ?OAI_ERROR(noSetHierarchy, Description)).