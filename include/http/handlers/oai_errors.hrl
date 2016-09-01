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

-define(BAD_ARGUMENT, #oai_error{
    code=badArgument,
    description = "The request includes illegal arguments, is missing required arguments, includes a repeated argument, or values for arguments have an illegal syntax."
}).

-define(BAD_RESUMPTION_TOKEN, #oai_error{
    code=badResumptionToken,
    description = "The value of the resumptionToken argument is invalid or expired."
}).

-define(BAD_VERB, #oai_error{
    code=badVerb,
    description = "Value of the verb argument is not a legal OAI-PMH verb, the verb argument is missing, or the verb argument is repeated."
}).

-define(CANNOT_DISSEMINATE_FORMAT, #oai_error{
    code=cannotDisseminateFormat,
    description = "The metadata format identified by the value given for the metadataPrefix argument is not supported by the item or by the repository."
}).

-define(ID_DOES_NOT_EXIST, #oai_error{
    code=idDoesNotExist,
    description = "The value of the identifier argument is unknown or illegal in this repository."
}).

-define(NO_RECORDS_MATCH, #oai_error{
    code=noRecordsMatch,
    description = "The combination of the values of the from, until, set and metadataPrefix arguments results in an empty list."
}).

-define(NO_METADATA_FORMATS, #oai_error{
    code=noMetadataFormats,
    description = "There are no metadata formats available for the specified item."
}).

-define(NO_SET_HIERARCHY, #oai_error{
    code=noSetHierarchy,
    description = "The repository does not support sets."
}).