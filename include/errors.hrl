%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Errors common for all entity logic modules.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERRORS_HRL).
-define(ERRORS_HRL, 1).

% General errors
-define(ERROR_INTERNAL_SERVER_ERROR, {error, internal_server_error}).
-define(ERROR_NOT_FOUND, {error, not_found}).
-define(ERROR_UNAUTHORIZED, {error, unauthorized}).
-define(ERROR_FORBIDDEN, {error, forbidden}).

% Errors connected with bad data
-define(ERROR_MALFORMED_DATA, {error, bad_data}).
-define(ERROR_MISSING_REQUIRED_DATA(__Key),
    {error, {missing_required_data, __Key}}
).
-define(ERROR_MISSING_AT_LEAST_ONE_DATA(__Keys),
    {error, {missing_at_least_one_data, __Keys}}
).
-define(ERROR_BAD_DATA(__Key), {error, {bad_data, __Key}}).
-define(ERROR_EMPTY_DATA(__Key), {error, {empty_data, __Key}}).
-define(ERROR_ID_NOT_FOUND(__Key), {error, {id_not_found, __Key}}).
-define(ERROR_ID_OCCUPIED(__Key), {error, {id_occupied, __Key}}).
-define(ERROR_BAD_TOKEN(__Key), {error, {bad_token, __Key}}).
-define(ERROR_BAD_TOKEN_TYPE(__Key), {error, {bad_token_type, __Key}}).
-define(ERROR_VALUE_NOT_POSITIVE(__Key), {error, {value_not_positive, __Key}}).
-define(ERROR_VALUE_TOO_LOW(__Key, __Threshold),
    {error, {value_too_low, __Key, __Threshold}}
).
-define(ERROR_VALUE_TOO_HIGH(__Key, __Threshold),
    {error, {value_too_high, __Key, __Threshold}}
).
-define(ERROR_VALUE_NOT_BETWEEN(__Key, __Low, __High),
    {error, {value_not_between, __Key, __Low, __High}}
).
% Errors connected with relations between entities
-define(ERROR_RELATION_DOES_NOT_EXIST(__ChModel, __ChId, __ParModel, __ParId),
    {error, {relation_does_not_exist, __ChModel, __ChId, __ParModel, __ParId}}
).
-define(ERROR_RELATION_ALREADY_EXISTS(__ChModel, __ChId, __ParModel, __ParId),
    {error, {relation_already_exists, __ChModel, __ChId, __ParModel, __ParId}}
).


-endif.
