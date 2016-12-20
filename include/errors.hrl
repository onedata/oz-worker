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
-define(ERROR_MISSING_REQUIRED_VALUE(__Key),
    {error, {missing_required_data, __Key}}
).
-define(ERROR_MISSING_AT_LEAST_ONE_VALUE(__Keys),
    {error, {missing_at_least_one_data, __Keys}}
).
-define(ERROR_BAD_DATA(__Key), {error, {bad_data, __Key}}).
-define(ERROR_BAD_VALUE_EMPTY(__Key), {error, {empty_value, __Key}}).
-define(ERROR_BAD_VALUE_ATOM(__Key), {error, {bad_value_atom, __Key}}).
-define(ERROR_BAD_VALUE_LIST_OF_ATOMS(__Key),
    {error, {bad_value_list_of_atoms, __Key}}
).
-define(ERROR_BAD_VALUE_BINARY(__Key), {error, {bad_value_binary, __Key}}).
-define(ERROR_BAD_VALUE_LIST_OF_BINARIES(__Key),
    {error, {bad_value_list_of_binaries, __Key}}
).
-define(ERROR_BAD_VALUE_INTEGER(__Key), {error, {bad_value_integer, __Key}}).
-define(ERROR_BAD_VALUE_FLOAT(__Key), {error, {bad_value_float, __Key}}).
-define(ERROR_BAD_VALUE_JSON(__Key), {error, {bad_value_json, __Key}}).
-define(ERROR_BAD_VALUE_TOKEN(__Key), {error, {bad_value_token, __Key}}).
-define(ERROR_BAD_VALUE_TOO_LOW(__Key, __Threshold),
    {error, {value_too_low, __Key, __Threshold}}
).
-define(ERROR_BAD_VALUE_TOO_HIGH(__Key, __Threshold),
    {error, {value_too_high, __Key, __Threshold}}
).
-define(ERROR_BAD_VALUE_NOT_BETWEEN(__Key, __Low, __High),
    {error, {value_not_between, __Key, __Low, __High}}
).
-define(ERROR_BAD_VALUE_NOT_ALLOWED(__Key, __AllowedVals),
    {error, {value_not_allowed, __Key, __AllowedVals}}
).
-define(ERROR_BAD_VALUE_LIST_NOT_ALLOWED(__Key, __AllowedVals),
    {error, {list_of_values_not_allowed, __Key, __AllowedVals}}
).
-define(ERROR_BAD_VALUE_ID_NOT_FOUND(__Key), {error, {id_not_found, __Key}}).
-define(ERROR_BAD_VALUE_ID_OCCUPIED(__Key), {error, {id_occupied, __Key}}).
-define(ERROR_BAD_VALUE_BAD_TOKEN_TYPE(__Key), {error, {bad_token_type, __Key}}).
% Errors connected with relations between entities
-define(ERROR_RELATION_DOES_NOT_EXIST(__ChModel, __ChId, __ParModel, __ParId),
    {error, {relation_does_not_exist, __ChModel, __ChId, __ParModel, __ParId}}
).
-define(ERROR_RELATION_ALREADY_EXISTS(__ChModel, __ChId, __ParModel, __ParId),
    {error, {relation_already_exists, __ChModel, __ChId, __ParModel, __ParId}}
).


-endif.
