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

-ifndef(ENTITY_LOGIC_ERRORS_HRL).
-define(ENTITY_LOGIC_ERRORS_HRL, 1).

-define(EL_INTERNAL_SERVER_ERROR, {error, internal_server_error}).
-define(EL_MALFORMED_DATA, {error, bad_data}).

-define(EL_NOT_FOUND, {error, not_found}).

-define(EL_UNAUTHORIZED, {error, unauthorized}).
-define(EL_FORBIDDEN, {error, forbidden}).

-define(EL_MISSING_REQUIRED_DATA(__Key), {error, {missing_required_data, __Key}}).
-define(EL_MISSING_AT_LEAST_ONE_DATA(__Keys), {error, {missing_at_least_one_data, __Keys}}).

-define(EL_BAD_DATA(__Key), {error, {bad_data, __Key}}).
-define(EL_EMPTY_DATA(__Key), {error, {empty_data, __Key}}).
-define(EL_ID_NOT_FOUND(__Key), {error, {id_not_found, __Key}}).
-define(EL_ID_OCCUPIED(__Key), {error, {id_occupied, __Key}}).
-define(EL_BAD_TOKEN(__Key), {error, {bad_token, (__Key)}}).
-define(EL_BAD_TOKEN_TYPE(__Key), {error, {bad_token_type, (__Key)}}).
-define(EL_RELATION_EXISTS, {error, relation_exists}).
-define(EL_RELATION_DOES_NOT_EXIST, {error, relation_does_not_exist}).


-endif.
