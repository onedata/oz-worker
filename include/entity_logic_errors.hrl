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

-define(EL_INTERNAL_SERVER_ERROR, internal_server_error).
-define(EL_NOT_FOUND, not_found).
-define(EL_UNAUTHORIZED, unauthorized).
-define(EL_MISSING_REQUIRED_DATA(__Key), {missing_req_data, __Key}).
-define(EL_MISSING_REQUIRED_DATA_MATCHER, {missing_req_data, _}).
-define(EL_MISSING_ANY_DATA, missing_any_data).
-define(EL_MISSING_ANY_DATA_MATCHER, missing_any_data).
-define(EL_BAD_DATA, bad_data).
-define(EL_BAD_DATA(__Key), {bad_data, __Key}).
-define(EL_BAD_DATA_MATCHER, bad_data, _).
-define(EL_EMPTY_DATA(__Key), {empty_data, __Key}).
-define(EL_EMPTY_DATA_MATCHER, empty_data, _).
-define(EL_ID_NOT_FOUND(__Key), {id_not_found, __Key}).
-define(EL_ID_OCCUPIED(__Key), {id_occupied, __Key}).
-define(EL_BAD_TOKEN(__Key), {bad_token, (__Key)}).
-define(EL_BAD_TOKEN_TYPE(__Key), {bad_token_type, (__Key)}).



-define(EL_RELATION_EXISTS, relation_exists).
-define(EL_RELATION_DOES_NOT_EXIST, relation_does_not_exist).

-endif.
