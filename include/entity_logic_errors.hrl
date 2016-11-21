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
-define(EL_NOT_FOUND, {error, not_found}).
-define(EL_UNAUTHORIZED, {error, unauthorized}).
-define(EL_MISSING_REQUIRED_DATA(__Key), {error, missing_req_data, __Key}).
-define(EL_MISSING_REQUIRED_DATA_MATCHER, {error, missing_req_data, _}).
-define(EL_MISSING_ANY_DATA, {error, missing_any_data}).
-define(EL_MISSING_ANY_DATA_MATCHER, {error, missing_any_data}).
-define(EL_BAD_DATA(__Key), {error, bad_data, __Key}).
-define(EL_BAD_DATA_MATCHER, {error, bad_data, _}).
-define(EL_EMPTY_DATA(__Key), {error, empty_data, __Key}).
-define(EL_EMPTY_DATA_MATCHER, {error, empty_data, _}).


-define(EL_RELATION_EXISTS, {error, relation_exists}).

-endif.
