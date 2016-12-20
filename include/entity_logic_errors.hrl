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

-define(ERROR_INTERNAL_SERVER_ERROR, {error, internal_server_error}).
-define(ERROR_MALFORMED_DATA, {error, bad_data}).

-define(ERROR_NOT_FOUND, {error, not_found}).

-define(ERROR_UNAUTHORIZED, {error, unauthorized}).
-define(ERROR_FORBIDDEN, {error, forbidden}).

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

-define(ERROR_USER_ALREADY_IN_SPACE(__User, __Space),
    {error, {user_already_in_space, __User, __Space}}
).
-define(ERROR_USER_ALREADY_IN_GROUP(__User, __Group),
    {error, {user_already_in_group, __User, __Group}}
).
-define(ERROR_USER_ALREADY_IN_HSERVICE(__User, __HService),
    {error, {user_already_in_hservice, __User, __HService}}
).
-define(ERROR_USER_ALREADY_IN_HANDLE(__User, __Handle),
    {error, {user_already_in_handle, __User, __Handle}}
).
-define(ERROR_GROUP_ALREADY_IN_SPACE(__Group, __Space),
    {error, {group_already_in_space, __Group, __Space}}
).
-define(ERROR_GROUP_ALREADY_IN_GROUP(__ChildGroup, __ParentGroup),
    {error, {group_already_in_group, __ChildGroup, __ParentGroup}}
).
-define(ERROR_GROUP_ALREADY_IN_HSERVICE(__User, __HService),
    {error, {group_already_in_hservice, __User, __HService}}
).
-define(ERROR_GROUP_ALREADY_IN_HANDLE(__User, __Handle),
    {error, {group_already_in_handle, __User, __Handle}}
).
-define(ERROR_SPACE_ALREADY_SUPPORTED(__Space, __Provider),
    {error, {space_already_supported, __Space, __Provider}})
.

-define(EL_RELATION_EXISTS, {error, relation_exists}).
-define(EL_RELATION_DOES_NOT_EXIST, {error, relation_does_not_exist}).


-endif.
