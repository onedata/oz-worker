%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains possible errors related to
%%% authentication and authorization.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AUTH_ERRORS_HRL).
-define(AUTH_ERRORS_HRL, 1).

% Errors were found in auth.config file
-define(ERROR_BAD_AUTH_CONFIG, {error, bad_auth_config}).

% The login request state is invalid
-define(ERROR_INVALID_STATE, {error, invalid_state}).

% The login request is invalid
-define(ERROR_INVALID_AUTH_REQUEST, {error, invalid_auth_request}).

% Could not connect to the IdP to perform a request
-define(ERROR_IDP_UNREACHABLE(__Error),
    {error, {idp_unreachable, __Error}}
).

% IdP has responded in a different way than expected
-define(ERROR_BAD_IDP_RESPONSE(__Endpoint, __Code, __Headers, __Body),
    {error, {bad_idp_response, __Endpoint, __Code, __Headers, __Body}}
).

% A required attribute was not found in received IdP attributes
-define(ERROR_CANNOT_RESOLVE_REQUIRED_ATTRIBUTE(__Attribute),
    {error, {cannot_resolve_required_attribute, __Attribute}}
).

% A required attribute was found in received IdP attributes,
% but is of invalid type
-define(ERROR_BAD_ATTRIBUTE_TYPE(__Attribute, __Type),
    {error, {bad_attribute_type, __Attribute, __Type}}
).

% A required attribute could not be mapped because of an error
% (logs must be examined for details)
-define(ERROR_ATTRIBUTE_MAPPING_ERROR(__Attribute, __IdPAttributes, __EType, __EReason, __Stacktrace),
    {error, {attribute_mapping_error, __Attribute, __IdPAttributes, __EType, __EReason, __Stacktrace}}
).

% The account has already been linked to current user profile
-define(ERROR_ACCOUNT_ALREADY_LINKED_TO_CURRENT_USER(__UserId),
    {error, {account_already_linked_to_current_user, __UserId}}
).

% The account has already been linked to another user profile
-define(ERROR_ACCOUNT_ALREADY_LINKED_TO_ANOTHER_USER(__UserId, __OtherUserId),
    {error, {account_already_linked_to_another_user, __UserId, __OtherUserId}}
).

% Other possible auth errors include:
% 1) ?ERROR_INTERNAL_SERVER_ERROR (api_errors.hrl)


-endif.

