%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This file contains common macros and records for control_panel modules
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AUTH_COMMON_HRL).
-define(AUTH_COMMON_HRL, 1).

% Error atom indicating that there was an unexpected server error during login
-define(error_auth_server_error, server_error).

% Error atom indicating that login request state was
-define(error_auth_invalid_state, invalid_state).

% Error atom indicating that login request is invalid
-define(error_auth_invalid_request, invalid_request).

% Error atom indicating that login request failed because of invalid
% access token issued by the IdP
-define(error_auth_access_token_invalid, access_token_invalid).

% Error atom indicating that account has already been linked to other profile
-define(error_auth_account_already_linked_to_another_user,
    account_already_linked_to_another_user
).

% Error atom indicating that account has already been linked to other profile
-define(error_auth_account_already_linked_to_current_user,
    account_already_linked_to_current_user
).

-endif.

