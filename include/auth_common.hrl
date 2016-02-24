%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This file contains common macros and records for control_panel modules
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AUTH_COMMON_HRL).
-define(AUTH_COMMON_HRL, 1).

% Location of auth.config file.
-define(auth_config_file_path, "data/auth.config").

% Endpoint for OpenID / OAuth validation
-define(local_auth_endpoint, "/validate_login.html").

% Endpoint for redirects to providers
-define(provider_auth_endpoint, "/validate_login.html").

% Error atom indicating that login request is invalid
-define(error_auth_invalid_request, "invalid_request").

% Error atom indicating that email address is occupied (while creating new acc)
-define(error_auth_new_email_occupied, "new_email_occupied").

% Error atom indicating that email address is occupied (while connecting an acc)
-define(error_auth_connect_email_occupied, "connect_email_occupied").

% Error atom indicating that account has already been connected to other profile
-define(error_auth_account_already_connected, "account_already_connected").

-endif.

