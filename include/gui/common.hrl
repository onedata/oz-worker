%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2013 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This file contains common macros and records for control_panel modules
%%% @end
%%%-------------------------------------------------------------------

-ifndef(CONTROL_PANEL_COMMON_HRL).
-define(CONTROL_PANEL_COMMON_HRL, 1).

-include("registered_names.hrl").

% Relative suffix of GUI address, leading to privacy policy page
-define(PRIVACY_POLICY_URL, "/privacy_policy").

% Endpoint at oneprovider to get its ID
-define(PROVIDER_ID_ENDPOINT, "/get_provider_id").

% Relative suffix of GUI address, leading to page with info about providers
-define(BECOME_A_PROVIDER_URL, "/become_a_provider").

% URL (relative) pointing to login page.
-define(LOGIN_PAGE, "/#/home/login").

% URL (relative) to redirect to after login.
-define(PAGE_AFTER_LOGIN, "/#/onezone").

% Macros used as ids of errors that can appear on GUI pages
-define(ERROR_INTERNAL_SERVER_ERROR, "internal_server_error").

-endif.

