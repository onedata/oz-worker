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
-define(privacy_policy_url, "/privacy_policy").

% Endpoint at oneprovider to get its ID
-define(provider_id_endpoint, "/get_provider_id").

% Relative suffix of GUI address, leading to page with info about providers
-define(become_a_provider_url, "/become_a_provider").

% URL (relative) with login page.
-define(page_login, "/#/home/login").

% URL (relative) to redirect to after login.
-define(page_after_login, "/#/onezone").

% URL (relative) to redirect to after login.
-define(page_after_login_expand_accounts, "/#/onezone?expand_accounts=true").

% Macros used as ids of errors that can appear on GUI pages
-define(error_internal_server_error, "internal_server_error").

-endif.

