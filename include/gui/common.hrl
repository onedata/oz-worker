%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains common macros and records for control_panel modules
%% @end
%% ===================================================================

-ifndef(CONTROL_PANEL_COMMON_HRL).
-define(CONTROL_PANEL_COMMON_HRL, 1).

-include_lib("ctool/include/gui/common.hrl").
-include("registered_names.hrl").

% Relative suffix of GUI address, leading to privacy policy page
-define(privacy_policy_url, "/privacy_policy").

% Relative suffix of GUI address, leading to page with info about providers
-define(become_a_provider_url, "/become_a_provider").

% Endpoint at oneprovider to check connectivity
-define(provider_connection_check_endpoint, "/connection_check").

% Macros used as ids of errors that can appear on GUI pages
-define(error_internal_server_error, "internal_server_error").

-endif.

