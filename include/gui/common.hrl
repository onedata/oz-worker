%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2013 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains common macros and records for GUI related modules.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(CONTROL_PANEL_COMMON_HRL).
-define(CONTROL_PANEL_COMMON_HRL, 1).

-include("registered_names.hrl").

% Endpoint for nagios healthcheck
-define(NAGIOS_ENDPOINT, "/nagios/[...]").

% Endpoint for viewing public shares
-define(PUBLIC_SHARE_ENDPOINT, "/share").

% URL (relative) pointing to login page.
-define(LOGIN_PAGE, "/#/home/login").

% URL (relative) to redirect to after login.
-define(PAGE_AFTER_LOGIN, "/#/onezone").

% Endpoint at oneprovider to get its ID
-define(PROVIDER_ID_ENDPOINT, "/get_provider_id").

-endif.
