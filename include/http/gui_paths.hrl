%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains macros defining paths to dynamic GUI pages.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(GUI_PATHS_HRL).
-define(GUI_PATHS_HRL, 1).

% Endpoint to get Onezone configuration
-define(CONFIGURATION_PATH, "/configuration").

% Endpoint for nagios healthcheck
-define(NAGIOS_PATH, "/nagios").

% Endpoint for viewing public shares
-define(SHARE_ID_BINDING, share_id).
-define(PUBLIC_SHARE_COWBOY_ROUTE, "/share/:share_id").
-define(PUBLIC_SHARE_PATH(ShareId), <<"/share/", ShareId/binary>>).

% Endpoint in Oneprovider consuming login requests
-define(PROVIDER_PUBLIC_SHARE_PATH(ShareId), <<"/share/", ShareId/binary>>).

% Endpoint in Oneprovider consuming login requests
-define(LEGACY_PROVIDER_PUBLIC_SHARE_PATH(ShareId), <<"/#/public/shares/", ShareId/binary>>).

% Endpoint serving custom, user defined static files
-define(CUSTOM_STATIC_GUI_PATH, "/custom/[...]").

% Endpoint for GUI upload (used tu upload OP_WORKER, ONEPANEL or harvester GUI)
-define(GUI_UPLOAD_PATH, "/:gui_prefix/:gui_id/gui-upload").

% Endpoints for GUI scripts to discover and preauthorize GUIs of different services
-define(GUI_CONTEXT_PATH, "/:gui_prefix/:gui_id/gui-context").
-define(GUI_PREAUTHORIZE_PATH, "/:gui_prefix/:gui_id/gui-preauthorize").

% Endpoint to perform basic auth login
-define(LOGIN_PATH, "/login").

% Endpoint to perform log out from current session
-define(LOGOUT_PATH, "/logout").

% Endpoint to consume OpenID message (redirect url from IdP)
-define(OIDC_CONSUME_PATH_DEPRECATED, "/validate_login").
-define(OIDC_CONSUME_PATH, "/oidc/consume").

% Endpoint to consume SAML message (redirect url from IdP)
-define(SAML_CONSUME_PATH, "/saml/consume").

% Endpoint serving SAML SP metadata
-define(SAML_METADATA_PATH, "/saml/sp.xml").

% Endpoint serving SAML certificate
-define(SAML_CERT_PATH, "/saml/certificate.pem").

% Endpoint for developer mode login page
-define(DEV_LOGIN_PATH, "/dev_login").

% Endpoint for validating developer mode login
-define(VALIDATE_DEV_LOGIN_PATH, "/validate_dev_login").

% Endpoint in Oneprovider consuming login requests
-define(PROVIDER_LOGIN_CONSUME_PATH, "/onezone-login/consume").
-define(PROVIDER_LOGIN_CONSUME_PATH_DEPRECATED, "/validate_login.html").

% URL (relative) pointing to login page.
-define(LOGIN_PAGE_PATH, "/#/home/login").

% URL (relative) to redirect to after login.
-define(AFTER_LOGIN_PAGE_PATH, "/#/onezone").

%% All requests to this endpoint will be proxied to onepanel.
-define(PANEL_REST_PROXY_PATH, "/api/v3/onepanel/").

-endif.
