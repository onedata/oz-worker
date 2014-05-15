%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains common macros and records for control_panel modules
%% @end
%% ===================================================================

-ifndef(AUTH_COMMON_HRL).
-define(AUTH_COMMON_HRL, 1).

% Endpoint for OpenID / OAuth validation
-define(local_auth_endpoint, "/validate_login").

% Error atom indicating that login request is invalid
-define(error_auth_invalid_request, invalid_request).

-record(user_info, {
    global_id = <<"">>,
    preferred_name = <<"">>,
    emails = [],
    provider_infos = []
}).

-record(provider_user_info, {
    provider_id = undefined,
    user_id = <<"">>,
    login = <<"">>,
    name = <<"">>,
    emails = <<"">>
}).

-endif.

