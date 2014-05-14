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

-define(local_auth_endpoint, "/validate_login").

-record(user_info, {
    preferred_login = "",
    preferred_name = "",
    emails = [],
    provider_infos = []
}).

-record(provider_user_info, {
    provider_id = undefined,
    login = "",
    name = "",
    email = ""
}).

-endif.

