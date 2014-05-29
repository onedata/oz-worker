%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module handles communication between global registry and
%% providers concerning user authentication.
%% @end
%% ===================================================================
-module(onedata_auth).
-define(provider_nonce_endpoint, "/rest/latest/openid_nonce").
-define(provider_login_endpoint, "/openid/login").


-include("logging.hrl").

%% API
-export([get_redirect_to_provider_url/2, generate_authorization_code/1]).


get_redirect_to_provider_url(ProviderIP, UserGlobalID) ->
    AuthorizationCode = generate_authorization_code(UserGlobalID),
    temp_user_logic:create_association(AuthorizationCode, UserGlobalID),
    ParamsProplist = [
        {<<"authorization_code">>, AuthorizationCode}
    ],
    ParamsString = auth_utils:proplist_to_params(ParamsProplist),
    <<ProviderIP/binary, ?provider_login_endpoint, "?", ParamsString/binary>>.


generate_authorization_code(UserID) ->
    {Code, _} = auth_utils:generate_uuid(),
    temp_user_logic:create_association(Code, UserID),
    Code.