%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This behaviour should be implemented by modules handling OpenID and OAuth requests.
%% It ensures the presence of required callbacks.
%% @end
%% ===================================================================
-module(auth_module_behaviour).
-include("dao/dao_types.hrl").
-include("auth_common.hrl").


%% get_redirect_url/1
%% ====================================================================
%% @doc Returns full URL, where the user will be redirected for authorization.
%% For the authentication flow to work, the request must include a state
%% token, which must be generated with auth_logic:generate_state_token/2.
%% This allows to store some information between the authentication request and
%% redirect back from the provider. The information is:
%% - where the user should be redirected after login
%% - if this was signing in or connecting next account to the profile.
%% @end
%% ====================================================================
-callback get_redirect_url(boolean()) -> binary().


%% validate_login/1
%% ====================================================================
%% @doc Validates login request that came back from the provider.
%% Will be called from auth_utils:validate_login/0 when the request
%% has been pre-validated. The argument is a proplist (pair of Key and Value)
%% representing the content of URL params. Beside validating the request,
%% the function must retrieve user info from the provider.
%% Must return oauth_account record upon success,
%% or error and its desription otherwise.
%% @end
%% ====================================================================
-callback validate_login([{binary(), binary()}]) ->
    {ok, #oauth_account{}} | {error, term()}.