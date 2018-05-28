%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour should be implemented by modules handling OpenID and OAuth
%%% requests. It ensures the presence of required callbacks.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_module_behaviour).
-include("datastore/oz_datastore_models.hrl").
-include("auth_common.hrl").

%%--------------------------------------------------------------------
%% @doc Returns full URL, where the user will be redirected for authorization.
%% For the authentication flow to work, the request must include a state
%% token, which must be generated with auth_logic:generate_state_token/2.
%% This allows to store some information between the authentication request and
%% redirect back from the provider. The information is:
%% - where the user should be redirected after login
%% - if this was signing in or connecting next account to the profile.
%% @end
%%--------------------------------------------------------------------
-callback get_redirect_url(auth_utils:idp(), boolean()) ->
    {ok, binary()} | {error, term()}.

%%--------------------------------------------------------------------
%% @doc Validates login request that came back from the provider.
%% Will be called from auth_utils:validate_login/0 when the request
%% has been pre-validated. Beside validating the request,
%% the function must retrieve user info from the provider.
%% Must return linked_account record upon success,
%% or error and its description otherwise.
%% @end
%%--------------------------------------------------------------------
-callback validate_login(auth_utils:idp(), QueryParams :: proplists:proplist()) ->
    {ok, #linked_account{}} | {error, term()}.



%%--------------------------------------------------------------------
%% @doc
%% Retrieves user info from oauth provider based on access token.
%% @end
%%--------------------------------------------------------------------
-callback get_user_info(auth_utils:idp(), AccessToken :: binary()) ->
    {ok, #linked_account{}} | {error, bad_access_token}.