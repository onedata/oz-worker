%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies the API for an auth plugin that handles Open ID
%%% login process.
%%% @end
%%%-------------------------------------------------------------------
-module(openid_plugin_behaviour).


-type callback() :: get_login_endpoint | get_user_info | validate_login.
-export_type([callback/0]).


%%--------------------------------------------------------------------
%% @doc
%% Builds an URL in given IdP where clients should be redirected for authentication.
%% @end
%%--------------------------------------------------------------------
-callback get_login_endpoint(auth_config:idp(), state_token:id(),
    auth_logic:redirect_uri()) ->
    auth_logic:login_endpoint().


%%--------------------------------------------------------------------
%% @doc
%% Validates a login request coming from given IdP.
%% @end
%%--------------------------------------------------------------------
-callback validate_login(auth_config:idp(), auth_logic:query_params(),
    auth_logic:redirect_uri()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user info from given IdP based on an access token.
%% @end
%%--------------------------------------------------------------------
-callback get_user_info(auth_config:idp(), auth_logic:access_token()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
