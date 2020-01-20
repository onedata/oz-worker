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


-type callback() :: get_login_endpoint | validate_login | refresh_access_token | get_user_info.
-export_type([callback/0]).


%%--------------------------------------------------------------------
%% @doc
%% Builds an URL in given IdP where clients should be redirected for authentication.
%% @end
%%--------------------------------------------------------------------
-callback get_login_endpoint(auth_config:idp(), state_token:state_token(),
    idp_auth:redirect_uri()) ->
    idp_auth:login_endpoint().


%%--------------------------------------------------------------------
%% @doc
%% Validates a login request coming from given IdP.
%% Returned attributes can contain <<"access_token">> and <<"refresh_token">> keys,
%% in such case they will be stored for offline access.
%% @end
%%--------------------------------------------------------------------
-callback validate_login(auth_config:idp(), idp_auth:query_params(),
    idp_auth:redirect_uri()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Acquires a new access token using given refresh token.
%% @end
%%--------------------------------------------------------------------
-callback refresh_access_token(auth_config:idp(), idp_auth:refresh_token()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user info from given IdP based on an access token.
%% Returned attributes can contain <<"access_token">> and <<"refresh_token">> keys,
%% in such case they will be stored for offline access.
%% @end
%%--------------------------------------------------------------------
-callback get_user_info(auth_config:idp(), idp_auth:access_token()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
