%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies the API for a Single Sign-On protocol handler,
%%% such as OpenID or SAML.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_protocol_behaviour).


%%--------------------------------------------------------------------
%% @doc
%% Builds an URL in given IdP where clients should be redirected for authentication.
%% Returns a map that includes three keys:
%%      <<"method">>
%%      <<"url">>
%%      <<"formData">>
%% that defines what request should be performed to redirect to the login page.
%% @end
%%--------------------------------------------------------------------
-callback get_login_endpoint(auth_config:idp(), state_token:id()) ->
    {ok, #{binary() => binary() | null}}.


%%--------------------------------------------------------------------
%% @doc
%% Validates a login request coming from given IdP.
%% @end
%%--------------------------------------------------------------------
-callback validate_login(auth_config:idp(), auth_logic:query_params()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
