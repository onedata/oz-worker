%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module implements auth_module_behaviour and handles signing in
%%% via ELIXIR OpenID.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_elixir).
-behaviour(auth_module_behaviour).

-include_lib("ctool/include/logging.hrl").
-include("auth_common.hrl").
-include("datastore/oz_datastore_models.hrl").

%% API
-export([get_redirect_url/2, validate_login/2, get_user_info/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns full URL, where the user will be redirected for authorization.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec get_redirect_url(auth_utils:idp(), boolean()) -> {ok, binary()} | {error, term()}.
get_redirect_url(IdP, LinkAccount) ->
    auth_oauth2_common:get_redirect_url(LinkAccount, IdP).


%%--------------------------------------------------------------------
%% @doc Validates login request that came back from the provider.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(auth_utils:idp(), QueryParams :: proplists:proplist()) ->
    {ok, #linked_account{}} | {error, term()}.
validate_login(IdP, QueryParams) ->
    auth_oauth2_common:validate_login(
        IdP, QueryParams, secret_over_http_basic, access_token_in_header
    ).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user info from oauth provider based on access token.
%% @end
%%--------------------------------------------------------------------
-spec get_user_info(auth_utils:idp(), AccessToken :: binary()) ->
    {ok, #linked_account{}} | {error, bad_access_token}.
get_user_info(IdP, AccessToken) ->
    auth_oauth2_common:get_user_info(
        IdP, access_token_in_url, AccessToken
    ).