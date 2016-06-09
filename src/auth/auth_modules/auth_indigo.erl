%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module implements auth_module_behaviour and handles singning in
%%% via Github.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_indigo).
-behaviour(auth_module_behaviour).

-include_lib("ctool/include/logging.hrl").
-include("auth_common.hrl").
-include("datastore/oz_datastore_models_def.hrl").

-define(PROVIDER_NAME, indigo).

%% API
-export([get_redirect_url/1, validate_login/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns full URL, where the user will be redirected for authorization.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec get_redirect_url(boolean()) -> {ok, binary()} | {error, term()}.
get_redirect_url(ConnectAccount) ->
    auth_oauth2_common:get_redirect_url(ConnectAccount, ?PROVIDER_NAME).

%%--------------------------------------------------------------------
%% @doc Validates login request that came back from the provider.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec validate_login() ->
    {ok, #oauth_account{}} | {error, term()}.
validate_login() ->
    auth_oauth2_common:validate_login(?PROVIDER_NAME, secret_over_http_basic).