%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module implements auth_module_behaviour and handles singning in
%%% via Github.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_github).
-behaviour(auth_module_behaviour).

-include_lib("ctool/include/logging.hrl").
-include("auth_common.hrl").
-include("datastore/oz_datastore_models_def.hrl").

%% Used in header required by GitHub (probably for statistical purposes)
-define(user_agent_name, "One Data").

-define(PROVIDER_NAME, github).

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
    try
        ParamsProplist = [
            {<<"client_id">>, auth_config:get_provider_app_id(?PROVIDER_NAME)},
            {<<"redirect_uri">>, auth_utils:local_auth_endpoint()},
            {<<"scope">>, <<"user,user:email">>},
            {<<"state">>, auth_logic:generate_state_token(?MODULE, ConnectAccount)}
        ],
        Params = http_utils:proplist_to_url_params(ParamsProplist),
        {ok, <<(authorize_endpoint())/binary, "?", Params/binary>>}
    catch
        Type:Message ->
            ?error_stacktrace("Cannot get redirect URL for ~p", [?PROVIDER_NAME]),
            {error, {Type, Message}}
    end.

%%--------------------------------------------------------------------
%% @doc Validates login request that came back from the provider.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec validate_login() ->
    {ok, #oauth_account{}} | {error, term()}.
validate_login() ->
    try
        % Retrieve URL params
        ParamsProplist = g_ctx:get_url_params(),
        % Parse out code parameter
        Code = proplists:get_value(<<"code">>, ParamsProplist),
        % Form access token request
        NewParamsProplist = [
            {<<"client_id">>, auth_config:get_provider_app_id(?PROVIDER_NAME)},
            {<<"client_secret">>, auth_config:get_provider_app_secret(?PROVIDER_NAME)},
            {<<"redirect_uri">>, auth_utils:local_auth_endpoint()},
            {<<"code">>, <<Code/binary>>}
        ],
        % Convert proplist to params string
        Params = http_utils:proplist_to_url_params(NewParamsProplist),
        % Send request to GitHub endpoint
        {ok, 200, _, Response} = http_client:post(access_token_endpoint(),
            [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}], Params),

        % Parse out received access token
        AccessToken = proplists:get_value(<<"access_token">>, cow_qs:parse_qs(Response)),

        % Form user info request
        URL = <<(user_info_endpoint())/binary, "?access_token=", AccessToken/binary>>,
        % Send request to GitHub endpoint
        {ok, 200, _, JSON} = http_client:get(URL, [
            {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
            {<<"User-Agent">>, <<?user_agent_name>>}
        ]),

        % Form user email request
        URLEmail = <<(user_emails_endpoint())/binary, "?access_token=", AccessToken/binary>>,
        % Send request to GitHub endpoint
        {ok, 200, _, JSONEmails} = http_client:get(URLEmail, [
            {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
            {<<"User-Agent">>, <<?user_agent_name>>}
        ]),

        % Parse received JSON
        JSONProplist = json_utils:decode(JSON),
        ProvUserInfo = #oauth_account{
            provider_id = ?PROVIDER_NAME,
            user_id = str_utils:to_binary(
                proplists:get_value(<<"id">>, JSONProplist, <<"">>)),
            email_list = extract_emails(JSONEmails),
            name = proplists:get_value(<<"name">>, JSONProplist, <<"">>),
            login = proplists:get_value(<<"login">>, JSONProplist, <<"">>)
        },
        {ok, ProvUserInfo}
    catch
        Type:Message ->
            ?debug_stacktrace("Error in ~p:validate_login - ~p:~p", [?MODULE, Type, Message]),
            {error, {Type, Message}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Provider endpoint, where users are redirected for authorization.
%% @end
%%--------------------------------------------------------------------
-spec authorize_endpoint() -> binary().
authorize_endpoint() ->
    proplists:get_value(authorize_endpoint, auth_config:get_auth_config(?PROVIDER_NAME)).

%%--------------------------------------------------------------------
%% @private
%% @doc Provider endpoint, where access token is aquired.
%% @end
%%--------------------------------------------------------------------
-spec access_token_endpoint() -> binary().
access_token_endpoint() ->
    proplists:get_value(access_token_endpoint, auth_config:get_auth_config(?PROVIDER_NAME)).

%%--------------------------------------------------------------------
%% @private
%% @doc Provider endpoint, where user info is aquired.
%% @end
%%--------------------------------------------------------------------
-spec user_info_endpoint() -> binary().
user_info_endpoint() ->
    proplists:get_value(user_info_endpoint, auth_config:get_auth_config(?PROVIDER_NAME)).

%%--------------------------------------------------------------------
%% @private
%% @doc Provider endpoint, where user's emails are aquired.
%% @end
%%--------------------------------------------------------------------
-spec user_emails_endpoint() -> binary().
user_emails_endpoint() ->
    proplists:get_value(user_emails_endpoint, auth_config:get_auth_config(?PROVIDER_NAME)).

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts email list from JSON.
%% @end
%%--------------------------------------------------------------------
-spec extract_emails(binary()) -> [binary()].
extract_emails(JSON) ->
    lists:map(
        fun(Email) ->
            proplists:get_value(<<"email">>, Email)
        end, json_utils:decode(JSON)).