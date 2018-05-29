%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module implements auth_module_behaviour and handles signing in
%%% via Github OpenID.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_github).
-behaviour(auth_module_behaviour).

-include_lib("ctool/include/logging.hrl").
-include("auth_common.hrl").
-include("datastore/oz_datastore_models.hrl").

%% Used in header required by GitHub (probably for statistical purposes)
-define(user_agent_name, "One Data").

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
    try
        ParamsProplist = [
            {<<"client_id">>, auth_config:get_provider_app_id(IdP)},
            {<<"redirect_uri">>, auth_utils:local_auth_endpoint()},
            {<<"scope">>, <<"user,user:email">>},
            {<<"state">>, auth_logic:generate_state_token(IdP, LinkAccount)}
        ],
        Params = http_utils:proplist_to_url_params(ParamsProplist),
        {ok, <<(authorize_endpoint(IdP))/binary, "?", Params/binary>>}
    catch
        Type:Message ->
            ?error_stacktrace("Cannot get redirect URL for ~p", [IdP]),
            {error, {Type, Message}}
    end.

%%--------------------------------------------------------------------
%% @doc Validates login request that came back from the provider.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(auth_utils:idp(), QueryParams :: proplists:proplist()) ->
    {ok, #linked_account{}} | {error, term()}.
validate_login(IdP, QueryParams) ->
    try
        % Parse out code parameter
        Code = proplists:get_value(<<"code">>, QueryParams),
        % Form access token request
        NewParamsProplist = [
            {<<"client_id">>, auth_config:get_provider_app_id(IdP)},
            {<<"client_secret">>, auth_config:get_provider_app_secret(IdP)},
            {<<"redirect_uri">>, auth_utils:local_auth_endpoint()},
            {<<"code">>, <<Code/binary>>}
        ],
        % Convert proplist to params string
        Params = http_utils:proplist_to_url_params(NewParamsProplist),
        % Send request to GitHub endpoint
        {ok, 200, _, Response} = http_client:post(access_token_endpoint(IdP), #{
            <<"Content-Type">> => <<"application/x-www-form-urlencoded">>
        }, Params),

        % Parse out received access token
        AccessToken = proplists:get_value(<<"access_token">>, cow_qs:parse_qs(Response)),

        get_user_info(IdP, AccessToken)
    catch
        Type:Message ->
            ?debug_stacktrace("Error in ~p:validate_login - ~p:~p", [?MODULE, Type, Message]),
            {error, {Type, Message}}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user info from oauth provider based on access token.
%% @end
%%--------------------------------------------------------------------
-spec get_user_info(auth_utils:idp(), AccessToken :: binary()) ->
    {ok, #linked_account{}} | {error, bad_access_token}.
get_user_info(IdP, AccessToken) ->
    URL = <<(user_info_endpoint(IdP))/binary, "?access_token=", AccessToken/binary>>,
    % Send request to GitHub endpoint
    {ok, 200, _, JSON} = http_client:get(URL, #{
        <<"Content-Type">> => <<"application/x-www-form-urlencoded">>,
        <<"User-Agent">> => <<?user_agent_name>>
    }, <<"">>),

    % Form user email request
    URLEmail = <<(user_emails_endpoint(IdP))/binary, "?access_token=", AccessToken/binary>>,
    % Send request to GitHub endpoint
    {ok, 200, _, JSONEmails} = http_client:get(URLEmail, #{
        <<"Content-Type">> => <<"application/x-www-form-urlencoded">>,
        <<"User-Agent">> => <<?user_agent_name>>
    }, <<"">>),
    % Parse received emails
    EmailList = lists:map(
        fun(Email) ->
            auth_utils:get_value_binary(<<"email">>, Email)
        end, json_utils:decode(JSONEmails)),

    % Parse received JSON
    JSONMap = json_utils:decode(JSON),
    ProvUserInfo = #linked_account{
        idp = IdP,
        subject_id = auth_utils:get_value_binary(<<"id">>, JSONMap),
        email_list = EmailList,
        name = auth_utils:get_value_binary(<<"name">>, JSONMap),
        login = auth_utils:get_value_binary(<<"login">>, JSONMap),
        groups = []
    },
    {ok, ProvUserInfo}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Provider endpoint, where users are redirected for authorization.
%% @end
%%--------------------------------------------------------------------
-spec authorize_endpoint(auth_utils:idp()) -> binary().
authorize_endpoint(IdP) ->
    proplists:get_value(authorize_endpoint, auth_config:get_auth_config(IdP)).

%%--------------------------------------------------------------------
%% @private
%% @doc Provider endpoint, where access token is acquired.
%% @end
%%--------------------------------------------------------------------
-spec access_token_endpoint(auth_utils:idp()) -> binary().
access_token_endpoint(IdP) ->
    proplists:get_value(access_token_endpoint, auth_config:get_auth_config(IdP)).

%%--------------------------------------------------------------------
%% @private
%% @doc Provider endpoint, where user info is acquired.
%% @end
%%--------------------------------------------------------------------
-spec user_info_endpoint(auth_utils:idp()) -> binary().
user_info_endpoint(IdP) ->
    proplists:get_value(user_info_endpoint, auth_config:get_auth_config(IdP)).

%%--------------------------------------------------------------------
%% @private
%% @doc Provider endpoint, where user's emails are acquired.
%% @end
%%--------------------------------------------------------------------
-spec user_emails_endpoint(auth_utils:idp()) -> binary().
user_emails_endpoint(IdP) ->
    proplists:get_value(user_emails_endpoint, auth_config:get_auth_config(IdP)).
