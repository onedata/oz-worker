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
-module(auth_google).
-behaviour(auth_module_behaviour).

-include_lib("ctool/include/logging.hrl").
-include("auth_common.hrl").
-include("datastore/oz_datastore_models_def.hrl").

-define(PROVIDER_NAME, google).

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
            {<<"response_type">>, <<"code">>},
            {<<"scope">>, <<"openid email profile">>},
            {<<"redirect_uri">>, auth_utils:local_auth_endpoint()},
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
            {<<"code">>, <<Code/binary>>},
            {<<"client_id">>, auth_config:get_provider_app_id(?PROVIDER_NAME)},
            {<<"client_secret">>, auth_config:get_provider_app_secret(?PROVIDER_NAME)},
            {<<"redirect_uri">>, auth_utils:local_auth_endpoint()},
            {<<"grant_type">>, <<"authorization_code">>}
        ],
        % Convert proplist to params string
        Params = http_utils:proplist_to_url_params(NewParamsProplist),
        % Send request to Google endpoint
        {ok, 200, _, Response} = http_client:post(access_token_endpoint(),
            [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}], Params),

        % Parse out received access token and form a user info request
        AccessToken = parse_json(Response, <<"access_token">>),
        URL = <<(user_info_endpoint())/binary, "?access_token=", AccessToken/binary>>,

        % Send request to Google endpoint
        {ok, 200, _, Response2} = http_client:get(URL,
            [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}]),

        % Parse JSON with user info
        JSONProplist = json_utils:decode(Response2),
        ProvUserInfo = #oauth_account{
            provider_id = ?PROVIDER_NAME,
            user_id = str_utils:to_binary(
                proplists:get_value(<<"sub">>, JSONProplist, <<"">>)),
            email_list = extract_emails(JSONProplist),
            name = proplists:get_value(<<"name">>, JSONProplist, <<"">>)
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
%% @doc Provider endpoint for XRDS file, which contains entries about other endpoints.
%% @end
%%--------------------------------------------------------------------
-spec xrds_endpoint() -> binary().
xrds_endpoint() ->
    proplists:get_value(xrds_endpoint, auth_config:get_auth_config(?PROVIDER_NAME)).

%%--------------------------------------------------------------------
%% @private
%% @doc Provider endpoint, where users are redirected for authorization.
%% @end
%%--------------------------------------------------------------------
-spec authorize_endpoint() -> binary().
authorize_endpoint() ->
    {ok, 200, _, XRDS} = http_client:get(xrds_endpoint(), [], <<>>,
        [{follow_redirect, true}, {max_redirect, 5}]),
    parse_json(XRDS, <<"authorization_endpoint">>).

%%--------------------------------------------------------------------
%% @private
%% @doc Provider endpoint, where access token is aquired.
%% @end
%%--------------------------------------------------------------------
-spec access_token_endpoint() -> binary().
access_token_endpoint() ->
    {ok, 200, _, XRDS} = http_client:get(xrds_endpoint(), [], <<>>,
        [{follow_redirect, true}, {max_redirect, 5}]),
    parse_json(XRDS, <<"token_endpoint">>).

%%--------------------------------------------------------------------
%% @private
%% @doc Provider endpoint, where user info is aquired.
%% @end
%%--------------------------------------------------------------------
-spec user_info_endpoint() -> binary().
user_info_endpoint() ->
    {ok, 200, _, XRDS} = http_client:get(xrds_endpoint(), [], <<>>,
        [{follow_redirect, true}, {max_redirect, 5}]),
    parse_json(XRDS, <<"userinfo_endpoint">>).

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts email list from JSON in erlang format (after decoding).
%% @end
%%--------------------------------------------------------------------
-spec extract_emails([{term(), term()}]) -> [binary()].
extract_emails(JSONProplist) ->
    case proplists:get_value(<<"email">>, JSONProplist, <<"">>) of
        <<"">> -> [];
        Email -> [Email]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts a given key from flat JSON (nested one level).
%% @end
%%--------------------------------------------------------------------
-spec parse_json(binary(), binary()) -> binary().
parse_json(JSON, Key) ->
    proplists:get_value(Key, json_utils:decode(JSON)).