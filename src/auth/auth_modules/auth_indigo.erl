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
    try
        ParamsProplist = [
            {<<"client_id">>,
                auth_config:get_provider_app_id(?PROVIDER_NAME)},
            {<<"response_type">>,
                <<"code">>},
            {<<"scope">>,
                <<"openid email profile">>},
            {<<"redirect_uri">>,
                auth_utils:local_auth_endpoint()},
            {<<"state">>,
                auth_logic:generate_state_token(?MODULE, ConnectAccount)}
        ],
        Params = http_utils:proplist_to_url_params(ParamsProplist),
        AuthorizeEndpoint = authorize_endpoint(get_xrds()),
        {ok, <<AuthorizeEndpoint/binary, "?", Params/binary>>}
    catch
        Type:Message ->
            ?error_stacktrace("Cannot get redirect URL for ~p",
                [?PROVIDER_NAME]),
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
        ClientId = auth_config:get_provider_app_id(?PROVIDER_NAME),
        ClientSecret = auth_config:get_provider_app_secret(?PROVIDER_NAME),
        % Form access token request
        NewParamsProplist = [
            {<<"code">>, Code},
%%            {<<"client_id">>, ClientId},
%%            {<<"client_secret">>, ClientSecret},
            {<<"redirect_uri">>, auth_utils:local_auth_endpoint()},
            {<<"grant_type">>, <<"authorization_code">>}
        ],
        % Convert proplist to params string
        Params = http_utils:proplist_to_url_params(NewParamsProplist),
        % Send request to Google endpoint
        XRDS = get_xrds(),
        B64 = base64:encode(<<ClientId/binary, ":", ClientSecret/binary>>),
        {ok, 200, _, ResponseBinary} = http_client:post(
            access_token_endpoint(XRDS),
            [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
                {<<"Authorization">>, <<"Basic ", B64/binary>>}],
            Params
        ),

        % Parse out received access token and form a user info request
        Response = json_utils:decode(ResponseBinary),
        AccessToken = proplists:get_value(<<"access_token">>, Response),
        UserInfoEndpoint = user_info_endpoint(XRDS),
        URL = <<UserInfoEndpoint/binary, "?access_token=", AccessToken/binary>>,

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
            ?error_stacktrace("Error in ~p:validate_login - ~p:~p",
                [?MODULE, Type, Message]),
            {error, {Type, Message}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get XRDS file and parse it. It contains entries well known configuration
%% of openid provider (endpoints etc).
%% @end
%%--------------------------------------------------------------------
-spec get_xrds() -> proplists:proplist().
get_xrds() ->
    ProviderConfig = auth_config:get_auth_config(?PROVIDER_NAME),
    XRDSEndpoint = proplists:get_value(xrds_endpoint, ProviderConfig),
    {ok, 200, _, XRDS} = http_client:get(XRDSEndpoint, [], <<>>,
        [{follow_redirect, true}, {max_redirect, 5}]),
    json_utils:decode(XRDS).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Provider endpoint, where users are redirected for authorization.
%% @end
%%--------------------------------------------------------------------
-spec authorize_endpoint(XRDS :: proplists:proplist()) -> binary().
authorize_endpoint(XRDS) ->
    proplists:get_value(<<"authorization_endpoint">>, XRDS).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Provider endpoint, where access token is acquired.
%% @end
%%--------------------------------------------------------------------
-spec access_token_endpoint(XRDS :: proplists:proplist()) -> binary().
access_token_endpoint(XRDS) ->
    proplists:get_value(<<"token_endpoint">>, XRDS).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Provider endpoint, where user info is acquired.
%% @end
%%--------------------------------------------------------------------
-spec user_info_endpoint(XRDS :: proplists:proplist()) -> binary().
user_info_endpoint(XRDS) ->
    proplists:get_value(<<"userinfo_endpoint">>, XRDS).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extracts email list from JSON in erlang format (after decoding).
%% @end
%%--------------------------------------------------------------------
-spec extract_emails([{term(), term()}]) -> [binary()].
extract_emails(JSONProplist) ->
    case proplists:get_value(<<"email">>, JSONProplist, <<"">>) of
        <<"">> -> [];
        Email -> [Email]
    end.
