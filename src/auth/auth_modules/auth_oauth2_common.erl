%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module implements common logic for integration with standard
%%% oauth2 servers.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_oauth2_common).

-include_lib("ctool/include/logging.hrl").
-include("auth_common.hrl").
-include("datastore/oz_datastore_models_def.hrl").

%% API
-export([
    get_redirect_url/3,
    validate_login/3,
    get_user_info/3, get_user_info/4
]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns full URL, where the user will be redirected for authorization.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec get_redirect_url(boolean(), ProviderId :: atom(),
    HandlerModule :: atom()) -> {ok, binary()} | {error, term()}.
get_redirect_url(ConnectAccount, ProviderId, HandlerModule) ->
    try
        ParamsProplist = [
            {<<"client_id">>,
                auth_config:get_provider_app_id(ProviderId)},
            {<<"response_type">>,
                <<"code">>},
            {<<"scope">>,
                <<"openid email profile">>},
            {<<"redirect_uri">>,
                auth_utils:local_auth_endpoint()},
            {<<"state">>,
                auth_logic:generate_state_token(HandlerModule, ConnectAccount)}
        ],
        Params = http_utils:proplist_to_url_params(ParamsProplist),
        AuthorizeEndpoint = authorize_endpoint(get_xrds(ProviderId)),
        {ok, <<AuthorizeEndpoint/binary, "?", Params/binary>>}
    catch
        Type:Message ->
            ?error_stacktrace("Cannot get redirect URL for ~p",
                [ProviderId]),
            {error, {Type, Message}}
    end.


%%--------------------------------------------------------------------
%% @doc Validates login request that came back from the provider.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(ProviderId :: atom(),
    SecretSendMethod :: secret_over_http_basic | secret_over_http_post,
    AccessTokenSendMethod :: access_token_in_url | access_token_in_header) ->
    {ok, #linked_account{}} | {error, term()}.
validate_login(ProviderId, SecretSendMethod, AccessTokenSendMethod) ->
    try
        % Retrieve URL params
        ParamsProplist = gui_ctx:get_url_params(),
        % Parse out code parameter
        Code = proplists:get_value(<<"code">>, ParamsProplist),
        ClientId = auth_config:get_provider_app_id(ProviderId),
        ClientSecret = auth_config:get_provider_app_secret(ProviderId),
        % Form access token request
        % Check which way we should send secret - HTTP Basic or POST
        SecretPostParams = case SecretSendMethod of
            secret_over_http_post ->
                [
                    {<<"client_id">>, ClientId},
                    {<<"client_secret">>, ClientSecret}
                ];
            secret_over_http_basic ->
                []
        end,
        SecretHeaders = case SecretSendMethod of
            secret_over_http_post ->
                #{};
            secret_over_http_basic ->
                B64 = base64:encode(
                    <<ClientId/binary, ":", ClientSecret/binary>>
                ),
                #{<<"Authorization">> => <<"Basic ", B64/binary>>}
        end,
        NewParamsProplist = SecretPostParams ++ [
            {<<"code">>, Code},
            {<<"redirect_uri">>, auth_utils:local_auth_endpoint()},
            {<<"grant_type">>, <<"authorization_code">>}
        ],
        % Convert proplist to params string
        Params = http_utils:proplist_to_url_params(NewParamsProplist),
        % Prepare headers
        Headers = SecretHeaders#{
            <<"Content-Type">> => <<"application/x-www-form-urlencoded">>
        },
        % Send request to access token endpoint
        XRDS = get_xrds(ProviderId),
        {ok, 200, _, ResponseBinary} = http_client:post(
            access_token_endpoint(XRDS),
            Headers,
            Params
        ),

        % Parse out received access token and form a user info request
        Response = json_utils:decode(ResponseBinary),
        AccessToken = proplists:get_value(<<"access_token">>, Response),

        get_user_info(ProviderId, AccessTokenSendMethod, AccessToken, XRDS)
    catch
        Type:Message ->
            ?debug_stacktrace("Error in OpenID validate_login (~p) - ~p:~p",
                [ProviderId, Type, Message]),
            {error, {Type, Message}}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user info for given OpenID provider and access token.
%% @end
%%--------------------------------------------------------------------
-spec get_user_info(ProviderId :: atom(),
    AccessTokenSendMethod :: access_token_in_url | access_token_in_header,
    AccessToken :: binary()) ->
    {ok, #linked_account{}} | {error, bad_access_token}.
get_user_info(ProviderId, AccessTokenSendMethod, AccessToken) ->
    XRDS = get_xrds(ProviderId),
    get_user_info(ProviderId, AccessTokenSendMethod, AccessToken, XRDS).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user info for given OpenID provider and access token.
%% Allows to specify XRDS proplist (useful when it has been already obtained to
%% avoid repeating requests).
%% @end
%%--------------------------------------------------------------------
-spec get_user_info(ProviderId :: atom(),
    AccessTokenSendMethod :: access_token_in_url | access_token_in_header,
    AccessToken :: binary(),
    XRDS :: proplists:proplist()) ->
    {ok, #linked_account{}} | {error, bad_access_token}.
get_user_info(ProviderId, AccessTokenSendMethod, AccessToken, XRDS) ->
    UserInfoEndpoint = user_info_endpoint(XRDS),

    URL = case AccessTokenSendMethod of
        access_token_in_url ->
            <<UserInfoEndpoint/binary, "?access_token=", AccessToken/binary>>;
        access_token_in_header ->
            UserInfoEndpoint
    end,

    Headers = case AccessTokenSendMethod of
        access_token_in_url ->
            #{<<"Content-Type">> => <<"application/x-www-form-urlencoded">>};
        access_token_in_header ->
            #{<<"Authorization">> => <<"Bearer ", AccessToken/binary>>}
    end,

    % Send request to user info endpoint
    Response = http_client:get(URL, Headers, <<"">>),

    case Response of
        {ok, 200, _, Body} ->
            % Parse JSON with user info
            JSONProplist = json_utils:decode(Body),
            UserGroups = case auth_config:has_group_mapping_enabled(ProviderId) of
                false ->
                    [];
                true ->
                    HandlerModule = auth_config:get_provider_module(ProviderId),
                    HandlerModule:normalized_membership_specs(JSONProplist)
            end,
            ProvUserInfo = #linked_account{
                provider_id = ProviderId,
                user_id = auth_utils:get_value_binary(<<"sub">>, JSONProplist),
                login = auth_utils:get_value_binary(<<"login">>, JSONProplist),
                name = auth_utils:get_value_binary(<<"name">>, JSONProplist),
                email_list = auth_utils:extract_emails(JSONProplist),
                groups = UserGroups
            },
            {ok, ProvUserInfo};
        _ ->
            {error, bad_access_token}
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
-spec get_xrds(ProviderId :: atom()) -> proplists:proplist().
get_xrds(ProviderId) ->
    ProviderConfig = auth_config:get_auth_config(ProviderId),
    XRDSEndpoint = proplists:get_value(xrds_endpoint, ProviderConfig),
    Opts = [{follow_redirect, true}, {max_redirect, 5}],
    {ok, 200, _, XRDS} = http_client:get(XRDSEndpoint, #{}, <<>>, Opts),
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
