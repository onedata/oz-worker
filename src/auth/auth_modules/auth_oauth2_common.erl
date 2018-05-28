%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
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
-include("datastore/oz_datastore_models.hrl").

%% API
-export([
    get_redirect_url/2,
    validate_login/4,
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
-spec get_redirect_url(boolean(), auth_utils:idp()) ->
    {ok, binary()} | {error, term()}.
get_redirect_url(LinkAccount, IdP) ->
    try
        ParamsProplist = [
            {<<"client_id">>,
                auth_config:get_provider_app_id(IdP)},
            {<<"response_type">>,
                <<"code">>},
            {<<"scope">>,
                <<"openid email profile">>},
            {<<"redirect_uri">>,
                auth_utils:local_auth_endpoint()},
            {<<"state">>,
                auth_logic:generate_state_token(IdP, LinkAccount)}
        ],
        Params = http_utils:proplist_to_url_params(ParamsProplist),
        AuthorizeEndpoint = authorize_endpoint(get_xrds(IdP)),
        {ok, <<AuthorizeEndpoint/binary, "?", Params/binary>>}
    catch
        Type:Message ->
            ?error_stacktrace("Cannot get redirect URL for ~p",
                [IdP]),
            {error, {Type, Message}}
    end.


%%--------------------------------------------------------------------
%% @doc Validates login request that came back from the provider.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(auth_utils:idp(), QueryParams :: proplists:proplist(),
    SecretSendMethod :: secret_over_http_basic | secret_over_http_post,
    AccessTokenSendMethod :: access_token_in_url | access_token_in_header) ->
    {ok, #linked_account{}} | {error, term()}.
validate_login(IdP, QueryParams, SecretSendMethod, AccessTokenSendMethod) ->
    try
        % Parse out code parameter
        Code = proplists:get_value(<<"code">>, QueryParams),
        ClientId = auth_config:get_provider_app_id(IdP),
        ClientSecret = auth_config:get_provider_app_secret(IdP),
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
        XRDS = get_xrds(IdP),
        {ok, 200, _, ResponseBinary} = http_client:post(
            access_token_endpoint(XRDS),
            Headers,
            Params
        ),

        % Parse out received access token and form a user info request
        Response = json_utils:decode(ResponseBinary),
        AccessToken = maps:get(<<"access_token">>, Response, undefined),

        get_user_info(IdP, AccessTokenSendMethod, AccessToken, XRDS)
    catch
        Type:Message ->
            ?debug_stacktrace("Error in OpenID validate_login (~p) - ~p:~p",
                [IdP, Type, Message]),
            {error, {Type, Message}}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user info for given OpenID provider and access token.
%% @end
%%--------------------------------------------------------------------
-spec get_user_info(auth_utils:idp(),
    AccessTokenSendMethod :: access_token_in_url | access_token_in_header,
    AccessToken :: binary()) ->
    {ok, #linked_account{}} | {error, bad_access_token}.
get_user_info(IdP, AccessTokenSendMethod, AccessToken) ->
    XRDS = get_xrds(IdP),
    get_user_info(IdP, AccessTokenSendMethod, AccessToken, XRDS).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user info for given OpenID provider and access token.
%% Allows to specify XRDS proplist (useful when it has been already obtained to
%% avoid repeating requests).
%% @end
%%--------------------------------------------------------------------
-spec get_user_info(auth_utils:idp(),
    AccessTokenSendMethod :: access_token_in_url | access_token_in_header,
    AccessToken :: binary(),
    XRDS :: maps:map()) ->
    {ok, #linked_account{}} | {error, bad_access_token}.
get_user_info(IdP, AccessTokenSendMethod, AccessToken, XRDS) ->
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
            JSONMap = json_utils:decode(Body),
            UserGroups = case auth_config:has_group_mapping_enabled(IdP) of
                false ->
                    [];
                true ->
                    HandlerModule = auth_config:get_provider_module(IdP),
                    HandlerModule:normalized_membership_specs(IdP, JSONMap)
            end,
            ProvUserInfo = #linked_account{
                idp = IdP,
                subject_id = auth_utils:get_value_binary(<<"sub">>, JSONMap),
                login = auth_utils:get_value_binary(<<"login">>, JSONMap),
                name = auth_utils:get_value_binary(<<"name">>, JSONMap),
                email_list = auth_utils:extract_emails(JSONMap),
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
-spec get_xrds(IdP :: atom()) -> maps:map().
get_xrds(IdP) ->
    ProviderConfig = auth_config:get_auth_config(IdP),
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
-spec authorize_endpoint(XRDS :: maps:map()) -> binary().
authorize_endpoint(XRDS) ->
    maps:get(<<"authorization_endpoint">>, XRDS, undefined).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Provider endpoint, where access token is acquired.
%% @end
%%--------------------------------------------------------------------
-spec access_token_endpoint(XRDS :: maps:map()) -> binary().
access_token_endpoint(XRDS) ->
    maps:get(<<"token_endpoint">>, XRDS, undefined).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Provider endpoint, where user info is acquired.
%% @end
%%--------------------------------------------------------------------
-spec user_info_endpoint(XRDS :: maps:map()) -> binary().
user_info_endpoint(XRDS) ->
    maps:get(<<"userinfo_endpoint">>, XRDS, undefined).
