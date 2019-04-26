%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin implementing openid_plugin_behaviour, used to handle standard
%%% OpenID Connect communication with an IdP server. These callbacks are called
%%% by openid_protocol module.
%%% @end
%%%-------------------------------------------------------------------
-module(default_oidc_plugin).
-behavior(openid_plugin_behaviour).
-author("Lukasz Opiola").

-include("auth/auth_common.hrl").
-include_lib("ctool/include/api_errors.hrl").

% Default configuration of the handler
-define(DEFAULT_SCOPE, "openid email profile").
-define(DEFAULT_ACCESS_TOKEN_ACQUIRE_METHOD, post).
-define(DEFAULT_CLIENT_SECRET_PASS_METHOD, urlencoded).
-define(DEFAULT_ACCESS_TOKEN_PASS_METHOD, urlencoded).
-define(DEFAULT_ENDPOINT(__Type), case __Type of
    authorize -> {xrds, "authorization_endpoint"};
    accessToken -> {xrds, "token_endpoint"};
    userInfo -> {xrds, "userinfo_endpoint"}
end).
-define(ASSUMED_TOKEN_LIFESPAN, oz_worker:get_env(assumed_idp_access_token_lifespan, 3600)).


% Macros for reading certain config params of given IdP
-define(cfg(__IdP, __Path, __Policy), auth_config:get_protocol_config(__IdP, __Path, __Policy)).

-define(CFG_CLIENT_ID(__IdP),
    ?bin(?cfg(__IdP, [pluginConfig, clientId], required))
).
-define(CFG_CLIENT_SECRET(__IdP),
    ?bin(?cfg(__IdP, [pluginConfig, clientSecret], required))
).
-define(CFG_ENDPOINT(__IdP, __Type),
    ?cfg(__IdP, [pluginConfig, endpoints, __Type], {default, ?DEFAULT_ENDPOINT(__Type)})
).
-define(CFG_XRDS_ENDPOINT(__IdP),
    ?bin(?cfg(__IdP, [pluginConfig, endpoints, xrds], required))
).
-define(CFG_SCOPE(__IdP),
    ?bin(?cfg(__IdP, [pluginConfig, scope], {default, ?DEFAULT_SCOPE}))
).
-define(CFG_PROMPT(__IdP),
    ?bin(?cfg(__IdP, [pluginConfig, prompt], {default, undefined}))
).
-define(CFG_ACCESS_TOKEN_ACQUIRE_METHOD(__IdP),
    ?cfg(__IdP, [pluginConfig, accessTokenAcquireMethod], {default, ?DEFAULT_ACCESS_TOKEN_ACQUIRE_METHOD})
).
-define(CFG_CLIENT_SECRET_PASS_METHOD(__IdP),
    ?cfg(__IdP, [pluginConfig, clientSecretPassMethod], {default, ?DEFAULT_CLIENT_SECRET_PASS_METHOD})
).
-define(CFG_ACCESS_TOKEN_PASS_METHOD(__IdP),
    ?cfg(__IdP, [pluginConfig, accessTokenPassMethod], {default, ?DEFAULT_ACCESS_TOKEN_PASS_METHOD})
).
-define(CFG_CUSTOM_HEADERS(__IdP, __Endpoint),
    ?cfg(__IdP, [pluginConfig, customData, __Endpoint, headers], {default, #{}})
).
-define(CFG_CUSTOM_PARAMETERS(__IdP, __Endpoint),
    ?cfg(__IdP, [pluginConfig, customData, __Endpoint, parameters], {default, #{}})
).


%% openid_plugin_behaviour callbacks
-export([
    get_login_endpoint/3,
    validate_login/3,
    refresh_access_token/2,
    get_user_info/2
]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link openid_plugin_behaviour} callback get_login_endpoint/3.
%% @end
%%--------------------------------------------------------------------
-spec get_login_endpoint(auth_config:idp(), state_token:id(),
    auth_logic:redirect_uri()) ->
    auth_logic:login_endpoint().
get_login_endpoint(IdP, State, RedirectUri) ->
    Params1 = #{
        <<"client_id">> => ?CFG_CLIENT_ID(IdP),
        <<"response_type">> => <<"code">>,
        <<"scope">> => ?CFG_SCOPE(IdP),
        <<"redirect_uri">> => RedirectUri,
        <<"state">> => State
    },
    Params2 = case auth_config:has_offline_access_enabled(IdP) of
        true -> Params1#{<<"access_type">> => <<"offline">>};
        false -> Params1
    end,
    Params3 = case ?CFG_PROMPT(IdP) of
        undefined -> Params2;
        Prompt -> Params2#{<<"prompt">> => Prompt}
    end,
    Params4 = parameters_append_custom(Params3, IdP, authorize),
    AuthorizeEndpoint = idp_endpoint(IdP, authorize),
    http_utils:append_url_parameters(AuthorizeEndpoint, Params4).


%%--------------------------------------------------------------------
%% @doc
%% {@link openid_plugin_behaviour} callback validate_login/3.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(auth_config:idp(), auth_logic:query_params(),
    auth_logic:redirect_uri()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
validate_login(IdP, #{<<"code">> := Code}, RedirectUri) ->
    {AccessToken, Expires, RefreshToken} = acquire_access_token(IdP, #{
        <<"code">> => Code,
        <<"redirect_uri">> => RedirectUri,
        <<"grant_type">> => <<"authorization_code">>
    }),
    {ok, Attributes} = get_user_info(IdP, AccessToken),
    case auth_config:has_offline_access_enabled(IdP) of
        false ->
            {ok, Attributes};
        true ->
            {ok, Attributes#{
                <<"access_token">> => {AccessToken, Expires},
                <<"refresh_token">> => RefreshToken
            }}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link openid_plugin_behaviour} callback refresh_access_token/2.
%% @end
%%--------------------------------------------------------------------
-spec refresh_access_token(auth_config:idp(), auth_logic:refresh_token()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
refresh_access_token(IdP, RefreshToken) ->
    case auth_config:has_offline_access_enabled(IdP) of
        false ->
            ?ERROR_NOT_SUPPORTED;
        true ->
            {NewAccessToken, Expires, NewRefreshToken} = acquire_access_token(IdP, #{
                <<"grant_type">> => <<"refresh_token">>,
                <<"refresh_token">> => RefreshToken,
                <<"scope">> => ?CFG_SCOPE(IdP)
            }),
            {ok, Attributes} = get_user_info(IdP, NewAccessToken),
            {ok, Attributes#{
                <<"access_token">> => {NewAccessToken, Expires},
                <<"refresh_token">> => NewRefreshToken
            }}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link openid_plugin_behaviour} callback get_user_info/2.
%% @end
%%--------------------------------------------------------------------
-spec get_user_info(auth_config:idp(), auth_logic:access_token()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
get_user_info(IdP, AccessToken) ->
    Parameters1 = parameters_append_access_token(#{}, IdP, AccessToken),
    Parameters2 = parameters_append_custom(Parameters1, IdP, userInfo),

    Headers1 = case Parameters2 of
        Map when map_size(Map) == 0 -> #{};
        _ -> #{<<"content-type">> => <<"application/x-www-form-urlencoded">>}
    end,
    Headers2 = headers_append_access_token(Headers1, IdP, AccessToken),
    Headers3 = headers_append_custom(Headers2, IdP, userInfo),

    UserInfoEndpoints = case idp_endpoint(IdP, userInfo) of
        SingleUrl when is_binary(SingleUrl) -> [SingleUrl];
        Urls when is_list(Urls) -> Urls
    end,

    % If there is more than one userinfo endpoint to query, the results are
    % merged.
    {ok, lists:foldl(fun(Endpoint, AccAttributes) ->
        Attributes = case Endpoint of
            {AttrKey, Url} ->
                #{AttrKey => request_user_info(Url, Parameters2, Headers3)};
            Url ->
                request_user_info(Url, Parameters2, Headers3)
        end,
        maps:merge(AccAttributes, Attributes)
    end, #{}, UserInfoEndpoints)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec acquire_access_token(auth_config:idp(), auth_logic:query_params()) ->
    auth_config:access_token().
acquire_access_token(IdP, Parameters) ->
    Parameters2 = parameters_append_auth(Parameters, IdP),
    Parameters3 = parameters_append_custom(Parameters2, IdP, accessToken),

    Headers1 = #{
        <<"content-type">> => <<"application/x-www-form-urlencoded">>
    },
    Headers2 = headers_append_auth(Headers1, IdP),
    Headers3 = headers_append_custom(Headers2, IdP, accessToken),

    AccessTokenEndpoint = idp_endpoint(IdP, accessToken),
    Method = ?CFG_ACCESS_TOKEN_ACQUIRE_METHOD(IdP),
    {RespHeaders, RespBinary} = openid_protocol:request_idp(
        Method, 200, AccessTokenEndpoint, Headers3, Parameters3
    ),

    case maps:get(<<"content-type">>, RespHeaders, maps:get(<<"Content-Type">>, RespHeaders, undefined)) of
        <<"application/json", _/binary>> ->
            Response = json_utils:decode(RespBinary),
            AccessToken = maps:get(<<"access_token">>, Response, undefined),
            ExpiresIn = maps:get(<<"expires_in">>, Response, ?ASSUMED_TOKEN_LIFESPAN),
            RefreshToken = maps:get(<<"refresh_token">>, Response, undefined),
            {AccessToken, time_utils:cluster_time_seconds() + ExpiresIn, RefreshToken};
        _ ->
            Response = cow_qs:parse_qs(RespBinary),
            AccessToken = proplists:get_value(<<"access_token">>, Response, undefined),
            ExpiresIn = case proplists:get_value(<<"expires_in">>, Response, undefined) of
                undefined -> ?ASSUMED_TOKEN_LIFESPAN;
                ValueBin -> binary_to_integer(ValueBin)
            end,
            RefreshToken = proplists:get_value(<<"refresh_token">>, Response, undefined),
            {AccessToken, time_utils:cluster_time_seconds() + ExpiresIn, RefreshToken}
    end.


%% @private
-spec request_user_info(http_client:url(), auth_logic:query_params(),
    http_client:headers()) -> ParsedJson :: #{}.
request_user_info(URL, Parameters, Headers) ->
    {_, ResponseBody} = openid_protocol:request_idp(get, 200, URL, Headers, Parameters),
    json_utils:decode(ResponseBody).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Resolves an endpoint for given IdP based on auth.config. There can be more than
%% one endpoint of given type (typically that applies only to the userinfo endpoint).
%% Possible Urls in config are:
%%      - literal url string
%%      - {xrds, "param"} tuple, where "param" is one of the fields in Xrds JSON
%%          if such entry is present, an entry for 'xrds' key must be present
%%          (and be a literal url)
%% When userinfo endpoint is concerned, it is possible to specify an Attr key,
%% under which the data will be placed in resulting attributes (see "emails" and
%% "config" below). If no attr key is specified, the attributes are placed
%% directly in the result attributes map.
%%
%% Example auth.config can look like this:
%%
%%   pluginConfig => #{
%%       endpoints => #{
%%           xrds => "https://my-idp.com/.well-known/openid-configuration",
%%           authorize => {xrds, "authorization_endpoint"},
%%           accessToken => {xrds, "token_endpoint"},
%%           userInfo => [
%%               {xrds, "userinfo_endpoint"},
%%               "https://my-idp.com/extra-user-info",
%%               {"emails", "https://my-idp.com/user-emails"},
%%               {"custom", {xrds, "custom_data_endpoint"}}
%%           ]
%%       }
%%   }
%% @end
%%--------------------------------------------------------------------
-spec idp_endpoint(auth_config:idp(), openid_protocol:endpoint_type()) ->
    Entry | [Entry] when Entry :: Url :: binary() | {Attr :: binary(), Url :: binary()}.
idp_endpoint(IdP, Type) ->
    case ?CFG_ENDPOINT(IdP, Type) of
        Entries = [F | _] when is_list(F) or is_tuple(F) ->
            lists:map(fun(Entry) -> resolve_endpoint(IdP, Entry) end, Entries);
        Entry ->
            resolve_endpoint(IdP, Entry)
    end.


%% @private
-spec resolve_endpoint(auth_config:idp(), Rule :: term()) ->
    Url :: binary() | {Attr :: binary(), Url :: binary()}.
resolve_endpoint(IdP, {xrds, JsonKey}) ->
    maps:get(?bin(JsonKey), get_xrds(IdP));
resolve_endpoint(IdP, {AttrKey, UrlOrXrds}) ->
    {?bin(AttrKey), resolve_endpoint(IdP, UrlOrXrds)};
resolve_endpoint(_IdP, Url) ->
    ?bin(Url).


%% @private
-spec get_xrds(auth_config:idp()) -> maps:map().
get_xrds(IdP) ->
    {ok, Xrds} = simple_cache:get({cached_xrds, IdP}, fun() ->
        {true, fetch_xrds(IdP), ?XRDS_CACHE_TTL}
    end),
    Xrds.


%% @private
-spec fetch_xrds(auth_config:idp()) -> maps:map().
fetch_xrds(IdP) ->
    XrdsEndpoint = ?CFG_XRDS_ENDPOINT(IdP),
    Params = parameters_append_custom(#{}, IdP, xrds),
    Url = http_utils:append_url_parameters(XrdsEndpoint, Params),
    Headers = headers_append_custom(#{}, IdP, xrds),
    Opts = [{follow_redirect, true}, {max_redirect, 5}],
    {_, Xrds} = openid_protocol:request_idp(get, 200, Url, Headers, #{}, Opts),
    json_utils:decode(Xrds).


%% @private
-spec headers_append_auth(http_client:headers(), auth_config:idp()) ->
    http_client:headers().
headers_append_auth(Headers, IdP) ->
    case ?CFG_CLIENT_SECRET_PASS_METHOD(IdP) of
        inAuthHeader ->
            ClientId = ?CFG_CLIENT_ID(IdP),
            ClientSecret = ?CFG_CLIENT_SECRET(IdP),
            B64 = base64:encode(<<ClientId/binary, ":", ClientSecret/binary>>),
            Headers#{<<"authorization">> => <<"Basic ", B64/binary>>};
        urlencoded ->
            Headers
    end.


%% @private
-spec headers_append_access_token(http_client:headers(), auth_config:idp(),
    auth_logic:access_token()) -> http_client:headers().
headers_append_access_token(Headers, IdP, AccessToken) ->
    case ?CFG_ACCESS_TOKEN_PASS_METHOD(IdP) of
        inAuthHeader ->
            Headers#{<<"authorization">> => <<"Bearer ", AccessToken/binary>>};
        urlencoded ->
            Headers
    end.


%% @private
-spec headers_append_custom(http_client:headers(), auth_config:idp(),
    openid_protocol:endpoint_type()) -> http_client:headers().
headers_append_custom(Headers, IdP, EndpointType) ->
    case ?CFG_CUSTOM_HEADERS(IdP, EndpointType) of
        undefined ->
            Headers;
        Custom ->
            maps:fold(fun(Key, Value, Acc) ->
                Acc#{?bin(Key) => ?bin(Value)}
            end, Headers, Custom)
    end.


%% @private
-spec parameters_append_auth(auth_logic:query_params(), auth_config:idp()) ->
    auth_logic:query_params().
parameters_append_auth(Parameters, IdP) ->
    case ?CFG_CLIENT_SECRET_PASS_METHOD(IdP) of
        urlencoded ->
            Parameters#{
                <<"client_id">> => ?CFG_CLIENT_ID(IdP),
                <<"client_secret">> => ?CFG_CLIENT_SECRET(IdP)
            };
        inAuthHeader ->
            Parameters
    end.


%% @private
-spec parameters_append_access_token(auth_logic:query_params(), auth_config:idp(),
    auth_logic:access_token()) -> auth_logic:query_params().
parameters_append_access_token(Parameters, IdP, AccessToken) ->
    case ?CFG_ACCESS_TOKEN_PASS_METHOD(IdP) of
        urlencoded -> Parameters#{<<"access_token">> => AccessToken};
        inAuthHeader -> Parameters
    end.


%% @private
-spec parameters_append_custom(auth_logic:query_params(), auth_config:idp(),
    openid_protocol:endpoint_type()) -> auth_logic:query_params().
parameters_append_custom(Parameters, IdP, EndpointType) ->
    case ?CFG_CUSTOM_PARAMETERS(IdP, EndpointType) of
        undefined ->
            Parameters;
        Custom ->
            maps:fold(fun(Key, Value, Acc) ->
                Acc#{?bin(Key) => ?bin(Value)}
            end, Parameters, Custom)
    end.
