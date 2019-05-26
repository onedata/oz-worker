%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions for OpenID Connect server mock. The mock supports only
%%% one IdP.
%%% @end
%%%-------------------------------------------------------------------
-module(oidc_server_mock).
-author("Lukasz Opiola").

-include("http/gui_paths.hrl").
-include("auth/auth_errors.hrl").
-include("oidc_server_mock.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

-export([mock/2, unmock/1, simulate_user_login/4]).


-spec mock(Config :: proplists:proplist(), #oidc_spec{}) -> ok.
mock(Config, OidcSpec) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, [openid_protocol], [passthrough]),
    ok = test_utils:mock_expect(Nodes, openid_protocol, request_idp, fun mock_request_idp/5),
    ok = test_utils:mock_expect(Nodes, openid_protocol, request_idp, fun mock_request_idp/6),
    testmaster_save_everywhere(Config, mock_spec, OidcSpec),

    ok.


unmock(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_unload(Nodes, [openid_protocol]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns {ok, RedirectUrl} if login simulation was successful, or error
%% if there are any inconsistencies in the config.
%% UserAttributes - attributes that will be returned upon successful login, in
%%      form of a map: #{Endpoint => Attributes}
%%      (as there can be more than one userinfo endpoint)
%% Return a mock of cowboy_req that would be generated during analogous redirect
%% in user's browser.
%% @end
%%--------------------------------------------------------------------
simulate_user_login(Config, OidcSpec, Url, UserAttributesByEndpoint) ->
    try
        #{qs := Qs} = url_utils:parse(Url),
        QsMap = maps:from_list(cowboy_req:parse_qs(#{qs => Qs})),
        #{
            <<"client_id">> := ClientId,
            <<"response_type">> := <<"code">>,
            <<"scope">> := Scope,
            <<"redirect_uri">> := OnezoneRedirectUri,
            <<"state">> := State
        } = QsMap,
        AccessType = maps:get(<<"access_type">>, QsMap, <<"online">>),
        ClientId = list_to_binary(OidcSpec#oidc_spec.clientId),
        Scope = list_to_binary(OidcSpec#oidc_spec.scope),
        AuthCode = datastore_utils:gen_key(),
        RefreshToken = case AccessType of
            <<"online">> -> undefined;
            <<"offline">> -> datastore_utils:gen_key()
        end,
        Token = datastore_utils:gen_key(),
        testmaster_save_everywhere(Config, {token, Token}, UserAttributesByEndpoint),
        testmaster_save_everywhere(Config, {auth, AuthCode}, {OnezoneRedirectUri, Token, RefreshToken}),
        RefreshToken /= undefined andalso
            testmaster_save_everywhere(Config, {refresh_token, RefreshToken}, UserAttributesByEndpoint),
        RedirectUrl = http_utils:append_url_parameters(OnezoneRedirectUri, #{
            <<"code">> => AuthCode,
            <<"state">> => State
        }),
        {ok, OzDomain} = oz_test_utils:get_oz_domain(Config),
        ExpPath = str_utils:format_bin("https://~s~s", [
            OzDomain, ?OIDC_CONSUME_PATH_DEPRECATED
        ]),
        Len = erlang:byte_size(ExpPath),
        ?assertMatch(<<ExpPath:Len/binary, _/binary>>, RedirectUrl),
        % Simulate browser redirect with the RedirectUrl
        #{qs := RedirectQs} = url_utils:parse(RedirectUrl),
        % Only QS parsing is done on the cowboy req, so its enough to omit other attrs
        MockedCowboyReq = #{qs => RedirectQs},
        {ok, MockedCowboyReq, Token}
    catch _:_ ->
        error
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec mock_request_idp(get | post, http_client:code(), http_client:url(),
    http_client:headers(), auth_logic:query_params()) ->
    {ResultHeaders :: http_client:headers(), ResultBody :: binary()}.
mock_request_idp(Method, ExpCode, Endpoint, Headers, Parameters) ->
    mock_request_idp(Method, ExpCode, Endpoint, Headers, Parameters, []).


-spec mock_request_idp(get | post, http_client:code(), http_client:url(),
    http_client:headers(), auth_logic:query_params(), http_client:opts()) ->
    {ResultHeaders :: http_client:headers(), ResultBody :: binary()}.
mock_request_idp(Method, ExpCode, Endpoint, Headers, Parameters, _Opts) ->
    {ok, OidcSpec} = onezone_get_saved(mock_spec),
    EndpointId = lookup_endpoint(Endpoint, OidcSpec),
    AccessTokenAcquireMethod = OidcSpec#oidc_spec.accessTokenAcquireMethod,
    {RespCode, RespHeaders, RespBody} = try
        case {Method, EndpointId} of
            {get, xrds} ->
                mock_xrds_endpoint(OidcSpec);
            {AccessTokenAcquireMethod, accessToken} ->
                mock_access_token_endpoint(OidcSpec, Headers, Parameters);
            {get, userInfo} ->
                mock_userinfo_endpoint(OidcSpec, Headers, Parameters);
            _ ->
                {404, #{}, <<"">>}
        end
    catch _:_ ->
        {400, #{}, <<"">>}
    end,
    case RespCode of
        ExpCode -> {RespHeaders, RespBody};
        _ -> throw(?ERROR_BAD_IDP_RESPONSE(Endpoint, RespCode, RespHeaders, RespBody))
    end.


mock_xrds_endpoint(OidcSpec) ->
    {200, #{<<"Content-Type">> => <<"application/json">>}, json_utils:encode(build_xrds(OidcSpec))}.


mock_access_token_endpoint(OidcSpec, Headers, #{<<"grant_type">> := <<"authorization_code">>} = Parameters) ->
    AuthCode = maps:get(<<"code">>, Parameters),
    RedirectUri = maps:get(<<"redirect_uri">>, Parameters),
    {ok, {ExpRedirectUri, Token, RefreshToken}} = onezone_get_saved({auth, AuthCode}),
    RedirectUri = ExpRedirectUri,
    mock_access_token_endpoint(OidcSpec, Headers, Parameters, Token, RefreshToken);

mock_access_token_endpoint(OidcSpec, Headers, #{<<"grant_type">> := <<"refresh_token">>} = Parameters) ->
    RefreshToken = maps:get(<<"refresh_token">>, Parameters),
    case onezone_get_saved({refresh_token, RefreshToken}) of
        {ok, UserAttributesByEndpoint} ->
            Token = datastore_utils:gen_key(),
            NewRefreshToken = datastore_utils:gen_key(),
            onezone_save_everywhere({refresh_token, RefreshToken}, UserAttributesByEndpoint),
            onezone_save_everywhere({token, Token}, UserAttributesByEndpoint),
            mock_access_token_endpoint(OidcSpec, Headers, Parameters, Token, NewRefreshToken);
        _ ->
            {403, #{}, <<"">>}
    end.

mock_access_token_endpoint(OidcSpec, Headers, Parameters, Token, RefreshToken) ->
    ClientSecretPassMethod = OidcSpec#oidc_spec.clientSecretPassMethod,
    ExpClientId = list_to_binary(OidcSpec#oidc_spec.clientId),
    ExpClientSecret = list_to_binary(OidcSpec#oidc_spec.clientSecret),
    AuthValid = case ClientSecretPassMethod of
        inAuthHeader ->
            AuthHeader = maps:get(<<"Authorization">>, Headers, <<"">>),
            ExpectedAuth = base64:encode(<<ExpClientId/binary, ":", ExpClientSecret/binary>>),
            <<"Basic ", ExpectedAuth/binary>> =:= AuthHeader;
        urlencoded ->
            ClientId = maps:get(<<"client_id">>, Parameters, <<"">>),
            ClientSecret = maps:get(<<"client_secret">>, Parameters, <<"">>),
            {ClientId, ClientSecret} =:= {ExpClientId, ExpClientSecret}
    end,

    ExpCustomData = maps:get(accessToken, OidcSpec#oidc_spec.customData, #{}),
    CustomDataValid = custom_data_valid(ExpCustomData, Headers, Parameters),
    Response1 = #{
        <<"access_token">> => Token,
        <<"expires_in">> => ?MOCK_ACCESS_TOKEN_TTL
    },
    Response2 = maps:merge(Response1, case RefreshToken of
        undefined -> #{};
        _ -> #{<<"refresh_token">> => RefreshToken}
    end),

    case {AuthValid, CustomDataValid} of
        {false, _} ->
            {403, #{}, <<"">>};
        {true, false} ->
            {400, #{}, <<"">>};
        {true, true} ->
            case rand:uniform(2) of
                1 ->
                    {200, #{
                        <<"Content-Type">> => <<"application/x-www-form-urlencoded">>
                    }, http_utils:encode_http_parameters(Response2)};
                2 ->
                    {200, #{
                        <<"Content-Type">> => <<"application/json">>
                    }, json_utils:encode(Response2)}
            end
    end.


mock_userinfo_endpoint(OidcSpec, Headers, Parameters) ->
    AccessTokenPassMethod = OidcSpec#oidc_spec.accessTokenPassMethod,
    Token = case AccessTokenPassMethod of
        inAuthHeader ->
            <<"Bearer ", AccessToken/binary>> = maps:get(<<"Authorization">>, Headers, <<"Bearer ">>),
            AccessToken;
        urlencoded ->
            maps:get(<<"access_token">>, Parameters, <<"">>)
    end,
    case onezone_get_saved({token, Token}) of
        {error, not_found} ->
            {403, #{}, <<"">>};
        {ok, UserAttributesByEndpoint} ->
            ExpCustomData = maps:get(userInfo, OidcSpec#oidc_spec.customData, #{}),
            case custom_data_valid(ExpCustomData, Headers, Parameters) of
                true ->
                    {200, #{}, json_utils:encode(UserAttributesByEndpoint)};
                false ->
                    {400, #{}, <<"">>}
            end
    end.


custom_data_valid(undefined, _Headers, _Parameters) ->
    true;
custom_data_valid(ExpCustomData, Headers, Parameters) ->
    ExpCustomHeaders = maps:get(headers, ExpCustomData, #{}),
    HeadersValid = maps:fold(fun(Key, Value, Acc) ->
        Acc andalso {ok, list_to_binary(Value)} =:= maps:find(list_to_binary(Key), Headers)
    end, true, ExpCustomHeaders),

    ExpCustomParameters = maps:get(parameters, ExpCustomData, #{}),
    ParametersValid = maps:fold(fun(Key, Value, Acc) ->
        Acc andalso {ok, list_to_binary(Value)} =:= maps:find(list_to_binary(Key), Parameters)
    end, true, ExpCustomParameters),

    HeadersValid andalso ParametersValid.


lookup_endpoint(Endpoint, #oidc_spec{endpoints = Endpoints}) ->
    maps:fold(fun
        (EndpointId, EndpointSpec, not_found) ->
            case resolve_endpoint(EndpointSpec) of
                {_, Endpoint} -> EndpointId;
                _ -> not_found
            end;
        (_, _, ResolvedEndpointId) ->
            ResolvedEndpointId
    end, not_found, Endpoints).


build_xrds(#oidc_spec{endpoints = Endpoints}) ->
    case maps:get(xrds, Endpoints, undefined) of
        undefined ->
            #{};
        _ ->
            AuthorizeEndpoint = maps:get(authorize, Endpoints),
            AccessTokenEndpoint = maps:get(accessToken, Endpoints),
            UserInfoEndpoint = maps:get(userInfo, Endpoints),
            AllEndpoints = lists:flatten([AuthorizeEndpoint, AccessTokenEndpoint, UserInfoEndpoint]),
            maps:from_list(lists:filtermap(fun(EndpointSpec) ->
                case resolve_endpoint(EndpointSpec) of
                    {{xrds, Key}, Url} ->
                        {true, {list_to_atom(Key), Url}};
                    {literal, _} ->
                        false
                end
            end, AllEndpoints))
    end.


resolve_endpoint({xrds, Key}) ->
    {{xrds, Key}, ?MOCK_ENDPOINT_FROM_XRDS(Key)};
resolve_endpoint({_, {xrds, Key}}) ->
    {{xrds, Key}, ?MOCK_ENDPOINT_FROM_XRDS(Key)};
resolve_endpoint({_, LiteralUrl}) ->
    {literal, LiteralUrl};
resolve_endpoint(LiteralUrl) ->
    {literal, LiteralUrl}.


testmaster_save_everywhere(Config, Key, Value) ->
    Nodes = ?config(oz_worker_nodes, Config),
    rpc:multicall(Nodes, simple_cache, put, [Key, Value]),
    ok.


onezone_save_everywhere(Key, Value) ->
    {ok, Nodes} = node_manager:get_cluster_nodes(),
    rpc:multicall(Nodes, simple_cache, put, [Key, Value]),
    ok.


onezone_get_saved(Key) ->
    simple_cache:get(Key).

