%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin implementing idp_auth_protocol_behaviour, used to handle authentication
%%% over OpenID protocol. These callbacks are called by auth_logic module.
%%% @end
%%%-------------------------------------------------------------------
-module(openid_protocol).
-behavior(idp_auth_protocol_behaviour).
-author("Lukasz Opiola").

-include("auth/auth_errors.hrl").
-include("http/gui_paths.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").

-type endpoint_type() :: xrds | authorize | accessToken | userInfo.
-export_type([endpoint_type/0]).

-define(redirect_uri, oz_worker:get_uri(<<?OIDC_CONSUME_PATH_DEPRECATED>>)).

-define(CFG_OPENID_PLUGIN(__IdP), auth_config:get_protocol_config(
    IdP, [plugin], {default, default_oidc_plugin}
)).

%% idp_auth_protocol_behaviour callbacks
-export([get_login_endpoint/2, validate_login/2]).

%% API
-export([authenticate_by_idp_access_token/1]).
-export([refresh_idp_access_token/2]).
-export([request_idp/5, request_idp/6]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link idp_auth_protocol_behaviour} callback get_login_endpoint/2.
%% @end
%%--------------------------------------------------------------------
-spec get_login_endpoint(auth_config:idp(), state_token:state_token()) ->
    {ok, #{binary() => binary() | null}}.
get_login_endpoint(IdP, State) ->
    Url = call_plugin(IdP, get_login_endpoint, [IdP, State, ?redirect_uri]),
    {ok, #{
        <<"method">> => <<"get">>,
        <<"url">> => Url,
        <<"formData">> => null
    }}.


%%--------------------------------------------------------------------
%% @doc
%% {@link idp_auth_protocol_behaviour} callback validate_login/2.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(auth_config:idp(), idp_auth:query_params()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
validate_login(IdP, QueryParams) ->
    call_plugin(IdP, validate_login, [IdP, QueryParams, ?redirect_uri]).


%%--------------------------------------------------------------------
%% @doc
%% Tries to authenticate a client by an access token originating from an
%% Identity Provider. The token must be prefixed with a string that matches any
%% of the configured openid providers supporting authority delegation
%% (tokenPrefix field in the auth.config).
%%   {true, #auth{}} - the client was authenticated
%%   false - access token was not found
%%   {error, term()} - provided access token was invalid
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_idp_access_token(AccessToken :: binary()) ->
    {true, {auth_config:idp(), attribute_mapping:idp_attributes()}} | false | {error, term()}.
authenticate_by_idp_access_token(AccessTokenWithPrefix) ->
    case auth_config:find_openid_idp_by_access_token(AccessTokenWithPrefix) of
        false ->
            false;
        {true, {IdP, TokenPrefix}} ->
            try
                Len = byte_size(TokenPrefix),
                <<TokenPrefix:Len/binary, AccessToken/binary>> = AccessTokenWithPrefix,
                {ok, Attributes} = call_plugin(IdP, get_user_info, [IdP, AccessToken]),
                {true, {IdP, Attributes}}
            catch
                throw:?ERROR_BAD_IDP_RESPONSE(_, 401, _, _) ->
                    ?ERROR_UNAUTHORIZED(?ERROR_BAD_IDP_ACCESS_TOKEN(IdP));
                throw:?ERROR_BAD_IDP_RESPONSE(_, 403, _, _) ->
                    ?ERROR_UNAUTHORIZED(?ERROR_BAD_IDP_ACCESS_TOKEN(IdP));
                Type:Reason ->
                    ?error_stacktrace(
                        "Unexpected error during authentication by IdP access token - ~p:~p",
                        [Type, Reason]
                    ),
                    ?ERROR_UNAUTHORIZED(?ERROR_INTERNAL_SERVER_ERROR)
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Acquires a new access token using given refresh token.
%% @end
%%--------------------------------------------------------------------
-spec refresh_idp_access_token(auth_config:idp(), idp_auth:refresh_token()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
refresh_idp_access_token(IdP, RefreshToken) ->
    call_plugin(IdP, refresh_access_token, [IdP, RefreshToken]).


%%--------------------------------------------------------------------
%% @doc
%% @equiv request_idp(Method, ExpCode, Endpoint, Headers, Parameters, [])
%% @end
%%--------------------------------------------------------------------
-spec request_idp(get | post, http_client:code(), http_client:url(),
    http_client:headers(), idp_auth:query_params()) ->
    {ResultHeaders :: http_client:headers(), ResultBody :: binary()}.
request_idp(Method, ExpCode, Endpoint, Headers, Parameters) ->
    request_idp(Method, ExpCode, Endpoint, Headers, Parameters, []).


%%--------------------------------------------------------------------
%% @doc
%% Sends a HTTP request to given endpoint and asserts if returned code is
%% the same as expected. If so, returns the response body. If not, generates
%% a proper error.
%% Parameters are appended to URL or encoded into request body, depending on
%% Method (get or post).
%% @end
%%--------------------------------------------------------------------
-spec request_idp(get | post, http_client:code(), http_client:url(),
    http_client:headers(), idp_auth:query_params(), http_client:opts()) ->
    {ResultHeaders :: http_client:headers(), ResultBody :: binary()}.
request_idp(Method, ExpCode, Endpoint, Headers, Parameters, Opts) ->
    {Url, Body} = case Method of
        get -> {http_utils:append_url_parameters(Endpoint, Parameters), <<"">>};
        post -> {Endpoint, http_utils:encode_http_parameters(Parameters)}
    end,
    case http_client:request(Method, Url, Headers, Body, Opts) of
        {ok, ExpCode, ResultHeaders, ResultBody} ->
            {ResultHeaders, ResultBody};
        {ok, Code, ResultHeaders, ResultBody} ->
            throw(?ERROR_BAD_IDP_RESPONSE(Endpoint, Code, ResultHeaders, ResultBody));
        {error, Reason} ->
            throw(?ERROR_IDP_UNREACHABLE({error, Reason}))
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec call_plugin(auth_config:idp(), openid_plugin_behaviour:callback(), [term()]) ->
    term().
call_plugin(IdP, Callback, Args) ->
    Plugin = ?CFG_OPENID_PLUGIN(IdP),
    apply(Plugin, Callback, Args).
