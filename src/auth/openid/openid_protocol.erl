%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin implementing auth_protocol_behaviour, used to handle authentication
%%% over OpenID protocol. These callbacks are called by auth_logic module.
%%% @end
%%%-------------------------------------------------------------------
-module(openid_protocol).
-behavior(auth_protocol_behaviour).
-author("Lukasz Opiola").

-include("auth/auth_errors.hrl").
-include("http/gui_paths.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/logging.hrl").

-type endpoint_type() :: xrds | authorize | accessToken | userInfo.
-export_type([endpoint_type/0]).

-define(redirect_uri, oz_worker:get_uri(<<?OIDC_CONSUME_PATH_DEPRECATED>>)).

-define(CFG_OPENID_PLUGIN(__IdP), auth_config:get_protocol_config(
    IdP, [plugin], {default, default_oidc_plugin}
)).

%% auth_protocol_behaviour callbacks
-export([get_login_endpoint/2, validate_login/2]).

%% API
-export([authorize_by_idp_access_token/1]).
-export([request_idp/5, request_idp/6]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link auth_protocol_behaviour} callback get_login_endpoint/2.
%% @end
%%--------------------------------------------------------------------
-spec get_login_endpoint(auth_config:idp(), state_token:id()) ->
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
%% {@link auth_protocol_behaviour} callback validate_login/2.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(auth_config:idp(), auth_logic:query_params()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
validate_login(IdP, QueryParams) ->
    call_plugin(IdP, validate_login, [IdP, QueryParams, ?redirect_uri]).


%%--------------------------------------------------------------------
%% @doc
%% Tries to a authorize a client by an access token originating from an
%% Identity Provider. Its prefix must match any of the configured openid
%% providers supporting authority delegation.
%% {true, Client} - client was authorized
%% false - this method cannot verify authorization, other methods should be tried
%% {error, term()} - authorization invalid
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_idp_access_token(AccessToken :: binary()) ->
    {true, entity_logic:client()} | false | {error, term()}.
authorize_by_idp_access_token(AccessTokenWithPrefix) ->
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
                    ?ERROR_BAD_EXTERNAL_ACCESS_TOKEN(IdP);
                throw:?ERROR_BAD_IDP_RESPONSE(_, 403, _, _) ->
                    ?ERROR_BAD_EXTERNAL_ACCESS_TOKEN(IdP);
                Type:Reason ->
                    ?error_stacktrace(
                        "Unexpected error during authorization by external access token - ~p:~p",
                        [Type, Reason]
                    ),
                    ?ERROR_INTERNAL_SERVER_ERROR
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% @equiv request_idp(Method, ExpCode, Endpoint, Headers, Parameters, [])
%% @end
%%--------------------------------------------------------------------
-spec request_idp(get | post, http_client:code(), http_client:url(),
    http_client:headers(), auth_logic:query_params()) ->
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
    http_client:headers(), auth_logic:query_params(), http_client:opts()) ->
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
