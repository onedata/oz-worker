%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for interacting with oz-worker's HTTP servers
%%% (including REST) in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_http).
-author("Lukasz Opiola").

-include("ozt.hrl").

-type audience_token() :: undefined | tokens:token() | tokens:serialized().
-type urn_tokens() :: binary() | [binary()].

%% API
-export([simulate_login/1]).
-export([rest_call/3, rest_call/4, rest_call/5]).
-export([build_url/1, build_url/2]).
-export([ssl_opts/0, get_ca_certs/0]).

%%%===================================================================
%%% API
%%%==================================================6=================

-spec simulate_login(od_user:id()) -> session:id().
simulate_login(UserId) ->
    MockedReq = #{},
    CookieKey = ?SESSION_COOKIE_KEY,
    #{resp_cookies := #{CookieKey := CookieIoList}} = ?assertMatch(#{}, ozt:rpc(
        gui_session, log_in, [UserId, MockedReq]
    )),
    Cookie = iolist_to_binary(CookieIoList),
    CookieLen = byte_size(?SESSION_COOKIE_KEY),
    [<<CookieKey:CookieLen/binary, "=", CookieVal/binary>> | _] = binary:split(Cookie, <<";">>, [global, trim_all]),
    ozt:rpc(gui_session, get_session_id, [CookieVal]).


-spec rest_call(gs_protocol:client_auth(), http_client:method(), urn_tokens()) ->
    {ok, json_utils:json_term()} | errors:error().
rest_call(ClientAuth, Method, UrnTokens) ->
    rest_call(ClientAuth, Method, UrnTokens, #{}).

-spec rest_call(gs_protocol:client_auth(), audience_token(), http_client:method(), urn_tokens()) ->
    {ok, json_utils:json_term()} | errors:error().
rest_call(ClientAuth, Method, UrnTokens, DataJson) ->
    rest_call(ClientAuth, undefined, Method, UrnTokens, DataJson).

-spec rest_call(gs_protocol:client_auth(), audience_token(), http_client:method(),
    urn_tokens(), json_utils:json_term()) ->
    {ok, json_utils:json_term()} | errors:error().
rest_call(ClientAuth, AudienceToken, Method, UrnTokens, DataJson) ->
    Url = build_rest_url(UrnTokens),
    AuthHeader = case ClientAuth of
        undefined -> #{};
        nobody -> #{};
        {token, Token} -> tokens:build_access_token_header(ozt_tokens:ensure_serialized(Token))
    end,
    AudienceHeader = case AudienceToken of
        undefined -> #{};
        AudToken -> tokens:build_audience_token_header(ozt_tokens:ensure_serialized(AudToken))
    end,
    Headers = maps:merge(AuthHeader#{?HDR_CONTENT_TYPE => <<"application/json">>}, AudienceHeader),
    Opts = [
        {ssl_options, ssl_opts()},
        {connect_timeout, timer:seconds(30)},
        {recv_timeout, timer:seconds(30)}
    ],
    case http_client:request(Method, Url, Headers, json_utils:encode(DataJson), Opts) of
        {ok, OkCode, _, Body} when OkCode >= 200 andalso OkCode < 300 ->
            {ok, json_utils:decode(Body)};
        {ok, Code, _, <<"">>} ->
            {error, {http_code, Code}};
        {ok, _, _, ErrorBody} ->
            #{<<"error">> := ErrorJson} = json_utils:decode(ErrorBody),
            errors:from_json(ErrorJson);
        Other ->
            ct:pal("REST call to oz-worker failed unexpectedly with: ~tp", [Other]),
            error(rest_call_failed)
    end.


-spec build_rest_url(urn_tokens()) -> http_client:url().
build_rest_url(UrnTokens) ->
    RestPrefix = list_to_binary(ozt:get_env(rest_api_prefix)),
    build_url(lists:flatten([RestPrefix, UrnTokens])).


-spec build_url(urn_tokens()) -> http_client:url().
build_url(UrnTokens) ->
    build_url(https, UrnTokens).

-spec build_url(http | https | wss, urn_tokens()) -> http_client:url().
build_url(Scheme, Tokens) when is_list(Tokens) ->
    build_url(Scheme, str_utils:join_binary(Tokens));
build_url(Scheme, Urn) ->
    PortStr = case ozt:get_env(https_server_port) of
        443 -> <<"">>;
        Port -> <<":", Port/binary>>
    end,
    str_utils:format_bin("~s://~s~s~s", [
        Scheme,
        ozt:get_domain(),
        PortStr,
        Urn
    ]).


-spec ssl_opts() -> [http_client:ssl_opt()].
ssl_opts() ->
    [{secure, only_verify_peercert}, {cacerts, get_ca_certs()}].


-spec get_ca_certs() -> [public_key:der_encoded()].
get_ca_certs() ->
    ozt:rpc(https_listener, get_cert_chain_pems, []).