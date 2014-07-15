%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module implementing the business logic for OpenID Connect end-user
%% authentication and authorization.
%% @end
%% ===================================================================
-module(auth_logic).
-author("Konrad Zemek").

-define(AUTH_CODE, aUTH_CODE).
-define(ACCESS_TOKEN, access_token).
-define(REFRESH_TOKEN, refresh_token).
-define(TABLES, [?AUTH_CODE, ?ACCESS_TOKEN, ?REFRESH_TOKEN]).

%% @todo: config
-define(AUTH_CODE_EXPIRATION_SECS, 600).
-define(ACCESS_TOKEN_EXPIRATION_SECS, 36000).
-define(REFRESH_TOKEN_EXPIRATION_SECS, 36000).
-define(ISSUER_URL, "https://onedata.org").

-include_lib("ctool/include/logging.hrl").
-include("dao/dao_types.hrl").

%% API
-export([start/0, stop/0, get_redirection_uri/2, grant_token/2, validate_token/2]).


start() ->
    lists:foreach(fun(Table) -> ets:new(Table, [named_table, public]) end, ?TABLES).


stop() ->
    lists:foreach(fun(Table) -> ets:delete(Table) end, ?TABLES).


-spec get_redirection_uri(UserId :: binary(), ProviderId :: binary()) ->
    RedirectionUri :: binary().
get_redirection_uri(UserId, ProviderId) ->
    AuthCode = random_token(),
    ExpirationTime = now_s() + ?AUTH_CODE_EXPIRATION_SECS,
    ets:insert(?AUTH_CODE, {AuthCode, {ProviderId, UserId, ExpirationTime}}),
    {ok, ProviderData} = provider_logic:get_data(ProviderId),
    {redirectionPoint, RedirectURL} = lists:keyfind(redirectionPoint, 1, ProviderData),
    <<RedirectURL/binary, "?code=", AuthCode/binary>>.

-spec grant_token(ProviderId :: binary(), AuthCode :: binary()) ->
    [proplists:property()].
grant_token(ProviderId, AuthCode) ->
    [{AuthCode, {ProviderId, UserId, _ExpirationTime}}] = ets:lookup(?AUTH_CODE, AuthCode),
    ets:delete(?AUTH_CODE, AuthCode),

    AccessToken = random_token(),
    RefreshToken = random_token(),
    AccessTokenExpirationTime = now_s() + ?ACCESS_TOKEN_EXPIRATION_SECS,
    RefreshTokenExpirationTime = now_s() + ?REFRESH_TOKEN_EXPIRATION_SECS,
    ets:insert(?ACCESS_TOKEN, {AccessToken, {ProviderId, UserId, AccessTokenExpirationTime}}),
    ets:insert(?REFRESH_TOKEN, {RefreshToken, {ProviderId, UserId, RefreshTokenExpirationTime}}),

    {ok, #veil_document{record = #user{name = Name, email_list = Emails}}} = user_logic:get_user(UserId),
    EmailsList = lists:map(fun(Email) -> {struct, [{email, Email}]} end, Emails),
    [
        {access_token, AccessToken},
        {token_type, bearer},
        {expires_in, ?ACCESS_TOKEN_EXPIRATION_SECS},
        {refresh_token, RefreshToken},
        {scope, openid},
        {id_token, jwt_encode([
            {iss, ?ISSUER_URL},
            {sub, UserId},
            {aud, ProviderId},
            {name, Name},
            {email, EmailsList},
            {exp, wut}, %% @todo: expiration time
            {iat, now} %% @todo: now
        ])}
    ].


%% @todo:
%% validate_authorization_request(ProviderId, AuthCode) ->
%%     SavedData = ets:lookup(?AUTH_CODE, AuthCode),
%%     validate_authorization_request(ProviderId, AuthCode, SavedData).
%%
%% validate_authorization_request(ProviderId, AuthCode, []) -> [{error, invalid_grant}];
%% validate_authorization_request(ProviderId, AuthCode, [{IntendedProviderId, UserId}])
%%     when IntendedProviderId =/= ProviderId -> [{error, invalid_request}]


-spec validate_token(ProviderId :: binary(), AccessToken :: binary()) ->
    UserId :: binary().
validate_token(ProviderId, AccessToken) ->
    [{AccessToken, {ProviderId, UserId, _ExpirationTime}}] = ets:lookup(?ACCESS_TOKEN, AccessToken),
    UserId.


jwt_encode(Claims) ->
    Header = mochijson2:encode([{typ, 'JWT'}, {alg, none}]),
    Payload = mochijson2:encode(Claims),
    Header64 = mochiweb_base64url:encode(Header),
    Payload64 = mochiweb_base64url:encode(Payload),
    <<Header64/binary, ".", Payload64/binary, ".">>.


-spec random_token() -> string().
random_token() ->
    binary:list_to_bin(
        mochihex:to_hex(
            crypto:hash(sha,
                term_to_binary({make_ref(), node(), now()})))).


-spec now_s() -> integer().
now_s() ->
    {MegaSecs, Secs, _} = erlang:now(),
    MegaSecs * 1000000 + Secs.