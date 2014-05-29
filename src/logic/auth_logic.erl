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

-define(AUTH_TOKEN, auth_token).
-define(ACCESS_TOKEN, access_token).
-define(REFRESH_TOKEN, refresh_token).
-define(TABLES, [?AUTH_TOKEN, ?ACCESS_TOKEN, ?REFRESH_TOKEN]).


%% API
-export([start/0, stop/0, get_redirection_uri/2, grant_token/2, validate_token/2]).


start() ->
    lists:foreach(fun(Table) -> ets:new(Table, [named_table, public]) end, ?TABLES).


stop() ->
    lists:foreach(fun(Table) -> ets:delete(Table) end, ?TABLES).


-spec get_redirection_uri(UserId :: binary(), ProviderId :: binary()) ->
    RedirectionUri :: binary().
get_redirection_uri(UserId, ProviderId) ->
    AuthToken = binary:list_to_bin(get_random_string(32)), %% @todo: configurable length
    ets:insert(?AUTH_TOKEN, {AuthToken, {ProviderId, UserId}}), %% @todo: expiration time
    ProviderData = provider_logic:get_data(ProviderId),
    [RedirectURL | _] = proplists:get_value(urls, ProviderData), %% @todo: check urls
    <<"https://", RedirectURL/binary, "/login?code=", AuthToken/binary>>. %% @todo: move most of this to provider_logic


-spec grant_token(ProviderId :: binary(), AuthToken :: binary()) ->
    [proplists:property()].
grant_token(ProviderId, AuthToken) ->
    [{IntendedProviderId, UserId}] = ets:lookup(?AUTH_TOKEN, AuthToken),
    ProviderId = IntendedProviderId, %% @todo: revoke
    ets:delete(?AUTH_TOKEN, AuthToken),

    AccessToken = binary:list_to_bin(get_random_string(32)),
    RefreshToken = binary:list_to_bin(get_random_string(32)),
    ets:insert(?ACCESS_TOKEN, {AccessToken, {ProviderId, UserId}}),
    ets:insert(?REFRESH_TOKEN, {RefreshToken, {ProviderId, UserId}}),

    [
        {access_token, AccessToken},
        {token_type, bearer},
        {expires_in, 3600}, %% @todo: custom expiration time
        {refresh_token, RefreshToken},
        {scope, openid},
        {id_token, jwt_encode([
            {iss, omg}, %% @todo: issuer url, can be also used to create rest cert
            {sub, UserId},
            {aud, ProviderId},
            {exp, wut}, %% @todo: expiration time
            {iat, now} %% @todo: now
        ])}
    ].


jwt_encode(Claims) ->
    Header = mochijson2:encode([{typ, 'JWT'}, {alg, none}]),
    Payload = mochijson2:encode(Claims),
    Header64 = mochiweb_base64url:encode(Header),
    Payload64 = mochiweb_base64url:encode(Payload),
    <<Header64/binary, ".", Payload64/binary, ".">>.


-spec validate_token(ProviderId :: binary(), AccessToken :: binary()) ->
    UserId :: binary().
validate_token(ProviderId, AccessToken) ->
    [{IntendedProviderId, UserId}] = ets:lookup(?ACCESS_TOKEN, AccessToken),
    ProviderId = IntendedProviderId, %% @todo: revoke
    UserId.


%% @todo: deduplicate (grpca)
-spec get_random_string(Length :: integer()) -> string().
get_random_string(Length) ->
    AllowedChars = "qwertyuiopasdfghjklzxcvbnm1234567890",
    lists:foldl(fun(_, Acc) ->
        [lists:nth(random:uniform(length(AllowedChars)), AllowedChars) | Acc]
    end, [], lists:seq(1, Length)).
