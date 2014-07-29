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

-include("dao/dao_types.hrl").
-include("auth_common.hrl").
-include_lib("ctool/include/logging.hrl").

%% @todo: openid tokens need to be persisted in the database. refresh token
%% shouldn't expire. the whole chain of authorization and refresh tokens should
%% be known as session, identifiable by session id.
-define(AUTH_CODE, auth_code). %% {AuthCode, {ExpirationTime, {UserId, ProviderId}}} [1:1]
-define(ACCESS_TOKEN, access_token). %% {AccessToken, AccessId} [1:1]
-define(REFRESH_TOKEN, refresh_token). %% {RefreshToken, AccessId} [1:1]
-define(USER_ID, user_id). %% {UserId, AccessId} [1:n]
-define(ACCESS, access). %% {AccessId, {ExpirationTime, {AccessToken, RefreshToken, UserId, ProviderId, ClientName}}} [1:1]
-define(STATE_TOKEN, state_token).

-define(TABLES_SET, [?AUTH_CODE, ?ACCESS_TOKEN, ?REFRESH_TOKEN, ?ACCESS, ?STATE_TOKEN]).
-define(TABLES_BAG, [?USER_ID]).

%% @todo: config
-define(AUTH_CODE_EXPIRATION_SECS, 600).
-define(ACCESS_EXPIRATION_SECS, 36000).
-define(STATE_TOKEN_EXPIRATION_SECS, 60).
-define(ISSUER_URL, <<"https://onedata.org">>).


%% ====================================================================
%% API
%% ====================================================================
-export([start/0, stop/0, get_redirection_uri/2, gen_auth_code/1,
    has_access/2, delete_access/1,  get_user_tokens/1,  grant_tokens/2,
    validate_token/2]).

%% ====================================================================
%% Handling state tokens
%% ====================================================================
-export([generate_state_token/2, lookup_state_token/1,
    clear_expired_state_tokens/0]).


%% ====================================================================
%% API functions
%% ====================================================================


%% start/0
%% ====================================================================
%% @doc Initializes temporary storage for OpenID tokens.
%% ====================================================================
-spec start() -> ok.
%% ====================================================================
start() ->
    lists:foreach(fun(Table) -> ets:new(Table, [set, named_table, public]) end, ?TABLES_SET),
    lists:foreach(fun(Table) -> ets:new(Table, [bag, named_table, public]) end, ?TABLES_BAG),
    ok.


%% stop/0
%% ====================================================================
%% @doc Deinitializes temporary storage for OpenID tokens.
%% ====================================================================
-spec stop() -> ok.
%% ====================================================================
stop() ->
    lists:foreach(fun(Table) -> ets:delete(Table) end, ?TABLES_SET ++ ?TABLES_BAG),
    ok.


%% get_redirection_uri/2
%% ====================================================================
%% @doc Returns a Provider URI to which the user should be redirected from
%% the global registry. The redirection is part of the OpenID flow and the URI
%% contains an Authorization token.
%% @end
%% ====================================================================
-spec get_redirection_uri(UserId :: binary(), ProviderId :: binary()) ->
    RedirectionUri :: binary().
%% ====================================================================
get_redirection_uri(UserId, ProviderId) ->
    AuthCode = gen_auth_code(UserId, ProviderId),
    {ok, ProviderData} = provider_logic:get_data(ProviderId),
    {redirectionPoint, RedirectURL} = lists:keyfind(redirectionPoint, 1, ProviderData),
    <<RedirectURL/binary, ?provider_auth_endpoint, "?code=", AuthCode/binary>>.


%% gen_auth_code/1
%% ====================================================================
%% @doc Creates an authorization code for a native client.
%% ====================================================================
-spec gen_auth_code(UserId :: binary()) -> Token :: binary().
%% ====================================================================
gen_auth_code(UserId) ->
    gen_auth_code(UserId, undefined).


%% gen_auth_code/2
%% ====================================================================
%% @doc Creates an authorization code for a Provider.
%% ====================================================================
-spec gen_auth_code(UserId :: binary(), ProviderId :: binary() | undefined) ->
    Token :: binary().
%% ====================================================================
gen_auth_code(UserId, ProviderId) ->
    Token = random_token(),
    insert_expirable(?AUTH_CODE, Token, ?AUTH_CODE_EXPIRATION_SECS, {UserId, ProviderId}),
    Token.


%% has_access/1
%% ====================================================================
%% @doc Checks if a given user authorized a given access id and it exists
%% within the system.
%% @end
%% ====================================================================
-spec has_access(UserId :: binary(), AccessId :: binary()) -> boolean().
%% ====================================================================
has_access(UserId, AccessId) ->
    case retrieve_expirable(?ACCESS, AccessId) of
        {ok, {_, _, UserId, _, _}} -> true;
        _ -> false
    end.


%% delete_access/1
%% ====================================================================
%% @doc Deletes access-related data identified by an access id.
%% ====================================================================
-spec delete_access(AccessId :: binary()) -> ok.
%% ====================================================================
delete_access(AccessId) ->
    {ok, {AccessToken, RefreshToken, UserId, _, _}} = retrieve_expirable(?ACCESS, AccessId),
    ets:delete(?REFRESH_TOKEN, RefreshToken),
    ets:delete(?ACCESS_TOKEN, AccessToken),
    ets:delete(?USER_ID, UserId),
    ets:delete(?ACCESS, AccessId),
    ok.


%% get_user_tokens/1
%% ====================================================================
%% @doc Returns all pseudo-tokens identifying access and refresh tokens in the
%% system.
%% ====================================================================
-spec get_user_tokens(UserId :: binary()) -> [[proplists:property()]].
%% ====================================================================
get_user_tokens(UserId) ->
    AccessIDs = ets:match(?USER_ID, {UserId, '$1'}),
    lists:map(
        fun([AccessId]) ->
            [{_, {_, {_, _, _, _, ClientName}}}] = ets:lookup(?ACCESS, AccessId),
            [{accessId, AccessId}, {clientName, ClientName}]
        end, AccessIDs).


%% insert_expirable/4
%% ====================================================================
%% @doc Inserts a {Key, Data} pair into internal storage with additional
%% expiration time management.
%% @end
%% ====================================================================
-spec insert_expirable(Tab :: atom(), Key :: term(),
                       SecondsToExpire :: integer(), Data :: term()) -> ok.
%% ====================================================================
insert_expirable(Tab, Key, SecondsToExpire, Data) ->
    ExpirationTime = now_s() + SecondsToExpire,
    ets:insert(Tab, {Key, {ExpirationTime, Data}}),
    ok.


%% retrieve_expirable/2
%% ====================================================================
%% @doc Retrieves data from the internal storage with additional expiration
%% time management.
%% @end
%% ====================================================================
-spec retrieve_expirable(Tab :: atom(), Key :: term()) ->
    {ok, Data :: term()} | {error, missing} | no_return().
%% ====================================================================
retrieve_expirable(Tab, Key) ->
    %% clear_expired(Tab),
    case ets:lookup(Tab, Key) of
        [] -> {error, missing};
        [{_, {_, Data}}] -> {ok, Data}
    end.


%% clear_expired/1
%% ====================================================================
%% @doc Clears all expired entries from an internal storage with additional
%% expiration time management.
%% @end
%% ====================================================================
-spec clear_expired(Tab :: atom()) -> DeletedRecords :: integer().
%% ====================================================================
clear_expired(?ACCESS = Tab) ->
    Now = now_s(),
    Expired = ets:select(Tab, [{{'$1', {'$2', {'_'}}}, [{'<', '$2', Now}], ['$1']}]),
    lists:foreach(fun delete_access/1, Expired);
clear_expired(Tab) ->
    Now = now_s(),
    ets:select_delete(Tab, [{{'$1', {'$2', '$3'}}, [{'<', '$2', Now}], [true]}]).


%% grant_tokens/2
%% ====================================================================
%% @doc Grants ID, Access and Refresh tokens to the provider or native client
%% identifying itself with a valid Authorization token.
%% @end
%% ====================================================================
-spec grant_tokens(Client :: {provider, ProviderId :: binary()} | native,
    AuthCode :: binary()) -> [proplists:property()].
%% ====================================================================
grant_tokens(Client, AuthCode) ->
    {ok, {UserId, ProviderId}} = retrieve_expirable(?AUTH_CODE, AuthCode), %% @todo: missing
    ets:delete(?AUTH_CODE, AuthCode),

    Audience = case ProviderId of undefined -> UserId; _ -> ProviderId end,

    case Client of %% poor man's validation
        {provider, ProviderId} -> ok; %% @todo: wrong provider
        native when ProviderId =:= undefined -> ok %% @todo: client using provider's token
    end,

    AccessToken = random_token(),
    RefreshToken = random_token(),
    AccessId = random_token(),

    ets:insert(?ACCESS_TOKEN, {AccessToken, AccessId}),
    ets:insert(?REFRESH_TOKEN, {RefreshToken, AccessId}),
    ets:insert(?USER_ID, {UserId, AccessId}),
    insert_expirable(?ACCESS, AccessId, ?ACCESS_EXPIRATION_SECS,
        {AccessToken, RefreshToken, UserId, ProviderId, client_name_placeholder}),

    {ok, #user{name = Name, email_list = Emails}} = user_logic:get_user(UserId),
    EmailsList = lists:map(fun(Email) -> {struct, [{email, Email}]} end, Emails),
    [
        {access_token, AccessToken},
        {token_type, bearer},
        {expires_in, ?ACCESS_EXPIRATION_SECS},
        {refresh_token, RefreshToken},
        {scope, openid},
        {id_token, jwt_encode([
            {iss, ?ISSUER_URL},
            {sub, UserId},
            {aud, Audience},
            {name, Name},
            {email, EmailsList},
            {exp, wut}, %% @todo: expiration time
            {iat, now} %% @todo: now
        ])}
    ].


%% validate_token/2
%% ====================================================================
%% @doc Validates an access token for an OpenID client and returns a UserId of
%% the user that gave the authorization.
%% @end
%% ====================================================================
-spec validate_token(Client :: {provider, ProviderId :: binary()} | native,
    AccessToken :: binary()) -> UserId :: binary() | no_return().
%% ====================================================================
validate_token(Client, AccessToken) ->
    [{_, AccessId}] = ets:lookup(?ACCESS_TOKEN, AccessToken), %% @todo: missing
    {ok, Data} = retrieve_expirable(?ACCESS, AccessId),

    ProviderId = case Client of {provider, Id} -> Id; native -> undefined end,
    {_, _, UserId, ProviderId, _} = Data, %% @todo: someone else's token
    UserId.


%% generate_state_token/2
%% ====================================================================
%% @doc Generates a state token and retuns it. In the process, it stores the token
%% and associates some login info, that can be later retrieved given the token.
%% For example, where to redirect the user after login.
%% @end
-spec generate_state_token(HandlerModule :: atom(), ConnectAccount :: boolean()) -> [tuple()] | error.
%% ====================================================================
generate_state_token(HandlerModule, ConnectAccount) ->
    clear_expired_state_tokens(),
    Token = random_token(),
    {M, S, N} = now(),
    Time = M * 1000000000000 + S * 1000000 + N,

    RedirectAfterLogin = case gui_ctx:url_param(<<"x">>) of
                             undefined -> <<"/">>;
                             TargetPage -> TargetPage
                         end,

    StateInfo = [
        {module, HandlerModule},
        {connect_account, ConnectAccount},
        {redirect_after_login, RedirectAfterLogin}
    ],

    ets:insert(?STATE_TOKEN, {Token, Time, StateInfo}),
    Token.


%% lookup_state_token/1
%% ====================================================================
%% @doc Checks if the given state token exists and returns login info
%% associated with it or error otherwise.
%% @end
-spec lookup_state_token(Token :: binary()) -> [tuple()] | error.
%% ====================================================================
lookup_state_token(Token) ->
    clear_expired_state_tokens(),
    case ets:lookup(?STATE_TOKEN, Token) of
        [{Token, Time, LoginInfo}] ->
            ets:delete_object(?STATE_TOKEN, {Token, Time, LoginInfo}),
            LoginInfo;
        _ ->
            error
    end.


%% clear_expired_state_tokens/0
%% ====================================================================
%% @doc Removes all state tokens that are no longer valid from ETS.
%% @end
-spec clear_expired_state_tokens() -> ok.
%% ====================================================================
clear_expired_state_tokens() ->
    {M, S, N} = now(),
    Now = M * 1000000000000 + S * 1000000 + N,

    ExpiredSessions = ets:select(?STATE_TOKEN, [{{'$1', '$2', '$3'}, [{'<', '$2', Now - (?STATE_TOKEN_EXPIRATION_SECS * 1000000)}], ['$_']}]),
    lists:foreach(
        fun({Token, Time, LoginInfo}) ->
            ets:delete_object(?STATE_TOKEN, {Token, Time, LoginInfo})
        end, ExpiredSessions).


%% ====================================================================
%% Internal functions
%% ====================================================================


%% jwt_encode/1
%% ====================================================================
%% @doc Encodes OpenID claims as an unsigned, unencrypted
%% <a href="tools.ietf.org/html/draft-ietf-oauth-json-web-token">JWT</a>
%% structure.
%% @end
%% ====================================================================
-spec jwt_encode(Claims ::[proplists:property()]) -> JWT :: binary().
%% ====================================================================
jwt_encode(Claims) ->
    Header = mochijson2:encode([{typ, 'JWT'}, {alg, none}]),
    Payload = mochijson2:encode(Claims),
    Header64 = mochiweb_base64url:encode(Header),
    Payload64 = mochiweb_base64url:encode(Payload),
    <<Header64/binary, ".", Payload64/binary, ".">>.


%% random_token/0
%% ====================================================================
%% @doc Generates a globally unique random token.
%% ====================================================================
-spec random_token() -> binary().
%% ====================================================================
random_token() ->
    binary:list_to_bin(
        mochihex:to_hex(
            crypto:hash(sha,
                term_to_binary({make_ref(), node(), now()})))).


%% now_s/0
%% ====================================================================
%% @doc Returns the time in seconds since epoch.
%% ====================================================================
-spec now_s() -> integer().
%% ====================================================================
now_s() ->
    {MegaSecs, Secs, _} = erlang:now(),
    MegaSecs * 1000000 + Secs.
