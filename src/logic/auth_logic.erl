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

-define(STATE_TOKEN, state_token).

%% @todo: config
-define(TOKEN_LENGTH, 30).
-define(AUTH_CODE_EXPIRATION_SECS, 600).
-define(ACCESS_EXPIRATION_SECS, 36000).
-define(STATE_TOKEN_EXPIRATION_SECS, 60).
-define(ISSUER_URL, <<"https://onedata.org">>).

-define(DB(Function, Arg), dao_lib:apply(dao_auth, Function, [Arg], 1)).
-define(DB(Function, Arg1, Arg2), dao_lib:apply(dao_auth, Function, [Arg1, Arg2], 1)).


%% ====================================================================
%% API
%% ====================================================================
-export([start/0, stop/0, get_redirection_uri/2, gen_auth_code/1,
    has_access/2, delete_access/1,  get_user_tokens/1,  grant_tokens/2,
    validate_token/2, verify/2, clear_expired_authorizations/0]).

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
    Pid = spawn(fun clear_expired_authorizations/0),
    register(clear_auth_process, Pid),
    ets:new(?STATE_TOKEN, [set, named_table, public]),
    ok.


%% stop/0
%% ====================================================================
%% @doc Deinitializes temporary storage for OpenID tokens.
%% ====================================================================
-spec stop() -> ok.
%% ====================================================================
stop() ->
    clear_auth_process ! stop,
    ets:delete(?STATE_TOKEN),
    ok.


%% get_redirection_uri/2
%% ====================================================================
%% @doc Returns provider hostname and a full URI to which the user should be redirected from
%% the global registry. The redirection is part of the OpenID flow and the URI
%% contains an Authorization token. The provider hostname is useful to check connectivity
%% before redirecting.
%% @end
%% ====================================================================
-spec get_redirection_uri(UserId :: binary(), ProviderId :: binary()) ->
    {ProviderHostname :: binary(), RedirectionUri :: binary()}.
%% ====================================================================
get_redirection_uri(UserId, ProviderId) ->
    AuthCode = gen_auth_code(UserId, ProviderId),
    {ok, ProviderData} = provider_logic:get_data(ProviderId),
    {redirectionPoint, RedirectURL} = lists:keyfind(redirectionPoint, 1, ProviderData),
    {RedirectURL, <<RedirectURL/binary, ?provider_auth_endpoint, "?code=", AuthCode/binary>>}.


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
    ExpirationPoint = vcn_utils:time() + ?AUTH_CODE_EXPIRATION_SECS,
    Auth = #authorization{code = Token, expiration_time = ExpirationPoint,
                          user_id = UserId, provider_id = ProviderId},

    {ok, _} = ?DB(save_authorization, Auth),
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
    case ?DB(get_access, AccessId) of
        {ok, #veil_document{record = #access{user_id = UserId}}} -> true;
        _ -> false
    end.


%% delete_access/1
%% ====================================================================
%% @doc Deletes access-related data identified by an access id.
%% ====================================================================
-spec delete_access(AccessId :: binary()) -> ok | no_return().
%% ====================================================================
delete_access(AccessId) ->
    ok = ?DB(remove_access, AccessId).


%% get_user_tokens/1
%% ====================================================================
%% @doc Returns all pseudo-tokens identifying access and refresh tokens in the
%% system.
%% ====================================================================
-spec get_user_tokens(UserId :: binary()) -> [[proplists:property()]].
%% ====================================================================
get_user_tokens(UserId) ->
    {ok, AccessDocs} = ?DB(get_accesses_by_user, UserId),
    lists:map(
        fun(AccessDoc) ->
            #veil_document{uuid = AccessId, record = #access{client_name = ClientName}} = AccessDoc,
            [{accessId, AccessId}, {clientName, ClientName}]
        end, AccessDocs).


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
    {ok, AuthDoc} = ?DB(get_authorization_by_code, AuthCode), %% @todo: missing
    #veil_document{uuid = AuthId, record = Auth} = AuthDoc,
    #authorization{provider_id = ProviderId, user_id = UserId, expiration_time = Expiration} = Auth,
    true = vcn_utils:time() < Expiration, %% @todo: expired
    ok = ?DB(remove_authorization, AuthId),

    Audience = case ProviderId of undefined -> UserId; _ -> ProviderId end,

    case Client of %% poor man's validation
        {provider, ProviderId} -> ok; %% @todo: wrong provider
        native when ProviderId =:= undefined -> ok %% @todo: client using provider's token
    end,

    AccessToken = random_token(),
    AccessTokenHash = access_token_hash(AccessToken),
    RefreshToken = random_token(),
    Now = vcn_utils:time(),
    ExpirationTime = Now + ?ACCESS_EXPIRATION_SECS,

    Access = #access{token = AccessToken, token_hash = AccessTokenHash,
        refresh_token = RefreshToken, user_id = UserId, provider_id = ProviderId,
        expiration_time = ExpirationTime, client_name = client_name_placeholder},

    {ok, _} = ?DB(save_access, Access),

    {ok, #user{name = Name, email_list = Emails}} = user_logic:get_user(UserId),
    EmailsList = lists:map(fun(Email) -> [{email, Email}] end, Emails),
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
            {exp, ExpirationTime},
            {iat, Now}
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
    ProviderId = case Client of {provider, Id} -> Id; native -> undefined end,
    {ok, #veil_document{record = Access}} = ?DB(get_access_by_key, token, AccessToken), %% @todo: missing
    #access{provider_id = ProviderId, user_id = UserId, expiration_time = Expiration} = Access, %% @todo: someone else's token
    true = vcn_utils:time() < Expiration, %% @todo: expired
    UserId.


%% remove_expired_authorizations_in_chunks/1
%% ====================================================================
%% @doc Removes expired authorizations in chunks of given size.
%% ====================================================================
-spec remove_expired_authorizations_in_chunks(ChunkSize :: non_neg_integer()) ->
    ok.
%% ====================================================================
remove_expired_authorizations_in_chunks(ChunkSize) ->
    {ok, ExpiredIds} = ?DB(get_expired_authorizations_ids, ChunkSize),
    lists:foreach(fun(AuthId) -> ?DB(remove_authorization, AuthId) end, ExpiredIds),
    case length(ExpiredIds) of
        ChunkSize -> remove_expired_authorizations_in_chunks(ChunkSize);
        _ -> ok
    end.


%% clear_expired_authorizations/0
%% ====================================================================
%% @doc Clears any and all expired authorization tokens and associated data. Intended
%% for use as a periodic job. Does not throw.
%% ====================================================================
-spec clear_expired_authorizations() -> ok.
%% ====================================================================
clear_expired_authorizations() ->
    receive
        stop -> ok
    after
        timer:seconds(?AUTH_CODE_EXPIRATION_SECS) ->
            try
                remove_expired_authorizations_in_chunks(50)
            catch
                Error:Reason ->
                    ?warning("error while clearing expired authorizations: ~p ~p", [Error, Reason])
            end,
            ?MODULE:clear_expired_authorizations()
    end.


%% generate_state_token/2
%% ====================================================================
%% @doc Generates a state token and retuns it. In the process, it stores the token
%% and associates some login info, that can be later retrieved given the token.
%% For example, where to redirect the user after login.
%% @end
-spec generate_state_token(HandlerModule :: atom(), ConnectAccount :: boolean()) -> binary().
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
        {redirect_after_login, RedirectAfterLogin},
        % PROBABLY DEVELOPER-ONLY FUNCTIONALITY
        % If this value was set on login page, the user will be redirected to
        % this certain provider if he click "go to your files"
        {referer, erlang:get(referer)}
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
%% ====================================================================
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


%% verify/2
%% ====================================================================
%% @doc Verifies if a given secret belongs to a given user.
%% ====================================================================
-spec verify(UserId :: binary(), Secret :: binary()) ->
    boolean().
%% ====================================================================
verify(UserId, Secret) ->
    case ?DB(get_access_by_key, token_hash, Secret) of
        {ok, #veil_document{record = #access{user_id = UserId}}} -> true;
        _ -> false
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================


%% access_token_hash/1
%% ====================================================================
%% @doc Hashes a token with SHA512 and encodes it with base64.
%% ====================================================================
-spec access_token_hash(AccessToken :: binary()) -> Hash :: binary().
%% ====================================================================
access_token_hash(AccessToken) ->
    Hash = crypto:hash(sha512, AccessToken),
    base64:encode(Hash).


%% jwt_encode/1
%% ====================================================================
%% @doc Encodes OpenID claims as an unsigned, unencrypted
%% <a href="tools.ietf.org/html/draft-ietf-oauth-json-web-token">JWT</a>
%% structure.
%% @end
%% ====================================================================
-spec jwt_encode(Claims :: [proplists:property()]) -> JWT :: binary().
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
            crypto:rand_bytes(?TOKEN_LENGTH))).
