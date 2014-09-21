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
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-define(STATE_TOKEN, state_token).
-define(STATE_TOKEN_EXPIRATION_SECS, 60). %% @todo: config

-define(EXPIRED_AUTHORIZATION_REMOVE_CHUNK, 50).

-define(DB(Function, Arg), dao_lib:apply(dao_auth, Function, [Arg], 1)).
-define(DB(Function, Arg1, Arg2), dao_lib:apply(dao_auth, Function, [Arg1, Arg2], 1)).


%% ====================================================================
%% API
%% ====================================================================
-export([start/0, stop/0, get_redirection_uri/2, gen_auth_code/1,
    has_access/2, delete_access/1, get_user_tokens/1, grant_tokens/2,
    refresh_tokens/2, validate_token/2, verify/2,
    clear_expired_authorizations/0]).

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
%% @doc Returns provider hostname and a full URI to which the user should be
%% redirected from the global registry. The redirection is part of the OpenID
%% flow and the URI contains an Authorization token. The provider hostname
%% is useful to check connectivity before redirecting.
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
    {ok, ExpirationSecs} = application:get_env(?APP_Name, authorization_code_expiration_seconds),

    Token = token_logic:random_token(),
    ExpirationPoint = vcn_utils:time() + ExpirationSecs,
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
                   AuthCode :: binary()) ->
    {ok, [proplists:property()]} |
    {error, invalid_or_expired | expired | wrong_client}.
%% ====================================================================
grant_tokens(Client, AuthCode) ->
    try
        AuthDoc = case ?DB(get_authorization_by_code, AuthCode) of
            {ok, AuthDoc1} -> AuthDoc1;
            {error, not_found} -> throw(invalid_or_expired)
        end,

        #veil_document{uuid = AuthId, record = Auth} = AuthDoc,
        #authorization{provider_id = ProviderId, user_id = UserId, expiration_time = Expiration} = Auth,

        case vcn_utils:time() < Expiration of
            true -> ok;
            false -> throw(expired)
        end,

        ok = ?DB(remove_authorization, AuthId),

        case Client of %% poor man's validation
            {provider, ProviderId} -> ok;
            native when ProviderId =:= undefined -> ok;
            _ -> throw(wrong_client)
        end,

        Now = vcn_utils:time(),
        {AccessToken, AccessTokenHash, RefreshToken, ExpirationTime} =
            generate_access_tokens(Now),

        %% For a provider, update an existing access document if possible
        CreateNewAccessDocument = case Client of
            {provider, ProviderId} ->
                case ?DB(get_access_by_user_and_provider, UserId, ProviderId) of
                    {ok, AccessDoc} ->
                        #veil_document{record = Access} = AccessDoc,
                        AccessDocUpdated = AccessDoc#veil_document{record = Access#access{
                            token = AccessToken,
                            token_hash = AccessTokenHash,
                            refresh_token = RefreshToken,
                            expiration_time = ExpirationTime
                        }},
                        {ok, _} = ?DB(save_access, AccessDocUpdated),
                        false;

                    {error, not_found} ->
                        true
                end;

            native ->
                true
        end,

        case CreateNewAccessDocument of
            true ->
                Access1 = #access{token = AccessToken, token_hash = AccessTokenHash,
                refresh_token = RefreshToken, user_id = UserId, provider_id = ProviderId,
                expiration_time = ExpirationTime, client_name = client_name_placeholder},
                {ok, _} = ?DB(save_access, Access1);

            false ->
                ok
        end,

        prepare_token_response(UserId, ProviderId, AccessToken, RefreshToken, ExpirationTime, Now)
    catch
        Error -> {error, Error}
    end.


%% refresh_tokens/2
%% ====================================================================
%% @doc Refreshes tokens granted through the token endpoint.
%% @end
%% ====================================================================
-spec refresh_tokens(Client :: {provider, ProviderId :: binary()} | native,
                     RefreshToken :: binary()) ->
    {ok, [proplists:property()]} |
    {error, refresh_invalid_or_revoked | refresh_wrong_client}.
%% ====================================================================
refresh_tokens(Client, RefreshToken) ->
    try
        AccessDoc = case ?DB(get_access_by_key, refresh_token, RefreshToken) of
            {ok, AccessDoc1} -> AccessDoc1;
            {error, not_found} -> throw(refresh_invalid_or_revoked)
        end,

        #veil_document{record = Access} = AccessDoc,
        #access{provider_id = ProviderId, user_id = UserId} = Access,

        case Client of
            {provider, ProviderId} -> ok;
            native when ProviderId =:= undefined -> ok;
            _ ->
                alert_revoke_access(AccessDoc),
                throw(refresh_wrong_client)
        end,

        Now = vcn_utils:time(),
        {AccessToken, AccessTokenHash, RefreshTokenNew, ExpirationTime} =
            generate_access_tokens(Now),

        AccessDocUpdated = AccessDoc#veil_document{record = Access#access{
            token = AccessToken,
            token_hash = AccessTokenHash,
            refresh_token = RefreshTokenNew,
            expiration_time = ExpirationTime
        }},

        {ok, _} = ?DB(save_access, AccessDocUpdated),

        prepare_token_response(UserId, ProviderId, AccessToken, RefreshTokenNew, ExpirationTime, Now)
    catch
        Error -> {error, Error}
    end.


%% validate_token/2
%% ====================================================================
%% @doc Validates an access token for an OpenID client and returns a UserId of
%% the user that gave the authorization.
%% @end
%% ====================================================================
-spec validate_token(Client :: {provider, ProviderId :: binary()} | native,
                     AccessToken :: binary()) ->
    {ok, UserId :: binary()} | {error, not_found | expired | bad_audience}.
%% ====================================================================
validate_token(Client, AccessToken) ->
    ProviderId = case Client of
        {provider, Id} -> Id;
        native -> undefined
    end,

    case ?DB(get_access_by_key, token, AccessToken) of
        {error, not_found} = Error -> Error;
        {ok, #veil_document{record = Access} = AccessDoc} ->
            #access{provider_id = IntendedAudience, user_id = UserId,
                    expiration_time = Expiration} = Access,

            case IntendedAudience of
                ProviderId ->
                    case vcn_utils:time() < Expiration of
                        false -> {error, expired};
                        true -> {ok, UserId}
                    end;

                _ ->
                    alert_revoke_access(AccessDoc),
                    {error, bad_audience}
            end
    end.


%% clear_expired_authorizations/0
%% ====================================================================
%% @doc Clears any and all expired authorization tokens and associated data. Intended
%% for use as a periodic job. Does not throw.
%% ====================================================================
-spec clear_expired_authorizations() -> ok.
%% ====================================================================
clear_expired_authorizations() ->
    {ok, ExpirationSecs} = application:get_env(?APP_Name, authorization_code_expiration_seconds),
    receive
        stop -> ok
    after
        timer:seconds(ExpirationSecs) ->
            try
                remove_expired_authorizations_in_chunks(?EXPIRED_AUTHORIZATION_REMOVE_CHUNK)
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
    Token = token_logic:random_token(),
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
    Now = vcn_utils:time(),
    case ?DB(get_access_by_key, token_hash, Secret) of
        {ok, #veil_document{record = #access{user_id = UserId, expiration_time = Exp}}} ->
            Now < Exp;
        {ok, #veil_document{record = #access{}} = AccessDoc} ->
            alert_revoke_access(AccessDoc),
            false;
        _ ->
            false
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


%% generate_access_tokens/1
%% ====================================================================
%% @doc Generates a new set of access tokens and token-related information.
%% ====================================================================
-spec generate_access_tokens(Now :: non_neg_integer()) ->
    {AccessToken :: binary(), AccessTokenHash :: binary(),
     RefreshToken :: binary(), ExpirationTime :: non_neg_integer()}.
%% ====================================================================
generate_access_tokens(Now) ->
    {ok, AccessExpirationSecs} = application:get_env(?APP_Name, access_token_expiration_seconds),
    AccessToken = token_logic:random_token(),
    AccessTokenHash = access_token_hash(AccessToken),
    RefreshToken = token_logic:random_token(),
    ExpirationTime = Now + AccessExpirationSecs,
    {AccessToken, AccessTokenHash, RefreshToken, ExpirationTime}.


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


%% alert_revoke_access/1
%% ====================================================================
%% @doc Removes an access and logs an alert.
%% ====================================================================
-spec alert_revoke_access(AccessDoc :: access_doc()) ->
    ok.
%% ====================================================================
alert_revoke_access(AccessDoc) ->
    #veil_document{uuid = AccessId, record = Access} = AccessDoc,
    ?alert("Revoking access id: ~p, contents: ~p", [AccessId, Access]),
    ok = ?DB(remove_access, AccessId).


%% prepare_token_response/1
%% ====================================================================
%% @doc Prepares an OpenID token response to encode into JSON.
%% ====================================================================
-spec prepare_token_response(UserId :: binary(), ProviderId :: binary(),
    AccessToken :: binary(), RefreshToken :: binary(),
    ExpirationTime :: non_neg_integer(), Now :: non_neg_integer()) ->
    proplists:proplist().
%% ====================================================================
prepare_token_response(UserId, ProviderId, AccessToken, RefreshToken, ExpirationTime, Now) ->
    {ok, AccessExpirationSecs} = application:get_env(?APP_Name, access_token_expiration_seconds),
    {ok, IssuerUrl} = application:get_env(?APP_Name, openid_issuer_url),

    Audience = case ProviderId of
        undefined -> UserId;
        _ -> ProviderId
    end,

    {ok, #user{name = Name, email_list = Emails}} = user_logic:get_user(UserId),
    {ok, [
        {access_token, AccessToken},
        {token_type, bearer},
        {expires_in, AccessExpirationSecs},
        {refresh_token, RefreshToken},
        {scope, openid},
        {id_token, jwt_encode([
            {iss, vcn_utils:ensure_binary(IssuerUrl)},
            {sub, UserId},
            {aud, Audience},
            {name, Name},
            {emails, Emails},
            {exp, ExpirationTime},
            {iat, Now}
        ])}
    ]}.
