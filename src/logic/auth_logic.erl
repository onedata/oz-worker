%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module implementing the business logic for OpenID Connect end-user
%%% authentication and authorization.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_logic).
-author("Konrad Zemek").

-include("auth_common.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-define(STATE_TOKEN, state_token).
-define(STATE_TOKEN_EXPIRATION_SECS, 60). %% @todo: config

% String that will be placed in macaroons' location field
-define(MACAROONS_LOCATION, <<"onezone">>).

%% API
-export([start/0, stop/0, get_redirection_uri/2,
    gen_token/1, gen_token/2, validate_token/5, invalidate_token/1,
    authenticate_user/1]).

%% Handling state tokens
-export([generate_state_token/2, lookup_state_token/1,
    clear_expired_state_tokens/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initializes temporary storage for OpenID tokens.
%%--------------------------------------------------------------------
-spec start() -> ok.
start() ->
    ets:new(?STATE_TOKEN, [set, named_table, public]),
    ok.

%%--------------------------------------------------------------------
%% @doc Deinitializes temporary storage for OpenID tokens.
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    ets:delete(?STATE_TOKEN),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Attempt to authenticate user versus user id found in database.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_user(Identifier :: binary()) -> {ok, Token :: binary()} | {error, any()}.
authenticate_user(Identifier) ->
    % @todo really authenticate; we're just pretending
    case onedata_auth:get(Identifier) of
        {ok, #document{value = #onedata_auth{secret = Secret, user_id = UserId}}} ->
            % @todo yeah, that seems like a very authenticated UserId
            UserId = UserId,

            {ok, ExpirationSecs} = application:get_env(?APP_Name,
                authentication_macaroon_expiration_seconds),

            Location = ?MACAROONS_LOCATION,
            M = macaroon:create(Location, Secret, Identifier),
            M2 = macaroon:add_first_party_caveat(M,
                ["time < ", integer_to_binary(erlang:system_time(seconds) + ExpirationSecs)]),

            case token_utils:serialize62(M2) of
                {ok, _} = Serialized -> Serialized;
                {error, _} = Error -> Error
            end;

        _ ->
            {error, no_auth_record}
    end.


%%--------------------------------------------------------------------
%% @doc Returns provider hostname and a full URI to which the user should be
%% redirected from the onezone. The redirection is part of the OpenID
%% flow and the URI contains an Authorization token. The provider hostname
%% is useful to check connectivity before redirecting.
%% @end
%%--------------------------------------------------------------------
-spec get_redirection_uri(UserId :: binary(), ProviderId :: binary()) ->
    {ok, RedirectionUri :: binary()}.
get_redirection_uri(UserId, ProviderId) ->
    Token = gen_token(UserId, ProviderId),
    _Hostname = list_to_binary(dns_query_handler:get_canonical_hostname()),
    {ok, #onedata_user{alias = Alias}} = user_logic:get_user(UserId),
    {ok, _} = onedata_user:update(UserId, fun(User) ->
        {ok, User#onedata_user{
            chosen_provider = ProviderId
        }}
    end),
    _Prefix = case Alias of
        ?EMPTY_ALIAS ->
            <<?NO_ALIAS_UUID_PREFIX, UserId/binary>>;
        _ ->
            Alias
    end,
    % TODO return IP address rather than alias.onedata.org
    % It shall be used normally when we have a possibility to
    % resolve domains on developer's host systems
    % (so their web browsers can connect).
    % To do this, we need a recursive DNS server in docker environment,
    % whose address must be fed to system's resolv.conf.
    {ok, ProviderURL} = provider_logic:get_url(ProviderId),
    URL = str_utils:format_bin("~s~s?code=~s", [
        ProviderURL, ?provider_auth_endpoint, Token
    ]),
    {ok, URL}.


%%--------------------------------------------------------------------
%% @doc Creates an authorization code for a native client.
%%--------------------------------------------------------------------
-spec gen_token(UserId :: binary()) -> Token :: binary().
gen_token(UserId) ->
    Secret = generate_secret(),
    Caveats = [],%["method = GET", "rootResource in spaces,user"],
    {ok, IdentifierBinary} = onedata_auth:save(#document{value = #onedata_auth{
        secret = Secret, user_id = UserId}}),
    Identifier = binary_to_list(IdentifierBinary),
    M = create_macaroon(Secret, str_utils:to_binary(Identifier), Caveats),
    {ok, Token} = token_utils:serialize62(M),
    Token.

%%--------------------------------------------------------------------
%% @doc Creates an authorization code for a Provider.
%%--------------------------------------------------------------------
-spec gen_token(UserId :: binary(), ProviderId :: binary() | undefined) ->
    Token :: binary().
gen_token(UserId, ProviderId) ->
    Secret = generate_secret(),
    Location = ?MACAROONS_LOCATION,
    {ok, IdentifierBinary} = onedata_auth:save(#document{value = #onedata_auth{
        secret = Secret, user_id = UserId}}),
    Identifier = binary_to_list(IdentifierBinary),
    %% @todo: VFS-1869
    M = create_macaroon(Secret, str_utils:to_binary(Identifier), []),

    CaveatKey = generate_secret(),
    {ok, CaveatIdBinary} = onedata_auth:save(#document{value = #onedata_auth{
        secret = CaveatKey, user_id = UserId}}),
    CaveatId = binary_to_list(CaveatIdBinary),
    M2 = macaroon:add_third_party_caveat(M, Location, CaveatKey, CaveatId),
    {ok, Token} = token_utils:serialize62(M2),
    Token.

%%--------------------------------------------------------------------
%% @doc Validates an access token for an OpenID client and returns a UserId of
%% the user that gave the authorization.
%% @end
%%--------------------------------------------------------------------
-spec validate_token(ProviderId :: binary(), Macaroon :: macaroon:macaroon(),
    DischargeMacaroons :: [macaroon:macaroon()], Method :: binary(),
    RootResource :: atom()) ->
    {ok, UserId :: binary()} | {error, Reason :: any()}.
validate_token(ProviderId, Macaroon, DischargeMacaroons, Method, RootResource) ->
    Identifier = macaroon:identifier(Macaroon),
    case onedata_auth:get(Identifier) of
        {ok, #document{value = #onedata_auth{secret = Secret, user_id = UserId}}} ->
            V = macaroon_verifier:create(),

            VerifyFun = fun
                (<<"time < ", Integer/binary>>) ->
                    erlang:system_time(seconds) < binary_to_integer(Integer);
                (<<"method = ", Met/binary>>) ->
                    Method =:= Met;
                (<<"rootResource in ", Resources/binary>>) ->
                    lists:member(atom_to_binary(RootResource, utf8),
                        binary:split(Resources, <<",">>, [global]));
                (<<"providerId = ", PID/binary>>) ->
                    PID =:= ProviderId;
                (_) ->
                    false
            end,

            V1 = macaroon_verifier:satisfy_general(V, VerifyFun),
            case macaroon_verifier:verify(V1, Macaroon, Secret, DischargeMacaroons) of
                ok -> {ok, UserId};
                {error, Reason} -> {error, Reason}
            end;

        _ ->
            {error, unknown_macaroon}
    end.

%%--------------------------------------------------------------------
%% @doc Invalidates a given token or all of user's tokens.
%% @end
%%--------------------------------------------------------------------
-spec invalidate_token({user_id, binary()} | binary()) -> ok.
invalidate_token({user_id, UserId}) ->
    {ok, AuthDocs} = onedata_auth:get_auth_by_user_id(UserId),
    lists:foreach(fun(#document{key = AuthId}) ->
        invalidate_token(AuthId) end, AuthDocs),
    ok;
invalidate_token(Identifier) when is_binary(Identifier) ->
    onedata_auth:delete(Identifier),
    ok.

%%--------------------------------------------------------------------
%% @doc Generates a state token and retuns it. In the process, it stores the token
%% and associates some login info, that can be later retrieved given the token.
%% For example, where to redirect the user after login.
%% @end
%%--------------------------------------------------------------------
-spec generate_state_token(HandlerModule :: atom(), ConnectAccount :: boolean()) -> binary().
generate_state_token(HandlerModule, ConnectAccount) ->
    clear_expired_state_tokens(),
    Token = list_to_binary(hex_utils:to_hex(crypto:rand_bytes(32))),

    StateInfo = [
        {module, HandlerModule},
        {connect_account, ConnectAccount},
        % Right now this always redirects to main page, although
        % might be used in the future.
        {redirect_after_login, <<"/">>}
    ],

    ets:insert(?STATE_TOKEN, {Token, erlang:monotonic_time(seconds), StateInfo}),
    Token.

%%--------------------------------------------------------------------
%% @doc Checks if the given state token exists and returns login info
%% associated with it or error otherwise.
%% @end
%%--------------------------------------------------------------------
-spec lookup_state_token(Token :: binary()) -> [tuple()] | error.
lookup_state_token(Token) ->
    clear_expired_state_tokens(),
    case ets:lookup(?STATE_TOKEN, Token) of
        [{Token, Time, LoginInfo}] ->
            ets:delete_object(?STATE_TOKEN, {Token, Time, LoginInfo}),
            LoginInfo;
        _ ->
            error
    end.

%%--------------------------------------------------------------------
%% @doc Removes all state tokens that are no longer valid from ETS.
%%--------------------------------------------------------------------
-spec clear_expired_state_tokens() -> ok.
clear_expired_state_tokens() ->
    Now = erlang:monotonic_time(seconds),

    ExpiredSessions = ets:select(?STATE_TOKEN, [{{'$1', '$2', '$3'}, [{'<', '$2', Now - (?STATE_TOKEN_EXPIRATION_SECS)}], ['$_']}]),
    lists:foreach(
        fun({Token, Time, LoginInfo}) ->
            ets:delete_object(?STATE_TOKEN, {Token, Time, LoginInfo})
        end, ExpiredSessions).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a macaroon with expiration time read from
%% `authorization_macaroon_expiration_seconds` environment variable.
%% @end
%%--------------------------------------------------------------------
-spec create_macaroon(Secret :: iodata(), Identifier :: iodata(),
    Caveats :: [iodata()]) -> macaroon:macaroon().
create_macaroon(Secret, Identifier, Caveats) ->
    {ok, ExpirationSeconds} = application:get_env(?APP_Name,
        authorization_macaroon_expiration_seconds),
    ExpirationTime = erlang:system_time(seconds) + ExpirationSeconds,

    Location = ?MACAROONS_LOCATION,

    lists:foldl(
        fun(Caveat, Macaroon) ->
            macaroon:add_first_party_caveat(Macaroon, Caveat)
        end,
        macaroon:create(Location, Secret, Identifier),
        [["time < ", integer_to_binary(ExpirationTime)] | Caveats]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates a hex-encoded random secret for use with Macaroon.
%% @end
%%--------------------------------------------------------------------
-spec generate_secret() -> binary().
generate_secret() ->
    BinSecret = crypto:rand_bytes(macaroon:suggested_secret_length()),
    <<<<Y>> || <<X:4>> <= BinSecret, Y <- integer_to_list(X, 16)>>.
