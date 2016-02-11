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

-include("dao/dao_types.hrl").
-include("auth_common.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-define(STATE_TOKEN, state_token).
-define(STATE_TOKEN_EXPIRATION_SECS, 60). %% @todo: config

% String that will be placed in macaroons' location field
-define(MACAROONS_LOCATION, <<"globalregistry">>).

-define(DB(Function, Arg), dao_lib:apply(dao_auth, Function, [Arg], 1)).

%% API
-export([start/0, stop/0, get_redirection_uri/3, gen_token/1, gen_token/2, validate_token/5,
    authenticate_user/1, invalidate_token/1]).

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
    case ?DB(get_auth, Identifier) of
        {ok, #db_document{record = #auth{secret = Secret, user_id = UserId}}} ->
            % @todo yeah, that seems like a very authenticated UserId
            UserId = UserId,

            {ok, ExpirationSecs} = application:get_env(?APP_Name,
                authentication_macaroon_expiration_seconds),

            Location = ?MACAROONS_LOCATION,
            {ok, M} = macaroon:create(Location, Secret, Identifier),
            {ok, M2} = macaroon:add_first_party_caveat(M,
                ["time < ", integer_to_binary(utils:time() + ExpirationSecs)]),

            case macaroon:serialize(M2) of
                {ok, _} = Serialized -> Serialized;
                {error, _} = Error -> Error
            end;

        _ ->
            {error, no_auth_record}
    end.


%%--------------------------------------------------------------------
%% @doc Returns provider hostname and a full URI to which the user should be
%% redirected from the global registry. The redirection is part of the OpenID
%% flow and the URI contains an Authorization token. The provider hostname
%% is useful to check connectivity before redirecting.
%% @end
%%--------------------------------------------------------------------
-spec get_redirection_uri(UserId :: binary(), ProviderId :: binary(), ProviderGUIPort :: integer()) ->
    {ok, RedirectionUri :: binary()}.
get_redirection_uri(UserId, ProviderId, _ProviderGUIPort) ->
    Token = gen_token(UserId, ProviderId),
    _Hostname = list_to_binary(dns_query_handler:get_canonical_hostname()),
    {ok, #user{alias = Alias}} = user_logic:get_user(UserId),
    ok = user_logic:modify(UserId, [{default_provider, ProviderId}]),
    _Prefix = case Alias of
                  ?EMPTY_ALIAS ->
                      <<?NO_ALIAS_UUID_PREFIX, UserId/binary>>;
                  _ ->
                      Alias
              end,
    % TODO return IP address rather than alias.onedata.org
    % It shall be used normally when we have a possibility to
    % resolve domains on developer's host systems (so their web browsers can connect).
    % To do this, we need a recursive DNS server in docker environment,
    % whose address must be fed to system's resolv.conf.
    {ok, PData} = provider_logic:get_data(ProviderId),
    [RedirectionIP | _] = proplists:get_value(urls, PData),
    {ok, <<"https://", RedirectionIP/binary, ?provider_auth_endpoint, "?code=", Token/binary>>}.

%% {ok, <<"https://", Prefix/binary, ".", Hostname/binary, ":", (integer_to_binary(ProviderGUIPort))/binary,
%% ?provider_auth_endpoint, "?code=", AuthCode/binary>>}.

%%--------------------------------------------------------------------
%% @doc Creates an authorization code for a native client.
%%--------------------------------------------------------------------
-spec gen_token(UserId :: binary()) -> Token :: binary().
gen_token(UserId) ->
    Secret = generate_secret(),
    Caveats = [],%["method = GET", "rootResource in spaces,user"],
    {ok, Identifier} = ?DB(save_auth, #auth{secret = Secret, user_id = UserId}),
    {ok, M} = create_macaroon(Secret, str_utils:to_binary(Identifier), Caveats),
    {ok, Token} = macaroon:serialize(M),
    Token.

%%--------------------------------------------------------------------
%% @doc Creates an authorization code for a Provider.
%%--------------------------------------------------------------------
-spec gen_token(UserId :: binary(), ProviderId :: binary() | undefined) ->
    Token :: binary().
gen_token(UserId, ProviderId) ->
    Secret = generate_secret(),
    Location = ?MACAROONS_LOCATION,
    {ok, Identifier} = ?DB(save_auth, #auth{secret = Secret, user_id = UserId}),
    {ok, M} = create_macaroon(Secret, str_utils:to_binary(Identifier),
        [["providerId = ", ProviderId]]),

    CaveatKey = generate_secret(),
    {ok, CaveatId} = ?DB(save_auth, #auth{secret = CaveatKey, user_id = UserId}),
    {ok, M2} = macaroon:add_third_party_caveat(M, Location, CaveatKey, CaveatId),
    {ok, Token} = macaroon:serialize(M2),
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
    {ok, Identifier} = macaroon:identifier(Macaroon),
    case ?DB(get_auth, Identifier) of
        {ok, #db_document{record = #auth{secret = Secret, user_id = UserId}}} ->
            {ok, V} = macaroon_verifier:create(),

            VerifyFun = fun
                (<<"time < ", Integer/binary>>) ->
                    utils:time() < binary_to_integer(Integer);
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

            macaroon_verifier:satisfy_general(V, VerifyFun),
            case macaroon_verifier:verify(V, Macaroon, Secret, DischargeMacaroons) of
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
    {ok, AuthDocs} = ?DB(get_auth_by_user_id, UserId),
    lists:foreach(fun(#db_document{uuid = AuthIdL}) ->
        invalidate_token(str_utils:to_binary(AuthIdL))
    end, AuthDocs),
    ok;
invalidate_token(Identifier) when is_binary(Identifier) ->
    ?DB(remove_auth, Identifier),
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
        {redirect_after_login, <<"/">>},
        % PROBABLY DEVELOPER-ONLY FUNCTIONALITY
        % If this value was set on login page, the user will be redirected to
        % this certain provider if he click "go to your files"
        {referer, erlang:get(referer)}
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
    Caveats :: [iodata()]) -> {ok, macaroon:macaroon()}.
create_macaroon(Secret, Identifier, Caveats) ->
    {ok, ExpirationSeconds} = application:get_env(?APP_Name,
        authorization_macaroon_expiration_seconds),
    ExpirationTime = utils:time() + ExpirationSeconds,

    Location = ?MACAROONS_LOCATION,

    {ok, M} = lists:foldl(
        fun(Caveat, {ok, Macaroon}) ->
            macaroon:add_first_party_caveat(Macaroon, Caveat)
        end,
        macaroon:create(Location, Secret, Identifier),
        [["time < ", integer_to_binary(ExpirationTime)] | Caveats]),

    {ok, M}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates a hex-encoded random secret for use with Macaroon.
%% @end
%%--------------------------------------------------------------------
-spec generate_secret() -> binary().
generate_secret() ->
    BinSecret = crypto:rand_bytes(macaroon:suggested_secret_length()),
    << <<Y>> ||<<X:4>> <= BinSecret, Y <- integer_to_list(X,16) >>.
