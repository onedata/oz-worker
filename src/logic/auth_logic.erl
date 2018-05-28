%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2014 ACK CYFRONET AGH
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

-include("http/gui_paths.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

% String that will be placed in macaroons' location field
-define(MACAROONS_LOCATION, <<"onezone">>).

%% API
-export([get_redirection_uri/2, get_redirection_uri/3]).
-export([gen_token/1, gen_token/2, validate_token/5,
    invalidate_token/1, invalidate_user_tokens/1,
    authenticate_user/1]).

%% Handling state tokens
-export([generate_state_token/2, lookup_state_token/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Attempt to authenticate user versus user id found in database.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_user(Identifier :: binary()) -> {ok, Token :: binary()} | {error, any()}.
authenticate_user(Identifier) ->
    case onedata_auth:get(Identifier) of
        {ok, #document{value = #onedata_auth{secret = Secret}}} ->

            {ok, ExpirationSecs} = oz_worker:get_env(
                authentication_macaroon_expiration_seconds),

            Location = ?MACAROONS_LOCATION,
            M = macaroon:create(Location, Secret, Identifier),
            M2 = macaroon:add_first_party_caveat(M,
                ["time < ", integer_to_binary(time_utils:cluster_time_seconds() + ExpirationSecs)]),

            case onedata_macaroons:serialize(M2) of
                {ok, _} = Serialized -> Serialized;
                {error, _} = Error -> Error
            end;

        _ ->
            {error, no_auth_record}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @equiv get_redirection_uri(UserId, ProviderId, <<"/">>).
%% @end
%%--------------------------------------------------------------------
-spec get_redirection_uri(UserId :: binary(), ProviderId :: binary()) ->
    {ok, RedirectionUri :: binary()}.
get_redirection_uri(UserId, ProviderId) ->
    get_redirection_uri(UserId, ProviderId, <<"/">>).


%%--------------------------------------------------------------------
%% @doc
%% Returns the full URI to which the user should be redirected from the onezone.
%% The redirection is part of the OpenID flow and the URI contains an
%% Authorization token. RedirectPath is an absolute path in oneprovider GUI
%% where user will be redirected upon successful login.
%% @end
%%--------------------------------------------------------------------
-spec get_redirection_uri(UserId :: binary(), ProviderId :: binary(),
    RedirectPath :: binary()) ->
    {ok, RedirectionUri :: binary()}.
get_redirection_uri(UserId, ProviderId, RedirectPath) ->
    Token = gen_token(UserId, ProviderId),
    {ok, ProviderURL} = provider_logic:get_url(ProviderId),
    URL = str_utils:format_bin("~s~s?code=~s&redirect-path=~s", [
        ProviderURL,
        ?PROVIDER_LOGIN_CONSUME_PATH_DEPRECATED,
        Token,
        http_utils:url_encode(RedirectPath)
    ]),
    {ok, URL}.


%%--------------------------------------------------------------------
%% @doc Creates an authorization code for a native client.
%%--------------------------------------------------------------------
-spec gen_token(UserId :: binary()) -> Token :: binary().
gen_token(UserId) ->
    Secret = generate_secret(),
    Caveats = [],%["method = GET", "rootResource in spaces,user"],
    {ok, Doc} = onedata_auth:save(#document{value = #onedata_auth{
        secret = Secret, user_id = UserId
    }}),
    Identifier = binary_to_list(Doc#document.key),
    M = create_macaroon(Secret, str_utils:to_binary(Identifier), Caveats),
    {ok, Token} = onedata_macaroons:serialize(M),
    Token.

%%--------------------------------------------------------------------
%% @doc Creates an authorization code for a Provider.
%%--------------------------------------------------------------------
-spec gen_token(UserId :: binary(), ProviderId :: binary() | undefined) ->
    Token :: binary().
gen_token(UserId, _ProviderId) ->
    Secret = generate_secret(),
    Location = ?MACAROONS_LOCATION,
    {ok, Doc} = onedata_auth:save(#document{value = #onedata_auth{
        secret = Secret, user_id = UserId
    }}),
    Identifier = binary_to_list(Doc#document.key),
    %% @todo: VFS-1869
    M = create_macaroon(Secret, str_utils:to_binary(Identifier), []),

    CaveatKey = generate_secret(),
    {ok, CaveatDoc} = onedata_auth:save(#document{value = #onedata_auth{
        secret = CaveatKey, user_id = UserId
    }}),
    CaveatId = binary_to_list(CaveatDoc#document.key),
    M2 = macaroon:add_third_party_caveat(M, Location, CaveatKey, CaveatId),
    {ok, Token} = onedata_macaroons:serialize(M2),
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
validate_token(ProviderId, Macaroon, DischargeMacaroons, _Method, _RootResource) ->
    Identifier = macaroon:identifier(Macaroon),
    case onedata_auth:get(Identifier) of
        {ok, #document{value = #onedata_auth{secret = Secret, user_id = UserId}}} ->
            V = macaroon_verifier:create(),

            VerifyFun = fun
                (<<"time < ", Integer/binary>>) ->
                    time_utils:cluster_time_seconds() < binary_to_integer(Integer);
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
%% @doc Invalidates all auth tokens of given user.
%% @end
%%--------------------------------------------------------------------
-spec invalidate_user_tokens(UserId :: od_user:id()) -> ok.
invalidate_user_tokens(UserId) ->
    {ok, AuthDocs} = onedata_auth:get_by_user_id(UserId),
    lists:foreach(
        fun(#document{key = AuthId}) ->
            invalidate_token(AuthId)
        end, AuthDocs).


%%--------------------------------------------------------------------
%% @doc Invalidates a given auth token.
%% @end
%%--------------------------------------------------------------------
-spec invalidate_token(binary()) -> ok.
invalidate_token(Identifier) when is_binary(Identifier) ->
    onedata_auth:delete(Identifier).


%%--------------------------------------------------------------------
%% @doc Generates a state token and returns it. In the process, it stores the token
%% and associates some login info, that can be later retrieved given the token.
%% For example, where to redirect the user after login.
%% @end
%%--------------------------------------------------------------------
-spec generate_state_token(auth_utils:idp(), LinkAccount :: boolean()) ->
    state_token:id().
generate_state_token(IdP, LinkAccount) ->
    StateInfo = #{
        idp => IdP,
        link_account => LinkAccount,
        % Right now this always redirects to main page, although
        % might be used in the future.
        redirect_after_login => <<?AFTER_LOGIN_PAGE_PATH>>
    },
    {ok, Token} = state_token:create(StateInfo),
    Token.

%%--------------------------------------------------------------------
%% @doc Checks if the given state token exists and returns login info
%% associated with it or error otherwise.
%% @end
%%--------------------------------------------------------------------
-spec lookup_state_token(Token :: binary()) ->
    {ok, state_token:state_info()} | error.
lookup_state_token(Token) ->
    state_token:lookup(Token).

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
    {ok, ExpirationSeconds} = oz_worker:get_env(
        authorization_macaroon_expiration_seconds),
    ExpirationTime = time_utils:cluster_time_seconds() + ExpirationSeconds,

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
    BinSecret = crypto:strong_rand_bytes(macaroon:suggested_secret_length()),
    <<<<Y>> || <<X:4>> <= BinSecret, Y <- integer_to_list(X, 16)>>.
