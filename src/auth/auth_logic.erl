%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles signing in to Onezone via different IdPs using
%%% Single Sign-On protocols (OpenId or SAML).
%%% @end
%%%-------------------------------------------------------------------
-module(auth_logic).

-include("auth/auth_errors.hrl").
-include("http/gui_paths.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").

% URL in IdP where the client should be redirected for authentication
-type login_endpoint() :: binary().
% URL where the client should be redirected after logging in to an IdP
-type redirect_uri() :: binary().
% Map of parsed parameters received in the query string on the consume endpoint (Key => Val)
-type query_params() :: #{}.
% OpenID access token, used to retrieve user info from an IdP
-type access_token() :: binary().
-export_type([
    login_endpoint/0,
    redirect_uri/0,
    query_params/0,
    access_token/0
]).

-type protocol_handler() :: saml_protocol | openid_protocol.

%% API
-export([get_login_endpoint/2, validate_login/2]).
-export([authorize_by_external_access_token/1]).
-export([authorize_by_macaroons/2]).
-export([authorize_by_basic_auth/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns an URL in where clients should be redirected for authentication
%% (either via SAML or OIDC) based on IdP.
%% Returns a map that includes three keys:
%%      <<"method">>
%%      <<"url">>
%%      <<"formData">>
%% that defines what request should be performed to redirect to the login page.
%% @end
%%--------------------------------------------------------------------
-spec get_login_endpoint(auth_config:idp(), LinkAccount :: false | {true, od_user:id()}) ->
    {ok, maps:map()} | {error, term()}.
get_login_endpoint(IdP, LinkAccount) ->
    try
        StateToken = auth_tokens:generate_state_token(IdP, LinkAccount),
        Handler = get_protocol_handler(IdP),
        case Handler:get_login_endpoint(IdP, StateToken) of
            {ok, Result} ->
                ?debug("Redirecting for login to IdP '~p' (state: ~s):~n~tp", [
                    IdP, StateToken, Result
                ]),
                {ok, Result};
            {error, _} = Err ->
                Err
        end
    catch
        throw:{error, _} = Error ->
            Error;
        Type:Reason ->
            ?error_stacktrace("Cannot resolve redirect URL for IdP '~p' - ~p:~p", [
                IdP, Type, Reason
            ]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


%%--------------------------------------------------------------------
%% @doc
%% Validates an incoming login request based on received data payload.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(new_gui:method(), cowboy_req:req()) ->
    {ok, od_user:id(), RedirectPage :: binary()} | {auth_error, {error, term()}, state_token:id()}.
validate_login(Method, Req) ->
    {StateToken, Payload} = parse_payload(Method, Req),
    case auth_tokens:lookup_state_token(StateToken) of
        error ->
            % This state token was not generated by us or has expired
            log_error(?ERROR_INVALID_STATE, undefined, StateToken, []),
            {auth_error, ?ERROR_INVALID_STATE, StateToken};
        {ok, #{idp := IdP} = StateInfo} ->
            try validate_login_by_state(Payload, StateToken, StateInfo) of
                {ok, UserId, RedirectPage} ->
                    {ok, UserId, RedirectPage};
                {error, Reason} ->
                    log_error({error, Reason}, IdP, StateToken, []),
                    {auth_error, {error, Reason}, StateToken}
            catch
                throw:Error ->
                    log_error(Error, IdP, StateToken, erlang:get_stacktrace()),
                    {auth_error, Error, StateToken};
                Type:Reason ->
                    log_error({Type, Reason}, IdP, StateToken, erlang:get_stacktrace()),
                    {auth_error, ?ERROR_INTERNAL_SERVER_ERROR, StateToken}
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to a authorize a client by an access token originating from an
%% Identity Provider.
%% {true, Client} - client was authorized
%% false - this method cannot verify authorization, other methods should be tried
%% {error, term()} - authorization invalid
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_external_access_token(access_token()) ->
    {true, entity_logic:client()} | false | {error, term()}.
authorize_by_external_access_token(AccessToken) ->
    case openid_protocol:authorize_by_external_access_token(AccessToken) of
        false ->
            false;
        {error, _} = Error ->
            Error;
        {true, {IdP, Attributes}} ->
            LinkedAccount = attribute_mapping:map_attributes(IdP, Attributes),
            {ok, #document{key = UserId}} = acquire_user_by_linked_account(LinkedAccount),
            {true, ?USER(UserId)}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to a authorize a client by macaroons.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_macaroons(Macaroon :: binary() | macaroon:macaroon(),
    DischargeMacaroons :: binary() | [macaroon:macaroon()]) ->
    {true, entity_logic:client()} | {error, term()}.
authorize_by_macaroons(Macaroon, DischargeMacaroons) when is_binary(Macaroon) ->
    case onedata_macaroons:deserialize(Macaroon) of
        {ok, DeserializedMacaroon} ->
            authorize_by_macaroons(DeserializedMacaroon, DischargeMacaroons);
        {error, _} ->
            ?ERROR_BAD_MACAROON
    end;
authorize_by_macaroons(Macaroon, <<"">>) ->
    authorize_by_macaroons(Macaroon, []);
authorize_by_macaroons(Macaroon, [Bin | _] = DischMacaroons) when is_binary(Bin) ->
    try
        DeserializedDischMacaroons = [
            begin {ok, DM} = onedata_macaroons:deserialize(S), DM end || S <- DischMacaroons
        ],
        authorize_by_macaroons(Macaroon, DeserializedDischMacaroons)
    catch
        _:_ ->
            ?ERROR_BAD_MACAROON
    end;
authorize_by_macaroons(Macaroon, DischargeMacaroons) ->
    %% Pass empty string as providerId because we do
    %% not expect the macaroon to have provider caveat
    %% (this is an authorization code for client).
    case auth_tokens:validate_token(<<>>, Macaroon, DischargeMacaroons, <<"">>, undefined) of
        {ok, UserId} ->
            {true, #client{type = user, id = UserId}};
        _ ->
            case macaroon_logic:verify_provider_auth(Macaroon) of
                {ok, IdP} ->
                    {true, #client{type = provider, id = IdP}};
                {error, _} ->
                    ?ERROR_BAD_MACAROON
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to a authorize a client by basic auth credentials.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_basic_auth(UserPasswdB64 :: binary()) ->
    {true, entity_logic:client()} | {error, bad_basic_credentials}.
authorize_by_basic_auth(UserPasswdB64) ->
    UserPasswd = base64:decode(UserPasswdB64),
    [User, Passwd] = binary:split(UserPasswd, <<":">>),
    case user_logic:authenticate_by_basic_credentials(User, Passwd) of
        {ok, #document{key = UserId}} ->
            Client = #client{type = user, id = UserId},
            {true, Client};
        {error, onepanel_auth_disabled} ->
            ?ERROR_NOT_SUPPORTED;
        _ ->
            ?ERROR_BAD_BASIC_CREDENTIALS
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec validate_login_by_state(Payload :: #{}, state_token:id(), state_token:state_info()) ->
    {ok, od_user:id(), RedirectPage :: binary()} | {error, term()}.
validate_login_by_state(Payload, StateToken, #{idp := IdP} = StateInfo) ->
    Handler = get_protocol_handler(IdP),
    ?debug("Login attempt from IdP '~p' (state: ~s), payload:~n~tp", [
        IdP, StateToken, Payload
    ]),
    {ok, Attributes} = Handler:validate_login(IdP, Payload),
    ?debug("Login from IdP '~p' (state: ~s) validated, attributes:~n~tp", [
        IdP, StateToken, Attributes
    ]),
    validate_login_by_attributes(IdP, Attributes, StateToken, StateInfo).


%% @private
-spec validate_login_by_attributes(auth_config:idp(), attribute_mapping:idp_attributes(),
    state_token:id(), state_token:state_info()) ->
    {ok, od_user:id(), RedirectPage :: binary()} | {error, term()}.
validate_login_by_attributes(IdP, Attributes, StateToken, StateInfo) ->
    LinkedAccount = attribute_mapping:map_attributes(IdP, Attributes),
    ?debug("Attributes from IdP '~p' (state: ~s) sucessfully mapped:~n~tp", [
        IdP, StateToken, LinkedAccount
    ]),
    ShouldLinkAccount = maps:get(link_account, StateInfo),
    RedirectAfterLogin = maps:get(redirect_after_login, StateInfo),
    case ShouldLinkAccount of
        false ->
            % Standard login, check if there is an account belonging to the user
            {ok, #document{key = UserId, value = #od_user{
                name = UserName
            }}} = acquire_user_by_linked_account(LinkedAccount),
            ?info("User ~ts (~s) has logged in", [UserName, UserId]),
            {ok, UserId, RedirectAfterLogin};
        {true, UserId} ->
            % Account adding flow - check if this account isn't connected to other profile
            case get_user_by_linked_account(LinkedAccount) of
                {ok, #document{key = UserId}} ->
                    % The account is already linked to this user, report error
                    ?ERROR_ACCOUNT_ALREADY_LINKED_TO_CURRENT_USER(UserId);
                {ok, #document{key = OtherUserId}} ->
                    % The account is used on some other profile, cannot proceed
                    ?ERROR_ACCOUNT_ALREADY_LINKED_TO_ANOTHER_USER(UserId, OtherUserId);
                {error, not_found} ->
                    % ok, add new linked account to the user
                    {ok, #document{key = UserId, value = #od_user{
                        name = UserName
                    }}} = user_logic:merge_linked_account(UserId, LinkedAccount),
                    ?info("User ~ts (~s) has linked his account from '~p'", [
                        UserName, UserId, IdP
                    ]),
                    {ok, UserId, <<?AFTER_LOGIN_PAGE_PATH, "?expand_accounts=true">>}
            end
    end.


%% @private
-spec get_protocol_handler(auth_config:idp()) -> protocol_handler().
get_protocol_handler(IdP) ->
    case auth_config:get_protocol(IdP) of
        saml -> saml_protocol;
        openid -> openid_protocol
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses OIDC / SAML payload and returns it as a map, along with resolved state token.
%% @end
%%--------------------------------------------------------------------
-spec parse_payload(new_gui:method(), cowboy_req:req()) ->
    {state_token:id(), Payload :: #{}}.
parse_payload(<<"POST">>, Req) ->
    {ok, PostBody, _} = cowboy_req:read_urlencoded_body(Req, #{length => 128000}),
    StateToken = proplists:get_value(<<"RelayState">>, PostBody, <<>>),
    {StateToken, maps:from_list(PostBody)};
parse_payload(<<"GET">>, Req) ->
    QueryParams = cowboy_req:parse_qs(Req),
    StateToken = proplists:get_value(<<"state">>, QueryParams, <<>>),
    {StateToken, maps:from_list(QueryParams)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves a user based on given linked account. Merges the info received
%% from IdP with user doc. If the account is not connected to any user, returns
%% {error, not_found}.
%% @end
%%--------------------------------------------------------------------
-spec get_user_by_linked_account(od_user:linked_account()) ->
    {ok, od_user:doc()} | {error, not_found}.
get_user_by_linked_account(LinkedAccount) ->
    #linked_account{idp = IdP, subject_id = SubjectId} = LinkedAccount,
    case od_user:get_by_criterion({linked_account, {IdP, SubjectId}}) of
        {ok, #document{key = UserId}} ->
            user_logic:merge_linked_account(UserId, LinkedAccount);
        {error, not_found} ->
            {error, not_found}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to retrieve system user by given linked account, and if it does not
%% exist, creates a new user with that account connected.
%% @end
%%--------------------------------------------------------------------
-spec acquire_user_by_linked_account(od_user:linked_account()) ->
    {ok, od_user:doc()}.
acquire_user_by_linked_account(LinkedAccount) ->
    case get_user_by_linked_account(LinkedAccount) of
        {ok, #document{} = Doc} ->
            {ok, Doc};
        {error, not_found} ->
            user_logic:create_user_by_linked_account(LinkedAccount)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs authentication errors with proper severity and message, depending on the
%% error type.
%% @end
%%--------------------------------------------------------------------
-spec log_error({Type :: term(), Reason :: term()}, auth_config:idp(),
    state_token:id(), Stacktrace :: term()) -> ok.
log_error(?ERROR_BAD_AUTH_CONFIG, _, _, _) ->
    % Logging is done when this error is generated
    ok;
log_error(?ERROR_INVALID_STATE, _, StateToken, _) ->
    ?debug(
        "Cannot validate login request - invalid state ~s (not found)",
        [StateToken]
    );
log_error(?ERROR_INVALID_AUTH_REQUEST, IdP, StateToken, Stacktrace) ->
    ?debug(
        "Cannot validate login request for IdP '~p' (state: ~s) - invalid auth request~n"
        "Stacktrace: ~s", [IdP, StateToken, iolist_to_binary(lager:pr_stacktrace(Stacktrace))]
    );
log_error(?ERROR_IDP_UNREACHABLE(Reason), IdP, StateToken, _) ->
    ?warning(
        "Cannot validate login request for IdP '~p' (state: ~s) - IdP not reachable: ~p",
        [IdP, StateToken, Reason]
    );
log_error(?ERROR_BAD_IDP_RESPONSE(Endpoint, Code, Headers, Body), IdP, StateToken, _) ->
    ?warning(
        "Cannot validate login request for IdP '~p' (state: ~s) - unexpected response from IdP:~n"
        "Endpoint: ~s~n"
        "Code: ~p~n"
        "Headers: ~p~n"
        "Body: ~s",
        [IdP, StateToken, Endpoint, Code, Headers, Body]
    );
log_error(?ERROR_CANNOT_RESOLVE_REQUIRED_ATTRIBUTE(Attr), IdP, StateToken, _) ->
    ?debug(
        "Cannot map attributes for IdP '~p' (state: ~s) - atrribute '~p' not found",
        [IdP, StateToken, Attr]
    );
log_error(?ERROR_BAD_ATTRIBUTE_TYPE(Attribute, Type), IdP, StateToken, _) ->
    ?debug(
        "Cannot map attributes for IdP '~p' (state: ~s) - atrribute '~p' "
        "does not have the required type '~p'",
        [IdP, StateToken, Attribute, Type]
    );
log_error(?ERROR_ATTRIBUTE_MAPPING_ERROR(Attribute, IdPAttributes, EType, EReason, Stacktrace), IdP, StateToken, _) ->
    ?debug(
        "Cannot map attributes for IdP '~p' (state: ~s) - atrribute '~p' "
        "could not be mapped due to an error - ~p:~p~n"
        "IdP attributes: ~p~n"
        "Stacktrace: ~s",
        [IdP, StateToken, Attribute, EType, EReason, IdPAttributes, iolist_to_binary(lager:pr_stacktrace(Stacktrace))]
    );
log_error(?ERROR_ACCOUNT_ALREADY_LINKED_TO_CURRENT_USER(UserId), IdP, StateToken, _) ->
    ?debug(
        "Cannot link account from IdP '~p' for user '~s' (state: ~s) - account already linked to the user",
        [IdP, UserId, StateToken]
    );
log_error(?ERROR_ACCOUNT_ALREADY_LINKED_TO_ANOTHER_USER(UserId, OtherUserId), IdP, StateToken, _) ->
    ?debug(
        "Cannot link account from IdP '~p' for user '~s' (state: ~s) - account already linked to user '~s'",
        [IdP, UserId, StateToken, OtherUserId]
    );
log_error(?ERROR_INTERNAL_SERVER_ERROR, IdP, StateToken, _) ->
    % The logging is already done when throwing this error
    ?debug(
        "Cannot validate login request for IdP '~p' (state: ~s) - internal server error",
        [IdP, StateToken]
    );
log_error({Type, Reason}, IdP, StateToken, Stacktrace) ->
    ?error(
        "Cannot validate login request for IdP '~p' (state: ~s) - ~p:~p~n"
        "Stacktrace: ~s", [IdP, StateToken, Type, Reason, iolist_to_binary(lager:pr_stacktrace(Stacktrace))]
    ).