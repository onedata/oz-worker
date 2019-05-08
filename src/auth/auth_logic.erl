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

-include("auth/auth_common.hrl").
-include("auth/auth_errors.hrl").
-include("http/gui_paths.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
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
-type access_token_ttl() :: non_neg_integer().
% Refresh token - used to refresh access tokens
-type refresh_token() :: binary().
-export_type([
    login_endpoint/0,
    redirect_uri/0,
    query_params/0,
    access_token/0,
    access_token_ttl/0,
    refresh_token/0
]).

-type protocol_handler() :: saml_protocol | openid_protocol.

-define(NOW(), time_utils:cluster_time_seconds()).
-define(REFRESH_THRESHOLD, oz_worker:get_env(idp_access_token_refresh_threshold, 300)).

%% API
-export([get_login_endpoint/4, validate_login/2]).
-export([authorize_by_access_token/1]).
-export([authorize_by_macaroons/1, authorize_by_macaroons/2]).
-export([authorize_by_oneprovider_gui_macaroon/1, authorize_by_onezone_gui_macaroon/1]).
-export([authorize_by_gui_macaroon/3]).
-export([authorize_by_basic_auth/1]).
-export([acquire_idp_access_token/2, refresh_idp_access_token/2]).

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
-spec get_login_endpoint(auth_config:idp(), LinkAccount :: false | {true, od_user:id()},
    RedirectAfterLogin :: binary(), TestMode :: boolean()) ->
    {ok, maps:map()} | {error, term()}.
get_login_endpoint(IdP, LinkAccount, RedirectAfterLogin, TestMode) ->
    TestMode andalso auth_test_mode:process_enable_test_mode(),
    try
        StateToken = auth_tokens:generate_state_token(
            IdP, LinkAccount, RedirectAfterLogin, TestMode
        ),
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
-spec validate_login(gui:method(), cowboy_req:req()) ->
    {ok, od_user:id(), RedirectPage :: binary()} |
    {auth_error, {error, term()}, state_token:id(), RedirectPage :: binary()}.
validate_login(Method, Req) ->
    {StateToken, Payload} = parse_payload(Method, Req),
    case auth_tokens:lookup_state_token(StateToken) of
        error ->
            % This state token was not generated by us or has expired
            log_error(?ERROR_INVALID_STATE, undefined, StateToken, []),
            {auth_error, ?ERROR_INVALID_STATE, StateToken, <<?LOGIN_PAGE_PATH>>};
        {ok, #{idp := IdP, redirect_after_login := RedirectAfterLogin} = StateInfo} ->
            try validate_login_by_state(Payload, StateToken, StateInfo) of
                {ok, UserId} ->
                    {ok, UserId, RedirectAfterLogin};
                {error, Reason} ->
                    log_error({error, Reason}, IdP, StateToken, []),
                    {auth_error, {error, Reason}, StateToken, RedirectAfterLogin}
            catch
                throw:{error, _} = Error ->
                    log_error(Error, IdP, StateToken, erlang:get_stacktrace()),
                    {auth_error, Error, StateToken, RedirectAfterLogin};
                Type:Reason ->
                    log_error({Type, Reason}, IdP, StateToken, erlang:get_stacktrace()),
                    {auth_error, ?ERROR_INTERNAL_SERVER_ERROR, StateToken, RedirectAfterLogin}
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Tries to a authorize a client by an access token - either a macaroon issued
%% by Onezone or an external access token - originating from an Identity Provider.
%% {true, Client} - client was authorized
%% false - this method cannot verify authorization, other methods should be tried
%% {error, term()} - authorization invalid
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_access_token(AccessToken :: binary()) ->
    {true, entity_logic:client()} | false | {error, term()}.
authorize_by_access_token(AccessToken) when is_binary(AccessToken) ->
    case authorize_by_macaroons(AccessToken, <<"">>) of
        {true, Client1} ->
            {true, Client1};
        _ ->
            case openid_protocol:authorize_by_idp_access_token(AccessToken) of
                {true, {IdP, Attributes}} ->
                    LinkedAccount = attribute_mapping:map_attributes(IdP, Attributes),
                    {ok, #document{key = UserId}} = acquire_user_by_linked_account(LinkedAccount),
                    {true, ?USER(UserId)};
                {error, _} = Error ->
                    Error;
                false ->
                    ?ERROR_BAD_MACAROON
            end
    end;
authorize_by_access_token(Req) ->
    case cowboy_req:header(<<"x-auth-token">>, Req, undefined) of
        undefined ->
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                <<"Bearer ", AccessToken/binary>> ->
                    authorize_by_access_token(AccessToken);
                _ ->
                    false
            end;
        XAuthToken ->
            authorize_by_access_token(XAuthToken)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to a authorize a client by macaroons based on cowboy req.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_macaroons(Req :: cowboy_req:req()) ->
    false | {true, #client{}} | {error, term()}.
authorize_by_macaroons(Req) ->
    case cowboy_req:header(<<"macaroon">>, Req, undefined) of
        undefined ->
            false;
        Macaroon ->
            DischargeMacaroons = cowboy_req:header(<<"discharge-macaroons">>, Req, <<"">>),
            authorize_by_macaroons(Macaroon, DischargeMacaroons)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to a authorize a client by macaroons.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_macaroons(Macaroon :: binary() | macaroon:macaroon(),
    DischargeMacaroons :: binary() | [binary()] | [macaroon:macaroon()]) ->
    {true, entity_logic:client()} | {error, term()}.
authorize_by_macaroons(Macaroon, DischargeMacaroons) when is_binary(Macaroon) ->
    case onedata_macaroons:deserialize(Macaroon) of
        {ok, DeserializedMacaroon} ->
            authorize_by_macaroons(DeserializedMacaroon, DischargeMacaroons);
        {error, _} ->
            ?ERROR_BAD_MACAROON
    end;
authorize_by_macaroons(Macaroon, SerializedDischarges) when is_binary(SerializedDischarges) ->
    authorize_by_macaroons(Macaroon, binary:split(SerializedDischarges, <<" ">>, [global, trim_all]));
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
            {true, ?USER(UserId)};
        _ ->
            case macaroon_logic:verify_provider_auth(Macaroon) of
                {ok, ProviderId} ->
                    {true, ?PROVIDER(ProviderId)};
                {error, _} ->
                    ?ERROR_BAD_MACAROON
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to a authorize a client by a GUI macaroon issued for a
%% op_worker or op_panel service.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_oneprovider_gui_macaroon(cowboy_req:req()) ->
    false | {true, entity_logic:client(), session:id()} | {error, term()}.
authorize_by_oneprovider_gui_macaroon(Req) ->
    SubjectToken = cowboy_req:header(<<"subject-token">>, Req, undefined),
    AudienceToken = cowboy_req:header(<<"audience-token">>, Req, undefined),
    case {SubjectToken, AudienceToken} of
        {undefined, undefined} ->
            false;
        {_, undefined} ->
            ?ERROR_UNAUTHORIZED;
        {undefined, _} ->
            ?ERROR_UNAUTHORIZED;
        {_, _} ->
            authorize_by_oneprovider_gui_macaroon(SubjectToken, AudienceToken)
    end.

%% @private
-spec authorize_by_oneprovider_gui_macaroon(binary(), binary()) ->
    {true, entity_logic:client(), session:id()} | {error, term()}.
authorize_by_oneprovider_gui_macaroon(SubjectToken, AudienceToken) ->
    try
        {ok, SubjectMacaroon} = onedata_macaroons:deserialize(SubjectToken),
        {ok, AudienceMacaroon} = onedata_macaroons:deserialize(AudienceToken),
        case macaroon_logic:verify_provider_auth(AudienceMacaroon) of
            {ok, ProviderId} ->
                authorize_by_gui_macaroon(SubjectMacaroon, ?ONEPROVIDER, ProviderId);
            {error, _} ->
                ?ERROR_MACAROON_INVALID
        end
    catch
        _:_ ->
            ?ERROR_BAD_MACAROON
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to a authorize a client by a GUI macaroon issued for the
%% oz_worker or oz_panel service.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_onezone_gui_macaroon(macaroon:macaroon() | binary()) ->
    {true, entity_logic:client(), session:id()} | {error, term()}.
authorize_by_onezone_gui_macaroon(Macaroon) ->
    authorize_by_gui_macaroon(Macaroon, ?ONEZONE, ?ONEZONE_CLUSTER_ID).


%% @private
-spec authorize_by_gui_macaroon(macaroon:macaroon() | binary(),
    onedata:cluster_type(), od_cluster:id()) ->
    {true, entity_logic:client(), session:id()} | {error, term()}.
authorize_by_gui_macaroon(Token, ClusterType, ClusterId) when is_binary(Token) ->
    case onedata_macaroons:deserialize(Token) of
        {ok, Macaroon} ->
            authorize_by_gui_macaroon(Macaroon, ClusterType, ClusterId);
        {error, _} = Error ->
            Error
    end;
authorize_by_gui_macaroon(Macaroon, ClusterType, ClusterId) ->
    case session:verify_gui_macaroon(Macaroon, ClusterType, ClusterId) of
        {ok, UserId, SessionId} -> {true, ?USER(UserId), SessionId};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to a authorize a client by basic auth credentials.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_basic_auth(UserPasswdB64 :: binary() | cowboy_req:req()) ->
    {true, #client{}} | {error, term()}.
authorize_by_basic_auth(UserPasswdB64) when is_binary(UserPasswdB64) ->
    UserPasswd = base64:decode(UserPasswdB64),
    [User, Passwd] = binary:split(UserPasswd, <<":">>),
    case user_logic:authenticate_by_basic_credentials(User, Passwd) of
        {ok, UserId} ->
            {true, ?USER(UserId)};
        {error, onepanel_auth_disabled} ->
            ?ERROR_NOT_SUPPORTED;
        _ ->
            ?ERROR_BAD_BASIC_CREDENTIALS
    end;
authorize_by_basic_auth(Req) ->
    case cowboy_req:header(<<"authorization">>, Req, undefined) of
        <<"Basic ", UserPasswdB64/binary>> ->
            authorize_by_basic_auth(UserPasswdB64);
        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @doc
%% Acquires an access token for given user, issued by given IdP.
%% Returns ?ERROR_NOT_FOUND when:
%%  * the user does not have an account in such IdP
%%  * there is no access token stored
%%  * the stored access token has expired and there is no viable refresh token
%% Can return ?ERROR_INTERNAL_SERVER_ERROR in case token refresh goes wrong.
%% @end
%%--------------------------------------------------------------------
-spec acquire_idp_access_token([od_user:linked_account()], auth_config:idp()) ->
    {ok, {access_token(), access_token_ttl()}} | {error, term()}.
acquire_idp_access_token(LinkedAccounts, IdP) ->
    lists:foldl(fun
        (_LinkedAccount, {ok, Result}) ->
            {ok, Result};
        (#linked_account{idp = CurrentIdP} = LinkedAcc, _) when CurrentIdP == IdP ->
            acquire_idp_access_token(LinkedAcc);
        (_, Acc) ->
            Acc
    end, ?ERROR_NOT_FOUND, LinkedAccounts).

%% @private
-spec acquire_idp_access_token(od_user:linked_account()) ->
    {ok, {access_token(), access_token_ttl()}} | {error, term()}.
acquire_idp_access_token(#linked_account{access_token = {undefined, 0}, refresh_token = _}) ->
    ?ERROR_NOT_FOUND;
acquire_idp_access_token(#linked_account{access_token = {AccessToken, Expires}, refresh_token = undefined}) ->
    % No refresh token - no point in trying to refresh the access token
    Now = ?NOW(),
    case Expires > Now of
        true -> {ok, {AccessToken, Expires - Now}};
        false -> ?ERROR_NOT_FOUND
    end;
acquire_idp_access_token(#linked_account{idp = IdP, access_token = {AccessToken, Expires}, refresh_token = RefreshToken}) ->
    Now = ?NOW(),
    case Expires - ?REFRESH_THRESHOLD > Now of
        true -> {ok, {AccessToken, Expires - Now}};
        false -> refresh_idp_access_token(IdP, RefreshToken)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Acquires a new access token using given refresh token.
%% @end
%%--------------------------------------------------------------------
-spec refresh_idp_access_token(auth_config:idp(), refresh_token()) ->
    {ok, {auth_logic:access_token(), auth_logic:access_token_ttl()}} | {error, term()}.
refresh_idp_access_token(IdP, RefreshToken) ->
    try
        {ok, Attributes} = openid_protocol:refresh_idp_access_token(IdP, RefreshToken),
        LinkedAccount = attribute_mapping:map_attributes(IdP, Attributes),
        {ok, _} = acquire_user_by_linked_account(LinkedAccount),
        #linked_account{access_token = {AccessToken, Expires}} = LinkedAccount,
        {ok, {AccessToken, Expires - ?NOW()}}
    catch
        throw:Error ->
            log_error(Error, IdP, <<"refresh_token_flow">>, erlang:get_stacktrace()),
            ?ERROR_INTERNAL_SERVER_ERROR;
        Type:Reason ->
            log_error({Type, Reason}, IdP, <<"refresh_token_flow">>, erlang:get_stacktrace()),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec validate_login_by_state(Payload :: #{}, state_token:id(), state_token:state_info()) ->
    {ok, od_user:id()} | {error, term()}.
validate_login_by_state(Payload, StateToken, #{idp := IdP, test_mode := TestMode, link_account := LinkAccount}) ->
    TestMode andalso auth_test_mode:process_enable_test_mode(),
    Handler = get_protocol_handler(IdP),
    ?auth_debug("Login attempt from IdP '~p' (state: ~s), payload:~n~tp", [
        IdP, StateToken, Payload
    ]),

    {ok, Attributes} = Handler:validate_login(IdP, Payload),
    % Do not print sensitive information
    PrintableAttributes = maps:without([<<"access_token">>, <<"refresh_token">>], Attributes),
    ?auth_debug("Login from IdP '~p' (state: ~s) validated, attributes:~n~ts", [
        IdP, StateToken, json_utils:encode(PrintableAttributes, [pretty])
    ]),

    LinkedAccount = attribute_mapping:map_attributes(IdP, Attributes),
    LinkedAccountMap = maps:without(
        [<<"login">>, <<"emailList">>, <<"groups">>], % do not include deprecated fields
        user_logic:linked_account_to_map(LinkedAccount)
    ),
    ?auth_debug("Attributes from IdP '~p' (state: ~s) sucessfully mapped:~n~ts", [
        IdP, StateToken, json_utils:encode(LinkedAccountMap, [pretty])
    ]),

    case {auth_test_mode:process_is_test_mode_enabled(), LinkAccount} of
        {true, _} ->
            validate_test_login_by_linked_account(LinkedAccount);
        {false, false} ->
            validate_login_by_linked_account(LinkedAccount);
        {false, {true, UserId}} ->
            validate_link_account_request(LinkedAccount, UserId)
    end.


%% @private
-spec validate_login_by_linked_account(od_user:linked_account()) ->
    {ok, od_user:id()}.
validate_login_by_linked_account(LinkedAccount) ->
    {ok, #document{key = UserId, value = #od_user{
        name = UserName
    }}} = acquire_user_by_linked_account(LinkedAccount),
    ?info("User '~ts' has logged in (~s)", [UserName, UserId]),
    {ok, UserId}.


%% @private
-spec validate_link_account_request(od_user:linked_account(), od_user:id()) ->
    {ok, od_user:id()} | {error, term()}.
validate_link_account_request(LinkedAccount, UserId) ->
    % Check if this account isn't connected to other profile
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
                UserName, UserId, LinkedAccount#linked_account.idp
            ]),
            {ok, UserId}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Rather than creating a user and his groups, stores info that was gathered
%% in the test login process for later use by the page_consume_login module.
%% @end
%%--------------------------------------------------------------------
-spec validate_test_login_by_linked_account(od_user:linked_account()) ->
    {ok, od_user:id()}.
validate_test_login_by_linked_account(LinkedAccount) ->
    {UserId, UserData} = user_logic:build_test_user_info(LinkedAccount),
    auth_test_mode:store_user_data(UserData),
    {ok, UserId}.


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
-spec parse_payload(gui:method(), cowboy_req:req()) ->
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
log_error(?ERROR_BAD_AUTH_CONFIG, _, _, Stacktrace) ->
    ?auth_debug(
        "Login request failed due to bad auth config: ~s", [
            iolist_to_binary(lager:pr_stacktrace(Stacktrace))
        ]
    );
log_error(?ERROR_INVALID_STATE, _, StateToken, _) ->
    ?auth_debug(
        "Cannot validate login request - invalid state ~s (not found)",
        [StateToken]
    );
log_error(?ERROR_INVALID_AUTH_REQUEST, IdP, StateToken, Stacktrace) ->
    ?auth_debug(
        "Cannot validate login request for IdP '~p' (state: ~s) - invalid auth request~n"
        "Stacktrace: ~s", [IdP, StateToken, iolist_to_binary(lager:pr_stacktrace(Stacktrace))]
    );
log_error(?ERROR_IDP_UNREACHABLE(Reason), IdP, StateToken, _) ->
    ?auth_warning(
        "Cannot validate login request for IdP '~p' (state: ~s) - IdP not reachable: ~p",
        [IdP, StateToken, Reason]
    );
log_error(?ERROR_BAD_IDP_RESPONSE(Endpoint, Code, Headers, Body), IdP, StateToken, _) ->
    ?auth_warning(
        "Cannot validate login request for IdP '~p' (state: ~s) - unexpected response from IdP:~n"
        "Endpoint: ~s~n"
        "Code: ~p~n"
        "Headers: ~p~n"
        "Body: ~s",
        [IdP, StateToken, Endpoint, Code, Headers, Body]
    );
log_error(?ERROR_CANNOT_RESOLVE_REQUIRED_ATTRIBUTE(Attr), IdP, StateToken, _) ->
    ?auth_debug(
        "Cannot map attributes for IdP '~p' (state: ~s) - atrribute '~p' not found",
        [IdP, StateToken, Attr]
    );
log_error(?ERROR_BAD_ATTRIBUTE_TYPE(Attribute, Type), IdP, StateToken, _) ->
    ?auth_debug(
        "Cannot map attributes for IdP '~p' (state: ~s) - atrribute '~p' "
        "does not have the required type '~p'",
        [IdP, StateToken, Attribute, Type]
    );
log_error(?ERROR_ATTRIBUTE_MAPPING_ERROR(Attribute, IdPAttributes, EType, EReason, Stacktrace), IdP, StateToken, _) ->
    ?auth_debug(
        "Cannot map attributes for IdP '~p' (state: ~s) - atrribute '~p' "
        "could not be mapped due to an error - ~p:~p~n"
        "IdP attributes: ~p~n"
        "Stacktrace: ~s",
        [IdP, StateToken, Attribute, EType, EReason, IdPAttributes, iolist_to_binary(lager:pr_stacktrace(Stacktrace))]
    );
log_error(?ERROR_ACCOUNT_ALREADY_LINKED_TO_CURRENT_USER(UserId), IdP, StateToken, _) ->
    ?auth_debug(
        "Cannot link account from IdP '~p' for user '~s' (state: ~s) - account already linked to the user",
        [IdP, UserId, StateToken]
    );
log_error(?ERROR_ACCOUNT_ALREADY_LINKED_TO_ANOTHER_USER(UserId, OtherUserId), IdP, StateToken, _) ->
    ?auth_debug(
        "Cannot link account from IdP '~p' for user '~s' (state: ~s) - account already linked to user '~s'",
        [IdP, UserId, StateToken, OtherUserId]
    );
log_error(?ERROR_INTERNAL_SERVER_ERROR, IdP, StateToken, _) ->
    % The logging is already done when throwing this error
    ?auth_debug(
        "Cannot validate login request for IdP '~p' (state: ~s) - internal server error",
        [IdP, StateToken]
    );
log_error(Error, IdP, StateToken, Stacktrace) ->
    ?auth_error(
        "Cannot validate login request for IdP '~p' (state: ~s) - ~p~n"
        "Stacktrace: ~s", [IdP, StateToken, Error, iolist_to_binary(lager:pr_stacktrace(Stacktrace))]
    ).