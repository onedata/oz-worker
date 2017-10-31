%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module provides functionalities used in authentication flow
%%% and convenience functions that can be used from auth modules.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_utils).

-include("datastore/oz_datastore_models.hrl").
-include("auth_common.hrl").
-include("gui/common.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("esaml/include/esaml.hrl").

%% API

% Authenticating based on various credentials
-export([
    authorize_by_oauth_provider/1,
    authorize_by_macaroons/2,
    authorize_by_provider_certs/1,
    authorize_by_basic_auth/1
]).

% Convenience functions
-export([local_auth_endpoint/0, get_value_binary/2, extract_emails/1,
    has_group_mapping_enabled/1, normalize_membership_spec/2]).

% Authentication flow handling
-export([
    get_all_idps/0,
    get_super_group/1,
    get_redirect_url/2,
    get_saml_redirect_url/2,
    validate_oidc_login/0,
    validate_saml_login/0,
    get_user_by_linked_account/1,
    acquire_user_by_linked_account/1,
    acquire_user_by_external_access_token/2
]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Tries to authenticate client by provided token, if its prefix matches
%% any of the configured oauth providers supporting authority delegation.
%% {true, Client} - client was authorized
%% false - this method cannot verify authorization, other methods should be tried
%% {error, term()} - authorization invalid
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_oauth_provider(AccessToken :: binary()) ->
    {true, #client{}} | false | {error, term()}.
authorize_by_oauth_provider(AccessToken) ->
    AuthDelegationProviders = auth_config:get_providers_with_auth_delegation(),
    MatchingProviders = lists:dropwhile(
        fun({_ProvId, TokenPrefix}) ->
            % If prefix matches, return false:
            % dropwhile will stop and return the rest
            % of the list with matched provider at the beginning
            not bin_starts_with(AccessToken, TokenPrefix)
        end, AuthDelegationProviders),
    case MatchingProviders of
        [] ->
            false;
        [{ProviderId, TokPrefix} | _] ->
            Len = byte_size(TokPrefix),
            <<TokPrefix:Len/binary, AccessTokenNoPrefix/binary>> = AccessToken,
            case auth_utils:acquire_user_by_external_access_token(
                ProviderId, AccessTokenNoPrefix
            ) of
                {error, bad_access_token} ->
                    {error, {bad_access_token, ProviderId}};
                {_, #document{key = UserId}} ->
                    {true, #client{type = user, id = UserId}}
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to authenticate client by macaroons.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_macaroons(Macaroon :: binary() | macaroon:macaroon(),
    DischargeMacaroons :: binary() | [macaroon:macaroon()]) ->
    {true, #client{}} | {error, term()}.
authorize_by_macaroons(Macaroon, DischargeMacaroons) when is_binary(Macaroon) ->
    case token_utils:deserialize(Macaroon) of
        {ok, DeserializedMacaroon} ->
            authorize_by_macaroons(DeserializedMacaroon, DischargeMacaroons);
        {error, _} ->
            {error, bad_macaroon}
    end;
authorize_by_macaroons(Macaroon, <<"">>) ->
    authorize_by_macaroons(Macaroon, []);
authorize_by_macaroons(Macaroon, [Bin | _] = DischMacaroons) when is_binary(Bin) ->
    try
        DeserializedDischMacaroons = [
            begin {ok, DM} = token_utils:deserialize(S), DM end || S <- DischMacaroons
        ],
        authorize_by_macaroons(Macaroon, DeserializedDischMacaroons)
    catch
        _:_ ->
            {error, bad_macaroon}
    end;
authorize_by_macaroons(Macaroon, DischargeMacaroons) ->
    %% @todo: VFS-1869
    %% Pass empty string as providerId because we do
    %% not expect the macaroon to have provider caveat
    %% (it is an authorization code for client).
    case auth_logic:validate_token(<<>>, Macaroon,
        DischargeMacaroons, <<"">>, undefined) of
        {ok, UserId} ->
            Client = #client{type = user, id = UserId},
            {true, Client};
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to authenticate client by provider certs.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_provider_certs(PeerCertDer :: public_key:der_encoded()) ->
    {true, #client{}} | {error, bad_cert}.
authorize_by_provider_certs(PeerCert) ->
    case worker_proxy:call(ozpca_worker, {verify_provider, PeerCert}) of
        {ok, ProviderId} ->
            % Make sure that provider exists - the client might hold a valid
            % certificate, but for a provider that has been removed.
            case provider_logic:exists(ProviderId) of
                true -> {true, #client{type = provider, id = ProviderId}};
                false -> {error, bad_cert}
            end;
        {error, {bad_cert, Reason}} ->
            ?warning("Attempted authentication with "
            "bad peer certificate: ~p", [Reason]),
            {error, bad_cert}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to authenticate client by provider certs.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_basic_auth(UserPasswdB64 :: binary()) ->
    {true, #client{}} | {error, bad_cert}.
authorize_by_basic_auth(UserPasswdB64) ->
    UserPasswd = base64:decode(UserPasswdB64),
    [User, Passwd] = binary:split(UserPasswd, <<":">>),
    case user_logic:authenticate_by_basic_credentials(User, Passwd) of
        {ok, #document{key = UserId}, _} ->
            Client = #client{type = user, id = UserId},
            {true, Client};
        _ ->
            {error, bad_credentials}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the URL that will be used to redirect back authentication flow.
%% @end
%%--------------------------------------------------------------------
-spec local_auth_endpoint() -> binary().
local_auth_endpoint() ->
    <<(http_utils:fully_qualified_url(gui_ctx:get_requested_hostname()))/binary,
        ?local_auth_endpoint>>.

%%--------------------------------------------------------------------
%% @doc
%% Gets value from a proplist and converts it to binary, if needed.
%% Useful in auth_xxx modules which are required to return binaries in the
%% #linked_account{} record.
%% @end
%%--------------------------------------------------------------------
-spec get_value_binary(Key :: term(), Proplist :: proplists:proplist()) ->
    binary().
get_value_binary(Key, Proplist) ->
    case proplists:get_value(Key, Proplist, <<"">>) of
        Bin when is_binary(Bin) ->
            Bin;
        Str when is_list(Str) ->
            str_utils:unicode_list_to_binary(Str);
        null ->
            <<"">>;
        Other ->
            str_utils:to_binary(Other)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Extracts email list from a JSON proplist (standard for OpenId).
%% @end
%%--------------------------------------------------------------------
-spec extract_emails(proplists:proplist()) -> [binary()].
extract_emails(JSONProplist) ->
    extract_emails(<<"email">>, JSONProplist).


%%--------------------------------------------------------------------
%% @doc
%% Extracts email list from a JSON proplist (standard for OpenId).
%% @end
%%--------------------------------------------------------------------
-spec extract_emails(EmailKey :: term(), proplists:proplist()) -> [binary()].
extract_emails(EmailKey, JSONProplist) ->
    case get_value_binary(EmailKey, JSONProplist) of
        <<"">> -> [];
        Email -> [Email]
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all configured identity providers (OIDC + SAML).
%% @end
%%--------------------------------------------------------------------
-spec get_all_idps() -> [atom()].
get_all_idps() ->
    auth_config:get_auth_providers() ++ saml_config:get_supported_idps().


%%--------------------------------------------------------------------
%% @doc
%% Returns super group identifier for given IdP, or undefined.
%% @end
%%--------------------------------------------------------------------
-spec get_super_group(ProviderId :: atom()) ->
    undefined | idp_group_mapping:group_spec().
get_super_group(ProviderId) ->
    case lists:member(ProviderId, auth_config:get_auth_providers()) of
        true -> auth_config:get_super_group(ProviderId);
        false -> saml_config:get_super_group(ProviderId)
    end.


%%--------------------------------------------------------------------
%% @doc Returns full URL, where the user will be redirected for authorization
%% (either via SAML or OIDC) based on provider id as specified in
%% auth.config and saml.config.
%% Returns a map that includes three keys:
%%      <<"method">>
%%      <<"url">>
%%      <<"formData">>
%% that defines what request should be performed to redirect to login page.
%% @end
%%--------------------------------------------------------------------
-spec get_redirect_url(ProviderId :: atom(), ConnectAccount :: boolean()) ->
    {ok, proplists:proplist()} | {error, term()}.
get_redirect_url(ProviderId, ConnectAccount) ->
    case lists:member(ProviderId, auth_config:get_auth_providers()) of
        true ->
            HandlerModule = auth_config:get_provider_module(ProviderId),
            {ok, Url} = HandlerModule:get_redirect_url(ConnectAccount),
            {ok, [
                {<<"method">>, <<"get">>},
                {<<"url">>, Url},
                {<<"formData">>, null}
            ]};
        false ->
            get_saml_redirect_url(ProviderId, ConnectAccount)
    end.


%%--------------------------------------------------------------------
%% @doc Returns full URL, where the user will be redirected for authorization
%% via SAML based on provider id as specified in saml.config.
%% @end
%%--------------------------------------------------------------------
-spec get_saml_redirect_url(ProviderId :: atom(), ConnectAccount :: boolean()) ->
    {ok, proplists:proplist()} | {error, term()}.
get_saml_redirect_url(ProviderId, ConnectAccount) ->
    IdpConfig = #esaml_idp{
        preferred_sso_binding = PreferredSSOBinding
    } = saml_config:get_idp_config(ProviderId),
    LoginLocation = case PreferredSSOBinding of
        http_redirect ->
            IdpConfig#esaml_idp.metadata#esaml_idp_metadata.redirect_login_location;
        http_post ->
            IdpConfig#esaml_idp.metadata#esaml_idp_metadata.post_login_location
    end,
    case LoginLocation of
        undefined -> throw({cannot_resolve_sso_url_for_provider, ProviderId});
        _ -> ok
    end,
    State = auth_logic:generate_state_token(ProviderId, ConnectAccount),
    SP = saml_config:get_sp_config(),
    AuthNReq = esaml_sp:generate_authn_request(LoginLocation, SP),
    case PreferredSSOBinding of
        http_redirect ->
            Url = esaml_binding:encode_http_redirect(LoginLocation, AuthNReq, State),
            {ok, [
                {<<"method">>, <<"get">>},
                {<<"url">>, Url},
                {<<"formData">>, null}
            ]};
        http_post ->
            FormData = maps:to_list(esaml_binding:encode_http_post_form_data(AuthNReq, State)),
            {ok, [
                {<<"method">>, <<"post">>},
                {<<"url">>, iolist_to_binary(LoginLocation)},
                {<<"formData">>, FormData}
            ]}
    end.


%%--------------------------------------------------------------------
%% @doc Function used to validate login by processing a redirect that came from
%% an OIDC identity provider. Must be called from gui context to work. Returns one of the following:
%% 1. atom 'new_user' if the login has been verified and a new user has been created
%% 2. {redirect, URL} if the account already exists, to state where the user should be redirected now
%% 3. {error, Description} if the validation failed or any other error occurred.
%% @end
%%--------------------------------------------------------------------
-spec validate_oidc_login() -> new_user | {redirect, URL :: binary()} | {error, term()}.
validate_oidc_login() ->
    try
        % Check url params for state parameter and validate it
        ParamsProplist = gui_ctx:get_url_params(),
        StateToken = proplists:get_value(<<"state">>, ParamsProplist),
        StateInfo = validate_state_token(StateToken),
        Module = maps:get(module, StateInfo),

        % Validate the request and gather user info
        case Module:validate_login() of
            {ok, LinkedAccount} ->
                validate_login_by_linked_account(LinkedAccount, StateInfo);
            {error, bad_access_token} ->
                {error, ?error_auth_access_token_invalid};
            {error, Reason} ->
                % The request could not be validated
                ?warning("Invalid login request via OIDC. Reason:~p~nRequest params:~n~p~nPOST params:~n~p",
                    [Reason, ParamsProplist, gui_ctx:get_form_params()]),
                {error, ?error_auth_invalid_request}
        end
    catch
        throw:{error, E} ->
            {error, E};
        T:M ->
            ?error_stacktrace("Error in validate_oidc_login - ~p:~p", [T, M]),
            {error, ?error_auth_server_error}
    end.


%%--------------------------------------------------------------------
%% @doc Function used to validate login by processing a redirect that came from
%% a SAML identity provider. Must be called from gui context to work. Returns one of the following:
%% 1. atom 'new_user' if the login has been verified and a new user has been created
%% 2. {redirect, URL} if the account already exists, to state where the user should be redirected now
%% 3. {error, Description} if the validation failed or any other error occurred.
%% @end
%%--------------------------------------------------------------------
-spec validate_saml_login() -> new_user | {redirect, URL :: binary()} | {error, term()}.
validate_saml_login() ->
    try
        SP = saml_config:get_sp_config(),
        % Check url params for state parameter and validate it
        {ok, PostVals, _} = cowboy_req:body_qs(gui_ctx:get_cowboy_req(), [{length, 128000}]),
        SAMLEncoding = proplists:get_value(<<"SAMLEncoding">>, PostVals),
        SAMLResponse = proplists:get_value(<<"SAMLResponse">>, PostVals),
        StateToken = proplists:get_value(<<"RelayState">>, PostVals),
        StateInfo = validate_state_token(StateToken),
        IdPId = maps:get(module, StateInfo),

        IdP = saml_config:get_idp_config(IdPId),

        case (catch esaml_binding:decode_response(SAMLEncoding, SAMLResponse)) of
            {'EXIT', Reason1} ->
                ?warning("Cannot decode SAML request. Reason:~p~nPOST params:~n~p",
                    [Reason1, PostVals]),
                {error, ?error_auth_invalid_request};
            Xml ->
                case esaml_sp:validate_assertion(Xml, SP, IdP) of
                    {ok, #esaml_assertion{attributes = Attributes}} ->
                        ?info("Login attempt from IdP '~p', attributes: ~n~p~n", [IdPId, Attributes]),
                        LinkedAccount = saml_assertions_to_linked_account(IdPId, IdP, Attributes),
                        validate_login_by_linked_account(LinkedAccount, StateInfo);
                    {error, Reason2} ->
                        % The request could not be validated
                        ?warning("Invalid login request via SAML. Reason:~p~nPOST params:~n~p",
                            [Reason2, PostVals]),
                        {error, ?error_auth_invalid_request}
                end
        end
    catch
        throw:{error, E} ->
            {error, E};
        T:M ->
            ?error_stacktrace("Error in validate_saml_login - ~p:~p", [T, M]),
            {error, ?error_auth_server_error}
    end.

%%-------------------------------------------------------------------
%% @doc
%% WRITEME
%% @end
%%-------------------------------------------------------------------
-spec normalize_membership_spec(ProviderId :: atom(), GroupIds :: binary()) ->
    idp_group_mapping:membership_spec().
normalize_membership_spec(ProviderId, GroupId) ->
    case lists:member(ProviderId, auth_config:get_auth_providers()) of
        true -> auth_config:normalize_membership_spec(ProviderId, GroupId);
        false -> saml_config:normalize_membership_spec(ProviderId, GroupId)
    end.

%%-------------------------------------------------------------------
%% @doc
%% Returns whether given provider has group mapping enabled.
%% @end
%%-------------------------------------------------------------------
-spec has_group_mapping_enabled(ProviderId :: atom()) -> boolean().
has_group_mapping_enabled(ProviderId) ->
    case lists:member(ProviderId, auth_config:get_auth_providers()) of
        true -> auth_config:has_group_mapping_enabled(ProviderId);
        false -> saml_config:has_group_mapping_enabled(ProviderId)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validates login based on linked account that was obtained from
%% OIDC / SAML login.
%% @end
%%--------------------------------------------------------------------
-spec validate_login_by_linked_account(OriginalLinkedAccount :: #linked_account{},
    StateInfo :: state_token:state_info()) ->
    new_user | {redirect, URL :: binary()} | {error, term()}.
validate_login_by_linked_account(LinkedAccount, StateInfo) ->
    ConnectAccount = maps:get(connect_account, StateInfo),
    RedirectAfterLogin = maps:get(redirect_after_login, StateInfo),
    case ConnectAccount of
        false ->
            case gui_session:is_logged_in() of
                true -> gui_session:log_out();
                false -> ok
            end,
            % Standard login, check if there is an account belonging to the user
            case acquire_user_by_linked_account(LinkedAccount) of
                {exists, #document{key = UserId}} ->
                    user_logic:merge_linked_account(UserId, LinkedAccount),
                    gui_session:log_in(UserId),
                    {redirect, RedirectAfterLogin};
                {created, #document{key = UserId}} ->
                    user_logic:merge_linked_account(UserId, LinkedAccount),
                    gui_session:log_in(UserId),
                    gui_session:put_value(firstLogin, true),
                    new_user
            end;
        true ->
            % Account adding flow
            % Check if this account isn't connected to other profile
            UserId = gui_session:get_user_id(),
            case get_user_by_linked_account(LinkedAccount) of
                {ok, #document{key = OtherUser}} when OtherUser /= UserId ->
                    % The account is used on some other profile, cannot proceed
                    {error, ?error_auth_account_already_linked_to_another_user};
                {ok, #document{key = UserId}} ->
                    % The account is used on some other profile, cannot proceed
                    {error, ?error_auth_account_already_linked_to_current_user};
                _ ->
                    % Not found = ok, get the user and add new provider info
                    user_logic:merge_linked_account(UserId, LinkedAccount),
                    {redirect, <<?PAGE_AFTER_LOGIN, "?expand_accounts=true">>}
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Lookups the state token and returns data that was assigned to it, or throws
%% an error that will be caught higher and reported to the browser.
%% @end
%%--------------------------------------------------------------------
-spec validate_state_token(state_token:id()) -> state_token:state_info().
validate_state_token(StateToken) ->
    case auth_logic:lookup_state_token(StateToken) of
        error ->
            % This state token was not generated by us or expired, invalid
            throw({error, ?error_auth_invalid_state});
        {ok, StateInfo} ->
            StateInfo
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts SAML attributes to linked account data base on attribute_mapping
%% specified in saml.config.
%% @end
%%--------------------------------------------------------------------
-spec saml_assertions_to_linked_account(IdPId :: atom(), IdP :: #esaml_idp{},
    Attributes :: proplists:proplist()) -> #linked_account{}.
saml_assertions_to_linked_account(IdPId, IdP, Attributes) ->
    #esaml_idp{attribute_mapping = AttributeMapping} = IdP,
    UserId = case get_value_binary(maps:get(id, AttributeMapping), Attributes) of
        <<"">> ->
            throw(user_id_attribute_not_received);
        Val ->
            Val
    end,
    Groups = case saml_config:has_group_mapping_enabled(IdPId) of
        false ->
            [];
        true ->
            GroupsAttr = proplists:get_value(
                maps:get(groups, AttributeMapping, undefined),
                Attributes, []
            ),
            % Groups attribute can be either a string or a list of strings
            GroupsStrings = case GroupsAttr of
                [Str | _] when is_list(Str) -> GroupsAttr;
                Str when is_list(Str) -> [Str]
            end,
            [str_utils:unicode_list_to_binary(G) || G <- GroupsStrings]
    end,
    #linked_account{
        provider_id = IdPId,
        user_id = UserId,
        email_list = extract_emails(maps:get(email, AttributeMapping, undefined), Attributes),
        name = get_value_binary(maps:get(name, AttributeMapping, undefined), Attributes),
        login = get_value_binary(maps:get(login, AttributeMapping, undefined), Attributes),
        groups = saml_config:normalize_membership_specs(IdPId, Groups)
    }.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves system user based on given linked account. If the account is not
%% connected to any user, returns {error, not_found}.
%% @end
%%--------------------------------------------------------------------
-spec get_user_by_linked_account(#linked_account{}) -> {ok, #document{}} | {error, not_found}.
get_user_by_linked_account(LinkedAccount) ->
    #linked_account{provider_id = ProviderId, user_id = UserId} = LinkedAccount,
    case od_user:get_by_criterion({linked_account, {ProviderId, UserId}}) of
        {ok, #document{} = Doc} ->
            {ok, Doc};
        {error, Reason} ->
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to retrieve system user by given linked account, and if it does not
%% exist, creates a new user with that account connected. If a new user is
%% created, the function returns when its effective relations have been
%% fully synchronized.
%% In both cases, returns the user id.
%% @end
%%--------------------------------------------------------------------
-spec acquire_user_by_linked_account(#linked_account{}) ->
    {exists | created, UserDoc :: #document{}}.
acquire_user_by_linked_account(LinkedAccount) ->
    case get_user_by_linked_account(LinkedAccount) of
        {ok, #document{} = Doc} ->
            {exists, Doc};
        {error, not_found} ->
            {ok, UserId} = user_logic:create_user_by_linked_account(LinkedAccount),
            entity_graph:ensure_up_to_date(),
            {ok, Doc} = od_user:get(UserId),
            {created, Doc}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to retrieve system user by given linked account, and if it does not
%% exist, creates a new user with that account connected. If a new user is
%% created, the function returns when its effective relations have been
%% fully synchronized.
%% In both cases, returns the user id.
%% @end
%%--------------------------------------------------------------------
-spec acquire_user_by_external_access_token(ProviderId :: atom(), AccessToken :: binary()) ->
    {exists | created, UserDoc :: #document{}} | {error, bad_access_token}.
acquire_user_by_external_access_token(ProviderId, AccessToken) ->
    Module = auth_config:get_provider_module(ProviderId),
    case Module:get_user_info(AccessToken) of
        {ok, LinkedAccount} ->
            acquire_user_by_linked_account(LinkedAccount);
        {error, bad_access_token} ->
            {error, bad_access_token}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if given binary starts with given prefix.
%% @end
%%--------------------------------------------------------------------
-spec bin_starts_with(Subject :: binary(), Prefix :: binary()) -> boolean().
bin_starts_with(Subject, Prefix) ->
    FirstChars = binary:part(Subject, 0, byte_size(Prefix)),
    FirstChars =:= Prefix.

