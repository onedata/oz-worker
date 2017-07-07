%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module provides functionalities used in authentication flow
%%% and convenience functions that can be used from auth modules.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_utils).

-include("datastore/oz_datastore_models_def.hrl").
-include("auth_common.hrl").
-include("gui/common.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("esaml/include/esaml.hrl").

%% API
% Convenience functions
-export([local_auth_endpoint/0, get_value_binary/2, extract_emails/1]).

% Authentication flow handling
-export([
    get_all_idps/0,
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
%% @doc Returns full URL, where the user will be redirected for authorization
%% (either via SAML or OIDC) based on provider id as specified in
%% auth.config and saml.config.
%% @end
%%--------------------------------------------------------------------
-spec get_redirect_url(ProviderId :: atom(), ConnectAccount :: boolean()) ->
    {ok, binary()} | {error, term()}.
get_redirect_url(ProviderId, ConnectAccount) ->
    case lists:member(ProviderId, auth_config:get_auth_providers()) of
        true ->
            HandlerModule = auth_config:get_provider_module(ProviderId),
            HandlerModule:get_redirect_url(ConnectAccount);
        false ->
            get_saml_redirect_url(ProviderId, ConnectAccount)
    end.


%%--------------------------------------------------------------------
%% @doc Returns full URL, where the user will be redirected for authorization
%% via SAML based on provider id as specified in saml.config.
%% @end
%%--------------------------------------------------------------------
-spec get_saml_redirect_url(ProviderId :: atom(), ConnectAccount :: boolean()) ->
    {ok, binary()} | {error, term()}.
get_saml_redirect_url(ProviderId, ConnectAccount) ->
    #esaml_idp{
        metadata = #esaml_idp_metadata{login_location = LoginLocation}
    } = saml_config:get_idp_config(ProviderId),
    State = auth_logic:generate_state_token(ProviderId, ConnectAccount),
    SP = saml_config:get_sp_config(),
    Xml = esaml_sp:generate_authn_request(LoginLocation, SP),
    {ok, esaml_binding:encode_http_redirect(LoginLocation, Xml, State)}.


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
        StateProps = validate_state_token(StateToken),
        Module = proplists:get_value(module, StateProps),
        RedirectAfterLogin = proplists:get_value(redirect_after_login, StateProps),

        % Validate the request and gather user info
        case Module:validate_login() of
            {error, Reason} ->
                % The request could not be validated
                ?warning("Invalid login request via OIDC. Reason:~p~nRequest params:~n~p~nPOST params:~n~p",
                    [Reason, ParamsProplist, gui_ctx:get_form_params()]),
                {error, ?error_auth_invalid_request};
            {ok, LinkedAccount} ->
                ConnectAccount = proplists:get_value(connect_account, StateProps),
                validate_login_by_linked_account(
                    LinkedAccount, ConnectAccount, RedirectAfterLogin
                )
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
        StateProps = validate_state_token(StateToken),
        IdPId = proplists:get_value(module, StateProps),
        RedirectAfterLogin = proplists:get_value(redirect_after_login, StateProps),

        IdP = saml_config:get_idp_config(IdPId),

        case (catch esaml_binding:decode_response(SAMLEncoding, SAMLResponse)) of
            {'EXIT', Reason1} ->
                ?warning("Cannot decode SAML request. Reason:~p~nPOST params:~n~p",
                    [Reason1, PostVals]),
                {error, ?error_auth_invalid_request};
            Xml ->
                ConnectAccount = proplists:get_value(connect_account, StateProps),
                case esaml_sp:validate_assertion(Xml, SP, IdP) of
                    {ok, #esaml_assertion{attributes = Attributes}} ->
                        LinkedAccount = saml_assertions_to_linked_account(IdPId, IdP, Attributes),
                        validate_login_by_linked_account(LinkedAccount, ConnectAccount, RedirectAfterLogin);
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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validates login based on linked account that was obtained from
%% OIDC / SAML login.
%% @end
%%--------------------------------------------------------------------
-spec validate_login_by_linked_account(OriginalLinkedAccount :: #linked_account{},
    ConnectAccount :: boolean(), RedirectAfterLogin :: binary()) ->
    new_user | {redirect, URL :: binary()} | {error, term()}.
validate_login_by_linked_account(OriginalLinkedAccount, ConnectAccount, RedirectAfterLogin) ->
    #linked_account{
        email_list = OriginalEmails
    } = OriginalLinkedAccount,
    Emails = lists:map(fun(Email) ->
        http_utils:normalize_email(Email)
    end, OriginalEmails),
    LinkedAccount = OriginalLinkedAccount#linked_account{
        email_list = Emails
    },
    case ConnectAccount of
        false ->
            case gui_session:is_logged_in() of
                true -> gui_session:log_out();
                false -> ok
            end,
            % Standard login, check if there is an account belonging to the user
            case acquire_user_by_linked_account(LinkedAccount) of
                {exists, UserId} ->
                    gui_session:log_in(UserId),
                    {redirect, RedirectAfterLogin};
                {created, UserId} ->
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
                    {error, ?error_auth_account_already_connected};
                _ ->
                    % Not found = ok, get the user and add new provider info
                    ok = user_logic:add_linked_account(UserId, LinkedAccount),
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
-spec validate_state_token(StateToken :: binary()) -> proplists:proplist().
validate_state_token(StateToken) ->
    case auth_logic:lookup_state_token(StateToken) of
        error ->
            % This state token was not generated by us or expired, invalid
            throw({error, ?error_auth_invalid_state});
        StateProps ->
            StateProps
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
    #linked_account{
        provider_id = IdPId,
        user_id = UserId,
        email_list = extract_emails(maps:get(email, AttributeMapping, undefined), Attributes),
        name = get_value_binary(maps:get(name, AttributeMapping, undefined), Attributes),
        login = get_value_binary(maps:get(login, AttributeMapping, undefined), Attributes)
    }.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves system user based on given linked account. If the account is not
%% connected to any user, returns {error, not_found}.
%% @end
%%--------------------------------------------------------------------
-spec get_user_by_linked_account(#linked_account{}) -> {ok, #document{}} | {error, not_found}.
get_user_by_linked_account(LinkedAccount) ->
    #linked_account{provider_id = ProviderId, user_id = LinkedUserId} = LinkedAccount,
    case od_user:get_by_criterion({linked_account, {ProviderId, LinkedUserId}}) of
        {ok, #document{} = Doc} ->
            {ok, Doc};
        {error, {not_found, od_user}} ->
            {error, not_found}
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
    {exists | created, UserId :: od_user:id()}.
acquire_user_by_linked_account(LinkedAccount) ->
    case get_user_by_linked_account(LinkedAccount) of
        {ok, #document{key = UserId}} ->
            {exists, UserId};
        {error, not_found} ->
            {ok, UserId} = create_user_by_linked_account(LinkedAccount),
            entity_graph:ensure_up_to_date(),
            {created, UserId}
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
    {exists | created, UserId :: od_user:id()} | {error, bad_access_token}.
acquire_user_by_external_access_token(ProviderId, AccessToken) ->
    Module = auth_config:get_provider_module(ProviderId),
    case Module:get_user_info(AccessToken) of
        {ok, LinkedAccount} ->
            acquire_user_by_linked_account(LinkedAccount);
        {error, bad_access_token} ->
            {error, bad_access_token}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a new user based on given linked account. Before creating such user,
%% it must be ensured that user with such linked account does not exist.
%% @end
%%--------------------------------------------------------------------
-spec create_user_by_linked_account(#linked_account{}) ->
    {ok, UserId :: od_user:id()} | {error, not_found}.
create_user_by_linked_account(LinkedAccount) ->
    #linked_account{
        provider_id = IdPName,
        user_id = IdPUserId,
        email_list = Emails
    } = LinkedAccount,
    UserInfo = #od_user{
        email_list = Emails,
        name = resolve_name_from_linked_account(LinkedAccount),
        linked_accounts = [LinkedAccount]
    },
    UserId = user_logic:idp_uid_to_system_uid(IdPName, IdPUserId),
    user_logic:create(UserInfo, UserId).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Resolve what name should be set for user based on his linked account.
%% If user name was send by linked provider, use it.
%% If not, try this with login.
%% If not, try using the email (the part before @) as name.
%% If there is no email, return a generic "unknown" string.
%% @end
%%--------------------------------------------------------------------
-spec resolve_name_from_linked_account(#linked_account{}) -> binary().
resolve_name_from_linked_account(LinkedAccount) ->
    case LinkedAccount of
        #linked_account{name = <<"">>, login = <<"">>, email_list = []} ->
            <<"Unknown Name">>;
        #linked_account{name = <<"">>, login = <<"">>, email_list = EmailList} ->
            hd(binary:split(hd(EmailList), <<"@">>));
        #linked_account{name = <<"">>, login = Login} ->
            Login;
        #linked_account{name = Name} ->
            Name
    end.



