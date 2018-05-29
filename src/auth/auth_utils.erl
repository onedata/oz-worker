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
-include("http/gui_paths.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("esaml/include/esaml.hrl").
-include_lib("ctool/include/api_errors.hrl").

-type idp() :: atom().
-export_type([idp/0]).

%% API

% Authenticating based on various credentials
-export([
    authorize_by_oauth_provider/1,
    authorize_by_macaroons/2,
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
    validate_oidc_login/1,
    validate_saml_login/1,
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
        [{IdP, TokPrefix} | _] ->
            Len = byte_size(TokPrefix),
            <<TokPrefix:Len/binary, AccessTokenNoPrefix/binary>> = AccessToken,
            case auth_utils:acquire_user_by_external_access_token(
                IdP, AccessTokenNoPrefix
            ) of
                {error, bad_access_token} ->
                    ?ERROR_BAD_EXTERNAL_ACCESS_TOKEN(IdP);
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
    case auth_logic:validate_token(<<>>, Macaroon, DischargeMacaroons, <<"">>, undefined) of
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
%% Tries to authenticate client by provider certs.
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_basic_auth(UserPasswdB64 :: binary()) ->
    {true, #client{}} | {error, bad_cert}.
authorize_by_basic_auth(UserPasswdB64) ->
    UserPasswd = base64:decode(UserPasswdB64),
    [User, Passwd] = binary:split(UserPasswd, <<":">>),
    case user_logic:authenticate_by_basic_credentials(User, Passwd) of
        {ok, #document{key = UserId}} ->
            Client = #client{type = user, id = UserId},
            {true, Client};
        _ ->
            ?ERROR_BAD_BASIC_CREDENTIALS
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the URL that will be used to redirect back authentication flow.
%% @end
%%--------------------------------------------------------------------
-spec local_auth_endpoint() -> binary().
local_auth_endpoint() ->
    oz_worker:get_uri(<<?OIDC_CONSUME_PATH_DEPRECATED>>).


%%--------------------------------------------------------------------
%% @doc
%% Gets value from a map and converts it to binary, if needed.
%% Useful in auth_xxx modules which are required to return binaries in the
%% #linked_account{} record.
%% @end
%%--------------------------------------------------------------------
-spec get_value_binary(Key :: term(), Map :: maps:map()) -> binary().
get_value_binary(Key, Map) ->
    case maps:get(Key, Map, <<"">>) of
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
%% Extracts email list from a JSON map (standard for OpenId).
%% @end
%%--------------------------------------------------------------------
-spec extract_emails(maps:map()) -> [binary()].
extract_emails(JSON) ->
    extract_emails(<<"email">>, JSON).


%%--------------------------------------------------------------------
%% @doc
%% Extracts email list from a JSON map (standard for OpenId).
%% @end
%%--------------------------------------------------------------------
-spec extract_emails(EmailKey :: term(), maps:map()) -> [binary()].
extract_emails(EmailKey, JSON) ->
    case get_value_binary(EmailKey, JSON) of
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
-spec get_super_group(idp()) ->
    undefined | idp_group_mapping:group_spec().
get_super_group(IdP) ->
    case lists:member(IdP, auth_config:get_auth_providers()) of
        true -> auth_config:get_super_group(IdP);
        false -> saml_config:get_super_group(IdP)
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
-spec get_redirect_url(idp(), LinkAccount :: boolean()) ->
    {ok, maps:map()} | {error, term()}.
get_redirect_url(IdP, LinkAccount) ->
    case lists:member(IdP, auth_config:get_auth_providers()) of
        true ->
            HandlerModule = auth_config:get_provider_module(IdP),
            {ok, Url} = HandlerModule:get_redirect_url(IdP, LinkAccount),
            {ok, #{
                <<"method">> => <<"get">>,
                <<"url">> => Url,
                <<"formData">> => null
            }};
        false ->
            get_saml_redirect_url(IdP, LinkAccount)
    end.


%%--------------------------------------------------------------------
%% @doc Returns full URL, where the user will be redirected for authorization
%% via SAML based on provider id as specified in saml.config.
%% @end
%%--------------------------------------------------------------------
-spec get_saml_redirect_url(idp(), LinkAccount :: boolean()) ->
    {ok, maps:map()} | {error, term()}.
get_saml_redirect_url(IdP, LinkAccount) ->
    IdpConfig = #esaml_idp{
        preferred_sso_binding = PreferredSSOBinding
    } = saml_config:get_idp_config(IdP),
    LoginLocation = case PreferredSSOBinding of
        http_redirect ->
            IdpConfig#esaml_idp.metadata#esaml_idp_metadata.redirect_login_location;
        http_post ->
            IdpConfig#esaml_idp.metadata#esaml_idp_metadata.post_login_location
    end,
    case LoginLocation of
        undefined -> throw({cannot_resolve_sso_url_for_provider, IdP});
        _ -> ok
    end,
    State = auth_logic:generate_state_token(IdP, LinkAccount),
    SP = saml_config:get_sp_config(),
    AuthNReq = esaml_sp:generate_authn_request(LoginLocation, SP),
    case PreferredSSOBinding of
        http_redirect ->
            Url = esaml_binding:encode_http_redirect(LoginLocation, AuthNReq, State),
            {ok, #{
                <<"method">> => <<"get">>,
                <<"url">> => Url,
                <<"formData">> => null
            }};
        http_post ->
            FormData = maps:to_list(esaml_binding:encode_http_post_form_data(AuthNReq, State)),
            {ok, #{
                <<"method">> => <<"post">>,
                <<"url">> => iolist_to_binary(LoginLocation),
                <<"formData">> => FormData
            }}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Function used to validate login by processing a redirect that came from
%% an OIDC identity provider. Upon success, returns authenticated user id and
%% the URL where the user should be redirected after login.
%% @end
%%--------------------------------------------------------------------
-spec validate_oidc_login(cowboy_req:req()) ->
    {ok, {od_user:id(), RedirectUrl :: binary()}} | {error, term()}.
validate_oidc_login(Req) ->
    try
        QueryParams = cowboy_req:parse_qs(Req),
        % Check url params for state parameter and validate it
        StateToken = proplists:get_value(<<"state">>, QueryParams),
        StateInfo = validate_state_token(StateToken),
        IdP = maps:get(idp, StateInfo),
        HandlerModule = auth_config:get_provider_module(IdP),

        % Validate the request and gather user info
        case HandlerModule:validate_login(IdP, QueryParams) of
            {ok, LinkedAccount} ->
                validate_login_by_linked_account(Req, LinkedAccount, StateInfo);
            {error, bad_access_token} ->
                {error, ?error_auth_access_token_invalid};
            {error, Reason} ->
                % The request could not be validated
                ?warning("Invalid login request via OIDC. Reason:~p~nQuery params:~n~p",
                    [Reason, QueryParams]
                ),
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
%% a SAML identity provider. Upon success, returns authenticated user id and
%% the URL where the user should be redirected after login.
%% @end
%%--------------------------------------------------------------------
-spec validate_saml_login(cowboy_req:req()) ->
    {ok, {od_user:id(), RedirectUrl :: binary()}} | {error, term()}.
validate_saml_login(Req) ->
    try
        {ok, PostBody, _} = cowboy_req:read_urlencoded_body(Req, #{length => 128000}),
        SP = saml_config:get_sp_config(),
        % Check url params for state parameter and validate it
        SAMLEncoding = proplists:get_value(<<"SAMLEncoding">>, PostBody),
        SAMLResponse = proplists:get_value(<<"SAMLResponse">>, PostBody),
        StateToken = proplists:get_value(<<"RelayState">>, PostBody),
        StateInfo = validate_state_token(StateToken),
        IdPId = maps:get(idp, StateInfo),

        IdP = saml_config:get_idp_config(IdPId),

        case (catch esaml_binding:decode_response(SAMLEncoding, SAMLResponse)) of
            {'EXIT', Reason1} ->
                ?warning("Cannot decode SAML request. Reason:~p~nPOST params:~n~p",
                    [Reason1, PostBody]),
                {error, ?error_auth_invalid_request};
            Xml ->
                case esaml_sp:validate_assertion(Xml, SP, IdP) of
                    {ok, #esaml_assertion{attributes = Attributes}} ->
                        ?info("Login attempt from IdP '~p', attributes: ~n~p~n", [IdPId, Attributes]),
                        LinkedAccount = saml_assertions_to_linked_account(IdPId, IdP, maps:from_list(Attributes)),
                        validate_login_by_linked_account(Req, LinkedAccount, StateInfo);
                    {error, Reason2} ->
                        % The request could not be validated
                        ?warning("Invalid login request via SAML. Reason:~p~nPOST params:~n~p",
                            [Reason2, PostBody]),
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
-spec normalize_membership_spec(idp(), GroupIds :: binary()) ->
    idp_group_mapping:membership_spec().
normalize_membership_spec(IdP, GroupId) ->
    case lists:member(IdP, auth_config:get_auth_providers()) of
        true -> auth_config:normalize_membership_spec(IdP, GroupId);
        false -> saml_config:normalize_membership_spec(IdP, GroupId)
    end.

%%-------------------------------------------------------------------
%% @doc
%% Returns whether given provider has group mapping enabled.
%% @end
%%-------------------------------------------------------------------
-spec has_group_mapping_enabled(idp()) -> boolean().
has_group_mapping_enabled(IdP) ->
    case lists:member(IdP, auth_config:get_auth_providers()) of
        true -> auth_config:has_group_mapping_enabled(IdP);
        false -> saml_config:has_group_mapping_enabled(IdP)
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
-spec validate_login_by_linked_account(cowboy_req:req(),
    OriginalLinkedAccount :: #linked_account{},
    StateInfo :: state_token:state_info()) ->
    {ok, {od_user:id(), RedirectUrl :: binary()}} | {error, term()}.
validate_login_by_linked_account(Req, LinkedAccount, StateInfo) ->
    ShouldLinkAccount = maps:get(link_account, StateInfo),
    RedirectAfterLogin = maps:get(redirect_after_login, StateInfo),
    case ShouldLinkAccount of
        false ->
            % Standard login, check if there is an account belonging to the user
            case acquire_user_by_linked_account(LinkedAccount) of
                {exists, #document{key = UserId}} ->
                    {ok, {UserId, RedirectAfterLogin}};
                {created, #document{key = UserId}} ->
                    {ok, {UserId, ?AFTER_LOGIN_PAGE_PATH}}
            end;
        true ->
            % Account adding flow
            % Check if this account isn't connected to other profile
            {ok, UserId} = oz_gui_session:get_user_id(Req),
            case get_user_by_linked_account(LinkedAccount) of
                {ok, #document{key = UserId}} ->
                    % The account is already linked to this user, report error
                    {error, ?error_auth_account_already_linked_to_current_user};
                {ok, #document{key = _OtherUser}} ->
                    % The account is used on some other profile, cannot proceed
                    {error, ?error_auth_account_already_linked_to_another_user};
                {error, not_found} ->
                    % Not found -> ok, add new linked account to the user
                    user_logic:merge_linked_account(UserId, LinkedAccount),
                    entity_graph:ensure_up_to_date(),
                    {ok, {UserId, <<?AFTER_LOGIN_PAGE_PATH, "?expand_accounts=true">>}}
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
    Attributes :: maps:map()) -> #linked_account{}.
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
            GroupsAttr = maps:get(
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
        idp = IdPId,
        subject_id = UserId,
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
    #linked_account{idp = IdP, subject_id = SubjectId} = LinkedAccount,
    case od_user:get_by_criterion({linked_account, {IdP, SubjectId}}) of
        {ok, #document{key = UserId}} ->
            {ok, Doc} = user_logic:merge_linked_account(UserId, LinkedAccount),
            entity_graph:ensure_up_to_date(),
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
            {ok, Doc} = user_logic:create_user_by_linked_account(LinkedAccount),
            entity_graph:ensure_up_to_date(),
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
-spec acquire_user_by_external_access_token(idp(), AccessToken :: binary()) ->
    {exists | created, UserDoc :: #document{}} | {error, bad_access_token}.
acquire_user_by_external_access_token(IdP, AccessToken) ->
    Module = auth_config:get_provider_module(IdP),
    case Module:get_user_info(IdP, AccessToken) of
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

