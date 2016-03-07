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

-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("handlers/rest_handler.hrl").
-include("auth_common.hrl").
-include("gui/common.hrl").

%% API
% Convenience functions
-export([local_auth_endpoint/0]).

% Authentication flow handling
-export([validate_login/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the URL that will be used to redirect back authentication flow.
%% @end
%%--------------------------------------------------------------------
-spec local_auth_endpoint() -> binary().
local_auth_endpoint() ->
    <<(http_utils:fully_qualified_url(g_ctx:get_requested_hostname()))/binary,
    ?local_auth_endpoint>>.

%%--------------------------------------------------------------------
%% @doc Function used to validate login by processing a redirect that came from
%% an OAuth provider. Must be called from n2o context to work. Returns one of the following:
%% 1. atom 'new_user' if the login has been verified and a new user has been created
%% 2. {redirect, URL} if the account already exists, to state where the user should be redirected now
%% 3. {error, Desription} if the validation failed or any other error occurred.
%% @end
%%--------------------------------------------------------------------
-spec validate_login() -> new_user | {redirect, URL :: binary()} | {error, term()}.
validate_login() ->
    try
        % Check url params for state parameter
        ParamsProplist = g_ctx:get_url_params(),
        State = proplists:get_value(<<"state">>, ParamsProplist),
        StateInfo = auth_logic:lookup_state_token(State),
        case StateInfo of
            error ->
                % This state token was not generated by us, invalid
                ?alert("Security breach attempt spotted - invalid state. GET params:~n~p~nPOST params:~n~p",
                    [ParamsProplist, g_ctx:get_form_params()]),
                {error, ?error_auth_invalid_request};

            Props ->
                % State token ok, get handler module and redirect address connected with this request
                Module = proplists:get_value(module, Props),
                Redirect = proplists:get_value(redirect_after_login, Props),

                % Validate the request and gather user info
                case Module:validate_login() of
                    {error, Reason} ->
                        % The request could not be validated
                        ?alert("Security breach attempt spotted. Reason:~p~nRequest params:~n~p~nPOST params:~n~p",
                            [Reason, ParamsProplist, g_ctx:get_form_params()]),
                        {error, ?error_auth_invalid_request};

                    {ok, OriginalOAuthAccount = #oauth_account{provider_id = ProviderID, user_id = UserID, email_list = OriginalEmails, name = Name}} ->
                        Emails = lists:map(fun(Email) ->
                            http_utils:normalize_email(Email) end, OriginalEmails),
                        OAuthAccount = OriginalOAuthAccount#oauth_account{email_list = Emails},
                        Result = case proplists:get_value(connect_account, Props) of
                            false ->
                                % Standard login, check if there is an account belonging to the user
                                case user_logic:get_user_doc({connected_account_user_id, {ProviderID, UserID}}) of
                                    {ok, #document{key = UserId}} ->
                                        % The account already exists
                                        g_session:log_in(UserId),
                                        {redirect, Redirect};
                                    _ ->
                                        % Error
                                        % This is a first login
                                        % Check if any of emails is in use
                                        case is_any_email_in_use(Emails, <<"">>) of
                                            true ->
                                                % At least one email is in database, cannot create account
                                                {error, ?error_auth_new_email_occupied};
                                            false ->
                                                % All emails are available, proceed
                                                UserInfo = #onedata_user{email_list = Emails, name = Name, connected_accounts = [
                                                    OAuthAccount
                                                ]},
                                                {ok, UserId} = user_logic:create(UserInfo),
                                                % Create a default space for the user and generate a token to support it
                                                {ok, SpaceID} = space_logic:create({user, UserId}, <<Name/binary, "'s space">>),
                                                true = user_logic:set_default_space(UserId, SpaceID),
                                                {ok, Token} = token_logic:create(#client{type = user, id = UserId}, space_support_token, {space, SpaceID}),
                                                user_logic:modify(UserId, [{first_space_support_token, Token}]),
                                                g_session:log_in(UserId),
                                                g_session:put_value(firstLogin, true),
                                                new_user
                                        end
                                end;

                            true ->
                                ?dump(connectlol),
                                % Account adding flow
                                % Check if this account isn't connected to other profile
                                case user_logic:get_user({connected_account_user_id, {ProviderID, UserID}}) of
                                    {ok, _} ->
                                        % The account is used on some other profile, cannot proceed
                                        {error, ?error_auth_account_already_connected};
                                    _ ->
                                        % Not found, ok
                                        % Check if any of emails is in use by another user
                                        case is_any_email_in_use(Emails, g_session:get_user_id()) of
                                            true ->
                                                % At least one email is in database, cannot connect account
                                                {error, ?error_auth_connect_email_occupied};
                                            false ->
                                                % Everything ok, get the user and add new provider info
                                                UserId = g_session:get_user_id(),
                                                {ok, #onedata_user{} = UserRecord} = user_logic:get_user(UserId),
                                                ModificationProplist = merge_connected_accounts(OAuthAccount, UserRecord),
                                                user_logic:modify(UserId, ModificationProplist),
                                                {redirect, <<?page_after_login, "?expand_accounts=true">>}
                                        end
                                end
                        end,

                        % PROBABLY DEVELOPER-ONLY FUNCTIONALITY
                        % Save the referer URL in user session. If it was specified, this will cause the user
                        % to be redirected to given provider when he clicks "go to your files"
                        g_session:put_value(referer, proplists:get_value(referer, Props)),

                        Result
                end
        end
    catch
        T:M ->
            ?error_stacktrace("Error in validate_login - ~p:~p", [T, M]),
            {error, ?error_auth_invalid_request}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Returns true if any of the given emails is occupied (but not by the specified user).
%%--------------------------------------------------------------------
-spec is_any_email_in_use(Emails :: [binary()], binary()) -> true | false.
is_any_email_in_use(Emails, UserID) ->
    lists:foldl(
        fun(Email, Acc) ->
            case Acc of
                true ->
                    true;
                _ ->
                    case user_logic:get_user_doc({email, Email}) of
                        {ok, #document{key = UserID}} ->
                            false;
                        {ok, #document{}} ->
                            true;
                        _ ->
                            false
                    end
            end
        end, false, Emails).

%%--------------------------------------------------------------------
%% @private
%% @doc Merges user account with information gathered from new connected account.
%% @end
%%--------------------------------------------------------------------
-spec merge_connected_accounts(OAuthAccount :: #oauth_account{}, UserInfo :: #onedata_user{}) -> [tuple()].
merge_connected_accounts(OAuthAccount, UserInfo) ->
    #onedata_user{name = Name, email_list = Emails, connected_accounts = ConnectedAccounts} = UserInfo,
    #oauth_account{name = ProvName, email_list = ConnAccEmails} = OAuthAccount,
    % If no name is specified, take the one provided with new info
    NewName = case Name of
                  <<"">> -> ProvName;
                  _ -> Name
              end,
    % Add emails from provider that are not yet added to account
    NewEmails = lists:foldl(
        fun(Email, Acc) ->
            case lists:member(Email, Acc) of
                true -> Acc;
                false -> Acc ++ [Email]
            end
        end, Emails, ConnAccEmails),
    [
        {name, NewName},
        {email_list, NewEmails},
        {connected_accounts, ConnectedAccounts ++ [OAuthAccount]}
    ].