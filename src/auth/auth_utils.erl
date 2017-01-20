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
-include("auth_common.hrl").
-include("gui/common.hrl").

%% API
% Convenience functions
-export([local_auth_endpoint/0, get_value_binary/2, extract_emails/1]).

% Authentication flow handling
-export([validate_login/0]).

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
%% #oauth_account{} record.
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
    case get_value_binary(<<"email">>, JSONProplist) of
        <<"">> -> [];
        Email -> [Email]
    end.


%%--------------------------------------------------------------------
%% @doc Function used to validate login by processing a redirect that came from
%% an OAuth provider. Must be called from gui context to work. Returns one of the following:
%% 1. atom 'new_user' if the login has been verified and a new user has been created
%% 2. {redirect, URL} if the account already exists, to state where the user should be redirected now
%% 3. {error, Description} if the validation failed or any other error occurred.
%% @end
%%--------------------------------------------------------------------
-spec validate_login() -> new_user | {redirect, URL :: binary()} | {error, term()}.
validate_login() ->
    try
        % Check url params for state parameter
        ParamsProplist = gui_ctx:get_url_params(),
        State = proplists:get_value(<<"state">>, ParamsProplist),
        StateInfo = auth_logic:lookup_state_token(State),
        case StateInfo of
            error ->
                % This state token was not generated by us, invalid
                ?alert("Security breach attempt spotted - invalid state. GET params:~n~p~nPOST params:~n~p",
                    [ParamsProplist, gui_ctx:get_form_params()]),
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
                            [Reason, ParamsProplist, gui_ctx:get_form_params()]),
                        {error, ?error_auth_invalid_request};

                    {ok, OriginalOAuthAccount = #oauth_account{provider_id = ProviderId, user_id = OauthUserId, email_list = OriginalEmails, name = Name}} ->
                        Emails = lists:map(fun(Email) ->
                            http_utils:normalize_email(Email)
                        end, OriginalEmails),
                        case Emails of
                            [] ->
                                throw(no_email_received_from_openid);
                            _ ->
                                ok
                        end,
                        OAuthAccount = OriginalOAuthAccount#oauth_account{email_list = Emails, provider_id = ProviderId},
                        case proplists:get_value(connect_account, Props) of
                            false ->
                                % Standard login, check if there is an account belonging to the user
                                case od_user:get({connected_account_user_id, {ProviderId, OauthUserId}}) of
                                    {ok, #document{key = UserId}} ->
                                        % The account already exists
                                        gui_session:log_in(UserId),
                                        {redirect, Redirect};
                                    _ ->
                                        % Error -> this is a first login
                                        % Check if any of emails is in use
                                        case is_any_email_in_use(<<"">>, Emails) of
                                            true ->
                                                % At least one email is in database, cannot create account
                                                {error, ?error_auth_new_email_occupied};
                                            false ->
                                                % All emails are available, proceed
                                                % check if name was returned from openid, if not set
                                                % email as name (the part before @)
                                                NameToSet = case Name of
                                                    <<"">> ->
                                                        hd(binary:split(hd(Emails), <<"@">>));
                                                    _ ->
                                                        Name
                                                end,
                                                UserInfo = #od_user{
                                                    email_list = Emails,
                                                    name = NameToSet,
                                                    connected_accounts = [
                                                        OAuthAccount
                                                    ]
                                                },
                                                {ok, UserId} = n_user_logic:create(UserInfo),
                                                gui_session:log_in(UserId),
                                                gui_session:put_value(firstLogin, true),
                                                new_user
                                        end
                                end;

                            true ->
                                % Account adding flow
                                % Check if this account isn't connected to other profile
                                case od_user:get({connected_account_user_id, {ProviderId, OauthUserId}}) of
                                    {ok, _} ->
                                        % The account is used on some other profile, cannot proceed
                                        {error, ?error_auth_account_already_connected};
                                    _ ->
                                        % Not found, ok
                                        % Check if any of emails is in use by another user
                                        case is_any_email_in_use(gui_session:get_user_id(), Emails) of
                                            true ->
                                                % At least one email is in database, cannot connect account
                                                {error, ?error_auth_connect_email_occupied};
                                            false ->
                                                % Everything ok, get the user and add new provider info
                                                UserId = gui_session:get_user_id(),
                                                ok = n_user_logic:add_oauth_account(UserId, OAuthAccount),
                                                {redirect, <<?PAGE_AFTER_LOGIN, "?expand_accounts=true">>}
                                        end
                                end
                        end
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
%% @doc
%% Returns true if any of the given emails is occupied
%% (but not by the specified user).
%% @end
%%--------------------------------------------------------------------
-spec is_any_email_in_use(UserId :: od_user:id(), Emails :: [binary()]) ->
    boolean().
is_any_email_in_use(UserId, Emails) ->
    lists:any(fun(Email) ->
        n_user_logic:is_email_occupied(UserId, Email)
    end, Emails).
