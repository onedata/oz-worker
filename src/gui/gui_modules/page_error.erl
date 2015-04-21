%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This file contains n2o website code.
%%% The page is displayed whenever an error occurs.
%%% @end
%%%-------------------------------------------------------------------

-module(page_error).

-include("gui/common.hrl").
-include("auth_common.hrl").

% n2o API
-export([main/0, event/1]).
% Functions used externally
-export([redirect_with_error/1]).

%% Template points to the template file, which will be filled with content
main() ->
    #dtl{file = "bare", app = ?APP_Name, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.

%% Page title
title() -> <<"Error">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
    {Reason, Description} = get_reason_and_description(),
    #panel{style = <<"position: relative;">>, body = [
        #panel{class = <<"alert alert-danger login-page">>, body = [
            #h3{body = <<"Error">>},
            #p{class = <<"login-info">>, style = <<"font-weight: bold;">>, body = Reason},
            #p{class = <<"login-info">>, body = Description},
            #link{url = <<"/">>, class = <<"btn btn-warning btn-block">>, body = <<"Main page">>}
        ]},
        gui_utils:cookie_policy_popup_body(<<?privacy_policy_url>>)
    ]}.


event(init) -> ok;
event(terminate) -> ok.

% This function causes a HTTP redirect to error page, which displays an error message.
redirect_with_error(ErrorID) ->
    gui_jq:redirect(<<"/error?id=", (gui_str:to_binary(ErrorID))/binary>>).


get_reason_and_description() ->
    id_to_reason_and_message(gui_ctx:url_param(<<"id">>)).


id_to_reason_and_message(<<?error_internal_server_error>>) ->
    {<<"500 - Internal server error">>, <<"Server encountered an unexpected error. Please contact the site administrator if the problem persists.">>};

id_to_reason_and_message(<<?error_auth_invalid_request>>) ->
    {<<"Invalid request">>, <<"This OpenID or OAuth request could not be validated.">>};

id_to_reason_and_message(<<?error_auth_new_email_occupied>>) ->
    {<<"Cannot create new account">>, <<"One or more e-mail addresses returned by the OpenID / OAuth provider are occupied.">>};

id_to_reason_and_message(<<?error_auth_connect_email_occupied>>) ->
    {<<"Cannot connect chosen account to the profile">>, <<"One or more e-mail addresses returned by the OpenID / OAuth provider are occupied.">>};

id_to_reason_and_message(<<?error_auth_account_already_connected>>) ->
    {<<"Cannot connect chosen account to the profile">>, <<"This account has already been connected with other profile.">>};

id_to_reason_and_message(_) ->
    {<<"Unknown">>, <<"No description">>}.