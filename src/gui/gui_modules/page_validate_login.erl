%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code.
%% The page handles user validation via OpenID or OAuth.
%% @end
%% ===================================================================

-module(page_validate_login).

-include_lib("ctool/include/logging.hrl").
-include("gui/common.hrl").

% n2o API
-export([main/0, event/1]).

%% Template points to the template file, which will be filled with content
main() -> #dtl{file = "bare", app = ?APP_Name, bindings = [{title, title()}, {body, body()}]}.

%% Page title
title() -> <<"Validate login">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
    case auth_utils:validate_login() of
        {redirect, URL} ->
            ?debug("User ~p logged in", [gui_ctx:get_user_id()]),
            gui_jq:redirect(URL);
        new_user ->
            ?debug("User ~p logged in for the first time", [gui_ctx:get_user_id()]),
            #panel{style = <<"position: relative;">>, body = [
                #panel{class = <<"alert alert-success login-page">>, body = [
                    #h3{body = <<"First login">>},
                    #p{class = <<"login-info">>, body = <<"You have successfully logged in and an account for you has been created. ",
                    "You will be now redirected to profile management page, where you can fill out some information about yourself ",
                    "and connect another accounts to your profile.">>},
                    #button{class = <<"btn btn-primary btn-block">>, postback = to_manage_account, body = <<"Proceed">>}
                ]}
            ] ++ gr_gui_utils:logotype_footer(120)};
        {error, ErrorID} ->
            page_error:redirect_with_error(ErrorID)
    end.

event(init) -> ok;

event(to_manage_account) -> wf:redirect(<<"/manage_account">>).