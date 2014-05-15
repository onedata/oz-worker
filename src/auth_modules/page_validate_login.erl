%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code
%% @end
%% ===================================================================

-module(page_validate_login).
-compile(export_all).
-include("gui_common.hrl").
-include("logging.hrl").

%% Template points to the template file, which will be filled with content
main() -> #dtl{file = "bare", app = veil_cluster_node, bindings = [{title, title()}, {body, body()}]}.

%% Page title
title() -> <<"Validate login">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
    case auth_utils:validate_login() of
        {redirect, URL} ->
            wf:redirect(URL);
        new_user ->
            #panel{style = <<"position: relative;">>, body = [
                #panel{class = <<"alert alert-success login-page">>, body = [
                    #h3{body = <<"First login">>},
                    #p{class = <<"login-info">>, body = <<"You have successfully logged in and an account for you has been created. ",
                    "You will be now redirected to profile management page, where you can fill out some information about yourself ",
                    "and connect another accounts to your profile.">>},
                    #button{class = <<"btn btn-primary btn-block">>, postback = to_manage_account, body = <<"Proceed">>}
                ]}
            ] ++ gui_utils:logotype_footer(120)};
        {error, ErrorID} ->
            page_error:redirect_with_error(ErrorID)
    end.

event(init) -> ok;

event(to_manage_account) -> wf:redirect(<<"/manage_account">>).