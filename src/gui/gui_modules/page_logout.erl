%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This file contains n2o website code.
%%% The page handles users' logging out.
%%% @end
%%%-------------------------------------------------------------------

-module(page_logout).

-include_lib("ctool/include/logging.hrl").
-include("gui/common.hrl").

% n2o API
-export([main/0, event/1]).

%% Template points to the template file, which will be filled with content
main() ->
    #dtl{file = "bare", app = ?APP_Name, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.

%% Page title
title() -> <<"Logout page">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
    ?debug("User ~p logged out", [gui_ctx:get_user_id()]),
    gui_session_handler:clear(),
    LogoutEndpoint = proplists:get_value(logout_endpoint, auth_config:get_auth_config(plgrid)),
    #panel{style = <<"position: relative;">>, body =
    [
        #panel{class = <<"alert alert-success login-page">>, body = [
            #h3{class = <<"">>, body = <<"Logout successful">>},
            #p{class = <<"login-info">>, body = <<"Come back soon.">>},
            #link{url = <<"/">>, class = <<"btn btn-primary btn-block">>, body = <<"Login page">>}
        ]},
        gui_utils:cookie_policy_popup_body(<<?privacy_policy_url>>)
    ] ++ [#p{body = <<"<iframe src=\"", LogoutEndpoint/binary, "\" style=\"display:none\"></iframe>">>}]
    }.


event(init) -> ok;
event(terminate) -> ok.