%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This file contains n2o website code.
%%% The page handles developer login (on any user, bypassing OpenID).
%%% This page will be available only when dev_mode is set to true.
%%% @end
%%%-------------------------------------------------------------------

-module(page_validate_dev_login).

-include_lib("ctool/include/logging.hrl").
-include("gui/common.hrl").

% n2o API
-export([main/0, event/1]).

%% Template points to the template file, which will be filled with content
main() ->
    #dtl{file = "bare", app = ?APP_Name, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.

%% Page title
title() -> <<"Validate dev login">>.

body() ->
    ParamsProplist = gui_ctx:get_request_params(),
    UserID = proplists:get_value(<<"user">>, ParamsProplist),
    gui_ctx:create_session(),
    gui_ctx:set_user_id(UserID),
    gui_jq:redirect(<<"/">>),
    <<"">>.

event(init) -> ok;
event(terminate) -> ok.