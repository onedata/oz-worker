%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements page_backend_behaviour and is called
%%% when /do_login page is visited. It is used to verify the data
%%% returned by auth providers and log the user in.
%%% @end
%%%-------------------------------------------------------------------
-module(basic_login_backend).
-author("Lukasz Opiola").
-behaviour(page_backend_behaviour).

-include("gui/common.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([page_init/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link page_backend_behaviour} callback page_init/0.
%% @end
%%--------------------------------------------------------------------
-spec page_init() -> gui_html_handler:page_init_result().
page_init() ->
    Req = g_ctx:get_cowboy_req(),
    try
        {<<"Basic ", UserAndPassword/binary>>, _} =
            cowboy_req:header(<<"authorization">>, Req),
        [User, Passwd] = binary:split(base64:decode(UserAndPassword), <<":">>),
        {ok, UserId} =
            user_logic:authenticate_by_basic_credentials(User, Passwd),
        g_session:log_in(UserId),
        {reply, 200}

    catch T:M ->
        ?error_stacktrace("Login by credentials failed - ~p:~p", [T, M]),
        {reply, 401}
    end.
