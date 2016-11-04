%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements page_backend_behaviour and is called
%%% when logout page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(logout_backend).
-author("Lukasz Opiola").
-behaviour(page_backend_behaviour).

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
    gui_session:log_out(),
    {redirect_relative, <<"/">>}.
