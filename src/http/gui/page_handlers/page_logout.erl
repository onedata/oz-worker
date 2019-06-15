%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when logout page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_logout).
-author("Lukasz Opiola").
-behaviour(dynamic_page_behaviour).

-include("http/codes.hrl").

%% API
-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"POST">>, Req) ->
    cowboy_req:reply(?HTTP_204_NO_CONTENT, gui_session:log_out(Req)).
