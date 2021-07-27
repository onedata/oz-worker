%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when public share path is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_public_share).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("http/gui_paths.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/logging.hrl").

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
handle(<<"GET">>, Req) ->
    ShareId = cowboy_req:binding(?SHARE_ID_BINDING, Req),
    try
        Uri = oz_worker:get_uri(gui_static:oz_worker_gui_path(?PUBLIC_SHARE_GUI_PATH(ShareId))),
        cowboy_req:reply(?HTTP_303_SEE_OTHER, #{?HDR_LOCATION => Uri}, Req)
    catch Type:Reason:Stacktrace ->
        ?debug_stacktrace("Error while redirecting to public share - ~p:~p", [Type, Reason], Stacktrace),
        cowboy_req:reply(?HTTP_400_BAD_REQUEST, Req)
    end.
