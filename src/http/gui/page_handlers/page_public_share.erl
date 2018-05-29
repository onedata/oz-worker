%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when zone version page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_public_share).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("registered_names.hrl").
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
-spec handle(new_gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"GET">>, Req) ->
    ShareId = cowboy_req:binding(share_id, Req),
    try
        URL = share_logic:share_id_to_redirect_url(ShareId),
        cowboy_req:reply(307, #{<<"location">> => URL}, Req)
    catch Type:Reason ->
        ?error_stacktrace("Error while redirecting to public share - ~p:~p",
            [Type, Reason]),
        cowboy_req:reply(404, Req)
    end.