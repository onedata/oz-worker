%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when the root page ("/") is visited - redirects to Onezone GUI index.
%%% @end
%%%-------------------------------------------------------------------
-module(page_redirect_to_oz_worker).
-author("Lukasz Opiola").
-behaviour(dynamic_page_behaviour).

-include_lib("ctool/include/http/codes.hrl").

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
handle(<<"GET">>, Req) ->
    cowboy_req:reply(?HTTP_302_FOUND, #{
        <<"location">> => gui_static:oz_worker_gui_path(<<"/i">>),
        <<"cache-control">> => <<"max-age=3600">>
    }, Req).
