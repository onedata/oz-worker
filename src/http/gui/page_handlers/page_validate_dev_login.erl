%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when validate developer login page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_validate_dev_login).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("http/codes.hrl").
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
    case oz_worker:get_env(dev_mode, false) of
        false ->
            cowboy_req:reply(?HTTP_404_NOT_FOUND, Req);
        true ->
            QueryParams = cowboy_req:parse_qs(Req),
            UserId = proplists:get_value(<<"user">>, QueryParams),
            NewReq = case gui_session:validate(Req) of
                {ok, UserId, _SessionId, Req2} ->
                    Req2;
                {ok, _AnotherUser, _SessionId, Req2} ->
                    Req3 = gui_session:log_out(Req2),
                    ?info("[DEV MODE] User ~p logged in", [UserId]),
                    gui_session:log_in(UserId, Req3);
                {error, _} ->
                    case UserId of
                        undefined ->
                            Req;
                        _ ->
                            ?info("[DEV MODE] User ~p logged in", [UserId]),
                            gui_session:log_in(UserId, Req)
                    end
            end,
            cowboy_req:reply(?HTTP_303_SEE_OTHER, #{<<"location">> => <<"/">>}, NewReq)
    end.