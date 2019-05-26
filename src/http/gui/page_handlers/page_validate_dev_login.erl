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
    case oz_worker:get_env(dev_mode, false) of
        false ->
            cowboy_req:reply(404, Req);
        true ->
            QueryParams = cowboy_req:parse_qs(Req),
            UserId = proplists:get_value(<<"user">>, QueryParams),
            NewReq = case oz_gui_session:get_user_id(Req) of
                {ok, UserId} ->
                    Req;
                {ok, _AnotherUser} ->
                    Req2 = oz_gui_session:log_out(Req),
                    oz_gui_session:log_in(UserId, Req2);
                _ ->
                    case UserId of
                        undefined ->
                            Req;
                        _ ->
                            oz_gui_session:log_in(UserId, Req)
                    end

            end,
            ?info("[DEV MODE] User ~p logged in", [UserId]),
            cowboy_req:reply(307, #{<<"location">> => <<"/">>}, NewReq)
    end.