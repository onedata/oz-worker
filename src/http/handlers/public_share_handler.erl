%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles requests for public shares and redirects a user
%%% to a random provider that supports the parent space of the share.
%%% @end
%%%-------------------------------------------------------------------
-module(public_share_handler).
-author("Lukasz Opiola").

-behaviour(cowboy_handler).

-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Cowboy handler callback.
%% Handles a request returning a HTTP Redirect (307 - Moved temporarily).
%% @end
%%--------------------------------------------------------------------
-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(#{method := <<"GET">>} = Req, State) ->
    ShareId = cowboy_req:binding(share_id, Req),
    NewReq = try
        URL = share_logic:share_id_to_redirect_url(ShareId),
        cowboy_req:reply(307, #{
            <<"location">> => URL,
            <<"content-type">> => <<"text/html">>
        }, Req)
    catch T:M ->
        ?debug_stacktrace("Error while redirecting to public share - ~p:~p",
            [T, M]),
        cowboy_req:reply(404, Req)
    end,
    {ok, NewReq, State};
init(Req, State) ->
    NewReq = cowboy_req:reply(405, #{<<"allow">> => <<"GET">>}, Req),
    {ok, NewReq, State}.
