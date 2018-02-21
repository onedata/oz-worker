%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles requests for current version of Onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(zone_version_handler).
-author("Bartosz Walkowicz").

-behaviour(cowboy_handler).

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-export([init/2]).


%%--------------------------------------------------------------------
%% @doc Cowboy handler callback.
%% Handles a request returning current version of Onezone.
%% @end
%%--------------------------------------------------------------------
-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(#{method := <<"GET">>} = Req, State) ->
    {_AppId, _AppName, AppVersion} = lists:keyfind(
        ?APP_NAME, 1, application:loaded_applications()
    ),
    NewReq = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>}, AppVersion, Req
    ),
    {ok, NewReq, State};
init(Req, State) ->
    NewReq = cowboy_req:reply(405, #{<<"allow">> => <<"GET">>}, Req),
    {ok, NewReq, State}.
