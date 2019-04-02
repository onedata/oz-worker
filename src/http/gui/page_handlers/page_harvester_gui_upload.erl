%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called to handle
%%% GUI package uploads of harvester plugin.
%%% @end
%%%-------------------------------------------------------------------
-module(page_harvester_gui_upload).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("http/rest.hrl").
-include_lib("ctool/include/logging.hrl").

%% Cowboy API
-export([handle/2]).

%% ====================================================================
%% Cowboy API functions
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"POST">>, Req) ->
    try
        gui_upload:handle_harvester_gui_upload(Req)
    catch
        throw:Code when is_integer(Code) ->
            cowboy_req:reply(Code, Req);
        Type:Reason ->
            ?error_stacktrace("Error while processing GUI upload - ~p:~p", [Type, Reason]),
            cowboy_req:reply(?HTTP_500_INTERNAL_SERVER_ERROR, Req)
    end.
