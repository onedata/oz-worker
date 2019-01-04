%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when configuration page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_configuration).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("registered_names.hrl").
-include("entity_logic.hrl").

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
    {ok, Config} = entity_logic:handle(#el_req{
        operation = get,
        client = ?NOBODY,
        gri = #gri{type = oz_worker, id = undefined, aspect = configuration}
    }),

    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        json_utils:encode(Config),
        Req
    ).
