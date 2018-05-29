%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when SAML metadata page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_saml_metadata).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("registered_names.hrl").

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
    SignedXml = esaml_sp:generate_metadata(saml_config:get_sp_config()),
    Metadata = xmerl:export([SignedXml], xmerl_xml),
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/xml">>},
        Metadata,
        Req
    ).
