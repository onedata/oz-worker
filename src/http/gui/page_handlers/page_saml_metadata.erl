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
    QsVals = cowboy_req:parse_qs(Req),
    Test = proplists:get_value(<<"test">>, QsVals, <<"false">>),
    Test =:= <<"true">> andalso auth_test_mode:process_enable_test_mode(),
    case auth_config:get_saml_sp_config() of
        {error, saml_disabled} ->
            cowboy_req:reply(404, Req);
        SpConfig ->
            SignedXml = esaml_sp:generate_metadata(SpConfig),
            Metadata = xmerl:export([SignedXml], xmerl_xml),
            cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/xml">>},
                Metadata,
                Req
            )
    end.
