%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when SAML certificate page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_saml_cert).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").

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
    case auth_config:get_saml_cert_pem() of
        {error, saml_disabled} ->
            cowboy_req:reply(?HTTP_404_NOT_FOUND, Req);
        {ok, Cert} ->
            cowboy_req:reply(?HTTP_200_OK, #{
                ?HDR_CONTENT_TYPE => <<"application/x-pem-file">>
            }, Cert, Req)
    end.
