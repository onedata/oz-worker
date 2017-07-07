%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements page_backend_behaviour and is called
%%% when /saml/sp.xml page is visited, returning SP metadata.
%%% @end
%%%-------------------------------------------------------------------
-module(saml_metadata_backend).
-author("Lukasz Opiola").
-behaviour(page_backend_behaviour).

-include("gui/common.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([page_init/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link page_backend_behaviour} callback page_init/|0.
%% @end
%%--------------------------------------------------------------------
-spec page_init() -> gui_html_handler:page_init_result().
page_init() ->
    SignedXml = esaml_sp:generate_metadata(saml_config:get_sp_config()),
    Metadata = xmerl:export([SignedXml], xmerl_xml),
    {reply, 200, #{<<"Content-Type">> => <<"text/xml">>}, Metadata}.
