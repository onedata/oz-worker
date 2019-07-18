%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% onezone configuration into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(zone_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Wojciech Geisler").

-include("http/rest.hrl").
-include_lib("ctool/include/api_errors.hrl").

-export([create_response/4, get_response/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback create_response/4.
%% @end
%%--------------------------------------------------------------------
-spec create_response(entity_logic:gri(), entity_logic:auth_hint(),
    entity_logic:data_format(), Result :: term() | {entity_logic:gri(), term()} |
    {entity_logic:gri(), entity_logic:auth_hint(), term()}) -> no_return().
create_response(_GRI, _, _Format, _Result) ->
    throw(?ERROR_NOT_SUPPORTED).


%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{id = undefined, aspect = configuration}, Configuration) ->
    rest_translator:ok_body_reply(Configuration);

get_response(#gri{id = undefined, aspect = test_image}, TestImage) ->
    rest_translator:ok_body_reply(TestImage).
