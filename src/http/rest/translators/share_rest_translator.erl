%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% share entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(share_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("datastore/oz_datastore_models.hrl").

-export([create_response/3, get_response/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic CREATE result into REST response
%% expressed by #rest_resp{} record. GRI holds the #gri{} od the request,
%% new GRI holds the #gri{} of new aspect that was created.
%% @end
%%--------------------------------------------------------------------
-spec create_response(entity_logic:gri(), entity_logic:auth_hint(),
    Result :: {data, term()} | {fetched, entity_logic:gri(), term()} |
    {not_fetched, entity_logic:gri()} |
    {not_fetched, entity_logic:gri(), entity_logic:auth_hint()}) -> #rest_resp{}.
create_response(#gri{id = undefined, aspect = instance}, _, {fetched, #gri{id = ShareId}, _}) ->
    rest_translator:created_reply([<<"shares">>, ShareId]).


%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic GET result into REST response
%% expressed by #rest_resp{} record.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Result :: term()) ->
    #rest_resp{}.
get_response(#gri{id = ShareId, aspect = instance, scope = private}, Share) ->
    #od_share{
        name = Name, public_url = PublicUrl, space = SpaceId,
        root_file = RootFileId, handle = HandleId
    } = Share,
    rest_translator:ok_body_reply(#{
        <<"shareId">> => ShareId, <<"name">> => Name,
        <<"publicUrl">> => PublicUrl, <<"spaceId">> => SpaceId,
        <<"rootFileId">> => RootFileId, <<"handleId">> => HandleId
    });
get_response(#gri{id = undefined, aspect = list, scope = private}, Shares) ->
    rest_translator:ok_body_reply(#{<<"shares">> => Shares}).
