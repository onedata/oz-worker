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

-include("http/rest.hrl").
-include("datastore/oz_datastore_models.hrl").

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
    {entity_logic:gri(), entity_logic:auth_hint(), term()}) -> #rest_resp{}.
create_response(#gri{id = undefined, aspect = instance}, _, resource, {#gri{id = ShareId}, _}) ->
    rest_translator:created_reply([<<"shares">>, ShareId]).


%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{id = ShareId, aspect = instance, scope = private}, Share) ->
    #od_share{
        name = Name, public_url = PublicUrl, space = SpaceId,
        root_file = RootFileId, handle = HandleId
    } = Share,
    rest_translator:ok_body_reply(#{
        <<"shareId">> => ShareId, <<"name">> => Name,
        <<"publicUrl">> => PublicUrl, <<"spaceId">> => SpaceId,
        <<"rootFileId">> => element(2, {ok, _} = file_id:guid_to_objectid(RootFileId)),
        <<"handleId">> => gs_protocol:undefined_to_null(HandleId)
    });
get_response(#gri{id = undefined, aspect = list, scope = private}, Shares) ->
    rest_translator:ok_body_reply(#{<<"shares">> => Shares}).
