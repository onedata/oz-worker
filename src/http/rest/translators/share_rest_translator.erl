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
    {entity_logic:gri(), entity_logic:auth_hint(), term()}) -> rest_handler:rest_resp().
create_response(#gri{id = undefined, aspect = instance}, _, resource, {#gri{id = ShareId}, _}) ->
    rest_translator:created_reply_with_location([<<"shares">>, ShareId]).


%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> rest_handler:rest_resp().
get_response(#gri{id = undefined, aspect = list, scope = private}, Shares) ->
    rest_translator:ok_body_reply(#{<<"shares">> => Shares});

get_response(#gri{id = ShareId, aspect = instance, scope = private}, Share) ->
    #od_share{
        space = SpaceId,
        name = Name,
        description = Description,
        root_file = RootFileId,
        file_type = FileType,
        handle = HandleId,
        creator = Creator, creation_time = CreationTime
    } = Share,
    rest_translator:ok_body_reply(#{
        <<"shareId">> => ShareId,
        <<"spaceId">> => SpaceId,
        <<"name">> => Name,
        <<"description">> => Description,
        <<"publicUrl">> => share_logic:build_public_url(ShareId),
        <<"publicRestUrl">> => share_logic:build_public_rest_url(ShareId),
        <<"rootFileId">> => element(2, {ok, _} = file_id:guid_to_objectid(RootFileId)),
        <<"fileType">> => FileType,
        <<"handleId">> => utils:undefined_to_null(HandleId),
        <<"creator">> => aai:subject_to_json(utils:ensure_defined(Creator, undefined, ?SUB(nobody))),
        <<"creationTime">> => CreationTime
    });

get_response(#gri{id = ShareId, aspect = instance, scope = public}, ShareData) ->
    #{
        <<"name">> := Name,
        <<"description">> := Description,
        <<"handleId">> := HandleId,
        <<"rootFileId">> := RootFileId,
        <<"fileType">> := FileType,
        <<"creationTime">> := CreationTime
    } = ShareData,
    rest_translator:ok_body_reply(#{
        <<"shareId">> => ShareId,
        <<"name">> => Name,
        <<"description">> => Description,
        <<"publicUrl">> => share_logic:build_public_url(ShareId),
        <<"publicRestUrl">> => share_logic:build_public_rest_url(ShareId),
        <<"rootFileId">> => element(2, {ok, _} = file_id:guid_to_objectid(RootFileId)),
        <<"fileType">> => FileType,
        <<"handleId">> => utils:undefined_to_null(HandleId),
        <<"creationTime">> => CreationTime
    }).
