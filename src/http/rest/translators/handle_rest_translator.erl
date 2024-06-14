%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% handle entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Lukasz Opiola").

-include("http/rest.hrl").

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
create_response(#gri{id = undefined, aspect = instance}, AuthHint, resource, {#gri{id = HandleId}, _}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            [<<"user">>, <<"handles">>, HandleId];
        ?AS_GROUP(GroupId) ->
            [<<"groups">>, GroupId, <<"handles">>, HandleId];
        _ ->
            [<<"handles">>, HandleId]
    end,
    rest_translator:created_reply_with_location(LocationTokens);

create_response(#gri{id = HandleId, aspect = {user, UserId}}, _, resource, _) ->
    rest_translator:created_reply_with_location(
        [<<"handles">>, HandleId, <<"users">>, UserId]
    );

create_response(#gri{id = HandleId, aspect = {group, GroupId}}, _, resource, _) ->
    rest_translator:created_reply_with_location(
        [<<"handles">>, HandleId, <<"groups">>, GroupId]
    ).

%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> rest_handler:rest_resp().
get_response(#gri{id = undefined, aspect = list}, Handles) ->
    rest_translator:ok_body_reply(#{<<"handles">> => Handles});

get_response(#gri{id = undefined, aspect = privileges}, Privileges) ->
    rest_translator:ok_body_reply(Privileges);

get_response(#gri{id = HandleId, aspect = instance, scope = protected}, HandleData) ->
    #{
        <<"handleServiceId">> := HandleService,
        <<"publicHandle">> := PublicHandle,
        <<"resourceType">> := ResourceType,
        <<"resourceId">> := ResourceId,
        <<"metadataPrefix">> := MetadataPrefix,
        <<"metadata">> := Metadata,
        <<"timestamp">> := Timestamp,
        <<"creator">> := Creator,
        <<"creationTime">> := CreationTime
    } = HandleData,
    rest_translator:ok_body_reply(#{
        <<"handleId">> => HandleId,
        <<"publicHandle">> => PublicHandle,
        <<"handleServiceId">> => HandleService,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadataPrefix">> => MetadataPrefix,
        <<"metadata">> => Metadata,
        <<"timestamp">> => time:seconds_to_iso8601(Timestamp),
        <<"creator">> => aai:subject_to_json(utils:ensure_defined(Creator, undefined, ?SUB(nobody))),
        <<"creationTime">> => CreationTime
    });

get_response(#gri{id = HandleId, aspect = instance, scope = public}, HandleData) ->
    #{
        <<"publicHandle">> := PublicHandle,
        <<"resourceType">> := ResourceType,
        <<"resourceId">> := ResourceId,
        <<"metadataPrefix">> := MetadataPrefix,
        <<"metadata">> := Metadata,
        <<"timestamp">> := Timestamp,
        <<"creationTime">> := CreationTime
    } = HandleData,
    rest_translator:ok_body_reply(#{
        <<"handleId">> => HandleId,
        <<"publicHandle">> => PublicHandle,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadataPrefix">> => MetadataPrefix,
        <<"metadata">> => Metadata,
        <<"timestamp">> => time:seconds_to_iso8601(Timestamp),
        <<"creationTime">> => CreationTime
    });

get_response(#gri{aspect = users}, Users) ->
    rest_translator:ok_body_reply(#{<<"users">> => Users});

get_response(#gri{aspect = eff_users}, Users) ->
    rest_translator:ok_body_reply(#{<<"users">> => Users});

get_response(#gri{aspect = {user_privileges, _UserId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_user_privileges, _UserId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = groups}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});

get_response(#gri{aspect = eff_groups}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});

get_response(#gri{aspect = {group_privileges, _GroupId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_group_privileges, _GroupId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges}).