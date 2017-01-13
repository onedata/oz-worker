%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of system errors into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(space_rest_translator).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([response/4]).

% TODO VFS-2918
response(create, _SpaceId, {deprecated_user_privileges, _UserId}, ok) ->
    n_rest_handler:ok_no_content_reply();
% TODO VFS-2918
response(create, _SpaceId, {deprecated_child_privileges, _GroupId}, ok) ->
    n_rest_handler:ok_no_content_reply();

response(create, undefined, entity, {ok, SpaceId}) ->
    n_rest_handler:created_reply([<<"spaces/">>, SpaceId]);

response(create, _SpaceId, invite_user_token, {ok, Token}) ->
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});

response(create, _SpaceId, invite_group_token, {ok, Token}) ->
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});

response(create, _SpaceId, invite_provider_token, {ok, Token}) ->
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});




response(get, SpaceId, data, {ok, SpaceData}) ->
    n_rest_handler:ok_body_reply(SpaceData#{
        <<"spaceId">> => SpaceId,
        % TODO Deprecated, VFS-2918
        <<"canonicalName">> => maps:get(<<"name">>, SpaceData)
    }).
