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
-module(user_rest_translator).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([response/4]).

response(create, _UserId, authorize, {ok, DischargeMacaroon}) ->
    n_rest_handler:ok_body_reply(DischargeMacaroon);

response(create, _UserId, client_tokens, {ok, Token}) ->
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});

response(create, _UserId, default_space, ok) ->
    n_rest_handler:ok_no_content_reply();

response(create, _UserId, space_alias, ok) ->
    n_rest_handler:ok_no_content_reply();

response(create, _UserId, default_provider, ok) ->
    n_rest_handler:ok_no_content_reply();

response(create, _UserId, join_group, {ok, GroupId}) ->
    n_rest_handler:created_reply([<<"user/groups/">>, GroupId]);

response(create, _UserId, join_space, {ok, SpaceId}) ->
    n_rest_handler:created_reply([<<"user/spaces/">>, SpaceId]);


response(get, UserId, data, {ok, UserData}) ->
    n_rest_handler:ok_body_reply(UserData#{<<"userId">> => UserId});

response(get, undefined, list, {ok, Users}) ->
    n_rest_handler:ok_body_reply(#{<<"users">> => Users});

response(get, _UserId, oz_privileges, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _UserId, eff_oz_privileges, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _UserId, default_space, {ok, DefaultSpace}) ->
    n_rest_handler:ok_body_reply(#{<<"spaceId">> => DefaultSpace});

response(get, _UserId, {space_alias, _SpaceId}, {ok, SpaceAlias}) ->
    n_rest_handler:ok_body_reply(#{<<"alias">> => SpaceAlias});

response(get, _UserId, default_provider, {ok, DefaultProvider}) ->
    n_rest_handler:ok_body_reply(#{<<"providerId">> => DefaultProvider});

response(get, _UserId, client_tokens, {ok, Tokens}) ->
    n_rest_handler:ok_body_reply(#{<<"tokens">> => Tokens});

response(get, _UserId, groups, {ok, Groups}) ->
    n_rest_handler:ok_body_reply(#{<<"groups">> => Groups});

response(get, _UserId, eff_groups, {ok, Groups}) ->
    n_rest_handler:ok_body_reply(#{<<"groups">> => Groups});

response(get, _UserId, {group, GroupId}, {ok, Group}) ->
    group_rest_translator:response(get, GroupId, entity, {ok, Group});

response(get, _UserId, {eff_group, GroupId}, {ok, Group}) ->
    group_rest_translator:response(get, GroupId, entity, {ok, Group});

response(get, _UserId, spaces, {ok, Spaces}) ->
    n_rest_handler:ok_body_reply(#{<<"spaces">> => Spaces});

response(get, _UserId, eff_spaces, {ok, Spaces}) ->
    n_rest_handler:ok_body_reply(#{<<"spaces">> => Spaces});

response(get, _UserId, {space, SpaceId}, {ok, Space}) ->
    space_rest_translator:response(get, SpaceId, entity, {ok, Space});

response(get, _UserId, {eff_space, SpaceId}, {ok, Space}) ->
    space_rest_translator:response(get, SpaceId, entity, {ok, Space});

response(get, _UserId, eff_providers, {ok, Providers}) ->
    n_rest_handler:ok_body_reply(#{<<"providers">> => Providers});

response(get, _UserId, {eff_provider, ProviderId}, {ok, Provider}) ->
    provider_rest_translator:response(get, ProviderId, entity, {ok, Provider});




response(update, _UserId, _, ok) ->
    n_rest_handler:updated_reply();


response(delete, _UserId, _, ok) ->
    n_rest_handler:deleted_reply().
