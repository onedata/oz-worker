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
-module(provider_rest_translator).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([response/4]).


response(create, undefined, entity, {ok, {ProvId, Certificate}}) ->
    #rest_resp{code = ?HTTP_200_OK, body = #{
        <<"providerId">> => ProvId,
        <<"certificate">> => Certificate
    }};
response(create, undefined, entity_dev, {ok, {ProvId, Certificate}}) ->
    #rest_resp{code = ?HTTP_200_OK, body = #{
        <<"providerId">> => ProvId,
        <<"certificate">> => Certificate
    }};
response(create, _ProvId, support, {ok, SpaceId}) ->
    rest_translator:created_reply([<<"provider/spaces/">>, SpaceId]);
response(create, _ProvId, check_my_ports, {ok, Body}) ->
    #rest_resp{code = ?HTTP_200_OK, body = Body};

response(get, undefined, list, {ok, ProviderIds}) ->
    #rest_resp{code = ?HTTP_200_OK, body = #{<<"providers">> => ProviderIds}};
response(get, EntityId, entity, {ok, Provider}) ->
    #od_provider{
        name = ClientName,
        urls = URLs,
        redirection_point = RedirectionPoint,
        latitude = Latitude,
        longitude = Longitude
    } = Provider,
    #rest_resp{code = ?HTTP_200_OK, body = #{
        <<"providerId">> => EntityId,
        <<"name">> => ClientName,
        <<"urls">> => URLs,
        <<"redirectionPoint">> => RedirectionPoint,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude
    }};
response(get, _ProvId, eff_users, {ok, UserIds}) ->
    #rest_resp{code = ?HTTP_200_OK, body = #{<<"users">> => UserIds}};
response(get, _ProvId, {eff_user, UserId}, {ok, User}) ->
    user_rest_translator:response(get, UserId, entity, {ok, User});
response(get, _ProvId, eff_groups, {ok, GroupIds}) ->
    #rest_resp{code = ?HTTP_200_OK, body = #{<<"groups">> => GroupIds}};
response(get, _ProvId, {eff_group, GroupId}, {ok, Group}) ->
    group_rest_translator:response(get, GroupId, entity, {ok, Group});
response(get, _ProvId, spaces, {ok, SpaceIds}) ->
    #rest_resp{code = ?HTTP_200_OK, body = #{<<"spaces">> => SpaceIds}};
response(get, _ProvId, {space, SpaceId}, {ok, Space}) ->
    space_rest_translator:response(get, SpaceId, entity, {ok, Space});
response(get, _ProvId, {check_my_ip, _}, {ok, IP}) ->
    #rest_resp{code = ?HTTP_200_OK, body = IP};

response(update, _ProvId, entity, ok) ->
    rest_translator:updated_reply();

response(update, _ProvId, {space, _SpaceId}, ok) ->
    rest_translator:updated_reply();

response(delete, _ProvId, entity, ok) ->
    rest_translator:deleted_reply();

response(delete, _ProvId, {space, _SpaceId}, ok) ->
    rest_translator:deleted_reply().