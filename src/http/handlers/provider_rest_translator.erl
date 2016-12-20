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
    #rest_resp{code = 200, body = #{
        <<"providerId">> => ProvId,
        <<"certificate">> => Certificate
    }};
response(create, undefined, entity_dev, {ok, {ProvId, Certificate}}) ->
    #rest_resp{code = 200, body = #{
        <<"providerId">> => ProvId,
        <<"certificate">> => Certificate
    }};
response(create, _ProvId, spaces, {ok, SpaceId}) ->
    rest_translator:created_reply([<<"provider/spaces/">>, SpaceId]);
response(create, _ProvId, support, {ok, SpaceId}) ->
    rest_translator:created_reply([<<"provider/spaces/">>, SpaceId]);
response(create, _ProvId, check_my_ports, {ok, Body}) ->
    #rest_resp{code = 200, body = Body};
response(get, undefined, list, {ok, ProviderIds}) ->
    #rest_resp{code = 200, body = #{<<"providers">> => ProviderIds}};
response(get, EntityId, entity, {ok, Provider}) ->
    #od_provider{
        name = ClientName,
        urls = URLs,
        redirection_point = RedirectionPoint,
        latitude = Latitude,
        longitude = Longitude
    } = Provider,
    #rest_resp{code = 200, body = #{
        <<"providerId">> => EntityId,
        <<"name">> => ClientName,
        <<"urls">> => URLs,
        <<"redirectionPoint">> => RedirectionPoint,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude
    }};
response(update, _ProvId, entity, ok) ->
    rest_translator:updated_reply();
response(Function, EntityId, Resource, Result) ->
    ?error("Cannot translate provider REST result for:~n"
    "Function: ~p~n"
    "EntityId: ~p~n"
    "Resource: ~p~n"
    "Result: ~p~n", [
        Function, EntityId, Resource, Result
    ]),
    error_rest_translator:response(?ERROR_INTERNAL_SERVER_ERROR).