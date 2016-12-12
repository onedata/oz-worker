%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains definitions of all REST operations, in a
%%% format of cowboy router.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_translator).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("entity_logic_errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([reply/6]).

-define(PROVIDER_PLUGIN, n_provider_logic_plugin).


reply(Function, LogicPlugin, EntityId, Resource, Result, Req) ->
    {Code, Body} = translate(Function, LogicPlugin, EntityId, Resource, Result),
    {ok, Req2} = cowboy_req:reply(Code, [], json_utils:encode_map(Body), Req),
    Req2.



translate(get, ?PROVIDER_PLUGIN, EntityId, entity, {ok, Provider}) ->
    #od_provider{
        name = ClientName,
        urls = URLs,
        redirection_point = RedirectionPoint,
        latitude = Latitude,
        longitude = Longitude
    } = Provider,
    {200, #{
        <<"providerId">> => EntityId,
        <<"name">> => ClientName,
        <<"urls">> => URLs,
        <<"redirectionPoint">> => RedirectionPoint,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude
    }};
translate(Function, LogicPlugin, EntityId, Resource, _) ->
    ?error("Cannot translate REST result for:~n"
    "Function: ~p~n"
    "LogicPlugin: ~p~n"
    "EntityId: ~p~n"
    "Resource: ~p~n", [
        Function, LogicPlugin, EntityId, Resource
    ]),
    translate([], [], [], [], {error, ?EL_INTERNAL_SERVER_ERROR}).
