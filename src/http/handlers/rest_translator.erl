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
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

% TODO definy dla codow

-export([reply/6, translate_error/1]).

-define(PROVIDER_PLUGIN, n_provider_logic_plugin).


reply(Function, LogicPlugin, EntityId, Resource, Result, Req) ->
    {Code, Headers, Body} =
        case translate(Function, LogicPlugin, EntityId, Resource, Result) of
            C when is_integer(C) -> {C, [], <<"">>};
            {C, B} -> {C, [], json_utils:encode_map(B)};
            {C, H, B} -> {C, H, json_utils:encode_map(B)}
        end,
    {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
    Req2.

% TODO moze wyciagnac error przed nawias
translate_error(?ERROR_INTERNAL_SERVER_ERROR) ->
    500;
translate_error(?ERROR_MALFORMED_DATA) ->
    {400, #{<<"error">> => <<"Provided data could not be understood by the server">>}};
translate_error(?ERROR_NOT_FOUND) ->
    404;
translate_error(?ERROR_UNAUTHORIZED) ->
    401;
translate_error(?ERROR_FORBIDDEN) ->
    403;
translate_error(?ERROR_MISSING_REQUIRED_DATA(Key)) ->
    {400, #{<<"error">> => <<"Missing required data: ", Key/binary>>}};
translate_error(?ERROR_MISSING_AT_LEAST_ONE_DATA(Keys)) ->
    KeysList = str_utils:join_binary(maps:keys(Keys), <<", ">>),
    {400, #{<<"error">> => <<"Missing data, you must provide at least one of: ", KeysList/binary>>}};
translate_error(?ERROR_BAD_DATA(Key)) ->
    {400, #{<<"error">> => <<"Bad data: ", Key/binary, "">>}};
translate_error(?ERROR_EMPTY_DATA(Key)) ->
    {400, #{<<"error">> => <<Key/binary, " cannot be empty">>}};
translate_error(?ERROR_ID_NOT_FOUND(Key)) ->
    {400, #{<<"error">> => <<"Provided ", Key/binary, " could not be found">>}};
translate_error(?ERROR_ID_OCCUPIED(Key)) ->
    {400, #{<<"error">> => <<"Provided ", Key/binary, " is occupied">>}};
translate_error(?ERROR_BAD_TOKEN(Key)) ->
    {400, #{<<"error">> => <<"Provided ", Key/binary, " is not valid">>}};
translate_error(?ERROR_BAD_TOKEN_TYPE(Key)) ->
    {400, #{<<"error">> => <<"Provided ", Key/binary, " is of incorrect type">>}};
translate_error(?EL_RELATION_EXISTS) ->
    {400, #{<<"error">> => <<"Such relation already exists">>}};
translate_error(?EL_RELATION_DOES_NOT_EXIST) ->
    {400, #{<<"error">> => <<"Such relation does not exist">>}};
translate_error({error, Reason}) ->
    ?warning("Unexpected error: {error, ~p} in rest error translator", [Reason]),
    translate_error(?ERROR_INTERNAL_SERVER_ERROR).


translate(_, _, _, _, {error, Type}) ->
    translate_error({error, Type});
translate(create, ?PROVIDER_PLUGIN, undefined, entity, {ok, {ProvId, Certificate}}) ->
    {200, #{
        <<"providerId">> => ProvId,
        <<"certificate">> => Certificate
    }};
translate(create, ?PROVIDER_PLUGIN, undefined, entity_dev, {ok, {ProvId, Certificate}}) ->
    {200, #{
        <<"providerId">> => ProvId,
        <<"certificate">> => Certificate
    }};
translate(create, ?PROVIDER_PLUGIN, _ProvId, spaces, {ok, SpaceId}) ->
    created_reply([<<"provider/spaces/">>, SpaceId]);
translate(create, ?PROVIDER_PLUGIN, _ProvId, support, {ok, SpaceId}) ->
    created_reply([<<"provider/spaces/">>, SpaceId]);
translate(create, ?PROVIDER_PLUGIN, _ProvId, check_my_ports, {ok, Body}) ->
    {200, Body};

translate(get, ?PROVIDER_PLUGIN, undefined, list, {ok, ProviderIds}) ->
    {200, #{<<"providers">> => ProviderIds}};
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
translate(update, ?PROVIDER_PLUGIN, _ProvId, entity, ok) ->
    204;


translate(Function, LogicPlugin, EntityId, Resource, Result) ->
    ?error("Cannot translate REST result for:~n"
    "Function: ~p~n"
    "LogicPlugin: ~p~n"
    "EntityId: ~p~n"
    "Resource: ~p~n"
    "Result: ~p~n", [
        Function, LogicPlugin, EntityId, Resource, Result
    ]),
    translate([], [], [], [], ?ERROR_INTERNAL_SERVER_ERROR).


% Make sure there is no leading slash
created_reply([<<"/", Path/binary>> | Tail]) ->
    created_reply([Path | Tail]);
created_reply(PathTokens) ->
    {ok, RestPrefix} = application:get_env(?APP_NAME, rest_api_prefix),
    LocationHeader = [
        {<<"location">>, filename:join([RestPrefix | PathTokens])}
    ],
    {201, LocationHeader, <<"">>}.