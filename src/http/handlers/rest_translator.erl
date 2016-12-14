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

-export([reply/6]).

-define(PROVIDER_PLUGIN, n_provider_logic_plugin).


reply(Function, LogicPlugin, EntityId, Resource, Result, Req) ->
    {Code, Headers, Body} =
        case translate(Function, LogicPlugin, EntityId, Resource, Result) of
            C -> {C, [], <<"">>};
            {C, B} -> {C, [], json_utils:encode_map(B)};
            {C, H, B} -> {C, H, json_utils:encode_map(B)}
        end,
    {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
    Req2.


translate(_, _, _, _, {error, ?EL_INTERNAL_SERVER_ERROR}) ->
    500;
translate(_, _, _, _, {error, ?EL_NOT_FOUND}) ->
    404;
translate(_, _, _, _, {error, ?EL_UNAUTHORIZED}) ->
    401;
translate(_, _, _, _, {error, ?EL_FORBIDDEN}) ->
    403;
translate(_, _, _, _, {error, ?EL_MISSING_REQUIRED_DATA(Key)}) ->
    {400, #{<<"error">> => <<"Missing required data: ", Key/binary>>}};
translate(Function, LogicPlugin, _, Resource, {error, ?EL_MISSING_AT_LEAST_ONE_DATA}) ->
    #{at_least_one := AtLeastOne} = LogicPlugin:validate(Function, Resource),
    KeysList = str_utils:join_binary(maps:keys(AtLeastOne), <<", ">>),
    {400, #{<<"error">> => <<"Missing data, you must provide at least one of: ", KeysList/binary>>}};
translate(_, _, _, _, {error, ?EL_BAD_DATA}) ->
    {400, #{<<"error">> => <<"Provided data could not be understood by the server">>}};
translate(_, _, _, _, {error, ?EL_BAD_DATA(Key)}) ->
    {400, #{<<"error">> => <<"Bad data: ", Key/binary, ". TODO HINT", >>}}; %TODO
translate(_, _, _, _, {error, ?EL_EMPTY_DATA(Key)}) ->
    {400, #{<<"error">> => <<Key/binary, " cannot be empty">>}};
translate(_, _, _, _, {error, ?EL_ID_NOT_FOUND(Key)}) ->
    {400, #{<<"error">> => <<"Provided ", Key/binary, " could not be found">>}};
translate(_, _, _, _, {error, ?EL_ID_OCCUPIED(Key)}) ->
    {400, #{<<"error">> => <<"Provided ", Key/binary, " is occupied">>}};
translate(_, _, _, _, {error, ?EL_BAD_TOKEN(Key)}) ->
    {400, #{<<"error">> => <<"Provided ", Key/binary, " is not valid">>}};
translate(_, _, _, _, {error, ?EL_RELATION_EXISTS}) ->
    {400, #{<<"error">> => <<"Such relation already exists">>}};
translate(_, _, _, _, {error, ?EL_RELATION_DOES_NOT_EXIST}) ->
    {400, #{<<"error">> => <<"Such relation does not exist">>}};


translate(_, _, _, _, {error, ?EL_NOT_FOUND}) ->
    404;
translate(_, _, _, _, {error, ?EL_NOT_FOUND}) ->
    404;
translate(_, _, _, _, {error, ?EL_NOT_FOUND}) ->
    404;




translate(create, ?PROVIDER_PLUGIN, undefined, entity, {ok, {ProvId, Certificate}}) ->
    {204, #{
        <<"providerId">> => ProvId,
        <<"certificate">> => Certificate
    }};
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
translate(Function, LogicPlugin, EntityId, Resource, Result) ->
    ?error("Cannot translate REST result for:~n"
    "Function: ~p~n"
    "LogicPlugin: ~p~n"
    "EntityId: ~p~n"
    "Resource: ~p~n"
    "Result: ~p~n", [
        Function, LogicPlugin, EntityId, Resource, Result
    ]),
    translate([], [], [], [], {error, ?EL_INTERNAL_SERVER_ERROR}).


% Make sure there is no leading slash
created_reply([<<"/", Path/binary>> | Tail]) ->
    created_reply([Path | Tail]);
created_reply(PathTokens) ->
    {ok, RestPrefix} = application:get_env(?APP_NAME, rest_api_prefix),
    LocationHeader = [
        {<<"location">>, filename:join([RestPrefix | PathTokens])}
    ],
    {201, LocationHeader, <<"">>}.