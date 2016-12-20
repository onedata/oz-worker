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
-include("errors.hrl").
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


% General errors
translate_error(?ERROR_INTERNAL_SERVER_ERROR) ->
    500;

translate_error(?ERROR_NOT_FOUND) ->
    404;

translate_error(?ERROR_UNAUTHORIZED) ->
    401;

translate_error(?ERROR_FORBIDDEN) ->
    403;

% Errors connected with bad data
translate_error(?ERROR_MALFORMED_DATA) ->
    {400, <<"Provided data could not be understood by the server">>};

translate_error(?ERROR_MISSING_REQUIRED_VALUE(Key)) ->
    {400, {<<"Missing required value: ~s">>, [Key]}};

translate_error(?ERROR_MISSING_AT_LEAST_ONE_VALUE(Keys)) ->
    KeysList = str_utils:join_binary(maps:keys(Keys), <<", ">>),
    {400, {<<"Missing data, you must provide at least one of: ">>, [KeysList]}};

translate_error(?ERROR_BAD_DATA(Key)) ->
    {400, {<<"Bad value: provided \"~s\" could not be understood">>, [Key]}};

translate_error(?ERROR_BAD_VALUE_ATOM(Key)) ->
    % Atoms are strings in json
    translate_error(?ERROR_BAD_VALUE_BINARY(Key));

translate_error(?ERROR_BAD_VALUE_LIST_OF_ATOMS(Key)) ->
    % Atoms are strings in json
    translate_error(?ERROR_BAD_VALUE_LIST_OF_BINARIES(Key));

translate_error(?ERROR_BAD_VALUE_BINARY(Key)) ->
    {400, {<<"Bad value: provided \"~s\" must be a string">>, [Key]}};

translate_error(?ERROR_BAD_VALUE_LIST_OF_BINARIES(Key)) ->
    {400, {<<"Bad value: provided \"~s\" must be a list of strings">>, [Key]}};

translate_error(?ERROR_BAD_VALUE_INTEGER(Key)) ->
    {400, {<<"Bad value: provided \"~s\" must be an integer">>, [Key]}};

translate_error(?ERROR_BAD_VALUE_FLOAT(Key)) ->
    {400, {<<"Bad value: provided \"~s\" must be a float">>, [Key]}};

translate_error(?ERROR_BAD_VALUE_JSON(Key)) ->
    {400, {<<"Bad value: provided \"~s\" must be a valid JSON">>, [Key]}};

translate_error(?ERROR_EMPTY_VALUE(Key)) ->
    {400, {<<"Bad value: provided \"~s\" must not be empty">>, [Key]}};

translate_error(?ERROR_VALUE_TOO_LOW(Key, Threshold)) ->
    {400, {<<"Bad value: provided \"~s\" must be at least ~B">>, [Key, Threshold]}};

translate_error(?ERROR_VALUE_TOO_HIGH(Key, Threshold)) ->
    {400, {<<"Bad value: provided \"~s\" must not exceed ~B">>, [Key, Threshold]}};

translate_error(?ERROR_VALUE_NOT_BETWEEN(Key, Low, High)) ->
    {400, {<<"Bad value: provided \"~s\" must be between <~B, ~B>">>, [Key, Low, High]}};

translate_error(?ERROR_VALUE_NOT_ALLOWED(Key, AllowedValues)) ->
    % Convert binaries to strings so that we do not include << >> signs in the response.
    AllowedValuesNotBin = lists:map(
        fun(Val) when is_binary(Val) -> binary_to_list(Val);
            (Val) -> Val
        end, AllowedValues),
    {400, {<<"Bad value: provided \"~s\" must be one of: ~p">>, [
        Key, AllowedValuesNotBin]
    }};

translate_error(?ERROR_LIST_OF_VALUES_NOT_ALLOWED(Key, AllowedValues)) ->
    % Convert binaries to strings so that we do not include << >> signs in the response.
    AllowedValuesNotBin = lists:map(
        fun(Val) when is_binary(Val) -> binary_to_list(Val);
            (Val) -> Val
        end, AllowedValues),
    {400, {<<"Bad value: provided \"~s\" must be a list containing zero or more following values: ~p">>, [
        Key, AllowedValuesNotBin]
    }};

translate_error(?ERROR_ID_NOT_FOUND(Key)) ->
    {400, {<<"Bad value: provided ID (\"~s\") does not exist">>, [Key]}};

translate_error(?ERROR_ID_OCCUPIED(Key)) ->
    {400, {<<"Bad value: provided ID (\"~s\") is already occupied">>, [Key]}};

translate_error(?ERROR_BAD_TOKEN(Key)) ->
    {400, {<<"Bad value: provided \"~s\" is not a valid token">>, [Key]}};

translate_error(?ERROR_BAD_TOKEN_TYPE(Key)) ->
    {400, {<<"Bad value: provided \"~s\" is of invalid type">>, [Key]}};

% Errors connected with relations between entities
translate_error(?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId)) ->
    RelationToString = case {ChType, ParType} of
        {od_space, od_provider} -> <<"is not supported by">>;
        {_, _} -> <<"is not a member of">>
    end,
    {400, {<<"Bad value: ~s \"~s\" ~s ~s \"~s\"">>, [
        model_to_string(ChType), ChId, RelationToString, model_to_string(ParType), ParId
    ]}};

translate_error(?ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId)) ->
    RelationToString = case {ChType, ParType} of
        {od_space, od_provider} -> <<"is alraedy supported by">>;
        {_, _} -> <<"is already a member of">>
    end,
    {400, {<<"Bad value: ~s \"~s\" ~s ~s \"~s\"">>, [
        model_to_string(ChType), ChId, RelationToString, model_to_string(ParType), ParId
    ]}};

% Wildcard match
translate_error({error, Reason}) ->
    ?warning("Unexpected error: {error, ~p} in rest error translator", [Reason]),
    translate_error(?ERROR_INTERNAL_SERVER_ERROR).


translate(_, _, _, _, {error, Type}) ->
    case translate_error({error, Type}) of
        Code when is_integer(Code) ->
            Code;
        {Code, MessageBinary} ->
            {Code, #{<<"error">> => MessageBinary}};
        {Code, {MessageFormat, FormatArgs}} ->
            {Code, #{<<"error">> => str_utils:format_bin(MessageFormat, FormatArgs)}}
    end;
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


model_to_string(od_user) -> <<"user">>;
model_to_string(od_group) -> <<"group">>;
model_to_string(od_provider) -> <<"provider">>;
model_to_string(od_space) -> <<"space">>;
model_to_string(od_share) -> <<"share">>;
model_to_string(od_handle_service) -> <<"handle service">>;
model_to_string(od_handle) -> <<"handle">>.