%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains helper functions for modules implementing
%%% rest_module_behavior.
%%% @see rest_module_behavior
%%% @end
%%%-------------------------------------------------------------------
-module(rest_module_helper).
-author("Konrad Zemek").

-include_lib("ctool/include/logging.hrl").

%% API
-export([report_error/2, report_error/3, report_invalid_value/3, report_missing_key/2]).
-export([assert_type/4, assert_key/4, assert_key_value/5]).

-type json_string() :: atom() | binary().
-type error() :: invalid_request | invalid_client | invalid_grant |
unauthorized_client | unsupported_grant_type | invalid_scope | would_introduce_cycle.
-export_type([error/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a value of a parameter if it has an expected type, or 'undefined'
%% if it doesn't exist. Throws otherwise.
%% @end
%%--------------------------------------------------------------------
-spec assert_type(Key :: json_string(), List :: [{json_string(), term()}],
    Type :: list_of_bin, Req :: cowboy_req:req()) ->
    [binary()] | undefined | no_return();
    (Key :: json_string(), List :: [{json_string(), term()}],
        Type :: binary, Req :: cowboy_req:req()) ->
    binary() | undefined | no_return();
    (Key :: json_string(), List :: [{json_string(), term()}],
        Type :: float, Req :: cowboy_req:req()) ->
    float() | undefined | no_return();
    (Key :: json_string(), List :: [{json_string(), term()}],
        Type :: pos_integer, Req :: cowboy_req:req()) ->
    pos_integer() | undefined | no_return().
assert_type(Key, List, Type, Req) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} when Type =:= binary andalso is_binary(Value) ->
            Value;

        {Key, Value} when Type =:= list_of_bin andalso is_list(Value) ->
            case lists:all(fun is_binary/1, Value) of
                true -> Value;
                false -> report_invalid_value(Key, Value, Req)
            end;

        {Key, Value} when Type =:= pos_integer andalso is_binary(Value) ->
            Regex = <<"[1-9][0-9]*">>,
            case re:run(Value, Regex, [{capture, first, binary}]) of
                {match, [Value]} -> binary_to_integer(Value);
                _ -> report_invalid_value(Key, Value, Req)
            end;

        {Key, Value} when Type =:= float andalso is_float(Value) ->
            Value;

        {Key, Value} ->
            report_invalid_value(Key, Value, Req);

        false ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @doc Returns a value of a parameter if it exists and has an expected type,
%% throws otherwise.
%% @end
%%--------------------------------------------------------------------
-spec assert_key(Key :: json_string(), List :: [{json_string(), term()}],
    Type :: list_of_bin, Req :: cowboy_req:req()) -> [binary()] | no_return();
    (Key :: json_string(), List :: [{json_string(), term()}],
        Type :: binary, Req :: cowboy_req:req()) -> binary() | no_return();
    (Key :: json_string(), List :: [{json_string(), term()}],
        Type :: pos_integer, Req :: cowboy_req:req()) -> pos_integer() | no_return().
assert_key(Key, List, Type, Req) ->
    case assert_type(Key, List, Type, Req) of
        undefined -> report_missing_key(Key, Req);
        Value -> Value
    end.

%%--------------------------------------------------------------------
%% @doc Returns a value of a parameter if it exists and its values are from
%% an accepted range, throws otherwise.
%% @end
%%--------------------------------------------------------------------
-spec assert_key_value(Key :: json_string(), AcceptedValues :: [binary()],
    List :: [{json_string(), term()}], Type :: list_of_bin,
    Req :: cowboy_req:req()) -> [binary()] | no_return();
    (Key :: json_string(), AcceptedValues :: [binary()],
        List :: [{json_string(), term()}], Type :: binary,
        Req :: cowboy_req:req()) -> binary() | no_return().
assert_key_value(Key, AcceptedValues, List, list_of_bin, Req) ->
    Value = assert_key(Key, List, list_of_bin, Req),
    ValuesSet = ordsets:from_list(Value),
    AcceptedValuesSet = ordsets:from_list(AcceptedValues),
    case ordsets:subtract(ValuesSet, AcceptedValuesSet) of
        [] -> Value;
        [InvalidValue | _] -> report_invalid_value(Key, InvalidValue, Req)
    end;
assert_key_value(Key, AcceptedValues, List, binary, Req) ->
    Value = assert_key(Key, List, binary, Req),
    case lists:member(Value, AcceptedValues) of
        true -> Value;
        false -> report_invalid_value(Key, Value, Req)
    end.

%%--------------------------------------------------------------------
%% @doc Throws an exception to report an invalid value.
%%--------------------------------------------------------------------
-spec report_invalid_value(Key :: json_string(), Value :: json_string(),
    Req :: cowboy_req:req()) ->
    no_return().
report_invalid_value(Key, Value, Req) ->
    Description = <<"invalid '", (str_utils:to_binary(Key))/binary,
        "' value: '", (str_utils:to_binary(Value))/binary, "'">>,
    report_error(invalid_request, Description, Req).

%%--------------------------------------------------------------------
%% @doc Throws an exception to report an missing key.
%%--------------------------------------------------------------------
-spec report_missing_key(Key :: json_string(), Req :: cowboy_req:req()) ->
    no_return().
report_missing_key(Key, Req) ->
    Description = <<"missing required key: '",
        (str_utils:to_binary(Key))/binary, "'">>,
    report_error(invalid_request, Description, Req).

%%--------------------------------------------------------------------
%% @doc Throws an exception to report a generic REST error with a description.
%%--------------------------------------------------------------------
-spec report_error(Type :: error(), Description :: binary(), Req :: cowboy_req:req()) ->
    no_return().
report_error(Type, Description, Req) when is_atom(Type), is_binary(Description) ->
    throw({rest_error, Type, Description, Req}).

%%--------------------------------------------------------------------
%% @doc Throws an exception to report a generic REST error.
%%--------------------------------------------------------------------
-spec report_error(Type :: error(), Req :: cowboy_req:req()) ->
    no_return().
report_error(Type, Req) when is_atom(Type) ->
    throw({rest_error, Type, Req}).
