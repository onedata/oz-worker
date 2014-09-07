%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains helper functions for modules implementing
%% rest_module_behavior.
%% @see rest_module_behavior
%% @end
%% ===================================================================
-module(rest_module_helper).
-author("Konrad Zemek").


%% API
-export([report_invalid_value/2, report_missing_key/1]).
-export([assert_key/2, assert_key_value/3]).

-type json_string() :: atom() | binary().


%% assert_key/2
%% ====================================================================
%% @doc Returns a value of a parameter if it exists, throws otherwise.
%% ====================================================================
-spec assert_key(Key :: json_string(), List :: [{json_string(), json_string()}]) ->
    [json_string()] | json_string() | no_return().
%% ====================================================================
assert_key(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false -> report_missing_key(Key)
    end.


%% assert_key_value/3
%% ====================================================================
%% @doc Returns a value of a parameter if it exists and its values are from
%% an accepted range, throws otherwise.
%% @end
%% ====================================================================
-spec assert_key_value(Key :: json_string(), AcceptedValues :: [json_string()],
                       List :: [{json_string(), json_string()}]) ->
    [json_string()] | json_string() | no_return().
%% ====================================================================
assert_key_value(Key, AcceptedValues, List) ->
    Value = assert_key(Key, List),
    case is_list(Value) of
        true ->
            ValuesSet = ordsets:from_list(Value),
            AcceptedValuesSet = ordsets:from_list(AcceptedValues),
            case ordsets:subtract(ValuesSet, AcceptedValuesSet) of
                [] -> Value;
                [InvalidValue | _] -> report_invalid_value(Key, InvalidValue)
            end;

        false ->
            case lists:member(Value, AcceptedValues) of
                true -> Value;
                false -> report_invalid_value(Key, Value)
            end
    end.


%% report_invalid_value/2
%% ====================================================================
%% @doc Throws an exception to report an invalid value.
%% ====================================================================
-spec report_invalid_value(Key :: json_string(), Value :: json_string()) ->
    no_return().
%% ====================================================================
report_invalid_value(Key, Value) ->
    throw({invalid_value, {Key, Value}}).


%% report_missing_key/1
%% ====================================================================
%% @doc Throws an exception to report an missing key.
%% ====================================================================
-spec report_missing_key(Key :: json_string()) ->
    no_return().
%% ====================================================================
report_missing_key(Key) ->
    throw({missing_key, Key}).
