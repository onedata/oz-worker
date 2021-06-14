%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles validation of automation lambda specification.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_validator).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").

-export([validate/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec validate(od_atm_lambda:record()) -> ok | errors:error().
validate(AtmLambda) ->
    atm_schema_validator:run_validation_procedures(AtmLambda, [
        fun validate_argument_names/1,
        fun validate_result_names/1,
        fun sanitize_initial_values/1
    ]).

%%%===================================================================
%%% Validation procedures
%%%===================================================================

%% @private
-spec validate_argument_names(od_atm_lambda:record()) ->
    ok | errors:error().
validate_argument_names(#od_atm_lambda{argument_specs = ArgumentSpecs}) ->
    ArgumentNames = [S#atm_lambda_argument_spec.name || S <- ArgumentSpecs],
    atm_schema_validator:assert_unique_identifiers(name, ArgumentNames, <<"argumentSpecs">>).


%% @private
-spec validate_result_names(od_atm_lambda:record()) ->
    ok | errors:error().
validate_result_names(#od_atm_lambda{result_specs = ResultSpecs}) ->
    ResultNames = [S#atm_lambda_result_spec.name || S <- ResultSpecs],
    atm_schema_validator:assert_unique_identifiers(name, ResultNames, <<"resultSpecs">>).


%% @private
-spec sanitize_initial_values(od_atm_lambda:record()) ->
    ok | errors:error().
sanitize_initial_values(#od_atm_lambda{argument_specs = ArgumentSpecs}) ->
    lists:foreach(fun(#atm_lambda_argument_spec{
        name = Name,
        default_value = DefaultValue,
        is_batch = IsBatch,
        data_spec = DataSpec
    }) ->
        DataKeyName = str_utils:format_bin("argumentSpecs[~s].defaultValue", [Name]),
        case IsBatch of
            false ->
                atm_schema_validator:sanitize_initial_value(DefaultValue, DataSpec, DataKeyName);
            true when DefaultValue == undefined ->
                ok;
            true when not is_list(DefaultValue) ->
                atm_schema_validator:raise_validation_error(DataKeyName, "Expected a list of values (batch)");
            true ->
                lists:foreach(fun(Value) ->
                    atm_schema_validator:sanitize_initial_value(Value, DataSpec, DataKeyName)
                end, DefaultValue)
        end
    end, ArgumentSpecs).
