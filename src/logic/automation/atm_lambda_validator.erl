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

-spec validate(atm_lambda_revision:record()) -> ok | no_return().
validate(AtmLambdaRevision) ->
    atm_schema_validator:run_validation_procedures(AtmLambdaRevision, [
        fun validate_argument_names/1,
        fun validate_result_names/1,
        fun sanitize_default_values/1
    ]).

%%%===================================================================
%%% Validation procedures
%%%===================================================================

%% @private
-spec validate_argument_names(atm_lambda_revision:record()) ->
    ok | errors:error().
validate_argument_names(#atm_lambda_revision{argument_specs = ArgumentSpecs}) ->
    ArgumentNames = [S#atm_lambda_argument_spec.name || S <- ArgumentSpecs],
    atm_schema_validator:assert_unique_identifiers(name, ArgumentNames, <<"argumentSpecs">>).


%% @private
-spec validate_result_names(atm_lambda_revision:record()) ->
    ok | errors:error().
validate_result_names(#atm_lambda_revision{result_specs = ResultSpecs}) ->
    ResultNames = [S#atm_lambda_result_spec.name || S <- ResultSpecs],
    atm_schema_validator:assert_unique_identifiers(name, ResultNames, <<"resultSpecs">>).


%% @private
-spec sanitize_default_values(atm_lambda_revision:record()) ->
    ok | errors:error().
sanitize_default_values(#atm_lambda_revision{argument_specs = ArgumentSpecs}) ->
    lists:foreach(fun(#atm_lambda_argument_spec{
        name = Name,
        default_value = DefaultValue,
        data_spec = DataSpec
    }) ->
        DataKeyName = str_utils:format_bin("argumentSpecs[~s].defaultValue", [Name]),
        atm_schema_validator:sanitize_predefined_value(DefaultValue, DataSpec, DataKeyName)
    end, ArgumentSpecs).
