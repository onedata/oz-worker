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
        fun validate_config_parameter_names/1,
        fun validate_argument_names/1,
        fun validate_result_names/1,
        fun sanitize_config_parameter_default_values/1,
        fun sanitize_argument_default_values/1,
        fun sanitize_config_parameter_data_specs/1,
        fun sanitize_argument_data_specs/1
    ]).

%%%===================================================================
%%% Validation procedures
%%%===================================================================

%% @private
-spec validate_config_parameter_names(atm_lambda_revision:record()) ->
    ok | no_return().
validate_config_parameter_names(#atm_lambda_revision{config_parameter_specs = ConfigParameterSpecs}) ->
    ParameterNames = [S#atm_parameter_spec.name || S <- ConfigParameterSpecs],
    atm_schema_validator:assert_unique_identifiers(name, ParameterNames, <<"configParameterSpecs">>).


%% @private
-spec validate_argument_names(atm_lambda_revision:record()) ->
    ok | no_return().
validate_argument_names(#atm_lambda_revision{argument_specs = ArgumentSpecs}) ->
    ArgumentNames = [S#atm_parameter_spec.name || S <- ArgumentSpecs],
    atm_schema_validator:assert_unique_identifiers(name, ArgumentNames, <<"argumentSpecs">>).


%% @private
-spec validate_result_names(atm_lambda_revision:record()) ->
    ok | no_return().
validate_result_names(#atm_lambda_revision{result_specs = ResultSpecs}) ->
    ResultNames = [S#atm_lambda_result_spec.name || S <- ResultSpecs],
    atm_schema_validator:assert_unique_identifiers(name, ResultNames, <<"resultSpecs">>).


%% @private
-spec sanitize_config_parameter_default_values(atm_lambda_revision:record()) ->
    ok | no_return().
sanitize_config_parameter_default_values(#atm_lambda_revision{config_parameter_specs = ConfigParameterSpecs}) ->
    sanitize_parameter_default_values(ConfigParameterSpecs, <<"configParameterSpecs">>).


%% @private
-spec sanitize_argument_default_values(atm_lambda_revision:record()) ->
    ok | no_return().
sanitize_argument_default_values(#atm_lambda_revision{argument_specs = ArgumentSpecs}) ->
    sanitize_parameter_default_values(ArgumentSpecs, <<"argumentSpecs">>).


%% @private
-spec sanitize_config_parameter_data_specs(atm_lambda_revision:record()) ->
    ok | no_return().
sanitize_config_parameter_data_specs(#atm_lambda_revision{config_parameter_specs = ConfigParameterSpecs}) ->
    sanitize_input_parameter_data_specs(ConfigParameterSpecs, <<"configParameterSpecs">>).


%% @private
-spec sanitize_argument_data_specs(atm_lambda_revision:record()) ->
    ok | no_return().
sanitize_argument_data_specs(#atm_lambda_revision{argument_specs = ArgumentSpecs}) ->
    sanitize_input_parameter_data_specs(ArgumentSpecs, <<"argumentSpecs">>).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% @private
-spec sanitize_parameter_default_values([atm_parameter_spec:record()], binary()) ->
    ok | no_return().
sanitize_parameter_default_values(ParameterSpecs, FieldName) ->
    lists:foreach(fun(#atm_parameter_spec{
        name = ParameterName,
        default_value = DefaultValue,
        data_spec = DataSpec
    }) ->
        DataKeyName = str_utils:format_bin("~ts[~ts].defaultValue", [FieldName, ParameterName]),
        atm_schema_validator:sanitize_predefined_value(DefaultValue, DataSpec, DataKeyName)
    end, ParameterSpecs).


%% @private
-spec sanitize_input_parameter_data_specs([atm_parameter_spec:record()], binary()) ->
    ok | no_return().
sanitize_input_parameter_data_specs(ParameterSpecs, FieldName) ->
    lists:foreach(fun(#atm_parameter_spec{
        name = ParameterName,
        data_spec = DataSpec
    }) ->
        DataKeyName = str_utils:format_bin("~ts[~ts].dataSpec", [FieldName, ParameterName]),
        sanitize_input_parameter_data_spec(DataSpec, DataKeyName)
    end, ParameterSpecs).


%% @private
-spec sanitize_input_parameter_data_spec(atm_data_spec:record(), atm_schema_validator:data_key_name()) ->
    ok | no_return().
sanitize_input_parameter_data_spec(#atm_file_data_spec{attributes = [_ | _]}, _DataKeyName) ->
    ok;
sanitize_input_parameter_data_spec(#atm_file_data_spec{attributes = _}, DataKeyName) ->
    % In case of data specs of lambda inputs (config, arguments), atm file data spec must
    % specify what file attributes are required.
    % In other contexts (result spec, store config), the attributes are not relevant, because
    % only a file reference is passed.
    atm_schema_validator:raise_validation_error(
        str_utils:format_bin("~ts.attributes", [DataKeyName]),
        <<"This field must be provided and must be a list containing at least one file attribute">>
    );
sanitize_input_parameter_data_spec(#atm_array_data_spec{item_data_spec = ItemDataSpec}, DataKeyName) ->
    sanitize_input_parameter_data_spec(ItemDataSpec, <<DataKeyName/binary, ".itemDataSpec">>);
sanitize_input_parameter_data_spec(_, _DataKeyName) ->
    ok.
