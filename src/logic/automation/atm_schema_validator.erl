%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common functions used by automation schema validators.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_schema_validator).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").

-export([run_validation_procedures/2]).
-export([raise_validation_error/2, raise_validation_error/3]).
-export([assert_unique_identifiers/3]).
-export([assert_known_names/3]).
-export([sanitize_predefined_value/3]).

% Name of the key in data object
-type data_key_name() :: binary().
-export_type([data_key_name/0]).

%% @TODO VFS-7755 make sure requested dispatch_function is supported for given store type
%% @TODO VFS-7755 check if result data spec is compatible with target store's data spec
%% @TODO VFS-7755 for item value builder, check if iterated store's data spec is compatible with argument's data spec
%% @TODO VFS-7755 check if argument has is_batch=true and if so, if the corresponding lane's iterator has batch strategy
%% @TODO VFS-7755 check if const value builder is compatible with argument's data spec
%% @TODO VFS-7755 consider built in audit log when validating referenced store schemas - do we need any other built in stores?
%% @TODO VFS-7755 sanitize result's data specs - e.g. store credentials can only appear in arguments
%% @TODO VFS-7755 check if storeSchemaId from recipe of single_value_store_content task arg builder points to single_value store
%% @TODO VFS-7755 examine the workflows diagram and think what else should be validated
%% @TODO VFS-7755 resource spec in lambdas/tasks should be limited for lambdas using OpenFaaS

%%%===================================================================
%%% API
%%%===================================================================

-spec run_validation_procedures(
    Ctx :: term(),
    [fun((jsonable_record:record()) -> ok | errors:error()) | no_return()]
) ->
    ok | no_return().
run_validation_procedures(Ctx, ValidationProcedures) ->
    lists:foreach(fun(ValidationProcedure) ->
        % each procedure may throw an error internally
        case ValidationProcedure(Ctx) of
            ok -> ok;
            {error, _} = Error -> throw(Error)
        end
    end, ValidationProcedures).


-spec raise_validation_error(data_key_name(), string() | binary()) -> no_return().
raise_validation_error(DataKeyName, Description) when is_list(Description) ->
    raise_validation_error(DataKeyName, str_utils:unicode_list_to_binary(Description));
raise_validation_error(DataKeyName, Description) when is_binary(Description) ->
    throw(?ERROR_BAD_DATA(DataKeyName, Description)).

-spec raise_validation_error(data_key_name(), string(), [term()]) -> no_return().
raise_validation_error(DataKeyName, Format, Args) ->
    throw(?ERROR_BAD_DATA(DataKeyName, str_utils:format_bin(Format, Args))).


-spec assert_unique_identifiers(id | name, [automation:id() | automation:name()], data_key_name()) -> ok | no_return().
assert_unique_identifiers(IdentifierType, Identifiers, DataKeyName) ->
    case length(Identifiers) == length(lists:usort(Identifiers)) of
        true ->
            ok;
        false ->
            raise_validation_error(DataKeyName, "The provided list contains duplicate ~tss", [IdentifierType])
    end.


-spec assert_known_names([automation:name()], [automation:name()], data_key_name()) -> ok | no_return().
assert_known_names(NamesToCheck, KnownNames, DataKeyName) ->
    case lists_utils:subtract(NamesToCheck, KnownNames) of
        [] ->
            ok;
        UnknownNames ->
            raise_validation_error(
                DataKeyName,
                "The following names were not recognized (they reference inexistent definitions): ~ts",
                [str_utils:join_binary(UnknownNames, <<", ">>)]
            )
    end.


%%--------------------------------------------------------------------
%% @doc
%% Ensures that the predefined value is valid in relation to given data spec.
%% @TODO VFS-7683 Limit the size of values inserted into atm_stores or initial contents
%% @TODO VFS-7755 Check if the predefined value's being an array corresponds to is_batch flag
%% @end
%%--------------------------------------------------------------------
-spec sanitize_predefined_value(json_utils:json_term(), atm_data_spec:record(), data_key_name()) ->
    ok | no_return().
sanitize_predefined_value(undefined, _DataSpec, _DataKeyName) ->
    ok;
sanitize_predefined_value(Array, #atm_array_data_spec{item_data_spec = ItemDataSpec}, DataKeyName) ->
    case atm_data_type:is_instance(atm_array_type, Array) of
        true ->
            lists:foreach(fun({Index, Value}) ->
                NestedDataKeyName = str_utils:format_bin("~ts[~B]", [
                    DataKeyName, Index - 1  % count from 0 rather than 1 (as Erlang does)
                ]),
                sanitize_predefined_value(Value, ItemDataSpec, NestedDataKeyName)
            end, lists_utils:enumerate(Array));
        false ->
            raise_validation_error(
                DataKeyName,
                "The provided predefined value for type '~ts' must be an array of values",
                [atm_data_type:type_to_json(atm_array_type)]
            )
    end;
sanitize_predefined_value(Value, AtmDataSpec, DataKeyName) ->
    DataType = atm_data_spec:get_data_type(AtmDataSpec),

    case atm_data_type:is_instance(DataType, Value) of
        true ->
            ok;
        false ->
            raise_validation_error(DataKeyName, "The provided predefined value is invalid for type '~ts'", [
                atm_data_type:type_to_json(DataType)
            ])
    end.
