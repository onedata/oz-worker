%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating automation lambdas of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_atm_lambdas).
-author("Lukasz Opiola").

-include("ozt.hrl").
-include_lib("ctool/include/automation/automation.hrl").

%% API
-export([list/0]).
-export([create/1, create/2, create/3]).
-export([try_create/3]).
-export([get/1]).
-export([exists/1]).
-export([delete/1]).
-export([get_atm_inventories/1]).
-export([get_atm_workflow_schemas/1]).
-export([link_to_inventory/2]).
-export([unlink_from_inventory/2]).
-export([dump_schema_to_json/1]).
-export([find_duplicate/2, find_all_duplicates/2]).
%% Example data generation
-export([gen_example_data_json/0]).
-export([gen_example_operation_spec_json/0]).
-export([gen_example_argument_spec_json/0, gen_example_argument_spec_json/3, gen_example_argument_specs_json/0]).
-export([gen_example_result_spec_json/0, gen_example_result_specs_json/0]).

-compile({no_auto_import, [get/1]}).

%%%===================================================================
%%% API
%%%===================================================================

-spec list() -> [od_atm_lambda:id()].
list() ->
    {ok, AtmLambdas} = ?assertMatch({ok, _}, ozt:rpc(atm_lambda_logic, list, [?ROOT])),
    AtmLambdas.


-spec create(od_atm_inventory:id()) -> od_atm_lambda:id().
create(AtmInventoryId) ->
    create(AtmInventoryId, gen_example_data_json()).

-spec create(od_atm_inventory:id(), entity_logic:data()) -> od_atm_lambda:id().
create(AtmInventoryId, Data) ->
    create(?ROOT, AtmInventoryId, Data).

-spec create(aai:auth(), od_atm_inventory:id(), entity_logic:data()) -> od_atm_lambda:id().
create(Auth, AtmInventoryId, Data) ->
    {ok, AtmLambdaId} = ?assertMatch({ok, _}, try_create(Auth, AtmInventoryId, Data)),
    AtmLambdaId.


-spec try_create(aai:auth(), od_atm_inventory:id(), entity_logic:data()) ->
    {ok, od_atm_lambda:id()} | errors:error().
try_create(Auth, AtmInventoryId, Data) ->
    ozt:rpc(atm_lambda_logic, create, [Auth, Data#{
        <<"atmInventoryId">> => AtmInventoryId
    }]).

-spec get(od_atm_lambda:id()) -> od_atm_lambda:record().
get(AtmLambdaId) ->
    {ok, AtmLambda} = ?assertMatch({ok, _}, ozt:rpc(atm_lambda_logic, get, [?ROOT, AtmLambdaId])),
    AtmLambda.


-spec exists(od_atm_lambda:id()) -> boolean().
exists(AtmLambdaId) ->
    ozt:rpc(atm_lambda_logic, exists, [AtmLambdaId]).


-spec delete(od_atm_lambda:id()) -> ok.
delete(AtmLambdaId) ->
    % atm_lambdas do not support the delete operation - they are deleted when
    % unlinked from all inventories
    #od_atm_lambda{atm_inventories = AtmInventories} = get(AtmLambdaId),
    lists:foreach(fun(AtmInventoryId) ->
        unlink_from_inventory(AtmLambdaId, AtmInventoryId)
    end, AtmInventories).


-spec get_atm_inventories(od_atm_lambda:id()) -> [od_atm_inventory:id()].
get_atm_inventories(AtmLambdaId) ->
    {ok, AtmInventories} = ?assertMatch({ok, _}, ozt:rpc(atm_lambda_logic, get_atm_inventories, [?ROOT, AtmLambdaId])),
    AtmInventories.


-spec get_atm_workflow_schemas(od_atm_lambda:id()) -> [od_atm_inventory:id()].
get_atm_workflow_schemas(AtmLambdaId) ->
    {ok, AtmWorkflowSchemas} = ?assertMatch({ok, _}, ozt:rpc(atm_lambda_logic, get_atm_workflow_schemas, [?ROOT, AtmLambdaId])),
    AtmWorkflowSchemas.


-spec link_to_inventory(od_atm_lambda:id(), od_atm_inventory:id()) -> ok.
link_to_inventory(AtmLambdaId, AtmInventoryId) ->
    ?assertMatch(ok, ozt:rpc(atm_lambda_logic, link_to_inventory, [?ROOT, AtmLambdaId, AtmInventoryId])).


-spec unlink_from_inventory(od_atm_lambda:id(), od_atm_inventory:id()) -> ok.
unlink_from_inventory(AtmLambdaId, AtmInventoryId) ->
    ?assertMatch(ok, ozt:rpc(atm_lambda_logic, unlink_from_inventory, [?ROOT, AtmLambdaId, AtmInventoryId])).


-spec dump_schema_to_json(od_atm_lambda:id() | od_atm_lambda:record()) ->
    json_utils:json_term().
dump_schema_to_json(AtmLambdaId) when is_binary(AtmLambdaId) ->
    dump_schema_to_json(get(AtmLambdaId));
dump_schema_to_json(AtmLambda) ->
    ozt:rpc(od_atm_lambda, dump_to_json, [AtmLambda]).


-spec find_duplicate(od_atm_lambda:id(), od_atm_inventory:id()) ->
    undefined | od_atm_lambda:id().
find_duplicate(PatternAtmLambdaId, TargetAtmInventoryId) ->
    #od_atm_lambda{checksum = TargetChecksum} = get(PatternAtmLambdaId),
    InventoryLambdas = ozt_atm_inventories:get_atm_lambdas(TargetAtmInventoryId),
    lists_utils:foldl_while(fun(CheckedAtmLambdaId, _) ->
        case get(CheckedAtmLambdaId) of
            #od_atm_lambda{checksum = TargetChecksum} ->
                {halt, CheckedAtmLambdaId};
            _ ->
                {cont, undefined}
        end
    end, undefined, InventoryLambdas -- [PatternAtmLambdaId]).


-spec find_all_duplicates([od_atm_lambda:id()], od_atm_inventory:id()) ->
    #{Original :: od_atm_lambda:id() => Duplicate :: od_atm_lambda:id()}.
find_all_duplicates(AtmLambdas, TargetAtmInventoryId) ->
    lists:foldl(fun(AtmLambdaId, Acc) ->
        Acc#{
            AtmLambdaId => case find_duplicate(AtmLambdaId, TargetAtmInventoryId) of
                undefined ->
                    error({lambda_duplicate_not_found, AtmLambdaId, TargetAtmInventoryId});
                DuplicateId ->
                    DuplicateId
            end
        }
    end, #{}, AtmLambdas).

%%%===================================================================
%%% Example data generation
%%%===================================================================

-spec gen_example_data_json() -> entity_logic:data().
gen_example_data_json() ->
    #{
        <<"name">> => ozt_atm:gen_example_name(),
        <<"summary">> => ozt_atm:gen_example_summary(),
        <<"description">> => ozt_atm:gen_example_description(),

        <<"operationSpec">> => gen_example_operation_spec_json(),
        <<"argumentSpecs">> => gen_example_argument_specs_json(),
        <<"resultSpecs">> => gen_example_result_specs_json()
    }.


-spec gen_example_operation_spec_json() -> json_utils:json_term().
gen_example_operation_spec_json() -> jsonable_record:to_json(
    lists_utils:random_element([
        #atm_openfaas_operation_spec{
            docker_image = ?RAND_STR(),
            docker_execution_options = #atm_docker_execution_options{
                readonly = ?RAND_BOOL(),
                mount_oneclient = ?RAND_BOOL(),
                oneclient_mount_point = <<"/a/b/c/d/", (?RAND_STR())/binary>>,
                oneclient_options = lists_utils:random_element([<<"">>, <<"--a --b">>])
            }
        },
        #atm_workflow_operation_spec{
            atm_workflow_id = ?RAND_STR()
        },
        #atm_user_form_operation_spec{
            user_form_id = ?RAND_STR()
        }
    ]), atm_lambda_operation_spec).


-spec gen_example_argument_spec_json() -> json_utils:json_term().
gen_example_argument_spec_json() ->
    DataSpec = ozt_atm:gen_example_data_spec(),
    IsBatch = ?RAND_BOOL(),
    DefaultValue = case IsBatch of
        false ->
            lists_utils:random_element([undefined, ozt_atm:gen_example_initial_value(DataSpec#atm_data_spec.type)]);
        true ->
            lists:filtermap(fun(_) ->
                case ozt_atm:gen_example_initial_value(DataSpec#atm_data_spec.type) of
                    undefined -> false;
                    Value -> {true, Value}
                end
            end, lists:seq(1, ?RAND_INT(0, 5)))
    end,
    gen_example_argument_spec_json(DataSpec, IsBatch, DefaultValue).

-spec gen_example_argument_spec_json(atm_data_spec:record(), boolean(), term()) -> json_utils:json_term().
gen_example_argument_spec_json(DataSpec, IsBatch, DefaultValue) ->
    jsonable_record:to_json(#atm_lambda_argument_spec{
        name = ?UNIQUE_STRING,
        data_spec = DataSpec,
        is_batch = IsBatch,
        is_optional = ?RAND_BOOL(),
        default_value = DefaultValue
    }, atm_lambda_argument_spec).

-spec gen_example_argument_specs_json() -> json_utils:json_term().
gen_example_argument_specs_json() -> lists:map(fun(_) ->
    gen_example_argument_spec_json()
end, lists:seq(1, ?RAND_INT(0, 5))).


-spec gen_example_result_spec_json() -> json_utils:json_term().
gen_example_result_spec_json() ->
    jsonable_record:to_json(#atm_lambda_result_spec{
        name = ?UNIQUE_STRING,
        data_spec = ozt_atm:gen_example_data_spec(),
        is_batch = ?RAND_BOOL()
    }, atm_lambda_result_spec).

-spec gen_example_result_specs_json() -> json_utils:json_term().
gen_example_result_specs_json() -> lists:map(fun(_) ->
    gen_example_result_spec_json()
end, lists:seq(1, ?RAND_INT(0, 5))).
