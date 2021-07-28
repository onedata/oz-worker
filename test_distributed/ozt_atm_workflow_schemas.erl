%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating automation workflow_schemas of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_atm_workflow_schemas).
-author("Lukasz Opiola").

-include("ozt.hrl").
-include_lib("ctool/include/automation/automation.hrl").

%% API
-export([list/0]).
-export([create/1, create/2, create/3]).
-export([try_create/3]).
-export([get/1]).
-export([get_atm_lambdas/1]).
-export([exists/1]).
-export([try_update/3]).
-export([delete/1]).
-export([dump_schema_to_json/1, dump_schema_to_json/2]).
-export([map_tasks/2]).
-export([extract_atm_lambdas_from_lanes/1]).
-export([substitute_atm_lambdas_for_duplicates_in_lanes/2, substitute_atm_lambdas_for_duplicates_in_lanes/3]).
%% Example data generation
-export([gen_example_data_json/1]).
-export([gen_example_state_json/0]).
-export([gen_example_store_json/0, gen_example_store_json/1, gen_example_store_json/3, gen_example_stores_json/0]).
-export([gen_example_lane_with_parallel_boxes_json/2, gen_example_lane_json/2, gen_example_lanes_json/2]).
-export([gen_parallel_box_with_tasks/1, gen_example_parallel_box/2, gen_example_parallel_boxes/2]).
-export([gen_example_task/2, gen_example_tasks/2]).
-export([gen_example_argument_mappings_for_specs/2, gen_example_argument_mappings/2]).
-export([gen_example_result_mappings_for_specs/2, gen_example_result_mappings/2]).
-export([gen_example_argument_value_builder/1, gen_example_store_iterator_spec/1]).
-export([available_store_types_for_data_spec/1]).

-compile({no_auto_import, [get/1]}).

%%%===================================================================
%%% API
%%%===================================================================

-spec list() -> [od_atm_workflow_schema:id()].
list() ->
    {ok, AtmWorkflowSchemas} = ?assertMatch({ok, _}, ozt:rpc(atm_workflow_schema_logic, list, [?ROOT])),
    AtmWorkflowSchemas.


-spec create(od_atm_inventory:id()) -> od_atm_workflow_schema:id().
create(AtmInventoryId) ->
    create(AtmInventoryId, gen_example_data_json(AtmInventoryId)).

-spec create(od_atm_inventory:id(), entity_logic:data()) -> od_atm_workflow_schema:id().
create(AtmInventoryId, Data) ->
    create(?ROOT, AtmInventoryId, Data).

-spec create(aai:auth(), od_atm_inventory:id(), entity_logic:data()) -> od_atm_workflow_schema:id().
create(Auth, AtmInventoryId, Data) ->
    {ok, AtmWorkflowSchemaId} = ?assertMatch({ok, _}, try_create(Auth, AtmInventoryId, Data)),
    AtmWorkflowSchemaId.


-spec try_create(aai:auth(), od_atm_inventory:id(), entity_logic:data()) ->
    {ok, od_atm_workflow_schema:id()} | errors:error().
try_create(Auth, AtmInventoryId, Data) ->
    ozt:rpc(atm_workflow_schema_logic, create, [Auth, Data#{<<"atmInventoryId">> => AtmInventoryId}]).


-spec get(od_atm_workflow_schema:id()) -> od_atm_workflow_schema:record().
get(AtmWorkflowSchemaId) ->
    {ok, AtmWorkflowSchema} = ?assertMatch({ok, _}, ozt:rpc(atm_workflow_schema_logic, get, [?ROOT, AtmWorkflowSchemaId])),
    AtmWorkflowSchema.


-spec get_atm_lambdas(od_atm_workflow_schema:id()) -> [od_atm_lambda:id()].
get_atm_lambdas(AtmInventoryId) ->
    {ok, Lambdas} = ?assertMatch({ok, _}, ozt:rpc(atm_workflow_schema_logic, get_atm_lambdas, [?ROOT, AtmInventoryId])),
    Lambdas.


-spec try_update(aai:auth(), od_atm_workflow_schema:id(), entity_logic:data()) -> ok | errors:error().
try_update(Auth, AtmWorkflowSchemaId, Data) ->
    ozt:rpc(atm_workflow_schema_logic, update, [Auth, AtmWorkflowSchemaId, Data]).


-spec exists(od_atm_workflow_schema:id()) -> boolean().
exists(AtmWorkflowSchemaId) ->
    ozt:rpc(atm_workflow_schema_logic, exists, [AtmWorkflowSchemaId]).


-spec delete(od_atm_workflow_schema:id()) -> ok.
delete(AtmWorkflowSchemaId) ->
    ?assertMatch(ok, ozt:rpc(atm_workflow_schema_logic, delete, [?ROOT, AtmWorkflowSchemaId])).


-spec dump_schema_to_json(od_atm_workflow_schema:id()) ->
    json_utils:json_term().
dump_schema_to_json(AtmWorkflowSchemaId) when is_binary(AtmWorkflowSchemaId) ->
    dump_schema_to_json(AtmWorkflowSchemaId, get(AtmWorkflowSchemaId)).

-spec dump_schema_to_json(od_atm_workflow_schema:id(), od_atm_workflow_schema:record()) ->
    json_utils:json_term().
dump_schema_to_json(AtmWorkflowSchemaId, AtmWorkflowSchema) ->
    ozt:rpc(od_atm_workflow_schema, dump_schema_to_json, [AtmWorkflowSchemaId, AtmWorkflowSchema]).


-spec map_tasks(
    fun((atm_task_schema:record()) -> atm_task_schema:record()),
    od_atm_workflow_schema:record() | [atm_lane_schema:record()]
) -> od_atm_workflow_schema:record().
map_tasks(MappingFunction, AtmWorkflowSchemaOrLanes) ->
    ozt:rpc(od_atm_workflow_schema, map_tasks, [MappingFunction, AtmWorkflowSchemaOrLanes]).


-spec extract_atm_lambdas_from_lanes([json_utils:json_map() | atm_lane_schema:record()]) ->
    od_atm_workflow_schema:record().
extract_atm_lambdas_from_lanes(Lanes) when is_map(hd(Lanes)) ->
    extract_atm_lambdas_from_lanes(jsonable_record:list_from_json(Lanes, atm_lane_schema));
extract_atm_lambdas_from_lanes(Lanes) ->
    ozt:rpc(od_atm_workflow_schema, extract_atm_lambdas_from_lanes, [Lanes]).


% Tries to find duplicates of lambdas that belong to given inventory and replaces
% their counterparts in given lanes.
-spec substitute_atm_lambdas_for_duplicates_in_lanes([atm_lane_schema:record()], od_atm_inventory:id()) ->
    [atm_lane_schema:record()].
substitute_atm_lambdas_for_duplicates_in_lanes(Lanes, TargetAtmInventoryId) ->
    AtmLambdas = extract_atm_lambdas_from_lanes(Lanes),
    substitute_atm_lambdas_for_duplicates_in_lanes(Lanes, AtmLambdas, TargetAtmInventoryId).

-spec substitute_atm_lambdas_for_duplicates_in_lanes(
    [atm_lane_schema:record()], [od_atm_lambda:id()], od_atm_inventory:id()
) ->
    [atm_lane_schema:record()].
substitute_atm_lambdas_for_duplicates_in_lanes(Lanes, AtmLambdasToSubstitute, TargetAtmInventoryId) ->
    OriginalToDuplicate = ozt_atm_lambdas:find_all_duplicates(AtmLambdasToSubstitute, TargetAtmInventoryId),
    ozt_atm_workflow_schemas:map_tasks(fun(Task = #atm_task_schema{lambda_id = OriginalAtmLambdaId}) ->
        case lists:member(OriginalAtmLambdaId, AtmLambdasToSubstitute) of
            true ->
                Task#atm_task_schema{
                    lambda_id = maps:get(OriginalAtmLambdaId, OriginalToDuplicate)
                };
            false ->
                Task
        end
    end, Lanes).

%%%===================================================================
%%% Example data generation
%%%===================================================================

-spec gen_example_data_json(od_atm_inventory:id() | atom()) -> entity_logic:data().
gen_example_data_json(AtmInventoryId) when is_binary(AtmInventoryId) ->
    % make sure there are some lambdas defined in the inventory
    AtmLambdas = case ozt_atm_inventories:get_atm_lambdas(AtmInventoryId) of
        [] ->
            lists:foreach(fun(_) ->
                ozt_atm_lambdas:create(AtmInventoryId)
            end, lists:seq(1, ?RAND_INT(1, 5))),
            ozt_atm_inventories:get_atm_lambdas(AtmInventoryId);
        List ->
            List
    end,
    Stores = gen_example_stores_json(),
    StoreSchemaIds = [StoreSchemaId || #{<<"id">> := StoreSchemaId} <- Stores],
    #{
        <<"name">> => ozt_atm:gen_example_name(),
        <<"description">> => ozt_atm:gen_example_description(),

        <<"stores">> => Stores,
        <<"lanes">> => gen_example_lanes_json(AtmLambdas, StoreSchemaIds),

        <<"state">> => gen_example_state_json()
    }.


-spec gen_example_state_json() -> json_utils:json_term().
gen_example_state_json() ->
    automation:workflow_schema_state_to_json(lists_utils:random_element(automation:all_workflow_schema_states())).


-spec gen_example_store_json() -> entity_logic:data().
gen_example_store_json() ->
    DataSpec = ozt_atm:gen_example_data_spec(),
    gen_example_store_json(DataSpec).

-spec gen_example_store_json(atm_data_spec:record()) -> entity_logic:data().
gen_example_store_json(DataSpec) ->
    StoreType = lists_utils:random_element(available_store_types_for_data_spec(DataSpec)),
    DefaultInitialValue = case StoreType of
        range ->
            #{<<"start">> => ?RAND_INT(0, 10), <<"end">> => ?RAND_INT(10, 20), <<"step">> => ?RAND_INT(0, 5)};
        list ->
            case ?RAND_BOOL() of
                true ->
                    undefined;
                false ->
                    lists_utils:random_sublist(lists:filtermap(fun(_) ->
                        case ozt_atm:gen_example_initial_value(DataSpec#atm_data_spec.type) of
                            undefined -> false;
                            Value -> {true, Value}
                        end
                    end, lists:seq(1, ?RAND_INT(0, 10))))
            end;
        _ ->
            lists_utils:random_element([undefined, ozt_atm:gen_example_initial_value(DataSpec#atm_data_spec.type)])
    end,
    gen_example_store_json(StoreType, DataSpec, DefaultInitialValue).

-spec gen_example_store_json(automation:store_type(), atm_data_spec:record(), term()) -> entity_logic:data().
gen_example_store_json(StoreType, DataSpec, DefaultInitialValue) ->
    jsonable_record:to_json(#atm_store_schema{
        id = ozt_atm:gen_example_id(),
        name = ozt_atm:gen_example_name(),
        description = ozt_atm:gen_example_description(),
        type = StoreType,
        data_spec = DataSpec,
        requires_initial_value = ?RAND_BOOL(),
        default_initial_value = DefaultInitialValue
    }, atm_store_schema).

-spec gen_example_stores_json() -> entity_logic:data().
gen_example_stores_json() -> lists:map(fun(_) ->
    gen_example_store_json()
end, lists:seq(1, ?RAND_INT(0, 5))).


-spec gen_example_lane_with_parallel_boxes_json([atm_parallel_box_schema:record()], [automation:id()]) ->
    entity_logic:data().
gen_example_lane_with_parallel_boxes_json(ParallelBoxes, StoreSchemaIds) ->
    jsonable_record:to_json(#atm_lane_schema{
        id = ozt_atm:gen_example_id(),
        name = ozt_atm:gen_example_name(),
        parallel_boxes = ParallelBoxes,
        store_iterator_spec = gen_example_store_iterator_spec(StoreSchemaIds)
    }, atm_lane_schema).

-spec gen_example_lane_json([od_atm_lambda:id()], [automation:id()]) -> entity_logic:data().
gen_example_lane_json(_AtmLambdas, []) ->
    error(empty_lambda_list);
gen_example_lane_json([], _StoreSchemaIds) ->
    error(empty_store_schema_list);
gen_example_lane_json(AtmLambdas, StoreSchemaIds) ->
    ParallelBoxes = lists_utils:random_sublist(gen_example_parallel_boxes(AtmLambdas, StoreSchemaIds)),
    gen_example_lane_with_parallel_boxes_json(ParallelBoxes, StoreSchemaIds).


-spec gen_example_lanes_json([od_atm_lambda:id()], [automation:id()]) -> entity_logic:data().
gen_example_lanes_json(_AtmLambdas, []) ->
    [];
gen_example_lanes_json([], _StoreSchemaIds) ->
    [];
gen_example_lanes_json(AtmLambdas, StoreSchemaIds) -> lists:map(fun(_) ->
    gen_example_lane_json(AtmLambdas, StoreSchemaIds)
end, lists:seq(1, ?RAND_INT(0, 5))).


-spec gen_parallel_box_with_tasks([atm_task_schema:record()]) -> atm_parallel_box_schema:record().
gen_parallel_box_with_tasks(Tasks) ->
    #atm_parallel_box_schema{
        id = ozt_atm:gen_example_id(),
        name = ozt_atm:gen_example_name(),
        tasks = Tasks
    }.

-spec gen_example_parallel_box([od_atm_lambda:id()], [automation:id()]) -> atm_parallel_box_schema:record().
gen_example_parallel_box(AtmLambdas, StoreSchemaIds) ->
    gen_parallel_box_with_tasks(lists_utils:random_sublist(gen_example_tasks(AtmLambdas, StoreSchemaIds))).

-spec gen_example_parallel_boxes([od_atm_lambda:id()], [automation:id()]) -> [atm_parallel_box_schema:record()].
gen_example_parallel_boxes(AtmLambdas, StoreSchemaIds) -> lists:map(fun(_) ->
    gen_example_parallel_box(AtmLambdas, StoreSchemaIds)
end, lists:seq(1, ?RAND_INT(0, 5))).


-spec gen_example_task([od_atm_lambda:id()], [automation:id()]) -> atm_task_schema:record().
gen_example_task(AtmLambdas, StoreSchemaIds) ->
    AtmLambdaId = lists_utils:random_element(AtmLambdas),
    #atm_task_schema{
        id = ozt_atm:gen_example_id(),
        name = ozt_atm:gen_example_name(),
        lambda_id = AtmLambdaId,
        argument_mappings = gen_example_argument_mappings(AtmLambdaId, StoreSchemaIds),
        result_mappings = gen_example_result_mappings(AtmLambdaId, StoreSchemaIds)
    }.

-spec gen_example_tasks([od_atm_lambda:id()], [automation:id()]) -> [atm_task_schema:record()].
gen_example_tasks(AtmLambdas, StoreSchemaIds) -> lists:map(fun(_) ->
    gen_example_task(AtmLambdas, StoreSchemaIds)
end, lists:seq(1, ?RAND_INT(0, 5))).


-spec gen_example_argument_mappings_for_specs([atm_lambda_argument_spec:record()], [automation:id()]) ->
    [atm_task_schema_argument_mapper:record()].
gen_example_argument_mappings_for_specs(ArgumentSpecs, StoreSchemaIds) ->
    lists:map(fun(ArgumentSpec) ->
        #atm_task_schema_argument_mapper{
            argument_name = ArgumentSpec#atm_lambda_argument_spec.name,
            value_builder = gen_example_argument_value_builder(StoreSchemaIds)
        }
    end, lists_utils:shuffle(ArgumentSpecs)).

-spec gen_example_argument_mappings(od_atm_lambda:id(), [automation:id()]) ->
    [atm_task_schema_argument_mapper:record()].
gen_example_argument_mappings(AtmLambdaId, StoreSchemaIds) ->
    #od_atm_lambda{argument_specs = ArgumentSpecs} = ozt_atm_lambdas:get(AtmLambdaId),
    case ArgumentSpecs of
        [] ->
            [];
        _ ->
            {OptionalArgumentSpecs, RequiredArgumentSpecs} = lists:partition(fun(ArgumentSpec) ->
                ArgumentSpec#atm_lambda_argument_spec.is_optional
            end, ArgumentSpecs),
            % randomly select what arguments are mapped, but ensuring that all required arguments are
            ReferencedArgumentSpecs = RequiredArgumentSpecs ++ lists_utils:random_sublist(OptionalArgumentSpecs),
            gen_example_argument_mappings_for_specs(ReferencedArgumentSpecs, StoreSchemaIds)
    end.


-spec gen_example_result_mappings_for_specs([atm_lambda_resultspec:record()], [automation:id()]) ->
    [atm_task_schema_result_mapper:record()].
gen_example_result_mappings_for_specs(ResultSpecs, StoreSchemaIds) ->
    lists:map(fun(ResultSpec) ->
        #atm_task_schema_result_mapper{
            result_name = ResultSpec#atm_lambda_result_spec.name,
            store_schema_id = lists_utils:random_element([
                ?CURRENT_TASK_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID,
                ?WORKFLOW_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID
                | StoreSchemaIds
            ]),
            dispatch_function = lists_utils:random_element(atm_task_schema_result_mapper:all_dispatch_functions())
        }
    end, lists_utils:shuffle(ResultSpecs)).


-spec gen_example_result_mappings(od_atm_lambda:id(), [automation:id()]) ->
    [atm_task_schema_result_mapper:record()].
gen_example_result_mappings(AtmLambdaId, StoreSchemaIds) ->
    #od_atm_lambda{result_specs = ResultSpecs} = ozt_atm_lambdas:get(AtmLambdaId),
    case {ResultSpecs, StoreSchemaIds} of
        {[], _} ->
            [];
        {_, []} ->
            [];
        _ ->
            ReferencedResultSpecs = lists:map(fun(_) ->
                lists_utils:random_element(ResultSpecs)
            end, lists:seq(1, ?RAND_INT(0, 5))),
            gen_example_result_mappings_for_specs(ReferencedResultSpecs, StoreSchemaIds)
    end.


-spec gen_example_argument_value_builder([automation:id()]) -> atm_task_argument_value_builder:record().
gen_example_argument_value_builder(StoreSchemaIds) ->
    case rand:uniform(6) of
        1 -> #atm_task_argument_value_builder{
            type = iterated_item, recipe = lists_utils:random_element([
                undefined,
                lists_utils:random_sublist(["key1", "key2", "key3", 0, 1, 2])
            ])
        };
        2 -> #atm_task_argument_value_builder{
            type = const, recipe = lists_utils:random_element([?RAND_STR(), 0, 151, 27.8])
        };
        3 -> #atm_task_argument_value_builder{
            type = object, recipe = maps:from_list(lists:map(fun(_) ->
                {?RAND_STR(), gen_example_argument_value_builder(StoreSchemaIds)}
            end, lists:seq(1, rand:uniform(7))))
        };
        4 ->
            % generated list of stores may be empty, in such case keep generating
            % until there is no store_credentials builder type included
            case StoreSchemaIds of
                [] ->
                    gen_example_argument_value_builder(StoreSchemaIds);
                _ ->
                    #atm_task_argument_value_builder{
                        type = store_credentials, recipe = lists_utils:random_element(StoreSchemaIds)
                    }
            end;
        5 ->
            % generated list of stores may be empty, in such case keep generating
            % until there is no single_value_store_content builder type included
            case StoreSchemaIds of
                [] ->
                    gen_example_argument_value_builder(StoreSchemaIds);
                _ ->
                    #atm_task_argument_value_builder{
                        type = single_value_store_content, recipe = lists_utils:random_element(StoreSchemaIds)
                    }
            end;
        6 -> #atm_task_argument_value_builder{
            type = onedatafs_credentials, recipe = undefined
        }
    end.


-spec gen_example_store_iterator_spec([automation:id()]) -> atm_store_iterator_spec:record().
gen_example_store_iterator_spec(StoreSchemaIds) ->
    lists_utils:random_element([
        #atm_store_iterator_spec{
            store_schema_id = lists_utils:random_element(StoreSchemaIds),
            strategy = #atm_store_iterator_serial_strategy{}
        },
        #atm_store_iterator_spec{
            store_schema_id = lists_utils:random_element(StoreSchemaIds),
            strategy = #atm_store_iterator_batch_strategy{size = ?RAND_INT(1, 1000)}
        }
    ]).


-spec available_store_types_for_data_spec(atm_data_spec:record()) -> [automation:store_type()].
available_store_types_for_data_spec(DataSpec) ->
    case DataSpec#atm_data_spec.type of
        atm_file_type -> automation:all_store_types() -- [range];
        atm_dataset_type -> automation:all_store_types() -- [range];
        atm_integer_type -> automation:all_store_types() -- [tree_forest];
        _ -> automation:all_store_types() -- [range, tree_forest]
    end.


