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
-export([get/1]).
-export([exists/1]).
-export([extract_atm_lambdas_from_lanes/1]).
-export([delete/1]).
%% Example data generation
-export([gen_example_data/1]).
-export([gen_example_state/0]).
-export([gen_example_stores/0]).
-export([gen_example_lanes/2, gen_example_parallel_boxes/2, gen_example_tasks/2]).
-export([gen_example_argument_mappings/2, gen_example_result_mappings/2]).
-export([gen_example_argument_value_builder/1, gen_example_store_iterator_spec/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec list() -> [od_atm_workflow_schema:id()].
list() ->
    {ok, AtmWorkflowSchemas} = ?assertMatch({ok, _}, ozt:rpc(atm_workflow_schema_logic, list, [?ROOT])),
    AtmWorkflowSchemas.


-spec create(od_atm_inventory:id()) -> od_atm_workflow_schema:id().
create(AtmInventoryId) ->
    create(AtmInventoryId, gen_example_data(AtmInventoryId)).

-spec create(od_atm_inventory:id(), entity_logic:data()) -> od_atm_workflow_schema:id().
create(AtmInventoryId, Data) ->
    create(?ROOT, AtmInventoryId, Data).

-spec create(aai:auth(), od_atm_inventory:id(), entity_logic:data()) -> od_atm_workflow_schema:id().
create(Auth, AtmInventoryId, Data) ->
    {ok, AtmWorkflowSchemaId} = ?assertMatch({ok, _}, ozt:rpc(atm_workflow_schema_logic, create, [Auth, Data#{
        <<"atmInventoryId">> => AtmInventoryId
    }])),
    AtmWorkflowSchemaId.


-spec get(od_atm_workflow_schema:id()) -> od_atm_workflow_schema:record().
get(AtmWorkflowSchemaId) ->
    {ok, AtmWorkflowSchema} = ?assertMatch({ok, _}, ozt:rpc(atm_workflow_schema_logic, get, [?ROOT, AtmWorkflowSchemaId])),
    AtmWorkflowSchema.


-spec exists(od_atm_workflow_schema:id()) -> boolean().
exists(AtmWorkflowSchemaId) ->
    ozt:rpc(atm_workflow_schema_logic, exists, [AtmWorkflowSchemaId]).


-spec extract_atm_lambdas_from_lanes([json_utils:json_map() | atm_lane_schema:record()]) -> od_atm_workflow_schema:record().
extract_atm_lambdas_from_lanes(Lanes) when is_map(hd(Lanes)) ->
    extract_atm_lambdas_from_lanes(jsonable_record:list_from_json(Lanes, atm_lane_schema));
extract_atm_lambdas_from_lanes(Lanes) ->
    ozt:rpc(od_atm_workflow_schema, extract_atm_lambdas_from_lanes, [Lanes]).


-spec delete(od_atm_workflow_schema:id()) -> ok.
delete(AtmWorkflowSchemaId) ->
    ?assertMatch(ok, ozt:rpc(atm_workflow_schema_logic, delete, [?ROOT, AtmWorkflowSchemaId])).

%%%===================================================================
%%% Example data generation
%%%===================================================================

-spec gen_example_data(od_atm_inventory:id() | atom()) -> entity_logic:data().
gen_example_data(AtmInventoryId) when is_binary(AtmInventoryId) ->
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
    Stores = gen_example_stores(),
    StoreSchemaIds = [StoreSchemaId || #{<<"id">> := StoreSchemaId} <- Stores],
    #{
        <<"name">> => ozt_atm:gen_example_name(),
        <<"description">> => ozt_atm:gen_example_description(),

        <<"stores">> => Stores,
        <<"lanes">> => gen_example_lanes(AtmLambdas, StoreSchemaIds),

        <<"state">> => gen_example_state()
    }.


-spec gen_example_state() -> json_utils:json_term().
gen_example_state() ->
    automation:workflow_schema_state_to_json(lists_utils:random_element([
        incomplete, ready, deprecated
    ])).


-spec gen_example_stores() -> entity_logic:data().
gen_example_stores() -> lists:map(fun(_) ->
    jsonable_record:to_json(#atm_store_schema{
        id = ozt_atm:gen_example_id(),
        name = ozt_atm:gen_example_name(),
        description = ozt_atm:gen_example_description(),
        type = ozt_atm:gen_example_store_type(),
        data_spec = ozt_atm:gen_example_data_spec(),
        requires_initial_value = ?RAND_BOOL()
    }, atm_store_schema)
end, lists:seq(1, ?RAND_INT(0, 5))).


-spec gen_example_lanes([od_atm_lambda:id()], [automation:id()]) -> entity_logic:data().
gen_example_lanes(AtmLambdas, StoreSchemaIds) -> lists:map(fun(_) ->
    jsonable_record:to_json(#atm_lane_schema{
        id = ozt_atm:gen_example_id(),
        name = ozt_atm:gen_example_name(),
        parallel_boxes = lists_utils:random_sublist(gen_example_parallel_boxes(AtmLambdas, StoreSchemaIds)),
        store_iterator_spec = gen_example_store_iterator_spec()
    }, atm_lane_schema)
end, lists:seq(1, ?RAND_INT(0, 5))).


-spec gen_example_parallel_boxes([od_atm_lambda:id()], [automation:id()]) -> entity_logic:data().
gen_example_parallel_boxes(AtmLambdas, StoreSchemaIds) -> lists:map(fun(_) ->
    #atm_parallel_box_schema{
        id = ozt_atm:gen_example_id(),
        name = ozt_atm:gen_example_name(),
        tasks = lists_utils:random_sublist(gen_example_tasks(AtmLambdas, StoreSchemaIds))
    }
end, lists:seq(1, ?RAND_INT(0, 5))).


-spec gen_example_tasks([od_atm_lambda:id()], [automation:id()]) -> entity_logic:data().
gen_example_tasks(AtmLambdas, StoreSchemaIds) -> lists:map(fun(_) ->
    AtmLambdaId = lists_utils:random_element(AtmLambdas),
    #atm_task_schema{
        id = ozt_atm:gen_example_id(),
        name = ozt_atm:gen_example_name(),
        lambda_id = AtmLambdaId,
        argument_mappings = lists_utils:random_sublist(gen_example_argument_mappings(AtmLambdaId, StoreSchemaIds)),
        result_mappings = lists_utils:random_sublist(gen_example_result_mappings(AtmLambdaId, StoreSchemaIds))
    }
end, lists:seq(1, ?RAND_INT(0, 5))).


-spec gen_example_argument_mappings(od_atm_lambda:id(), [automation:id()]) -> entity_logic:data().
gen_example_argument_mappings(AtmLambdaId, StoreSchemaIds) ->
    #od_atm_lambda{argument_specs = ArgumentSpecs} = ozt_atm_lambdas:get(AtmLambdaId),
    case ArgumentSpecs of
        [] ->
            [];
        _ ->
            lists:map(fun(_) ->
                ReferencedArgumentSpec = lists_utils:random_element(ArgumentSpecs),
                #atm_task_schema_argument_mapper{
                    argument_name = ReferencedArgumentSpec#atm_lambda_argument_spec.name,
                    value_builder = gen_example_argument_value_builder(StoreSchemaIds)
                }
            end, lists:seq(1, ?RAND_INT(0, 5)))
    end.


-spec gen_example_result_mappings(od_atm_lambda:id(), [automation:id()]) -> entity_logic:data().
gen_example_result_mappings(AtmLambdaId, StoreSchemaIds) ->
    #od_atm_lambda{result_specs = ResultSpecs} = ozt_atm_lambdas:get(AtmLambdaId),
    case {ResultSpecs, StoreSchemaIds} of
        {[], _} ->
            [];
        {_, []} ->
            [];
        _ ->
            lists:map(fun(_) ->
                ReferencedResultSpec = lists_utils:random_element(ResultSpecs),
                #atm_task_schema_result_mapper{
                    result_name = ReferencedResultSpec#atm_lambda_result_spec.name,
                    store_schema_id = lists_utils:random_element(StoreSchemaIds),
                    dispatch_function = lists_utils:random_element([add, remove, set, append, prepend])
                }
            end, lists:seq(1, ?RAND_INT(0, 5)))
    end.


-spec gen_example_argument_value_builder([automation:id()]) -> atm_argument_value_builder:record().
gen_example_argument_value_builder(StoreSchemaIds) ->
    case rand:uniform(5) of
        1 -> #atm_argument_value_builder{
            type = iterated_item, recipe = lists_utils:random_element([
                undefined,
                lists_utils:random_sublist(["key1", "key2", "key3", 0, 1, 2])
            ])
        };
        2 -> #atm_argument_value_builder{
            type = const, recipe = lists_utils:random_element([?RAND_STR(), 0, 151, 27.8])
        };
        3 -> #atm_argument_value_builder{
            type = object, recipe = maps:from_list(lists:map(fun(_) ->
                {?RAND_STR(), gen_example_argument_value_builder(StoreSchemaIds)}
            end, lists:seq(1, rand:uniform(7))))
        };
        4 ->
            % generated list of stores may be empty, in such case keep generating
            % until the is no store_credentials builder type included
            case StoreSchemaIds of
                [] ->
                    gen_example_argument_value_builder(StoreSchemaIds);
                _ ->
                    #atm_argument_value_builder{
                        type = store_credentials, recipe = lists_utils:random_element(StoreSchemaIds)
                    }
            end;
        5 -> #atm_argument_value_builder{
            type = onedatafs_credentials, recipe = undefined
        }
    end.


-spec gen_example_store_iterator_spec() -> atm_store_iterator_spec:record().
gen_example_store_iterator_spec() ->
    lists_utils:random_element([
        #atm_store_iterator_spec{
            store_schema_id = ?RAND_STR(),
            strategy = #atm_store_iterator_serial_strategy{}
        },
        #atm_store_iterator_spec{
            store_schema_id = ?RAND_STR(),
            strategy = #atm_store_iterator_batch_strategy{size = ?RAND_INT(1, 1000)}
        }
    ]).
