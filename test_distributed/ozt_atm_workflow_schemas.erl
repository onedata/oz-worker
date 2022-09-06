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
-export([update/3]).
-export([try_update/3]).
-export([insert_revision/3, try_insert_revision/4]).
-export([delete/1]).
-export([dump_to_json/1, dump_to_json/2, dump_to_json/3]).
-export([get_largest_revision_number/1]).
-export([extract_referenced_atm_lambda_ids/1]).
-export([update_revision_with/3]).
-export([substitute_atm_lambdas_for_duplicates/2, substitute_atm_lambdas_for_duplicates/3]).
%% Example data generation
-export([example_data_json/1]).
-export([example_revision_json/1]).
-export([example_revision_with_nonempty_tasks_json/1]).
-export([example_store_schema_json/0, example_store_schema_json/1, example_store_schema_json/3]).
-export([example_lane_schema_with_parallel_boxes_json/2, example_lane_schema_json/2, example_lane_schemas_json/2]).
-export([example_parallel_box_schema/2, example_parallel_box_schemas/2]).
-export([example_task_schema/2, example_task_schemas/2]).

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
    AtmWorkflowSchemaId = create(AtmInventoryId, #{
        <<"name">> => atm_test_utils:example_name(),
        <<"summary">> => atm_test_utils:example_summary()
    }),
    lists:foreach(fun(RevisionNumber) ->
        insert_revision(AtmWorkflowSchemaId, RevisionNumber, #{
            <<"originalRevisionNumber">> => RevisionNumber,
            <<"atmWorkflowSchemaRevision">> => example_revision_json(AtmInventoryId)
        })
    end, lists_utils:random_sublist(lists:seq(1, 100), 1, 5)),
    AtmWorkflowSchemaId.

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


-spec update(aai:auth(), od_atm_workflow_schema:id(), entity_logic:data()) -> ok | errors:error().
update(Auth, AtmWorkflowSchemaId, Data) ->
    ?assertMatch(ok, try_update(Auth, AtmWorkflowSchemaId, Data)).


-spec try_update(aai:auth(), od_atm_workflow_schema:id(), entity_logic:data()) -> ok | errors:error().
try_update(Auth, AtmWorkflowSchemaId, Data) ->
    ozt:rpc(atm_workflow_schema_logic, update, [Auth, AtmWorkflowSchemaId, Data]).


-spec insert_revision(
    od_atm_workflow_schema:id(),
    atm_workflow_schema_revision:revision_number() | binary(),
    entity_logic:data()
) ->
    ok | errors:error().
insert_revision(AtmWorkflowSchemaId, RevisionNumber, Data) ->
    ?assertEqual(ok, try_insert_revision(?ROOT, AtmWorkflowSchemaId, RevisionNumber, Data)).


-spec try_insert_revision(
    aai:auth(),
    od_atm_workflow_schema:id(),
    atm_workflow_schema_revision:revision_number() | binary(),
    entity_logic:data()
) ->
    ok | errors:error().
try_insert_revision(Auth, AtmWorkflowSchemaId, RevisionNumber, Data) ->
    ozt:rpc(atm_workflow_schema_logic, insert_revision, [Auth, AtmWorkflowSchemaId, RevisionNumber, Data]).


-spec exists(od_atm_workflow_schema:id()) -> boolean().
exists(AtmWorkflowSchemaId) ->
    ozt:rpc(atm_workflow_schema_logic, exists, [AtmWorkflowSchemaId]).


-spec delete(od_atm_workflow_schema:id()) -> ok.
delete(AtmWorkflowSchemaId) ->
    ?assertMatch(ok, ozt:rpc(atm_workflow_schema_logic, delete, [?ROOT, AtmWorkflowSchemaId])).


-spec dump_to_json(od_atm_workflow_schema:id()) ->
    json_utils:json_term().
dump_to_json(AtmWorkflowSchemaId) when is_binary(AtmWorkflowSchemaId) ->
    dump_to_json(AtmWorkflowSchemaId, get(AtmWorkflowSchemaId)).

-spec dump_to_json(
    od_atm_workflow_schema:id(),
    od_atm_workflow_schema:record() | atm_workflow_schema_revision:revision_number()
) ->
    json_utils:json_term().
dump_to_json(AtmWorkflowSchemaId, IncludedRevisionNumber) when is_integer(IncludedRevisionNumber) ->
    dump_to_json(AtmWorkflowSchemaId, get(AtmWorkflowSchemaId), IncludedRevisionNumber);
dump_to_json(AtmWorkflowSchemaId, AtmWorkflowSchema) ->
    case get_largest_revision_number(AtmWorkflowSchema) of
        undefined ->
            error(badarg);
        LatestRevisionNumber ->
            dump_to_json(AtmWorkflowSchemaId, AtmWorkflowSchema, LatestRevisionNumber)
    end.

-spec dump_to_json(
    od_atm_workflow_schema:id(),
    od_atm_workflow_schema:record(),
    atm_workflow_schema_revision:revision_number()
) ->
    json_utils:json_term().
dump_to_json(AtmWorkflowSchemaId, AtmWorkflowSchema, IncludedRevisionNumber) ->
    ozt:rpc(od_atm_workflow_schema, dump_to_json, [
        AtmWorkflowSchemaId, AtmWorkflowSchema, IncludedRevisionNumber
    ]).


-spec get_largest_revision_number(od_atm_workflow_schema:id() | od_atm_workflow_schema:record()) ->
    undefined | atm_workflow_schema_revision:revision_number().
get_largest_revision_number(AtmWorkflowSchemaId) when is_binary(AtmWorkflowSchemaId) ->
    get_largest_revision_number(get(AtmWorkflowSchemaId));
get_largest_revision_number(#od_atm_workflow_schema{revision_registry = RevisionRegistry}) ->
    case atm_workflow_schema_revision_registry:get_all_revision_numbers(RevisionRegistry) of
        [] -> undefined;
        AllRevisionNumbers -> lists:max(AllRevisionNumbers)
    end.


-spec extract_referenced_atm_lambda_ids(json_utils:json_map() | atm_workflow_schema_revision:record()) ->
    [od_atm_lambda:id()].
extract_referenced_atm_lambda_ids(Json) when is_map(Json) ->
    extract_referenced_atm_lambda_ids(jsonable_record:from_json(Json, atm_workflow_schema_revision));
extract_referenced_atm_lambda_ids(AtmWorkflowSchemaRevision) ->
    maps:keys(atm_workflow_schema_revision:extract_atm_lambda_references(AtmWorkflowSchemaRevision)).


-spec update_revision_with(
    od_atm_workflow_schema:record(),
    atm_workflow_schema_revision:revision_number(),
    fun((atm_workflow_schema_revision:record()) -> atm_workflow_schema_revision:record())
) ->
    od_atm_workflow_schema:record().
update_revision_with(#od_atm_workflow_schema{
    revision_registry = RevisionRegistry
} = AtmWorkflowSchema, RevisionNumber, Diff) ->
    PreviousRevision = atm_workflow_schema_revision_registry:get_revision(RevisionNumber, RevisionRegistry),
    AtmWorkflowSchema#od_atm_workflow_schema{
        revision_registry = atm_workflow_schema_revision_registry:insert_revision(
            RevisionNumber,
            Diff(PreviousRevision),
            RevisionRegistry
        )
    }.


% Tries to find duplicates of lambdas that belong to given inventory and replaces
% their counterparts in tasks.
-spec substitute_atm_lambdas_for_duplicates(
    atm_workflow_schema_revision:record(), od_atm_inventory:id()
) ->
    atm_workflow_schema_revision:record().
substitute_atm_lambdas_for_duplicates(AtmWorkflowSchemaRevision, TargetAtmInventoryId) ->
    substitute_atm_lambdas_for_duplicates(
        AtmWorkflowSchemaRevision,
        atm_workflow_schema_revision:extract_atm_lambda_references(AtmWorkflowSchemaRevision),
        TargetAtmInventoryId
    ).

-spec substitute_atm_lambdas_for_duplicates(
    atm_workflow_schema_revision:record(), atm_workflow_schema_revision:atm_lambda_references(), od_atm_inventory:id()
) ->
    atm_workflow_schema_revision:record().
substitute_atm_lambdas_for_duplicates(AtmWorkflowSchemaRevision, AtmLambdaReferencesToSubstitute, TargetAtmInventoryId) ->
    atm_workflow_schema_revision:map_tasks(fun(Task = #atm_task_schema{
        lambda_id = OriginalAtmLambdaId,
        lambda_revision_number = RevisionNumber
    }) ->
        case maps:is_key(OriginalAtmLambdaId, AtmLambdaReferencesToSubstitute) of
            true ->
                {ok, DuplicateAtmLambdaId} = ?assertMatch({ok, _}, ozt_atm_lambdas:find_duplicate(
                    OriginalAtmLambdaId, RevisionNumber, TargetAtmInventoryId
                )),
                Task#atm_task_schema{lambda_id = DuplicateAtmLambdaId};
            false ->
                Task
        end
    end, AtmWorkflowSchemaRevision).

%%%===================================================================
%%% Example data generation
%%%===================================================================

-spec example_data_json(od_atm_inventory:id()) -> entity_logic:data().
example_data_json(AtmInventoryId) when is_binary(AtmInventoryId) ->
    #{
        <<"name">> => atm_test_utils:example_name(),
        <<"summary">> => atm_test_utils:example_summary(),

        <<"revision">> => #{
            <<"originalRevisionNumber">> => ?RAND_INT(1, 100),
            <<"atmWorkflowSchemaRevision">> => example_revision_json(AtmInventoryId)
        }
    }.


-spec example_revision_json(od_atm_inventory:id() | [od_atm_lambda:id()]) -> entity_logic:data().
example_revision_json(AtmInventoryId) when is_binary(AtmInventoryId) ->
    % make sure there are some lambdas defined in the inventory
    AtmLambdas = case ozt_atm_inventories:get_atm_lambdas(AtmInventoryId) of
        [] ->
            lists_utils:generate(fun() ->
                ozt_atm_lambdas:create(AtmInventoryId)
            end, ?RAND_INT(1, 4)),
            ozt_atm_inventories:get_atm_lambdas(AtmInventoryId);
        List ->
            List
    end,
    example_revision_json(AtmLambdas);
example_revision_json(AtmLambdas) when is_list(AtmLambdas) ->
    StoreSchemas = atm_test_utils:example_store_schemas(),
    StoreSchemasJson = jsonable_record:list_to_json(StoreSchemas, atm_store_schema),
    #{
        <<"description">> => atm_test_utils:example_description(),
        <<"state">> => automation:lifecycle_state_to_json(atm_test_utils:example_lifecycle_state()),
        <<"stores">> => StoreSchemasJson,
        <<"lanes">> => example_lane_schemas_json(AtmLambdas, StoreSchemas),
        <<"dashboardSpec">> => example_dashboard_spec_json()
    }.


-spec example_revision_with_nonempty_tasks_json(od_atm_inventory:id() | atom()) -> entity_logic:data().
example_revision_with_nonempty_tasks_json(AtmInventoryId) when is_binary(AtmInventoryId) ->
    Candidate = example_revision_json(AtmInventoryId),
    DecodedCandidate = jsonable_record:from_json(Candidate, atm_workflow_schema_revision),
    case atm_workflow_schema_revision:extract_atm_lambda_references(DecodedCandidate) of
        EmptyMap when map_size(EmptyMap) == 0 ->
            example_revision_with_nonempty_tasks_json(AtmInventoryId);
        _ ->
            Candidate
    end.


-spec example_store_schema_json() -> entity_logic:data().
example_store_schema_json() ->
    jsonable_record:to_json(atm_test_utils:example_store_schema(), atm_store_schema).

-spec example_store_schema_json(automation:store_type()) -> entity_logic:data().
example_store_schema_json(StoreType) ->
    jsonable_record:to_json(atm_test_utils:example_store_schema(StoreType), atm_store_schema).

-spec example_store_schema_json(automation:store_type(), atm_data_spec:record(), term()) -> entity_logic:data().
example_store_schema_json(StoreType, StoreConfig, DefaultInitialContent) ->
    jsonable_record:to_json(
        atm_test_utils:example_store_schema(StoreType, StoreConfig, DefaultInitialContent),
        atm_store_schema
    ).


-spec example_lane_schema_with_parallel_boxes_json([atm_parallel_box_schema:record()], [atm_store_schema:record()]) ->
    entity_logic:data().
example_lane_schema_with_parallel_boxes_json(ParallelBoxes, StoreSchemas) ->
    ViableStoreSchemas = [S || S <- StoreSchemas, S#atm_store_schema.type /= time_series, S#atm_store_schema.type /= audit_log],
    ViableStoreSchemas == [] andalso error(no_viable_stores),
    ViableStoreSchemaIds = [S#atm_store_schema.id || S <- ViableStoreSchemas],
    jsonable_record:to_json(
        atm_test_utils:example_lane_schema_with_parallel_boxes(ParallelBoxes, ViableStoreSchemaIds),
        atm_lane_schema
    ).

-spec example_lane_schema_json([od_atm_lambda:id()], [atm_store_schema:record()]) -> entity_logic:data().
example_lane_schema_json(_AtmLambdas, []) ->
    error(empty_lambda_list);
example_lane_schema_json([], _StoreSchemas) ->
    error(empty_store_schema_list);
example_lane_schema_json(AtmLambdas, StoreSchemas) ->
    StoreSchemaIds = [S#atm_store_schema.id || S <- StoreSchemas],
    ParallelBoxes = lists_utils:random_sublist(example_parallel_box_schemas(AtmLambdas, StoreSchemaIds)),
    example_lane_schema_with_parallel_boxes_json(ParallelBoxes, StoreSchemas).


-spec example_lane_schemas_json([od_atm_lambda:id()], [atm_store_schema:record()]) -> entity_logic:data().
example_lane_schemas_json(_AtmLambdas, []) ->
    [];
example_lane_schemas_json([], _StoreSchemas) ->
    [];
example_lane_schemas_json(AtmLambdas, StoreSchemas) ->
    lists_utils:generate(fun() ->
        example_lane_schema_json(AtmLambdas, StoreSchemas)
    end, ?RAND_INT(0, 4)).


-spec example_parallel_box_schema([od_atm_lambda:id()], [automation:id()]) -> atm_parallel_box_schema:record().
example_parallel_box_schema(AtmLambdas, StoreSchemaIds) ->
    atm_test_utils:example_parallel_box_schema_with_tasks(
        lists_utils:random_sublist(example_task_schemas(AtmLambdas, StoreSchemaIds))
    ).

-spec example_parallel_box_schemas([od_atm_lambda:id()], [automation:id()]) -> [atm_parallel_box_schema:record()].
example_parallel_box_schemas(AtmLambdas, StoreSchemaIds) ->
    lists_utils:generate(fun() ->
        example_parallel_box_schema(AtmLambdas, StoreSchemaIds)
    end, ?RAND_INT(0, 4)).


-spec example_task_schema([od_atm_lambda:id()], [automation:id()]) -> atm_task_schema:record().
example_task_schema(AtmLambdas, StoreSchemaIds) ->
    atm_test_utils:example_task_schema(build_lambda_registries(AtmLambdas), StoreSchemaIds).


-spec example_task_schemas([od_atm_lambda:id()], [automation:id()]) -> [atm_task_schema:record()].
example_task_schemas(AtmLambdas, StoreSchemaIds) ->
    atm_test_utils:example_task_schemas(build_lambda_registries(AtmLambdas), StoreSchemaIds).

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
-spec build_lambda_registries([od_atm_lambda:id()]) -> atm_test_utils:lambda_registries().
build_lambda_registries(AtmLambdas) ->
    maps_utils:generate_from_list(fun(AtmLambdaId) ->
        {AtmLambdaId, (ozt_atm_lambdas:get(AtmLambdaId))#od_atm_lambda.revision_registry}
    end, AtmLambdas).


%% @private
-spec example_dashboard_spec_json() -> json_utils:json_term().
example_dashboard_spec_json() ->
    % generate an example once and reuse it to avoid overheads - repetitive generation could
    % significantly slow down the tests
    {ok, Example} = node_cache:acquire({?MODULE, ?FUNCTION_NAME}, fun() ->
        {ok, jsonable_record:to_json(time_series_test_utils:example_dashboard_spec()), infinity}
    end),
    Example.
