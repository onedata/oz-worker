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
-export([update/2]).
-export([add_revision/3, try_add_revision/4]).
-export([delete/1]).
-export([get_atm_inventories/1]).
-export([get_atm_workflow_schemas/1]).
-export([link_to_inventory/2]).
-export([unlink_from_inventory/2]).
-export([dump_to_json/1, dump_to_json/2, dump_to_json/3]).
-export([get_largest_revision_number/1, get_revision_with_largest_number/1]).
-export([find_duplicate/3]).
-export([substitute_lambdas_for_duplicates/2, substitute_lambdas_for_duplicates/3]).
-export([default_resource_spec/0]).
%% Example data generation
-export([example_data_json/0]).
-export([example_revision_json/0]).
-export([example_argument_spec_json/0, example_argument_spec_json/2]).
-export([example_result_spec_json/0]).

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
    AllRevisionNumbers = ?RAND_SUBLIST(lists:seq(1, 100), 1, 3),
    AtmLambdaId = create(AtmInventoryId, #{
        <<"revision">> => #{
            <<"originalRevisionNumber">> => hd(AllRevisionNumbers),
            <<"atmLambdaRevision">> => example_revision_json()
        }
    }),
    lists:foreach(fun(RevisionNumber) ->
        add_revision(AtmLambdaId, RevisionNumber, #{
            <<"originalRevisionNumber">> => RevisionNumber,
            <<"atmLambdaRevision">> => example_revision_json()
        })
    end, tl(AllRevisionNumbers)),
    AtmLambdaId.

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


-spec update(od_atm_lambda:id(), entity_logic:data()) -> boolean().
update(AtmLambdaId, Data) ->
    ?assertMatch(ok, ozt:rpc(atm_lambda_logic, update, [?ROOT, AtmLambdaId, Data])).


-spec add_revision(od_atm_lambda:id(), atm_lambda_revision:revision_number(), entity_logic:data()) -> boolean().
add_revision(AtmLambdaId, RevisionNumber, Data) ->
    ?assertMatch(ok, try_add_revision(?ROOT, AtmLambdaId, RevisionNumber, Data)).


-spec try_add_revision(aai:auth(), od_atm_lambda:id(), atm_lambda_revision:revision_number(), entity_logic:data()) -> boolean().
try_add_revision(Auth, AtmLambdaId, RevisionNumber, Data) ->
    ozt:rpc(atm_lambda_logic, add_revision, [Auth, AtmLambdaId, RevisionNumber, Data]).


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


-spec dump_to_json(od_atm_lambda:id()) ->
    json_utils:json_term().
dump_to_json(AtmLambdaId) when is_binary(AtmLambdaId) ->
    dump_to_json(AtmLambdaId, get(AtmLambdaId)).

-spec dump_to_json(
    od_atm_lambda:id(),
    od_atm_lambda:record() | atm_lambda_revision:revision_number()
) ->
    json_utils:json_term().
dump_to_json(AtmLambdaId, IncludedRevisionNumber) when is_integer(IncludedRevisionNumber) ->
    dump_to_json(AtmLambdaId, get(AtmLambdaId), IncludedRevisionNumber);
dump_to_json(AtmLambdaId, AtmLambda) ->
    case get_largest_revision_number(AtmLambda) of
        undefined ->
            error(badarg);
        LatestRevisionNumber ->
            dump_to_json(AtmLambdaId, AtmLambda, LatestRevisionNumber)
    end.

-spec dump_to_json(
    od_atm_lambda:id(),
    od_atm_lambda:record(),
    atm_lambda_revision:revision_number()
) ->
    json_utils:json_term().
dump_to_json(AtmLambdaId, AtmLambda, IncludedRevisionNumber) ->
    ozt:rpc(od_atm_lambda, dump_to_json, [
        AtmLambdaId, AtmLambda, IncludedRevisionNumber
    ]).


-spec get_largest_revision_number(od_atm_lambda:id() | od_atm_lambda:record()) ->
    undefined | atm_lambda_revision:revision_number().
get_largest_revision_number(AtmLambdaId) when is_binary(AtmLambdaId) ->
    get_largest_revision_number(get(AtmLambdaId));
get_largest_revision_number(#od_atm_lambda{revision_registry = RevisionRegistry}) ->
    case atm_lambda_revision_registry:get_all_revision_numbers(RevisionRegistry) of
        [] -> undefined;
        AllRevisionNumbers -> lists:max(AllRevisionNumbers)
    end.


-spec get_revision_with_largest_number(od_atm_lambda:id() | od_atm_lambda:record()) ->
    atm_lambda_revision:record().
get_revision_with_largest_number(AtmLambdaId) when is_binary(AtmLambdaId) ->
    get_revision_with_largest_number(get(AtmLambdaId));
get_revision_with_largest_number(#od_atm_lambda{revision_registry = RevisionRegistry} = AtmLambda) ->
    case get_largest_revision_number(AtmLambda) of
        undefined ->
            error(badarg);
        RevisionNumber ->
            atm_lambda_revision_registry:get_revision(RevisionNumber, RevisionRegistry)
    end.


-spec find_duplicate(
    od_atm_lambda:id(),
    atm_lambda_revision:revision_number() | [atm_lambda_revision:revision_number()],
    od_atm_inventory:id()
) ->
    {ok, od_atm_lambda:id()} | false.
find_duplicate(OriginalAtmLambdaId, RevisionNumber, TargetAtmInventoryId) when is_integer(RevisionNumber) ->
    find_duplicate(OriginalAtmLambdaId, [RevisionNumber], TargetAtmInventoryId);
find_duplicate(_OriginalAtmLambdaId, [], _TargetAtmInventoryId) ->
    error(badarg);
find_duplicate(OriginalAtmLambdaId, RevisionNumbers, TargetAtmInventoryId) when is_list(RevisionNumbers) ->
    OriginalAtmLambda = get(OriginalAtmLambdaId),
    InventoryLambdas = ozt_atm_inventories:get_atm_lambdas(TargetAtmInventoryId),
    lists_utils:searchmap(fun(CurrentAtmLambdaId) ->
        CurrentAtmLambda = get(CurrentAtmLambdaId),
        HasDuplicatesOfAllRevisions = lists:all(fun(RevisionNumber) ->
            #atm_lambda_revision{checksum = OriginalChecksum} = atm_lambda_revision_registry:get_revision(
                RevisionNumber, OriginalAtmLambda#od_atm_lambda.revision_registry
            ),
            case atm_lambda_revision_registry:has_revision(RevisionNumber, CurrentAtmLambda#od_atm_lambda.revision_registry) of
                false ->
                    false;
                true ->
                    case atm_lambda_revision_registry:get_revision(RevisionNumber, CurrentAtmLambda#od_atm_lambda.revision_registry) of
                        #atm_lambda_revision{checksum = OriginalChecksum} ->
                            true;
                        _ ->
                            false
                    end
            end
        end, RevisionNumbers),
        case HasDuplicatesOfAllRevisions of
            false -> false;
            true -> {true, CurrentAtmLambdaId}
        end
    end, InventoryLambdas -- [OriginalAtmLambdaId]).


-spec substitute_lambdas_for_duplicates(
    atm_workflow_schema_revision:lambda_references(),
    od_atm_inventory:id()
) ->
    atm_workflow_schema_revision:lambda_references().
substitute_lambdas_for_duplicates(OriginalLambdaReferences, TargetAtmInventoryId) ->
    substitute_lambdas_for_duplicates(
        OriginalLambdaReferences, maps:keys(OriginalLambdaReferences), TargetAtmInventoryId
    ).


-spec substitute_lambdas_for_duplicates(
    atm_workflow_schema_revision:lambda_references(),
    [od_atm_lambda:id()],
    od_atm_inventory:id()
) ->
    atm_workflow_schema_revision:lambda_references().
substitute_lambdas_for_duplicates(OriginalLambdaReferences, AtmLambdasToSubstitute, TargetAtmInventoryId) ->
    maps_utils:map_key_value(fun(OriginalAtmLambdaId, ReferencedRevisionNumbers) ->
        case lists:member(OriginalAtmLambdaId, AtmLambdasToSubstitute) of
            false ->
                {OriginalAtmLambdaId, ReferencedRevisionNumbers};
            true ->
                {ok, DuplicateAtmLambdaId} = ozt_atm_lambdas:find_duplicate(
                    OriginalAtmLambdaId, ReferencedRevisionNumbers, TargetAtmInventoryId
                ),
                {DuplicateAtmLambdaId, ReferencedRevisionNumbers}
        end
    end, OriginalLambdaReferences).


-spec default_resource_spec() -> atm_resource_spec:record().
default_resource_spec() ->
    ozt:rpc(od_atm_lambda, default_resource_spec, []).

%%%===================================================================
%%% Example data generation
%%%===================================================================

-spec example_data_json() -> entity_logic:data().
example_data_json() ->
    #{
        <<"revision">> => #{
            <<"originalRevisionNumber">> => ?RAND_INT(1, 100),
            <<"atmLambdaRevision">> => example_revision_json()
        }
    }.


-spec example_revision_json() -> entity_logic:data().
example_revision_json() ->
    jsonable_record:to_json(atm_test_utils:example_lambda_revision(), atm_lambda_revision).


-spec example_argument_spec_json() -> json_utils:json_term().
example_argument_spec_json() ->
    jsonable_record:to_json(atm_test_utils:example_argument_spec(), atm_lambda_argument_spec).

-spec example_argument_spec_json(atm_data_spec:record(), term()) -> json_utils:json_term().
example_argument_spec_json(DataSpec, DefaultValue) ->
    jsonable_record:to_json(atm_test_utils:example_argument_spec(DataSpec, DefaultValue), atm_lambda_argument_spec).


-spec example_result_spec_json() -> json_utils:json_term().
example_result_spec_json() ->
    jsonable_record:to_json(atm_test_utils:example_result_spec(), atm_lambda_result_spec).
