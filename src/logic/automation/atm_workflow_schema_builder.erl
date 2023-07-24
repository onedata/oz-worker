%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles management of workflow schema revisions, performing
%%% validation as well as transformations required due to linking and creation
%%% of supplementary lambdas (if provided).
%%%
%%% When a new workflow schema revision is submitted (either a new one, or an
%%% update of the existing one), this module handles verification of the request,
%%% validation of the schema and resolution of all referenced lambdas. Especially
%%% when the revision is created from a dump, it includes the "supplementaryAtmLambdas"
%%% field, which provides self-contained definitions of all lambdas referenced by the
%%% workflow schema revision. This field may be used to link or create missing lambdas
%%% according to the following procedure (applied for each lambda):
%%%
%%%   1. If the original referenced lambda is available in the target inventory,
%%%      it is used and the procedure finishes.
%%%
%%%   2. If there is a duplicate of the referenced lambda in the target inventory,
%%%      it is used [*] and the procedure finishes.
%%%
%%%   3. If the original referenced lambda can be linked to the target inventory,
%%%      it is linked and used and the procedure finishes.
%%%
%%%   4. If there is a duplicate of the referenced lambda in any user's inventory
%%%      that can be linked to the target inventory,
%%%      it is linked and used [*] and the procedure finishes.
%%%
%%%   5. Finally, the lambda is duplicated and the duplicate is used.
%%%
%%% [*] - if a suitable duplicate is found, but it is missing the referenced lambda revision,
%%%       the missing revision is created.
%%%
%%% All steps of the procedure take into account if the user
%%% has required privileges to do corresponding actions.
%%%
%%% Consult functions 'resolve_referenced_lambdas' and 'resolve_missing_lambdas' that
%%% implement the above-mentioned mechanisms.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_workflow_schema_builder).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/logging.hrl").

-export([insert_revision/4]).
-export([delete_revision/2]).
-export([delete_all_revisions/1]).

-type revision_definitions() :: #{atm_lambda_revision:revision_number() => entity_logic:data()}.

-record(builder_ctx, {
    auth :: aai:auth(),
    atm_workflow_schema_revision :: atm_workflow_schema_revision:record(),
    target_atm_inventory_id :: od_atm_inventory:id(),
    target_atm_inventory = undefined :: undefined | od_atm_inventory:record(),
    can_manage_lambdas_in_target_inventory = false :: boolean(),
    % During workflow schema creation, one can supply additional information
    % including full definitions of lambdas referenced by tasks in the schema.
    % This way, in case any lambdas are missing or unavailable for the creating
    % client, they can be created for the user. This is especially useful when
    % copying workflow schemas between zones or users that do not belong to the
    % same inventories.
    supplementary_lambdas = #{} :: #{od_atm_lambda:id() => entity_logic:data()},
    lambdas_to_link = [] :: [od_atm_lambda:id()],
    lambda_revisions_to_add = #{} :: #{od_atm_lambda:id() => revision_definitions()},
    % this map contains original lambda ids that are only used for cross-referencing with the
    % schema, they are substituted with actual lambda ids after they are created
    lambdas_to_duplicate = #{} :: #{od_atm_lambda:id() => revision_definitions()}
}).
-type builder_ctx() :: #builder_ctx{}.

-record(lambda_search_ctx, {
    original_lambda_id :: od_atm_lambda:id(),
    revision_number :: atm_lambda_revision:revision_number(),
    checksum :: undefined | atm_lambda_revision:checksum(),
    json_dump :: undefined | entity_logic:data()
}).
-type lambda_search_ctx() :: #lambda_search_ctx{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec insert_revision(
    aai:auth(),
    od_atm_workflow_schema:id(),
    atm_workflow_schema_revision:revision_number(),
    entity_logic:data()
) ->
    ok | errors:error().
insert_revision(Auth, AtmWorkflowSchemaId, RevisionNumber, Data) ->
    od_atm_workflow_schema:critical_section(AtmWorkflowSchemaId, fun() ->
        try
            {ok, #document{value = PreviousAtmWorkflowSchema}} = od_atm_workflow_schema:get(AtmWorkflowSchemaId),

            SupplementaryAtmLambdas = maps:get(<<"supplementaryAtmLambdas">>, Data, #{}),
            AtmWorkflowSchemaRevision = maps:get(<<"atmWorkflowSchemaRevision">>, Data),

            FinalBuilderCtx = validate_revision_and_resolve_lambdas(#builder_ctx{
                auth = Auth,
                atm_workflow_schema_revision = AtmWorkflowSchemaRevision,
                target_atm_inventory_id = PreviousAtmWorkflowSchema#od_atm_workflow_schema.atm_inventory,
                supplementary_lambdas = SupplementaryAtmLambdas
            }),

            update_revision_registry_unsafe(AtmWorkflowSchemaId, PreviousAtmWorkflowSchema, fun(RevisionRegistry) ->
                {ok, atm_workflow_schema_revision_registry:insert_revision(
                    RevisionNumber, FinalBuilderCtx#builder_ctx.atm_workflow_schema_revision, RevisionRegistry
                )}
            end)
        catch
            throw:{error, _} = Error ->
                Error;
            Class:Reason:Stacktrace ->
                ?error_stacktrace(
                    "Unexpected error in ~w:~w - ~w:~p",
                    [?MODULE, ?FUNCTION_NAME, Class, Reason],
                    Stacktrace
                ),
                ?ERROR_INTERNAL_SERVER_ERROR
        end
    end).


-spec delete_revision(od_atm_workflow_schema:id(), atm_workflow_schema_revision:revision_number()) ->
    ok | no_return().
delete_revision(AtmWorkflowSchemaId, RevisionNumber) ->
    od_atm_workflow_schema:critical_section(AtmWorkflowSchemaId, fun() ->
        update_revision_registry_unsafe(AtmWorkflowSchemaId, fun(RevisionRegistry) ->
            case atm_workflow_schema_revision_registry:delete_revision(RevisionNumber, RevisionRegistry) of
                {ok, UpdatedRegistry} ->
                    {ok, UpdatedRegistry};
                error ->
                    ?ERROR_NOT_FOUND
            end
        end)
    end).


-spec delete_all_revisions(od_atm_workflow_schema:id()) ->
    ok | no_return().
delete_all_revisions(AtmWorkflowSchemaId) ->
    od_atm_workflow_schema:critical_section(AtmWorkflowSchemaId, fun() ->
        update_revision_registry_unsafe(AtmWorkflowSchemaId, fun(_) ->
            {ok, atm_workflow_schema_revision_registry:empty()}
        end)
    end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec validate_revision_and_resolve_lambdas(builder_ctx()) -> builder_ctx().
validate_revision_and_resolve_lambdas(Ctx1) ->
    Ctx2 = resolve_referenced_lambdas(Ctx1),
    validate_workflow_schema_revision(Ctx2),
    add_revisions_to_lambdas(Ctx2),
    Ctx3 = duplicate_lambdas(Ctx2),
    link_referenced_lambdas(Ctx3),
    Ctx3.


%% @private
-spec resolve_referenced_lambdas(builder_ctx()) -> builder_ctx().
resolve_referenced_lambdas(BuilderCtx = #builder_ctx{
    auth = Auth,
    atm_workflow_schema_revision = AtmWorkflowSchemaRevision,
    target_atm_inventory_id = AtmInventoryId
}) ->
    {ok, #document{
        value = #od_atm_inventory{
            atm_lambdas = InventoryLambdas
        } = AtmInventory
    }} = od_atm_inventory:get(AtmInventoryId),
    RevisionReferencedLambdas = atm_workflow_schema_revision:extract_atm_lambda_references(AtmWorkflowSchemaRevision),
    case lists_utils:subtract(maps:keys(RevisionReferencedLambdas), InventoryLambdas) of
        [] ->
            BuilderCtx;
        MissingLambdaIds ->
            UpdatedBuilderCtx = BuilderCtx#builder_ctx{
                target_atm_inventory = AtmInventory,
                can_manage_lambdas_in_target_inventory = can_manage_lambdas_in_inventory(Auth, AtmInventory)
            },
            MissingReferencedLambdas = maps:with(MissingLambdaIds, RevisionReferencedLambdas),
            resolve_missing_lambdas(UpdatedBuilderCtx, MissingReferencedLambdas)
    end.


-spec resolve_missing_lambdas(builder_ctx(), #{od_atm_lambda:id() => [atm_lambda_revision:revision_number()]}) ->
    builder_ctx().
resolve_missing_lambdas(BuilderCtx = #builder_ctx{
    supplementary_lambdas = SupplementaryAtmLambdas
}, MissingReferencedLambdas) ->
    maps:fold(fun(AtmLambdaId, ReferencedRevisionNumbers, BuilderCtxAccExternal) ->
        lists:foldl(fun(ReferencedRevisionNumber, BuilderCtxAccInternal) ->
            AtmLambdaData = kv_utils:get(
                [AtmLambdaId, integer_to_binary(ReferencedRevisionNumber)], SupplementaryAtmLambdas, undefined
            ),
            resolve_missing_lambda(BuilderCtxAccInternal, AtmLambdaId, ReferencedRevisionNumber, AtmLambdaData)
        end, BuilderCtxAccExternal, ReferencedRevisionNumbers)
    end, BuilderCtx, MissingReferencedLambdas).


%% @private
-spec resolve_missing_lambda(
    builder_ctx(),
    od_atm_lambda:id(),
    atm_lambda_revision:revision_number(),
    undefined | entity_logic:data()
) ->
    builder_ctx().
resolve_missing_lambda(BuilderCtx, AtmLambdaId, RevisionNumber, AtmLambdaData) ->
    LambdaSearchCtx = build_lambda_search_ctx(AtmLambdaId, RevisionNumber, AtmLambdaData),
    SearchResult = lists_utils:searchmap(fun(Fun) -> Fun(BuilderCtx, LambdaSearchCtx) end, [
        fun try_reusing_lambda_duplicate_in_current_inventory/2,
        fun try_linking_referenced_lambda/2,
        fun try_linking_any_lambda_duplicate/2,
        fun try_qualifying_lambda_for_duplication/2
    ]),
    case SearchResult of
        {ok, FinalBuilderCtx} ->
            FinalBuilderCtx;
        error ->
            atm_schema_validator:raise_validation_error(
                <<"tasks">>,
                "The lambda id '~s' referenced by one of the tasks was not found or is not "
                "available for the requesting client. Consider providing supplementary lambdas "
                "so that missing ones can be linked or created along with the workflow schema "
                "(however, this requires lambda management privileges in the target inventory).",
                [AtmLambdaId]
            )
    end.


%% @private
-spec build_lambda_search_ctx(od_atm_lambda:id(), atm_lambda_revision:revision_number(), undefined | entity_logic:data()) ->
    lambda_search_ctx().
build_lambda_search_ctx(OriginalAtmLambdaId, RevisionNumber, AtmLambdaData) ->
    #lambda_search_ctx{
        original_lambda_id = OriginalAtmLambdaId,
        revision_number = RevisionNumber,
        checksum = case AtmLambdaData of
            undefined ->
                undefined;
            _ ->
                case maps:get(<<"revision">>, AtmLambdaData, undefined) of
                    #{<<"schemaFormatVersion">> := 3, <<"atmLambdaRevision">> := AtmLambdaRevisionBin} ->
                        kv_utils:get(
                            [<<"_data">>, <<"checksum">>],
                            json_utils:decode(AtmLambdaRevisionBin),
                            undefined
                        );
                    _ ->
                        kv_utils:get(
                            [<<"revision">>, <<"atmLambdaRevision">>, <<"checksum">>],
                            AtmLambdaData,
                            undefined
                        )
                end
        end,
        json_dump = AtmLambdaData
    }.


%% @private
-spec try_reusing_lambda_duplicate_in_current_inventory(builder_ctx(), lambda_search_ctx()) ->
    {true, builder_ctx()} | false.
try_reusing_lambda_duplicate_in_current_inventory(_BuilderCtx, #lambda_search_ctx{checksum = undefined}) ->
    false;
try_reusing_lambda_duplicate_in_current_inventory(BuilderCtx1, LambdaSearchCtx) ->
    case find_lambda_duplicate_in_inventory(BuilderCtx1#builder_ctx.target_atm_inventory, LambdaSearchCtx) of
        {ok, {Qualification, DuplicateLambdaId}} ->
            BuilderCtx2 = substitute_lambda_for_duplicate(BuilderCtx1, LambdaSearchCtx, DuplicateLambdaId),
            case Qualification of
                exact_duplicate ->
                    {true, BuilderCtx2};
                duplicate_without_referenced_revision when BuilderCtx1#builder_ctx.can_manage_lambdas_in_target_inventory ->
                    {true, mark_lambda_revision_for_duplication(BuilderCtx2, LambdaSearchCtx, DuplicateLambdaId)};
                duplicate_without_referenced_revision ->
                    false
            end;
        error ->
            false
    end.


%% @private
-spec try_linking_referenced_lambda(builder_ctx(), lambda_search_ctx()) ->
    {true, builder_ctx()} | false.
try_linking_referenced_lambda(#builder_ctx{can_manage_lambdas_in_target_inventory = false}, _LambdaSearchCtx) ->
    false;
try_linking_referenced_lambda(BuilderCtx, #lambda_search_ctx{original_lambda_id = OriginalAtmLambdaId}) ->
    case can_manage_lambda(BuilderCtx#builder_ctx.auth, OriginalAtmLambdaId) of
        true ->
            {true, mark_lambda_for_linking(BuilderCtx, OriginalAtmLambdaId)};
        false ->
            false
    end.


%% @private
-spec try_linking_any_lambda_duplicate(builder_ctx(), lambda_search_ctx()) ->
    {true, builder_ctx()} | false.
try_linking_any_lambda_duplicate(#builder_ctx{can_manage_lambdas_in_target_inventory = false}, _LambdaSearchCtx) ->
    false;
try_linking_any_lambda_duplicate(_BuilderCtx, #lambda_search_ctx{checksum = undefined}) ->
    false;
try_linking_any_lambda_duplicate(BuilderCtx1, LambdaSearchCtx) ->
    case find_linkable_lambda_duplicate(BuilderCtx1, LambdaSearchCtx) of
        {ok, {Qualification, DuplicateLambdaId}} ->
            BuilderCtx2 = substitute_lambda_for_duplicate(BuilderCtx1, LambdaSearchCtx, DuplicateLambdaId),
            BuilderCtx3 = mark_lambda_for_linking(BuilderCtx2, DuplicateLambdaId),
            case Qualification of
                exact_duplicate ->
                    {true, BuilderCtx3};
                duplicate_without_referenced_revision ->
                    {true, mark_lambda_revision_for_duplication(BuilderCtx3, LambdaSearchCtx, DuplicateLambdaId)}
            end;
        error ->
            false
    end.


%% @private
-spec try_qualifying_lambda_for_duplication(builder_ctx(), lambda_search_ctx()) ->
    {true, builder_ctx()} | false.
try_qualifying_lambda_for_duplication(#builder_ctx{can_manage_lambdas_in_target_inventory = false}, _LambdaSearchCtx) ->
    false;
try_qualifying_lambda_for_duplication(_BuilderCtx, #lambda_search_ctx{json_dump = undefined}) ->
    false;
try_qualifying_lambda_for_duplication(BuilderCtx, LambdaSearchCtx) ->
    {true, mark_lambda_for_duplication(BuilderCtx, LambdaSearchCtx)}.


%% @private
-spec find_linkable_lambda_duplicate(builder_ctx(), lambda_search_ctx()) ->
    {ok, {exact_duplicate | duplicate_without_referenced_revision, od_atm_lambda:id()}} | error.
find_linkable_lambda_duplicate(#builder_ctx{
    auth = ?USER(UserId) = Auth,
    target_atm_inventory_id = TargetAtmInventoryId
}, LambdaSearchCtx) ->
    {ok, EffAtmInventories} = user_logic:get_eff_atm_inventories(Auth, UserId),
    lists_utils:searchmap(fun(AtmInventoryId) ->
        case od_atm_inventory:get(AtmInventoryId) of
            {error, not_found} ->
                false;
            {ok, #document{value = AtmInventory}} ->
                case find_linkable_lambda_duplicate_in_inventory(Auth, AtmInventory, LambdaSearchCtx) of
                    {ok, Result} ->
                        {true, Result};
                    error ->
                        false
                end
        end
    end, EffAtmInventories -- [TargetAtmInventoryId]);
find_linkable_lambda_duplicate(_, _) ->
    error.


%% @private
-spec find_linkable_lambda_duplicate_in_inventory(aai:auth(), od_atm_inventory:record(), lambda_search_ctx()) ->
    {ok, {exact_duplicate | duplicate_without_referenced_revision, od_atm_lambda:id()}} | error.
find_linkable_lambda_duplicate_in_inventory(Auth, AtmInventory, LambdaSearchCtx) ->
    case can_manage_lambdas_in_inventory(Auth, AtmInventory) of
        false ->
            error;
        true ->
            find_lambda_duplicate_in_inventory(AtmInventory, LambdaSearchCtx)
    end.


%% @private
-spec find_lambda_duplicate_in_inventory(od_atm_inventory:record(), lambda_search_ctx()) ->
    {ok, {exact_duplicate | duplicate_without_referenced_revision, od_atm_lambda:id()}} | error.
find_lambda_duplicate_in_inventory(AtmInventory, LambdaSearchCtx) ->
    % prefer lambdas that were created as a duplicate of the original lambda,
    % but fall back to any lambda that has the exact checksum
    case find_lambda_duplicate_in_inventory(by_original_reference, AtmInventory, LambdaSearchCtx) of
        {ok, Result} ->
            {ok, Result};
        error ->
            find_lambda_duplicate_in_inventory(by_checksum, AtmInventory, LambdaSearchCtx)
    end.


%% @private
-spec find_lambda_duplicate_in_inventory(by_original_reference | by_checksum, od_atm_inventory:record(), lambda_search_ctx()) ->
    {ok, {exact_duplicate | duplicate_without_referenced_revision, od_atm_lambda:id()}} | error.
find_lambda_duplicate_in_inventory(Strategy, AtmInventory, LambdaSearchCtx) ->
    lists_utils:searchmap(fun(AtmLambdaId) ->
        case od_atm_lambda:get(AtmLambdaId) of
            {ok, Doc} ->
                qualify_lambda_as_duplicate(Strategy, Doc, LambdaSearchCtx);
            _ ->
                false
        end
    end, AtmInventory#od_atm_inventory.atm_lambdas).


%% @private
-spec qualify_lambda_as_duplicate(by_original_reference | by_checksum, od_atm_lambda:doc(), lambda_search_ctx()) ->
    {true, {exact_duplicate | duplicate_without_referenced_revision, od_atm_lambda:id()}} | false.
qualify_lambda_as_duplicate(by_original_reference, #document{key = AtmLambdaId, value = AtmLambda}, #lambda_search_ctx{
    original_lambda_id = OriginalAtmLambdaId,
    revision_number = RevisionNumber,
    checksum = Checksum
}) ->
    case AtmLambda of
        #od_atm_lambda{original_atm_lambda = OriginalAtmLambdaId, revision_registry = RevisionRegistry} ->
            case atm_lambda_revision_registry:has_revision(RevisionNumber, RevisionRegistry) of
                false ->
                    {true, {duplicate_without_referenced_revision, AtmLambdaId}};
                true ->
                    case atm_lambda_revision_registry:get_revision(RevisionNumber, RevisionRegistry) of
                        #atm_lambda_revision{checksum = Checksum} ->
                            {true, {exact_duplicate, AtmLambdaId}};
                        _ ->
                            false
                    end
            end;
        _ ->
            false
    end;
qualify_lambda_as_duplicate(by_checksum, #document{key = AtmLambdaId, value = AtmLambda}, #lambda_search_ctx{
    revision_number = RevisionNumber, checksum = Checksum
}) ->
    RevisionRegistry = AtmLambda#od_atm_lambda.revision_registry,
    case atm_lambda_revision_registry:has_revision(RevisionNumber, RevisionRegistry) of
        false ->
            false;
        true ->
            case atm_lambda_revision_registry:get_revision(RevisionNumber, RevisionRegistry) of
                #atm_lambda_revision{checksum = Checksum} ->
                    {true, {exact_duplicate, AtmLambdaId}};
                _ ->
                    false
            end
    end.


%% @private
-spec substitute_lambda_for_duplicate(builder_ctx(), lambda_search_ctx(), od_atm_lambda:id()) ->
    builder_ctx().
substitute_lambda_for_duplicate(BuilderCtx, #lambda_search_ctx{
    original_lambda_id = OriginalAtmLambdaId, revision_number = RevisionNumber
}, TargetLambdaId) ->
    substitute_lambda_for_duplicate(BuilderCtx, OriginalAtmLambdaId, RevisionNumber, TargetLambdaId).

%% @private
-spec substitute_lambda_for_duplicate(
    builder_ctx(),
    od_atm_lambda:id(),
    atm_lambda_revision:revision_number(),
    od_atm_lambda:id()
) -> builder_ctx().
substitute_lambda_for_duplicate(BuilderCtx = #builder_ctx{
    atm_workflow_schema_revision = AtmWorkflowSchemaRevision
}, CurrentLambdaId, RevisionNumber, TargetLambdaId) ->
    BuilderCtx#builder_ctx{
        atm_workflow_schema_revision = atm_workflow_schema_revision:map_tasks(fun(AtmTaskSchema) ->
            case AtmTaskSchema of
                #atm_task_schema{lambda_id = CurrentLambdaId, lambda_revision_number = RevisionNumber} ->
                    AtmTaskSchema#atm_task_schema{lambda_id = TargetLambdaId};
                AtmTaskSchema ->
                    AtmTaskSchema
            end
        end, AtmWorkflowSchemaRevision)
    }.


%% @private
-spec mark_lambda_for_duplication(builder_ctx(), lambda_search_ctx()) -> builder_ctx().
mark_lambda_for_duplication(BuilderCtx = #builder_ctx{lambdas_to_duplicate = LambdasToDuplicate}, #lambda_search_ctx{
    original_lambda_id = OriginalAtmLambdaId,
    revision_number = RevisionNumber,
    json_dump = #{<<"revision">> := RevisionData}
}) ->
    BuilderCtx#builder_ctx{
        lambdas_to_duplicate = maps:update_with(OriginalAtmLambdaId, fun(RevisionsToCreate) ->
            RevisionsToCreate#{RevisionNumber => RevisionData}
        end, #{RevisionNumber => RevisionData}, LambdasToDuplicate)
    }.


%% @private
-spec mark_lambda_for_linking(builder_ctx(), od_atm_lambda:id()) ->
    builder_ctx().
mark_lambda_for_linking(BuilderCtx = #builder_ctx{lambdas_to_link = LambdasToLink}, AtmLambdaId) ->
    BuilderCtx#builder_ctx{
        lambdas_to_link = lists_utils:union([AtmLambdaId], LambdasToLink)
    }.


%% @private
-spec mark_lambda_revision_for_duplication(builder_ctx(), lambda_search_ctx(), od_atm_lambda:id()) ->
    builder_ctx().
mark_lambda_revision_for_duplication(BuilderCtx = #builder_ctx{lambda_revisions_to_add = RevisionsToAdd}, #lambda_search_ctx{
    revision_number = RevisionNumber,
    json_dump = #{<<"revision">> := RevisionData}
}, DuplicateLambdaId) ->
    BuilderCtx#builder_ctx{
        lambda_revisions_to_add = maps:update_with(DuplicateLambdaId, fun(RevisionsToCreate) ->
            RevisionsToCreate#{RevisionNumber => RevisionData}
        end, #{RevisionNumber => RevisionData}, RevisionsToAdd)
    }.


%% @private
-spec validate_workflow_schema_revision(builder_ctx()) -> ok.
validate_workflow_schema_revision(#builder_ctx{
    target_atm_inventory_id = TargetAtmInventoryId,
    atm_workflow_schema_revision = AtmWorkflowSchemaRevision,
    lambdas_to_duplicate = LambdasToDuplicate,
    lambda_revisions_to_add = LambdaRevisionsToAdd
}) ->
    ParsedLambdasToDuplicate = maps:map(fun(OriginalAtmLambdaId, RevisionDefinitions) ->
        AtmLambda = #od_atm_lambda{
            original_atm_lambda = OriginalAtmLambdaId,
            atm_inventories = [TargetAtmInventoryId]
        },
        case parse_and_add_revisions_to_lambda(AtmLambda, RevisionDefinitions) of
            {ok, AtmLambdaWithRevisions} ->
                AtmLambdaWithRevisions;
            {error, _} = Error ->
                throw(?ERROR_BAD_DATA(
                    <<"supplementaryAtmLambdas[", OriginalAtmLambdaId/binary, "].revision.atmLambdaRevision">>,
                    Error
                ))
        end
    end, LambdasToDuplicate),
    LambdasWithParsedRevisionsToAdd = maps:map(fun(AtmLambdaId, RevisionDefinitions) ->
        {ok, #document{value = #od_atm_lambda{
            original_atm_lambda = OriginalAtmLambdaId
        } = AtmLambda}} = od_atm_lambda:get(AtmLambdaId),
        case parse_and_add_revisions_to_lambda(AtmLambda, RevisionDefinitions) of
            {ok, AtmLambdaWithRevisions} ->
                AtmLambdaWithRevisions;
            {error, _} = Error ->
                throw(?ERROR_BAD_DATA(
                    <<"supplementaryAtmLambdas[", OriginalAtmLambdaId/binary, "].revision.atmLambdaRevision">>,
                    Error
                ))
        end
    end, LambdaRevisionsToAdd),
    atm_workflow_schema_validator:validate(
        AtmWorkflowSchemaRevision,
        maps:merge(ParsedLambdasToDuplicate, LambdasWithParsedRevisionsToAdd)
    ).


%% @private
-spec parse_and_add_revisions_to_lambda(od_atm_lambda:record(), revision_definitions()) ->
    {ok, od_atm_lambda:record()} | errors:error().
parse_and_add_revisions_to_lambda(AtmLambda, RevisionDefinitions) ->
    maps_utils:fold_while(fun(RevisionNumber, AtmLambdaRevisionData, {ok, AtmLambdaAcc}) ->
        case atm_lambda_logic:parse_revision(?NOBODY, AtmLambdaRevisionData) of
            {ok, AtmLambdaRevision} ->
                {cont, {ok, AtmLambdaAcc#od_atm_lambda{
                    revision_registry = atm_lambda_revision_registry:add_revision(
                        RevisionNumber, AtmLambdaRevision, AtmLambdaAcc#od_atm_lambda.revision_registry
                    )
                }}};
            {error, _} = Error ->
                {halt, Error}
        end
    end, {ok, AtmLambda}, RevisionDefinitions).


%% @private
-spec add_revisions_to_lambdas(builder_ctx()) -> ok | no_return().
add_revisions_to_lambdas(BuilderCtx = #builder_ctx{lambda_revisions_to_add = LambdaRevisionsToAdd}) ->
    maps:foreach(fun(AtmLambdaId, RevisionDefinitions) ->
        case add_revisions_to_lambda(BuilderCtx, AtmLambdaId, RevisionDefinitions) of
            ok ->
                ok;
            {error, _} = Error ->
                throw(Error)
        end
    end, LambdaRevisionsToAdd).


%% @private
-spec duplicate_lambdas(builder_ctx()) -> builder_ctx().
duplicate_lambdas(BuilderCtx = #builder_ctx{lambdas_to_duplicate = LambdasToDuplicate}) ->
    maps:fold(fun(OriginalAtmLambdaId, RevisionDefinitions, BuilderCtxAccExternal) ->
        ActualAtmLambdaId = create_lambda_duplicate_with_revisions(BuilderCtx, OriginalAtmLambdaId, RevisionDefinitions),
        maps:fold(fun(RevisionNumber, _, BuilderCtxAccInternal) ->
            substitute_lambda_for_duplicate(BuilderCtxAccInternal, OriginalAtmLambdaId, RevisionNumber, ActualAtmLambdaId)
        end, BuilderCtxAccExternal, RevisionDefinitions)
    end, BuilderCtx, LambdasToDuplicate).


%% @private
-spec create_lambda_duplicate_with_revisions(builder_ctx(), od_atm_lambda:id(), revision_definitions()) ->
    od_atm_lambda:id().
create_lambda_duplicate_with_revisions(BuilderCtx = #builder_ctx{
    auth = Auth,
    target_atm_inventory_id = AtmInventoryId
}, OriginalAtmLambdaId, RevisionDefinitions) ->
    LatestRevisionNumber = lists:max(maps:keys(RevisionDefinitions)),
    AtmLambdaData = #{
        <<"atmInventoryId">> => AtmInventoryId,
        <<"originalAtmLambdaId">> => OriginalAtmLambdaId,
        <<"revision">> => maps:get(LatestRevisionNumber, RevisionDefinitions)
    },
    case atm_lambda_logic:create(Auth, AtmLambdaData) of
        {error, _} = CreateError ->
            throw(CreateError);
        {ok, ActualAtmLambdaId} ->
            RemainingRevisionDefinitions = maps:without([LatestRevisionNumber], RevisionDefinitions),
            case add_revisions_to_lambda(BuilderCtx, ActualAtmLambdaId, RemainingRevisionDefinitions) of
                ok ->
                    ActualAtmLambdaId;
                {error, _} = AddRevisionError ->
                    % this will trigger lambda deletion
                    atm_lambda_logic:unlink_from_inventory(?ROOT, ActualAtmLambdaId, AtmInventoryId),
                    throw(AddRevisionError)
            end
    end.


%% @private
-spec add_revisions_to_lambda(builder_ctx(), od_atm_lambda:id(), revision_definitions()) ->
    ok | errors:error().
add_revisions_to_lambda(#builder_ctx{auth = Auth}, AtmLambdaId, RevisionDefinitions) ->
    maps_utils:fold_while(fun(RevisionNumber, AtmLambdaRevisionData, _) ->
        case atm_lambda_logic:add_revision(Auth, AtmLambdaId, RevisionNumber, AtmLambdaRevisionData) of
            {error, _} = Error ->
                {halt, Error};
            ok ->
                {cont, ok}
        end
    end, ok, RevisionDefinitions).


%% @private
-spec link_referenced_lambdas(builder_ctx()) -> ok.
link_referenced_lambdas(#builder_ctx{
    target_atm_inventory_id = TargetAtmInventoryId,
    lambdas_to_link = LambdasToLink
}) ->
    lists:foreach(fun(AtmLambdaId) ->
        % the client's auth to link the lambdas to the inventory has been already checked,
        % link with root auth to avoid race conditions
        case atm_lambda_logic:link_to_inventory(?ROOT, AtmLambdaId, TargetAtmInventoryId) of
            ok ->
                ok;
            {error, _} = Error ->
                throw(Error)
        end
    end, LambdasToLink).


%% @private
-spec can_manage_lambdas_in_inventory(aai:auth(), od_atm_inventory:id() | od_atm_inventory:record()) ->
    boolean().
can_manage_lambdas_in_inventory(?ROOT, _) ->
    true;
can_manage_lambdas_in_inventory(?USER(UserId), AtmInventoryOrId) ->
    atm_inventory_logic:has_eff_privilege(AtmInventoryOrId, UserId, ?ATM_INVENTORY_MANAGE_LAMBDAS) orelse
        user_logic:has_eff_oz_privilege(UserId, ?OZ_ATM_INVENTORIES_UPDATE);
can_manage_lambdas_in_inventory(_, _) ->
    false.


%% @private
-spec can_manage_lambda(aai:auth(), od_atm_lambda:id()) -> boolean().
can_manage_lambda(?ROOT, AtmLambdaId) ->
    atm_lambda_logic:exists(AtmLambdaId);
can_manage_lambda(?USER(UserId), AtmLambdaId) ->
    atm_lambda_logic_plugin:can_manage_lambda_in_any_inventory(UserId, AtmLambdaId) orelse (
        user_logic:has_eff_oz_privilege(UserId, ?OZ_ATM_INVENTORIES_UPDATE) andalso
            atm_lambda_logic:exists(AtmLambdaId)
    );
can_manage_lambda(_, _) ->
    false.


%% @private
-spec update_revision_registry_unsafe(
    od_atm_workflow_schema:id(),
    fun((atm_workflow_schema_revision_registry:record()) -> atm_workflow_schema_revision_registry:record())
) -> ok | no_return().
update_revision_registry_unsafe(AtmWorkflowSchemaId, RevisionRegistryDiff) ->
    {ok, #document{value = PreviousAtmWorkflowSchema}} = od_atm_workflow_schema:get(AtmWorkflowSchemaId),
    update_revision_registry_unsafe(AtmWorkflowSchemaId, PreviousAtmWorkflowSchema, RevisionRegistryDiff).

%% @private
-spec update_revision_registry_unsafe(
    od_atm_workflow_schema:id(),
    od_atm_workflow_schema:record(),
    fun((atm_workflow_schema_revision_registry:record()) -> atm_workflow_schema_revision_registry:record())
) -> ok | no_return().
update_revision_registry_unsafe(AtmWorkflowSchemaId, PreviousAtmWorkflowSchema, RevisionRegistryDiff) ->
    DocDiff = fun(AtmWorkflowSchema = #od_atm_workflow_schema{
        revision_registry = PreviousRevisionRegistry
    }) ->
        case RevisionRegistryDiff(PreviousRevisionRegistry) of
            {ok, NewRegistry} ->
                {ok, AtmWorkflowSchema#od_atm_workflow_schema{revision_registry = NewRegistry}};
            {error, _} = ApplyDiffError ->
                ApplyDiffError
        end
    end,
    case od_atm_workflow_schema:update(AtmWorkflowSchemaId, DocDiff) of
        {error, _} = UpdateError ->
            throw(UpdateError);
        {ok, #document{value = NewAtmWorkflowSchema}} ->
            reconcile_referenced_lambdas_unsafe(AtmWorkflowSchemaId, PreviousAtmWorkflowSchema, NewAtmWorkflowSchema)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Each workflow schema tracks what lambdas are used throughout its lanes and
%% this information must be refreshed after each update.
%% @end
%%--------------------------------------------------------------------
-spec reconcile_referenced_lambdas_unsafe(
    od_atm_workflow_schema:id(),
    od_atm_workflow_schema:record(),
    od_atm_workflow_schema:record()
) -> ok.
reconcile_referenced_lambdas_unsafe(AtmWorkflowSchemaId, PreviousAtmWorkflowSchema, NewAtmWorkflowSchema) ->
    OldAtmLambdas = maps:keys(od_atm_workflow_schema:extract_all_atm_lambda_references(PreviousAtmWorkflowSchema)),
    NewAtmLambdas = maps:keys(od_atm_workflow_schema:extract_all_atm_lambda_references(NewAtmWorkflowSchema)),
    ToAdd = lists_utils:subtract(NewAtmLambdas, OldAtmLambdas),
    ToDelete = lists_utils:subtract(OldAtmLambdas, NewAtmLambdas),
    lists:foreach(fun(AtmLambdaId) ->
        od_atm_lambda:critical_section(AtmLambdaId, fun() ->
            entity_graph:add_relation(
                od_atm_lambda, AtmLambdaId,
                od_atm_workflow_schema, AtmWorkflowSchemaId
            )
        end)
    end, ToAdd),
    lists:foreach(fun(AtmLambdaId) ->
        od_atm_lambda:critical_section(AtmLambdaId, fun() ->
            entity_graph:remove_relation(
                od_atm_lambda, AtmLambdaId,
                od_atm_workflow_schema, AtmWorkflowSchemaId
            )
        end)
    end, ToDelete).
