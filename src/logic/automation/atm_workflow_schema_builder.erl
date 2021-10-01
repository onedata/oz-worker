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
%%% @end
%%%-------------------------------------------------------------------
-module(atm_workflow_schema_builder).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/privileges.hrl").

-export([insert_revision/4]).
-export([delete_revision/2]).
-export([delete_all_revisions/1]).

% map with atm_lambda ids and corresponding json encoded payload
-type lambda_definitions() :: #{od_atm_lambda:id() => entity_logic:data()}.

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
    supplementary_lambdas = #{} :: lambda_definitions(),
    lambdas_to_link = [] :: [od_atm_lambda:id()],
    % this map contains ids that are only used for cross-referencing with the
    % schema, they are substituted with actual lambda ids after they are created
    lambdas_to_create = #{} :: lambda_definitions()
}).
-type builder_ctx() :: #builder_ctx{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec insert_revision(
    aai:auth(),
    od_atm_workflow_schema:id(),
    atm_workflow_schema_revision:revision_number(),
    entity_logic:data()
) ->
    ok | no_return().
insert_revision(Auth, AtmWorkflowSchemaId, RevisionNumber, Data) ->
    od_atm_workflow_schema:critical_section(AtmWorkflowSchemaId, fun() ->
        {ok, #document{value = PreviousAtmWorkflowSchema}} = od_atm_workflow_schema:get(AtmWorkflowSchemaId),

        SupplementaryAtmLambdas = maps:get(<<"supplementaryAtmLambdas">>, Data, #{}),
        AtmWorkflowSchemaRevision = maps:get(<<"schema">>, Data),

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
    end).


-spec delete_revision(od_atm_workflow_schema:id(), atm_workflow_schema_revision:revision_number()) ->
    ok | no_return().
delete_revision(AtmWorkflowSchemaId, RevisionNumber) ->
    od_atm_workflow_schema:critical_section(AtmWorkflowSchemaId, fun() ->
        update_revision_registry_unsafe(AtmWorkflowSchemaId, fun(RevisionRegistry) ->
            case atm_workflow_schema_revision_registry:take_revision(RevisionNumber, RevisionRegistry) of
                error ->
                    ?ERROR_NOT_FOUND;
                {_, UpdatedRegistry} ->
                    {ok, UpdatedRegistry}
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
    Ctx3 = create_missing_lambdas(Ctx2),
    link_referenced_lambdas(Ctx3),
    Ctx3.


%% @private
-spec resolve_referenced_lambdas(builder_ctx()) -> builder_ctx().
resolve_referenced_lambdas(BuilderCtx = #builder_ctx{
    auth = Auth,
    atm_workflow_schema_revision = AtmWorkflowSchemaRevision,
    target_atm_inventory_id = AtmInventoryId,
    supplementary_lambdas = SupplementaryAtmLambdas
}) ->
    {ok, #document{
        value = #od_atm_inventory{
            atm_lambdas = InventoryLambdas
        } = AtmInventory
    }} = od_atm_inventory:get(AtmInventoryId),
    RevisionLambdas = atm_workflow_schema_revision:extract_referenced_atm_lambdas(AtmWorkflowSchemaRevision),
    MissingLambdas = lists_utils:subtract(RevisionLambdas, InventoryLambdas),
    case MissingLambdas of
        [] ->
            BuilderCtx;
        _ ->
            UpdatedBuilderCtx = BuilderCtx#builder_ctx{
                target_atm_inventory = AtmInventory,
                can_manage_lambdas_in_target_inventory = can_manage_lambdas_in_inventory(Auth, AtmInventory)
            },
            lists:foldl(fun(AtmLambdaId, BuilderCtxAcc) ->
                AtmLambdaData = maps:get(AtmLambdaId, SupplementaryAtmLambdas, undefined),
                resolve_missing_lambda(BuilderCtxAcc, AtmLambdaId, AtmLambdaData)
            end, UpdatedBuilderCtx, MissingLambdas)
    end.


%% @private
-spec resolve_missing_lambda(builder_ctx(), od_atm_lambda:id(), undefined | entity_logic:data()) ->
    builder_ctx().
resolve_missing_lambda(BuilderCtx1, AtmLambdaId, AtmLambdaData) ->
    Checksum = case AtmLambdaData of
        undefined ->
            undefined;
        _ ->
            maps:get(<<"checksum">>, AtmLambdaData, undefined)
    end,
    case try_reusing_lambda_duplicate_in_current_inventory(BuilderCtx1, AtmLambdaId, Checksum) of
        {true, BuilderCtx2} ->
            BuilderCtx2;
        false ->
            % at least one lambda will have to be linked or created,
            % skip further checks if the client is not authorized to do so
            BuilderCtx1#builder_ctx.can_manage_lambdas_in_target_inventory orelse throw(?ERROR_FORBIDDEN),
            case try_linking_referenced_lambda(BuilderCtx1, AtmLambdaId) of
                {true, BuilderCtx3} ->
                    BuilderCtx3;
                false ->
                    case try_linking_any_lambda_duplicate(BuilderCtx1, AtmLambdaId, Checksum) of
                        {true, BuilderCtx4} ->
                            BuilderCtx4;
                        false ->
                            case try_creating_lambda_duplicate(BuilderCtx1, AtmLambdaId, AtmLambdaData) of
                                {true, BuilderCtx5} ->
                                    BuilderCtx5;
                                false ->
                                    atm_schema_validator:raise_validation_error(
                                        <<"tasks">>,
                                        "The lambda id '~s' referenced by one of the tasks was not found or is not "
                                        "available for the requesting client. Consider providing supplementary lambdas "
                                        "so that missing ones can be linked or created along with the workflow schema.",
                                        [AtmLambdaId]
                                    )
                            end
                    end
            end
    end.


%% @private
-spec try_reusing_lambda_duplicate_in_current_inventory(
    builder_ctx(),
    od_atm_lambda:id(),
    undefined | od_atm_lambda:checksum()
) ->
    {true, builder_ctx()} | false.
try_reusing_lambda_duplicate_in_current_inventory(_BuilderCtx, _AtmLambdaId, undefined) ->
    false;
try_reusing_lambda_duplicate_in_current_inventory(BuilderCtx = #builder_ctx{
    target_atm_inventory = TargetAtmInventory
}, AtmLambdaId, Checksum) ->
    case find_lambda_duplicate_in_inventory(Checksum, TargetAtmInventory) of
        {ok, DuplicateLambdaId} ->
            {true, substitute_lambda_id(BuilderCtx, AtmLambdaId, DuplicateLambdaId)};
        error ->
            false
    end.


%% @private
-spec try_linking_referenced_lambda(
    builder_ctx(),
    od_atm_lambda:id()
) ->
    {true, builder_ctx()} | false.
try_linking_referenced_lambda(BuilderCtx, AtmLambdaId) ->
    case can_manage_lambda(BuilderCtx#builder_ctx.auth, AtmLambdaId) of
        true ->
            {true, BuilderCtx#builder_ctx{
                lambdas_to_link = lists_utils:union([AtmLambdaId], BuilderCtx#builder_ctx.lambdas_to_link)
            }};
        false ->
            false
    end.


%% @private
-spec try_linking_any_lambda_duplicate(
    builder_ctx(),
    od_atm_lambda:id(),
    undefined | od_atm_lambda:checksum()
) ->
    {true, builder_ctx()} | false.
try_linking_any_lambda_duplicate(_BuilderCtx, _AtmLambdaId, undefined) ->
    false;
try_linking_any_lambda_duplicate(BuilderCtx, AtmLambdaId, Checksum) ->
    case find_linkable_lambda_duplicate(BuilderCtx, Checksum) of
        {ok, DuplicateLambdaId} ->
            BuilderCtxWithSubstitutions = substitute_lambda_id(BuilderCtx, AtmLambdaId, DuplicateLambdaId),
            {true, BuilderCtxWithSubstitutions#builder_ctx{
                lambdas_to_link = lists_utils:union([DuplicateLambdaId], BuilderCtx#builder_ctx.lambdas_to_link)
            }};
        error ->
            false
    end.


%% @private
-spec try_creating_lambda_duplicate(
    builder_ctx(),
    od_atm_lambda:id(),
    undefined | entity_logic:data()
) ->
    {true, builder_ctx()} | false.
try_creating_lambda_duplicate(_BuilderCtx, _AtmLambdaId, undefined) ->
    false;
try_creating_lambda_duplicate(BuilderCtx, AtmLambdaId, AtmLambdaData) ->
    {true, BuilderCtx#builder_ctx{
        lambdas_to_create = maps:put(
            AtmLambdaId, AtmLambdaData, BuilderCtx#builder_ctx.lambdas_to_create
        )
    }}.


%% @private
-spec validate_workflow_schema_revision(builder_ctx()) -> ok.
validate_workflow_schema_revision(#builder_ctx{
    auth = Auth,
    atm_workflow_schema_revision = AtmWorkflowSchemaRevision,
    lambdas_to_create = LambdasToCreate
}) ->
    ParsedLambdasToCreate = maps:map(fun(ProposedAtmLambdaId, AtmLambdaData) ->
        case atm_lambda_logic:parse(Auth, AtmLambdaData) of
            {error, _} = Error ->
                throw(?ERROR_BAD_DATA(
                    <<"supplementaryAtmLambdas[", ProposedAtmLambdaId/binary, "]">>,
                    Error
                ));
            {ok, AtmLambda} ->
                AtmLambda
        end
    end, LambdasToCreate),
    atm_workflow_schema_validator:validate(AtmWorkflowSchemaRevision, ParsedLambdasToCreate).


%% @private
-spec create_missing_lambdas(builder_ctx()) -> builder_ctx().
create_missing_lambdas(BuilderCtx = #builder_ctx{
    auth = Auth,
    target_atm_inventory_id = AtmInventoryId,
    lambdas_to_create = LambdasToCreate
}) ->
    maps:fold(fun(ProposedAtmLambdaId, AtmLambdaData, BuilderCtxAcc) ->
        case atm_lambda_logic:create(Auth, AtmLambdaData#{<<"atmInventoryId">> => AtmInventoryId}) of
            {error, _} = Error ->
                throw(?ERROR_BAD_DATA(
                    <<"supplementaryAtmLambdas[", ProposedAtmLambdaId/binary, "]">>,
                    Error
                ));
            {ok, ActualAtmLambdaId} ->
                substitute_lambda_id(BuilderCtxAcc, ProposedAtmLambdaId, ActualAtmLambdaId)
        end
    end, BuilderCtx, LambdasToCreate).


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
-spec find_linkable_lambda_duplicate(builder_ctx(), od_atm_lambda:checksum()) ->
    {ok, od_atm_lambda:id()} | error.
find_linkable_lambda_duplicate(#builder_ctx{
    auth = ?USER(UserId) = Auth,
    target_atm_inventory_id = TargetAtmInventoryId
}, Checksum) ->
    {ok, EffAtmInventories} = user_logic:get_eff_atm_inventories(Auth, UserId),
    lists_utils:foldl_while(fun(AtmInventoryId, _) ->
        case od_atm_inventory:get(AtmInventoryId) of
            {error, not_found} ->
                {cont, error};
            {ok, #document{value = AtmInventory}} ->
                case find_linkable_lambda_duplicate_in_inventory(Auth, AtmInventory, Checksum) of
                    {ok, DuplicateLambdaId} ->
                        {halt, {ok, DuplicateLambdaId}};
                    error ->
                        {cont, error}
                end
        end
    end, error, EffAtmInventories -- [TargetAtmInventoryId]);
find_linkable_lambda_duplicate(_, _) ->
    error.


%% @private
-spec find_linkable_lambda_duplicate_in_inventory(
    aai:auth(),
    od_atm_inventory:record(),
    od_atm_lambda:checksum()
) ->
    {ok, od_atm_lambda:id()} | error.
find_linkable_lambda_duplicate_in_inventory(Auth, AtmInventory, Checksum) ->
    case can_manage_lambdas_in_inventory(Auth, AtmInventory) of
        false ->
            error;
        true ->
            find_lambda_duplicate_in_inventory(Checksum, AtmInventory)
    end.


%% @private
-spec find_lambda_duplicate_in_inventory(od_atm_lambda:checksum(), od_atm_inventory:record()) ->
    {ok, od_atm_lambda:id()} | error.
find_lambda_duplicate_in_inventory(Checksum, #od_atm_inventory{atm_lambdas = AtmLambdas}) ->
    lists_utils:foldl_while(fun(AtmLambdaId, _) ->
        case od_atm_lambda:get(AtmLambdaId) of
            {ok, #document{key = Id, value = #od_atm_lambda{checksum = Checksum}}} ->
                {halt, {ok, Id}};
            _ ->
                {cont, error}
        end
    end, error, AtmLambdas).


%% @private
-spec substitute_lambda_id(builder_ctx(), od_atm_lambda:id(), od_atm_lambda:id()) -> builder_ctx().
substitute_lambda_id(BuilderCtx = #builder_ctx{
    atm_workflow_schema_revision = AtmWorkflowSchemaRevision
}, CurrentLambdaId, TargetLambdaId) ->
    BuilderCtx#builder_ctx{
        atm_workflow_schema_revision = atm_workflow_schema_revision:map_tasks(fun
            (#atm_task_schema{lambda_id = Id} = AtmTaskSchema) when Id == CurrentLambdaId ->
                AtmTaskSchema#atm_task_schema{lambda_id = TargetLambdaId};
            (AtmTaskSchema) ->
                AtmTaskSchema
        end, AtmWorkflowSchemaRevision)
    }.


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
    atm_lambda_logic_plugin:can_manage_lambda(UserId, AtmLambdaId) orelse (
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
    OldAtmLambdas = od_atm_workflow_schema:extract_all_referenced_atm_lambdas(PreviousAtmWorkflowSchema),
    NewAtmLambdas = od_atm_workflow_schema:extract_all_referenced_atm_lambdas(NewAtmWorkflowSchema),
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
