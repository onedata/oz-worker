%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_atm_workflow_schema model.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_workflow_schema_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/automation/automation.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity and its revision from datastore, if applicable.
%% Should return:
%%  * {true, entity_logic:versioned_entity()}
%%      if the fetch was successful
%%  * false
%%      if fetch is not applicable for this operation
%%  * {error, _}
%%      if there was an error, such as ?ERROR_NOT_FOUND
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(gri:gri()) ->
    {true, entity_logic:versioned_entity()} | false | errors:error().
fetch_entity(#gri{id = AtmWorkflowSchemaId}) ->
    case od_atm_workflow_schema:get(AtmWorkflowSchemaId) of
        {ok, #document{value = AtmWorkflowSchema, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {AtmWorkflowSchema, Revision}};
        _ ->
            ?ERROR_NOT_FOUND
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given operation is supported based on operation, aspect and
%% scope (entity type is known based on the plugin itself).
%% @end
%%--------------------------------------------------------------------
-spec operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().
operation_supported(create, instance, private) -> true;
operation_supported(create, dump, private) -> true;
operation_supported(create, {revision, _}, private) -> true;
operation_supported(create, {dump_revision, _}, private) -> true;

operation_supported(get, list, private) -> true;
operation_supported(get, instance, private) -> true;
operation_supported(get, atm_lambdas, private) -> true;

operation_supported(update, instance, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {revision, _}, private) -> true;

operation_supported(_, _, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given {Aspect, Scope} pair is subscribable, i.e. clients can
%% subscribe to receive updates concerning the aspect of entity.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().
is_subscribable(instance, _) -> true;
is_subscribable(atm_lambdas, private) -> true;
is_subscribable(_, _) -> false.

%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(#el_req{auth = Auth, gri = #gri{id = undefined, aspect = instance} = GRI, data = Data}) ->
    Name = maps:get(<<"name">>, Data),
    Summary = maps:get(<<"summary">>, Data, ?DEFAULT_SUMMARY),
    OriginalAtmWorkflowSchemaId = maps:get(<<"originalAtmWorkflowSchemaId">>, Data, undefined),
    AtmInventoryId = maps:get(<<"atmInventoryId">>, Data),

    AtmWorkflowSchema = #od_atm_workflow_schema{
        name = Name,
        summary = Summary,
        original_atm_workflow_schema = OriginalAtmWorkflowSchemaId,
        atm_inventory = AtmInventoryId,
        creator = aai:normalize_subject(Auth#auth.subject),
        creation_time = global_clock:timestamp_seconds()
    },
    {ok, #document{key = AtmWorkflowSchemaId}} = od_atm_workflow_schema:create(#document{value = AtmWorkflowSchema}),

    case lookup_and_sanitize_revision_data(Data) of
        false ->
            ok;
        {true, InitialRevisionData} ->
            case insert_revision(Auth, AtmWorkflowSchemaId, <<"auto">>, InitialRevisionData) of
                ok ->
                    ok;
                {error, _} = Error ->
                    od_atm_workflow_schema:force_delete(AtmWorkflowSchemaId),
                    throw(Error)
            end
    end,

    od_atm_inventory:critical_section(AtmInventoryId, fun() ->
        entity_graph:add_relation(
            od_atm_workflow_schema, AtmWorkflowSchemaId,
            od_atm_inventory, AtmInventoryId
        )
    end),

    {true, {FetchedAtmWorkflowSchema, Rev}} = fetch_entity(#gri{aspect = instance, id = AtmWorkflowSchemaId}),
    {ok, resource, {GRI#gri{id = AtmWorkflowSchemaId}, {FetchedAtmWorkflowSchema, Rev}}};

create(#el_req{gri = #gri{id = AtmWorkflowSchemaId, aspect = dump}, data = Data}) ->
    fun(AtmWorkflowSchema) ->
        IncludedRevision = maps:get(<<"includeRevision">>, Data),

        atm_workflow_schema_revision_registry:has_revision(
            IncludedRevision, AtmWorkflowSchema#od_atm_workflow_schema.revision_registry
        ) orelse throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"includeRevision">>)),

        {ok, value, od_atm_workflow_schema:dump_to_json(
            AtmWorkflowSchemaId, AtmWorkflowSchema, IncludedRevision
        )}
    end;

create(#el_req{auth = Auth, gri = #gri{id = AtmWorkflowSchemaId, aspect = {revision, RevisionNumberBinary}}, data = Data}) ->
    insert_revision(Auth, AtmWorkflowSchemaId, RevisionNumberBinary, Data);

create(#el_req{gri = #gri{aspect = {dump_revision, RevisionNumberBinary}}}) ->
    fun(AtmWorkflowSchema) ->
        {ok, value, od_atm_workflow_schema:dump_revision_to_json(
            AtmWorkflowSchema, binary_to_integer(RevisionNumberBinary)
        )}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    {ok, AtmWorkflowSchemaDocs} = od_atm_workflow_schema:list(),
    {ok, [AtmWorkflowSchemaId || #document{key = AtmWorkflowSchemaId} <- AtmWorkflowSchemaDocs]};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, AtmWorkflowSchema) ->
    {ok, AtmWorkflowSchema};

get(#el_req{gri = #gri{aspect = atm_lambdas}}, AtmWorkflowSchema) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_atm_lambda, AtmWorkflowSchema)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{auth = Auth, gri = #gri{id = AtmWorkflowSchemaId, aspect = instance}, data = Data}) ->
    InsertRevisionResult = case lookup_and_sanitize_revision_data(Data) of
        false ->
            ok;
        {true, RevisionData} ->
            insert_revision(Auth, AtmWorkflowSchemaId, <<"auto">>, RevisionData)
    end,
    case InsertRevisionResult of
        {error, _} = Error ->
            Error;
        ok ->
            ?extract_ok(od_atm_workflow_schema:update(AtmWorkflowSchemaId, fun(AtmWorkflowSchema) ->
                {ok, AtmWorkflowSchema#od_atm_workflow_schema{
                    name = maps:get(<<"name">>, Data, AtmWorkflowSchema#od_atm_workflow_schema.name),
                    summary = maps:get(<<"summary">>, Data, AtmWorkflowSchema#od_atm_workflow_schema.summary)
                }}
            end))
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = AtmWorkflowSchemaId, aspect = instance}}) ->
    atm_workflow_schema_builder:delete_all_revisions(AtmWorkflowSchemaId),
    entity_graph:delete_with_relations(od_atm_workflow_schema, AtmWorkflowSchemaId);

delete(#el_req{gri = #gri{id = AtmWorkflowSchemaId, aspect = {revision, RevisionNumberBinary}}}) ->
    atm_workflow_schema_builder:delete_revision(AtmWorkflowSchemaId, binary_to_integer(RevisionNumberBinary)).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(#el_req{gri = #gri{aspect = {revision, RevisionNumberBin}}}, #od_atm_workflow_schema{
    revision_registry = RevisionRegistry
}) ->
    atm_workflow_schema_revision_registry:has_revision(binary_to_integer(RevisionNumberBin), RevisionRegistry);

exists(#el_req{}, FetchedAtmWorkflowSchema) ->
    % all other aspects exist if atm_workflow_schema record exists
    FetchedAtmWorkflowSchema =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(#el_req{auth = ?USER(UserId), operation = create, gri = #gri{id = undefined, aspect = instance}, data = Data}, _) ->
    AtmInventoryId = maps:get(<<"atmInventoryId">>, Data, <<"">>),
    can_manage_workflow_schemas(UserId, AtmInventoryId);

authorize(#el_req{auth = ?USER(UserId), operation = create, gri = #gri{aspect = dump}}, AtmWorkflowSchema) ->
    is_inventory_member(UserId, AtmWorkflowSchema);

authorize(#el_req{auth = ?USER(UserId), operation = create, gri = #gri{aspect = {revision, _}}}, AtmWorkflowSchema) ->
    can_manage_workflow_schemas(UserId, AtmWorkflowSchema);

authorize(#el_req{auth = ?USER(UserId), operation = create, gri = #gri{aspect = {dump_revision, _}}}, AtmWorkflowSchema) ->
    is_inventory_member(UserId, AtmWorkflowSchema);

authorize(#el_req{auth = ?USER(UserId), operation = get}, AtmWorkflowSchema) ->
    is_inventory_member(UserId, AtmWorkflowSchema);

authorize(#el_req{auth = ?USER(UserId), operation = get, gri = #gri{aspect = atm_lambdas}}, AtmWorkflowSchema) ->
    is_inventory_member(UserId, AtmWorkflowSchema);

authorize(#el_req{auth = ?USER(UserId), operation = update, gri = #gri{aspect = instance}}, AtmWorkflowSchema) ->
    can_manage_workflow_schemas(UserId, AtmWorkflowSchema);

authorize(#el_req{auth = ?USER(UserId), operation = delete, gri = #gri{aspect = instance}}, AtmWorkflowSchema) ->
    can_manage_workflow_schemas(UserId, AtmWorkflowSchema);

authorize(#el_req{auth = ?USER(UserId), operation = delete, gri = #gri{aspect = {revision, _}}}, AtmWorkflowSchema) ->
    can_manage_workflow_schemas(UserId, AtmWorkflowSchema);

authorize(_, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = instance}}) ->
    [?OZ_ATM_INVENTORIES_UPDATE];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = dump}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {revision, _}}}) ->
    [?OZ_ATM_INVENTORIES_UPDATE];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {dump_revision, _}}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = atm_lambdas}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_ATM_INVENTORIES_UPDATE];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    [?OZ_ATM_INVENTORIES_UPDATE];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {revision, _}}}) ->
    [?OZ_ATM_INVENTORIES_UPDATE];

required_admin_privileges(_) ->
    forbidden.


%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given request.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(entity_logic:req()) -> entity_logic_sanitizer:sanitizer_spec().
validate(#el_req{operation = create, gri = #gri{aspect = instance}}) -> #{
    required => #{
        <<"atmInventoryId">> => {binary, {exists, fun atm_inventory_logic:exists/1}},
        <<"name">> => {binary, name}
    },
    optional => #{
        <<"summary">> => {binary, {size_limit, ?SUMMARY_SIZE_LIMIT}},
        % validation of revision data is performed during the workflow schema creation procedure
        <<"revision">> => {json, any},
        % this can be an arbitrary string and is not verified, as for example a dump
        % from another Onezone may be used to create a workflow schema
        <<"originalAtmWorkflowSchemaId">> => {binary, non_empty}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = dump}}) -> #{
    required => #{
        <<"includeRevision">> => {integer, {not_lower_than, 1}}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {revision, TargetRevisionNumber}}}) ->
    revision_sanitizer_spec(TargetRevisionNumber);

validate(#el_req{operation = create, gri = #gri{aspect = {dump_revision, _}}}) -> #{
    required => #{
        {aspect, <<"revisionNumber">>} => {any, fun(RevisionNumberBinary) ->
            try
                binary_to_integer(RevisionNumberBinary) > 0
            catch _:_ ->
                false
            end
        end}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"summary">> => {binary, {size_limit, ?SUMMARY_SIZE_LIMIT}},
        % validation of revision data is performed during the workflow schema update procedure
        <<"revision">> => {json, any}
    }
}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec revision_sanitizer_spec(binary()) -> entity_logic_sanitizer:sanitizer_spec().
revision_sanitizer_spec(TargetRevisionNumber) ->
    % NOTE: the target revision is generally given in the aspect, but a special "auto"
    % keyword is accepted to indicate that the original revision number should be taken
    % and then originalRevisionNumber key is mandatory. This is used especially when
    % creating a revision from a JSON dump (which includes the originalRevisionNumber).
    CommonRequired = #{
        {aspect, <<"targetRevisionNumber">>} => {any, fun
            (<<"auto">>) ->
                true;
            (Bin) when is_binary(Bin) ->
                try
                    binary_to_integer(Bin) > 0
                catch _:_ ->
                    false
                end
        end},
        <<"atmWorkflowSchemaRevision">> => {{jsonable_record, single, atm_workflow_schema_revision}, any}
    },

    #{
        required => case TargetRevisionNumber of
            <<"auto">> ->
                CommonRequired#{<<"originalRevisionNumber">> => {integer, {not_lower_than, 1}}};
            _ ->
                CommonRequired
        end,
        optional => #{
            <<"supplementaryAtmLambdas">> => {json, any}  % validation is performed by atm_workflow_schema_builder
        }
    }.


%% @private
-spec lookup_and_sanitize_revision_data(entity_logic:data()) ->
    {true, entity_logic:sanitized_data()} | false | no_return().
lookup_and_sanitize_revision_data(Data) ->
    case maps:find(<<"revision">>, Data) of
        error ->
            false;
        {ok, RevisionData} ->
            SanitizedRevisionData = entity_logic_sanitizer:ensure_valid(
                revision_sanitizer_spec(<<"auto">>),
                {revision, <<"auto">>},
                RevisionData
            ),
            {true, SanitizedRevisionData}
    end.


%% @private
-spec insert_revision(aai:auth(), od_atm_workflow_schema:id(), binary(), entity_logic:sanitized_data()) ->
    ok | errors:error().
insert_revision(Auth, AtmWorkflowSchemaId, RevisionNumberBinary, RevisionData) ->
    RevisionNumber = case RevisionNumberBinary of
        <<"auto">> -> maps:get(<<"originalRevisionNumber">>, RevisionData);
        _ -> binary_to_integer(RevisionNumberBinary)
    end,
    atm_workflow_schema_builder:insert_revision(Auth, AtmWorkflowSchemaId, RevisionNumber, RevisionData).


%% @private
-spec can_manage_workflow_schemas(od_user:id(), od_atm_workflow_schema:record() | od_atm_inventory:id()) -> boolean().
can_manage_workflow_schemas(UserId, #od_atm_workflow_schema{atm_inventory = AtmInventoryId}) ->
    can_manage_workflow_schemas(UserId, AtmInventoryId);
can_manage_workflow_schemas(UserId, AtmInventoryId) ->
    atm_inventory_logic:has_eff_privilege(AtmInventoryId, UserId, ?ATM_INVENTORY_MANAGE_WORKFLOW_SCHEMAS).


%% @private
-spec is_inventory_member(od_user:id(), od_atm_workflow_schema:record() | od_atm_inventory:id()) -> boolean().
is_inventory_member(UserId, #od_atm_workflow_schema{atm_inventory = AtmInventoryId}) ->
    atm_inventory_logic:has_eff_user(AtmInventoryId, UserId).
