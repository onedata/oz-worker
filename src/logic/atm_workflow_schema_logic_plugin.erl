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

operation_supported(get, list, private) -> true;
operation_supported(get, instance, private) -> true;
operation_supported(get, atm_lambdas, private) -> true;

operation_supported(update, instance, private) -> true;

operation_supported(delete, instance, private) -> true;

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
create(Req = #el_req{gri = #gri{id = undefined, aspect = instance} = GRI, auth = Auth}) ->
    Name = maps:get(<<"name">>, Req#el_req.data),
    Description = maps:get(<<"description">>, Req#el_req.data, ?DEFAULT_DESCRIPTION),

    AtmInventoryId = maps:get(<<"atmInventoryId">>, Req#el_req.data),
    Stores = maps:get(<<"stores">>, Req#el_req.data, []),
    Lanes = maps:get(<<"lanes">>, Req#el_req.data, []),

    State = maps:get(<<"state">>, Req#el_req.data, case {Stores, Lanes} of
        {[], _} -> incomplete;
        {_, []} -> incomplete;
        {_, _} -> ready
    end),

    AtmWorkflowSchema = #od_atm_workflow_schema{
        name = Name,
        description = Description,

        stores = Stores,
        lanes = Lanes,

        state = State,

        creator = aai:normalize_subject(Auth#auth.subject),
        creation_time = global_clock:timestamp_seconds()
    },
    case atm_workflow_schema_validator:validate(AtmWorkflowSchema#od_atm_workflow_schema{atm_inventory = AtmInventoryId}) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end,

    {ok, #document{key = AtmWorkflowSchemaId}} = od_atm_workflow_schema:create(#document{value = AtmWorkflowSchema}),
    entity_graph:add_relation(
        od_atm_workflow_schema, AtmWorkflowSchemaId,
        od_atm_inventory, AtmInventoryId
    ),

    lock_on_workflow(AtmWorkflowSchemaId, fun() ->
        reconcile_referenced_lambdas_unsafe(AtmWorkflowSchemaId, [], Lanes)
    end),
    {true, {FetchedAtmWorkflowSchema, Rev}} = fetch_entity(#gri{aspect = instance, id = AtmWorkflowSchemaId}),
    {ok, resource, {GRI#gri{id = AtmWorkflowSchemaId}, {FetchedAtmWorkflowSchema, Rev}}}.


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
update(#el_req{gri = #gri{id = AtmWorkflowSchemaId, aspect = instance}, data = Data}) ->
    lock_on_workflow(AtmWorkflowSchemaId, fun() ->
        {ok, #document{value = #od_atm_workflow_schema{
            lanes = OldLanes
        }}} = od_atm_workflow_schema:get(AtmWorkflowSchemaId),

        Diff = fun(AtmWorkflowSchema) ->
            NewAtmWorkflowSchema = AtmWorkflowSchema#od_atm_workflow_schema{
                name = maps:get(<<"name">>, Data, AtmWorkflowSchema#od_atm_workflow_schema.name),
                description = maps:get(<<"description">>, Data, AtmWorkflowSchema#od_atm_workflow_schema.description),

                stores = maps:get(<<"stores">>, Data, AtmWorkflowSchema#od_atm_workflow_schema.stores),
                lanes = maps:get(<<"lanes">>, Data, OldLanes),

                state = maps:get(<<"state">>, Data, AtmWorkflowSchema#od_atm_workflow_schema.state)
            },
            case atm_workflow_schema_validator:validate(NewAtmWorkflowSchema) of
                ok -> {ok, NewAtmWorkflowSchema};
                {error, _} = Error -> Error
            end
        end,
        case od_atm_workflow_schema:update(AtmWorkflowSchemaId, Diff) of
            {error, _} = Error ->
                Error;
            {ok, #document{value = #od_atm_workflow_schema{lanes = NewLanes}}} ->
                reconcile_referenced_lambdas_unsafe(AtmWorkflowSchemaId, OldLanes, NewLanes)
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = AtmWorkflowSchemaId, aspect = instance}}) ->
    entity_graph:delete_with_relations(od_atm_workflow_schema, AtmWorkflowSchemaId).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(#el_req{gri = #gri{id = Id}}, #od_atm_workflow_schema{}) ->
    % All aspects exist if atm_workflow_schema record exists.
    Id =/= undefined.


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

authorize(#el_req{auth = ?USER(UserId), operation = get}, AtmWorkflowSchema) ->
    is_inventory_member(UserId, AtmWorkflowSchema);

authorize(#el_req{auth = ?USER(UserId), operation = get, gri = #gri{aspect = atm_lambdas}}, AtmWorkflowSchema) ->
    is_inventory_member(UserId, AtmWorkflowSchema);

authorize(#el_req{auth = ?USER(UserId), operation = update, gri = #gri{aspect = instance}}, AtmWorkflowSchema) ->
    can_manage_workflow_schemas(UserId, AtmWorkflowSchema);

authorize(#el_req{auth = ?USER(UserId), operation = delete, gri = #gri{aspect = instance}}, AtmWorkflowSchema) ->
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
-spec validate(entity_logic:req()) -> entity_logic:validity_verificator().
validate(#el_req{operation = create, gri = #gri{aspect = instance}}) ->
    #{
        required => #{
            <<"atmInventoryId">> => {binary, {exists, fun atm_inventory_logic:exists/1}},
            <<"name">> => {binary, name}
        },
        optional => #{
            <<"description">> => {binary, {size_limit, ?DESCRIPTION_SIZE_LIMIT}},
            <<"stores">> => {{jsonable_record, list, atm_store_schema}, any},
            <<"lanes">> => {{jsonable_record, list, atm_lane_schema}, any},
            <<"state">> => {atom, automation:all_workflow_schema_states()}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"description">> => {binary, {size_limit, ?DESCRIPTION_SIZE_LIMIT}},

        <<"stores">> => {{jsonable_record, list, atm_store_schema}, any},
        <<"lanes">> => {{jsonable_record, list, atm_lane_schema}, any},

        <<"state">> => {atom, automation:all_workflow_schema_states()}
    }
}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

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


%% @private
-spec lock_on_workflow(od_atm_workflow_schema:id(), fun(() -> Result)) -> Result.
lock_on_workflow(AtmWorkflowSchemaId, Callback) ->
    critical_section:run({?MODULE, AtmWorkflowSchemaId}, Callback).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Each workflow schema tracks what lambdas are used throughout its lanes and
%% this information must be refreshed after each update.
%% @end
%%--------------------------------------------------------------------
-spec reconcile_referenced_lambdas_unsafe(
    od_atm_workflow_schema:id(),
    [atm_lane_schema:record()],
    [atm_lane_schema:record()]
) -> ok.
reconcile_referenced_lambdas_unsafe(AtmWorkflowSchemaId, OldLanes, NewLanes) ->
    OldAtmLambdas = od_atm_workflow_schema:extract_atm_lambdas_from_lanes(OldLanes),
    NewAtmLambdas = od_atm_workflow_schema:extract_atm_lambdas_from_lanes(NewLanes),
    ToAdd = lists_utils:subtract(NewAtmLambdas, OldAtmLambdas),
    ToDelete = lists_utils:subtract(OldAtmLambdas, NewAtmLambdas),
    lists:foreach(fun(AtmLambdaId) ->
        entity_graph:add_relation(
            od_atm_lambda, AtmLambdaId,
            od_atm_workflow_schema, AtmWorkflowSchemaId
        )
    end, ToAdd),
    lists:foreach(fun(AtmLambdaId) ->
        entity_graph:remove_relation(
            od_atm_lambda, AtmLambdaId,
            od_atm_workflow_schema, AtmWorkflowSchemaId
        )
    end, ToDelete).
