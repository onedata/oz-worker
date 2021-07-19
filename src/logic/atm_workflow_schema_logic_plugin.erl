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
create(#el_req{auth = Auth, gri = #gri{id = undefined, aspect = instance} = GRI, data = Data}) ->
    AtmWorkflowSchemaId = atm_workflow_schema_builder:create(Auth, Data),

    {true, {FetchedAtmWorkflowSchema, Rev}} = fetch_entity(#gri{aspect = instance, id = AtmWorkflowSchemaId}),
    {ok, resource, {GRI#gri{id = AtmWorkflowSchemaId}, {FetchedAtmWorkflowSchema, Rev}}};

create(#el_req{gri = #gri{id = AtmWorkflowSchemaId, aspect = dump}}) ->
    case fetch_entity(#gri{id = AtmWorkflowSchemaId}) of
        {error, _} = Error ->
            Error;
        {true, {AtmWorkflowSchema, _}} ->
            {ok, value, od_atm_workflow_schema:dump_schema_to_json(AtmWorkflowSchemaId, AtmWorkflowSchema)}
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
    atm_workflow_schema_builder:update(Auth, AtmWorkflowSchemaId, Data).


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

authorize(#el_req{auth = ?USER(UserId), operation = create, gri = #gri{aspect = dump}}, AtmWorkflowSchema) ->
    is_inventory_member(UserId, AtmWorkflowSchema);

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
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = dump}}) ->
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
validate(#el_req{operation = create, gri = #gri{aspect = instance}}) -> #{
    required => #{
        <<"atmInventoryId">> => {binary, {exists, fun atm_inventory_logic:exists/1}},
        <<"name">> => {binary, name}
    },
    optional => #{
        <<"description">> => {binary, {size_limit, ?DESCRIPTION_SIZE_LIMIT}},

        <<"stores">> => {{jsonable_record, list, atm_store_schema}, any},
        <<"lanes">> => {{jsonable_record, list, atm_lane_schema}, any},

        <<"state">> => {atom, automation:all_workflow_schema_states()},

        <<"supplementaryAtmLambdas">> => {json, any}  % validation is performed by atm_workflow_schema_builder
    }
};
validate(#el_req{operation = create, gri = #gri{aspect = dump}}) -> #{
};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"description">> => {binary, {size_limit, ?DESCRIPTION_SIZE_LIMIT}},

        <<"stores">> => {{jsonable_record, list, atm_store_schema}, any},
        <<"lanes">> => {{jsonable_record, list, atm_lane_schema}, any},

        <<"state">> => {atom, automation:all_workflow_schema_states()},

        <<"supplementaryAtmLambdas">> => {json, any}  % validation is performed by atm_workflow_schema_builder
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
