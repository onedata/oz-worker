%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_atm_lambda model.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_logic_plugin).
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
fetch_entity(#gri{id = AtmLambdaId}) ->
    case od_atm_lambda:get(AtmLambdaId) of
        {ok, #document{value = AtmLambda, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {AtmLambda, Revision}};
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
operation_supported(create, {atm_inventory, _}, private) -> true;

operation_supported(get, list, private) -> true;
operation_supported(get, instance, private) -> true;
operation_supported(get, atm_inventories, private) -> true;
operation_supported(get, atm_workflow_schemas, private) -> true;

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
is_subscribable(atm_inventories, private) -> true;
is_subscribable(atm_workflow_schemas, private) -> true;
is_subscribable(_, _) -> false.

%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(Req = #el_req{gri = #gri{id = undefined, aspect = instance} = GRI, auth = Auth}) ->
    AtmInventoryId = maps:get(<<"atmInventoryId">>, Req#el_req.data),

    Name = maps:get(<<"name">>, Req#el_req.data),
    Summary = maps:get(<<"summary">>, Req#el_req.data, ?DEFAULT_SUMMARY),
    Description = maps:get(<<"description">>, Req#el_req.data, ?DEFAULT_DESCRIPTION),

    OperationSpec = maps:get(<<"operationSpec">>, Req#el_req.data),
    ArgumentSpecs = maps:get(<<"argumentSpecs">>, Req#el_req.data),
    ResultSpecs = maps:get(<<"resultSpecs">>, Req#el_req.data),

    AtmLambda = #od_atm_lambda{
        name = Name,
        summary = Summary,
        description = Description,

        operation_spec = OperationSpec,
        argument_specs = ArgumentSpecs,
        result_specs = ResultSpecs,

        creator = aai:normalize_subject(Auth#auth.subject),
        creation_time = global_clock:timestamp_seconds()
    },

    case atm_lambda_validator:validate(AtmLambda) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end,

    {ok, #document{key = AtmLambdaId}} = od_atm_lambda:create(#document{value = AtmLambda}),
    entity_graph:add_relation(
        od_atm_lambda, AtmLambdaId,
        od_atm_inventory, AtmInventoryId
    ),
    {true, {FetchedAtmLambda, Rev}} = fetch_entity(#gri{aspect = instance, id = AtmLambdaId}),
    {ok, resource, {GRI#gri{id = AtmLambdaId}, {FetchedAtmLambda, Rev}}};

create(#el_req{gri = #gri{id = AtmLambdaId, aspect = {atm_inventory, AtmInventoryId}}}) ->
    entity_graph:add_relation(
        od_atm_lambda, AtmLambdaId,
        od_atm_inventory, AtmInventoryId
    ).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    {ok, AtmLambdaDocs} = od_atm_lambda:list(),
    {ok, [AtmLambdaId || #document{key = AtmLambdaId} <- AtmLambdaDocs]};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, AtmLambda) ->
    {ok, AtmLambda};

get(#el_req{gri = #gri{aspect = atm_inventories}}, AtmLambda) ->
    {ok, entity_graph:get_relations(direct, top_down, od_atm_inventory, AtmLambda)};

get(#el_req{gri = #gri{aspect = atm_workflow_schemas}}, AtmLambda) ->
    {ok, entity_graph:get_relations(direct, top_down, od_atm_workflow_schema, AtmLambda)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = AtmLambdaId, aspect = instance}, data = Data}) ->
    ?extract_ok(od_atm_lambda:update(AtmLambdaId, fun(AtmLambda) ->
        {ok, AtmLambda#od_atm_lambda{
            name = maps:get(<<"name">>, Data, AtmLambda#od_atm_lambda.name),
            summary = maps:get(<<"summary">>, Data, AtmLambda#od_atm_lambda.summary),
            description = maps:get(<<"description">>, Data, AtmLambda#od_atm_lambda.description)
        }}
    end)).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = AtmLambdaId, aspect = instance}}) ->
    entity_graph:delete_with_relations(od_atm_lambda, AtmLambdaId).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(#el_req{gri = #gri{id = Id}}, #od_atm_lambda{}) ->
    % All aspects exist if lambda record exists.
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
    atm_inventory_logic:has_eff_privilege(AtmInventoryId, UserId, ?ATM_INVENTORY_MANAGE_LAMBDAS);

authorize(#el_req{auth = ?USER(UserId), operation = create, gri = #gri{aspect = {atm_inventory, AtmInventoryId}}}, AtmLambda) ->
    can_manage_lambda(UserId, AtmLambda) andalso
        atm_inventory_logic:has_eff_privilege(AtmInventoryId, UserId, ?ATM_INVENTORY_MANAGE_LAMBDAS);

authorize(#el_req{auth = ?USER(UserId), operation = get}, #od_atm_lambda{atm_inventories = AtmInventories}) ->
    % All lambda resources can be accessed by the members of all automation inventories that include the lambda
    {ok, #document{value = User}} = od_user:get(UserId),
    lists:any(fun(AtmInventoryId) ->
        user_logic:has_eff_atm_inventory(User, AtmInventoryId)
    end, AtmInventories);

authorize(#el_req{auth = ?USER(UserId), operation = update, gri = #gri{aspect = instance}}, AtmLambda) ->
    can_manage_lambda(UserId, AtmLambda);

authorize(#el_req{auth = ?USER(UserId), operation = delete, gri = #gri{aspect = instance}}, AtmLambda) ->
    % @TODO VFS-7596 Temporary solution -> implement lambda removal with reference count checking
    can_manage_lambda(UserId, AtmLambda);

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

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {atm_inventory, _}}}) ->
    [?OZ_ATM_INVENTORIES_UPDATE];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = atm_inventories}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = atm_workflow_schemas}}) ->
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
            <<"name">> => {binary, name},

            <<"operationSpec">> => {{jsonable_record, single, atm_lambda_operation_spec}, fun(OperationSpec) ->
                Engine = atm_lambda_operation_spec:get_engine(OperationSpec),
                case lists:member(Engine, atm_lambda_operation_spec:allowed_engines_for_custom_lambdas()) of
                    true ->
                        true;
                    false ->
                        throw(?ERROR_BAD_VALUE_NOT_ALLOWED(<<"operationSpec.engine">>, lists:map(fun(AllowedEngine) ->
                            atm_lambda_operation_spec:engine_to_json(AllowedEngine)
                        end, atm_lambda_operation_spec:allowed_engines_for_custom_lambdas())))
                end
            end},
            <<"argumentSpecs">> => {{jsonable_record, list, atm_lambda_argument_spec}, any},
            <<"resultSpecs">> => {{jsonable_record, list, atm_lambda_result_spec}, any}
        },
        optional => #{
            <<"summary">> => {binary, {size_limit, ?SUMMARY_SIZE_LIMIT}},
            <<"description">> => {binary, {size_limit, ?DESCRIPTION_SIZE_LIMIT}}
        }
    };

validate(#el_req{operation = create, gri = #gri{aspect = {atm_inventory, _}}}) -> #{
};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"summary">> => {binary, {size_limit, ?SUMMARY_SIZE_LIMIT}},
        <<"description">> => {binary, {size_limit, ?DESCRIPTION_SIZE_LIMIT}}
    }
}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec can_manage_lambda(od_user:id(), od_atm_lambda:record()) -> boolean().
can_manage_lambda(UserId, #od_atm_lambda{atm_inventories = AtmInventories}) ->
    % A lambda can be updated by each user that has the manage lambdas privilege in at least one
    % of automation inventories that include the lambda
    lists:any(fun(AtmInventoryId) ->
        atm_inventory_logic:has_eff_privilege(AtmInventoryId, UserId, ?ATM_INVENTORY_MANAGE_LAMBDAS)
    end, AtmInventories).
