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

-include("automation.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/automation/automation.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").

%% entity logic API
-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).
%% convenience functions
-export([can_manage_lambda_in_any_inventory/2]).

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
operation_supported(create, parse_revision, public) -> true;
operation_supported(create, dump, private) -> true;
operation_supported(create, {revision, _}, private) -> true;
operation_supported(create, {dump_revision, _}, private) -> true;
operation_supported(create, {atm_inventory, _}, private) -> true;

operation_supported(get, list, private) -> true;
operation_supported(get, instance, private) -> true;
operation_supported(get, atm_inventories, private) -> true;
operation_supported(get, atm_workflow_schemas, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, {revision, _}, private) -> true;

operation_supported(delete, {atm_inventory, _}, private) -> true;

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
create(#el_req{gri = #gri{id = undefined, aspect = instance} = GRI, auth = Auth, data = Data}) ->
    AtmInventoryId = maps:get(<<"atmInventoryId">>, Data),
    OriginalAtmLambdaId = maps:get(<<"originalAtmLambdaId">>, Data, undefined),

    AtmLambdaWithEmptyRevisionRegistry = #od_atm_lambda{
        revision_registry = atm_lambda_revision_registry:empty(),
        original_atm_lambda = OriginalAtmLambdaId,
        creator = aai:normalize_subject(Auth#auth.subject),
        creation_time = global_clock:timestamp_seconds()
    },

    InitialRevisionData = lookup_and_sanitize_revision_data(Data),
    case add_revision_to_lambda(AtmLambdaWithEmptyRevisionRegistry, <<"auto">>, InitialRevisionData) of
        {error, _} = Error ->
            Error;
        {ok, AtmLambda} ->
            {ok, #document{key = AtmLambdaId}} = od_atm_lambda:create(#document{value = AtmLambda}),

            od_atm_inventory:critical_section(AtmInventoryId, fun() ->
                od_atm_lambda:critical_section(AtmLambdaId, fun() ->
                    entity_graph:add_relation(
                        od_atm_lambda, AtmLambdaId,
                        od_atm_inventory, AtmInventoryId
                    )
                end)
            end),
            {true, {FetchedAtmLambda, Rev}} = fetch_entity(#gri{aspect = instance, id = AtmLambdaId}),
            {ok, resource, {GRI#gri{id = AtmLambdaId}, {FetchedAtmLambda, Rev}}}
    end;

create(#el_req{gri = #gri{id = undefined, aspect = parse_revision, scope = public}, data = Data}) ->
    case validate_revision(Data) of
        {ok, AtmLambdaRevision} ->
            {ok, value, AtmLambdaRevision};
        {error, _} = Error ->
            Error
    end;

create(#el_req{gri = #gri{id = AtmLambdaId, aspect = dump}, data = Data}) ->
    fun(AtmLambda) ->
        IncludedRevision = maps:get(<<"includeRevision">>, Data),

        atm_lambda_revision_registry:has_revision(
            IncludedRevision, AtmLambda#od_atm_lambda.revision_registry
        ) orelse throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"includeRevision">>)),

        {ok, value, od_atm_lambda:dump_to_json(
            AtmLambdaId, AtmLambda, IncludedRevision
        )}
    end;

create(#el_req{gri = #gri{id = AtmLambdaId, aspect = {revision, RevisionNumberBinary}}, data = Data}) ->
    ?extract_ok(od_atm_lambda:update(AtmLambdaId, fun(AtmLambda) ->
        add_revision_to_lambda(AtmLambda, RevisionNumberBinary, Data)
    end));

create(#el_req{gri = #gri{aspect = {dump_revision, RevisionNumberBinary}}}) ->
    fun(AtmLambda) ->
        {ok, value, od_atm_lambda:dump_revision_to_json(
            AtmLambda, binary_to_integer(RevisionNumberBinary)
        )}
    end;

create(#el_req{gri = #gri{id = AtmLambdaId, aspect = {atm_inventory, AtmInventoryId}}}) ->
    od_atm_inventory:critical_section(AtmInventoryId, fun() ->
        od_atm_lambda:critical_section(AtmLambdaId, fun() ->
            entity_graph:add_relation(
                od_atm_lambda, AtmLambdaId,
                od_atm_inventory, AtmInventoryId
            )
        end)
    end).


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
    RevisionData = lookup_and_sanitize_revision_data(Data),
    ?extract_ok(od_atm_lambda:update(AtmLambdaId, fun(AtmLambda) ->
        add_revision_to_lambda(AtmLambda, <<"auto">>, RevisionData)
    end));

update(#el_req{gri = #gri{id = AtmLambdaId, aspect = {revision, RevisionNumberBinary}}, data = Data}) ->
    NewState = automation:lifecycle_state_from_json(maps:get(<<"state">>, Data)),
    ?extract_ok(od_atm_lambda:update(AtmLambdaId, fun(AtmLambda) ->
        {ok, AtmLambda#od_atm_lambda{
            revision_registry = atm_lambda_revision_registry:update_revision_lifecycle_state(
                binary_to_integer(RevisionNumberBinary),
                NewState,
                AtmLambda#od_atm_lambda.revision_registry
            )
        }}
    end)).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = AtmLambdaId, aspect = {atm_inventory, AtmInventoryId}}}) ->
    od_atm_inventory:critical_section(AtmInventoryId, fun() ->
        od_atm_lambda:critical_section(AtmLambdaId, fun() ->
            {ok, #document{value = AtmLambda}} = od_atm_lambda:get(AtmLambdaId),
            case AtmLambda of
                #od_atm_lambda{atm_workflow_schemas = []} ->
                    ok;
                #od_atm_lambda{atm_workflow_schemas = LambdaWorkflowSchemas} ->
                    {ok, #document{
                        value = #od_atm_inventory{
                            atm_workflow_schemas = InventoryWorkflowSchemas
                        }
                    }} = od_atm_inventory:get(AtmInventoryId),
                    case lists_utils:intersect(LambdaWorkflowSchemas, InventoryWorkflowSchemas) of
                        [] ->
                            ok;
                        ConflictingAtmWorkflowSchemas ->
                            throw(?ERROR_ATM_LAMBDA_IN_USE(ConflictingAtmWorkflowSchemas))
                    end
            end,

            entity_graph:remove_relation(
                od_atm_lambda, AtmLambdaId,
                od_atm_inventory, AtmInventoryId
            ),
            % remove the lambda if it was its last inventory
            case AtmLambda of
                #od_atm_lambda{atm_inventories = [AtmInventoryId]} ->
                    entity_graph:delete_with_relations(od_atm_lambda, AtmLambdaId);
                _ ->
                    ok
            end
        end)
    end).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(#el_req{gri = #gri{aspect = {atm_inventory, AtmInventoryId}}}, AtmLambda) ->
    entity_graph:has_relation(direct, top_down, od_atm_inventory, AtmInventoryId, AtmLambda);

exists(#el_req{gri = #gri{aspect = {revision, RevisionNumberBin}}}, #od_atm_lambda{
    revision_registry = RevisionRegistry
}) ->
    atm_lambda_revision_registry:has_revision(binary_to_integer(RevisionNumberBin), RevisionRegistry);

exists(#el_req{}, FetchedAtmLambda) ->
    % all other aspects exist if atm_lambda record exists
    FetchedAtmLambda =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(#el_req{auth = ?USER(UserId), operation = create, gri = #gri{id = undefined, aspect = instance}, data = Data}, _) ->
    AtmInventoryId = maps:get(<<"atmInventoryId">>, Data, <<"">>),
    can_manage_lambdas_in_specific_inventory(UserId, AtmInventoryId);

authorize(#el_req{operation = create, gri = #gri{id = undefined, aspect = parse_revision, scope = public}}, _) ->
    true;

authorize(#el_req{auth = ?USER(UserId), operation = create, gri = #gri{aspect = dump}}, AtmLambda) ->
    can_view_lambda_in_any_inventory(UserId, AtmLambda);

authorize(#el_req{auth = ?USER(UserId), operation = create, gri = #gri{aspect = {revision, _}}}, AtmLambda) ->
    can_manage_lambda_in_any_inventory(UserId, AtmLambda);

authorize(#el_req{auth = ?USER(UserId), operation = create, gri = #gri{aspect = {dump_revision, _}}}, AtmLambda) ->
    can_view_lambda_in_any_inventory(UserId, AtmLambda);

authorize(#el_req{auth = ?USER(UserId), operation = create, gri = #gri{aspect = {atm_inventory, AtmInventoryId}}}, AtmLambda) ->
    can_manage_lambda_in_any_inventory(UserId, AtmLambda) andalso
        can_manage_lambdas_in_specific_inventory(UserId, AtmInventoryId);

authorize(#el_req{auth = ?USER(UserId), operation = get}, AtmLambda) ->
    can_view_lambda_in_any_inventory(UserId, AtmLambda);

authorize(#el_req{auth = ?USER(UserId), operation = update, gri = #gri{aspect = instance}}, AtmLambda) ->
    can_manage_lambda_in_any_inventory(UserId, AtmLambda);

authorize(#el_req{auth = ?USER(UserId), operation = update, gri = #gri{aspect = {revision, _}}}, AtmLambda) ->
    can_manage_lambda_in_any_inventory(UserId, AtmLambda);

authorize(#el_req{auth = ?USER(UserId), operation = delete, gri = #gri{aspect = {atm_inventory, AtmInventoryId}}}, _) ->
    can_manage_lambdas_in_specific_inventory(UserId, AtmInventoryId);

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
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {revision, _}}}) ->
    [?OZ_ATM_INVENTORIES_UPDATE];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {atm_inventory, _}}}) ->
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
validate(#el_req{operation = create, gri = #gri{aspect = instance}}) ->
    #{
        required => #{
            <<"atmInventoryId">> => {binary, {exists, fun atm_inventory_logic:exists/1}},
            % validation of revision data is performed during the lambda creation procedure
            <<"revision">> => {json, any}
        },
        optional => #{
            % this can be an arbitrary string and is not verified, as for example a dump
            % from another Onezone may be used to create a lambda
            <<"originalAtmLambdaId">> => {binary, non_empty},
            <<"schemaFormatVersion">> => ?SCHEMA_FORMAT_VERSION_SPEC
        }
    };

validate(#el_req{operation = create, gri = #gri{aspect = parse_revision}}) -> #{
    required => #{
        <<"atmLambdaRevision">> => {{jsonable_record, single, atm_lambda_revision}, any}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = dump}}) -> #{
    required => #{
        <<"includeRevision">> => {integer, {not_lower_than, 1}}
    }
};

validate(#el_req{operation = create, data = Data, gri = #gri{aspect = {revision, TargetRevisionNumber}}}) ->
    SchemaFormatVersion = maps:get(<<"schemaFormatVersion">>, Data, undefined),
    revision_sanitizer_spec(TargetRevisionNumber, SchemaFormatVersion);

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

validate(#el_req{operation = create, gri = #gri{aspect = {atm_inventory, _}}}) -> #{
};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    required => #{
        % validation of revision data is performed during the lambda update procedure
        <<"revision">> => {json, any}
    },
    optional => #{<<"schemaFormatVersion">> => ?SCHEMA_FORMAT_VERSION_SPEC}
};

validate(#el_req{operation = update, gri = #gri{aspect = {revision, _}}}) -> #{
    required => #{
        <<"state">> => {binary, lists:map(fun automation:lifecycle_state_to_json/1, automation:all_lifecycle_states())}
    }
}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec revision_sanitizer_spec(
    binary(),
    undefined | ?MIN_SUPPORTED_SCHEMA_FORMAT_VERSION..?CURRENT_SCHEMA_FORMAT_VERSION
) ->
    entity_logic_sanitizer:sanitizer_spec().
revision_sanitizer_spec(TargetRevisionNumber, SchemaFormatVersion) ->
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
        <<"atmLambdaRevision">> => begin
            Decoder = case SchemaFormatVersion of
                undefined -> jsonable_record;
                2 -> jsonable_record;
                _ -> persistent_record
            end,
            {{Decoder, single, atm_lambda_revision}, any}
        end
    },

    #{
        required => case TargetRevisionNumber of
            <<"auto">> ->
                CommonRequired#{<<"originalRevisionNumber">> => {integer, {not_lower_than, 1}}};
            _ ->
                CommonRequired
        end,
        optional => #{<<"schemaFormatVersion">> => ?SCHEMA_FORMAT_VERSION_SPEC}
    }.


%% @private
-spec lookup_and_sanitize_revision_data(entity_logic:data()) ->
    entity_logic:sanitized_data() | no_return().
lookup_and_sanitize_revision_data(Data) ->
    RevisionData = maps:get(<<"revision">>, Data),
    SchemaFormatVersion = maps:get(<<"schemaFormatVersion">>, RevisionData, undefined),

    entity_logic_sanitizer:ensure_valid(
        revision_sanitizer_spec(<<"auto">>, SchemaFormatVersion),
        {revision, <<"auto">>},
        RevisionData
    ).


%% @private
-spec add_revision_to_lambda(od_atm_lambda:record(), binary(), entity_logic:sanitized_data()) ->
    {ok, od_atm_lambda:record()} | errors:error().
add_revision_to_lambda(AtmLambda = #od_atm_lambda{
    revision_registry = RevisionRegistry
}, RevisionNumberBinary, SanitizedRevisionData) ->
    case validate_revision(SanitizedRevisionData) of
        {error, _} = Error ->
            Error;
        {ok, AtmLambdaRevision} ->
            TargetRevisionNumber = case RevisionNumberBinary of
                <<"auto">> -> maps:get(<<"originalRevisionNumber">>, SanitizedRevisionData);
                _ -> binary_to_integer(RevisionNumberBinary)
            end,
            case atm_lambda_revision_registry:has_revision(TargetRevisionNumber, RevisionRegistry) of
                true ->
                    ?ERROR_ALREADY_EXISTS;
                false ->
                    {ok, AtmLambda#od_atm_lambda{
                        revision_registry = atm_lambda_revision_registry:add_revision(
                            TargetRevisionNumber, AtmLambdaRevision, AtmLambda#od_atm_lambda.revision_registry
                        )
                    }}
            end
    end.


%% @private
-spec validate_revision(entity_logic:sanitized_data()) -> {ok, atm_lambda_revision:record()} | errors:error().
validate_revision(SanitizedRevisionData) ->
    AtmLambdaRevision = maps:get(<<"atmLambdaRevision">>, SanitizedRevisionData),
    try atm_lambda_validator:validate(AtmLambdaRevision) of
        ok ->
            {ok, AtmLambdaRevision}
    catch
        throw:{error, _} = Error ->
            Error
    end.


%% @private
-spec can_view_lambda_in_any_inventory(od_user:id(), od_atm_lambda:record()) -> boolean().
can_view_lambda_in_any_inventory(UserId, #od_atm_lambda{atm_inventories = AtmInventories}) ->
    % All lambda resources can be accessed by the members of all automation inventories that include the lambda
    {ok, #document{value = User}} = od_user:get(UserId),
    lists:any(fun(AtmInventoryId) ->
        user_logic:has_eff_atm_inventory(User, AtmInventoryId)
    end, AtmInventories).


%% @private
-spec can_manage_lambdas_in_specific_inventory(od_user:id(), od_atm_inventory:id()) -> boolean().
can_manage_lambdas_in_specific_inventory(UserId, AtmInventoryId) ->
    atm_inventory_logic:has_eff_privilege(AtmInventoryId, UserId, ?ATM_INVENTORY_MANAGE_LAMBDAS).


%% @private
-spec can_manage_lambda_in_any_inventory(od_user:id(), od_atm_lambda:record() | od_atm_lambda:id()) -> boolean().
can_manage_lambda_in_any_inventory(UserId, AtmLambdaId) when is_binary(AtmLambdaId) ->
    case od_atm_lambda:get(AtmLambdaId) of
        {ok, #document{value = AtmLambda}} ->
            can_manage_lambda_in_any_inventory(UserId, AtmLambda);
        {error, not_found} ->
            false
    end;
can_manage_lambda_in_any_inventory(UserId, #od_atm_lambda{atm_inventories = AtmInventories}) ->
    % A lambda can be managed by each user that has the manage lambdas privilege in at least one
    % of automation inventories that include the lambda
    lists:any(fun(AtmInventoryId) ->
        can_manage_lambdas_in_specific_inventory(UserId, AtmInventoryId)
    end, AtmInventories).
