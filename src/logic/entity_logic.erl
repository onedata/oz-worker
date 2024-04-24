%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all common logic concerning entities in onezone.
%%% It is used to process entity requests is a standardized way, i.e.:
%%%     # checks existence of given entity
%%%     # checks authorization of client to perform certain action
%%%     # checks validity of data provided in the request
%%%     # handles all errors in a uniform way
%%% It uses a callback system, accepting modules that implement
%%% entity_logic_plugin_behaviour.
%%% @end
%%%-------------------------------------------------------------------
-module(entity_logic).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").

% Some of the types are just aliases for types from gs_protocol, this is
% for better readability of logic modules.
-type req() :: #el_req{}.
-type el_plugin() :: module().
-type operation() :: gs_protocol:operation().
-type entity_id() :: undefined | od_user:id() | od_group:id() | od_space:id()
| od_share:id() | od_provider:id() | od_handle_service:id() | od_handle:id()
| od_cluster:id() | od_storage:id()
| od_atm_inventory:id() | od_atm_lambda:id() | od_atm_workflow_schema:id().
-type entity_type() :: od_user | od_group | od_space | od_share | od_provider
| od_handle_service | od_handle | od_harvester | od_cluster | od_storage
| oz_privileges | temporary_token_secret
| od_atm_inventory | od_atm_lambda | od_atm_workflow_schema.
-type entity() :: undefined | #od_user{} | #od_group{} | #od_space{} |
#od_share{} | #od_provider{} | #od_handle_service{} | #od_handle{}
| #od_harvester{} | #od_cluster{} | #od_storage{}
| #temporary_token_secret{}
| #od_atm_inventory{} | #od_atm_lambda{} | #od_atm_workflow_schema{}.
-type revision() :: gs_protocol:revision().
-type versioned_entity() :: gs_protocol:versioned_entity().
-type aspect() :: gs_protocol:aspect().
-type scope() :: gs_protocol:scope().
-type data_format() :: gs_protocol:data_format().
-type sanitized_data() :: map().
-type data() :: gs_protocol:data() | sanitized_data().
-type gri() :: gri:gri().
-type auth_hint() :: gs_protocol:auth_hint().

-type create_result() :: gs_protocol:graph_create_result() | fun((entity()) -> gs_protocol:graph_create_result()).
-type get_result() :: gs_protocol:graph_get_result() | {ok, term()} | {ok, gri(), term()}.
-type delete_result() :: gs_protocol:graph_delete_result() | fun((entity()) -> gs_protocol:graph_delete_result()).
-type update_result() :: gs_protocol:graph_update_result() | fun((entity()) -> gs_protocol:graph_update_result()).
-type result() :: create_result() | get_result() | update_result() | delete_result().

% Common fields for all records
-type creation_time() :: time:seconds().

-export_type([
    req/0,
    el_plugin/0,
    operation/0,
    entity_id/0,
    entity_type/0,
    entity/0,
    revision/0,
    versioned_entity/0,
    aspect/0,
    scope/0,
    gri/0,
    data_format/0,
    data/0,
    sanitized_data/0,
    auth_hint/0,
    create_result/0,
    get_result/0,
    update_result/0,
    delete_result/0,
    result/0,
    creation_time/0
]).

% Internal record containing the request data and state.
-record(state, {
    req = #el_req{} :: req(),
    plugin = undefined :: el_plugin(),
    versioned_entity = {undefined, 1} :: versioned_entity()
}).

-export([handle/1, handle/2]).
-export([is_authorized/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Handles an entity logic request expressed by a #el_req{} record.
%% @end
%%--------------------------------------------------------------------
-spec handle(req()) -> result().
handle(ElReq) ->
    handle(ElReq, {undefined, 1}).


%%--------------------------------------------------------------------
%% @doc
%% Handles an entity logic request expressed by a #el_req{} record. Entity can
%% be provided if it was prefetched.
%% @end
%%--------------------------------------------------------------------
-spec handle(req(), VersionedEntity :: versioned_entity()) -> result().
handle(#el_req{gri = #gri{type = EntityType}} = ElReq, VersionedEntity) ->
    ElPlugin = EntityType:entity_logic_plugin(),
    ?catch_exceptions(handle_unsafe(#state{
        req = ElReq, plugin = ElPlugin, versioned_entity = VersionedEntity
    })).


%%--------------------------------------------------------------------
%% @doc
%% Return if given client is authorized to perform given request, as specified
%% in the #el_req{} record. Entity can be provided if it was prefetched.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(req(), versioned_entity()) -> {true, gri:gri()} | false.
is_authorized(#el_req{gri = #gri{type = EntityType}} = ElReq, VersionedEntity) ->
    try
        ElPlugin = EntityType:entity_logic_plugin(),
        % Existence must be checked too, as sometimes authorization depends
        % on that.
        NewState = ensure_authorized(
            ensure_exists(#state{
                req = ElReq, plugin = ElPlugin, versioned_entity = VersionedEntity
            })),
        {true, NewState#state.req#el_req.gri}
    catch
        _:_ ->
            false
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Handles an entity logic request based on operation,
%% should be wrapped in a try-catch.
%% @end
%%--------------------------------------------------------------------
-spec handle_unsafe(#state{}) -> result().
handle_unsafe(State = #state{req = Req = #el_req{operation = create}}) ->
    Result = call_create(
        ensure_valid(
            ensure_authorized(
                fetch_entity(
                    ensure_operation_supported(
                        State))))),
    case {Result, Req} of
        {{ok, resource, Resource}, #el_req{gri = #gri{aspect = instance}, auth = Auth}} ->
            % If an entity instance is created, log an information about it
            % (it's a significant operation and this information might be useful).
            {EntType, EntId} = case Resource of
                {#gri{type = Type, id = Id}, _} -> {Type, Id};
                {#gri{type = Type, id = Id}, _, _} -> {Type, Id}
            end,
            ?debug("~s has been created by client: ~s", [
                EntType:to_string(EntId),
                aai:auth_to_printable(Auth)
            ]);
        _ ->
            ok
    end,
    Result;


handle_unsafe(State = #state{req = #el_req{operation = get}}) ->
    NewState = ensure_authorized(
        ensure_exists(
            fetch_entity(
                ensure_operation_supported(
                    State)))),
    case call_get(NewState) of
        {ok, GetResult} ->
            % Return the new GRI if auto scope was requested
            case State#state.req#el_req.gri#gri.scope of
                auto -> {ok, NewState#state.req#el_req.gri, GetResult};
                _ -> {ok, GetResult}
            end;
        Other ->
            Other
    end;


handle_unsafe(State = #state{req = #el_req{operation = update}}) ->
    call_update(
        ensure_valid(
            ensure_authorized(
                ensure_exists(
                    fetch_entity(
                        ensure_operation_supported(
                            State))))));

handle_unsafe(State = #state{req = Req = #el_req{operation = delete}}) ->
    Result = call_delete(
        ensure_authorized(
            ensure_exists(
                fetch_entity(
                    ensure_operation_supported(
                        State))))),
    case {Result, Req} of
        {ok, #el_req{gri = #gri{type = Type, id = Id, aspect = instance}, auth = Auth}} ->
            % If an entity instance is deleted, log an information about it
            % (it's a significant operation and this information might be useful).
            ?debug("~s has been deleted by client: ~s", [
                Type:to_string(Id),
                aai:auth_to_printable(Auth)
            ]),
            ok;
        _ ->
            Result
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves the entity specified in request by calling back proper entity
%% logic plugin. Does nothing if the entity is prefetched, GRI of the
%% request is not related to any entity or fetching is not applicable to
%% given operation.
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(State :: #state{}) -> #state{}.
fetch_entity(State = #state{versioned_entity = {Entity, _}}) when Entity /= undefined ->
    State;
fetch_entity(State = #state{req = #el_req{gri = #gri{id = undefined}}}) ->
    State;
fetch_entity(State = #state{req = #el_req{operation = create, gri = #gri{aspect = instance}}}) ->
    % Skip when creating an instance with predefined Id
    State#state{versioned_entity = {undefined, 1}};
fetch_entity(State) ->
    case call_plugin(fetch_entity, State) of
        {true, {Entity, Revision}} ->
            % The entity was fetched
            State#state{versioned_entity = {Entity, Revision}};
        false ->
            % Fetch is not applicable to this operation, execution should continue
            State#state{versioned_entity = {undefined, 1}};
        {error, _} = Error ->
            % There was an error, fail
            throw(Error)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates an aspect of entity specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_create(State :: #state{}) -> create_result().
call_create(State = #state{versioned_entity = {_, InheritedRev}}) ->
    case call_plugin(create, State) of
        {ok, resource, {ResultGRI, {ResData, inherit_rev}}} ->
            {ok, resource, {ResultGRI, {ResData, InheritedRev}}};
        {ok, resource, {ResultGRI, {ResData, NewRev}}} ->
            {ok, resource, {ResultGRI, {ResData, NewRev}}};
        {ok, resource, {ResultGRI, NAuthHint, {ResData, inherit_rev}}} ->
            {ok, resource, {ResultGRI, NAuthHint, {ResData, InheritedRev}}};
        {ok, resource, {ResultGRI, NAuthHint, {ResData, NewRev}}} ->
            {ok, resource, {ResultGRI, NAuthHint, {ResData, NewRev}}};
        Other ->
            Other
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves an aspect specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_get(State :: #state{}) -> get_result().
call_get(State = #state{req = #el_req{return_revision = true}, versioned_entity = {_, Rev}}) ->
    case call_plugin(get, State) of
        {ok, Data} -> {ok, {Data, Rev}};
        {error, _} = Error -> Error
    end;
call_get(State) ->
    call_plugin(get, State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates an aspect of entity specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_update(State :: #state{}) -> update_result().
call_update(State) ->
    call_plugin(update, State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes an aspect of entity specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_delete(State :: #state{}) -> delete_result().
call_delete(State) ->
    call_plugin(delete, State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures requested operation is supported by calling back
%% proper entity logic plugin, throws a proper error of not.
%% @end
%%--------------------------------------------------------------------
-spec ensure_operation_supported(#state{}) -> #state{}.
ensure_operation_supported(State = #state{req = #el_req{gri = #gri{scope = auto}}}) ->
    % If auto scope is requested, defer the check until ensure_authorized
    State;
ensure_operation_supported(State) ->
    case ensure_operation_supported_internal(State) of
        true -> State;
        false -> throw(?ERROR_NOT_SUPPORTED)
    end.


-spec ensure_operation_supported_internal(#state{}) -> boolean().
ensure_operation_supported_internal(State) ->
    try
        call_plugin(operation_supported, State)
    catch
        throw:{error, _} = Error ->
            throw(Error);
        _:_ ->
            % No need for log here, 'operation_supported' may crash depending on
            % what the request contains and this is expected.
            false
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures aspect of entity specified in request exists, throws on error.
%% @end
%%--------------------------------------------------------------------
-spec ensure_exists(#state{}) -> #state{}.
ensure_exists(State) ->
    case ensure_exists_internal(State) of
        true -> State;
        false -> throw(?ERROR_NOT_FOUND)
    end.


-spec ensure_exists_internal(#state{}) -> boolean().
ensure_exists_internal(#state{req = #el_req{gri = #gri{id = undefined}}}) ->
    true;
ensure_exists_internal(State) ->
    try
        call_plugin(exists, State)
    catch _:_ ->
        % No need for log here, 'exists' may crash depending on what the
        % request contains and this is expected.
        false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures client specified in request is authorized to perform the request,
%% throws on error.
%% @end
%%--------------------------------------------------------------------
-spec ensure_authorized(State :: #state{}) -> #state{}.
ensure_authorized(State = #state{req = #el_req{gri = #gri{scope = auto}}}) ->
    % Resolve auto scope - maximum scope allowed for given client and place it
    % in state, which will result in specific scope being used in the operation.
    case resolve_auto_scope(State) of
        {true, NewState} -> NewState;
        false -> report_unauthorized(State)
    end;
ensure_authorized(State) ->
    case ensure_authorized_internal(State) of
        true -> State;
        false -> report_unauthorized(State)
    end.


-spec ensure_authorized_internal(#state{}) -> boolean().
ensure_authorized_internal(#state{req = #el_req{auth = ?ROOT}}) ->
    % Root client is authorized to do everything (that client is only available
    % internally).
    true;
ensure_authorized_internal(State) ->
    ensure_authorized_regarding_api_caveats(State),
    is_client_authorized(State) orelse is_authorized_as_admin(State).


-spec resolve_auto_scope(#state{}) -> false | {true, #state{}}.
resolve_auto_scope(State) ->
    resolve_auto_scope([private, protected, shared, public], State).

-spec resolve_auto_scope([gs_protocol:scope()], #state{}) -> false | {true, #state{}}.
resolve_auto_scope([], _State) ->
    false;
resolve_auto_scope([Scope | Rest], State = #state{req = Req = #el_req{operation = Operation, gri = GRI}}) ->
    StateWithScope = State#state{req = Req#el_req{gri = GRI#gri{scope = Scope}}},
    Authorized = ensure_operation_supported_internal(StateWithScope) andalso
        (Operation == create orelse ensure_exists_internal(StateWithScope)) andalso
        ensure_authorized_internal(StateWithScope),
    case Authorized of
        true -> {true, StateWithScope};
        false -> resolve_auto_scope(Rest, State)
    end.


-spec is_client_authorized(#state{}) -> boolean().
is_client_authorized(State) ->
    try
        call_plugin(authorize, State)
    catch
        throw:{error, _} = Error ->
            throw(Error);
        _:_ ->
            false
    end.


-spec is_authorized_as_admin(#state{}) -> boolean().
is_authorized_as_admin(#state{req = ElReq} = State) ->
    try
        case call_plugin(required_admin_privileges, State) of
            forbidden ->
                false;
            Privileges ->
                lists:all(fun(Privilege) ->
                    user_logic_plugin:auth_by_oz_privilege(ElReq, Privilege)
                end, Privileges)
        end
    catch _:_ ->
        false
    end.


-spec ensure_authorized_regarding_api_caveats(#state{}) -> ok | no_return().
ensure_authorized_regarding_api_caveats(#state{req = #el_req{auth = Auth, operation = Operation, gri = GRI}}) ->
    case api_auth:check_authorization(Auth, ?OZ_WORKER, Operation, GRI) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end.


-spec report_unauthorized(#state{}) -> no_return().
report_unauthorized(#state{req = #el_req{auth = ?NOBODY}}) ->
    % The client was not authenticated -> unauthorized
    throw(?ERROR_UNAUTHORIZED);
report_unauthorized(_) ->
    % The client was authenticated but cannot access the aspect -> forbidden
    throw(?ERROR_FORBIDDEN).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures data specified in request is valid, throws on error.
%% @end
%%--------------------------------------------------------------------
-spec ensure_valid(State :: #state{}) -> #state{}.
ensure_valid(#state{req = #el_req{gri = #gri{aspect = Aspect}, data = Data} = Req} = State) ->
    SanitizerSpec = call_plugin(validate, State),
    SanitizedData = entity_logic_sanitizer:ensure_valid(SanitizerSpec, Aspect, Data),
    State#state{req = Req#el_req{data = SanitizedData}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs operation on proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_plugin(Operation :: atom(), #state{}) -> term().
call_plugin(fetch_entity, #state{plugin = Plugin, req = #el_req{gri = GRI}}) ->
    Plugin:fetch_entity(GRI);
call_plugin(operation_supported, #state{plugin = Plugin, req = #el_req{operation = Operation, gri = GRI}}) ->
    Plugin:operation_supported(Operation, GRI#gri.aspect, GRI#gri.scope);
call_plugin(exists, #state{plugin = Plugin, req = ElReq, versioned_entity = {Entity, _}}) ->
    Plugin:exists(ElReq, Entity);
call_plugin(authorize, #state{plugin = Plugin, req = ElReq, versioned_entity = {Entity, _}}) ->
    Plugin:authorize(ElReq, Entity);
call_plugin(required_admin_privileges, #state{plugin = Plugin, req = ElReq}) ->
    Plugin:required_admin_privileges(ElReq);
call_plugin(get, #state{plugin = Plugin, req = ElReq, versioned_entity = {Entity, _}}) ->
    Plugin:get(ElReq, Entity);
call_plugin(Operation, #state{plugin = Plugin, req = ElReq, versioned_entity = {Entity, _}}) ->
    % covers create, update, delete, validate
    case Plugin:Operation(ElReq) of
        Fun when is_function(Fun, 1) ->
            Fun(Entity);
        Result ->
            Result
    end.
