%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements logic that guards the consistency of entity graph.
%%% Entity graph concerns all entities in the system that are related to each
%%% other and decide of user's privileges in the system. These models are:
%%%     - od_user
%%%     - od_group
%%%     - od_space
%%%     - od_provider
%%%     - od_handle_service
%%%     - od_handle
%%% There are two main jobs of the entity graph:
%%%     - encapsulate all logic concerning relations between entities
%%%     - ensure that effective relations are always up to date
%%% The graph can be in two states:
%%%   - up-to-date: all entities have their effective relations up-to-date,
%%%   - dirty: some documents require recalculation.
%%% All operations concerning effective graph state are guarded by a lock.
%%% Recalculations are scheduled after a relation has been modified - during
%%% documents update, they are marked as dirty, and added to the queue in
%%% effective graph state to be processed. After that, the modifying process
%%% schedules a refresh by spawning an async process.
%%% Operations that concern group relations cannot be performed while
%%% effective graph is dirty. They will wait on critical section lock until
%%% the graph has been fully recalculated. This is because groups can be
%%% nested, but cycles must be disallowed.
%%% Other operations can be performed in parallel, as effective graph logic
%%% operates only on effective relations, so no conflicts are possible.
%%% The relations introduced by other operations do not have possible negative
%%% effects, they will just mark some records as dirty and the effective graph
%%% will become eventually consistent.
%%% Graph refreshes can be scheduled multiple times, only one process at a time
%%% can recalculate the graph, and if the graph is already up-to-date, the
%%% refresh does nothing.
%%% The refreshing of effective relations goes as follows:
%%% 1) bottom-up traversing:
%%%     1.1) groups with least children to most children
%%%     1.2) spaces, handles, handle_services
%%%     1.3) providers
%%% 2) top-down traversing:
%%%     2.1) spaces
%%%     2.2) groups with least parents to most parents
%%%     2.3) users
%%% When a record is marked dirty, it is given a priority and the list is
%%% sorted by priorities, so later they can be taken one by one and processed.
%%% @end
%%%-------------------------------------------------------------------
-module(entity_graph).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("errors.hrl").

-define(ENTITY_GRAPH_LOCK, entity_graph).
-define(STATE_KEY, <<"entity_graph_state">>).

% Data types that hold different types of relations.
-type relation(EntityId) :: [EntityId].
-type relation_with_attrs(EntityId, Attributes) :: #{EntityId => Attributes}.
% Data types that hold different types of effective relations. {Type, Id} pairs
% hold pairs of {record type, record id} through which the effective
% relation exists (intermediaries).
% There may be multiple such pairs on the list. If the effective neighbour
% has a direct relation to the entity, it will be listed there (which means
% that it belongs "by itself").
-type eff_relation(EntityId) :: #{EntityId => [{Type :: atom(), Id :: binary()}]}.
-type eff_relation_with_attrs(EntityId, Attributes) ::
#{EntityId => {Attributes, [{Type :: atom(), Id :: binary()}]}}.

% Types of operations on privileges
-type privileges_operation() :: set | grant | revoke.

-export_type([
    relation/1, relation_with_attrs/2,
    eff_relation/1, eff_relation_with_attrs/2,
    privileges_operation/0
]).

% Internal types
% Direction in effective graph
-type direction() :: top_down | bottom_up.
-type entity_id() :: od_user:id() | od_group:id() | od_space:id() |
od_share:id() | od_provider:id() | od_handle_service:id() | od_handle:id().
% OZ privileges are treated differently, but the recalculation process is
% in large part the same as other relations.
-type entity_type() :: od_user | od_group | od_space |
od_share | od_provider | od_handle_service | od_handle | oz_privileges.
-type entity() :: #od_user{} | #od_group{} | #od_space{} |
#od_share{} | #od_provider{} | #od_handle_service{} | #od_handle{}.
% Relation attributes
-type attributes() :: term().

-type map_of_relations() :: #{entity_type() => relation(entity_id())}.
-type map_of_eff_relations() ::
#{entity_type() => [eff_relation(entity_id()) | privileges:oz_privilege()]}.
-type map_of_eff_relations_with_attrs() ::
#{entity_type() => [eff_relation_with_attrs(entity_id(), attributes())]}.
-type map_of_any_eff_relations() ::
#{entity_type() =>
eff_relation(entity_id()) | eff_relation_with_attrs(entity_id(), attributes()) | [privileges:oz_privilege()]
}.

%% API
-export([init_state/0]).
-export([schedule_refresh/0, ensure_up_to_date/0]).
-export([add_relation/4, add_relation/5]).
-export([update_relation/5]).
-export([remove_relation/4]).
-export([delete_with_relations/2]).
-export([update_oz_privileges/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes effective graph state, which is a globally shared singleton.
%% Should be run during node initialization. The critical section makes sure
%% that only one state record creation is attempted.
%% @end
%%--------------------------------------------------------------------
-spec init_state() -> ok.
init_state() ->
    critical_section:run(?ENTITY_GRAPH_LOCK, fun() ->
        case entity_graph_state:get(?STATE_KEY) of
            {ok, #document{value = #entity_graph_state{}}} ->
                ok;
            {error, {not_found, entity_graph_state}} ->
                {ok, ?STATE_KEY} = entity_graph_state:create(
                    #document{key = ?STATE_KEY, value = #entity_graph_state{}}
                ),
                ok
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Spawns an async process that will attempt to refresh the entity graph.
%% @end
%%--------------------------------------------------------------------
-spec schedule_refresh() -> ok.
schedule_refresh() ->
    spawn(fun refresh_if_needed/0),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Waits until entity graph is recalculated and returns true. Returns false upon
%% failure (10 retries, with 3 second interval between them).
%% @end
%%--------------------------------------------------------------------
-spec ensure_up_to_date() -> boolean().
ensure_up_to_date() ->
    ensure_up_to_date(10).
ensure_up_to_date(0) ->
    false;
ensure_up_to_date(Retries) ->
    Result = critical_section:run(?ENTITY_GRAPH_LOCK, fun() ->
        case get_state() of
            #entity_graph_state{bottom_up_dirty = [], top_down_dirty = []} ->
                true;
            _ ->
                false
        end
    end),
    case Result of
        true ->
            true;
        false ->
            schedule_refresh(),
            timer:sleep(3000),
            ensure_up_to_date(Retries - 1)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Adds a relation between given entities. Used for relations without attributes.
%% @end
%%--------------------------------------------------------------------
-spec add_relation(ChildType :: entity_type(), ChildId :: entity_id(),
    ParentType :: entity_type(), ParentId :: entity_id()) -> ok | {error, term()}.
add_relation(od_share, ShareId, od_space, SpaceId) ->
    add_relation(od_share, ShareId, undefined, od_space, SpaceId, undefined);
add_relation(od_handle, HandleId, od_share, ShareId) ->
    add_relation(od_handle, HandleId, undefined, od_share, ShareId, undefined);
add_relation(od_handle, HandleId, od_handle_service, HServiceId) ->
    add_relation(od_handle, HandleId, undefined, od_handle_service, HServiceId, undefined).


%%--------------------------------------------------------------------
%% @doc
%% Adds a relation between given entities. Used for relations with attributes.
%% Based on entity types, attributes are added to proper relation.
%% @end
%%--------------------------------------------------------------------
-spec add_relation(ChildType :: entity_type(), ChildId :: entity_id(),
    ParentType :: entity_type(), ParentId :: entity_id(),
    Attributes :: attributes()) -> ok | {error, term()}.
add_relation(od_user, UserId, od_group, GroupId, Privileges) ->
    add_relation(od_user, UserId, Privileges, od_group, GroupId, undefined);
add_relation(od_user, UserId, od_space, SpaceId, Privileges) ->
    add_relation(od_user, UserId, Privileges, od_space, SpaceId, undefined);
add_relation(od_user, UserId, od_handle, HandleId, Privileges) ->
    add_relation(od_user, UserId, Privileges, od_handle, HandleId, undefined);
add_relation(od_user, UserId, od_handle_service, HServiceId, Privileges) ->
    add_relation(od_user, UserId, Privileges, od_handle_service, HServiceId, undefined);

add_relation(od_group, ChildId, od_group, ParentId, Privs) ->
    add_relation(od_group, ChildId, Privs, od_group, ParentId, undefined);
add_relation(od_group, GroupId, od_space, SpaceId, Privileges) ->
    add_relation(od_group, GroupId, Privileges, od_space, SpaceId, undefined);
add_relation(od_group, GroupId, od_handle, HandleId, Privileges) ->
    add_relation(od_group, GroupId, Privileges, od_handle, HandleId, undefined);
add_relation(od_group, GroupId, od_handle_service, HServiceId, Privileges) ->
    add_relation(od_group, GroupId, Privileges, od_handle_service, HServiceId, undefined);

add_relation(od_space, GroupId, od_provider, ProviderId, SupportSize) ->
    % Support size is kept in both records
    add_relation(od_space, GroupId, SupportSize, od_provider, ProviderId, SupportSize).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds a relation between given entities. Used for relations with attributes.
%% @end
%%--------------------------------------------------------------------
-spec add_relation(ChildType :: entity_type(), ChildId :: entity_id(),
    ChildAttributes :: attributes(), ParentType :: entity_type(),
    ParentId :: entity_id(), ParentAttributes :: attributes()) ->
    ok | no_return().
add_relation(ChType, ChId, ChAttrs, ParType, ParId, ParAttrs) ->
    ParentUpdateFun = fun(Parent) ->
        case has_child(Parent, ChType, ChId) of
            true ->
                ?ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId);
            false ->
                {ok, mark_dirty(bottom_up, true, ParType, ParId, add_child(
                    Parent, ChType, ChId, ChAttrs
                ))}
        end
    end,
    ChildUpdateFun = fun(Child) ->
        case has_parent(Child, ParType, ParId) of
            true ->
                ?ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId);
            false ->
                {ok, mark_dirty(top_down, true, ChType, ChId, add_parent(
                    Child, ParType, ParId, ParAttrs
                ))}
        end
    end,
    ParentRevertFun = fun(Parent) ->
        {ok, mark_dirty(bottom_up, true, ParType, ParId, remove_child(
            Parent, ChType, ChId
        ))}
    end,
    Result = case update_entity_sync(ParType, ParId, ParentUpdateFun) of
        ok ->
            case update_entity_sync(ChType, ChId, ChildUpdateFun) of
                ok ->
                    ok;
                ?ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId) ->
                    % Relation exists, but apparently it did not exist
                    % in the parent, so we just fixed the relation -> ok.
                    ok;
                Err1 ->
                    % Some other error, we have to attempt reverting the
                    % relation in parent.
                    update_entity_sync(ParType, ParId, ParentRevertFun),
                    Err1
            end;
        Err2 ->
            Err2
    end,
    schedule_refresh(),
    case Result of
        {error, Reason} ->
            throw({error, Reason});
        _ ->
            Result
    end.


%%--------------------------------------------------------------------
%% @doc
%% Updates a relation between given entities. Used for relations with attributes.
%% Based on entity types, attributes are updated in proper relation.
%% @end
%%--------------------------------------------------------------------
-spec update_relation(ChildType :: entity_type(), ChildId :: entity_id(),
    ParentType :: entity_type(), ParentId :: entity_id(),
    Attributes :: attributes()) -> ok | {error, term()}.
update_relation(od_user, UserId, od_group, GroupId, NewPrivs) ->
    update_relation(od_user, UserId, NewPrivs, od_group, GroupId, undefined);
update_relation(od_group, ChGroupId, od_group, ParGroupId, NewPrivs) ->
    update_relation(od_group, ChGroupId, NewPrivs, od_group, ParGroupId, undefined);

update_relation(od_user, GroupId, od_space, SpaceId, NewPrivs) ->
    update_relation(od_user, GroupId, NewPrivs, od_space, SpaceId, undefined);
update_relation(od_group, GroupId, od_space, SpaceId, NewPrivs) ->
    update_relation(od_group, GroupId, NewPrivs, od_space, SpaceId, undefined);

update_relation(od_user, UserId, od_handle_service, HServiceId, NewPrivs) ->
    update_relation(od_user, UserId, NewPrivs, od_handle_service, HServiceId, undefined);
update_relation(od_group, GroupId, od_handle_service, HServiceId, NewPrivs) ->
    update_relation(od_group, GroupId, NewPrivs, od_handle_service, HServiceId, undefined);

update_relation(od_user, UserId, od_handle, HServiceId, NewPrivs) ->
    update_relation(od_user, UserId, NewPrivs, od_handle, HServiceId, undefined);
update_relation(od_group, GroupId, od_handle, HandleId, NewPrivs) ->
    update_relation(od_group, GroupId, NewPrivs, od_handle, HandleId, undefined);

update_relation(od_space, SpaceId, od_provider, ProviderId, NewSupportSize) ->
    update_relation(od_group, SpaceId, NewSupportSize, od_provider, ProviderId, NewSupportSize).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates a relation between given entities. Used for relations with attributes.
%% @end
%%--------------------------------------------------------------------
-spec update_relation(ChildType :: entity_type(), ChildId :: entity_id(),
    ParentType :: entity_type(), ChildAttributes :: attributes(),
    ParentId :: entity_id(), ParentAttributes :: attributes()) ->
    ok | no_return().
update_relation(ChType, ChId, ChAttrs, ParType, ParId, ParAttrs) ->
    ParentUpdateFun = fun(Parent) ->
        case has_child(Parent, ChType, ChId) of
            false ->
                ?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId);
            true ->
                {ok, mark_dirty(bottom_up, true, ParType, ParId, update_child(
                    Parent, ChType, ChId, ParAttrs
                ))}
        end
    end,
    ChildUpdateFun = fun(Child) ->
        case has_parent(Child, ParType, ParId) of
            false ->
                ?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId);
            true ->
                {ok, mark_dirty(top_down, true, ChType, ChId, update_parent(
                    Child, ParType, ParId, ChAttrs
                ))}
        end
    end,
    Result = case update_entity_sync(ParType, ParId, ParentUpdateFun) of
        ok ->
            update_entity_sync(ChType, ChId, ChildUpdateFun);
        Error ->
            Error
    end,
    schedule_refresh(),
    case Result of
        {error, Reason} ->
            throw({error, Reason});
        _ ->
            Result
    end.


%%--------------------------------------------------------------------
%% @doc
%% Removes a relation between given entities.
%% @end
%%--------------------------------------------------------------------
-spec remove_relation(ChildType :: entity_type(), ChildId :: entity_id(),
    ParentType :: entity_type(), ParentId :: entity_id()) -> ok | no_return().
remove_relation(ChType, ChId, ParType, ParId) ->
    ParentUpdateFun = fun(Parent) ->
        case has_child(Parent, ChType, ChId) of
            false ->
                {error, relation_does_not_exist};
            true ->
                {ok, mark_dirty(bottom_up, true, ParType, ParId, remove_child(
                    Parent, ChType, ChId
                ))}
        end
    end,
    ChildUpdateFun = fun(Child) ->
        case has_parent(Child, ParType, ParId) of
            false ->
                {error, relation_does_not_exist};
            true ->
                {ok, mark_dirty(top_down, true, ChType, ChId, remove_parent(
                    Child, ParType, ParId
                ))}
        end
    end,
    Result1 = update_entity_sync(ParType, ParId, ParentUpdateFun),
    Result2 = update_entity_sync(ChType, ChId, ChildUpdateFun),
    schedule_refresh(),
    case {Result1, Result2} of
        {{error, relation_does_not_exist}, {error, relation_does_not_exist}} ->
            % Both sides of relation were not found, report an error
            throw(?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId));
        {_, _} ->
            % At least one side of relation existed, which means success
            % (either both sides were removed, or
            % a broken one-side relation was fixed by removing the side)
            ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% Safely deletes an entity, first removing all its relations and dependent
%% entities. Fails when anything goes wrong in the cleanup procedure.
%% @end
%%--------------------------------------------------------------------
-spec delete_with_relations(EntityType :: entity_type(), EntityId :: entity_id()) ->
    ok | no_return().
delete_with_relations(EntityType, EntityId) ->
    {ok, #document{value = Entity}} = EntityType:get(EntityId),
    Parents = get_parents(Entity),
    DependentParents = maps:get(dependent, Parents, #{}),
    IndependentParents = maps:get(independent, Parents, #{}),
    Children = get_children(Entity),
    DependentChildren = maps:get(dependent, Children, #{}),
    IndependentChildren = maps:get(independent, Children, #{}),
    % Try catch will catch all unexpected failures in relations removal
    % (all sensitive operations are matched to ok)
    try
        % Remove all independent relations
        maps:map(
            fun(ParType, ParentIds) ->
                lists:foreach(
                    fun(ParId) ->
                        ok = remove_relation(EntityType, EntityId, ParType, ParId)
                    end, ParentIds)
            end, IndependentParents),
        maps:map(
            fun(ChType, ChIds) ->
                lists:foreach(
                    fun(ChId) ->
                        ok = remove_relation(ChType, ChId, EntityType, EntityId)
                    end, ChIds)
            end, IndependentChildren),
        % Remove all dependent relations and dependent entities
        maps:map(
            fun(ParType, ParentIds) ->
                lists:foreach(
                    fun(ParId) ->
                        ok = remove_relation(EntityType, EntityId, ParType, ParId),
                        ok = ParType:delete(ParId)
                    end, ParentIds)
            end, DependentParents),
        maps:map(
            fun(ChType, ChIds) ->
                lists:foreach(
                    fun(ChId) ->
                        ok = remove_relation(ChType, ChId, EntityType, EntityId),
                        ok = ChType:delete(ChId)
                    end, ChIds)
            end, DependentChildren),
        ok
    catch
        Type:Message ->
            ?error_stacktrace(
                "Unexpected error while deleting ~p#~s with relations - ~p:~p",
                [EntityType, EntityId, Type, Message]
            ),
            throw(?ERROR_CANNOT_DELETE_ENTITY(EntityType, EntityId))
    end.


%%--------------------------------------------------------------------
%% @doc
%% Updates oz privileges of a user or a group.
%% @end
%%--------------------------------------------------------------------
-spec update_oz_privileges(EntityType :: entity_type(), EntityId :: entity_id(),
    Operation :: privileges_operation(),
    Privileges :: [privileges:oz_privilege()]) -> ok.
update_oz_privileges(EntityType, EntityId, Operation, Privileges) ->
    {ok, _} = EntityType:update(EntityId, fun(Entity) ->
        OzPrivileges = get_oz_privileges(Entity),
        NewOzPrivileges = case Operation of
            set ->
                privileges:from_list(Privileges);
            grant ->
                privileges:union(OzPrivileges, Privileges);
            revoke ->
                privileges:subtract(OzPrivileges, Privileges)
        end,
        {ok, mark_dirty(top_down, true, EntityType, EntityId, update_oz_privileges(
            Entity, NewOzPrivileges))}
    end),
    schedule_refresh(),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves effective graph state, which is a globally shared singleton.
%% @end
%%--------------------------------------------------------------------
-spec get_state() -> #entity_graph_state{}.
get_state() ->
    case entity_graph_state:get(?STATE_KEY) of
        {ok, #document{value = State}} ->
            State;
        Error ->
            ?error("Cannot retrieve state of effective graph: ~p", [Error]),
            error(cannot_get_state)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates effective graph state.
%% @end
%%--------------------------------------------------------------------
-spec update_state(fun((#entity_graph_state{}) -> #entity_graph_state{})) -> ok.
update_state(UpdateFun) ->
    {ok, _} = entity_graph_state:update(?STATE_KEY, UpdateFun),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs refreshing of effective graph. If the graph is already up to date
%% (i.e. not dirty according to its state), it does nothing. If the graph is
%% dirty, it is processed until all documents that were marked dirty are
%% recalculated.
%% @end
%%--------------------------------------------------------------------
-spec refresh_if_needed() -> ok.
refresh_if_needed() ->
    critical_section:run(?ENTITY_GRAPH_LOCK, fun() ->
        case get_state() of
            #entity_graph_state{bottom_up_dirty = [], top_down_dirty = []} ->
                ok;
            State ->
                try
                    refresh_entity_graph(State)
                catch Type:Message ->
                    ?error_stacktrace("Cannot refresh entity graph - ~p:~p", [
                        Type, Message
                    ]),
                    % Sleep for a while to avoid an aggressive loop
                    % (in general, the refresh process should not fail at all).
                    timer:sleep(5000),
                    schedule_refresh()
                end
        end
    end),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Refreshes the entity graph, starting with bottom up dirty entities with
%% highest priorities (lowest priority values) and finishing with top down
%% dirty entities with lowest priorities.
%% @end
%%--------------------------------------------------------------------
-spec refresh_entity_graph(#entity_graph_state{}) -> ok.
refresh_entity_graph(#entity_graph_state{bottom_up_dirty = [First | _]}) ->
    {_Priority, EntityType, EntityId} = First,
    % Make sure that the entity is not modified during the whole update
    sync_on_entity(EntityType, EntityId, fun() ->
        refresh_entity(bottom_up, EntityType, EntityId)
    end),
    refresh_entity_graph(get_state());
refresh_entity_graph(#entity_graph_state{top_down_dirty = [First | _]}) ->
    {_Priority, EntityType, EntityId} = First,
    % Make sure that the entity is not modified during the whole update
    sync_on_entity(EntityType, EntityId, fun() ->
        refresh_entity(top_down, EntityType, EntityId)
    end),
    refresh_entity_graph(get_state());
refresh_entity_graph(#entity_graph_state{}) ->
    % There are no entities to update, finish.
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Refreshes a single entity. Gathers its effective relations from its own
%% relations and from effective relations of neighbours. If anything changed,
%% all successors (direction-wise) are marked as dirty.
%% @end
%%--------------------------------------------------------------------
-spec refresh_entity(Direction :: direction(), EntityType :: entity_type(),
    EntityId :: entity_id()) -> ok.
refresh_entity(Direction, EntityType, EntityId) ->
    {ok, #document{value = Entity}} = EntityType:get(EntityId),
    % Get effective relations from the entity itself
    EffOfItself = gather_eff_from_itself(Direction, EntityId, Entity),
    % Get effective relations from all neighbours
    EffOfNeighbours = gather_eff_from_neighbours(Direction, Entity),
    % Aggregate all effective relations
    AggregatedEffRelations = lists:foldl(
        fun(EffOfNeighbour, OuterMapAcc) ->
            lists:foldl(
                fun({NType, EffRelationsMap}, InnerMapAcc) ->
                    InnerMapAcc#{NType => merge_eff_relations(
                        maps:get(NType, InnerMapAcc, #{}), EffRelationsMap
                    )}
                end, OuterMapAcc, maps:to_list(EffOfNeighbour))
        end, EffOfItself, EffOfNeighbours),
    % Check if anything changed, if so successors should be marked dirty
    case AggregatedEffRelations =:= get_eff_relations(Direction, Entity) of
        true ->
            % Nothing changed, no action is needed
            ok;
        false ->
            % Mark all successors as dirty
            % (these that are in the direction of update)
            Successors = get_successors(Direction, Entity),
            lists:foreach(
                fun({NType, NList}) ->
                    lists:foreach(
                        fun(NId) ->
                            NType:update(NId, fun(Ent) ->
                                {ok, mark_dirty(Direction, true, NType, NId, Ent)}
                            end)
                        end, NList)
                end, maps:to_list(Successors))
    end,
    % Update the record marking it not dirty and setting newly calculated
    % effective relations.
    {ok, _} = EntityType:update(EntityId, fun(Ent) ->
        {ok, mark_dirty(Direction, false, EntityType, EntityId, update_eff_relations(
            Direction, Ent, AggregatedEffRelations
        ))}
    end),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Marks given entity as dirty and returns update entity record (it still needs
%% to be saved to datastore). If an entity is already marked as dirty, it's
%% priority is updated.
%% @end
%%--------------------------------------------------------------------
-spec mark_dirty(Direction :: direction(), Flag :: boolean(),
    EntityType :: entity_type(), EntityId :: entity_id(),
    Entity :: entity()) -> entity().
% Shares do not take part in eff graph recomputation
mark_dirty(_, _, _, _, #od_share{} = Entity) ->
    Entity;
mark_dirty(Direction, Flag, EntityType, EntityId, Entity) ->
    Priority = get_priority(Direction, Entity),
    update_state(
        fun(EffGraphState) ->
            #entity_graph_state{
                bottom_up_dirty = BottomUpDirty,
                top_down_dirty = TopDownDirty
            } = EffGraphState,
            NewState = case {Direction, Flag} of
                {bottom_up, true} ->
                    EffGraphState#entity_graph_state{
                        bottom_up_dirty = lists:sort(lists:keystore(
                            EntityId, 3, BottomUpDirty, {Priority, EntityType, EntityId}
                        ))
                    };
                {bottom_up, false} ->
                    EffGraphState#entity_graph_state{
                        bottom_up_dirty = lists:keydelete(EntityId, 3, BottomUpDirty)
                    };
                {top_down, true} ->
                    EffGraphState#entity_graph_state{
                        top_down_dirty = lists:sort(lists:keystore(
                            EntityId, 3, TopDownDirty, {Priority, EntityType, EntityId}
                        ))
                    };
                {top_down, false} ->
                    EffGraphState#entity_graph_state{
                        top_down_dirty = lists:keydelete(EntityId, 3, TopDownDirty)
                    }
            end,
            {ok, NewState}
        end),
    set_dirty_flag(Direction, Flag, Entity).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Priorities for entities during effective graph recomputation.
%% For bottom-up:
%%    1) groups (sorted by children num)
%%    2) spaces, handles and handle_services
%%    3) providers
%% For top-down:
%%    1) spaces
%%    2) groups (sorted by parents num)
%%    3) users
%% @end
%%--------------------------------------------------------------------
-spec get_priority(Direction :: direction(), Entity :: entity()) -> integer().
get_priority(bottom_up, #od_group{children = Children}) ->
    maps:size(Children);
get_priority(bottom_up, #od_space{}) ->
    1000000000;
get_priority(bottom_up, #od_handle_service{}) ->
    1000000000;
get_priority(bottom_up, #od_handle{}) ->
    1000000000;
get_priority(bottom_up, #od_provider{}) ->
    1000000001;
get_priority(top_down, #od_space{}) ->
    -1;
get_priority(top_down, #od_group{parents = Parents}) ->
    length(Parents);
get_priority(top_down, #od_user{}) ->
    1000000000.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets dirty flag in given entity record, depending on direction.
%% @end
%%--------------------------------------------------------------------
-spec set_dirty_flag(Direction :: direction(), Flag :: boolean(),
    Entity :: entity()) -> entity().
set_dirty_flag(bottom_up, Flag, #od_group{} = Group) ->
    Group#od_group{bottom_up_dirty = Flag};
set_dirty_flag(bottom_up, Flag, #od_space{} = Space) ->
    Space#od_space{bottom_up_dirty = Flag};
set_dirty_flag(bottom_up, Flag, #od_provider{} = Provider) ->
    Provider#od_provider{bottom_up_dirty = Flag};
set_dirty_flag(bottom_up, Flag, #od_handle_service{} = HandleService) ->
    HandleService#od_handle_service{bottom_up_dirty = Flag};
set_dirty_flag(bottom_up, Flag, #od_handle{} = Handle) ->
    Handle#od_handle{bottom_up_dirty = Flag};
set_dirty_flag(top_down, Flag, #od_user{} = User) ->
    User#od_user{top_down_dirty = Flag};
set_dirty_flag(top_down, Flag, #od_group{} = Group) ->
    Group#od_group{top_down_dirty = Flag};
set_dirty_flag(top_down, Flag, #od_space{} = Space) ->
    Space#od_space{top_down_dirty = Flag}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Predicate saying if given entity has given child relation.
%% @end
%%--------------------------------------------------------------------
-spec has_child(Entity :: entity(), ChildType :: entity_type(),
    ChildId :: entity_id()) -> boolean().
has_child(#od_group{users = Users}, od_user, UserId) ->
    maps:is_key(UserId, Users);
has_child(#od_group{children = Children}, od_group, GroupId) ->
    maps:is_key(GroupId, Children);

has_child(#od_space{users = Users}, od_user, UserId) ->
    maps:is_key(UserId, Users);
has_child(#od_space{groups = Groups}, od_group, GroupId) ->
    maps:is_key(GroupId, Groups);
has_child(#od_space{shares = Shares}, od_share, ShareId) ->
    lists:member(ShareId, Shares);

has_child(#od_share{handle = Handle}, od_handle, HandleId) ->
    Handle =:= HandleId;

has_child(#od_provider{spaces = Spaces}, od_space, SpaceId) ->
    maps:is_key(SpaceId, Spaces);

has_child(#od_handle_service{users = Users}, od_user, UserId) ->
    maps:is_key(UserId, Users);
has_child(#od_handle_service{groups = Groups}, od_group, GroupId) ->
    maps:is_key(GroupId, Groups);
has_child(#od_handle_service{handles = Handles}, od_handle, HandleId) ->
    lists:member(HandleId, Handles);

has_child(#od_handle{users = Users}, od_user, UserId) ->
    maps:is_key(UserId, Users);
has_child(#od_handle{groups = Groups}, od_group, GroupId) ->
    maps:is_key(GroupId, Groups).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds a child relation to given entity.
%% @end
%%--------------------------------------------------------------------
-spec add_child(Entity :: entity(), ChildType :: entity_type(),
    ChildId :: entity_id(), Attributes :: attributes()) -> entity().
add_child(#od_group{users = Users} = Group, od_user, UserId, Privs) ->
    Group#od_group{users = maps:put(UserId, Privs, Users)};
add_child(#od_group{children = Children} = Group, od_group, GroupId, Privs) ->
    Group#od_group{children = maps:put(GroupId, Privs, Children)};

add_child(#od_space{users = Users} = Space, od_user, UserId, Privs) ->
    Space#od_space{users = maps:put(UserId, Privs, Users)};
add_child(#od_space{groups = Groups} = Space, od_group, GroupId, Privs) ->
    Space#od_space{groups = maps:put(GroupId, Privs, Groups)};
add_child(#od_space{shares = Shares} = Space, od_share, ShareId, _) ->
    Space#od_space{shares = [ShareId | Shares]};

add_child(#od_share{} = Share, od_handle, HandleId, _) ->
    Share#od_share{handle = HandleId};

add_child(#od_provider{spaces = Spaces} = Provider, od_space, SpaceId, SupportSize) ->
    Provider#od_provider{spaces = maps:put(SpaceId, SupportSize, Spaces)};

add_child(#od_handle_service{users = Users} = HS, od_user, UserId, Privs) ->
    HS#od_handle_service{users = maps:put(UserId, Privs, Users)};
add_child(#od_handle_service{groups = Groups} = HS, od_group, GroupId, Privs) ->
    HS#od_handle_service{groups = maps:put(GroupId, Privs, Groups)};
add_child(#od_handle_service{handles = Handles} = HS, od_handle, HandleId, _) ->
    HS#od_handle_service{handles = [HandleId | Handles]};

add_child(#od_handle{users = Users} = Handle, od_user, UserId, Privs) ->
    Handle#od_handle{users = maps:put(UserId, Privs, Users)};
add_child(#od_handle{groups = Groups} = Handle, od_group, GroupId, Privs) ->
    Handle#od_handle{groups = maps:put(GroupId, Privs, Groups)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates a child relation of given entity. Only applicable to relations
%% with attributes.
%% @end
%%--------------------------------------------------------------------
-spec update_child(Entity :: entity(), ChildType :: entity_type(),
    ChildId :: entity_id(), NewAttributes :: attributes()) -> entity().
update_child(#od_group{users = Users} = Group, od_user, UserId, NewPrivs) ->
    Group#od_group{users = maps:put(UserId, NewPrivs, Users)};
update_child(#od_group{children = Children} = Group, od_group, GroupId, NewPrivs) ->
    Group#od_group{users = maps:put(GroupId, NewPrivs, Children)};

update_child(#od_space{users = Users} = Space, od_user, UserId, NewPrivs) ->
    Space#od_space{users = maps:put(UserId, NewPrivs, Users)};
update_child(#od_space{groups = Groups} = Space, od_group, GroupId, NewPrivs) ->
    Space#od_space{groups = maps:put(GroupId, NewPrivs, Groups)};

update_child(#od_handle_service{users = Users} = HS, od_user, UserId, NewPrivs) ->
    HS#od_handle_service{users = maps:put(UserId, NewPrivs, Users)};
update_child(#od_handle_service{groups = Groups} = HS, od_group, GroupId, NewPrivs) ->
    HS#od_handle_service{groups = maps:put(GroupId, NewPrivs, Groups)};

update_child(#od_handle{users = Users} = Handle, od_user, UserId, NewPrivs) ->
    Handle#od_handle{users = maps:put(UserId, NewPrivs, Users)};
update_child(#od_handle{groups = Groups} = Handle, od_group, GroupId, NewPrivs) ->
    Handle#od_handle{groups = maps:put(GroupId, NewPrivs, Groups)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes a child relation from given entity.
%% @end
%%--------------------------------------------------------------------
-spec remove_child(Entity :: entity(), ChildType :: entity_type(),
    ChildId :: entity_id()) -> entity().
remove_child(#od_group{users = Users} = Group, od_user, UserId) ->
    Group#od_group{users = maps:remove(UserId, Users)};
remove_child(#od_group{children = Children} = Group, od_group, GroupId) ->
    Group#od_group{children = maps:remove(GroupId, Children)};

remove_child(#od_space{users = Users} = Space, od_user, UserId) ->
    Space#od_space{users = maps:remove(UserId, Users)};
remove_child(#od_space{groups = Groups} = Space, od_group, GroupId) ->
    Space#od_space{groups = maps:remove(GroupId, Groups)};
remove_child(#od_space{shares = Shares} = Space, od_share, ShareId) ->
    Space#od_space{shares = lists:delete(ShareId, Shares)};

remove_child(#od_share{} = Share, od_handle, _HandleId) ->
    Share#od_share{handle = undefined};

remove_child(#od_provider{spaces = Spaces} = Provider, od_space, SpaceId) ->
    Provider#od_provider{spaces = maps:remove(SpaceId, Spaces)};

remove_child(#od_handle_service{users = Users} = HS, od_user, UserId) ->
    HS#od_handle_service{users = maps:remove(UserId, Users)};
remove_child(#od_handle_service{groups = Groups} = HS, od_group, GroupId) ->
    HS#od_handle_service{groups = maps:remove(GroupId, Groups)};
remove_child(#od_handle_service{handles = Handles} = HS, od_handle, HandleId) ->
    HS#od_handle_service{handles = lists:delete(HandleId, Handles)};

remove_child(#od_handle{users = Users} = Handle, od_user, UserId) ->
    Handle#od_handle{users = maps:remove(UserId, Users)};
remove_child(#od_handle{groups = Groups} = Handle, od_group, GroupId) ->
    Handle#od_handle{groups = maps:remove(GroupId, Groups)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Predicate saying if given entity has given parent relation.
%% @end
%%--------------------------------------------------------------------
-spec has_parent(Entity :: entity(), ParentType :: entity_type(),
    ParentId :: entity_id()) -> boolean().
has_parent(#od_user{groups = Groups}, od_group, GroupId) ->
    lists:member(GroupId, Groups);
has_parent(#od_user{spaces = Spaces}, od_space, SpaceId) ->
    lists:member(SpaceId, Spaces);
has_parent(#od_user{handle_services = HandleServices}, od_handle_service, HSId) ->
    lists:member(HSId, HandleServices);
has_parent(#od_user{handles = Handles}, od_handle, HandleId) ->
    lists:member(HandleId, Handles);

has_parent(#od_group{parents = Parents}, od_group, GroupId) ->
    lists:member(GroupId, Parents);
has_parent(#od_group{spaces = Spaces}, od_space, SpaceId) ->
    lists:member(SpaceId, Spaces);
has_parent(#od_group{handle_services = HandleServices}, od_handle_service, HSId) ->
    lists:member(HSId, HandleServices);
has_parent(#od_group{handles = Handles}, od_handle, HandleId) ->
    lists:member(HandleId, Handles);

has_parent(#od_space{providers = Providers}, od_provider, ProviderId) ->
    maps:is_key(ProviderId, Providers);

has_parent(#od_share{space = Space}, od_space, SpaceId) ->
    SpaceId =:= Space;

has_parent(#od_handle{resource_type = ResType, resource_id = ResId}, od_share, ShareId) ->
    ResType =:= <<"Share">> andalso ResId =:= ShareId;
has_parent(#od_handle{handle_service = HService}, od_handle_service, HServiceId) ->
    HService =:= HServiceId.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds a parent relation to given entity.
%% @end
%%--------------------------------------------------------------------
-spec add_parent(Entity :: entity(), ParentType :: entity_type(),
    ParentId :: entity_id(), Attributes :: attributes()) -> entity().
add_parent(#od_user{groups = Groups} = User, od_group, GroupId, _) ->
    User#od_user{groups = [GroupId | Groups]};
add_parent(#od_user{spaces = Spaces} = User, od_space, SpaceId, _) ->
    User#od_user{spaces = [SpaceId | Spaces]};
add_parent(#od_user{handle_services = HServices} = User, od_handle_service, HSId, _) ->
    User#od_user{handle_services = [HSId | HServices]};
add_parent(#od_user{handles = Handles} = User, od_handle, HandleId, _) ->
    User#od_user{handles = [HandleId | Handles]};

add_parent(#od_group{parents = Parents} = Group, od_group, GroupId, _) ->
    Group#od_group{parents = [GroupId | Parents]};
add_parent(#od_group{spaces = Spaces} = Group, od_space, SpaceId, _) ->
    Group#od_group{spaces = [SpaceId | Spaces]};
add_parent(#od_group{handle_services = HandleServices} = Group, od_handle_service, HSId, _) ->
    Group#od_group{handle_services = [HSId | HandleServices]};
add_parent(#od_group{handles = Handles} = Group, od_handle, HandleId, _) ->
    Group#od_group{handles = [HandleId | Handles]};

add_parent(#od_space{providers = Providers} = Space, od_provider, ProviderId, SupportSize) ->
    Space#od_space{providers = maps:put(ProviderId, SupportSize, Providers)};

add_parent(#od_share{} = Share, od_space, SpaceId, _) ->
    Share#od_share{space = SpaceId};

add_parent(#od_handle{} = Handle, od_share, ShareId, _) ->
    Handle#od_handle{resource_type = <<"Share">>, resource_id = ShareId};

add_parent(#od_handle{} = Handle, od_handle_service, HServiceId, _) ->
    Handle#od_handle{handle_service = HServiceId}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates a parent relation of given entity. Only applicable to relations
%% with attributes.
%% @end
%%--------------------------------------------------------------------
-spec update_parent(Entity :: entity(), ParentType :: entity_type(),
    ParentId :: entity_id(), NewAttributes :: attributes()) -> entity().
update_parent(#od_space{providers = Providers} = Space, od_provider, ProviderId, SupportSize) ->
    Space#od_space{providers = maps:put(ProviderId, SupportSize, Providers)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes a parent relation from given entity.
%% @end
%%--------------------------------------------------------------------
-spec remove_parent(Entity :: entity(), ParentType :: entity_type(),
    ParentId :: entity_id()) -> entity().
remove_parent(#od_user{groups = Groups} = User, od_group, GroupId) ->
    User#od_user{groups = lists:delete(GroupId, Groups)};
remove_parent(#od_user{spaces = Spaces} = User, od_space, SpaceId) ->
    User#od_user{spaces = lists:delete(SpaceId, Spaces)};
remove_parent(#od_user{handle_services = HServices} = User, od_handle_service, HSId) ->
    User#od_user{handle_services = lists:delete(HSId, HServices)};
remove_parent(#od_user{handles = Handles} = User, od_handle, HandleId) ->
    User#od_user{handles = lists:delete(HandleId, Handles)};

remove_parent(#od_group{parents = Parents} = Group, od_group, GroupId) ->
    Group#od_group{parents = lists:delete(GroupId, Parents)};
remove_parent(#od_group{spaces = Spaces} = Group, od_space, SpaceId) ->
    Group#od_group{spaces = lists:delete(SpaceId, Spaces)};
remove_parent(#od_group{handle_services = HandleServices} = Group, od_handle_service, HSId) ->
    Group#od_group{handle_services = lists:delete(HSId, HandleServices)};
remove_parent(#od_group{handles = Handles} = Group, od_handle, HandleId) ->
    Group#od_group{handles = lists:delete(HandleId, Handles)};

remove_parent(#od_space{providers = Providers} = Space, od_provider, ProviderId) ->
    Space#od_space{providers = maps:remove(ProviderId, Providers)};

remove_parent(#od_share{} = Share, od_space, _SpaceId) ->
    Share#od_share{space = undefined};

remove_parent(#od_handle{} = Handle, od_share, _ShareId) ->
    Handle#od_handle{resource_type = undefined, resource_id = undefined};

remove_parent(#od_handle{} = Handle, od_handle_service, _HServiceId) ->
    Handle#od_handle{handle_service = undefined}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return relations of given entity transformed into effective relations.
%% @end
%%--------------------------------------------------------------------
-spec gather_eff_from_itself(Direction :: direction(), EntityId :: entity_id(),
    Entity :: entity()) -> map_of_any_eff_relations().
gather_eff_from_itself(bottom_up, EntityId, #od_group{} = Group) ->
    #od_group{users = Users, children = Groups} = Group,
    #{
        od_user => relation_to_eff_relation(od_group, EntityId, Users),
        od_group => relation_to_eff_relation(od_group, EntityId, Groups)
    };
gather_eff_from_itself(bottom_up, EntityId, #od_space{} = Space) ->
    #od_space{users = Users, groups = Groups} = Space,
    #{
        od_user => relation_to_eff_relation(od_space, EntityId, Users),
        od_group => relation_to_eff_relation(od_space, EntityId, Groups)
    };
gather_eff_from_itself(bottom_up, EntityId, #od_provider{} = Provider) ->
    #od_provider{spaces = Spaces} = Provider,
    #{od_space => relation_to_eff_relation(od_provider, EntityId, Spaces)};
gather_eff_from_itself(bottom_up, EntityId, #od_handle_service{} = HService) ->
    #od_handle_service{users = Users, groups = Groups} = HService,
    #{
        od_user => relation_to_eff_relation(od_handle_service, EntityId, Users),
        od_group => relation_to_eff_relation(od_handle_service, EntityId, Groups)
    };
gather_eff_from_itself(bottom_up, EntityId, #od_handle{} = Handle) ->
    #od_handle{users = Users, groups = Groups} = Handle,
    #{
        od_user => relation_to_eff_relation(od_handle, EntityId, Users),
        od_group => relation_to_eff_relation(od_handle, EntityId, Groups)
    };
gather_eff_from_itself(top_down, EntityId, #od_user{} = User) ->
    #od_user{
        groups = Groups, spaces = Spaces,
        handle_services = HServices, handles = Handles,
        oz_privileges = OzPrivileges
    } = User,
    #{
        od_group => relation_to_eff_relation(od_user, EntityId, Groups),
        od_space => relation_to_eff_relation(od_user, EntityId, Spaces),
        od_handle_service => relation_to_eff_relation(od_user, EntityId, HServices),
        od_handle => relation_to_eff_relation(od_user, EntityId, Handles),
        oz_privileges => OzPrivileges
    };
gather_eff_from_itself(top_down, EntityId, #od_group{} = Group) ->
    #od_group{
        parents = Groups, spaces = Spaces,
        handle_services = HServices, handles = Handles,
        oz_privileges = OzPrivileges
    } = Group,
    #{
        od_group => relation_to_eff_relation(od_group, EntityId, Groups),
        od_space => relation_to_eff_relation(od_group, EntityId, Spaces),
        od_handle_service => relation_to_eff_relation(od_group, EntityId, HServices),
        od_handle => relation_to_eff_relation(od_group, EntityId, Handles),
        oz_privileges => OzPrivileges
    };
gather_eff_from_itself(top_down, _EntityId, #od_space{}) ->
    #{}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns effective relations of neighbours of given entity, in the form
%% expected by given entity.
%% @end
%%--------------------------------------------------------------------
-spec gather_eff_from_neighbours(Direction :: direction(), Entity :: entity()) ->
    [map_of_any_eff_relations()].
gather_eff_from_neighbours(bottom_up, #od_group{} = Group) ->
    #od_group{children = Children} = Group,
    lists:map(
        fun({GroupId, Privileges}) ->
            EffRelations = get_eff_relations(bottom_up, od_group, GroupId),
            override_eff_privileges(EffRelations, [od_user, od_group], Privileges)
        end, maps:to_list(Children));
gather_eff_from_neighbours(bottom_up, #od_space{} = Space) ->
    #od_space{groups = Groups} = Space,
    lists:map(
        fun({GroupId, Privileges}) ->
            EffRelations = get_eff_relations(bottom_up, od_group, GroupId),
            override_eff_privileges(EffRelations, [od_user, od_group], Privileges)
        end, maps:to_list(Groups));
gather_eff_from_neighbours(bottom_up, #od_provider{} = Provider) ->
    #od_provider{spaces = Spaces} = Provider,
    lists:map(
        fun({SpaceId, _SupportSize}) ->
            EffRelations = get_eff_relations(bottom_up, od_space, SpaceId),
            eff_rel_with_attrs_to_eff_rel(EffRelations, [od_user, od_group])
        end, maps:to_list(Spaces));
gather_eff_from_neighbours(bottom_up, #od_handle_service{} = HService) ->
    #od_handle_service{groups = Groups} = HService,
    lists:map(
        fun({GroupId, Privileges}) ->
            EffRelations = get_eff_relations(bottom_up, od_group, GroupId),
            override_eff_privileges(EffRelations, [od_user, od_group], Privileges)
        end, maps:to_list(Groups));
gather_eff_from_neighbours(bottom_up, #od_handle{} = Handle) ->
    #od_handle{groups = Groups} = Handle,
    lists:map(
        fun({GroupId, Privileges}) ->
            EffRelations = get_eff_relations(bottom_up, od_group, GroupId),
            override_eff_privileges(EffRelations, [od_user, od_group], Privileges)
        end, maps:to_list(Groups));
gather_eff_from_neighbours(top_down, #od_user{} = User) ->
    #od_user{groups = Groups, spaces = Spaces} = User,
    FromGroups = lists:map(
        fun(GroupId) ->
            get_eff_relations(top_down, od_group, GroupId)
        end, Groups),
    FromSpaces = lists:map(
        fun(SpaceId) ->
            {ok, #document{
                value = #od_space{
                    providers = Providers
                }}} = od_space:get(SpaceId),
            #{od_provider => relation_to_eff_relation(
                od_space, SpaceId, relation_with_attrs_to_relation(Providers)
            )}
        end, Spaces),
    FromGroups ++ FromSpaces;
gather_eff_from_neighbours(top_down, #od_group{} = Group) ->
    #od_group{children = Groups, spaces = Spaces} = Group,
    FromGroups = lists:map(
        fun(GroupId) ->
            get_eff_relations(top_down, od_group, GroupId)
        end, maps:keys(Groups)),
    FromSpaces = lists:map(
        fun(SpaceId) ->
            {ok, #document{
                value = #od_space{
                    providers = Providers
                }}} = od_space:get(SpaceId),
            #{od_provider => relation_to_eff_relation(od_space, SpaceId,
                relation_with_attrs_to_relation(Providers)
            )}
        end, Spaces),
    FromGroups ++ FromSpaces;
gather_eff_from_neighbours(top_down, #od_space{}) ->
    [].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a map of effective relations of given entity (by type and id).
%% @end
%%--------------------------------------------------------------------
-spec get_eff_relations(Direction :: direction(), EntityType :: entity_type(),
    EntityId :: entity_id()) -> map_of_any_eff_relations().
get_eff_relations(Direction, EntityType, EntityId) ->
    {ok, #document{value = Entity}} = EntityType:get(EntityId),
    get_eff_relations(Direction, Entity).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a map of effective relations of given entity (by entity record).
%% @end
%%--------------------------------------------------------------------
-spec get_eff_relations(Direction :: direction(),
    Entity :: entity()) -> map_of_any_eff_relations().
get_eff_relations(bottom_up, #od_group{} = Group) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{od_user => EffUsers, od_group => EffGroups};
get_eff_relations(bottom_up, #od_space{} = Space) ->
    #od_space{eff_users = EffUsers, eff_groups = EffGroups} = Space,
    #{od_user => EffUsers, od_group => EffGroups};
get_eff_relations(bottom_up, #od_provider{} = Provider) ->
    #od_provider{eff_users = EffUsers, eff_groups = EffGroups} = Provider,
    #{od_user => EffUsers, od_group => EffGroups};
get_eff_relations(bottom_up, #od_handle_service{} = HService) ->
    #od_handle_service{eff_users = EffUsers, eff_groups = EffGroups} = HService,
    #{od_user => EffUsers, od_group => EffGroups};
get_eff_relations(bottom_up, #od_handle{} = Handle) ->
    #od_handle{eff_users = EffUsers, eff_groups = EffGroups} = Handle,
    #{od_user => EffUsers, od_group => EffGroups};

get_eff_relations(top_down, #od_user{} = User) ->
    #od_user{
        eff_groups = EffGroups, eff_spaces = EffSpaces,
        eff_providers = EffProviders,
        eff_handle_services = EffHServices, eff_handles = EffHandles,
        eff_oz_privileges = EffOzPrivileges
    } = User,
    #{
        od_group => EffGroups, od_space => EffSpaces,
        od_provider => EffProviders,
        od_handle_service => EffHServices, od_handle => EffHandles,
        oz_privileges => EffOzPrivileges
    };
get_eff_relations(top_down, #od_group{} = Group) ->
    #od_group{
        eff_parents = EffGroups, eff_spaces = EffSpaces,
        eff_providers = EffProviders,
        eff_handle_services = EffHServices, eff_handles = EffHandles,
        eff_oz_privileges = EffOzPrivileges
    } = Group,
    #{
        od_group => EffGroups, od_space => EffSpaces,
        od_provider => EffProviders,
        od_handle_service => EffHServices, od_handle => EffHandles,
        oz_privileges => EffOzPrivileges
    };
get_eff_relations(top_down, #od_space{}) ->
    #{}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates effective relations of given entity and returns
%% modified entity record.
%% @end
%%--------------------------------------------------------------------
-spec update_eff_relations(Direction :: direction(), Entity :: entity(),
    EffNeighbours :: map_of_any_eff_relations()) -> entity().
update_eff_relations(bottom_up, #od_group{} = Group, EffNeighbours) ->
    Group#od_group{
        eff_users = maps:get(od_user, EffNeighbours, #{}),
        eff_children = maps:get(od_group, EffNeighbours, #{})
    };
update_eff_relations(bottom_up, #od_space{} = Space, EffNeighbours) ->
    Space#od_space{
        eff_users = maps:get(od_user, EffNeighbours, #{}),
        eff_groups = maps:get(od_group, EffNeighbours, #{})
    };
update_eff_relations(bottom_up, #od_provider{} = Provider, EffNeighbours) ->
    Provider#od_provider{
        eff_users = maps:get(od_user, EffNeighbours, #{}),
        eff_groups = maps:get(od_group, EffNeighbours, #{})
    };
update_eff_relations(bottom_up, #od_handle_service{} = HService, EffNeighbours) ->
    HService#od_handle_service{
        eff_users = maps:get(od_user, EffNeighbours, #{}),
        eff_groups = maps:get(od_group, EffNeighbours, #{})
    };
update_eff_relations(bottom_up, #od_handle{} = Handle, EffNeighbours) ->
    Handle#od_handle{
        eff_users = maps:get(od_user, EffNeighbours, #{}),
        eff_groups = maps:get(od_group, EffNeighbours, #{})
    };

update_eff_relations(top_down, #od_user{} = User, EffNeighbours) ->
    User#od_user{
        eff_groups = maps:get(od_group, EffNeighbours, #{}),
        eff_spaces = maps:get(od_space, EffNeighbours, #{}),
        eff_providers = maps:get(od_provider, EffNeighbours, #{}),
        eff_handle_services = maps:get(od_handle_service, EffNeighbours, #{}),
        eff_handles = maps:get(od_handle, EffNeighbours, #{}),
        eff_oz_privileges = maps:get(oz_privileges, EffNeighbours, #{})
    };
update_eff_relations(top_down, #od_group{} = Group, EffNeighbours) ->
    Group#od_group{
        eff_parents = maps:get(od_group, EffNeighbours, #{}),
        eff_spaces = maps:get(od_space, EffNeighbours, #{}),
        eff_providers = maps:get(od_provider, EffNeighbours, #{}),
        eff_handle_services = maps:get(od_handle_service, EffNeighbours, #{}),
        eff_handles = maps:get(od_handle, EffNeighbours, #{}),
        eff_oz_privileges = maps:get(oz_privileges, EffNeighbours, #{})
    };
update_eff_relations(top_down, #od_space{} = Space, _) ->
    Space.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns all children of given entity, divided into two groups. Dependent
%% entities are those which cannot exist without its parent, independent can.
%% @end
%%--------------------------------------------------------------------
-spec get_children(Entity :: entity()) ->
    #{dependent => map_of_relations(), independent => map_of_relations()}.
get_children(#od_space{shares = Shares} = Space) -> #{
    dependent => #{od_share => Shares},
    independent => get_successors(top_down, Space)
};
get_children(#od_share{handle = undefined}) -> #{
};
get_children(#od_share{handle = Handle}) -> #{
    dependent => #{od_handle => [Handle]}
};
get_children(#od_handle_service{handles = Handles}) -> #{
    dependent => #{od_handle => Handles}
};
get_children(Entity) -> #{
    independent => get_successors(top_down, Entity)
}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns all parents of given entity, divided into two groups. Dependent
%% entities are those which cannot exist without its child, independent can.
%% @end
%%--------------------------------------------------------------------
-spec get_parents(Entity :: entity()) ->
    #{dependent => map_of_relations(), independent => map_of_relations()}.
get_parents(#od_handle{} = Handle) ->
    #od_handle{
        resource_type = ResourceType, resource_id = ResourceId,
        handle_service = HService
    } = Handle,
    Independent = case ResourceType of
        <<"Share">> -> #{od_share => [ResourceId]};
        _ -> #{}
    end,
    #{
        independent => Independent#{od_handle_service => [HService]}
    };
get_parents(#od_share{space = Space}) -> #{
    independent => #{od_space => [Space]}
};
get_parents(Entity) -> #{
    independent => get_successors(bottom_up, Entity)
}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns all successors of given entity (direction-wise). Those entities will
%% be marked dirty if the effective relations of the entity change.
%% @end
%%--------------------------------------------------------------------
-spec get_successors(Direction :: direction(), Entity :: entity()) ->
    map_of_relations().
get_successors(bottom_up, #od_user{} = User) ->
    #od_user{
        groups = Groups, spaces = Spaces,
        handle_services = HServices, handles = Handles
    } = User,
    #{
        od_group => Groups, od_space => Spaces,
        od_handle_service => HServices, od_handle => Handles
    };
get_successors(bottom_up, #od_group{} = Group) ->
    #od_group{
        parents = Groups, spaces = Spaces,
        handle_services = HServices, handles = Handles
    } = Group,
    #{
        od_group => Groups, od_space => Spaces,
        od_handle_service => HServices, od_handle => Handles
    };
get_successors(bottom_up, #od_space{} = Space) ->
    #od_space{providers = Providers} = Space,
    #{od_provider => relation_with_attrs_to_relation(Providers)};

get_successors(top_down, #od_group{} = Group) ->
    #od_group{users = Users, children = Groups} = Group,
    #{
        od_user => relation_with_attrs_to_relation(Users),
        od_group => relation_with_attrs_to_relation(Groups)
    };
get_successors(top_down, #od_space{} = Space) ->
    #od_space{users = Users, groups = Groups} = Space,
    #{
        od_user => relation_with_attrs_to_relation(Users),
        od_group => relation_with_attrs_to_relation(Groups)
    };
get_successors(top_down, #od_provider{} = Provider) ->
    #od_provider{spaces = Spaces} = Provider,
    #{od_space => relation_with_attrs_to_relation(Spaces)};
get_successors(top_down, #od_handle_service{} = HService) ->
    #od_handle_service{users = Users, groups = Groups} = HService,
    #{
        od_user => relation_with_attrs_to_relation(Users),
        od_group => relation_with_attrs_to_relation(Groups)
    };
get_successors(top_down, #od_handle{} = Handle) ->
    #od_handle{users = Users, groups = Groups} = Handle,
    #{
        od_user => relation_with_attrs_to_relation(Users),
        od_group => relation_with_attrs_to_relation(Groups)
    };
% All other relations should return an empty map (have no successors).
get_successors(_, _) ->
    #{}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns oz privileges of user or group.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_privileges(Entity :: entity()) -> [privileges:oz_privilege()].
get_oz_privileges(#od_user{oz_privileges = OzPrivs}) ->
    OzPrivs;
get_oz_privileges(#od_group{oz_privileges = OzPrivs}) ->
    OzPrivs.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates oz privileges of user or group and returns modified entity record.
%% @end
%%--------------------------------------------------------------------
-spec update_oz_privileges(Entity :: entity(), [privileges:oz_privilege()]) ->
    Entity :: entity().
update_oz_privileges(#od_user{} = User, NewOzPrivileges) ->
    User#od_user{oz_privileges = NewOzPrivileges};
update_oz_privileges(#od_group{} = Group, NewOzPrivileges) ->
    Group#od_group{oz_privileges = NewOzPrivileges}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Merges two maps of effective relations. Privileges and intermediaries are
%% merged as a union of two sets.
%% @end
%%--------------------------------------------------------------------
-spec merge_eff_relations(EffMap1 :: map_of_any_eff_relations(),
    EffMap2 :: map_of_any_eff_relations()) -> map_of_any_eff_relations().
merge_eff_relations(EffMap1, EffMap2) when is_map(EffMap1) andalso is_map(EffMap2) ->
    lists:foldl(
        fun({EntityId, EffRelation1}, MapAcc) ->
            NewValue = case maps:get(EntityId, EffMap1, undefined) of
                undefined ->
                    EffRelation1;
                EffRelation2 ->
                    case {EffRelation1, EffRelation2} of
                        {{Privs1, Int1}, {Privs2, Int2}} ->
                            % Covers eff_relation_with_attrs() type
                            {ordsets_union(Privs1, Privs2), ordsets_union(Int1, Int2)};
                        {List1, List2} ->
                            % Covers eff_relation() type and lists of atoms
                            % (effective oz privileges)
                            ordsets_union(List1, List2)
                    end
            end,
            MapAcc#{EntityId => NewValue}
        end, EffMap1, maps:to_list(EffMap2)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Overrides effective privileges of all entities of given types with given
%% privileges. Other entities are filtered out.
%% @end
%%--------------------------------------------------------------------
-spec override_eff_privileges(EffRelations :: map_of_eff_relations_with_attrs(),
    EntityTypes :: [entity_type()], NewPrivileges :: [atom()]) ->
    map_of_eff_relations_with_attrs().
override_eff_privileges(EffRelations, [], _) ->
    EffRelations;
override_eff_privileges(EffRelations, [EntityType | Tail], Privileges) ->
    OverridenPrivs = maps:map(
        fun(_, {_OldPrivileges, Intermediaries}) ->
            {Privileges, Intermediaries}
        end, maps:get(EntityType, EffRelations)),
    NewEffRelations = EffRelations#{EntityType => OverridenPrivs},
    override_eff_privileges(NewEffRelations, Tail, Privileges).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts a relation (with attrs or without) to effective relation.
%% @end
%%--------------------------------------------------------------------
-spec relation_to_eff_relation(EntityType :: entity_type(), EntityId :: entity_id(),
    Relation :: relation(entity_id()) | relation_with_attrs(entity_id(), attributes())) ->
    map_of_any_eff_relations().
relation_to_eff_relation(EntityType, EntityId, List) when is_list(List) ->
    lists:foldl(
        fun(NeighbourId, AccMap) ->
            AccMap#{NeighbourId => [{EntityType, EntityId}]}
        end, #{}, List);
relation_to_eff_relation(EntityType, EntityId, Map) when is_map(Map) ->
    maps:map(
        fun(_NeighbourId, Attributes) ->
            {Attributes, [{EntityType, EntityId}]}
        end, Map).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts an effective relation with attrs to effective relation without attrs
%% for all provided entity types.
%% @end
%%--------------------------------------------------------------------
-spec eff_rel_with_attrs_to_eff_rel(
    EffRelations :: map_of_eff_relations_with_attrs(),
    EntityTypes :: [entity_type()]) -> map_of_eff_relations().
eff_rel_with_attrs_to_eff_rel(EffRelations, []) ->
    EffRelations;
eff_rel_with_attrs_to_eff_rel(EffRelations, [EntityType | Tail]) ->
    OverridenPrivs = maps:map(
        fun(_Key, {_OldPrivileges, Intermediaries}) ->
            Intermediaries
        end, maps:get(EntityType, EffRelations)),
    NewEffRelations = EffRelations#{EntityType => OverridenPrivs},
    eff_rel_with_attrs_to_eff_rel(NewEffRelations, Tail).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts a relation with attrs to a relation without attrs.
%% @end
%%--------------------------------------------------------------------
-spec relation_with_attrs_to_relation(
    RelationWithAttrs :: relation_with_attrs(entity_id(), attributes())) ->
    relation(entity_id()).
relation_with_attrs_to_relation(RelationWithAttrs) ->
    maps:keys(RelationWithAttrs).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a union of two lists, converted into ordsets (ordered lists).
%% @end
%%--------------------------------------------------------------------
-spec ordsets_union(ListA :: [term()], ListB :: [term()]) -> [term()].
ordsets_union(ListA, ListB) ->
    ordsets:union(
        ordsets:from_list(ListA),
        ordsets:from_list(ListB)
    ).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates an entity synchronously (sequentially with another synced operations).
%% @end
%%--------------------------------------------------------------------
-spec update_entity_sync(EntityType :: entity_type(), EntityId :: entity_id(),
    UpdateFun :: fun((entity()) -> entity())) -> ok | {error, term()}.
update_entity_sync(EntityType, EntityId, UpdateFun) ->
    sync_on_entity(EntityType, EntityId, fun() ->
        case EntityType:update(EntityId, UpdateFun) of
            {ok, _} ->
                ok;
            Error ->
                Error
        end
    end).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Runs a function synchronously locking on given entity.
%% @end
%%--------------------------------------------------------------------
-spec sync_on_entity(EntityType :: entity_type(), EntityId :: entity_id(),
    Function :: fun()) -> term().
sync_on_entity(EntityType, EntityId, Function) ->
    critical_section:run({EntityType, EntityId}, Function).
