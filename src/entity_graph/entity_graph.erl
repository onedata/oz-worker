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
%%%     - od_harvester
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
%%% All operations concerning relations can be performed in parallel,
%%% as effective graph logic operates only on effective relations, so no
%%% conflicts are possible.
%%% The relations introduced by other operations do not have possible negative
%%% effects, they will just mark some records as dirty and the effective graph
%%% will become eventually consistent.
%%% Graph refreshes can be scheduled multiple times, only one process at a time
%%% can recalculate the graph, and if the graph is already up-to-date, the
%%% refresh does nothing.
%%% The refreshing of effective relations goes as follows:
%%% 1) bottom-up traversing:
%%%     1.1) groups with least children to most children
%%%     1.2) spaces, handles, handle_services, harvesters
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

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").

-define(ENTITY_GRAPH_LOCK, entity_graph).
% How often should effective graph state be checked during ensure_up_to_date -
% exponential backoff is used.
-define(UP_TO_DATE_CHECK_INTERVAL, 100).
-define(UP_TO_DATE_CHECK_BACKOFF, 1.2).
-define(UP_TO_DATE_CHECK_RETRIES, 30).  % 30 retries take about 140 seconds

-define(THROW_ON_ERROR(__Term), case __Term of
    {error, _} -> throw(__Term);
    _ -> __Term
end).

% direct - direct membership, effective - inherited membership
-type relation_type() :: direct | effective.
% Data types that hold different types of relations.
-type relations(EntityId) :: [EntityId].
-type relations_with_attrs(EntityId, Attributes) :: #{EntityId => Attributes}.
% Data types that hold different types of effective relations. {Type, Id} pairs
% hold pairs of {record type, record id} through which the effective
% relation exists (intermediaries).
% There may be multiple such pairs on the list. If the effective neighbour
% has a direct relation to the entity, the ?SELF_INTERMEDIARY keyword is used as
% Entity Id. The list of intermediaries contains only the first intermediary on
% the effective relation path (which means they all are in a direct relation
% with the entity). To examine the full path of effective relation, a recursive
% lookup must be performed. For example, consider user1 being an effective
% member in space1 through 2 groups:
%   user1 -> groupA -> groupB -> space1
% the entities would have the following effective relations:
%
%   user1 -> eff_groups = #{
%       groupA => [{od_user, ?SELF_INTERMEDIARY}],
%       groupB => [{od_group, groupA}]
%   }
%   user1 -> eff_spaces = #{
%       space1 => [{od_group, groupA}]
%   }
%
%   groupA -> eff_parents = #{
%       groupB => [{od_group, ?SELF_INTERMEDIARY}]
%   }
%   groupA -> eff_spaces = #{
%       space1 => [{od_group, groupB}]
%   }
%
%   groupB -> eff_children = #{
%       groupA => [{od_group, ?SELF_INTERMEDIARY}]
%   }
%   groupB -> eff_spaces = #{
%       space1 => [{od_group, ?SELF_INTERMEDIARY}]
%   }
%
%   space1 -> eff_groups = #{
%       groupA => [{od_group, groupB}],
%       groupB => [{od_space, ?SELF_INTERMEDIARY}]
%   }
%   space1 -> eff_users = #{
%       user1 => [{od_group, groupB}]
%   }

-type intermediaries() :: [{entity_logic:entity_type(), entity_logic:entity_id()}].
-type eff_relations(EntityId) :: #{EntityId => intermediaries()}.
-type eff_relations_with_attrs(EntityId, Attributes) :: #{EntityId => {Attributes, intermediaries()}}.

-export_type([
    relation_type/0, relations/1, relations_with_attrs/2,
    intermediaries/0, eff_relations/1, eff_relations_with_attrs/2
]).

% Types imported from entity_logic for shorter code
-type entity_id() :: entity_logic:entity_id().
-type entity_type() :: entity_logic:entity_type().
-type entity() :: entity_logic:entity().

% Internal types
% Direction in effective graph
-type direction() :: top_down | bottom_up.
% Relation attributes
-type attributes() :: term().
-type privileges() :: [atom()].
% Possible values for attributes update - either new attributes or a pair
% {PrivsToGrant, PrivsToRevoke}
-type attributes_update() :: attributes() | {PrivsToGrant :: privileges(), PrivsToRevoke :: privileges()}.

-type relations() :: relations(entity_id()).
-type relations_with_attrs() :: relations_with_attrs(entity_id(), attributes()).
-type eff_relations() :: eff_relations(entity_id()).
-type eff_relations_with_attrs() :: eff_relations_with_attrs(entity_id(), attributes()).

% OZ privileges are treated differently, but the recalculation process is
% in large part the same as other relations.
-type map_of_eff_relations() :: #{
entity_type() | oz_privileges => eff_relations() | eff_relations_with_attrs() | [privileges:oz_privilege()]
}.

%% API
-export([init_state/0, verify_state_of_all_entities/0]).
-export([schedule_refresh/0, ensure_up_to_date/0]).
-export([add_relation/4, add_relation/5]).
-export([update_relation/5]).
-export([remove_relation/4]).
-export([get_relations/4, get_relations_with_privileges/4]).
-export([has_relation/5, has_relation/6]).
-export([get_privileges/5, has_privilege/6, has_privilege/7]).
-export([get_intermediaries/4]).
-export([delete_with_relations/2, delete_with_relations/3]).
-export([update_oz_privileges/4]).
-export([get_oz_privileges/2, has_oz_privilege/3, has_oz_privilege/4]).

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
    entity_graph_state:initialize(),
    verify_state_of_all_entities().


%%--------------------------------------------------------------------
%% @doc
%% Checks all entities in the system and looks for those which are marked as
%% dirty. Schedule refreshes of such entities.
%% @end
%%--------------------------------------------------------------------
-spec verify_state_of_all_entities() -> ok.
verify_state_of_all_entities() ->
    EntityTypes = [
        od_user, od_group, od_space, od_provider, od_handle_service, od_handle, od_harvester
    ],
    lists:foreach(
        fun(EntityType) ->
            {ok, Entities} = EntityType:list(),
            lists:foreach(
                fun(#document{key = EntityId, value = Entity}) ->
                    lists:foreach(
                        fun(Direction) ->
                            case is_dirty(Direction, Entity) of
                                true ->
                                    ?info("Scheduling ~p refresh of dirty entity: ~p", [
                                        Direction, EntityType:to_string(EntityId)
                                    ]),
                                    update_dirty_queue(
                                        Direction, true, EntityType, EntityId
                                    );
                                false ->
                                    ok
                            end
                        end, [top_down, bottom_up])
                end, Entities)
        end, EntityTypes),
    schedule_refresh().


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
%% Waits until entity graph is recalculated and returns true.
%% Retries with a predefined interval, returns false upon failure.
%% @end
%%--------------------------------------------------------------------
-spec ensure_up_to_date() -> boolean().
ensure_up_to_date() ->
    ensure_up_to_date(?UP_TO_DATE_CHECK_RETRIES, ?UP_TO_DATE_CHECK_INTERVAL).

%% @private
-spec ensure_up_to_date(RetriesLeft :: integer(), Timeout :: integer()) -> boolean().
ensure_up_to_date(0, _) ->
    false;
ensure_up_to_date(RetriesLeft, Timeout) ->
    case is_up_to_date() of
        true ->
            true;
        false ->
            schedule_refresh(),
            timer:sleep(Timeout),
            NewTimeout = round(Timeout * ?UP_TO_DATE_CHECK_BACKOFF),
            ensure_up_to_date(RetriesLeft - 1, NewTimeout)
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
    add_relation(od_handle, HandleId, undefined, od_handle_service, HServiceId, undefined);
add_relation(od_space, SpaceId, od_harvester, HarvesterId) ->
    add_relation(od_space, SpaceId, undefined, od_harvester, HarvesterId, undefined).


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
    add_relation(od_space, GroupId, SupportSize, od_provider, ProviderId, SupportSize);

add_relation(od_user, UserId, od_harvester, HarvesterId, Privileges) ->
    add_relation(od_user, UserId, Privileges, od_harvester, HarvesterId, undefined);
add_relation(od_group, GroupId, od_harvester, HarvesterId, Privileges) ->
    add_relation(od_group, GroupId, Privileges, od_harvester, HarvesterId, undefined).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds a relation between given entities.
%% @end
%%--------------------------------------------------------------------
-spec add_relation(ChildType :: entity_type(), ChildId :: entity_id(),
    ChildAttributes :: attributes(), ParentType :: entity_type(),
    ParentId :: entity_id(), ParentAttributes :: attributes()) ->
    ok | no_return().
add_relation(EntityType, EntityId, _, EntityType, EntityId, _) ->
    throw(?ERROR_CANNOT_ADD_RELATION_TO_SELF);
add_relation(ChType, ChId, ChAttrs, ParType, ParId, ParAttrs) ->
    ParentUpdateFun = fun(Parent) ->
        case has_child(Parent, ChType, ChId) of
            true ->
                ?ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId);
            false ->
                {ok, mark_record_dirty(bottom_up, true, add_child(
                    Parent, ChType, ChId, ChAttrs
                ))}
        end
    end,
    ChildUpdateFun = fun(Child) ->
        case has_parent(Child, ParType, ParId) of
            true ->
                ?ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId);
            false ->
                {ok, mark_record_dirty(top_down, true, add_parent(
                    Child, ParType, ParId, ParAttrs
                ))}
        end
    end,
    ParentRevertFun = fun(Parent) ->
        {ok, mark_record_dirty(bottom_up, true, remove_child(
            Parent, ChType, ChId
        ))}
    end,

    ParentSync = fun() ->
        update_dirty_queue(bottom_up, true, ParType, ParId),
        update_entity(ParType, ParId, ParentUpdateFun)
    end,

    ChildSync = fun() ->
        update_dirty_queue(top_down, true, ChType, ChId),
        update_entity(ChType, ChId, ChildUpdateFun)
    end,

    Result = case sync_on_entity(ParType, ParId, ParentSync) of
        ok ->
            case sync_on_entity(ChType, ChId, ChildSync) of
                ok ->
                    ok;
                ?ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId) ->
                    % Relation exists, but apparently it did not exist
                    % in the parent, so we just fixed the relation -> ok.
                    ok;
                Err1 ->
                    % Some other error, we have to attempt reverting the
                    % relation in parent.
                    sync_on_entity(ParType, ParId, fun() ->
                        update_entity(ParType, ParId, ParentRevertFun)
                    end),
                    Err1
            end;
        Err2 ->
            Err2
    end,
    schedule_refresh(),
    ?THROW_ON_ERROR(Result).


%%--------------------------------------------------------------------
%% @doc
%% Updates a relation between given entities. Used for relations with attributes.
%% Based on entity types, attributes are updated in proper relation.
%% @end
%%--------------------------------------------------------------------
-spec update_relation(ChildType :: entity_type(), ChildId :: entity_id(),
    ParentType :: entity_type(), ParentId :: entity_id(),
    Attributes :: attributes_update()) -> ok | {error, term()}.
update_relation(od_user, UserId, od_group, GroupId, NewPrivs) ->
    update_relation(od_user, UserId, NewPrivs, od_group, GroupId, undefined);
update_relation(od_group, ChGroupId, od_group, ParGroupId, NewPrivs) ->
    update_relation(od_group, ChGroupId, NewPrivs, od_group, ParGroupId, undefined);

update_relation(od_user, UserId, od_space, SpaceId, NewPrivs) ->
    update_relation(od_user, UserId, NewPrivs, od_space, SpaceId, undefined);
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
    update_relation(od_space, SpaceId, NewSupportSize, od_provider, ProviderId, NewSupportSize);

update_relation(od_user, UserId, od_harvester, HarvesterId, NewPrivs) ->
    update_relation(od_user, UserId, NewPrivs, od_harvester, HarvesterId, undefined);
update_relation(od_group, GroupId, od_harvester, HarvesterId, NewPrivs) ->
    update_relation(od_group, GroupId, NewPrivs, od_harvester, HarvesterId, undefined).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates a relation between given entities. Used for relations with attributes.
%% @end
%%--------------------------------------------------------------------
-spec update_relation(ChildType :: entity_type(), ChildId :: entity_id(),
    ChildAttributes :: attributes_update(), ParentType :: entity_type(),
    ParentId :: entity_id(), ParentAttributes :: attributes_update()) ->
    ok | no_return().
update_relation(ChType, ChId, ChAttrs, ParType, ParId, ParAttrs) ->
    ParentUpdateFun = fun(Parent) ->
        case has_child(Parent, ChType, ChId) of
            false ->
                ?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId);
            true ->
                {ok, mark_record_dirty(bottom_up, true, update_child(
                    Parent, ChType, ChId, ChAttrs
                ))}
        end
    end,
    ChildUpdateFun = fun(Child) ->
        case has_parent(Child, ParType, ParId) of
            false ->
                ?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId);
            true ->
                {ok, mark_record_dirty(top_down, true, update_parent(
                    Child, ParType, ParId, ParAttrs
                ))}
        end
    end,

    ParentSync = fun() ->
        update_dirty_queue(bottom_up, true, ParType, ParId),
        update_entity(ParType, ParId, ParentUpdateFun)
    end,

    ChildSync = fun() ->
        update_dirty_queue(top_down, true, ChType, ChId),
        update_entity(ChType, ChId, ChildUpdateFun)
    end,

    Result = case sync_on_entity(ParType, ParId, ParentSync) of
        ok ->
            sync_on_entity(ChType, ChId, ChildSync);
        Error ->
            Error
    end,
    schedule_refresh(),
    ?THROW_ON_ERROR(Result).


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
                {ok, mark_record_dirty(bottom_up, true, remove_child(
                    Parent, ChType, ChId
                ))}
        end
    end,
    ChildUpdateFun = fun(Child) ->
        case has_parent(Child, ParType, ParId) of
            false ->
                {error, relation_does_not_exist};
            true ->
                {ok, mark_record_dirty(top_down, true, remove_parent(
                    Child, ParType, ParId
                ))}
        end
    end,

    ParentSync = fun() ->
        update_dirty_queue(bottom_up, true, ParType, ParId),
        update_entity(ParType, ParId, ParentUpdateFun)
    end,

    ChildSync = fun() ->
        update_dirty_queue(top_down, true, ChType, ChId),
        update_entity(ChType, ChId, ChildUpdateFun)
    end,

    Result1 = sync_on_entity(ParType, ParId, ParentSync),
    Result2 = sync_on_entity(ChType, ChId, ChildSync),
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
%% Returns all relations of an entity with entities of given EntityType.
%% NOTE: will return empty list if there are no such relations, rather than an error.
%% @end
%%--------------------------------------------------------------------
-spec get_relations(relation_type(), direction(), entity_type(), entity()) ->
    relations().
get_relations(direct, Direction, EntityType, Entity) ->
    get_ids(get_direct_relations(Direction, EntityType, Entity));
get_relations(effective, Direction, EntityType, Entity) ->
    case is_dirty(Direction, Entity) of
        false ->
            get_ids(get_eff_relations(Direction, EntityType, Entity));
        true ->
            % If the entity is not up to date, return the sum of direct and
            % effective relations.
            Direct = get_ids(get_direct_relations(Direction, EntityType, Entity)),
            Effective = get_eff_relations(Direction, EntityType, Entity),
            lists:usort(maps:fold(fun(EntityId, Relation, AccList) ->
                case get_intermediaries(Relation) of
                    [{_, ?SELF_INTERMEDIARY}] ->
                        % Do not include effective relations that have
                        % only the direct intermediary but do not appear
                        % among direct relations.
                        AccList;
                    _ ->
                        [EntityId | AccList]
                end
            end, Direct, Effective))
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns all relations of an entity with entities of given EntityType,
%% including the privileges.
%% NOTE: will return empty map if there are no such relations, rather than an error.
%% @end
%%--------------------------------------------------------------------
-spec get_relations_with_privileges(relation_type(), direction(), entity_type(), entity()) ->
    relations_with_attrs().
get_relations_with_privileges(direct, Direction, EntityType, Entity) ->
    get_direct_relations(Direction, EntityType, Entity);
get_relations_with_privileges(effective, Direction, EntityType, Entity) ->
    case is_dirty(Direction, Entity) of
        false ->
            eff_relations_to_relations(get_eff_relations(Direction, EntityType, Entity));
        true ->
            % If the entity is not up to date, return the merged map of direct
            % and effective relations with privileges.
            AllDirect = get_all_direct_relations(Direction, Entity),
            Direct = maps:get(EntityType, AllDirect, #{}),
            Effective = get_eff_relations(Direction, EntityType, Entity),
            maps:fold(fun
                (_EntityId, {_, [{_, ?SELF_INTERMEDIARY}]}, AccMap) ->
                    % Do not include effective relations that have
                    % only the direct intermediary but do not appear
                    % among direct relations.
                    AccMap;
                (EntityId, {_, Intermediaries}, AccMap) ->
                    DirectPrivs = maps:get(EntityId, AccMap, []),
                    InheritedPrivs = lists:foldl(fun({Type, Id}, Acc) ->
                        IntermediaryPrivs = maps:get(Id, maps:get(Type, AllDirect, #{}), []),
                        privileges:union(Acc, IntermediaryPrivs)
                    % Skip eff privileges via self (direct membership)
                    end, [], lists:keydelete(?SELF_INTERMEDIARY, 2, Intermediaries)),
                    maps:put(EntityId, privileges:union(DirectPrivs, InheritedPrivs), AccMap)
            end, Direct, Effective)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying if Subject Entity has a relation with given Entity, denoted
%% by EntityType and EntityId.
%% @end
%%--------------------------------------------------------------------
-spec has_relation(relation_type(), direction(), SubjectEntityType :: entity_type(),
    SubjectEntityId :: entity_id(), entity_type(), entity_id()) -> boolean().
has_relation(RelationType, Direction, SubjectEntityType, SubjectEntityId, EntityType, EntityId) ->
    case EntityType:get(EntityId) of
        {ok, #document{value = Entity}} ->
            has_relation(RelationType, Direction, SubjectEntityType, SubjectEntityId, Entity);
        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying if Subject Entity has a relation with given Entity.
%% @end
%%--------------------------------------------------------------------
-spec has_relation(relation_type(), direction(), SubjectEntityType :: entity_type(),
    SubjectEntityId :: entity_id(), entity()) -> boolean().
has_relation(RelationType, Direction, SubjectEntityType, SubjectEntityId, Entity) ->
    Relations = get_relations(RelationType, Direction, SubjectEntityType, Entity),
    lists:member(SubjectEntityId, Relations).


%%--------------------------------------------------------------------
%% @doc
%% Returns privileges of Subject Entity towards given Entity.
%% NOTE: will return empty list if there is no such relation, rather than an error.
%% @end
%%--------------------------------------------------------------------
-spec get_privileges(relation_type(), direction(), SubjectEntityType :: entity_type(),
    SubjectEntityId :: entity_id(), entity()) -> privileges().
get_privileges(RelationType, Direction, SubjectEntityType, SubjectEntityId, Entity) ->
    Relations = get_relations_with_privileges(RelationType, Direction, SubjectEntityType, Entity),
    maps:get(SubjectEntityId, Relations, []).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying if Subject Entity has a privilege towards given Entity, denoted
%% by EntityType and EntityId.
%% @end
%%--------------------------------------------------------------------
-spec has_privilege(relation_type(), direction(),
    SubjectEntityType :: entity_type(), SubjectEntityId :: entity_id(),
    Privilege :: atom(), entity_type(), entity_id()) -> boolean().
has_privilege(RelationType, Direction, SubjectEntityType, SubjectEntityId, Privilege, EntityType, EntityId) ->
    case EntityType:get(EntityId) of
        {ok, #document{value = Entity}} ->
            has_privilege(RelationType, Direction, SubjectEntityType, SubjectEntityId, Privilege, Entity);
        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying if Subject Entity has a privilege towards given Entity.
%% @end
%%--------------------------------------------------------------------
-spec has_privilege(relation_type(), direction(),
    SubjectEntityType :: entity_type(), SubjectEntityId :: entity_id(),
    Privilege :: atom(), entity()) -> boolean().
has_privilege(RelationType, Direction, SubjectEntityType, SubjectEntityId, Privilege, Entity) ->
    Privileges = get_privileges(RelationType, Direction, SubjectEntityType, SubjectEntityId, Entity),
    lists:member(Privilege, Privileges).


%%--------------------------------------------------------------------
%% @doc
%% Returns the intermediaries of the effective relation between the
%% Subject Entity and Entity.
%% NOTE: will return empty list if there is no such relation, rather than an error.
%% @end
%%--------------------------------------------------------------------
-spec get_intermediaries(direction(), SubjectEntityType :: entity_type(),
    SubjectEntityId :: entity_id(), entity()) -> intermediaries().
get_intermediaries(Direction, SubjectEntityType, SubjectEntityId, Entity) ->
    EffRelations = get_eff_relations(Direction, SubjectEntityType, Entity),
    case maps:find(SubjectEntityId, EffRelations) of
        error -> [];
        {ok, Relation} -> get_intermediaries(Relation)
    end.


%% @private
-spec get_intermediaries(#{entity_id() => intermediaries() | {attributes(), intermediaries()}}) ->
    intermediaries().
get_intermediaries({_Attributes, Intermediaries}) -> Intermediaries;
get_intermediaries(Intermediaries) -> Intermediaries.


%%--------------------------------------------------------------------
%% @doc
%% Safely deletes an entity, first removing all its relations and dependent
%% entities. Fails when anything goes wrong in the cleanup procedure.
%% @end
%%--------------------------------------------------------------------
-spec delete_with_relations(entity_type(), entity_id()) -> ok.
delete_with_relations(EntityType, EntityId) ->
    {ok, #document{value = Entity}} = EntityType:get(EntityId),
    delete_with_relations(EntityType, EntityId, Entity).

-spec delete_with_relations(entity_type(), entity_id(), entity()) -> ok | no_return().
delete_with_relations(EntityType, EntityId, Entity) ->
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
        maps:map(fun(ParType, ParentIds) ->
            lists:foreach(fun(ParId) ->
                ok = remove_relation(EntityType, EntityId, ParType, ParId)
            end, ParentIds)
        end, IndependentParents),
        maps:map(fun(ChType, ChIds) ->
            lists:foreach(fun(ChId) ->
                ok = remove_relation(ChType, ChId, EntityType, EntityId)
            end, ChIds)
        end, IndependentChildren),
        % Remove all dependent relations and dependent entities
        maps:map(fun(ParType, ParentIds) ->
            lists:foreach(fun(ParId) ->
                ok = delete_with_relations(ParType, ParId),
                ?debug("~s has been deleted because it depended on ~s "
                "(which is being deleted)", [
                    ParType:to_string(ParId),
                    EntityType:to_string(EntityId)
                ])
            end, ParentIds)
        end, DependentParents),
        maps:map(fun(ChType, ChIds) ->
            lists:foreach(fun(ChId) ->
                ok = delete_with_relations(ChType, ChId),
                ?debug("~s has been deleted because it depended on ~s "
                "(which is being deleted)", [
                    ChType:to_string(ChId),
                    EntityType:to_string(EntityId)
                ])
            end, ChIds)
        end, DependentChildren),
        % Remove the entity itself (synchronize with other process which might
        % be updating the entity)
        ok = sync_on_entity(EntityType, EntityId, fun() ->
            EntityType:force_delete(EntityId)
        end)
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
%% Returns oz privileges of given Entity.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_privileges(relation_type(), entity()) -> [privileges:oz_privilege()].
get_oz_privileges(direct, #od_user{oz_privileges = Privileges}) ->
    Privileges;
get_oz_privileges(effective, #od_user{eff_oz_privileges = Privileges, top_down_dirty = false}) ->
    Privileges;
get_oz_privileges(effective, #od_user{oz_privileges = Privileges, eff_oz_privileges = EffPrivileges}) ->
    % If the entity is not up to date, return the sum of direct and effective privileges
    privileges:union(Privileges, EffPrivileges);

get_oz_privileges(direct, #od_group{oz_privileges = Privileges}) ->
    Privileges;
get_oz_privileges(effective, #od_group{eff_oz_privileges = Privileges, top_down_dirty = false}) ->
    Privileges;
get_oz_privileges(effective, #od_group{oz_privileges = Privileges, eff_oz_privileges = EffPrivileges}) ->
    % If the entity is not up to date, return the sum of direct and effective privileges
    privileges:union(Privileges, EffPrivileges).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying if given Entity (denoted by EntityType and EntityId)
%% has given OZ privilege.
%% @end
%%--------------------------------------------------------------------
-spec has_oz_privilege(relation_type(), privileges:oz_privilege(), entity_type(), entity_id()) ->
    boolean().
has_oz_privilege(RelationType, Privilege, EntityType, EntityId) ->
    case EntityType:get(EntityId) of
        {ok, #document{value = Entity}} ->
            has_oz_privilege(RelationType, Privilege, Entity);
        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying if given Entity has given OZ privilege.
%% @end
%%--------------------------------------------------------------------
-spec has_oz_privilege(relation_type(), privileges:oz_privilege(), entity()) ->
    boolean().
has_oz_privilege(RelationType, Privilege, Entity) ->
    lists:member(Privilege, get_oz_privileges(RelationType, Entity)).


%%--------------------------------------------------------------------
%% @doc
%% Updates oz privileges of a user or a group.
%% @end
%%--------------------------------------------------------------------
-spec update_oz_privileges(entity_type(), entity_id(),
    [privileges:oz_privilege()], [privileges:oz_privilege()]) -> ok.
update_oz_privileges(EntityType, EntityId, PrivsToGrant, PrivsToRevoke) ->
    sync_on_entity(EntityType, EntityId, fun() ->
        update_dirty_queue(top_down, true, EntityType, EntityId),
        ok = update_entity(EntityType, EntityId, fun(Entity) ->
            OzPrivileges = get_oz_privileges(direct, Entity),
            NewOzPrivileges = privileges:union(PrivsToGrant,
                privileges:subtract(OzPrivileges, PrivsToRevoke)
            ),
            {ok, mark_record_dirty(top_down, true, update_oz_privileges(
                Entity, NewOzPrivileges)
            )}
        end)
    end),
    schedule_refresh(),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec is_up_to_date() -> boolean().
is_up_to_date() ->
    is_up_to_date(entity_graph_state:get()).


%% @private
-spec is_up_to_date(entity_graph_state:state()) -> boolean().
is_up_to_date(#entity_graph_state{bottom_up_dirty = [], top_down_dirty = []}) ->
    true;
is_up_to_date(_) ->
    false.


%% @private
-spec is_refresh_in_progress(entity_graph_state:state()) -> boolean().
is_refresh_in_progress(#entity_graph_state{refresh_in_progress = Flag}) ->
    Flag.


%% @private
-spec set_refresh_in_progress(boolean()) -> ok.
set_refresh_in_progress(Flag) ->
    entity_graph_state:update(fun(EffGraphState) ->
        {ok, EffGraphState#entity_graph_state{refresh_in_progress = Flag}}
    end).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates effective graph state by marking given entity dirty or not dirty
%% (direction-wise).
%% @end
%%--------------------------------------------------------------------
-spec update_dirty_queue(direction(), Flag :: boolean(),
    entity_type(), entity_id()) -> ok.
update_dirty_queue(_, _, od_share, _) ->
    % Shares do not take part in eff graph recomputation
    ok;
update_dirty_queue(top_down, _, od_handle, _) ->
    % Handles are children towards shares only, modifying this relation should
    % not cause graph recalculation.
    ok;
update_dirty_queue(Direction, Flag, EntityType, EntityId) ->
    Priority = get_priority(Direction, EntityType),
    entity_graph_state:update(fun(EffGraphState) ->
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
    end).


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
    State = entity_graph_state:get(),
    case {is_up_to_date(State), is_refresh_in_progress(State)} of
        {false, false} ->
            try
                critical_section:run(?ENTITY_GRAPH_LOCK, fun() ->
                    refresh_entity_graph()
                end)
            catch Type:Message ->
                ?error_stacktrace("Cannot refresh entity graph - ~p:~p", [
                    Type, Message
                ]),
                % Sleep for a while to avoid an aggressive loop
                % (in general, the refresh process should not fail at all).
                timer:sleep(5000),
                schedule_refresh()
            end;
        {_, _} ->
            ok
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Marks that entity graph refresh is in progress, performs the refresh and
%% marks refresh complete.
%% @end
%%--------------------------------------------------------------------
-spec refresh_entity_graph() -> ok.
refresh_entity_graph() ->
    set_refresh_in_progress(true),
    refresh_entity_graph(entity_graph_state:get()),
    set_refresh_in_progress(false).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Refreshes the entity graph, starting with bottom up dirty entities with
%% highest priorities (lowest priority values) and finishing with top down
%% dirty entities with lowest priorities.
%% @end
%%--------------------------------------------------------------------
-spec refresh_entity_graph(entity_graph_state:state()) -> ok.
refresh_entity_graph(#entity_graph_state{bottom_up_dirty = [First | _]}) ->
    {_Priority, EntityType, EntityId} = First,
    % Make sure that the entity is not modified during the whole update
    sync_on_entity(EntityType, EntityId, fun() ->
        refresh_entity(bottom_up, EntityType, EntityId),
        update_dirty_queue(bottom_up, false, EntityType, EntityId)
    end),
    refresh_entity_graph(entity_graph_state:get());
refresh_entity_graph(#entity_graph_state{top_down_dirty = [First | _]}) ->
    {_Priority, EntityType, EntityId} = First,
    % Make sure that the entity is not modified during the whole update
    sync_on_entity(EntityType, EntityId, fun() ->
        refresh_entity(top_down, EntityType, EntityId),
        update_dirty_queue(top_down, false, EntityType, EntityId)
    end),
    refresh_entity_graph(entity_graph_state:get());
refresh_entity_graph(#entity_graph_state{}) ->
    % There are no entities to update, finish.
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Refreshes a single entity. First tries to retrieve the entity from DB.
%% It might happen that the entity has been deleted since it was scheduled
%% for refresh - in that case return 'ok' as no action is needed.
%% If the entity exists, refresh_entity/4 is called.
%% @end
%%--------------------------------------------------------------------
-spec refresh_entity(direction(), entity_type(), entity_id()) -> ok.
refresh_entity(Direction, EntityType, EntityId) ->
    case EntityType:get(EntityId) of
        {ok, #document{value = Entity}} ->
            refresh_entity(Direction, EntityType, EntityId, Entity);
        {error, not_found} ->
            % The entity no longer exists - treat as successfully refreshed.
            ok
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Refreshes a single entity. Gathers its effective relations from its own
%% relations and from effective relations of neighbours. If anything changed,
%% all successors (direction-wise) are marked as dirty.
%% @end
%%--------------------------------------------------------------------
-spec refresh_entity(direction(), entity_type(), entity_id(), entity()) -> ok.
refresh_entity(Direction, EntityType, EntityId, Entity) ->
    % Get effective relations from the entity itself
    EffOfItself = gather_eff_from_itself(Direction, Entity),
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
    case AggregatedEffRelations =:= get_all_eff_relations(Direction, Entity) of
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
                            update_dirty_queue(Direction, true, NType, NId),
                            ok = update_entity(NType, NId, fun(Ent) ->
                                {ok, mark_record_dirty(Direction, true, Ent)}
                            end)
                        end, NList)
                end, maps:to_list(Successors))
    end,
    % Update the record marking it not dirty and setting newly calculated
    % effective relations.
    ok = update_entity(EntityType, EntityId, fun(Ent) ->
        {ok, mark_record_dirty(Direction, false, update_eff_relations(
            Direction, Ent, AggregatedEffRelations
        ))}
    end),
    ?debug("Entity refreshed: ~s", [EntityType:to_string(EntityId)]),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Priorities for entities during effective graph recomputation.
%% For bottom-up:
%%    1) groups
%%    2) spaces, handles, handle_services and harvesters
%%    3) providers
%% For top-down:
%%    1) spaces
%%    2) groups
%%    3) users
%% @end
%%--------------------------------------------------------------------
-spec get_priority(direction(), entity_type()) -> integer().
get_priority(bottom_up, od_group) -> 0;
get_priority(bottom_up, od_space) -> 1;
get_priority(bottom_up, od_handle_service) -> 1;
get_priority(bottom_up, od_handle) -> 1;
get_priority(bottom_up, od_harvester) -> 1;
get_priority(bottom_up, od_provider) -> 2;
get_priority(top_down, od_space) -> 0;
get_priority(top_down, od_group) -> 1;
get_priority(top_down, od_user) -> 2.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets dirty flag in given entity record, depending on direction.
%% @end
%%--------------------------------------------------------------------
-spec mark_record_dirty(direction(), Flag :: boolean(), entity()) -> entity().
mark_record_dirty(_, _, #od_share{} = Entity) ->
    % Shares do not take part in eff graph recomputation
    Entity;
mark_record_dirty(bottom_up, Flag, #od_group{} = Group) ->
    Group#od_group{bottom_up_dirty = Flag};
mark_record_dirty(bottom_up, Flag, #od_space{} = Space) ->
    Space#od_space{bottom_up_dirty = Flag};
mark_record_dirty(bottom_up, Flag, #od_provider{} = Provider) ->
    Provider#od_provider{bottom_up_dirty = Flag};
mark_record_dirty(bottom_up, Flag, #od_handle_service{} = HandleService) ->
    HandleService#od_handle_service{bottom_up_dirty = Flag};
mark_record_dirty(bottom_up, Flag, #od_handle{} = Handle) ->
    Handle#od_handle{bottom_up_dirty = Flag};
mark_record_dirty(bottom_up, Flag, #od_harvester{} = Harvester) ->
    Harvester#od_harvester{bottom_up_dirty = Flag};
mark_record_dirty(top_down, _, #od_handle{} = Entity) ->
    % Handles are children towards shares only, modifying this relation should
    % not cause graph recalculation.
    Entity;
mark_record_dirty(top_down, Flag, #od_user{} = User) ->
    User#od_user{top_down_dirty = Flag};
mark_record_dirty(top_down, Flag, #od_group{} = Group) ->
    Group#od_group{top_down_dirty = Flag};
mark_record_dirty(top_down, Flag, #od_space{} = Space) ->
    Space#od_space{top_down_dirty = Flag}.


%%--------------------------------------------------------------------
%% @doc
%% Predicate telling if given record is dirty (direction-wise).
%% @end
%%--------------------------------------------------------------------
-spec is_dirty(direction(), entity()) -> boolean().
is_dirty(top_down, #od_user{top_down_dirty = Flag}) -> Flag;
is_dirty(top_down, #od_group{top_down_dirty = Flag}) -> Flag;
is_dirty(bottom_up, #od_group{bottom_up_dirty = Flag}) -> Flag;
is_dirty(top_down, #od_space{top_down_dirty = Flag}) -> Flag;
is_dirty(bottom_up, #od_space{bottom_up_dirty = Flag}) -> Flag;
is_dirty(bottom_up, #od_provider{bottom_up_dirty = Flag}) -> Flag;
is_dirty(bottom_up, #od_handle_service{bottom_up_dirty = Flag}) -> Flag;
is_dirty(bottom_up, #od_handle{bottom_up_dirty = Flag}) -> Flag;
is_dirty(bottom_up, #od_harvester{bottom_up_dirty = Flag}) -> Flag;
is_dirty(_, _) -> false.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Predicate saying if given entity has given child relation.
%% @end
%%--------------------------------------------------------------------
-spec has_child(entity(), entity_type(), entity_id()) -> boolean().
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
    maps:is_key(GroupId, Groups);

has_child(#od_harvester{users = Users}, od_user, UserId) ->
    maps:is_key(UserId, Users);
has_child(#od_harvester{groups = Groups}, od_group, GroupId) ->
    maps:is_key(GroupId, Groups);
has_child(#od_harvester{spaces = Spaces}, od_space, SpaceId) ->
    lists:member(SpaceId, Spaces).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds a child relation to given entity.
%% @end
%%--------------------------------------------------------------------
-spec add_child(entity(), entity_type(), entity_id(), attributes()) -> entity().
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
    Handle#od_handle{groups = maps:put(GroupId, Privs, Groups)};

add_child(#od_harvester{users = Users} = Harvester, od_user, UserId, Privs) ->
    Harvester#od_harvester{users = maps:put(UserId, Privs, Users)};
add_child(#od_harvester{groups = Groups} = Harvester, od_group, GroupId, Privs) ->
    Harvester#od_harvester{groups = maps:put(GroupId, Privs, Groups)};
add_child(#od_harvester{spaces = Spaces} = Harvester, od_space, SpaceId, _) ->
    Harvester#od_harvester{spaces = [SpaceId | Spaces]}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates a child relation of given entity. Only applicable to relations
%% with attributes.
%% @end
%%--------------------------------------------------------------------
-spec update_child(entity(), entity_type(), entity_id(), attributes_update()) -> entity().
update_child(#od_group{users = Users} = Group, od_user, UserId, {PrivsToGrant, PrivsToRevoke}) ->
    Group#od_group{users = update_privileges(UserId, Users, PrivsToGrant, PrivsToRevoke)};
update_child(#od_group{children = Children} = Group, od_group, GroupId, {PrivsToGrant, PrivsToRevoke}) ->
    Group#od_group{children = update_privileges(GroupId, Children, PrivsToGrant, PrivsToRevoke)};

update_child(#od_space{users = Users} = Space, od_user, UserId, {PrivsToGrant, PrivsToRevoke}) ->
    Space#od_space{users = update_privileges(UserId, Users, PrivsToGrant, PrivsToRevoke)};
update_child(#od_space{groups = Groups} = Space, od_group, GroupId, {PrivsToGrant, PrivsToRevoke}) ->
    Space#od_space{groups = update_privileges(GroupId, Groups, PrivsToGrant, PrivsToRevoke)};

update_child(#od_provider{spaces = Spaces} = Provider, od_space, SpaceId, NewSupportSize) ->
    Provider#od_provider{spaces = maps:put(SpaceId, NewSupportSize, Spaces)};

update_child(#od_handle_service{users = Users} = HS, od_user, UserId, {PrivsToGrant, PrivsToRevoke}) ->
    HS#od_handle_service{users = update_privileges(UserId, Users, PrivsToGrant, PrivsToRevoke)};
update_child(#od_handle_service{groups = Groups} = HS, od_group, GroupId, {PrivsToGrant, PrivsToRevoke}) ->
    HS#od_handle_service{groups = update_privileges(GroupId, Groups, PrivsToGrant, PrivsToRevoke)};

update_child(#od_handle{users = Users} = Handle, od_user, UserId, {PrivsToGrant, PrivsToRevoke}) ->
    Handle#od_handle{users = update_privileges(UserId, Users, PrivsToGrant, PrivsToRevoke)};
update_child(#od_handle{groups = Groups} = Handle, od_group, GroupId, {PrivsToGrant, PrivsToRevoke}) ->
    Handle#od_handle{groups = update_privileges(GroupId, Groups, PrivsToGrant, PrivsToRevoke)};

update_child(#od_harvester{users = Users} = Harvester, od_user, UserId, {PrivsToGrant, PrivsToRevoke}) ->
    Harvester#od_harvester{users = update_privileges(UserId, Users, PrivsToGrant, PrivsToRevoke)};
update_child(#od_harvester{groups = Groups} = Harvester, od_group, GroupId, {PrivsToGrant, PrivsToRevoke}) ->
    Harvester#od_harvester{groups = update_privileges(GroupId, Groups, PrivsToGrant, PrivsToRevoke)};

update_child(Entity, _, _, undefined) ->
    % Other entities do not have updatable children relations.
    Entity.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes a child relation from given entity.
%% @end
%%--------------------------------------------------------------------
-spec remove_child(entity(), entity_type(), entity_id()) -> entity().
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
    Handle#od_handle{groups = maps:remove(GroupId, Groups)};

remove_child(#od_harvester{users = Users} = Harvester, od_user, UserId) ->
    Harvester#od_harvester{users = maps:remove(UserId, Users)};
remove_child(#od_harvester{groups = Groups} = Harvester, od_group, GroupId) ->
    Harvester#od_harvester{groups = maps:remove(GroupId, Groups)};
remove_child(#od_harvester{spaces = Spaces} = Harvester, od_space, SpaceId) ->
    Harvester#od_harvester{spaces = lists:delete(SpaceId, Spaces)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Predicate saying if given entity has given parent relation.
%% @end
%%--------------------------------------------------------------------
-spec has_parent(entity(), entity_type(), entity_id()) -> boolean().
has_parent(#od_user{groups = Groups}, od_group, GroupId) ->
    lists:member(GroupId, Groups);
has_parent(#od_user{spaces = Spaces}, od_space, SpaceId) ->
    lists:member(SpaceId, Spaces);
has_parent(#od_user{handle_services = HandleServices}, od_handle_service, HSId) ->
    lists:member(HSId, HandleServices);
has_parent(#od_user{handles = Handles}, od_handle, HandleId) ->
    lists:member(HandleId, Handles);
has_parent(#od_user{harvesters = Harvesters}, od_harvester, HarvesterId) ->
    lists:member(HarvesterId, Harvesters);

has_parent(#od_group{parents = Parents}, od_group, GroupId) ->
    lists:member(GroupId, Parents);
has_parent(#od_group{spaces = Spaces}, od_space, SpaceId) ->
    lists:member(SpaceId, Spaces);
has_parent(#od_group{handle_services = HandleServices}, od_handle_service, HSId) ->
    lists:member(HSId, HandleServices);
has_parent(#od_group{handles = Handles}, od_handle, HandleId) ->
    lists:member(HandleId, Handles);
has_parent(#od_group{harvesters = Harvesters}, od_harvester, HarvesterId) ->
    lists:member(HarvesterId, Harvesters);

has_parent(#od_space{providers = Providers}, od_provider, ProviderId) ->
    maps:is_key(ProviderId, Providers);
has_parent(#od_space{harvesters = Harvesters}, od_harvester, HarvesterId) ->
    lists:member(HarvesterId, Harvesters);

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
-spec add_parent(entity(), ParentType :: entity_type(),
    ParentId :: entity_id(), Attributes :: attributes()) -> entity().
add_parent(#od_user{groups = Groups} = User, od_group, GroupId, _) ->
    User#od_user{groups = [GroupId | Groups]};
add_parent(#od_user{spaces = Spaces} = User, od_space, SpaceId, _) ->
    User#od_user{spaces = [SpaceId | Spaces]};
add_parent(#od_user{handle_services = HServices} = User, od_handle_service, HSId, _) ->
    User#od_user{handle_services = [HSId | HServices]};
add_parent(#od_user{handles = Handles} = User, od_handle, HandleId, _) ->
    User#od_user{handles = [HandleId | Handles]};
add_parent(#od_user{harvesters = Harvesters} = User, od_harvester, HarvesterId, _) ->
    User#od_user{harvesters = [HarvesterId | Harvesters]};

add_parent(#od_group{parents = Parents} = Group, od_group, GroupId, _) ->
    Group#od_group{parents = [GroupId | Parents]};
add_parent(#od_group{spaces = Spaces} = Group, od_space, SpaceId, _) ->
    Group#od_group{spaces = [SpaceId | Spaces]};
add_parent(#od_group{handle_services = HandleServices} = Group, od_handle_service, HSId, _) ->
    Group#od_group{handle_services = [HSId | HandleServices]};
add_parent(#od_group{handles = Handles} = Group, od_handle, HandleId, _) ->
    Group#od_group{handles = [HandleId | Handles]};
add_parent(#od_group{harvesters = Harvesters} = Group, od_harvester, HarvesterId, _) ->
    Group#od_group{harvesters = [HarvesterId | Harvesters]};

add_parent(#od_space{providers = Providers} = Space, od_provider, ProviderId, SupportSize) ->
    Space#od_space{providers = maps:put(ProviderId, SupportSize, Providers)};
add_parent(#od_space{harvesters = Harvesters} = Space, od_harvester, HarvesterId, _) ->
    Space#od_space{harvesters = [HarvesterId | Harvesters]};

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
-spec update_parent(entity(), entity_type(), entity_id(), attributes_update()) -> entity().
update_parent(#od_space{providers = Providers} = Space, od_provider, ProviderId, SupportSize) ->
    Space#od_space{providers = maps:put(ProviderId, SupportSize, Providers)};
update_parent(Entity, _, _, undefined) ->
    % Other entities do not have updatable parent relations.
    Entity.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes a parent relation from given entity.
%% @end
%%--------------------------------------------------------------------
-spec remove_parent(entity(), entity_type(), entity_id()) -> entity().
remove_parent(#od_user{groups = Groups} = User, od_group, GroupId) ->
    User#od_user{groups = lists:delete(GroupId, Groups)};
remove_parent(#od_user{spaces = Spaces} = User, od_space, SpaceId) ->
    User#od_user{spaces = lists:delete(SpaceId, Spaces)};
remove_parent(#od_user{handle_services = HServices} = User, od_handle_service, HSId) ->
    User#od_user{handle_services = lists:delete(HSId, HServices)};
remove_parent(#od_user{handles = Handles} = User, od_handle, HandleId) ->
    User#od_user{handles = lists:delete(HandleId, Handles)};
remove_parent(#od_user{harvesters = Harvesters} = User, od_harvester, HarvesterId) ->
    User#od_user{harvesters = lists:delete(HarvesterId, Harvesters)};

remove_parent(#od_group{parents = Parents} = Group, od_group, GroupId) ->
    Group#od_group{parents = lists:delete(GroupId, Parents)};
remove_parent(#od_group{spaces = Spaces} = Group, od_space, SpaceId) ->
    Group#od_group{spaces = lists:delete(SpaceId, Spaces)};
remove_parent(#od_group{handle_services = HandleServices} = Group, od_handle_service, HSId) ->
    Group#od_group{handle_services = lists:delete(HSId, HandleServices)};
remove_parent(#od_group{handles = Handles} = Group, od_handle, HandleId) ->
    Group#od_group{handles = lists:delete(HandleId, Handles)};
remove_parent(#od_group{harvesters = Harvesters} = Group, od_harvester, HarvesterId) ->
    Group#od_group{harvesters = lists:delete(HarvesterId, Harvesters)};

remove_parent(#od_space{providers = Providers} = Space, od_provider, ProviderId) ->
    Space#od_space{providers = maps:remove(ProviderId, Providers)};
remove_parent(#od_space{harvesters = Harvesters} = Space, od_harvester, HarvesterId) ->
    Space#od_space{harvesters = lists:delete(HarvesterId, Harvesters)};

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
-spec gather_eff_from_itself(direction(), entity()) -> map_of_eff_relations().
gather_eff_from_itself(bottom_up, #od_group{} = Group) ->
    #od_group{users = Users, children = Groups} = Group,
    #{
        od_user => relations_to_eff_relations(Users, [{od_group, ?SELF_INTERMEDIARY}]),
        od_group => relations_to_eff_relations(Groups, [{od_group, ?SELF_INTERMEDIARY}])
    };
gather_eff_from_itself(bottom_up, #od_space{} = Space) ->
    #od_space{users = Users, groups = Groups} = Space,
    #{
        od_user => relations_to_eff_relations(Users, [{od_space, ?SELF_INTERMEDIARY}]),
        od_group => relations_to_eff_relations(Groups, [{od_space, ?SELF_INTERMEDIARY}])
    };
gather_eff_from_itself(bottom_up, #od_provider{} = Provider) ->
    #od_provider{spaces = Spaces} = Provider,
    #{od_space => relations_to_eff_relations(Spaces, [{od_provider, ?SELF_INTERMEDIARY}])};
gather_eff_from_itself(bottom_up, #od_handle_service{} = HService) ->
    #od_handle_service{users = Users, groups = Groups} = HService,
    #{
        od_user => relations_to_eff_relations(Users, [{od_handle_service, ?SELF_INTERMEDIARY}]),
        od_group => relations_to_eff_relations(Groups, [{od_handle_service, ?SELF_INTERMEDIARY}])
    };
gather_eff_from_itself(bottom_up, #od_handle{} = Handle) ->
    #od_handle{users = Users, groups = Groups} = Handle,
    #{
        od_user => relations_to_eff_relations(Users, [{od_handle, ?SELF_INTERMEDIARY}]),
        od_group => relations_to_eff_relations(Groups, [{od_handle, ?SELF_INTERMEDIARY}])
    };
gather_eff_from_itself(bottom_up, #od_harvester{} = Harvester) ->
    #od_harvester{users = Users, groups = Groups} = Harvester,
    #{
        od_user => relations_to_eff_relations(Users, [{od_harvester, ?SELF_INTERMEDIARY}]),
        od_group => relations_to_eff_relations(Groups, [{od_harvester, ?SELF_INTERMEDIARY}])
    };
gather_eff_from_itself(top_down, #od_user{} = User) ->
    #od_user{
        groups = Groups, spaces = Spaces, handle_services = HServices,
        handles = Handles, harvesters = Harvesters,
        oz_privileges = OzPrivileges
    } = User,
    #{
        od_group => relations_to_eff_relations(Groups, [{od_user, ?SELF_INTERMEDIARY}]),
        od_space => relations_to_eff_relations(Spaces, [{od_user, ?SELF_INTERMEDIARY}]),
        od_handle_service => relations_to_eff_relations(HServices, [{od_user, ?SELF_INTERMEDIARY}]),
        od_handle => relations_to_eff_relations(Handles, [{od_user, ?SELF_INTERMEDIARY}]),
        od_harvester => relations_to_eff_relations(Harvesters, [{od_user, ?SELF_INTERMEDIARY}]),
        oz_privileges => OzPrivileges
    };
gather_eff_from_itself(top_down, #od_group{} = Group) ->
    #od_group{
        parents = Groups, spaces = Spaces, handle_services = HServices,
        handles = Handles, harvesters = Harvesters,
        oz_privileges = OzPrivileges
    } = Group,
    #{
        od_group => relations_to_eff_relations(Groups, [{od_group, ?SELF_INTERMEDIARY}]),
        od_space => relations_to_eff_relations(Spaces, [{od_group, ?SELF_INTERMEDIARY}]),
        od_handle_service => relations_to_eff_relations(HServices, [{od_group, ?SELF_INTERMEDIARY}]),
        od_handle => relations_to_eff_relations(Handles, [{od_group, ?SELF_INTERMEDIARY}]),
        od_harvester => relations_to_eff_relations(Harvesters, [{od_user, ?SELF_INTERMEDIARY}]),
        oz_privileges => OzPrivileges
    };
gather_eff_from_itself(top_down, #od_space{} = Space) ->
    #od_space{providers = Providers} = Space,
    #{od_provider => relations_to_eff_relations(get_ids(Providers), [{od_space, ?SELF_INTERMEDIARY}])}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns effective relations of neighbours of given entity, in the form
%% expected by given entity.
%% @end
%%--------------------------------------------------------------------
-spec gather_eff_from_neighbours(direction(), entity()) -> [map_of_eff_relations()].
gather_eff_from_neighbours(bottom_up, #od_group{} = Group) ->
    #od_group{children = Children} = Group,
    lists:map(
        fun({GroupId, Privileges}) ->
            EffRelations = get_all_eff_relations(bottom_up, od_group, GroupId),
            override_eff_relations(EffRelations, [od_user, od_group], {Privileges, [{od_group, GroupId}]})
        end, maps:to_list(Children));
gather_eff_from_neighbours(bottom_up, #od_space{} = Space) ->
    #od_space{groups = Groups} = Space,
    lists:map(
        fun({GroupId, Privileges}) ->
            EffRelations = get_all_eff_relations(bottom_up, od_group, GroupId),
            override_eff_relations(EffRelations, [od_user, od_group], {Privileges, [{od_group, GroupId}]})
        end, maps:to_list(Groups));
gather_eff_from_neighbours(bottom_up, #od_provider{} = Provider) ->
    #od_provider{spaces = Spaces} = Provider,
    lists:map(
        fun({SpaceId, _SupportSize}) ->
            EffRelations = get_all_eff_relations(bottom_up, od_space, SpaceId),
            override_eff_relations(EffRelations, [od_user, od_group], [{od_space, SpaceId}])
        end, maps:to_list(Spaces));
gather_eff_from_neighbours(bottom_up, #od_handle_service{} = HService) ->
    #od_handle_service{groups = Groups} = HService,
    lists:map(
        fun({GroupId, Privileges}) ->
            EffRelations = get_all_eff_relations(bottom_up, od_group, GroupId),
            override_eff_relations(EffRelations, [od_user, od_group], {Privileges, [{od_group, GroupId}]})
        end, maps:to_list(Groups));
gather_eff_from_neighbours(bottom_up, #od_handle{} = Handle) ->
    #od_handle{groups = Groups} = Handle,
    lists:map(
        fun({GroupId, Privileges}) ->
            EffRelations = get_all_eff_relations(bottom_up, od_group, GroupId),
            override_eff_relations(EffRelations, [od_user, od_group], {Privileges, [{od_group, GroupId}]})
        end, maps:to_list(Groups));
gather_eff_from_neighbours(bottom_up, #od_harvester{} = Harvester) ->
    #od_harvester{groups = Groups} = Harvester,
    lists:map(
        fun({GroupId, Privileges}) ->
            EffRelations = get_all_eff_relations(bottom_up, od_group, GroupId),
            override_eff_relations(EffRelations, [od_user, od_group], {Privileges, [{od_group, GroupId}]})
        end, maps:to_list(Groups));
gather_eff_from_neighbours(top_down, #od_user{} = User) ->
    #od_user{groups = Groups, spaces = Spaces} = User,
    FromGroups = lists:map(
        fun(GroupId) ->
            EffRelations = get_all_eff_relations(top_down, od_group, GroupId),
            override_eff_relations(EffRelations, [
                od_group, od_space, od_provider, od_handle_service, od_handle, od_harvester
            ], [{od_group, GroupId}])
        end, Groups),
    FromSpaces = lists:map(
        fun(SpaceId) ->
            {ok, #document{
                value = #od_space{
                    providers = Providers
                }}} = od_space:get(SpaceId),
            #{od_provider => relations_to_eff_relations(
                get_ids(Providers), [{od_space, SpaceId}]
            )}
        end, Spaces),
    FromGroups ++ FromSpaces;
gather_eff_from_neighbours(top_down, #od_group{} = Group) ->
    #od_group{parents = Groups, spaces = Spaces} = Group,
    FromGroups = lists:map(
        fun(GroupId) ->
            EffRelations = get_all_eff_relations(top_down, od_group, GroupId),
            override_eff_relations(EffRelations, [
                od_group, od_space, od_provider, od_handle_service, od_handle, od_harvester
            ], [{od_group, GroupId}])
        end, Groups),
    FromSpaces = lists:map(
        fun(SpaceId) ->
            {ok, #document{
                value = #od_space{
                    providers = Providers
                }}} = od_space:get(SpaceId),
            #{od_provider => relations_to_eff_relations(
                get_ids(Providers), [{od_space, SpaceId}]
            )}
        end, Spaces),
    FromGroups ++ FromSpaces;
gather_eff_from_neighbours(top_down, #od_space{}) ->
    [].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates effective relations of given entity and returns
%% modified entity record.
%% @end
%%--------------------------------------------------------------------
-spec update_eff_relations(direction(), entity(), map_of_eff_relations()) -> entity().
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
update_eff_relations(bottom_up, #od_harvester{} = Harvester, EffNeighbours) ->
    Harvester#od_harvester{
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
        eff_harvesters = maps:get(od_harvester, EffNeighbours, #{}),
        eff_oz_privileges = maps:get(oz_privileges, EffNeighbours, #{})
    };
update_eff_relations(top_down, #od_group{} = Group, EffNeighbours) ->
    Group#od_group{
        eff_parents = maps:get(od_group, EffNeighbours, #{}),
        eff_spaces = maps:get(od_space, EffNeighbours, #{}),
        eff_providers = maps:get(od_provider, EffNeighbours, #{}),
        eff_handle_services = maps:get(od_handle_service, EffNeighbours, #{}),
        eff_handles = maps:get(od_handle, EffNeighbours, #{}),
        eff_harvesters = maps:get(od_harvester, EffNeighbours, #{}),
        eff_oz_privileges = maps:get(oz_privileges, EffNeighbours, #{})
    };
update_eff_relations(top_down, #od_space{} = Space, EffNeighbours) ->
    Space#od_space{
        eff_providers = maps:get(od_provider, EffNeighbours, #{})
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns all children of given entity, divided into two groups. Dependent
%% entities are those which cannot exist without its parent, independent can.
%% @end
%%--------------------------------------------------------------------
-spec get_children(entity()) ->
    #{dependent => #{entity_type() => relations()}, independent => #{entity_type() => relations()}}.
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
-spec get_parents(entity()) ->
    #{dependent => #{entity_type() => relations()}, independent => #{entity_type() => relations()}}.
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
%% Returns all direct relations of Entity with other entities of given EntityType.
%% Will return empty result when there are no such relations.
%% @end
%%--------------------------------------------------------------------
-spec get_direct_relations(direction(), entity_type(), entity()) ->
    relations() | relations_with_attrs().
get_direct_relations(Direction, EntityType, Entity) ->
    maps:get(EntityType, get_all_direct_relations(Direction, Entity), #{}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns all effective relations of Entity with other entities of given EntityType.
%% Will return empty result when there are no such relations.
%% @end
%%--------------------------------------------------------------------
-spec get_all_direct_relations(direction(), entity()) ->
    #{entity_type() => relations() | relations_with_attrs()}.
get_all_direct_relations(bottom_up, #od_group{} = Group) ->
    #od_group{users = Users, children = Groups} = Group,
    #{od_user => Users, od_group => Groups};
get_all_direct_relations(bottom_up, #od_space{} = Space) ->
    #od_space{users = Users, groups = Groups, harvesters = Harvesters} = Space,
    #{od_user => Users, od_group => Groups, od_harvester => Harvesters};
get_all_direct_relations(bottom_up, #od_provider{} = Provider) ->
    #od_provider{spaces = Spaces} = Provider,
    #{od_space => Spaces};
get_all_direct_relations(bottom_up, #od_handle_service{} = HService) ->
    #od_handle_service{users = Users, groups = Groups, handles = Handles} = HService,
    #{od_user => Users, od_group => Groups, od_handle => Handles};
get_all_direct_relations(bottom_up, #od_handle{} = Handle) ->
    #od_handle{users = Users, groups = Groups} = Handle,
    #{od_user => Users, od_group => Groups};
get_all_direct_relations(bottom_up, #od_harvester{} = Harvester) ->
    #od_harvester{users = Users, groups = Groups, spaces = Spaces} = Harvester,
    #{od_user => Users, od_group => Groups, od_space => Spaces};

get_all_direct_relations(top_down, #od_user{} = User) ->
    #od_user{
        groups = Groups, spaces = Spaces,
        handle_services = HServices, handles = Handles,
        harvesters = Harvesters
    } = User,
    #{
        od_group => Groups, od_space => Spaces,
        od_handle_service => HServices, od_handle => Handles,
        od_harvester => Harvesters
    };
get_all_direct_relations(top_down, #od_group{} = Group) ->
    #od_group{
        parents = Groups, spaces = Spaces,
        handle_services = HServices, handles = Handles,
        harvesters = Harvesters
    } = Group,
    #{
        od_group => Groups, od_space => Spaces,
        od_handle_service => HServices, od_handle => Handles,
        od_harvester => Harvesters
    };
get_all_direct_relations(top_down, #od_space{} = Space) ->
    #od_space{providers = Providers, harvesters = Harvesters} = Space,
    #{
        od_provider => Providers,
        od_harvester => Harvesters
    };
get_all_direct_relations(top_down, #od_handle{handle_service = HServiceId}) ->
    #{od_handle_service => [HServiceId]}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns all effective relations of Entity with other entities of given EntityType.
%% Will return empty result when there are no such relations.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_relations(direction(), entity_type(), entity()) ->
    eff_relations() | eff_relations_with_attrs().
get_eff_relations(Direction, EntityType, Entity) ->
    maps:get(EntityType, get_all_eff_relations(Direction, Entity), #{}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a map of effective relations of given entity (by type and id).
%% @end
%%--------------------------------------------------------------------
-spec get_all_eff_relations(direction(), entity_type(), entity_id()) ->
    map_of_eff_relations().
get_all_eff_relations(Direction, EntityType, EntityId) ->
    {ok, #document{value = Entity}} = EntityType:get(EntityId),
    get_all_eff_relations(Direction, Entity).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a map of effective relations of given entity (by entity record).
%% @end
%%--------------------------------------------------------------------
-spec get_all_eff_relations(direction(), entity()) -> map_of_eff_relations().
get_all_eff_relations(bottom_up, #od_group{} = Group) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{od_user => EffUsers, od_group => EffGroups};
get_all_eff_relations(bottom_up, #od_space{} = Space) ->
    #od_space{eff_users = EffUsers, eff_groups = EffGroups} = Space,
    #{od_user => EffUsers, od_group => EffGroups};
get_all_eff_relations(bottom_up, #od_provider{} = Provider) ->
    #od_provider{eff_users = EffUsers, eff_groups = EffGroups} = Provider,
    #{od_user => EffUsers, od_group => EffGroups};
get_all_eff_relations(bottom_up, #od_handle_service{} = HService) ->
    #od_handle_service{eff_users = EffUsers, eff_groups = EffGroups} = HService,
    #{od_user => EffUsers, od_group => EffGroups};
get_all_eff_relations(bottom_up, #od_handle{} = Handle) ->
    #od_handle{eff_users = EffUsers, eff_groups = EffGroups} = Handle,
    #{od_user => EffUsers, od_group => EffGroups};
get_all_eff_relations(bottom_up, #od_harvester{} = Harvester) ->
    #od_harvester{eff_users = EffUsers, eff_groups = EffGroups} = Harvester,
    #{od_user => EffUsers, od_group => EffGroups};

get_all_eff_relations(top_down, #od_user{} = User) ->
    #od_user{
        eff_groups = EffGroups, eff_spaces = EffSpaces,
        eff_providers = EffProviders,
        eff_handle_services = EffHServices, eff_handles = EffHandles,
        eff_harvesters = EffHarvesters,
        eff_oz_privileges = EffOzPrivileges
    } = User,
    #{
        od_group => EffGroups, od_space => EffSpaces,
        od_provider => EffProviders,
        od_handle_service => EffHServices, od_handle => EffHandles,
        od_harvester => EffHarvesters,
        oz_privileges => EffOzPrivileges
    };
get_all_eff_relations(top_down, #od_group{} = Group) ->
    #od_group{
        eff_parents = EffGroups, eff_spaces = EffSpaces,
        eff_providers = EffProviders,
        eff_handle_services = EffHServices, eff_handles = EffHandles,
        eff_harvesters = EffHarvesters,
        eff_oz_privileges = EffOzPrivileges
    } = Group,
    #{
        od_group => EffGroups, od_space => EffSpaces,
        od_provider => EffProviders,
        od_handle_service => EffHServices, od_handle => EffHandles,
        od_harvester => EffHarvesters,
        oz_privileges => EffOzPrivileges
    };
get_all_eff_relations(top_down, #od_space{} = Space) ->
    #od_space{eff_providers = EffProviders} = Space,
    #{od_provider => EffProviders}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns all successors of given entity (direction-wise). Those entities will
%% be marked dirty if the effective relations of the entity change.
%% @end
%%--------------------------------------------------------------------
-spec get_successors(direction(), entity()) -> #{entity_type() => relations()}.
get_successors(bottom_up, #od_user{} = User) ->
    get_all_direct_relations(top_down, User);
get_successors(bottom_up, #od_group{} = Group) ->
    get_all_direct_relations(top_down, Group);
get_successors(bottom_up, #od_space{} = Space) ->
    #{od_provider := Providers} = get_all_direct_relations(top_down, Space),
    #{od_provider => get_ids(Providers)};

get_successors(top_down, #od_group{} = Group) ->
    #{
        od_user := Users, od_group := Groups
    } = get_all_direct_relations(bottom_up, Group),
    #{
        od_user => get_ids(Users),
        od_group => get_ids(Groups)
    };
get_successors(top_down, #od_space{} = Space) ->
    #{
        od_user := Users, od_group := Groups
    } = get_all_direct_relations(bottom_up, Space),
    #{
        od_user => get_ids(Users),
        od_group => get_ids(Groups)
    };
get_successors(top_down, #od_provider{} = Provider) ->
    #{od_space := Spaces} = get_all_direct_relations(bottom_up, Provider),
    #{od_space => get_ids(Spaces)};
get_successors(top_down, #od_handle_service{} = HService) ->
    #{
        od_user := Users, od_group := Groups
    } = get_all_direct_relations(bottom_up, HService),
    #{
        od_user => get_ids(Users),
        od_group => get_ids(Groups)
    };
get_successors(top_down, #od_handle{} = Handle) ->
    #{
        od_user := Users, od_group := Groups
    } = get_all_direct_relations(bottom_up, Handle),
    #{
        od_user => get_ids(Users),
        od_group => get_ids(Groups)
    };
get_successors(top_down, #od_harvester{} = Harvester) ->
    #{
        od_user := Users, od_group := Groups
    } = get_all_direct_relations(bottom_up, Harvester),
    #{
        od_user => get_ids(Users),
        od_group => get_ids(Groups)
    };
% All other relations should return an empty map (have no successors).
get_successors(_, _) ->
    #{}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates oz privileges of user or group and returns modified entity record.
%% @end
%%--------------------------------------------------------------------
-spec update_oz_privileges(entity(), [privileges:oz_privilege()]) ->
    entity().
update_oz_privileges(#od_user{} = User, NewOzPrivileges) ->
    User#od_user{oz_privileges = NewOzPrivileges};
update_oz_privileges(#od_group{} = Group, NewOzPrivileges) ->
    Group#od_group{oz_privileges = NewOzPrivileges}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates privileges in a relations_with_attrs().
%% @end
%%--------------------------------------------------------------------
-spec update_privileges(entity_id(), relations_with_attrs(entity_id(), privileges()),
    privileges(), privileges()) ->
    relations_with_attrs(entity_id(), privileges()).
update_privileges(EntityId, Relations, PrivsToGrant, PrivsToRevoke) ->
    OldPrivileges = maps:get(EntityId, Relations),
    NewPrivileges = privileges:union(PrivsToGrant,
        privileges:subtract(OldPrivileges, PrivsToRevoke)
    ),
    maps:put(EntityId, NewPrivileges, Relations).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Merges two maps of effective relations. Privileges and intermediaries are
%% merged as a union of two sets.
%% @end
%%--------------------------------------------------------------------
-spec merge_eff_relations(EffRelation, EffRelation) -> EffRelation when
    EffRelation :: eff_relations() | eff_relations_with_attrs() | [privileges:oz_privilege()].
merge_eff_relations(List1, List2) when is_list(List1) andalso is_list(List2) ->
    % Covers lists of atoms (effective oz privileges)
    ordsets_union(List1, List2);
merge_eff_relations(EffMap1, EffMap2) when is_map(EffMap1) andalso is_map(EffMap2) ->
    lists:foldl(
        fun({EntityId, EffRelation1}, MapAcc) ->
            NewValue = case maps:get(EntityId, EffMap1, undefined) of
                undefined ->
                    EffRelation1;
                EffRelation2 ->
                    case {EffRelation1, EffRelation2} of
                        {{Privs1, Int1}, {Privs2, Int2}} ->
                            % Covers eff_relations_with_attrs() type
                            {privileges:union(Privs1, Privs2), ordsets_union(Int1, Int2)};
                        {List1, List2} ->
                            % Covers eff_relations() type
                            ordsets_union(List1, List2)
                    end
            end,
            MapAcc#{EntityId => NewValue}
        end, EffMap1, maps:to_list(EffMap2)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts relations (with attrs or without) to effective relations.
%% @end
%%--------------------------------------------------------------------
-spec relations_to_eff_relations(relations() | relations_with_attrs(),
    intermediaries()) ->
    eff_relations() | eff_relations_with_attrs().
relations_to_eff_relations(List, Intermediaries) when is_list(List) ->
    lists:foldl(
        fun(NeighbourId, AccMap) ->
            AccMap#{NeighbourId => Intermediaries}
        end, #{}, List);
relations_to_eff_relations(Map, Intermediaries) when is_map(Map) ->
    maps:map(
        fun(_NeighbourId, Attributes) ->
            {Attributes, Intermediaries}
        end, Map).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts effective relations with attrs to relations with attrs.
%% @end
%%--------------------------------------------------------------------
-spec eff_relations_to_relations(eff_relations_with_attrs()) -> relations_with_attrs().
eff_relations_to_relations(Map) ->
    maps:map(
        fun(_NeighbourId, {Attributes, _Intermediaries}) ->
            Attributes
        end, Map).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Overrides effective relations of given types in a map of effective relations
%% with given effective relation (with or without attrs).
%% @end
%%--------------------------------------------------------------------
-spec override_eff_relations(map_of_eff_relations(), [entity_type()],
    intermediaries() | {attributes(), intermediaries()}) ->
    map_of_eff_relations().
override_eff_relations(MapOfEffRelations, [], _Value) ->
    MapOfEffRelations;
override_eff_relations(MapOfEffRelations, [EntityType | RestTypes], Value) ->
    OverridenPrivs = maps:map(fun(_Key, _Value) ->
        Value
    end, maps:get(EntityType, MapOfEffRelations)),
    NewMapOfEffRelations = MapOfEffRelations#{EntityType => OverridenPrivs},
    override_eff_relations(NewMapOfEffRelations, RestTypes, Value).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the bare list of ids of given relations.
%% @end
%%--------------------------------------------------------------------
-spec get_ids(relations() | relations_with_attrs() | eff_relations() | eff_relations_with_attrs()) ->
    relations().
get_ids(Relations) when is_list(Relations) ->
    Relations;
get_ids(RelationsWithAttrs) ->
    maps:keys(RelationsWithAttrs).


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
%% Updates an entity using given update function.
%% @end
%%--------------------------------------------------------------------
-spec update_entity(entity_type(), entity_id(),
    UpdateFun :: fun((entity()) -> entity())) -> ok | {error, term()}.
update_entity(EntityType, EntityId, UpdateFun) ->
    case EntityType:update(EntityId, UpdateFun) of
        {ok, _} -> ok;
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Runs a function synchronously locking on given entity.
%% @end
%%--------------------------------------------------------------------
-spec sync_on_entity(entity_type(), entity_id(),
    Function :: fun()) -> term().
sync_on_entity(EntityType, EntityId, Function) ->
    critical_section:run({EntityType, EntityId}, Function).
