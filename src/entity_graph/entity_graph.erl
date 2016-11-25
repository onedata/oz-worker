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
%%%     - od_share
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
%%%     1.3) providers and shares
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
-include("entity_logic_errors.hrl").

-define(STATE_KEY, <<"entity_graph_state">>).
-define(ENTITY_GRAPH_LOCK, entity_graph_lock).

%% API
-export([init_state/0, get_state/0]).
-export([schedule_refresh/0]).
-export([add_relation/4, add_relation/5, add_relation/6]).
%%-export([remove_relation/4]).

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
                entity_graph_state:create(
                    #document{key = ?STATE_KEY, value = #entity_graph_state{}}
                )
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves effective graph state, which is a globally shared singleton.
%% @end
%%--------------------------------------------------------------------
-spec get_state() -> #entity_graph_state{} | {error, term()}.
get_state() ->
    case entity_graph_state:get(?STATE_KEY) of
        {ok, #document{value = State}} ->
            State;
        Error ->
            Error
    end.


update_state(UpdateFun) ->
    {ok, _} = entity_graph_state:update(?STATE_KEY, UpdateFun),
    ok.


schedule_refresh() ->
    ?emergency("Refresh scheduled"),
    spawn(fun refresh_if_needed/0).


modify_group_relations(UpdateFunction) ->
    Success = critical_section:run(?ENTITY_GRAPH_LOCK, fun() ->
        #entity_graph_state{dirty = Dirty} = get_state(),
        case Dirty of
            true ->
                ?emergency("Cannot modify relations, waiting"),
                false;
            false ->
                ?emergency("Modifying relations..."),
                % TODO try catch
                UpdateFunction(),
                timer:sleep(1000), % TODO Zeby nie za czesto
                update_state(fun(EffGraphState) ->
                    {ok, EffGraphState#entity_graph_state{dirty = true}}
                end),
                true
        end
    end),
    % Schedule a refresh no matter if the operation succeeded
    schedule_refresh(),
    case Success of
        false ->
            % Try again in some time to acquire the lock and make modifications
            timer:sleep(100),
            modify_group_relations(UpdateFunction);
        true ->
            ok
    end.


%%--------------------------------------------------------------------
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
        #entity_graph_state{dirty = Dirty} = get_state(),
        case Dirty of
            true ->
                % TODO try catch, jak blad to spimy z 2 sekundy i schedule
                try
                    ?emergency("Refresh needed, performing..."),
                    refresh_entity_graph(),
                    update_state(fun(EffGraphState) ->
                        {ok, EffGraphState#entity_graph_state{dirty = false}}
                    end),
                    ?emergency("Refresh done")
                catch T:M ->
                    ?error_stacktrace("BLEH: ~p ~p", [T, M]),
                    timer:sleep(1500)
                end;
            false ->
                ?emergency("Refresh skipped."),
                ok
        end
    end),
    ok.


refresh_entity_graph() ->
    #entity_graph_state{
        bottom_up_dirty = BottomUp,
        top_down_dirty = TopDown
    } = get_state(),
    ?emergency("Refresh round:~nBottom-up dirty:~p~nTop-down dirty:~p", [
        BottomUp, TopDown
    ]),
    case BottomUp of
        [{_, ModelBU, IdBU} | _] ->
            refresh_entity(bottom_up, ModelBU, IdBU),
            refresh_entity_graph();
        [] ->
            case TopDown of
                [{_, ModelTD, IdTD} | _] ->
                    refresh_entity(top_down, ModelTD, IdTD),
                    refresh_entity_graph();
                [] ->
                    ok
            end
    end.


refresh_entity(WhichWay, ModelType, EntityId) ->
    OtherWay = case WhichWay of
        top_down -> bottom_up;
        bottom_up -> top_down
    end,
    {ok, #document{value = Entity}} = ModelType:get(EntityId),
    ?emergency("Refreshing (~p) ~s", [WhichWay, readable(EntityId, Entity)]),
    % Get neighbours of the entity in given direction
    NeighboursMap = extract_neighbours(WhichWay, Entity),
    ?emergency("NeighboursMap: ~p", [NeighboursMap]),
    % Check which neighbours have effective relations to aggregate
    ToAggregate = neighbours_with_eff_relations(WhichWay, ModelType),
    ?emergency("ToAggregate: ~p", [ToAggregate]),
    % Get significant effective relations from all neighbours
    EffRelationsOfNeighbours = lists:flatmap(
        fun(NbrType) ->
            Neighbours = maps:get(NbrType, NeighboursMap),
            lists:map(
                fun(Neighbour) ->
                    {NbrId, RelationAttrs} = case Neighbour of
                        {NId, NRA} ->
                            {NId, NRA};
                        NId ->
                            {NId, undefined}
                    end,
                    {ok, #document{value = NbrEntity}} = NbrType:get(NbrId),
                    eff_relations_of_neighbour(
                        WhichWay, ModelType, NbrEntity, RelationAttrs
                    )
                end, Neighbours)
        end, ToAggregate),
    ?emergency("EffRelationsOfNeighbours: ~p", [EffRelationsOfNeighbours]),
    % Aggregate all effective relations - from neighbours and the entity itself
    AggregatedEffRelations = lists:foldl(
        fun(RelationsMap, AccMapOuter) ->
            lists:foldl(
                fun({NType, NList}, AccMapInner) ->
                    NewNeighbours = aggregate_eff_neighbours(
                        NList, maps:get(NType, AccMapInner, [])
                    ),
                    AccMapInner#{NType => NewNeighbours}
                end, AccMapOuter, maps:to_list(RelationsMap))
        end, #{}, [NeighboursMap | EffRelationsOfNeighbours]),
    ?emergency("AggregatedEffRelations: ~p", [AggregatedEffRelations]),
    % Check if neighbours should be marked dirty (depending on direction)
    case AggregatedEffRelations =:= eff_relations(WhichWay, Entity) of
        true ->
            % Nothing changed, no action is needed
            ?emergency("Nothing changed"),
            ok;
        false ->
            % Mark all neighbours as dirty
            % (these that are in the direction of update)
            Successors = extract_neighbours(OtherWay, Entity),
            ?emergency("Successors: ~p", [Successors]),
            lists:foreach(
                fun({NType, NList}) ->
                    lists:foreach(
                        fun(NId) ->
                            NType:update(NId, fun(Ent) ->
                                {ok, mark_dirty(WhichWay, true, NType, NId, Ent)}
                            end)
                        end, NList)
                end, maps:to_list(Successors))
    end,
    % Update the record marking it not dirty and setting newly calculated
    % effective relations.
    {ok, _} = ModelType:update(EntityId, fun(Ent) ->
        {ok, mark_dirty(WhichWay, false, ModelType, EntityId, update_eff_relations(
            WhichWay, Ent, AggregatedEffRelations
        ))}
    end),
    ok.


add_relation(od_space, GroupId, od_share, ShareId) ->
    add_relation(od_space, GroupId, undefined, od_share, ShareId, undefined).


add_relation(od_user, UserId, od_group, GroupId, Privileges) ->
    add_relation(od_user, UserId, Privileges, od_group, GroupId, undefined);
add_relation(od_user, UserId, od_space, SpaceId, Privileges) ->
    add_relation(od_user, UserId, Privileges, od_space, SpaceId, undefined);
add_relation(od_user, UserId, od_handle, HandleId, Privileges) ->
    add_relation(od_user, UserId, Privileges, od_handle, HandleId, undefined);
add_relation(od_user, UserId, od_handle_service, HServiceId, Privileges) ->
    add_relation(od_user, UserId, Privileges, od_handle_service, HServiceId, undefined);

add_relation(od_group, ChildId, od_group, ParentId, Privs) ->
    modify_group_relations(fun() ->
        add_relation(od_group, ChildId, Privs, od_group, ParentId, undefined)
    end);
add_relation(od_group, GroupId, od_space, SpaceId, Privileges) ->
    add_relation(od_group, GroupId, Privileges, od_space, SpaceId, undefined);
add_relation(od_group, GroupId, od_handle, HandleId, Privileges) ->
    add_relation(od_group, GroupId, Privileges, od_handle, HandleId, undefined);
add_relation(od_group, GroupId, od_handle_service, HServiceId, Privileges) ->
    add_relation(od_group, GroupId, Privileges, od_handle_service, HServiceId, undefined);

add_relation(od_space, GroupId, od_provider, ProviderId, SupportSize) ->
    add_relation(od_space, GroupId, undefined, od_provider, ProviderId, SupportSize).


add_relation(ChildModel, ChildId, ChildAttributes, ParentModel, ParentId, ParentAttributes) ->
    ParentUpdateFun = fun(Parent) ->
        case has_child(Parent, ChildModel, ChildId) of
            true ->
                ?EL_RELATION_EXISTS;
            false ->
                {ok, mark_dirty(bottom_up, true, ParentModel, ParentId, add_child(
                    Parent, ChildModel, ChildId, ChildAttributes
                ))}
        end
    end,
    ChildUpdateFun = fun(Child) ->
        case has_parent(Child, ParentModel, ParentId) of
            true ->
                ?EL_RELATION_EXISTS;
            false ->
                {ok, mark_dirty(top_down, true, ChildModel, ChildId, add_parent(
                    Child, ParentModel, ParentId, ParentAttributes
                ))}
        end
    end,
    ParentRevertFun = fun(Parent) ->
        {ok, mark_dirty(bottom_up, true, ParentModel, ParentId, remove_child(
            Parent, ChildModel, ChildId
        ))}
    end,
    case ParentModel:update(ParentId, ParentUpdateFun) of
        {ok, _} ->
            case ChildModel:update(ChildId, ChildUpdateFun) of
                {ok, _} ->
                    ok;
                ?EL_RELATION_EXISTS ->
                    % Relation exists, but apparently it did not exist in
                    % the parent, so we just fixed the relation -> ok.
                    ok;
                Err1 ->
                    % Some other error, we have to attempt reverting the
                    % relation in parent.
                    ParentModel:update(ParentId, ParentRevertFun),
                    Err1
            end;
        Err2 ->
            Err2
    end,
    schedule_refresh(),
    ok.


mark_dirty(WhichWay, Flag, Model, Id, Entity) ->
    Priority = get_priority(WhichWay, Entity),
    update_state(
        fun(EffGraphState) ->
            #entity_graph_state{
                bottom_up_dirty = BottomUpDirty,
                top_down_dirty = TopDownDirty
            } = EffGraphState,
            NewState = case {WhichWay, Flag} of
                {bottom_up, true} ->
                    ?emergency("bottom-up dirty: ~s", [readable(Id, Entity)]),
                    EffGraphState#entity_graph_state{
                        bottom_up_dirty = lists:sort([{Priority, Model, Id} | BottomUpDirty])
                    };
                {bottom_up, false} ->
                    ?emergency("bottom-up clean: ~s", [readable(Id, Entity)]),
                    EffGraphState#entity_graph_state{
                        bottom_up_dirty = lists:keydelete(Id, 3, BottomUpDirty)
                    };
                {top_down, true} ->
                    ?emergency("top-down dirty: ~s", [readable(Id, Entity)]),
                    EffGraphState#entity_graph_state{
                        top_down_dirty = lists:sort([{Priority, Model, Id} | TopDownDirty])
                    };
                {top_down, false} ->
                    ?emergency("top-down clean: ~s", [readable(Id, Entity)]),
                    EffGraphState#entity_graph_state{
                        top_down_dirty = lists:keydelete(Id, 3, TopDownDirty)
                    }
            end,
            {ok, NewState#entity_graph_state{dirty = true}}
        end),
    set_dirty_flag(WhichWay, Flag, Entity).


% Priorities for records during effective graph recomputation

% For bottom-up:
%   1) groups (sorted by children num)
%   2) spaces, handles and handle_services
%   3) providers and shares
get_priority(bottom_up, #od_group{children = Children}) ->
    length(Children) + 1;
get_priority(bottom_up, #od_space{}) ->
    1000000000;
get_priority(bottom_up, #od_handle_service{}) ->
    1000000000;
get_priority(bottom_up, #od_handle{}) ->
    1000000000;
get_priority(bottom_up, #od_provider{}) ->
    1000000001;
get_priority(bottom_up, #od_share{}) ->
    1000000001;
% For top-down:
%   1) spaces
%   1) groups (sorted by parents num)
%   3) users
get_priority(top_down, #od_space{}) ->
    -1000000000;
get_priority(top_down, #od_group{parents = Parents}) ->
    length(Parents) + 1;
get_priority(top_down, #od_user{}) ->
    1000000000.


set_dirty_flag(bottom_up, Flag, #od_group{} = Group) ->
    Group#od_group{bottom_up_dirty = Flag};
set_dirty_flag(bottom_up, Flag, #od_space{} = Space) ->
    Space#od_space{bottom_up_dirty = Flag};
set_dirty_flag(bottom_up, Flag, #od_share{} = Share) ->
    Share#od_share{bottom_up_dirty = Flag};
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


has_child(#od_provider{spaces = Spaces}, od_space, SpaceId) ->
    lists:member(SpaceId, Spaces);

has_child(#od_share{space = Space}, od_space, SpaceId) ->
    SpaceId =:= Space;

has_child(#od_space{users = Users}, od_user, UserId) ->
    proplists:is_defined(UserId, Users);
has_child(#od_space{groups = Groups}, od_group, GroupId) ->
    proplists:is_defined(GroupId, Groups);

has_child(#od_handle_service{users = Users}, od_user, UserId) ->
    proplists:is_defined(UserId, Users);
has_child(#od_handle_service{groups = Groups}, od_group, GroupId) ->
    proplists:is_defined(GroupId, Groups);

has_child(#od_handle{users = Users}, od_user, UserId) ->
    proplists:is_defined(UserId, Users);
has_child(#od_handle{groups = Groups}, od_group, GroupId) ->
    proplists:is_defined(GroupId, Groups);

has_child(#od_group{users = Users}, od_user, UserId) ->
    proplists:is_defined(UserId, Users);
has_child(#od_group{children = Children}, od_group, GroupId) ->
    proplists:is_defined(GroupId, Children).


add_child(#od_provider{spaces = Spaces} = Provider, od_space, SpaceId, _) ->
    Provider#od_provider{spaces = [SpaceId | Spaces]};

add_child(#od_share{} = Share, od_space, SpaceId, _) ->
    Share#od_share{space = SpaceId};

add_child(#od_space{users = Users} = Space, od_user, UserId, Privs) ->
    Space#od_space{users = [{UserId, Privs} | Users]};
add_child(#od_space{groups = Groups} = Space, od_group, GroupId, Privs) ->
    Space#od_space{groups = [{GroupId, Privs} | Groups]};

add_child(#od_handle_service{users = Users} = HS, od_user, UserId, Privs) ->
    HS#od_handle_service{users = [{UserId, Privs} | Users]};
add_child(#od_handle_service{groups = Groups} = HS, od_group, GroupId, Privs) ->
    HS#od_handle_service{groups = [{GroupId, Privs} | Groups]};

add_child(#od_handle{users = Users} = Handle, od_user, UserId, Privs) ->
    Handle#od_handle{users = [{UserId, Privs} | Users]};
add_child(#od_handle{groups = Groups} = Handle, od_group, GroupId, Privs) ->
    Handle#od_handle{groups = [{GroupId, Privs} | Groups]};

add_child(#od_group{users = Users} = Group, od_user, UserId, Privs) ->
    Group#od_group{users = [{UserId, Privs} | Users]};
add_child(#od_group{children = Children} = Group, od_group, GroupId, Privs) ->
    Group#od_group{children = [{GroupId, Privs} | Children]}.


remove_child(#od_provider{spaces = Spaces} = Provider, od_space, SpaceId) ->
    Provider#od_provider{spaces = lists:delete(SpaceId, Spaces)};

remove_child(#od_share{} = Share, od_space, _SpaceId) ->
    Share#od_share{space = undefined};

remove_child(#od_space{users = Users} = Space, od_user, UserId) ->
    Space#od_space{users = lists:keydelete(UserId, 1, Users)};
remove_child(#od_space{groups = Groups} = Space, od_group, GroupId) ->
    Space#od_space{groups = lists:keydelete(GroupId, 1, Groups)};

remove_child(#od_handle_service{users = Users} = HS, od_user, UserId) ->
    HS#od_handle_service{users = lists:keydelete(UserId, 1, Users)};
remove_child(#od_handle_service{groups = Groups} = HS, od_group, GroupId) ->
    HS#od_handle_service{groups = lists:keydelete(GroupId, 1, Groups)};

remove_child(#od_handle{users = Users} = Handle, od_user, UserId) ->
    Handle#od_handle{users = lists:keydelete(UserId, 1, Users)};
remove_child(#od_handle{groups = Groups} = Handle, od_group, GroupId) ->
    Handle#od_handle{groups = lists:keydelete(GroupId, 1, Groups)};

remove_child(#od_group{users = Users} = Group, od_user, UserId) ->
    Group#od_group{users = lists:keydelete(UserId, 1, Users)};
remove_child(#od_group{children = Children} = Group, od_group, GroupId) ->
    Group#od_group{children = lists:keydelete(GroupId, 1, Children)}.


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

has_parent(#od_space{shares = Shares}, od_share, ShareId) ->
    lists:member(ShareId, Shares);
has_parent(#od_space{providers_supports = Providers}, od_provider, ProviderId) ->
    proplists:is_defined(ProviderId, Providers).


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

add_parent(#od_space{shares = Shares} = Space, od_share, ShareId, _) ->
    Space#od_space{shares = [ShareId | Shares]};
add_parent(#od_space{providers_supports = Providers} = Space, od_provider, ProviderId, SupportSize) ->
    Space#od_space{providers_supports = [{ProviderId, SupportSize} | Providers]}.


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

remove_parent(#od_space{shares = Shares} = Space, od_share, ShareId) ->
    Space#od_space{shares = lists:delete(ShareId, Shares)};
remove_parent(#od_space{providers_supports = Providers} = Space, od_provider, ProviderId) ->
    Space#od_space{providers_supports = lists:keydelete(ProviderId, 1, Providers)}.


extract_neighbours(bottom_up, #od_group{} = Group) ->
    #od_group{users = Users, children = Groups} = Group,
    #{od_user => Users, od_group => Groups};
extract_neighbours(bottom_up, #od_space{} = Space) ->
    #od_space{users = Users, groups = Groups} = Space,
    #{od_user => Users, od_group => Groups};
extract_neighbours(bottom_up, #od_share{} = Share) ->
    #od_share{space = Space} = Share,
    #{od_space => [Space]};
extract_neighbours(bottom_up, #od_provider{} = Provider) ->
    #od_provider{spaces = Spaces} = Provider,
    #{od_space => Spaces};
extract_neighbours(bottom_up, #od_handle_service{} = HService) ->
    #od_handle_service{users = Users, groups = Groups} = HService,
    #{od_user => Users, od_group => Groups};
extract_neighbours(bottom_up, #od_handle{} = Handle) ->
    #od_handle{users = Users, groups = Groups} = Handle,
    #{od_user => Users, od_group => Groups};
extract_neighbours(top_down, #od_user{} = User) ->
    #od_user{
        groups = Groups, spaces = Spaces,
        handle_services = HServices, handles = Handles
    } = User,
    #{
        od_group => Groups, od_space => Spaces,
        od_handle_service => HServices, od_handle => Handles
    };
extract_neighbours(top_down, #od_group{} = Group) ->
    #od_group{
        parents = Groups, spaces = Spaces,
        handle_services = HServices, handles = Handles
    } = Group,
    #{
        od_group => Groups, od_space => Spaces,
        od_handle_service => HServices, od_handle => Handles
    };
extract_neighbours(top_down, #od_space{} = Space) ->
    #od_space{providers_supports = Providers, shares = Shares} = Space,
    #{od_provider => element(1, lists:unzip(Providers)), od_share => Shares};
% Above clauses cover all the relations, so return an empty list in other cases.
extract_neighbours(_, _) ->
    #{}.


neighbours_with_eff_relations(bottom_up, od_group) ->
    [od_group];
neighbours_with_eff_relations(bottom_up, od_space) ->
    [od_group];
neighbours_with_eff_relations(bottom_up, od_share) ->
    [od_space];
neighbours_with_eff_relations(bottom_up, od_provider) ->
    [od_space];
neighbours_with_eff_relations(bottom_up, od_handle_service) ->
    [od_group];
neighbours_with_eff_relations(bottom_up, od_handle) ->
    [od_group];

neighbours_with_eff_relations(top_down, od_user) ->
    [od_group, od_space];
neighbours_with_eff_relations(top_down, od_group) ->
    [od_group, od_space];
neighbours_with_eff_relations(top_down, od_space) ->
    [].



eff_relations_of_neighbour(bottom_up, od_group, #od_group{} = Group, Privs) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{
        od_user => override_privileges(EffUsers, Privs),
        od_group => override_privileges(EffGroups, Privs)
    };
eff_relations_of_neighbour(bottom_up, od_space, #od_group{} = Group, Privs) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{
        od_user => override_privileges(EffUsers, Privs),
        od_group => override_privileges(EffGroups, Privs)
    };
eff_relations_of_neighbour(bottom_up, od_share, #od_space{} = Space, _) ->
    #od_space{eff_users = EffUsers, eff_groups = EffGroups} = Space,
    #{od_user => EffUsers, od_group => EffGroups};
eff_relations_of_neighbour(bottom_up, od_provider, #od_space{} = Space, _) ->
    #od_space{eff_users = EffUsers, eff_groups = EffGroups} = Space,
    #{od_user => EffUsers, od_group => EffGroups};
eff_relations_of_neighbour(bottom_up, od_handle_service, #od_group{} = Group, Privs) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{
        od_user => override_privileges(EffUsers, Privs),
        od_group => override_privileges(EffGroups, Privs)
    };
eff_relations_of_neighbour(bottom_up, od_handle, #od_group{} = Group, Privs) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{
        od_user => override_privileges(EffUsers, Privs),
        od_group => override_privileges(EffGroups, Privs)
    };

eff_relations_of_neighbour(top_down, od_user, #od_group{} = Group, _) ->
    #od_group{
        eff_parents = EffGroups,
        eff_spaces = EffSpaces,
        eff_shares = EffShares,
        eff_providers = EffProviders,
        eff_handle_services = EffHServices,
        eff_handles = EffHandles
    } = Group,
    #{
        od_group => EffGroups, od_space => EffSpaces,
        od_share => EffShares, od_provider => EffProviders,
        od_handle_service => EffHServices, od_handle => EffHandles
    };
eff_relations_of_neighbour(top_down, od_user, #od_space{} = Space, _) ->
    #od_space{providers_supports = Providers, shares = Shares} = Space,
    #{od_provider => element(1, Providers), od_share => Shares};
eff_relations_of_neighbour(top_down, od_group, #od_group{} = Group, _) ->
    #od_group{
        eff_parents = EffGroups,
        eff_spaces = EffSpaces,
        eff_shares = EffShares,
        eff_providers = EffProviders,
        eff_handle_services = EffHServices,
        eff_handles = EffHandles
    } = Group,
    #{
        od_group => EffGroups, od_space => EffSpaces,
        od_share => EffShares, od_provider => EffProviders,
        od_handle_service => EffHServices, od_handle => EffHandles
    };
eff_relations_of_neighbour(top_down, od_group, #od_space{} = Space, _) ->
    #od_space{providers_supports = Providers, shares = Shares} = Space,
    #{od_provider => element(1, lists:unzip(Providers)), od_share => Shares}.


eff_relations(bottom_up, #od_group{} = Group) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{od_user => EffUsers, od_group => EffGroups};
eff_relations(bottom_up, #od_space{} = Space) ->
    #od_space{eff_users = EffUsers, eff_groups = EffGroups} = Space,
    #{od_user => EffUsers, od_group => EffGroups};
eff_relations(bottom_up, #od_share{} = Share) ->
    #od_share{eff_users = EffUsers, eff_groups = EffGroups} = Share,
    #{od_user => EffUsers, od_group => EffGroups};
eff_relations(bottom_up, #od_provider{} = Provider) ->
    #od_provider{eff_users = EffUsers, eff_groups = EffGroups} = Provider,
    #{od_user => EffUsers, od_group => EffGroups};
eff_relations(bottom_up, #od_handle_service{} = HService) ->
    #od_handle_service{eff_users = EffUsers, eff_groups = EffGroups} = HService,
    #{od_user => EffUsers, od_group => EffGroups};
eff_relations(bottom_up, #od_handle{} = Handle) ->
    #od_handle{eff_users = EffUsers, eff_groups = EffGroups} = Handle,
    #{od_user => EffUsers, od_group => EffGroups};

eff_relations(top_down, #od_user{} = User) ->
    #od_user{
        eff_groups = EffGroups, eff_spaces = EffSpaces,
        eff_shares = EffShares, eff_providers = EffProviders,
        eff_handle_services = EffHServices, eff_handles = EffHandles
    } = User,
    #{
        od_group => EffGroups, od_space => EffSpaces,
        od_share => EffShares, od_provider => EffProviders,
        od_handle_service => EffHServices, od_handle => EffHandles
    };
eff_relations(top_down, #od_group{} = Group) ->
    #od_group{
        eff_parents = EffGroups, eff_spaces = EffSpaces,
        eff_shares = EffShares, eff_providers = EffProviders,
        eff_handle_services = EffHServices, eff_handles = EffHandles
    } = Group,
    #{
        od_group => EffGroups, od_space => EffSpaces,
        od_share => EffShares, od_provider => EffProviders,
        od_handle_service => EffHServices, od_handle => EffHandles
    };
eff_relations(top_down, #od_space{}) ->
    #{}.


update_eff_relations(bottom_up, #od_group{} = Group, EffNeighbours) ->
    #{od_user := EffUsers, od_group := EffGroups} = EffNeighbours,
    Group#od_group{eff_users = EffUsers, eff_children = EffGroups};
update_eff_relations(bottom_up, #od_space{} = Space, EffNeighbours) ->
    #{od_user := EffUsers, od_group := EffGroups} = EffNeighbours,
    Space#od_space{eff_users = EffUsers, eff_groups = EffGroups};
update_eff_relations(bottom_up, #od_share{} = Share, EffNeighbours) ->
    #{od_user := EffUsers, od_group := EffGroups} = EffNeighbours,
    Share#od_share{eff_users = EffUsers, eff_groups = EffGroups};
update_eff_relations(bottom_up, #od_provider{} = Provider, EffNeighbours) ->
    #{od_user := EffUsers, od_group := EffGroups} = EffNeighbours,
    Provider#od_provider{eff_users = EffUsers, eff_groups = EffGroups};
update_eff_relations(bottom_up, #od_handle_service{} = HService, EffNeighbours) ->
    #{od_user := EffUsers, od_group := EffGroups} = EffNeighbours,
    HService#od_handle_service{eff_users = EffUsers, eff_groups = EffGroups};
update_eff_relations(bottom_up, #od_handle{} = Handle, EffNeighbours) ->
    #{od_user := EffUsers, od_group := EffGroups} = EffNeighbours,
    Handle#od_handle{eff_users = EffUsers, eff_groups = EffGroups};

update_eff_relations(top_down, #od_user{} = User, EffNeighbours) ->
    #{
        od_group := EffGroups, od_space := EffSpaces,
        od_share := EffShares, od_provider := EffProviders,
        od_handle_service := EffHServices, od_handle := EffHandles
    } = EffNeighbours,
    User#od_user{
        eff_groups = EffGroups, eff_spaces = EffSpaces,
        eff_shares = EffShares, eff_providers = EffProviders,
        eff_handle_services = EffHServices, eff_handles = EffHandles
    };
update_eff_relations(top_down, #od_group{} = Group, EffNeighbours) ->
    #{
        od_group := EffGroups, od_space := EffSpaces,
        od_share := EffShares, od_provider := EffProviders,
        od_handle_service := EffHServices, od_handle := EffHandles
    } = EffNeighbours,
    Group#od_group{
        eff_parents = EffGroups, eff_spaces = EffSpaces,
        eff_shares = EffShares, eff_providers = EffProviders,
        eff_handle_services = EffHServices, eff_handles = EffHandles
    };
update_eff_relations(top_down, #od_space{} = Space, _) ->
    Space.


%%refresh_entity_top_down(ModelType, EntityId) ->
%%    {ok, #document{value = Entity}} = ModelType:get(EntityId),
%%    ParentRelations = extract_parents(Entity),
%%    ToAggregate = parents_to_aggregate_from(ModelType),
%%    EffRelations = lists:flatmap(
%%        fun(ParentType) ->
%%            Parents = maps:get(ParentType, ParentRelations),
%%            lists:map(
%%                fun(ParentId) ->
%%                    aggregate_from_parent(ModelType, ParentType, ParentId)
%%                end, Parents)
%%        end, ToAggregate),
%%    AggregatedParentRelations = lists:foldl(
%%        fun(RelationsMap, AccMapOuter) ->
%%            lists:foldl(
%%                fun({ModelType, Entities}, AccMapInner) ->
%%                    NewEntities = ordsets:union(
%%                        ordsets:from_list(Entities),
%%                        maps:get(ModelType, RelationsMap, [])
%%                    ),
%%                    AccMapInner#{ModelType => NewEntities}
%%                end, AccMapOuter, maps:to_list(RelationsMap))
%%        end, #{}, [ParentRelations | EffRelations]),
%%    % Check if parents should be marked dirty
%%    case AggregatedParentRelations =:= eff_relations(Entity) of
%%        true ->
%%            % Nothing changed, no action is needed
%%            ok;
%%        false ->
%%            % Mark all children as dirty
%%            ChildrenRelations = extract_children(Entity),
%%            lists:foreach(
%%                fun({ModelType, Entities}) ->
%%                    lists:foreach(
%%                        fun(EntityId) ->
%%                            ModelType:update(EntityId, fun(E) ->
%%                                {ok, mark_dirty(top_down, true, ModelType, EntityId, E)}
%%                            end)
%%                        end, Entities)
%%                end, maps:to_list(ChildrenRelations))
%%    end,
%%    % Update the record marking it not dirty and setting newly calculated
%%    % effective relations.
%%    {ok, _} = ModelType:update(EntityId, fun(Ent) ->
%%        {ok, mark_dirty(top_down, false, ModelType, EntityId, update_eff_relations(
%%            Ent, AggregatedParentRelations
%%        ))}
%%    end),
%%    ok.

%%% TODO should return lists without privs
%%extract_parents(#od_user{} = User) ->
%%    #od_user{
%%        groups = Groups, spaces = Spaces,
%%        handle_services = HServices, handles = Handles
%%    } = User,
%%    #{
%%        od_group => Groups, od_space => Spaces,
%%        od_handle_service => HServices, od_handle => Handles
%%    }.
%%
%%
%%parents_to_aggregate_from(od_user) ->
%%    [od_space, od_group].
%%
%%aggregate_from_parent(od_user, od_space, SpaceId) ->
%%    {ok, #document{value = #od_space{
%%        providers_supports = Providers,
%%        shares = Shares
%%    }}} = od_space:get(SpaceId),
%%    #{od_provider => element(1, Providers), od_share => Shares};
%%aggregate_from_parent(od_user, od_group, GroupId) ->
%%    {ok, #document{value = #od_group{
%%        eff_parents = EffGroups,
%%        eff_spaces = EffSpaces,
%%        eff_shares = EffShares,
%%        eff_providers = EffProviders,
%%        eff_handle_services = EffHServices,
%%        eff_handles = EffHandles
%%    }}} = od_group:get(GroupId),
%%    #{
%%        od_group => EffGroups, od_space => EffSpaces,
%%        od_share => EffShares, od_provider => EffProviders,
%%        od_handle_service => EffHServices, od_handle => EffHandles
%%    }.
%%
%%
%%eff_relations(#od_user{} = User) ->
%%    #od_user{
%%        eff_groups = EffGroups, eff_spaces = EffSpaces,
%%        eff_shares = EffShares, eff_providers = EffProviders,
%%        eff_handle_services = EffHServices, eff_handles = EffHandles
%%    } = User,
%%    #{
%%        od_group => EffGroups, od_space => EffSpaces,
%%        od_share => EffShares, od_provider => EffProviders,
%%        od_handle_service => EffHServices, od_handle => EffHandles
%%    }.
%%
%%
%%update_eff_relations(#od_user{} = User, AggregatedParentRelations) ->
%%    #{
%%        od_group => EffGroups, od_space => EffSpaces,
%%        od_share => EffShares, od_provider => EffProviders,
%%        od_handle_service => EffHServices, od_handle => EffHandles
%%    } = AggregatedParentRelations,
%%    User#od_user{
%%        eff_groups = EffGroups, eff_spaces = EffSpaces,
%%        eff_shares = EffShares, eff_providers = EffProviders,
%%        eff_handle_services = EffHServices, eff_handles = EffHandles
%%    }.
%%
%%
%%extract_children(#od_space{users = Users, groups = Groups}) ->
%%    #{users => Users, groups => Groups}.


%%refresh_entity_bottom_up(od_group, GroupId) ->
%%    {ok, #document{value = #od_group{
%%        parents = Parents,
%%        children = Children,
%%        users = Users,
%%
%%        eff_children = OldEffChildren,
%%        eff_users = OldEffUsers
%%    }}} = od_group:get(GroupId),
%%    EffRelationsOfChildren = lists:foldl(
%%        fun({ChId, PrivsInGroup}, AccMap) ->
%%            {ok, #document{
%%                value = #od_group{
%%                    eff_children = EC,
%%                    eff_users = EU
%%                }}} = od_group:get(ChId),
%%            AccMap#{
%%                eff_children => ordsets:union(
%%                    ordsets:from_list(override_privileges(EC, PrivsInGroup)),
%%                    maps:get(eff_children, AccMap, [])),
%%                eff_users => ordsets:union(
%%                    ordsets:from_list(override_privileges(EU, PrivsInGroup)),
%%                    maps:get(eff_users, AccMap, []))
%%            }
%%        end, #{}, Children),
%%    NewEffChildren = privileges:proplists_union(
%%        Children ++ maps:get(eff_children, EffRelationsOfChildren, [])
%%    ),
%%    NewEffUsers = privileges:proplists_union(
%%        Users ++ maps:get(eff_users, EffRelationsOfChildren, [])
%%    ),
%%    % Update the record TODO MOZE TYLKO WTEDY JAK SIE ZMIENIL?
%%    {ok, _} = od_group:update(GroupId, fun(Group) ->
%%        {ok, Group#od_group{
%%            eff_children = NewEffChildren,
%%            eff_users = NewEffUsers
%%        }}
%%    end),
%%    % TODO mark_bottom_up_clean(IdBU),
%%    % Check if related entities should be marked dirty
%%    EffChildrenChanged = did_relations_change(OldEffChildren, NewEffChildren),
%%    EffUsersChanged = did_relations_change(OldEffUsers, NewEffUsers),
%%    case EffChildrenChanged orelse EffUsersChanged of
%%        false ->
%%            ok;
%%        true ->
%%            % Mark parent groups as bottom-up dirty.
%%            lists:foreach(
%%                fun(ParentId) ->
%%                    {ok, _} = od_group:update(ParentId, fun(Group) ->
%%                        #od_group{children = Groups} = Group,
%%                        mark_bottom_up_dirty(?PRIORITY_GROUP_BU(length(Groups)),
%%                            od_group, ParentId),
%%                        {ok, Group#od_group{bottom_up_dirty = true}}
%%                    end)
%%                end, Parents)
%%    end.
%%
%%
%%refresh_entity_top_down(od_group, GroupId) ->
%%    {ok, #document{value = #od_group{
%%        parents = Parents,
%%        spaces = Spaces,
%%        handle_services = HandleServices,
%%        handles = Handles,
%%
%%        eff_parents = OldEffParents,
%%        eff_spaces = OldEffSpaces,
%%        eff_shares = OldEffShares,
%%        eff_providers = OldEffProviders,
%%        eff_handle_services = OldEffHandleServices,
%%        eff_handles = OldEffHandles
%%    }}} = od_group:get(GroupId),
%%    EffRelationsOfParents = lists:foldl(
%%        fun(ParentId, AccMap) ->
%%            {ok, #document{
%%                value = #od_group{
%%                    eff_parents = EPa,
%%                    eff_spaces = ESp,
%%                    eff_shares = ESh,
%%                    eff_providers = EPr,
%%                    eff_handle_services = EHS,
%%                    eff_handles = EH
%%                }}} = od_group:get(ParentId),
%%            AccMap#{
%%                eff_parents => ordsets:union(ordsets:from_list(EPa),
%%                    maps:get(eff_parents, AccMap, [])),
%%                eff_spaces => ordsets:union(ordsets:from_list(ESp),
%%                    maps:get(eff_spaces, AccMap, [])),
%%                eff_shares => ordsets:union(ordsets:from_list(ESh),
%%                    maps:get(eff_shares, AccMap, [])),
%%                eff_providers => ordsets:union(ordsets:from_list(EPr),
%%                    maps:get(eff_providers, AccMap, [])),
%%                eff_h_services => ordsets:union(ordsets:from_list(EHS),
%%                    maps:get(eff_h_services, AccMap, [])),
%%                eff_handles => ordsets:union(ordsets:from_list(EH),
%%                    maps:get(eff_handles, AccMap, []))
%%            }
%%        end, #{}, Parents),
%%    RelationsOfSpaces = lists:foldl(
%%        fun(SpaceId, AccMap) ->
%%            {ok, #document{
%%                value = #od_space{
%%                    shares = Sh,
%%                    providers_supports = PrSup
%%                }}} = od_space:get(SpaceId),
%%            {Pr, _} = lists:unzip(PrSup),
%%            AccMap#{
%%                shares => ordsets:union(
%%                    ordsets:from_list(Sh),
%%                    maps:get(shares, AccMap, [])),
%%                providers => ordsets:union(
%%                    ordsets:from_list(Pr),
%%                    maps:get(providers, AccMap, []))
%%            }
%%        end, #{}, Spaces),
%%    Shares = maps:get(shares, RelationsOfSpaces, []),
%%    Providers = maps:get(providers, RelationsOfSpaces, []),
%%    NewEffParents = ordsets:union(
%%        Parents, maps:get(eff_parents, EffRelationsOfParents, [])
%%    ),
%%    NewEffSpaces = ordsets:union(
%%        Spaces, maps:get(eff_spaces, EffRelationsOfParents, [])
%%    ),
%%    NewEffShares = ordsets:union(
%%        Shares, maps:get(eff_shares, EffRelationsOfParents, [])
%%    ),
%%    NewEffProviders = ordsets:union(
%%        Providers, maps:get(eff_providers, EffRelationsOfParents, [])
%%    ),
%%    NewEffHandleServices = ordsets:union(
%%        HandleServices, maps:get(eff_h_services, EffRelationsOfParents, [])
%%    ),
%%    NewEffHandles = ordsets:union(
%%        Handles, maps:get(eff_handles, EffRelationsOfParents, [])
%%    ),
%%    % Update the record TODO MOZE TYLKO WTEDY JAK SIE ZMIENIL?
%%    {ok, _} = od_group:update(GroupId, fun(Group) ->
%%        {ok, Group#od_group{
%%            eff_parents = NewEffParents,
%%            eff_spaces = NewEffSpaces,
%%            eff_shares = NewEffShares,
%%            eff_providers = NewEffProviders,
%%            eff_handle_services = NewEffHandleServices,
%%            eff_handles = NewEffHandles,
%%            top_down_dirty = false
%%        }}
%%    end),
%%    % TODO     mark_top_down_clean(IdTD),
%%    % Check if related entities should be marked dirty
%%    DidRelationsChange = OldEffParents =/= NewEffParents
%%        orelse OldEffSpaces =/= NewEffSpaces
%%        orelse OldEffShares =/= NewEffShares
%%        orelse OldEffProviders =/= NewEffProviders
%%        orelse OldEffHandleServices =/= NewEffHandleServices
%%        orelse OldEffHandles =/= NewEffHandles,
%%    case DidRelationsChange of
%%        false ->
%%            ok;
%%        true ->
%%            % Mark child groups as top-down dirty.
%%            lists:foreach(
%%                fun(ParentId) ->
%%                    {ok, _} = od_group:update(ParentId, fun(Group) ->
%%                        #od_group{children = Groups} = Group,
%%                        mark_top_down_dirty(?PRIORITY_GROUP_TD(length(Groups)),
%%                            od_group, ParentId),
%%                        {ok, Group#od_group{top_down_dirty = true}}
%%                    end)
%%                end, Parents)
%%    end.



aggregate_eff_neighbours([{_, _} | _] = ProplistA, [{_, _} | _] = ProplistB) ->
    privileges_proplists_union(ProplistA ++ ProplistB);
aggregate_eff_neighbours(PrivilegesA, PrivilegesB) ->
    privileges_union(PrivilegesA, PrivilegesB).



privileges_union(PrivilegesA, PrivilegesB) ->
    ordsets:union(
        ordsets:from_list(PrivilegesA),
        ordsets:from_list(PrivilegesB)
    ).


privileges_proplists_union(PrivilegesProplist) ->
    PrivilegesMap = lists:foldl(
        fun({Id, Privs}, AccMap) ->
            NewPrivs = case maps:get(Id, AccMap, undefined) of
                undefined ->
                    ordsets:from_list(Privs);
                OtherPrivs ->
                    privileges_union(Privs, OtherPrivs)
            end,
            maps:put(Id, NewPrivs, AccMap)
        end, #{}, PrivilegesProplist),
    ordsets:from_list(maps:to_list(PrivilegesMap)).


override_privileges(PrivilegesProplist, NewPrivileges) ->
    lists:map(
        fun({Id, _}) ->
            {Id, NewPrivileges}
        end, PrivilegesProplist).


readable(Id, #od_user{name = Name}) ->
    readable(Name, <<"usr">>, Id);
readable(Id, #od_group{name = Name}) ->
    readable(Name, <<"grp">>, Id);
readable(Id, #od_space{name = Name}) ->
    readable(Name, <<"spc">>, Id);
readable(Id, #od_share{name = Name}) ->
    readable(Name, <<"shr">>, Id);
readable(Id, #od_provider{client_name = Name}) ->
    readable(Name, <<"prv">>, Id);
readable(Id, #od_handle_service{name = Name}) ->
    readable(Name, <<"hsr">>, Id);
readable(Id, #od_handle{resource_id = ResId}) ->
    readable(ResId, <<"hnd">>, Id).


readable(Name, Type, Id) ->
    str_utils:format_bin("~s-~s#~s", [Name, Type, binary:part(Id, {0, 4})]).