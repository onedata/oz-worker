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
-include("entity_logic_errors.hrl").

-define(ENTITY_GRAPH_LOCK, entity_graph).
-define(STATE_KEY, <<"entity_graph_state">>).

%% API
-export([init_state/0, get_state/0]).
-export([schedule_refresh/0, ensure_up_to_date/0]).
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


ensure_up_to_date() ->
    ensure_up_to_date(5).
ensure_up_to_date(0) ->
    error(cannot_ensure_up_to_date);
ensure_up_to_date(Retries) ->
    Res = critical_section:run(?ENTITY_GRAPH_LOCK, fun() ->
        case get_state() of
            #entity_graph_state{bottom_up_dirty = [], top_down_dirty = []} ->
                true;
            _ ->
                false
        end
    end),
    case Res of
        true ->
            ok;
        false ->
            timer:sleep(1000),
            ensure_up_to_date(Retries - 1)
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
        case get_state() of
            #entity_graph_state{bottom_up_dirty = [], top_down_dirty = []} ->
                ?emergency("Refresh skipped."),
                ok;
            _ ->
                try
                    ?emergency("Refresh needed, performing..."),
                    refresh_entity_graph(),
                    ?emergency("Refresh done")
                catch T:M ->
                    ?error_stacktrace("BLEH: ~p ~p", [T, M]),
                    % TODO try catch, jak blad to spimy z 2 sekundy i schedule
                    timer:sleep(1500)
                end
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
            critical_section:run({ModelBU, IdBU}, fun() ->
                refresh_entity(bottom_up, ModelBU, IdBU)
            end),
            refresh_entity_graph();
        [] ->
            case TopDown of
                [{_, ModelTD, IdTD} | _] ->
                    critical_section:run({ModelTD, IdTD}, fun() ->
                        refresh_entity(top_down, ModelTD, IdTD)
                    end),
                    refresh_entity_graph();
                [] ->
                    ok
            end
    end.


% TODO przeladowanie synchronous
% TODO trycatch jakis?
refresh_entity(WhichWay, ModelType, EntityId) ->
    OtherWay = case WhichWay of
        % TODO makra
        top_down -> bottom_up;
        bottom_up -> top_down
    end,
    {ok, #document{value = Entity}} = ModelType:get(EntityId),
    ?emergency("Refreshing (~p) ~s", [WhichWay, readable(EntityId, Entity)]),
    % Get neighbours of the entity in given direction
    NeighboursAsEff = neighbours_to_eff_relations(WhichWay, ModelType, EntityId, Entity),
%%    ?emergency("NeighboursAsEff: ~p", [NeighboursAsEff]),
    EffOfNeighbours = eff_relations_of_neighbours(WhichWay, EntityId, Entity),
%%    ?emergency("EffOfNeighbours: ~p", [EffOfNeighbours]),

    AggregatedEffRelations = lists:foldl(
        fun(EffOfNeighbour, OuterMapAcc) ->
            lists:foldl(
                fun({NType, EffRelationsMap}, InnerMapAcc) ->
                    InnerMapAcc#{NType => merge_eff_relations(
                        WhichWay, ModelType, NType,
                        maps:get(NType, InnerMapAcc, #{}), EffRelationsMap
                    )}
                end, OuterMapAcc, maps:to_list(EffOfNeighbour))
        end, NeighboursAsEff, EffOfNeighbours),
%%    ?emergency("AggregatedEffRelations: ~p", [AggregatedEffRelations]),
    % Get significant effective relations from all neighbours
    % Check if neighbours should be marked dirty (depending on direction)
    case AggregatedEffRelations =:= eff_relations(WhichWay, Entity) of
        true ->
            % Nothing changed, no action is needed
            ?emergency("Nothing changed"),
            ok;
        false ->
            % Mark all neighbours as dirty
            % (these that are in the direction of update)
            Successors = neighbours_to_ids(OtherWay, Entity),
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
    add_relation(od_group, ChildId, Privs, od_group, ParentId, undefined);
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
    Result = critical_section:run({ParentModel, ParentId}, fun() ->
        case ParentModel:update(ParentId, ParentUpdateFun) of
            {ok, _} ->
                critical_section:run({ChildModel, ChildId}, fun() ->
                    case ChildModel:update(ChildId, ChildUpdateFun) of
                        {ok, _} ->
                            ok;
                        ?EL_RELATION_EXISTS ->
                            % Relation exists, but apparently it did not exist
                            % in the parent, so we just fixed the relation -> ok.
                            ok;
                        Err1 ->
                            % Some other error, we have to attempt reverting the
                            % relation in parent.
                            ParentModel:update(ParentId, ParentRevertFun),
                            Err1
                    end
                end);
            Err2 ->
                Err2
        end
    end),
    schedule_refresh(),
    Result.


% Shares do not take part in eff graph recomputation
mark_dirty(_, _, _, _, #od_share{} = Entity) ->
    Entity;
mark_dirty(WhichWay, Flag, Model, Id, Entity) ->
    % TODO jak juz jest dirty to ne dodawac!
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
            {ok, NewState}
        end),
    set_dirty_flag(WhichWay, Flag, Entity).


% Priorities for records during effective graph recomputation

% For bottom-up:
%   1) groups (sorted by children num)
%   2) spaces, handles and handle_services
%   3) providers
get_priority(bottom_up, #od_group{children = Children}) ->
    maps:size(Children) + 1;
get_priority(bottom_up, #od_space{}) ->
    1000000000;
get_priority(bottom_up, #od_handle_service{}) ->
    1000000000;
get_priority(bottom_up, #od_handle{}) ->
    1000000000;
get_priority(bottom_up, #od_provider{}) ->
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
    maps:is_key(UserId, Users);
has_child(#od_space{groups = Groups}, od_group, GroupId) ->
    maps:is_key(GroupId, Groups);

has_child(#od_handle_service{users = Users}, od_user, UserId) ->
    maps:is_key(UserId, Users);
has_child(#od_handle_service{groups = Groups}, od_group, GroupId) ->
    maps:is_key(GroupId, Groups);

has_child(#od_handle{users = Users}, od_user, UserId) ->
    maps:is_key(UserId, Users);
has_child(#od_handle{groups = Groups}, od_group, GroupId) ->
    maps:is_key(GroupId, Groups);

has_child(#od_group{users = Users}, od_user, UserId) ->
    maps:is_key(UserId, Users);
has_child(#od_group{children = Children}, od_group, GroupId) ->
    maps:is_key(GroupId, Children).


add_child(#od_provider{spaces = Spaces} = Provider, od_space, SpaceId, _) ->
    Provider#od_provider{spaces = [SpaceId | Spaces]};

add_child(#od_share{} = Share, od_space, SpaceId, _) ->
    Share#od_share{space = SpaceId};

add_child(#od_space{users = Users} = Space, od_user, UserId, Privs) ->
    Space#od_space{users = maps:put(UserId, Privs, Users)};
add_child(#od_space{groups = Groups} = Space, od_group, GroupId, Privs) ->
    Space#od_space{groups = maps:put(GroupId, Privs, Groups)};

add_child(#od_handle_service{users = Users} = HS, od_user, UserId, Privs) ->
    HS#od_handle_service{users = maps:put(UserId, Privs, Users)};
add_child(#od_handle_service{groups = Groups} = HS, od_group, GroupId, Privs) ->
    HS#od_handle_service{groups = maps:put(GroupId, Privs, Groups)};

add_child(#od_handle{users = Users} = Handle, od_user, UserId, Privs) ->
    Handle#od_handle{users = maps:put(UserId, Privs, Users)};
add_child(#od_handle{groups = Groups} = Handle, od_group, GroupId, Privs) ->
    Handle#od_handle{groups = maps:put(GroupId, Privs, Groups)};

add_child(#od_group{users = Users} = Group, od_user, UserId, Privs) ->
    Group#od_group{users = maps:put(UserId, Privs, Users)};
add_child(#od_group{children = Children} = Group, od_group, GroupId, Privs) ->
    Group#od_group{children = maps:put(GroupId, Privs, Children)}.


remove_child(#od_provider{spaces = Spaces} = Provider, od_space, SpaceId) ->
    Provider#od_provider{spaces = lists:delete(SpaceId, Spaces)};

remove_child(#od_share{} = Share, od_space, _SpaceId) ->
    Share#od_share{space = undefined};

remove_child(#od_space{users = Users} = Space, od_user, UserId) ->
    Space#od_space{users = maps:remove(UserId, Users)};
remove_child(#od_space{groups = Groups} = Space, od_group, GroupId) ->
    Space#od_space{groups = maps:remove(GroupId, Groups)};

remove_child(#od_handle_service{users = Users} = HS, od_user, UserId) ->
    HS#od_handle_service{users = maps:remove(UserId, Users)};
remove_child(#od_handle_service{groups = Groups} = HS, od_group, GroupId) ->
    HS#od_handle_service{groups = maps:remove(GroupId, Groups)};

remove_child(#od_handle{users = Users} = Handle, od_user, UserId) ->
    Handle#od_handle{users = maps:remove(UserId, Users)};
remove_child(#od_handle{groups = Groups} = Handle, od_group, GroupId) ->
    Handle#od_handle{groups = maps:remove(GroupId, Groups)};

remove_child(#od_group{users = Users} = Group, od_user, UserId) ->
    Group#od_group{users = maps:remove(UserId, Users)};
remove_child(#od_group{children = Children} = Group, od_group, GroupId) ->
    Group#od_group{children = maps:remove(GroupId, Children)}.


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
has_parent(#od_space{providers = Providers}, od_provider, ProviderId) ->
    maps:is_key(ProviderId, Providers).


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
add_parent(#od_space{providers = Providers} = Space, od_provider, ProviderId, SupportSize) ->
    Space#od_space{providers = maps:put(ProviderId, SupportSize, Providers)}.


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
remove_parent(#od_space{providers = Providers} = Space, od_provider, ProviderId) ->
    Space#od_space{providers = maps:remove(ProviderId, Providers)}.


neighbours(bottom_up, #od_group{} = Group) ->
    #od_group{users = Users, children = Groups} = Group,
    #{od_user => Users, od_group => Groups};
neighbours(bottom_up, #od_space{} = Space) ->
    #od_space{users = Users, groups = Groups} = Space,
    #{od_user => Users, od_group => Groups};
neighbours(bottom_up, #od_provider{} = Provider) ->
    #od_provider{spaces = Spaces} = Provider,
    #{od_space => Spaces};
neighbours(bottom_up, #od_handle_service{} = HService) ->
    #od_handle_service{users = Users, groups = Groups} = HService,
    #{od_user => Users, od_group => Groups};
neighbours(bottom_up, #od_handle{} = Handle) ->
    #od_handle{users = Users, groups = Groups} = Handle,
    #{od_user => Users, od_group => Groups};
neighbours(top_down, #od_user{} = User) ->
    #od_user{
        groups = Groups, spaces = Spaces,
        handle_services = HServices, handles = Handles,
        oz_privileges = OzPrivileges
    } = User,
    #{
        od_group => Groups, od_space => Spaces,
        od_handle_service => HServices, od_handle => Handles,
        oz_privileges => OzPrivileges
    };
neighbours(top_down, #od_group{} = Group) ->
    #od_group{
        parents = Groups, spaces = Spaces,
        handle_services = HServices, handles = Handles,
        oz_privileges = OzPrivileges
    } = Group,
    #{
        od_group => Groups, od_space => Spaces,
        od_handle_service => HServices, od_handle => Handles,
        oz_privileges => OzPrivileges
    };
neighbours(top_down, #od_space{} = Space) ->
    #od_space{providers = Providers} = Space,
    #{od_provider => Providers};
% All other relations should return an empty map.
neighbours(_, _) ->
    #{}.


neighbours_to_ids(WhichWay, Entity) ->
    Neighbours = neighbours(WhichWay, Entity),
    maps:map(
        fun(oz_privileges, _Relations) ->
            [];
            (_ModelType, Relations) ->
                case Relations of
                    List when is_list(List) ->
                        List;
                    Map when is_map(Map) ->
                        maps:keys(Map)
                end
        end, Neighbours).


neighbours_to_eff_relations(bottom_up, EntityType, EntityId, #od_group{} = Entity) ->
    neighbours_to_eff_relations(bottom_up, EntityType, EntityId, Entity, [od_user, od_group]);
neighbours_to_eff_relations(bottom_up, EntityType, EntityId, #od_space{} = Entity) ->
    neighbours_to_eff_relations(bottom_up, EntityType, EntityId, Entity, [od_user, od_group]);
neighbours_to_eff_relations(bottom_up, EntityType, EntityId, #od_provider{} = Entity) ->
    neighbours_to_eff_relations(bottom_up, EntityType, EntityId, Entity, [od_space]);
neighbours_to_eff_relations(bottom_up, EntityType, EntityId, #od_handle_service{} = Entity) ->
    neighbours_to_eff_relations(bottom_up, EntityType, EntityId, Entity, [od_user, od_group]);
neighbours_to_eff_relations(bottom_up, EntityType, EntityId, #od_handle{} = Entity) ->
    neighbours_to_eff_relations(bottom_up, EntityType, EntityId, Entity, [od_user, od_group]);

neighbours_to_eff_relations(top_down, EntityType, EntityId, #od_user{} = Entity) ->
    neighbours_to_eff_relations(top_down, EntityType, EntityId, Entity, [
        od_group, od_space, od_handle_service, od_handle, oz_privileges]);
neighbours_to_eff_relations(top_down, EntityType, EntityId, #od_group{} = Entity) ->
    neighbours_to_eff_relations(top_down, EntityType, EntityId, Entity, [
        od_group, od_space, od_handle_service, od_handle, oz_privileges]);
neighbours_to_eff_relations(_, _, _, _) ->
    #{}.


neighbours_to_eff_relations(WhichWay, EntityType, EntityId, Entity, ModelsToAccount) ->
    Neighbours = neighbours(WhichWay, Entity),
    RelevantNeighbours = maps:filter(
        fun(ModelType, _) ->
            lists:member(ModelType, ModelsToAccount)
        end, Neighbours),
    maps:map(
        fun(ModelType, Relations) ->
            relation_to_eff(EntityType, EntityId, ModelType, Relations)
        end, RelevantNeighbours).


eff_relations_of_neighbours(bottom_up, EntityId, #od_group{} = Entity) ->
    eff_relations_of_neighbours(bottom_up, od_group, EntityId, Entity, [od_group]);
eff_relations_of_neighbours(bottom_up, EntityId, #od_space{} = Entity) ->
    eff_relations_of_neighbours(bottom_up, od_space, EntityId, Entity, [od_group]);
eff_relations_of_neighbours(bottom_up, EntityId, #od_provider{} = Entity) ->
    eff_relations_of_neighbours(bottom_up, od_provider, EntityId, Entity, [od_space]);
eff_relations_of_neighbours(bottom_up, EntityId, #od_handle_service{} = Entity) ->
    eff_relations_of_neighbours(bottom_up, od_handle_service, EntityId, Entity, [od_group]);
eff_relations_of_neighbours(bottom_up, EntityId, #od_handle{} = Entity) ->
    eff_relations_of_neighbours(bottom_up, od_handle, EntityId, Entity, [od_group]);

eff_relations_of_neighbours(top_down, EntityId, #od_user{} = Entity) ->
    eff_relations_of_neighbours(top_down, od_user, EntityId, Entity, [od_group, od_space]);
eff_relations_of_neighbours(top_down, EntityId, #od_group{} = Entity) ->
    eff_relations_of_neighbours(top_down, od_group, EntityId, Entity, [od_group, od_space]);
% All other relations should return an empty list.
eff_relations_of_neighbours(_, _, _) ->
    [].


eff_relations_of_neighbours(WhichWay, EntityType, EntityId, Entity, ModelsToAccount) ->
    AllNeighbours = neighbours(WhichWay, Entity),
    RelevantNeighbours = maps:filter(
        fun(ModelType, _) ->
            lists:member(ModelType, ModelsToAccount)
        end, AllNeighbours),
    ?emergency("RelevantNeighbours: ~p", [RelevantNeighbours]),
    lists:flatmap(
        fun({NeighbourType, NeighboursOfType}) ->
            % Convert all neighbour relations so they have attrs
            NeighboursWithAttrs = case NeighboursOfType of
                List when is_list(List) ->
                    [{X, undefined} || X <- List];
                Map when is_map(Map) ->
                    maps:to_list(Map)
            end,
            ?emergency("NeighboursWithAttrs: ~p", [NeighboursWithAttrs]),
            lists:map(
                fun({NeighbourId, Attrs}) ->
                    {ok, #document{value = Neighbour}} = NeighbourType:get(NeighbourId),
                    eff_relations_of_neighbour(WhichWay, EntityType, EntityId, NeighbourType, NeighbourId, Neighbour, Attrs)
                end, NeighboursWithAttrs)
        end, maps:to_list(RelevantNeighbours)).


eff_relations_of_neighbour(WhichWay, EntityType, EntityId, NeighbourType, NeighbourId, Neighbour, Privs) ->
    eff_relations_of_neighbour(WhichWay, EntityType, NeighbourId, Neighbour, Privs).
%%    maps:map(
%%        fun(_ModelType, EffRelations) ->
%%            override_intermediaries(EffRelations, [{NeighbourType, NeighbourId}])
%%        end, Map).

% TODO PO CO TO JEST, SKORO NA NIC NIE WPLYWA? TYLKO WCHICH WAY I ENTITY COS ZMIENIA LOL!
eff_relations_of_neighbour(bottom_up, od_group, _NeighbourId, #od_group{} = Group, Privs) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{
        od_user => override_eff_privileges(EffUsers, Privs),
        od_group => override_eff_privileges(EffGroups, Privs)
    };
eff_relations_of_neighbour(bottom_up, od_space, _NeighbourId, #od_group{} = Group, Privs) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{
        od_user => override_eff_privileges(EffUsers, Privs),
        od_group => override_eff_privileges(EffGroups, Privs)
    };
eff_relations_of_neighbour(bottom_up, od_provider, _NeighbourId, #od_space{} = Space, _) ->
    #od_space{eff_users = EffUsers, eff_groups = EffGroups} = Space,
    #{
        od_user => remove_eff_privileges(EffUsers),
        od_group => remove_eff_privileges(EffGroups)
    };
eff_relations_of_neighbour(bottom_up, od_handle_service, _NeighbourId, #od_group{} = Group, Privs) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{
        od_user => override_eff_privileges(EffUsers, Privs),
        od_group => override_eff_privileges(EffGroups, Privs)
    };
eff_relations_of_neighbour(bottom_up, od_handle, _NeighbourId, #od_group{} = Group, Privs) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{
        od_user => override_eff_privileges(EffUsers, Privs),
        od_group => override_eff_privileges(EffGroups, Privs)
    };

eff_relations_of_neighbour(top_down, od_user, _NeighbourId, #od_group{} = Group, _) ->
    #od_group{
        eff_parents = EffGroups,
        eff_spaces = EffSpaces,
        eff_providers = EffProviders,
        eff_handle_services = EffHServices,
        eff_handles = EffHandles,
        eff_oz_privileges = EffOzPrivileges
    } = Group,
    #{
        od_group => EffGroups, od_space => EffSpaces,
        od_provider => EffProviders,
        od_handle_service => EffHServices, od_handle => EffHandles,
        oz_privileges => EffOzPrivileges
    };
eff_relations_of_neighbour(top_down, od_user, NeighbourId, #od_space{} = Space, _) ->
    #od_space{providers = Providers} = Space,
    #{od_provider => relation_to_eff(od_space, NeighbourId, od_provider, maps:keys(Providers))};
eff_relations_of_neighbour(top_down, od_group, _NeighbourId, #od_group{} = Group, _) ->
    #od_group{
        eff_parents = EffGroups,
        eff_spaces = EffSpaces,
        eff_providers = EffProviders,
        eff_handle_services = EffHServices,
        eff_handles = EffHandles,
        eff_oz_privileges = EffOzPrivileges
    } = Group,
    #{
        od_group => EffGroups, od_space => EffSpaces,
        od_provider => EffProviders,
        od_handle_service => EffHServices, od_handle => EffHandles,
        oz_privileges => EffOzPrivileges
    };
eff_relations_of_neighbour(top_down, od_group, NeighbourId, #od_space{} = Space, _) ->
    #od_space{providers = Providers} = Space,
    #{od_provider => relation_to_eff(od_space, NeighbourId, od_provider, maps:keys(Providers))}.
% All other relations should return an empty map. TODO CZY ABY NA PEWNO? NIE WIDAC BUGOW
%%eff_relations_of_neighbour(_, _, _, _) ->
%%    #{}.


eff_relations(bottom_up, #od_group{} = Group) ->
    #od_group{eff_users = EffUsers, eff_children = EffGroups} = Group,
    #{od_user => EffUsers, od_group => EffGroups};
eff_relations(bottom_up, #od_space{} = Space) ->
    #od_space{eff_users = EffUsers, eff_groups = EffGroups} = Space,
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
eff_relations(top_down, #od_group{} = Group) ->
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
eff_relations(top_down, #od_space{}) ->
    #{}.


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
        eff_handles = maps:get(od_handle, EffNeighbours, #{})
    };
update_eff_relations(top_down, #od_group{} = Group, EffNeighbours) ->
    Group#od_group{
        eff_parents = maps:get(od_group, EffNeighbours, #{}),
        eff_spaces = maps:get(od_space, EffNeighbours, #{}),
        eff_providers = maps:get(od_provider, EffNeighbours, #{}),
        eff_handle_services = maps:get(od_handle_service, EffNeighbours, #{}),
        eff_handles = maps:get(od_handle, EffNeighbours, #{})
    };
update_eff_relations(top_down, #od_space{} = Space, _) ->
    Space.


merge_eff_relations(WhichWay, EntityType, NeighbourType, Eff1, Eff2) when is_map(Eff1) andalso is_map(Eff2) ->
    lists:foldl(
        fun({EntityId, EffRelation2}, MapAcc) ->
            NewValue = case maps:get(EntityId, Eff1, undefined) of
                undefined ->
                    EffRelation2;
                EffRelation1 ->
                    merge_eff_relations(WhichWay, EntityType, NeighbourType, EffRelation1, EffRelation2)
            end,
            MapAcc#{EntityId => NewValue}
        end, Eff1, maps:to_list(Eff2));

merge_eff_relations(bottom_up, od_group, od_user, {Privs1, Intermediaries1}, {Privs2, Intermediaries2}) ->
    {ordsets_union(Privs1, Privs2), ordsets_union(Intermediaries1, Intermediaries2)};

merge_eff_relations(bottom_up, od_group, od_group, {Privs1, Intermediaries1}, {Privs2, Intermediaries2}) ->
    {ordsets_union(Privs1, Privs2), ordsets_union(Intermediaries1, Intermediaries2)};

merge_eff_relations(bottom_up, od_space, od_user, {Privs1, Intermediaries1}, {Privs2, Intermediaries2}) ->
    {ordsets_union(Privs1, Privs2), ordsets_union(Intermediaries1, Intermediaries2)};

merge_eff_relations(bottom_up, od_space, od_group, {Privs1, Intermediaries1}, {Privs2, Intermediaries2}) ->
    {ordsets_union(Privs1, Privs2), ordsets_union(Intermediaries1, Intermediaries2)};

merge_eff_relations(bottom_up, od_provider, od_user, Intermediaries1, Intermediaries2) ->
    ordsets_union(Intermediaries1, Intermediaries2);

merge_eff_relations(bottom_up, od_provider, od_group, Intermediaries1, Intermediaries2) ->
    ordsets_union(Intermediaries1, Intermediaries2);

merge_eff_relations(bottom_up, od_handle_service, od_user, {Privs1, Intermediaries1}, {Privs2, Intermediaries2}) ->
    {ordsets_union(Privs1, Privs2), ordsets_union(Intermediaries1, Intermediaries2)};

merge_eff_relations(bottom_up, od_handle_service, od_group, {Privs1, Intermediaries1}, {Privs2, Intermediaries2}) ->
    {ordsets_union(Privs1, Privs2), ordsets_union(Intermediaries1, Intermediaries2)};

merge_eff_relations(bottom_up, od_handle, od_user, {Privs1, Intermediaries1}, {Privs2, Intermediaries2}) ->
    {ordsets_union(Privs1, Privs2), ordsets_union(Intermediaries1, Intermediaries2)};

merge_eff_relations(bottom_up, od_handle, od_group, {Privs1, Intermediaries1}, {Privs2, Intermediaries2}) ->
    {ordsets_union(Privs1, Privs2), ordsets_union(Intermediaries1, Intermediaries2)};

merge_eff_relations(top_down, _, oz_privileges, Privs1, Privs2) ->
    ordsets_union(Privs1, Privs2);

merge_eff_relations(top_down, _, _, Intermediaries1, Intermediaries2) ->
    ordsets_union(Intermediaries1, Intermediaries2).

relation_to_eff(_, _, oz_privileges, List) when is_list(List) ->
    List;
relation_to_eff(EntityType, EntityId, ModelType, List) when is_list(List) ->
    lists:foldl(
        fun(NeighbourId, AccMap) ->
            AccMap#{NeighbourId => [{EntityType, EntityId}]}
        end, #{}, List);
relation_to_eff(EntityType, EntityId, ModelType, Map) when is_map(Map) ->
    maps:map(
        fun(_NeighbourId, Attributes) ->
            {Attributes, [{EntityType, EntityId}]}
        end, Map).



ordsets_union(ListA, ListB) ->
    ordsets:union(
        ordsets:from_list(ListA),
        ordsets:from_list(ListB)
    ).


override_eff_privileges(PrivilegesMap, NewPrivileges) ->
    maps:map(
        fun(_Key, {_OldPrivileges, Intermediaries}) ->
            {NewPrivileges, Intermediaries}
        end, PrivilegesMap).


remove_eff_privileges(PrivilegesMap) ->
    maps:map(
        fun(_Key, {_OldPrivileges, Intermediaries}) ->
            Intermediaries
        end, PrivilegesMap).



override_intermediaries(EffRelationsMap, NewIntermediaries) ->
    maps:map(
        fun(_Key, Value) ->
            case Value of
                {Privileges, _Intermediaries} ->
                    {Privileges, NewIntermediaries};
                _Intermediaries ->
                    NewIntermediaries
            end
        end, EffRelationsMap).


readable(Id, #od_user{name = Name}) ->
    readable(Name, <<"usr">>, Id);
readable(Id, #od_group{name = Name}) ->
    readable(Name, <<"grp">>, Id);
readable(Id, #od_space{name = Name}) ->
    readable(Name, <<"spc">>, Id);
readable(Id, #od_provider{name = Name}) ->
    readable(Name, <<"prv">>, Id);
readable(Id, #od_handle_service{name = Name}) ->
    readable(Name, <<"hsr">>, Id);
readable(Id, #od_handle{resource_id = ResId}) ->
    readable(ResId, <<"hnd">>, Id).


readable(Name, Type, Id) ->
    str_utils:format_bin("~s-~s#~s", [Name, Type, binary:part(Id, {0, 8})]).
