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

-define(STATE_KEY, <<"entity_graph_state">>).
-define(ENTITY_GRAPH_LOCK, entity_graph_lock).


% Priorities for records during effective graph recomputation

% For bottom-up: groups (sorted by children num) -> spaces, handles and
%    handle_services -> providers and shares
-define(PRIORITY_GROUP_BU(_NumChildren),
    _NumChildren
).
-define(PRIORITY_SPACE_BU, 1000000000).
-define(PRIORITY_HANDLE_SERVICE_BU, 1000000000).
-define(PRIORITY_HANDLE_BU, 1000000000).
-define(PRIORITY_PROVIDER_BU, 1000000001).
-define(PRIORITY_SHARE_BU, 1000000001).

% For top-down: spaces -> groups (sorted by parents num) -> users
-define(PRIORITY_SPACE_TD, -1000000000).
-define(PRIORITY_GROUP_TD(_NumParents),
    _NumParents
).
-define(PRIORITY_USER_TD, 1000000000).

%% API
-export([init_state/0, get_state/0]).
-export([schedule_refresh/0]).
-export([add_relation/5]).
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
                timer:sleep(1000),
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


add_relation(od_group, ChildGroupId, od_group, ParentGroupId, Privileges) ->
    modify_group_relations(fun() ->
        {ok, _} = od_group:update(ParentGroupId, fun(Group) ->
            #od_group{children = Groups} = Group,
            mark_bottom_up_dirty(
                ?PRIORITY_GROUP_BU(length(Groups) + 1), od_group, ParentGroupId
            ),
            {ok, Group#od_group{
                children = [{ChildGroupId, Privileges} | Groups],
                bottom_up_dirty = true
            }}
        end),
        {ok, _} = od_group:update(ChildGroupId, fun(Group) ->
            #od_group{parents = Groups} = Group,
            mark_top_down_dirty(length(Groups) + 1, od_group, ChildGroupId),
            {ok, Group#od_group{
                parents = [ParentGroupId | Groups],
                top_down_dirty = true
            }}
        end)
    end);


add_relation(od_user, UserId, od_space, SpaceId, Privileges) ->
    {ok, _} = od_space:update(SpaceId, fun(Space) ->
        #od_space{users = Users} = Space,
        {ok, Space#od_space{users = [{UserId, Privileges} | Users]}}
    end),
    {ok, _} = od_user:update(UserId, fun(User) ->
        #od_user{spaces = USpaces} = User,
        {ok, User#od_user{spaces = [SpaceId | USpaces]}}
    end),
    ok.


mark_bottom_up_dirty(Priority, Model, Id) ->
    ?emergency("bottom-up dirty: ~p#~s", [Model, Id]),
    update_state(
        fun(#entity_graph_state{bottom_up_dirty = List} = EffGraphState) ->
            {ok, EffGraphState#entity_graph_state{
                bottom_up_dirty = lists:sort([{Priority, Model, Id} | List])
            }}
        end).


mark_bottom_up_clean(Id) ->
    ?emergency("bottom-up clean: ~s", [Id]),
    update_state(
        fun(#entity_graph_state{bottom_up_dirty = List} = EffGraphState) ->
            {ok, EffGraphState#entity_graph_state{
                bottom_up_dirty = lists:keydelete(Id, 3, List)
            }}
        end).



mark_top_down_dirty(Priority, Model, Id) ->
    ?emergency("top-down dirty: ~p#~s", [Model, Id]),
    update_state(
        fun(#entity_graph_state{top_down_dirty = List} = EffGraphState) ->
            {ok, EffGraphState#entity_graph_state{
                top_down_dirty = lists:sort([{Priority, Model, Id} | List])
            }}
        end).


mark_top_down_clean(Id) ->
    ?emergency("top-down clean: ~s", [Id]),
    update_state(
        fun(#entity_graph_state{top_down_dirty = List} = EffGraphState) ->
            {ok, EffGraphState#entity_graph_state{
                top_down_dirty = lists:keydelete(Id, 3, List)
            }}
        end).


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
            ?emergency("Refreshing (BU) ~p#~s", [ModelBU, IdBU]),
            refresh_entity_bottom_up(ModelBU, IdBU),
            mark_bottom_up_clean(IdBU),
            refresh_entity_graph();
        [] ->
            case TopDown of
                [{_, ModelTD, IdTD} | _] ->
                    ?emergency("Refreshing (TD) ~p#~s", [ModelTD, IdTD]),
                    refresh_entity_top_down(ModelTD, IdTD),
                    mark_top_down_clean(IdTD),
                    refresh_entity_graph();
                [] ->
                    ok
            end
    end.


refresh_entity_bottom_up(od_group, GroupId) ->
    {ok, #document{value = #od_group{
        parents = Parents,
        children = Children,
        users = Users,

        eff_children = OldEffChildren,
        eff_users = OldEffUsers
    }}} = od_group:get(GroupId),
    EffRelationsOfChildren = lists:foldl(
        fun({ChId, PrivsInGroup}, AccMap) ->
            {ok, #document{
                value = #od_group{
                    eff_children = EC,
                    eff_users = EU
                }}} = od_group:get(ChId),
            AccMap#{
                eff_children => ordsets:union(
                    ordsets:from_list(override_privileges(EC, PrivsInGroup)),
                    maps:get(eff_children, AccMap, [])),
                eff_users => ordsets:union(
                    ordsets:from_list(override_privileges(EU, PrivsInGroup)),
                    maps:get(eff_users, AccMap, []))
            }
        end, #{}, Children),
    NewEffChildren = privileges:proplists_union(
        Children ++ maps:get(eff_children, EffRelationsOfChildren, [])
    ),
    NewEffUsers = privileges:proplists_union(
        Users ++ maps:get(eff_users, EffRelationsOfChildren, [])
    ),
    % Update the record TODO MOZE TYLKO WTEDY JAK SIE ZMIENIL?
    {ok, _} = od_group:update(GroupId, fun(Group) ->
        {ok, Group#od_group{
            eff_children = NewEffChildren,
            eff_users = NewEffUsers
        }}
    end),
    % Check if related entities should be marked dirty
    EffChildrenChanged = did_relations_change(OldEffChildren, NewEffChildren),
    EffUsersChanged = did_relations_change(OldEffUsers, NewEffUsers),
    case EffChildrenChanged orelse EffUsersChanged of
        false ->
            ok;
        true ->
            % Mark parent groups as bottom-up dirty.
            lists:foreach(
                fun(ParentId) ->
                    {ok, _} = od_group:update(ParentId, fun(Group) ->
                        #od_group{children = Groups} = Group,
                        mark_bottom_up_dirty(?PRIORITY_GROUP_BU(length(Groups)),
                            od_group, ParentId),
                        {ok, Group#od_group{bottom_up_dirty = true}}
                    end)
                end, Parents)
    end.


refresh_entity_top_down(od_group, GroupId) ->
    {ok, #document{value = #od_group{
        parents = Parents,
        spaces = Spaces,
        handle_services = HandleServices,
        handles = Handles,

        eff_parents = OldEffParents,
        eff_spaces = OldEffSpaces,
        eff_shares = OldEffShares,
        eff_providers = OldEffProviders,
        eff_handle_services = OldEffHandleServices,
        eff_handles = OldEffHandles
    }}} = od_group:get(GroupId),
    EffRelationsOfParents = lists:foldl(
        fun(ParentId, AccMap) ->
            {ok, #document{
                value = #od_group{
                    eff_parents = EPa,
                    eff_spaces = ESp,
                    eff_shares = ESh,
                    eff_providers = EPr,
                    eff_handle_services = EHS,
                    eff_handles = EH
                }}} = od_group:get(ParentId),
            AccMap#{
                eff_parents => ordsets:union(ordsets:from_list(EPa),
                    maps:get(eff_parents, AccMap, [])),
                eff_spaces => ordsets:union(ordsets:from_list(ESp),
                    maps:get(eff_spaces, AccMap, [])),
                eff_shares => ordsets:union(ordsets:from_list(ESh),
                    maps:get(eff_shares, AccMap, [])),
                eff_providers => ordsets:union(ordsets:from_list(EPr),
                    maps:get(eff_providers, AccMap, [])),
                eff_h_services => ordsets:union(ordsets:from_list(EHS),
                    maps:get(eff_h_services, AccMap, [])),
                eff_handles => ordsets:union(ordsets:from_list(EH),
                    maps:get(eff_handles, AccMap, []))
            }
        end, #{}, Parents),
    RelationsOfSpaces = lists:foldl(
        fun(SpaceId, AccMap) ->
            {ok, #document{
                value = #od_space{
                    shares = Sh,
                    providers_supports = PrSup
                }}} = od_space:get(SpaceId),
            {Pr, _} = lists:unzip(PrSup),
            AccMap#{
                shares => ordsets:union(
                    ordsets:from_list(Sh),
                    maps:get(shares, AccMap, [])),
                providers => ordsets:union(
                    ordsets:from_list(Pr),
                    maps:get(providers, AccMap, []))
            }
        end, #{}, Spaces),
    Shares = maps:get(shares, RelationsOfSpaces, []),
    Providers = maps:get(providers, RelationsOfSpaces, []),
    NewEffParents = ordsets:union(
        Parents, maps:get(eff_parents, EffRelationsOfParents, [])
    ),
    NewEffSpaces = ordsets:union(
        Spaces, maps:get(eff_spaces, EffRelationsOfParents, [])
    ),
    NewEffShares = ordsets:union(
        Shares, maps:get(eff_shares, EffRelationsOfParents, [])
    ),
    NewEffProviders = ordsets:union(
        Providers, maps:get(eff_providers, EffRelationsOfParents, [])
    ),
    NewEffHandleServices = ordsets:union(
        HandleServices, maps:get(eff_h_services, EffRelationsOfParents, [])
    ),
    NewEffHandles = ordsets:union(
        Handles, maps:get(eff_handles, EffRelationsOfParents, [])
    ),
    % Update the record TODO MOZE TYLKO WTEDY JAK SIE ZMIENIL?
    {ok, _} = od_group:update(GroupId, fun(Group) ->
        {ok, Group#od_group{
            eff_parents = NewEffParents,
            eff_spaces = NewEffSpaces,
            eff_shares = NewEffShares,
            eff_providers = NewEffProviders,
            eff_handle_services = NewEffHandleServices,
            eff_handles = NewEffHandles,
            top_down_dirty = false
        }}
    end),
    % Check if related entities should be marked dirty
    DidRelationsChange = OldEffParents =/= NewEffParents
        orelse OldEffSpaces =/= NewEffSpaces
        orelse OldEffShares =/= NewEffShares
        orelse OldEffProviders =/= NewEffProviders
        orelse OldEffHandleServices =/= NewEffHandleServices
        orelse OldEffHandles =/= NewEffHandles,
    case DidRelationsChange of
        false ->
            ok;
        true ->
            % Mark child groups as top-down dirty.
            lists:foreach(
                fun(ParentId) ->
                    {ok, _} = od_group:update(ParentId, fun(Group) ->
                        #od_group{children = Groups} = Group,
                        mark_top_down_dirty(?PRIORITY_GROUP_TD(length(Groups)),
                            od_group, ParentId),
                        {ok, Group#od_group{top_down_dirty = true}}
                    end)
                end, Parents)
    end.


did_relations_change(ProplistA, ProplistB) ->
    % to sa ordsets wiec dziala
    proplists:get_keys(ProplistA) =/= proplists:get_keys(ProplistB).


override_privileges(PrivilegesProplist, NewPrivileges) ->
    lists:map(
        fun({Id, _}) ->
            {Id, NewPrivileges}
        end, PrivilegesProplist).