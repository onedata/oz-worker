%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is responsible for refreshment of effective users and groups
%%% in both user and group documents.
%%% It gathers 'group changed' reports and is periodically invoked to update
%%% documents related to the changed groups.
%%% The 'refresh' is to be executed only by one process at the time (and
%%% the lock protects that property).
%%% @end
%%%-------------------------------------------------------------------
-module(group_graph).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-define(KEY, <<"groups_graph_caches_state">>).
-define(LOCK_ID, <<"group_graph">>).

%% API
-export([mark_group_changed/1, mark_user_changed/1, refresh_effective_caches/0]).

-type effective_users() :: [{UserID :: binary(), [privileges:group_privilege()]}].
-type effective_groups() :: [GroupID :: binary()].
-export_type([effective_users/0, effective_groups/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Marks groups as changed. The group becomes a target for effective
%% users & groups refresh.
%% @end
%%--------------------------------------------------------------------
-spec mark_group_changed(GroupID :: binary()) -> ok.
mark_group_changed(GroupID) ->
    ensure_state_initialised(),
    {ok, _} = groups_graph_caches_state:update(?KEY, fun(Context) ->
        NewGroups = [GroupID | Context#groups_graph_caches_state.changed_groups],
        {ok, Context#groups_graph_caches_state{changed_groups = NewGroups}}
    end), ok.

%%--------------------------------------------------------------------
%% @doc Marks user as changed. The user will have it's effective groups updated.
%% @end
%%--------------------------------------------------------------------
-spec mark_user_changed(UserID :: binary()) -> ok.
mark_user_changed(UserID) ->
    ensure_state_initialised(),
    {ok, _} = groups_graph_caches_state:update(?KEY, fun(Context) ->
        NewUsers = [UserID | Context#groups_graph_caches_state.changed_users],
        {ok, Context#groups_graph_caches_state{changed_users = NewUsers}}
    end), ok.

%%--------------------------------------------------------------------
%% @doc Updates effective groups in users and effective users & groups in groups.
%% The update is not applicable if interval between updates would be too low.
%% Any encountered cycles are broken.
%% @end
%%--------------------------------------------------------------------
-spec refresh_effective_caches() -> ok | not_applicable.
refresh_effective_caches() ->
    ensure_state_initialised(),
    {ok, Interval} = application:get_env(?APP_Name, group_graph_refresh_interval),
    Now = erlang:system_time(),

    datastore:run_synchronized(groups_graph_caches_state, ?LOCK_ID, fun() ->
        {ok, #document{value = #groups_graph_caches_state{
            last_rebuild = Timestamp,
            changed_groups = Groups,
            changed_users = Users
        }}} = groups_graph_caches_state:get(?KEY),

        case Timestamp + Interval < Now of
            false -> not_applicable;
            true ->
                {ok, _} = groups_graph_caches_state:update(?KEY, fun(Val) ->
                    {ok, Val#groups_graph_caches_state{
                        last_rebuild = Now,
                        changed_groups = Val#groups_graph_caches_state.changed_groups -- Groups
                    }} end),
                break_cycles(Groups, []),
                effective_groups_update_traverse(Users, Groups),
                effective_users_update_traverse(Groups)
        end
    end).

%%%===================================================================
%%% Internal
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @private performs group graph traverse & update effective groups
%% (both in groups and users)
%% @end
%%--------------------------------------------------------------------
-spec effective_groups_update_traverse(UserIDs :: [binary()], GroupIDs :: [binary()]) -> ok.
effective_groups_update_traverse(Users, GroupIDs) ->
    ToVisit = topsort(GroupIDs, fun children/1),
    traverse(ToVisit, fun update_effective_groups_visitor/2, #{}),
    update_effective_groups_in_users(Users, ToVisit).

%%--------------------------------------------------------------------
%% @doc @private performs group graph traverse & update effective users
%% @end
%%--------------------------------------------------------------------
-spec effective_users_update_traverse(GroupIDs :: [binary()]) -> ok.
effective_users_update_traverse(GroupIDs) ->
    ToVisit = topsort(GroupIDs, fun parents/1),
    traverse(ToVisit, fun update_effective_users_visitor/2, #{}).

%%--------------------------------------------------------------------
%% @doc @private performs actual effective groups update
%% (based on provided context)
%% @end
%%--------------------------------------------------------------------
-spec update_effective_groups_visitor(GroupDoc :: datastore:document(),
    InContext :: #{GID :: binary() => effective_users()}) ->
    OutContext :: #{GID :: binary() => effective_users()}.
update_effective_groups_visitor(GroupDoc, Context) ->
    #document{key = ID, value = #user_group{parent_groups = ParentGroups}} = GroupDoc,
    EffectiveOfParents = lists:foldl(fun(ParentID, All) ->
        case maps:get(ParentID, Context, undef) of
            undef -> All ++ get_effective_groups(ParentID);
            ParentEffective -> All ++ ParentEffective
        end
    end, [], ParentGroups),
    Effective = lists:usort([ID | EffectiveOfParents]),
    update_effective_groups(ID, Effective),
    maps:put(ID, Effective, Context).

%%--------------------------------------------------------------------
%% @doc @private performs actual effective users update
%% (based on provided context)
%% @end
%%--------------------------------------------------------------------
-spec update_effective_users_visitor(GroupDoc :: datastore:document(),
    InContext :: #{GID :: binary() => effective_users()}) ->
    OutContext :: #{GID :: binary() => effective_users()}.
update_effective_users_visitor(GroupDoc, Context) ->
    #document{key = ID, value = #user_group{nested_groups = ChildGroups,
        users = Users}} = GroupDoc,
    EffectiveFromChildren = lists:foldl(fun({ChildID, ChildPrivileges}, All) ->
        ChildEffective = case maps:get(ChildID, Context, undef) of
            undef -> get_effective_users(ChildID);
            _ChildEffective -> _ChildEffective
        end,
        merge_effective_users(All, ChildEffective, ChildPrivileges)
    end, [], ChildGroups),
    Effective = merge_effective_users(EffectiveFromChildren, Users,
        privileges:group_privileges()),
    update_effective_users(ID, Effective),
    maps:put(ID, Effective, Context).

%%--------------------------------------------------------------------
%% @doc @private merges users with privileges accounting given privileges mask
%% @end
%%--------------------------------------------------------------------
-spec merge_effective_users(CurrentPrivileges :: effective_users(),
    PrivilegesToAdd :: effective_users(),
    PrivilegesMask :: [privileges:group_privilege()]) ->
    MergedPrivileges :: effective_users().
merge_effective_users(CurrentPrivileges, PrivilegesToAdd, PrivilegesMask) ->
    Mask = ordsets:from_list(PrivilegesMask),
    lists:foldl(fun({UserID, UPrivileges}, UpdatedPrivileges) ->
        UserPrivileges = ordsets:from_list(UPrivileges),
        UserPrivilegesToAdd = ordsets:intersection(UserPrivileges, Mask),
        CurrentUserPrivileges = proplists:get_value(UserID, UpdatedPrivileges, []),
        MergedPrivileges = ordsets:from_list(CurrentUserPrivileges ++ UserPrivilegesToAdd),
        [{UserID, MergedPrivileges} | proplists:delete(UserID, UpdatedPrivileges)]
    end, CurrentPrivileges, PrivilegesToAdd).


-spec get_effective_users(GroupID :: binary()) -> effective_users().
get_effective_users(ID) ->
    case user_group:get(ID) of
        {ok, #document{value = #user_group{effective_users = Users}}} ->
            Users;
        _Err ->
            ?warning_stacktrace("Unable to access group ~p due to ~p", [ID, _Err]),
            []
    end.

-spec get_effective_groups(GroupID :: binary()) -> effective_groups().
get_effective_groups(ID) ->
    case user_group:get(ID) of
        {ok, #document{value = #user_group{effective_groups = Groups}}} ->
            Groups;
        _Err ->
            ?warning_stacktrace("Unable to access group ~p due to ~p", [ID, _Err]),
            []
    end.

-spec children(#user_group{}) -> [binary()].
children(#user_group{nested_groups = Tuples}) ->
    {Groups, _} = lists:unzip(Tuples),
    Groups.

-spec parents(#user_group{}) -> [binary()].
parents(#user_group{parent_groups = Groups}) ->
    Groups.

%%--------------------------------------------------------------------
%% @doc @private ensures that shared state is properly initialised
%% @end
%%--------------------------------------------------------------------
-spec ensure_state_initialised() -> ok.
ensure_state_initialised() ->
    Result = groups_graph_caches_state:create(#document{key = ?KEY,
        value = #groups_graph_caches_state{last_rebuild = erlang:system_time()}}),

    case Result of
        {ok, _} ->
            {ok, AllGroups} = user_group:all(),
            lists:foreach(fun(#document{key = GID}) ->
                mark_group_changed(GID)
            end, AllGroups);
        _ -> ok
    end.

%%%===================================================================
%%% Internal: DAG functions
%%%===================================================================
-spec traverse(GroupIdsToVisit :: [binary()],
    Visitor :: fun((GroupDoc :: datastore:document(), InContext) -> OutContext),
    InitialContext) -> ok
    when
    InContext :: #{GID :: binary() => effective_users()},
    OutContext :: #{GID :: binary() => effective_users()},
    InitialContext :: #{GroupID :: binary() => term()}.
traverse([], _, _) -> ok;
traverse([ID | NextIDsToVisit], Visitor, Context) ->
    NewContext = case user_group:get(ID) of
        {ok, Doc} ->
            Visitor(Doc, Context);
        _Err ->
            ?warning_stacktrace("Unable to access group ~p due to ~p", [ID, _Err]),
            Context
    end,
    traverse(NextIDsToVisit, Visitor, NewContext).


%%--------------------------------------------------------------------
%% @doc @private traverses group graph and returns ordered ids
%% @end
%%--------------------------------------------------------------------
-spec topsort(StartingIDs :: [binary()], fun((#user_group{}) -> NextIDs :: [binary()])) ->
    OrderedGroups :: [binary()].
topsort(StartingIDs, GetNext) ->
    topsort(StartingIDs, GetNext, [], []).

-spec topsort(Groups :: [binary()], fun((#user_group{}) -> NextIDs :: [binary()]),
    AlreadyOrdered :: [binary()], Parents :: [binary()]) ->
    OrderedGroups :: [binary()].
topsort(Groups, GetNext, Ordered, Parents) ->
    lists:foldl(fun(ID, AlreadyOrdered) ->
        case lists:member(ID, Parents) of
            true ->
                ?error("Cycle introduced during update - update aborted"),
                throw(cycle_introduced_during_update);
            false -> ok
        end,
        case lists:member(ID, AlreadyOrdered) of
            true -> AlreadyOrdered;
            false -> case user_group:get(ID) of
                {ok, #document{value = Val}} ->
                    Children = GetNext(Val),
                    OrderedUpdated = topsort(Children, GetNext, AlreadyOrdered, [ID | Parents]),
                    [ID | OrderedUpdated];
                _Err ->
                    ?warning("Unable to access group ~p due to ~p", [ID, _Err]),
                    AlreadyOrdered
            end
        end
    end, Ordered, Groups).

%%--------------------------------------------------------------------
%% @doc @private Traverses group graph and breaks cycles.
%% Cycles are broken at first encountered cycle edge. Smarter policy
%% cannot be implemented as we are unaware of updates order (and *total*
%% would be required to implement truly good strategy)
%% Yet the cycles shouldn't occur often as adding edge checks for cycles
%% (but as the check operates on cache this procedure is needed).
%% @end
%%--------------------------------------------------------------------
-spec break_cycles(Groups :: [binary()], Parents :: [binary()]) -> ok.
break_cycles(Groups, Ancestors) ->
    lists:foreach(fun(ID) ->
        case lists:member(ID, Ancestors) of
            true ->
                Parent = hd(Ancestors),
                ?warning("Cycle detected - breaking relation: ~p - ~p", [ID, Parent]),
                group_logic:remove_nested_group(Parent, ID);
            false -> case user_group:get(ID) of
                {ok, #document{value = #user_group{nested_groups = GroupsTuples}}} ->
                    {GroupIDs, _} = lists:unzip(GroupsTuples),
                    break_cycles(GroupIDs, [ID | Ancestors]);
                _Err ->
                    ?warning("Unable to access group ~p due to ~p", [ID, _Err])
            end
        end
    end, Groups).

%%%===================================================================
%%% Internal: Conditional updates
%%%===================================================================

-spec update_effective_groups(GroupID :: binary(), effective_groups()) -> ok.
update_effective_groups(ID, Effective) ->
    user_group:update(ID, fun(Group) ->
        Current = ordsets:from_list(Group#user_group.effective_groups),
        Removed = ordsets:subtract(Current, Effective),
        Added = ordsets:subtract(Effective, Current),
        case {Added, Removed} of
            {[], []} -> {error, update_not_needed};
            _ -> {ok, Group#user_group{effective_groups = Effective}}
        end
    end), ok.

-spec update_effective_users(GroupID :: binary(), effective_users()) -> ok.
update_effective_users(ID, Effective) ->
    user_group:update(ID, fun(Group) ->
        EffectiveOrdset = ordsets:from_list(Effective),
        Current = ordsets:from_list(Group#user_group.effective_users),
        Removed = ordsets:subtract(Current, EffectiveOrdset),
        Added = ordsets:subtract(EffectiveOrdset, Current),
        case {Added, Removed} of
            {[], []} -> {error, update_not_needed};
            _ -> {ok, Group#user_group{effective_users = Effective}}
        end
    end), ok.

-spec update_effective_groups_in_users(UserIDs :: [binary()],
    ChangedGroupIDs :: [binary()]) -> ok.
update_effective_groups_in_users(UserIDs, ChangedGroupIDs) ->
    {AffectedUsers, EffectiveGroupsMap} =
        gather_user_effective_groups_context(ChangedGroupIDs, UserIDs),

    lists:foreach(fun(UID) ->
        onedata_user:update(UID, fun(User) ->
            #onedata_user{groups = GIDs, effective_groups = EGIDs} = User,

            NewEGIDs = lists:usort(lists:foldl(fun(GID, Acc) ->
                Acc ++ maps:get(GID, EffectiveGroupsMap, [])
            end, [], GIDs)),

            case lists:usort(EGIDs) of
                NewEGIDs -> {error, update_not_needed};
                _ -> {ok, User#onedata_user{effective_groups = NewEGIDs}}
            end
        end)
    end, AffectedUsers),
    ok.

-spec gather_user_effective_groups_context(GIDs :: [binary()], UIDs :: [binary()]) ->
    {AffectedUsers :: [binary()], EffectiveGroupsMap :: #{
    GID :: binary() => effective_groups()}}.
gather_user_effective_groups_context(GIDs, UIDs) ->
    AffectedUsers = gather_affected_users(GIDs) ++ UIDs,
    RequiredGroupsIDs = gather_users_groups(AffectedUsers),
    EffectiveGroupsMap = gather_effective_groups(RequiredGroupsIDs),
    {AffectedUsers, EffectiveGroupsMap}.

-spec gather_affected_users(GIDs :: [binary()]) -> AffectedUsers :: [binary()].
gather_affected_users(GIDs) ->
    lists:foldl(fun(GID, UsersList) ->
        case user_group:get(GID) of
            {ok, #document{value = #user_group{users = Users}}} ->
                {UIDs, _} = lists:unzip(Users),
                UsersList ++ UIDs;
            _Err ->
                ?warning_stacktrace("Unable to access group ~p due to ~p", [GID, _Err]),
                UsersList
        end
    end, [], GIDs).

-spec gather_users_groups(UIDs :: [binary()]) -> GIDs :: [binary()].
gather_users_groups(Users) ->
    lists:foldl(fun(UID, Acc) ->
        case onedata_user:get(UID) of
            {ok, #document{value = #onedata_user{groups = Groups}}} ->
                Acc ++ Groups;
            _Err ->
                ?warning_stacktrace("Unable to access user ~p due to ~p", [UID, _Err]),
                Acc
        end
    end, [], Users).

-spec gather_effective_groups(GIDs :: [binary()]) ->
    EffectiveGroupsMap :: #{GID :: binary() => effective_groups()}.
gather_effective_groups(GIDs) ->
    lists:foldl(fun(GID, GroupsMap) ->
        case maps:get(GID, GroupsMap, undefined) of
            undefined ->
                case user_group:get(GID) of
                    {ok, #document{value = #user_group{effective_groups = Effective}}} ->
                        maps:put(GID, Effective, GroupsMap);
                    _Err ->
                        ?warning_stacktrace("Unable to access group ~p due to ~p", [GID, _Err]),
                        maps:put(GID, [], GroupsMap)
                end;
            _ -> GroupsMap
        end
    end, #{}, GIDs).