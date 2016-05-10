%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(group_graph).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-define(KEY, <<"group_graph_context">>).
-define(LOCK_ID, <<"group_graph">>).

%% API
-export([mark_group_changed/1, refresh_effective_caches/0]).


-type effective_users() :: [{UserID :: binary(), [privileges:group_privilege()]}].
-type effective_groups() :: [GroupID :: binary()].
-export_type([effective_users/0, effective_groups/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec mark_group_changed(GroupID :: binary()) -> ok.
mark_group_changed(GroupID) ->
    ensure_state_present(),
    {ok, _} = group_graph_context:update(?KEY, fun(Context) ->
        NewGroups = [GroupID | Context#group_graph_context.changed_groups],
        {ok, Context#group_graph_context{changed_groups = NewGroups}}
    end), ok.

-spec refresh_effective_caches() -> ok | not_applicable.
refresh_effective_caches() ->
    ensure_state_present(),
    Interval = application:get_env(?APP_Name, group_graph_refresh_interval, 500),
    Now = erlang:system_time(),

    datastore:run_synchronized(group_graph_context, ?LOCK_ID, fun() ->
        {ok, #document{value = #group_graph_context{last_rebuild = Timestamp,
            changed_groups = Groups}}} = group_graph_context:get(?KEY),

        case Timestamp + Interval < Now of
            false -> not_applicable;
            true ->
                {ok, _} = group_graph_context:update(?KEY, fun(Val) ->
                    {ok, Val#group_graph_context{
                        last_rebuild = Now,
                        changed_groups = Val#group_graph_context.changed_groups -- Groups
                    }} end),
                effective_groups_update_traverse(Groups),
                effective_users_update_traverse(Groups)
        end
    end).

%%%===================================================================
%%% Internal
%%%===================================================================

-spec effective_groups_update_traverse(GroupIDs :: [binary()]) -> ok.
effective_groups_update_traverse(GroupIDs) ->
    ToVisit = topsort(GroupIDs, [], fun children/1),
    traverse(ToVisit, fun update_effective_groups_visitor/2, #{}),
    update_effective_groups_in_users(ToVisit).

-spec effective_users_update_traverse(GroupIDs :: [binary()]) -> ok.
effective_users_update_traverse(GroupIDs) ->
    ToVisit = topsort(GroupIDs, [], fun parents/1),
    traverse(ToVisit, fun update_effective_users_visitor/2, #{}).

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

update_effective_users_visitor(GroupDoc, Context) ->
    #document{key = ID, value = #user_group{child_groups = ChildGroups, users = Users}} = GroupDoc,
    EffectiveFromChildren = lists:foldl(fun({ChildID, ChildPrivileges}, All) ->
        ChildEffective = case maps:get(ChildID, Context, undef) of
            undef -> get_effective_users(ChildID);
            _ChildEffective -> _ChildEffective
        end,
        merge_effective_users(All, ChildEffective, ChildPrivileges)
    end, [], ChildGroups),
    Effective = merge_effective_users(EffectiveFromChildren, Users, privileges:group_privileges()),
    update_effective_users(ID, Effective),
    maps:put(ID, Effective, Context).


-spec merge_effective_users(CurrentPrivileges :: effective_users(), PrivilegesToAdd :: effective_users(),
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

children(#user_group{child_groups = Tuples}) ->
    {Groups, _} = lists:unzip(Tuples),
    Groups.
parents(#user_group{parent_groups = Groups}) ->
    Groups.

-spec ensure_state_present() -> ok.
ensure_state_present() ->
    group_graph_context:create(#document{key = ?KEY,
        value = #group_graph_context{last_rebuild = erlang:system_time()}
    }), ok.

%%%===================================================================
%%% Internal: DAG functions
%%%===================================================================

traverse([], _, _) -> ok;
traverse(ToVisit, Visitor, Context) ->
    [ID | NextIDs] = ToVisit,
    NewContext = case user_group:get(ID) of
        {ok, Doc} ->
            Visitor(Doc, Context);
        _Err ->
            ?warning_stacktrace("Unable to access group ~p due to ~p", [ID, _Err]),
            Context
    end,
    traverse(NextIDs, Visitor, NewContext).


-spec topsort(Groups :: [binary()], AlreadyOrdered :: [binary()],
    GetNext :: fun((#user_group{}) -> GroupIDs :: [binary()])) ->
    OrderedGroups :: [binary()].
topsort(Groups, Ordered, GetNext) ->
    lists:foldl(fun(ID, AlreadyOrdered) ->
        case lists:member(ID, AlreadyOrdered) of
            true -> AlreadyOrdered;
            false -> case user_group:get(ID) of
                {ok, #document{value = Val}} ->
                    Children = GetNext(Val),
                    OrderedUpdated = topsort(Children, AlreadyOrdered, GetNext),
                    [ID | OrderedUpdated];
                _Err ->
                    ?warning("Unable to access group ~p due to ~p", [ID, _Err]),
                    AlreadyOrdered
            end
        end
    end, Ordered, Groups).

%%%===================================================================
%%% Internal: Conditional updates
%%%===================================================================

update_effective_groups(ID, Effective) ->
    user_group:update(ID, fun(Group) ->
        Current = ordsets:from_list(Group#user_group.effective_groups),
        Removed = ordsets:subtract(Current, Effective),
        Added = ordsets:subtract(Effective, Current),
        case {Added, Removed} of
            {[], []} -> {error, update_not_needed};
            _ -> {ok, Group#user_group{effective_groups = Effective}}
        end
    end).

update_effective_users(ID, Effective) ->
    user_group:update(ID, fun(Group) ->
        Current = ordsets:from_list(Group#user_group.effective_users),
        Removed = ordsets:subtract(Current, Effective),
        Added = ordsets:subtract(Effective, Current),
        case {Added, Removed} of
            {[], []} -> {error, update_not_needed};
            _ -> {ok, Group#user_group{effective_users = Effective}}
        end
    end).

update_effective_groups_in_users(GroupIDs) ->
    {AffectedUsers, EffectiveGroupsMap} = gather_user_effective_groups_context(GroupIDs),
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

gather_user_effective_groups_context(GIDs) ->
    AffectedUsers = gather_affected_users(GIDs),
    RequiredGroupsIDs = gather_users_groups(AffectedUsers),
    EffectiveGroupsMap = gather_effective_groups(RequiredGroupsIDs),
    {AffectedUsers, EffectiveGroupsMap}.

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