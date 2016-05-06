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
-export([set_poi/1, refresh/0]).


-type effective_users() :: [{UserID :: binary(), [privileges:group_privilege()]}].
-type effective_groups() :: [GroupID :: binary()].
-export_type([effective_users/0, effective_groups/0]).

-spec set_poi(GroupID :: binary()) -> ok.
set_poi(GroupID) ->
    ensure_state_present(),
    {ok, _} = group_graph_context:update(?KEY, fun(Context) ->
        NewGroups = [GroupID | Context#group_graph_context.changed_groups],
        {ok, Context#group_graph_context{changed_groups = NewGroups}}
    end), ok.

-spec refresh() -> ok | not_applicable.
refresh() ->
    ensure_state_present(),
    Interval = application:get_env(?APP_Name, group_graph_refresh_interval, 500),
    Now = erlang:system_time(),

    datastore:run_synchronized(group_graph_context, ?LOCK_ID, fun() ->
        {ok, #document{value = #group_graph_context{last_rebuild = Timestamp,
            changed_groups = Groups}} = Doc} = group_graph_context:get(?KEY),

        ?emergency("INIT ~p", [{Interval, Now, Timestamp + Interval < Now, Doc}]),

        case Timestamp + Interval < Now of
            false -> not_applicable;
            true ->
                {ok, _} = group_graph_context:update(?KEY, fun(Val) ->
                    {ok, Val#group_graph_context{
                        last_rebuild = Now,
                        changed_groups = Val#group_graph_context.changed_groups -- Groups
                    }} end),
                perform_refresh(Groups)
        end
    end).

-spec perform_refresh(GroupIDs :: [binary()]) -> ok.
perform_refresh(Groups) ->
    ChildGroups = topsort(Groups, [], fun children/1),
    ParentGroups = topsort(Groups, [], fun parents/1),
    ?emergency("TOPSORT ~p", [{Groups, ChildGroups, ParentGroups}]),
    refresh_effective_groups(Groups, ParentGroups),
    refresh_effective_users(Groups, ChildGroups).

-spec refresh_effective_users(InitialGroups :: [binary()],
    OrderedGroups :: [binary()]) -> ok.
refresh_effective_users(InitialGroups, Ordered) ->
    InitialContext = get_initial_context(InitialGroups -- Ordered,
        fun children/1, fun get_effective_users/1),
    effective_users_traverse(Ordered, InitialContext).

-spec refresh_effective_groups(InitialGroups :: [binary()],
    GroupIDsToVisit :: [binary()]) -> ok.
refresh_effective_groups(InitialGroups, Ordered) ->
    InitialContext = get_initial_context(InitialGroups -- Ordered,
        fun parents/1, fun get_effective_groups/1),
    effective_groups_traverse(Ordered, InitialContext).


-spec effective_groups_traverse(OrderedGroupIDsToVisit :: [binary()],
    #{GroupID :: binary() => effective_groups()}) -> ok.
effective_groups_traverse(ToVisit, Results) ->
    GetAttrUpdate = fun(DepsWithValues, #document{key = ID}) ->
        {_, EffectiveGroups} = lists:unzip(DepsWithValues),
        Final = lists:usort([ID | lists:append(EffectiveGroups)]),
        #{effective_groups => Final}
    end,
    traverse(ToVisit, Results, fun parents/1, GetAttrUpdate).


-spec effective_users_traverse(GroupIDsToVisit :: [binary()],
    #{GroupID :: binary() => effective_users()}) -> ok.
effective_users_traverse(ToVisit, Results) ->
    GetAttrUpdate = fun(DepsWithValues, #document{value =
    #user_group{users = Users, child_groups = Groups}}) ->
        Merged = lists:foldl(fun({ChildID, ChildEffective}, CurrEffective) ->
            Privileges = proplists:get_value(ChildID, Groups, []),
            merge_effective_users(CurrEffective, ChildEffective, Privileges)
        end, [], DepsWithValues),
        Final = merge_effective_users(Users, Merged, privileges:group_privileges()),
        #{effective_users => Final}
    end,
    traverse(ToVisit, Results, fun children/1, GetAttrUpdate).


-spec merge_effective_users(CurrentUsers :: effective_users(), ChildUsers :: effective_users(),
    ChildPrivileges :: [privileges:group_privilege()]) ->
    FinalPrivileges :: effective_users().
merge_effective_users(CurrentUsers, ChildUsers, ChildPrivilegesSrc) ->
    ChildPrivileges = ordsets:from_list(ChildPrivilegesSrc),
    lists:foldl(fun({UserID, Privileges}, Curr) ->
        UserPrivileges = ordsets:from_list(Privileges),
        New = ordsets:intersection(ChildPrivileges, UserPrivileges),
        {_, Old} = proplists:get_value(UserID, Curr, []),
        Final = ordsets:union(ordsets:from_list(Old), New),
        lists:keyreplace(UserID, 1, CurrentUsers, {UserID, Final})
    end, CurrentUsers, ChildUsers).


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


get_initial_context(InitialGroups, GetDeps, GetVal) ->
    ?emergency("Building initial context for ~p ", [InitialGroups]),
    lists:foldl(fun(ID, Context) ->
        case user_group:get(ID) of
            {ok, #document{value = GroupRecord}} ->
                Groups = GetDeps(GroupRecord),
                ?emergency("Got deps ~p ", [Groups]),
                Tmp = lists:foldl(fun(GroupID, UpdatedContext) ->
                    Value = GetVal(GroupID),
                    maps:put(GroupID, Value, UpdatedContext)
                end, Context, Groups),
                ?emergency("Updated to ~p ", [Tmp]),
                Tmp;
            _Err ->
                ?warning("Unable to access group ~p due to ~p", [ID, _Err]),
                maps:put(ID, [], Context)
        end
    end, #{}, InitialGroups).


traverse([], _, _, _) -> ok;
traverse(ToVisit, Context, GetDeps, GetAttrUpdate) ->
    [ID | Remaining] = ToVisit,
    ?emergency("Visiting ~p while traversing with context ~p; deps resovler: ~p; attr resolver ~p", [ID, Context, GetDeps, GetAttrUpdate]),
    ValueToPut = case user_group:get(ID) of
        {ok, Doc = #document{value = Record}} ->
            Deps = GetDeps(Record),
            ?emergency("Got deps ~p", [Deps]),
            DepsWithValues = lists:map(fun(GroupID) ->
                {GroupID, maps:get(GroupID, Context, [])}
            end, Deps),
            ?emergency("retrieved values ~p", [DepsWithValues]),

            Diff = GetAttrUpdate(DepsWithValues, Doc),
            ?emergency("generated diff ~p", [Diff]),
            user_group:update(ID, Diff),
            hd(maps:values(Diff));
        _Err ->
            ?warning_stacktrace("Unable to access group ~p due to ~p", [ID, _Err]),
            []
    end,
    traverse(Remaining, maps:put(ID, ValueToPut, Context), GetDeps, GetAttrUpdate).


-spec topsort(Groups :: [binary()], AlreadyOrdered :: [binary()],
    fun((#user_group{}) -> GroupIDs :: [binary()])) ->
    OrderedGroups :: [binary()].
topsort(Groups, Ordered, NextFun) ->
    lists:foldl(fun(ID, AlreadyOrdered) ->
        case lists:member(ID, AlreadyOrdered) of
            true -> AlreadyOrdered;
            false -> case user_group:get(ID) of
                {ok, #document{value = Val}} ->
                    Children = NextFun(Val),
                    OrderedUpdated = topsort(Children, AlreadyOrdered, NextFun),
                    [ID | OrderedUpdated];
                _Err ->
                    ?warning("Unable to access group ~p due to ~p", [ID, _Err]),
                    AlreadyOrdered
            end
        end
    end, Ordered, Groups).