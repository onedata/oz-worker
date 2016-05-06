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
    OrderedGroups = topsort(Groups, []),
    ?emergency("TOPSORT ~p", [{Groups, OrderedGroups}]),
    refresh_effective_groups(lists:reverse(OrderedGroups)),
    refresh_effective_users(Groups, OrderedGroups).

-spec topsort(Groups :: [binary()], AlreadyOrdered :: [binary()]) ->
    OrderedGroups :: [binary()].
topsort(Groups, Ordered) ->
    lists:foldl(fun(ID, AlreadyOrdered) ->
        case lists:member(ID, AlreadyOrdered) of
            true -> AlreadyOrdered;
            false -> case user_group:get(ID) of
                {ok, #document{value = #user_group{child_groups = Children}}} ->
                    OrderedUpdated = topsort(Children, AlreadyOrdered),
                    [ID | OrderedUpdated];
                _Err ->
                    ?warning("Unable to access group ~p due to ~p", [ID, _Err]),
                    AlreadyOrdered
            end
        end
    end, Ordered, Groups).

-spec refresh_effective_users(InitialGroups :: [binary()],
    OrderedGroups :: [binary()]) -> ok.
refresh_effective_users(InitialGroups, Ordered) ->
    InitialEffectiveUsers = lists:foldl(fun(ID, EffectiveUsers) ->
        case user_group:get(ID) of
            {ok, #document{value = #user_group{child_groups = Groups}}} ->
                lists:foldl(fun(ChildID, Acc) ->
                    maps:put(ChildID, get_effective_users(ChildID), Acc)
                end, EffectiveUsers, Groups);
            _Err ->
                ?warning("Unable to access group ~p due to ~p", [ID, _Err]),
                maps:put(ID, [], EffectiveUsers)
        end
    end, #{}, InitialGroups),
    ?emergency("effective_users_traverse0 ~p", [{Ordered, InitialEffectiveUsers}]),
    effective_users_traverse(Ordered, InitialEffectiveUsers).


-spec effective_users_traverse(GroupIDsToVisit :: [binary()],
    #{GroupID :: binary() => effective_users()}) -> ok.
effective_users_traverse([], _) -> ok;
effective_users_traverse(ToVisit, Results) ->
    [ID | Remaining] = ToVisit,
    EffectiveUsers = case user_group:get(ID) of
        {ok, #document{value = #user_group{child_groups = Groups, users = Users}}} ->
            Effective = lists:foldl(fun({ChildID, Privileges}, EUsers) ->
                merge_effective_users(EUsers, maps:get(ChildID, Results), Privileges)
            end, Groups, Groups),
            FinalEffective = Effective ++ Users,
            user_group:update(ID, #{effective_users => FinalEffective}),
            FinalEffective;
        _Err ->
            ?warning_stacktrace("Unable to access group ~p due to ~p", [ID, _Err]),
            []
    end,
    ?emergency("effective_users_traverse ~p", [{Remaining, maps:put(ID, EffectiveUsers, Results)}]),
    effective_users_traverse(Remaining, maps:put(ID, EffectiveUsers, Results)).

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

-spec refresh_effective_groups(OrderedGroupIDsToVisit :: [binary()]) -> ok.
refresh_effective_groups(OrderedGroupIDsToVisit) ->
    effective_groups_traverse(OrderedGroupIDsToVisit, #{}).

-spec effective_groups_traverse(OrderedGroupIDsToVisit :: [binary()],
    #{GroupID :: binary() => effective_groups()}) -> ok.
effective_groups_traverse([], _) -> ok;
effective_groups_traverse(ToVisit, Results) ->
    [ID | Remaining] = ToVisit,
    EffectiveGroups = case user_group:get(ID) of
        {ok, #document{value = #user_group{parent_groups = Groups}}} ->
            Effective = [ID | lists:foldl(fun(ParentID, Acc) ->
                Acc ++ maps:get(ParentID, Results)
            end, Groups, Groups)],
            user_group:update(ID, #{effective_groups => Effective}),
            Effective;
        _Err ->
            ?warning_stacktrace("Unable to access group ~p due to ~p", [ID, _Err]),
            []
    end,
    ?emergency("effective_groups_traverse ~p", [{Remaining, maps:put(ID, EffectiveGroups, Results)}]),
    effective_groups_traverse(Remaining, maps:put(ID, EffectiveGroups, Results)).


-spec get_effective_users(GroupID :: binary()) -> effective_users().
get_effective_users(ID) ->
    case user_group:get(ID) of
        {ok, #document{value = #user_group{effective_users = Users}}} ->
            Users;
        _Err ->
            ?warning_stacktrace("Unable to access group ~p due to ~p", [ID, _Err]),
            []
    end.

-spec ensure_state_present() -> ok.
ensure_state_present() ->
    group_graph_context:create(#document{key = ?KEY,
        value = #group_graph_context{last_rebuild = erlang:system_time()}
    }), ok.
