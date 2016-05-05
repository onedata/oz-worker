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
-include_lib("ctool/include/logging.hrl").

%% API
-export([find_effective_user_privileges/2]).


-spec find_effective_user_privileges(UserID :: binary(), GroupID :: binary()) ->
    ordsets:ordset(privileges:group_privilege()).
find_effective_user_privileges(UserID, GroupID) ->
    maps:get(GroupID, find_effective_user_privileges(UserID, GroupID, #{})).


find_effective_user_privileges(UserID, GroupID, Status) ->
    case user_group:get(GroupID) of
        {ok, #document{value = #user_group{users = Users, groups = Groups}}} ->
            {_, DirectPrivileges} = lists:keyfind(UserID, 1, Users ++ [{UserID, []}]),
            {AllIndirectPrivileges, StatusOfChildren} = lists:mapfoldl(
                fun({ChildGroupID, ChildPrivileges}, CurrStatus) ->
                    ChildStatus = case maps:is_key(ChildGroupID, CurrStatus) of
                        true -> CurrStatus;
                        false -> find_effective_user_privileges(UserID, ChildGroupID, CurrStatus)
                    end,
                    EffectiveInChild = maps:get(ChildGroupID, ChildStatus),
                    EffectiveFromChild = ordsets:intersection(EffectiveInChild, ordsets:from_list(ChildPrivileges)),
                    {EffectiveFromChild, ChildStatus}
                end, Status, Groups),
            Effective = ordsets:union([ordsets:from_list(DirectPrivileges) | AllIndirectPrivileges]),
            maps:put(GroupID, Effective, StatusOfChildren);
        _Error -> Status
    end.