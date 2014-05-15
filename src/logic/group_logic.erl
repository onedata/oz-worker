%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module implementing the business logic for groups of users.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(group_logic).
-author("Konrad Zemek").


%% API
-export([exists/1, has_user/2, has_privilege/3]).
-export([create/2, modify/2, set_privileges/3]).
-export([get_data/1, get_users/1, get_spaces/1, get_user/2, get_space/2,
    get_privileges/2]).
-export([remove/1, remove_user/2, remove_space/2]).


%% exists/1
%% ====================================================================
%% @doc Returns whether a group exists.
%% ====================================================================
-spec exists(GroupId :: binary()) -> boolean().
%% ====================================================================
exists(GroupId) ->
    true.


%% has_user/2
%% ====================================================================
%% @doc Returns whether the user identified by UserId is a member of the group.
%% Shall return false in any other case (group doesn't exist, etc).
%% @end
%% ====================================================================
-spec has_user(GroupId :: binary(), UserId :: binary()) -> boolean().
%% ====================================================================
has_user(GroupId, UserId) ->
    true.


%% has_privilege/3
%% ====================================================================
%% @doc Returns whether the group's member identified by UserId has privilege
%% in the group. Shall return false in any other case (group doesn't exist,
%% user is not group's member, etc).
%% @end
%% ====================================================================
-spec has_privilege(GroupId :: binary(), UserId :: binary(),
                    Privilege :: privileges:privilege()) -> boolean().
%% ====================================================================
has_privilege(GroupId, UserId, Privilege) ->
    true.


%% create/2
%% ====================================================================
%% @doc Creates a group for a user.
%% ====================================================================
-spec create(UserId :: binary(), Name :: binary()) ->
    {ok, GroupId :: binary()} | {error, Reason :: any()}.
%% ====================================================================
create(UserId, Name) ->
    {ok, <<"new_group_id">>}.


%% modify/2
%% ====================================================================
%% @doc Modifies group's data.
%% ====================================================================
-spec modify(GroupId :: binary(), Name :: binary()) ->
    ok | {error, Reason :: any()}.
%% ====================================================================
modify(GroupId, Name) ->
    ok.


%% set_privileges/3
%% ====================================================================
%% @doc Sets privileges for a member of the group.
%% ====================================================================
-spec set_privileges(GroupId :: binary(), UserId :: binary(),
                     Privileges :: [privileges:privilege()]) ->
    ok | {error, Reason :: any()}.
%% ====================================================================
set_privileges(GroupId, UserId, Privileges) ->
    ok.


%% get_data/1
%% ====================================================================
%% @doc Returns details about the group.
%% ====================================================================
-spec get_data(GroupId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_data(GroupId) ->
    {ok, [{name, <<"groupname">>}]}.


%% get_users/1
%% ====================================================================
%% @doc Returns details about group's members.
%% ====================================================================
-spec get_users(GroupId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_users(GroupId) ->
    {ok, [{users, [<<"user">>]}]}.


%% get_spaces/1
%% ====================================================================
%% @doc Returns details about group's spaces.
%% ====================================================================
-spec get_spaces(GroupId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_spaces(GroupId) ->
    {ok, [{spaces, [<<"space">>]}]}.


%% get_user/2
%% ====================================================================
%% @doc Returns details about group's member.
%% ====================================================================
-spec get_user(GroupId :: binary(), UserId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_user(GroupId, UserId) ->
    {ok, [{name, [<<"name">>]}]}.


%% get_space/2
%% ====================================================================
%% @doc Returns details about group's space.
%% ====================================================================
-spec get_space(GroupId :: binary(), SpaceId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_space(GroupId, SpaceId) ->
    {ok, [{name, [<<"name">>]}]}.


%% get_privileges/2
%% ====================================================================
%% @doc Returns list of group's member privileges.
%% ====================================================================
-spec get_privileges(GroupId :: binary(), UserId) ->
    {ok, [privileges:group_privilege()]} | {error, Reason :: any()}.
%% ====================================================================
get_privileges(GroupId, UserId) ->
    sets:to_list(privileges:group_admin()).


%% remove/1
%% ====================================================================
%% @doc Removes the group. Should return true if after the call the group
%% doesn't exist; in particular if it never existed at all.
%% @end
%% ====================================================================
-spec remove(GroupId :: binary()) -> boolean().
%% ====================================================================
remove(GroupId) ->
    true.


%% remove_user/2
%% ====================================================================
%% @doc Removes user from the group. Should return true if after the call the
%% user will no longer be a member of the group; in particular if he never
%% was a member or the group didn't exist.
%% @end
%% ====================================================================
-spec remove_user(GroupId :: binary(), UserId :: binary()) -> boolean().
%% ====================================================================
remove_user(GroupId, UserId) ->
    true.


%% remove_space/2
%% ====================================================================
%% @doc Removes Space from the group. Should return true if after the call the
%% group will no longer be a member of the Space; in particular if it never
%% was a member or the group didn't exist.
%% @end
%% ====================================================================
-spec remove_space(GroupId :: binary(), SpaceId :: binary()) -> boolean().
%% ====================================================================
remove_space(GroupId, SpaceId) ->
    true.
