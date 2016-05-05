%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module implementing the business logic for groups of users.
%%% This module serves as a buffer between the database and the REST API.
%%% @end
%%%-------------------------------------------------------------------
-module(group_logic).
-author("Konrad Zemek").

-include("datastore/oz_datastore_models_def.hrl").
-include("datastore/oz_datastore_models_def.hrl").

%% API
-export([exists/1, has_user/2, has_effective_user/2, has_privilege/3, has_effective_privilege/3]).
-export([create/2, modify/2, join/2, set_privileges/3]).
-export([get_data/1, get_users/1, get_effective_users/1, get_spaces/1, get_providers/1, get_user/2, get_privileges/2]).
-export([remove/1, remove_user/2, cleanup/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns whether a group exists.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec exists(GroupId :: binary()) ->
    boolean().
exists(GroupId) ->
    user_group:exists(GroupId).

%%--------------------------------------------------------------------
%% @doc Returns whether the user identified by UserId is a member of the group.
%% Shall return false in any other case (group doesn't exist, etc).
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec has_user(GroupId :: binary(), UserId :: binary()) ->
    boolean().
has_user(GroupId, UserId) ->
    case exists(GroupId) of
        false -> false;
        true ->
            {ok, #document{value = #user_group{users = Users}}} = user_group:get(GroupId),
            lists:keymember(UserId, 1, Users)
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the group's member identified by UserId has privilege
%% in the group. Shall return false in any other case (group doesn't exist,
%% user is not group's member, etc).
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec has_privilege(GroupId :: binary(), UserId :: binary(),
    Privilege :: privileges:group_privilege()) ->
    boolean().
has_privilege(GroupId, UserId, Privilege) ->
    case has_user(GroupId, UserId) of
        false -> false;
        true ->
            {ok, #document{value = #user_group{users = Users}}} = user_group:get(GroupId),
            {_, Privileges} = lists:keyfind(UserId, 1, Users),
            lists:member(Privilege, Privileges)
    end.


%%--------------------------------------------------------------------
%% @doc Returns whether the user identified by UserId is a member of
%% the group (both direct or indirect).
%% Shall return false in any other case (group doesn't exist, etc).
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec has_effective_user(GroupId :: binary(), UserId :: binary()) ->
    boolean().
has_effective_user(GroupId, UserId) ->
    case exists(GroupId) of
        false -> false;
        true ->
            {ok, #document{value = #user_group{effective_users = Users}}}
                = user_group:get(GroupId),
            lists:keymember(UserId, 1, Users)
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the group's member identified by UserId has privilege
%% in the group (direct or through nested groups).
%% Shall return false in any other case (group doesn't exist,
%% user is not group's member, etc).
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec has_effective_privilege(GroupId :: binary(), UserId :: binary(),
    Privilege :: privileges:group_privilege()) ->
    boolean().
has_effective_privilege(GroupId, UserId, Privilege) ->
    case exists(GroupId) of
        false -> false;
        true ->
            {ok, #document{value = #user_group{effective_users = Users}}}
                = user_group:get(GroupId),
            {_, Privileges} = lists:keyfind(UserId, 1, Users),
            lists:member(Privilege, Privileges)
    end.

%%--------------------------------------------------------------------
%% @doc Creates a group for a user.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create(UserId :: binary(), Name :: binary()) ->
    {ok, GroupId :: binary()}.
create(UserId, Name) ->
    {ok, UserDoc} = onedata_user:get(UserId),
    #document{value = #onedata_user{groups = Groups} = User} = UserDoc,

    Privileges = privileges:group_admin(),
    Group = #user_group{name = Name, users = [{UserId, Privileges}]},
    {ok, GroupId} = user_group:save(#document{value = Group}),

    UserNew = User#onedata_user{groups = [GroupId | Groups]},
    onedata_user:save(UserDoc#document{value = UserNew}),

    {ok, GroupId}.

%%--------------------------------------------------------------------
%% @doc Modifies group's data.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec modify(GroupId :: binary(), Name :: binary()) ->
    ok.
modify(GroupId, Name) ->
    {ok, _} = user_group:update(GroupId, fun(Group) ->
        {ok, Group#user_group{name = Name}}
    end),
    ok.

%%--------------------------------------------------------------------
%% @doc Adds user to a group identified by a token.
%% Throws exception when call to the datastore fails, or token/user/group_from_token
%% doesn't exist in db.
%% @end
%%--------------------------------------------------------------------
-spec join(UserId :: binary(), Macaroon :: macaroon:macaroon()) ->
    {ok, GroupId :: binary()}.
join(UserId, Macaroon) ->
    {ok, {group, GroupId}} = token_logic:consume(Macaroon),
    case has_user(GroupId, UserId) of
        true -> ok;
        false ->
            Privileges = privileges:group_user(),
            {ok, _} = user_group:update(GroupId, fun(Group) ->
                #user_group{users = Users} = Group,
                {ok, Group#user_group{users = [{UserId, Privileges} | Users]}}
            end),
            {ok, _} = onedata_user:update(UserId, fun(User) ->
                #onedata_user{groups = Groups} = User,
                {ok, User#onedata_user{groups = [GroupId | Groups]}}
            end)
    end,
    {ok, GroupId}.

%%--------------------------------------------------------------------
%% @doc Sets privileges for a member of the group.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_privileges(GroupId :: binary(), UserId :: binary(),
    Privileges :: [privileges:group_privilege()]) ->
    ok.
set_privileges(GroupId, UserId, Privileges) ->
    {ok, _} = user_group:update(GroupId, fun(Group) ->
        #user_group{users = Users} = Group,
        UsersNew = lists:keyreplace(UserId, 1, Users, {UserId, Privileges}),
        {ok, Group#user_group{users = UsersNew}}
    end),
    ok.

%%--------------------------------------------------------------------
%% @doc Returns details about the group.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_data(GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_data(GroupId) ->
    {ok, #document{value = #user_group{name = Name}}} = user_group:get(GroupId),
    {ok, [
        {groupId, GroupId},
        {name, Name}
    ]}.

%%--------------------------------------------------------------------
%% @doc Returns details about group's members.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_users(GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_users(GroupId) ->
    {ok, #document{value = #user_group{users = UserTuples}}} = user_group:get(GroupId),
    {Users, _} = lists:unzip(UserTuples),
    {ok, [{users, Users}]}.

%%--------------------------------------------------------------------
%% @doc Returns details about group's members (both direct and indirect).
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_users(GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_effective_users(GroupId) ->
    {ok, #document{value = #user_group{effective_users = UserTuples}}}
        = user_group:get(GroupId),
    {Users, _} = lists:unzip(UserTuples),
    {ok, [{users, Users}]}.

%%--------------------------------------------------------------------
%% @doc Returns details about group's spaces.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_spaces(GroupId) ->
    {ok, #document{value = #user_group{spaces = Spaces}}} = user_group:get(GroupId),
    {ok, [{spaces, Spaces}]}.

%%--------------------------------------------------------------------
%% @doc Returns providers of user's spaces.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_providers(GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_providers(GroupId) ->
    {ok, #document{value = #user_group{spaces = Spaces}}} = user_group:get(GroupId),
    GroupProviders = lists:foldl(fun(Space, Providers) ->
        {ok, #document{value = #space{providers_supports = ProvidersSupports}}}
            = space:get(Space),
        {SpaceProviders, _} = lists:unzip(ProvidersSupports),
        ordsets:union(ordsets:from_list(SpaceProviders), Providers)
    end, ordsets:new(), Spaces),
    {ok, [{providers, GroupProviders}]}.

%%--------------------------------------------------------------------
%% @doc Returns details about group's member.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_user(GroupId :: binary(), UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_user(_GroupId, UserId) ->
    user_logic:get_data(UserId, provider).

%%--------------------------------------------------------------------
%% @doc Returns list of group's member privileges.
%% Throws exception when call to the datastore fails, or group/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_privileges(GroupId :: binary(), UserId :: binary()) ->
    {ok, [privileges:group_privilege()]}.
get_privileges(GroupId, UserId) ->
    {ok, #document{value = #user_group{users = UserTuples}}} = user_group:get(GroupId),
    {_, Privileges} = lists:keyfind(UserId, 1, UserTuples),
    {ok, Privileges}.

%%--------------------------------------------------------------------
%% @doc Removes the group.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec remove(GroupId :: binary()) ->
    true.
remove(GroupId) ->
    {ok, #document{value = #user_group{users = Users, spaces = Spaces}}} = user_group:get(GroupId),
    lists:foreach(fun({UserId, _}) ->
        {ok, _} = onedata_user:update(UserId, fun(User) ->
            #onedata_user{groups = Groups} = User,
            {ok, User#onedata_user{groups = lists:delete(GroupId, Groups)}}
        end)
    end, Users),
    lists:foreach(fun(SpaceId) ->
        {ok, _} = space:update(SpaceId, fun(Space) ->
            #space{groups = SGroups} = Space,
            {ok, Space#space{groups = lists:keydelete(GroupId, 1, SGroups)}}
        end)
    end, Spaces),
    user_group:delete(GroupId),
    true.

%%--------------------------------------------------------------------
%% @doc Removes user from the group.
%% Throws exception when call to the datastore fails, or group/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(GroupId :: binary(), UserId :: binary()) ->
    true.
remove_user(GroupId, UserId) ->
    {ok, _} = user_group:update(GroupId, fun(Group) ->
        #user_group{users = Users} = Group,
        {ok, Group#user_group{users = lists:keydelete(UserId, 1, Users)}}
    end),
    {ok, _} = onedata_user:update(UserId, fun(User) ->
        #onedata_user{groups = Groups} = User,
        {ok, User#onedata_user{groups = lists:delete(GroupId, Groups)}}
    end),
    cleanup(GroupId),
    true.

%%--------------------------------------------------------------------
%% @doc Removes the group if empty.
%% Throws exception when call to the datastore fails, or group is already removed.
%% @end
%%--------------------------------------------------------------------
-spec cleanup(GroupId :: binary()) -> boolean().
cleanup(GroupId) ->
    {ok, #document{value = Group}} = user_group:get(GroupId),
    case Group of
        #user_group{users = []} -> remove(GroupId);
        _ -> false
    end.
