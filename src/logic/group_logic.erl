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

-include("dao/dao_types.hrl").


%% API
-export([exists/1, has_user/2, has_privilege/3]).
-export([create/2, modify/2, join/2, set_privileges/3]).
-export([get_data/1, get_users/1, get_spaces/1, get_user/2, get_privileges/2]).
-export([remove/1, remove_user/2, cleanup/1]).


%% exists/1
%% ====================================================================
%% @doc Returns whether a group exists.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec exists(GroupId :: binary()) -> boolean() | no_return().
%% ====================================================================
exists(GroupId) ->
    dao_adapter:group_exists(GroupId).


%% has_user/2
%% ====================================================================
%% @doc Returns whether the user identified by UserId is a member of the group.
%% Shall return false in any other case (group doesn't exist, etc).
%% @end
%% ====================================================================
-spec has_user(GroupId :: binary(), UserId :: binary()) -> boolean().
%% ====================================================================
has_user(GroupId, UserId) ->
    case exists(GroupId) of
        false -> false;
        true ->
            #user_group{users = Users} = dao_adapter:group(GroupId),
            lists:keymember(UserId, 1, Users)
    end.


%% has_privilege/3
%% ====================================================================
%% @doc Returns whether the group's member identified by UserId has privilege
%% in the group. Shall return false in any other case (group doesn't exist,
%% user is not group's member, etc).
%% @end
%% ====================================================================
-spec has_privilege(GroupId :: binary(), UserId :: binary(),
                    Privilege :: privileges:group_privilege()) -> boolean().
%% ====================================================================
has_privilege(GroupId, UserId, Privilege) ->
    case has_user(GroupId, UserId) of
        false -> false;
        true ->
            #user_group{users = Users} = dao_adapter:group(GroupId),
            {_, Privileges} = lists:keyfind(UserId, 1, Users),
            lists:member(Privilege, Privileges)
    end.


%% create/2
%% ====================================================================
%% @doc Creates a group for a user.
%% ====================================================================
-spec create(UserId :: binary(), Name :: binary()) ->
    {ok, GroupId :: binary()} | no_return().
%% ====================================================================
create(UserId, Name) ->
    UserDoc = dao_adapter:user_doc(UserId),
    #veil_document{record = #user{groups = Groups} = User} = UserDoc,

    Privileges = privileges:group_admin(),
    Group = #user_group{name = Name, users = [{UserId, Privileges}]},
    GroupId = dao_adapter:save(Group),

    UserNew = User#user{groups = [GroupId | Groups]},
    dao_adapter:save(UserDoc#veil_document{record = UserNew}),

    {ok, GroupId}.


%% modify/2
%% ====================================================================
%% @doc Modifies group's data.
%% ====================================================================
-spec modify(GroupId :: binary(), Name :: binary()) ->
    ok | no_return().
%% ====================================================================
modify(GroupId, Name) ->
    Doc = dao_adapter:group_doc(GroupId),
    #veil_document{record = #user_group{} = Group} = Doc,
    GroupNew = Group#user_group{name = Name},
    dao_adapter:save(Doc#veil_document{record = GroupNew}),
    ok.


%% join/2
%% ====================================================================
%% @doc Adds user to a group identified by a token.
%% ====================================================================
-spec join(UserId :: binary(), Token :: binary()) ->
    {ok, GroupId :: binary()} | no_return().
%% ====================================================================
join(UserId, Token) ->
    {ok, {group, GroupId}} = token_logic:consume(Token, group_invite_token),
    case has_user(GroupId, UserId) of
        true -> ok;
        false ->
            Privileges = privileges:group_user(),
            GroupDoc = dao_adapter:group_doc(GroupId),
            #veil_document{record = #user_group{users = Users} = Group} = GroupDoc,
            GroupNew = Group#user_group{users = [{UserId, Privileges} | Users]},

            UserDoc = dao_adapter:user_doc(UserId),
            #veil_document{record = #user{groups = Groups} = User} = UserDoc,
            UserNew = User#user{groups = [GroupId | Groups]},

            dao_adapter:save(GroupDoc#veil_document{record = GroupNew}),
            dao_adapter:save(UserDoc#veil_document{record = UserNew})
    end,
    {ok, GroupId}.


%% set_privileges/3
%% ====================================================================
%% @doc Sets privileges for a member of the group.
%% ====================================================================
-spec set_privileges(GroupId :: binary(), UserId :: binary(),
                     Privileges :: [privileges:group_privileges()]) ->
    ok | no_return().
%% ====================================================================
set_privileges(GroupId, UserId, Privileges) ->
    Doc = dao_adapter:group_doc(GroupId),
    #veil_document{record = #user_group{users = Users} = Group} = Doc,
    UsersNew = lists:keyreplace(UserId, 1, Users, {UserId, Privileges}),
    GroupNew = Group#user_group{users = UsersNew},
    dao_adapter:save(Doc#veil_document{record = GroupNew}),
    ok.


%% get_data/1
%% ====================================================================
%% @doc Returns details about the group.
%% ====================================================================
-spec get_data(GroupId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_data(GroupId) ->
    #user_group{name = Name} = dao_adapter:group(GroupId),
    {ok, [
        {groupId, GroupId},
        {name, Name}
    ]}.


%% get_users/1
%% ====================================================================
%% @doc Returns details about group's members.
%% ====================================================================
-spec get_users(GroupId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_users(GroupId) ->
    #user_group{users = UserTuples} = dao_adapter:group(GroupId),
    {Users, _} = lists:unzip(UserTuples),
    {ok, [{users, Users}]}.


%% get_spaces/1
%% ====================================================================
%% @doc Returns details about group's spaces.
%% ====================================================================
-spec get_spaces(GroupId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_spaces(GroupId) ->
    #user_group{spaces = Spaces} = dao_adapter:group(GroupId),
    {ok, [{spaces, Spaces}]}.


%% get_user/2
%% ====================================================================
%% @doc Returns details about group's member.
%% ====================================================================
-spec get_user(GroupId :: binary(), UserId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_user(_GroupId, UserId) ->
    %% @todo: we don't want to give out every bit of data once clients have more data stored
    user_logic:get_data(UserId).


%% get_privileges/2
%% ====================================================================
%% @doc Returns list of group's member privileges.
%% ====================================================================
-spec get_privileges(GroupId :: binary(), UserId :: binary()) ->
    {ok, [privileges:group_privilege()]} | no_return().
%% ====================================================================
get_privileges(GroupId, UserId) ->
    #user_group{users = UserTuples} = dao_adapter:group(GroupId),
    {_, Privileges} = lists:keyfind(UserId, 1, UserTuples),
    {ok, Privileges}.


%% remove/1
%% ====================================================================
%% @doc Removes the group.
%% ====================================================================
-spec remove(GroupId :: binary()) -> true | no_return().
%% ====================================================================
remove(GroupId) ->
    Group = dao_adapter:group(GroupId),
    #user_group{users = Users, spaces = Spaces} = Group,

    lists:foreach(fun({UserId, _}) ->
        UserDoc = dao_adapter:user_doc(UserId),
        #veil_document{record = #user{groups = UGroups} = User} = UserDoc,
        NewUser = User#user{groups = lists:delete(GroupId, UGroups)},
        dao_adapter:save(UserDoc#veil_document{record = NewUser})
    end, Users),

    lists:foreach(fun(SpaceId) ->
        SpaceDoc = dao_adapter:space_doc(SpaceId),
        #veil_document{record = #space{groups = SGroups} = Space} = SpaceDoc,
        NewSpace = Space#space{groups = lists:keydelete(GroupId, 1, SGroups)},
        dao_adapter:save(SpaceDoc#veil_document{record = NewSpace}),
        space_logic:cleanup(SpaceId)
    end, Spaces),

    dao_adapter:group_remove(GroupId).


%% remove_user/2
%% ====================================================================
%% @doc Removes user from the group.
%% ====================================================================
-spec remove_user(GroupId :: binary(), UserId :: binary()) -> true | no_return().
%% ====================================================================
remove_user(GroupId, UserId) ->
    UserDoc = dao_adapter:user_doc(UserId),
    #veil_document{record = #user{groups = Groups} = User} = UserDoc,
    UserNew = User#user{groups = lists:delete(GroupId, Groups)},

    GroupDoc = dao_adapter:group_doc(GroupId),
    #veil_document{record = #user_group{users = Users} = Group} = GroupDoc,
    GroupNew = Group#user_group{users = lists:keydelete(UserId, 1, Users)},

    dao_adapter:save(UserDoc#veil_document{record = UserNew}),
    dao_adapter:save(GroupDoc#veil_document{record = GroupNew}),
    cleanup(GroupId),
    true.


%% cleanup/1
%% ====================================================================
%% @doc Removes the group if empty.
%% ====================================================================
-spec cleanup(GroupId :: binary()) -> ok.
%% ====================================================================
cleanup(GroupId) ->
    #user_group{users = Users} = dao_adapter:group(GroupId),
    case Users of
        [] -> remove(GroupId);
        _ -> ok
    end,
    ok.
