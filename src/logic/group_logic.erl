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
-export([remove/1, remove_user/2]).


%% exists/1
%% ====================================================================
%% @doc Returns whether a group exists.
%% ====================================================================
-spec exists(GroupId :: binary()) -> boolean().
%% ====================================================================
exists(GroupId) ->
    SGroupId = binary:bin_to_list(GroupId),
    {ok, Exists} = dao_lib:apply(dao_groups, exist_group, [SGroupId], 1),
    Exists.


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
            #veil_document{record = #user_group{users = Users}} = get_doc(GroupId),
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
    case exists(GroupId) andalso has_user(GroupId, UserId) of
        false -> false;
        true ->
            #veil_document{record = #user_group{users = Users}} = get_doc(GroupId),
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
create(UserId, Name) -> %% @todo: the adding-to-user part can be extracted
    SUserId = binary:bin_to_list(UserId),
    {ok, UserDoc} = dao_lib:apply(dao_users, get_user, [SUserId], 1),
    #veil_document{record = #user{groups = Groups} = User} = UserDoc,

    Privileges = privileges:group_admin(),
    Group = #user_group{name = Name, users = [{UserId, Privileges}]},
    {ok, SGroupId} = dao_lib:apply(dao_groups, save_group, [Group], 1),
    GroupId = binary:list_to_bin(SGroupId),

    UserNew = User#user{groups = [GroupId | Groups]},
    UserDocNew = UserDoc#veil_document{record = UserNew},
    {ok, _} = dao_lib:apply(dao_users, save_user, [UserDocNew], 1),

    {ok, GroupId}.


%% modify/2
%% ====================================================================
%% @doc Modifies group's data.
%% ====================================================================
-spec modify(GroupId :: binary(), Name :: binary()) ->
    ok | no_return().
%% ====================================================================
modify(GroupId, Name) ->
    Doc = get_doc(GroupId),
    #veil_document{record = #user_group{} = Group} = Doc,
    DocNew = Doc#veil_document{record = Group#user_group{name = Name}},
    {ok, _} = dao_lib:apply(dao_groups, save_group, [DocNew], 1),
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
    Doc = get_doc(GroupId),
    Privileges = privileges:group_user(),
    #veil_document{record = #user_group{users = Users} = Group} = Doc,
    GroupNew = Group#user_group{users = [{UserId, Privileges} | Users]},
    DocNew = Doc#veil_document{record = GroupNew},

    SUserId = binary:bin_to_list(UserId),
    {ok, UserDoc} = dao_lib:apply(dao_users, get_user, [SUserId], 1),
    #veil_document{record = #user{groups = Groups} = User} = UserDoc,
    UserNew = User#user{groups = [GroupId | Groups]},
    UserDocNew = UserDoc#veil_document{record = UserNew},

    {ok, _} = dao_lib:apply(dao_groups, save_group, [DocNew], 1), %% @todo: something more transaction-like across the board
    {ok, _} = dao_lib:apply(dao_users, save_user, [UserDocNew], 1),
    {ok, GroupId}.


%% set_privileges/3
%% ====================================================================
%% @doc Sets privileges for a member of the group.
%% ====================================================================
-spec set_privileges(GroupId :: binary(), UserId :: binary(),
                     Privileges :: [privileges:privilege()]) ->
    ok | no_return().
%% ====================================================================
set_privileges(GroupId, UserId, Privileges) ->
    Doc = get_doc(GroupId),
    #veil_document{record = #user_group{users = Users} = Group} = Doc,
    UsersNew = lists:keyreplace(UserId, 1, Users, {UserId, Privileges}),
    DocNew = Doc#veil_document{record = Group#user_group{users = UsersNew}},
    {ok, _} = {ok, _} = dao_lib:apply(dao_groups, save_group, [DocNew], 1),
    ok.


%% get_data/1
%% ====================================================================
%% @doc Returns details about the group.
%% ====================================================================
-spec get_data(GroupId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_data(GroupId) ->
    Doc = get_doc(GroupId),
    #veil_document{record = #user_group{name = Name}} = Doc,
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
    Doc = get_doc(GroupId),
    #veil_document{record = #user_group{users = Users}} = Doc,
    UserIds = lists:unzip(Users),
    {ok, [{users, UserIds}]}.


%% get_spaces/1
%% ====================================================================
%% @doc Returns details about group's spaces.
%% ====================================================================
-spec get_spaces(GroupId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_spaces(GroupId) ->
    Doc = get_doc(GroupId),
    #veil_document{record = #user_group{spaces = Spaces}} = Doc,
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
    Doc = get_doc(GroupId),
    #veil_document{record = #user_group{users = Users}} = Doc,
    {_, Privileges} = lists:keyfind(UserId, 1, Users),
    {ok, Privileges}.


%% remove/1
%% ====================================================================
%% @doc Removes the group. Should return true if after the call the group
%% doesn't exist; in particular if it never existed at all.
%% @end
%% ====================================================================
-spec remove(GroupId :: binary()) -> boolean().
%% ====================================================================
remove(GroupId) ->
    case exists(GroupId) of
        false -> true;
        true ->
            SGroupId = binary:bin_to_list(GroupId),
            ok = dao_lib:apply(dao_groups, remove_group, [SGroupId], 1),
            true
    end.


%% remove_user/2
%% ====================================================================
%% @doc Removes user from the group. Should return true if after the call the
%% user will no longer be a member of the group; in particular if he never
%% was a member or the group didn't exist.
%% @end
%% ====================================================================
-spec remove_user(GroupId :: binary(), UserId :: binary()) -> boolean().
%% ====================================================================
remove_user(GroupId, UserId) -> %% @todo: when nobody's left, group should be destroyed
    Doc = get_doc(GroupId),
    #veil_document{record = #user_group{users = Users} = Group} = Doc,
    GroupNew = Group#user_group{users = lists:keydelete(UserId, 1, Users)},
    DocNew = Doc#veil_document{record = GroupNew},

    SUserId = binary:bin_to_list(UserId),
    {ok, UserDoc} = dao_lib:apply(dao_users, get_user, [SUserId], 1),
    #veil_document{record = #user{groups = Groups} = User} = UserDoc,
    UserNew = User#user{groups = lists:keydelete(GroupId, 1, Groups)},
    UserDocNew = Doc#veil_document{record = UserNew},

    {ok, _} = dao_lib:apply(dao_groups, save_group, [DocNew], 1),
    {ok, _} = dao_lib:apply(dao_users, save_user, [UserDocNew], 1),

    true.


%% get_doc/1
%% ====================================================================
%% @doc Retrieves a group from the database.
%% ====================================================================
-spec get_doc(GroupId :: binary()) -> group_doc() | no_return().
%% ====================================================================
get_doc(GroupId) ->
    SGroupId = binary:bin_to_list(GroupId),
    {ok, #veil_document{record = #user_group{}} = Doc} = dao_lib:apply(dao_groups, get_group, [SGroupId], 1),
    Doc.
