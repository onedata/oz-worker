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

-include("datastore/gr_datastore_models_def.hrl").
-include("datastore/gr_datastore_models_def.hrl").

%% API
-export([exists/1, has_user/2, has_privilege/3]).
-export([create/2, modify/2, join/2, set_privileges/3]).
-export([get_data/1, get_users/1, get_spaces/1, get_providers/1, get_user/2, get_privileges/2]).
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
%% @doc Creates a group for a user.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create(UserId :: binary(), Name :: binary()) ->
    {ok, GroupId :: binary()}.
create(UserId, Name) ->
    {ok, [{providers, UserProviders}]} = user_logic:get_providers(UserId),

    {ok, UserDoc} = onedata_user:get(UserId),
    #document{value = #onedata_user{groups = Groups} = User} = UserDoc,

    Privileges = privileges:group_admin(),
    Group = #user_group{name = Name, users = [{UserId, Privileges}]},
    {ok, GroupId} = user_group:save(#document{value = Group}),

    UserNew = User#onedata_user{groups = [GroupId | Groups]},
    onedata_user:save(UserDoc#document{value = UserNew}),

    op_channel_logic:user_modified(UserProviders, UserId, UserNew),
    {ok, GroupId}.

%%--------------------------------------------------------------------
%% @doc Modifies group's data.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec modify(GroupId :: binary(), Name :: binary()) ->
    ok.
modify(GroupId, Name) ->
    {ok, [{providers, GroupProviders}]} = group_logic:get_providers(GroupId),

    {ok, Doc} = user_group:get(GroupId),
    #document{value = #user_group{} = Group} = Doc,
    GroupNew = Group#user_group{name = Name},
    user_group:save(Doc#document{value = GroupNew}),

    op_channel_logic:group_modified(GroupProviders, GroupId, GroupNew),
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
    {ok, [{providers, UserProviders}]} = user_logic:get_providers(UserId),
    {ok, {group, GroupId}} = token_logic:consume(Macaroon),
    case has_user(GroupId, UserId) of
        true -> ok;
        false ->
            Privileges = privileges:group_user(),
            {ok, GroupDoc} = user_group:get(GroupId),
            #document{value = #user_group{users = Users} = Group} = GroupDoc,
            GroupNew = Group#user_group{users = [{UserId, Privileges} | Users]},

            {ok, UserDoc} = onedata_user:get(UserId),
            #document{value = #onedata_user{groups = Groups} = User} = UserDoc,
            UserNew = User#onedata_user{groups = [GroupId | Groups]},

            user_group:save(GroupDoc#document{value = GroupNew}),
            onedata_user:save(UserDoc#document{value = UserNew}),

            op_channel_logic:user_modified(UserProviders, UserId, UserNew)
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
    {ok, Doc} = user_group:get(GroupId),
    #document{value = #user_group{users = Users} = Group} = Doc,
    UsersNew = lists:keyreplace(UserId, 1, Users, {UserId, Privileges}),
    GroupNew = Group#user_group{users = UsersNew},
    user_group:save(Doc#document{value = GroupNew}),
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
        {ok, #document{value = #space{providers = SpaceProviders}}} = space:get(Space),
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
    %% @todo: we don't want to give out every bit of data once clients have more data stored
    user_logic:get_data(UserId).

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
    {ok, [{providers, GroupProviders}]} = group_logic:get_providers(GroupId),
    {ok, #document{value = #user_group{users = Users, spaces = Spaces}}} = user_group:get(GroupId),

    lists:foreach(fun({UserId, _}) ->
        {ok, UserDoc} = onedata_user:get(UserId),
        #document{value = #onedata_user{groups = UGroups} = User} = UserDoc,
        NewUser = User#onedata_user{groups = lists:delete(GroupId, UGroups)},
        onedata_user:save(UserDoc#document{value = NewUser}),

        op_channel_logic:user_modified(GroupProviders, UserId, NewUser)
    end, Users),

    lists:foreach(fun(SpaceId) ->
        {ok, SpaceDoc} = space:get(SpaceId),
        #document{value = #space{providers = SpaceProviders, groups = SGroups} = Space} = SpaceDoc,
        NewSpace = Space#space{groups = lists:keydelete(GroupId, 1, SGroups)},
        space:save(SpaceDoc#document{value = NewSpace}),
        case space_logic:cleanup(SpaceId) of
            true -> ok;
            false ->
                op_channel_logic:space_modified(SpaceProviders, SpaceId, NewSpace)
        end
    end, Spaces),

    user_group:delete(GroupId),
    op_channel_logic:group_removed(GroupProviders, GroupId),
    true.

%%--------------------------------------------------------------------
%% @doc Removes user from the group.
%% Throws exception when call to the datastore fails, or group/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(GroupId :: binary(), UserId :: binary()) ->
    true.
remove_user(GroupId, UserId) ->
    {ok, [{providers, GroupProviders}]} = group_logic:get_providers(GroupId),

    {ok, UserDoc} = onedata_user:get(UserId),
    #document{value = #onedata_user{groups = Groups} = User} = UserDoc,
    UserNew = User#onedata_user{groups = lists:delete(GroupId, Groups)},

    {ok, GroupDoc} = user_group:get(GroupId),
    #document{value = #user_group{users = Users} = Group} = GroupDoc,
    GroupNew = Group#user_group{users = lists:keydelete(UserId, 1, Users)},

    onedata_user:save(UserDoc#document{value = UserNew}),
    user_group:save(GroupDoc#document{value = GroupNew}),
    cleanup(GroupId),

    op_channel_logic:user_modified(GroupProviders, UserId, UserNew),
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
