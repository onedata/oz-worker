%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module implementing the business logic for spaces in the registry.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(space_logic).
-author("Konrad Zemek").

-include("dao/dao_types.hrl").

-include_lib("ctool/include/logging.hrl").


%% API
-export([exists/1, has_provider/2, has_user/2, has_effective_user/2, has_group/2,
    has_effective_privilege/3]).
-export([create/2, create/4, modify/2, set_privileges/3, join/2, support/3]).
-export([get_data/2, get_users/1, get_effective_users/1, get_groups/1,
    get_providers/2, get_user/3, get_group/2, get_provider/3, get_privileges/2,
    get_effective_privileges/2]).
-export([remove/1, remove_user/2, remove_group/2, remove_provider/2, cleanup/1]).


%% exists/1
%% ====================================================================
%% @doc Returns whether a Space exists.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec exists(SpaceId :: binary()) ->
    boolean().
%% ====================================================================
exists(SpaceId) ->
    dao_adapter:space_exists(SpaceId).


%% has_provider/2
%% ====================================================================
%% @doc Returns whether the provider identified by ProviderId supports the
%% Space. Shall return false in any other case (Space doesn't exist, etc).
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec has_provider(SpaceId :: binary(), ProviderId :: binary()) ->
    boolean().
%% ====================================================================
has_provider(SpaceId, ProviderId) ->
    case exists(SpaceId) of
        false -> false;
        true ->
            #space{providers = Providers} = dao_adapter:space(SpaceId),
            lists:member(ProviderId, Providers)
    end.


%% has_user/2
%% ====================================================================
%% @doc Returns whether the user identified by UserId is a member of the Space.
%% Shall return false in any other case (Space doesn't exist, etc).
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec has_user(SpaceId :: binary(), UserId :: binary()) ->
    boolean().
%% ====================================================================
has_user(SpaceId, UserId) ->
    case exists(SpaceId) of
        false -> false;
        true ->
            #space{users = Users} = dao_adapter:space(SpaceId),
            lists:keymember(UserId, 1, Users)
    end.


%% has_effective_user/2
%% ====================================================================
%% @doc Returns whether the user identified by UserId is a member of the Space,
%% either directly or through a group.
%% Shall return false in any other case (Space doesn't exist, etc).
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec has_effective_user(SpaceId :: binary(), UserId :: binary()) ->
    boolean().
%% ====================================================================
has_effective_user(SpaceId, UserId) ->
    case exists(SpaceId) of
        false -> false;
        true ->
            #space{users = Users, groups = SpaceGroups} = dao_adapter:space(SpaceId),
            case lists:keymember(UserId, 1, Users) of
                true -> true;
                false ->
                    case user_logic:exists(UserId) of
                        false -> false;
                        true ->
                            #user{groups = UserGroups} = dao_adapter:user(UserId),
                            SpaceGroupsSet = ordsets:from_list([GroupId || {GroupId, _} <- SpaceGroups]),
                            UserGroupsSet  = ordsets:from_list(UserGroups),
                            not ordsets:is_disjoint(SpaceGroupsSet, UserGroupsSet)
                    end
            end
    end.


%% has_group/2
%% ====================================================================
%% @doc Returns whether the group identified by GroupId is a member of the
%% Space. Shall return false in any other case (Space doesn't exist, etc).
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec has_group(SpaceId :: binary(), GroupId :: binary()) ->
    boolean().
%% ====================================================================
has_group(SpaceId, GroupId) ->
    case exists(SpaceId) of
        false -> false;
        true ->
            #space{groups = Groups} = dao_adapter:space(SpaceId),
            lists:keymember(GroupId, 1, Groups)
    end.


%% has_privilege/3
%% ====================================================================
%% @doc Returns whether the Space's user identified by UserId has privilege
%% in the Space. Shall return false in any other case (Space doesn't exist,
%% user is not Space's member, etc).
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec has_effective_privilege(SpaceId :: binary(), UserId :: binary(),
                              Privilege :: privileges:space_privilege()) ->
    boolean().
%% ====================================================================
has_effective_privilege(SpaceId, UserId, Privilege) ->
    case has_effective_user(SpaceId, UserId) of
        false -> false;
        true ->
            {ok, UserPrivileges} = get_effective_privileges(SpaceId, UserId),
            ordsets:is_element(Privilege, UserPrivileges)
    end.


%% create/2
%% ====================================================================
%% @doc Creates a Space for a user.
%% Throws exception when call to dao fails, or given member doesn't exist.
%% @end
%% ====================================================================
-spec create({user | group, Id :: binary()}, Name :: binary()) ->
    {ok, SpaceId :: binary()} | no_return().
%% ====================================================================
create(Member, Name) ->
    create_with_provider(Member, Name, [], []).


%% create/4
%% ====================================================================
%% @doc Creates a Space for a user, by a provider that will support it.
%% Throws exception when call to dao fails, or token/member_from_token doesn't exist.
%% @end
%% ====================================================================
-spec create({provider, ProviderId :: binary()}, Name :: binary(),
             Token :: binary(), Size :: pos_integer()) ->
    {ok, SpaceId :: binary()}.
%% ====================================================================
create({provider, ProviderId}, Name, Token, Size) ->
    {ok, Member} = token_logic:consume(Token, space_create_token),
    create_with_provider(Member, Name, [ProviderId], [{ProviderId, Size}]).


%% modify/2
%% ====================================================================
%% @doc Modifies Space's data.
%% Throws exception when call to dao fails, or space doesn't exist.
%% @end
%% ====================================================================
-spec modify(SpaceId :: binary(), Name :: binary()) ->
    ok.
%% ====================================================================
modify(SpaceId, Name) ->
    #db_document{record = #space{providers = SpaceProviders} = Space}
        = Doc = dao_adapter:space_doc(SpaceId),
    SpaceNew = Space#space{name = Name},
    dao_adapter:save(Doc#db_document{record = SpaceNew}),
    op_channel_logic:space_modified(SpaceProviders, SpaceId, SpaceNew),
    ok.


%% set_privileges/3
%% ====================================================================
%% @doc Sets privileges for a member of the Space.
%% Throws exception when call to dao fails, or space doesn't exist.
%% @end
%% ====================================================================
-spec set_privileges(SpaceId :: binary(), {user | group, Id :: binary()},
                     Privileges :: [privileges:space_privilege()]) ->
    ok.
%% ====================================================================
set_privileges(SpaceId, Member, Privileges) ->
    #db_document{record = Space} = Doc = dao_adapter:space_doc(SpaceId),
    PrivilegesNew = ordsets:from_list(Privileges),
    SpaceNew = set_privileges_aux(Space, Member, PrivilegesNew),
    dao_adapter:save(Doc#db_document{record = SpaceNew}),
    ok.


%% join/2
%% ====================================================================
%% @doc Adds a new member to a Space identified by a token.
%% Throws exception when call to dao fails, or member/token/space_from_token doesn't exist.
%% @end
%% ====================================================================
-spec join({group | user, Id :: binary()}, Token :: binary()) ->
    {ok, SpaceId :: binary()}.
%% ====================================================================
join({user, UserId}, Token) ->
    {ok, {space, SpaceId}} = token_logic:consume(Token, space_invite_user_token),
    case has_user(SpaceId, UserId) of
        true -> ok;
        false ->
            Privileges = privileges:space_user(),
            SpaceDoc = dao_adapter:space_doc(SpaceId),
            #db_document{record = #space{users = Users, providers = SpaceProviders} = Space} = SpaceDoc,
            SpaceNew = Space#space{users = [{UserId, Privileges} | Users]},

            UserDoc = dao_adapter:user_doc(UserId),
            #db_document{record = #user{spaces = Spaces} = User} = UserDoc,
            UserNew = User#user{spaces = [SpaceId | Spaces]},

            dao_adapter:save(SpaceDoc#db_document{record = SpaceNew}),
            dao_adapter:save(UserDoc#db_document{record = UserNew}),

            op_channel_logic:space_modified(SpaceProviders, SpaceId, SpaceNew),
            {ok, [{providers, UserProviders}]} = user_logic:get_providers(UserId),
            op_channel_logic:user_modified(UserProviders, UserId, UserNew)
    end,
    {ok, SpaceId};
join({group, GroupId}, Token) ->
    {ok, {space, SpaceId}} = token_logic:consume(Token, space_invite_group_token),
    case has_group(SpaceId, GroupId) of
        true -> ok;
        false ->
            Privileges = privileges:space_user(),
            SpaceDoc = dao_adapter:space_doc(SpaceId),
            #db_document{record = #space{groups = Groups, providers = SpaceProviders} = Space} = SpaceDoc,
            SpaceNew = Space#space{groups = [{GroupId, Privileges} | Groups]},

            GroupDoc = dao_adapter:group_doc(GroupId),
            #db_document{record = #user_group{users = Users, spaces = Spaces} = Group} = GroupDoc,
            GroupNew = Group#user_group{spaces = [SpaceId | Spaces]},

            dao_adapter:save(SpaceDoc#db_document{record = SpaceNew}),
            dao_adapter:save(GroupDoc#db_document{record = GroupNew}),

            op_channel_logic:space_modified(SpaceProviders, SpaceId, SpaceNew),
            {ok, [{providers, GroupProviders}]} = group_logic:get_providers(GroupId),
            op_channel_logic:group_modified(GroupProviders, GroupId, GroupNew),
            lists:foreach(fun({UserId, _}) ->
                op_channel_logic:user_modified(GroupProviders, UserId, dao_adapter:user(UserId))
            end, Users)
    end,
    {ok, SpaceId}.


%% support/2
%% ====================================================================
%% @doc Adds a new supporting provider to a Space identified by a token.
%% Throws exception when call to dao fails, or provider/token/space_from_token doesn't exist.
%% @end
%% ====================================================================
-spec support(ProviderId :: binary(), Token :: binary(), SupportedSize :: pos_integer()) ->
    {ok, SpaceId :: binary()}.
%% ====================================================================
support(ProviderId, Token, SupportedSize) ->
    {ok, {space, SpaceId}} = token_logic:consume(Token, space_support_token),
    case has_provider(SpaceId, ProviderId) of
        true -> ok;
        false ->
            SpaceDoc = dao_adapter:space_doc(SpaceId),
            #db_document{record = #space{size = Size, users = Users, groups = Groups, providers = Providers} = Space} = SpaceDoc,
            SpaceNew = Space#space{size = [{ProviderId, SupportedSize} | Size], providers = [ProviderId | Providers]},

            ProviderDoc = dao_adapter:provider_doc(ProviderId),
            #db_document{record = #provider{spaces = Spaces} = Provider} = ProviderDoc,
            ProviderNew = Provider#provider{spaces = [SpaceId | Spaces]},

            dao_adapter:save(SpaceDoc#db_document{record = SpaceNew}),
            dao_adapter:save(ProviderDoc#db_document{record = ProviderNew}),
            op_channel_logic:space_modified([ProviderId | Providers], SpaceId, SpaceNew),
            lists:foreach(fun({UserId, _}) ->
                op_channel_logic:user_modified([ProviderId], UserId, dao_adapter:user(UserId))
            end, Users),
            lists:foreach(fun({GroupId, _}) ->
                #user_group{users = GroupUsers} = Group = dao_adapter:group(GroupId),
                op_channel_logic:group_modified([ProviderId], GroupId, Group),
                lists:foreach(fun({GroupUserId, _}) ->
                    op_channel_logic:user_modified([ProviderId], GroupUserId, dao_adapter:user(GroupUserId))
                end, GroupUsers)
            end, Groups)
    end,
    {ok, SpaceId}.


%% get_data/2
%% ====================================================================
%% @doc Returns details about the Space.
%% Throws exception when call to dao fails, or space doesn't exist.
%% @end
%% ====================================================================
-spec get_data(SpaceId :: binary(), Client :: user | provider) ->
    {ok, [proplists:property()]}.
%% ====================================================================
get_data(SpaceId, _Client) ->
    #space{name = Name, size = Size} = dao_adapter:space(SpaceId),
    {ok, [
        {spaceId, SpaceId},
        {name, Name},
        {size, Size}
    ]}.


%% get_users/1
%% ====================================================================
%% @doc Returns details about Space's users.
%% Throws exception when call to dao fails, or space doesn't exist.
%% @end
%% ====================================================================
-spec get_users(SpaceId :: binary()) ->
    {ok, [proplists:property()]}.
%% ====================================================================
get_users(SpaceId) ->
    #space{users = Users} = dao_adapter:space(SpaceId),
    {UserIds, _} = lists:unzip(Users),
    {ok, [{users, UserIds}]}.


%% get_effective_users/1
%% ====================================================================
%% @doc Returns details about Space's users. The users may belong to the Space
%% directly or indirectly through their groups.
%% Throws exception when call to dao fails, or space doesn't exist.
%% @end
%% ====================================================================
-spec get_effective_users(SpaceId :: binary()) ->
    {ok, [proplists:property()]}.
%% ====================================================================
get_effective_users(SpaceId) ->
    #space{users = SpaceUserTuples, groups = GroupTuples} = dao_adapter:space(SpaceId),

    GroupUsersSets = lists:map(fun({GroupId, _}) ->
        #user_group{users = GroupUserTuples} = dao_adapter:group(GroupId),
        {GroupUsers, _} = lists:unzip(GroupUserTuples),
        ordsets:from_list(GroupUsers)
    end, GroupTuples),

    {SpaceUsers, _} = lists:unzip(SpaceUserTuples),
    SpaceUsersSet = ordsets:from_list(SpaceUsers),

    AllUserIds = ordsets:union([SpaceUsersSet | GroupUsersSets]),
    {ok, [{users, ordsets:to_list(AllUserIds)}]}.


%% get_groups/1
%% ====================================================================
%% @doc Returns details about Space's groups.
%% Throws exception when call to dao fails, or space doesn't exist.
%% @end
%% ====================================================================
-spec get_groups(SpaceId :: binary()) ->
    {ok, [proplists:property()]}.
%% ====================================================================
get_groups(SpaceId) ->
    #space{groups = GroupTuples} = dao_adapter:space(SpaceId),
    {Groups, _} = lists:unzip(GroupTuples),
    {ok, [{groups, Groups}]}.


%% get_providers/2
%% ====================================================================
%% @doc Returns details about Space's providers.
%% Throws exception when call to dao fails, or space doesn't exist.
%% @end
%% ====================================================================
-spec get_providers(SpaceId :: binary(), Client :: user | provider) ->
    {ok, [proplists:property()]}.
%% ====================================================================
get_providers(SpaceId, _Client) ->
    #space{providers = Providers} = dao_adapter:space(SpaceId),
    {ok, [{providers, Providers}]}.


%% get_user/3
%% ====================================================================
%% @doc Returns details about Space's user.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%% ====================================================================
-spec get_user(SpaceId :: binary(), Client :: user | provider,
               UserId :: binary()) ->
    {ok, [proplists:property()]}.
%% ====================================================================
get_user(_SpaceId, _Client, UserId) ->
    %% @todo: we don't want to give out every bit of data once clients have more data stored
    user_logic:get_data(UserId).


%% get_group/2
%% ====================================================================
%% @doc Returns details about Space's group.
%% Throws exception when call to dao fails, or group doesn't exist.
%% @end
%% ====================================================================
-spec get_group(SpaceId :: binary(), GroupId :: binary()) ->
    {ok, [proplists:property()]}.
%% ====================================================================
get_group(_SpaceId, GroupId) ->
    %% @todo: we don't want to give out every bit of data once groups have more data stored
    group_logic:get_data(GroupId).


%% get_provider/3
%% ====================================================================
%% @doc Returns details about Space's provider.
%% Throws exception when call to dao fails, or provider doesn't exist.
%% @end
%% ====================================================================
-spec get_provider(SpaceId :: binary(), Client :: user | provider,
                   UserId :: binary()) ->
    {ok, [proplists:property()]}.
%% ====================================================================
get_provider(_SpaceId, _Client, ProviderId) ->
    %% @todo: we don't want to give out every bit of data once providers have more data stored
    provider_logic:get_data(ProviderId).


%% get_privileges/2
%% ====================================================================
%% @doc Returns list of Space's member privileges.
%% Throws exception when call to dao fails, or space doesn't exist.
%% @end
%% ====================================================================
-spec get_privileges(SpaceId :: binary(), {user | group, Id :: binary()}) ->
    {ok, [privileges:space_privilege()]}.
%% ====================================================================
get_privileges(SpaceId, {user, UserId}) ->
    #space{users = Users} = dao_adapter:space(SpaceId),
    {_, Privileges} = lists:keyfind(UserId, 1, Users),
    {ok, Privileges};
get_privileges(SpaceId, {group, GroupId}) ->
    #space{groups = Groups} = dao_adapter:space(SpaceId),
    {_, Privileges} = lists:keyfind(GroupId, 1, Groups),
    {ok, Privileges}.


%% remove/1
%% ====================================================================
%% @doc Removes the Space.
%% Throws exception when call to dao fails, or space is already removed.
%% @end
%% ====================================================================
-spec remove(SpaceId :: binary()) ->
    true.
%% ====================================================================
remove(SpaceId) ->
    Space = dao_adapter:space(SpaceId),
    #space{users = Users, groups = Groups, providers = Providers} = Space,

    lists:foreach(fun({UserId, _}) ->
        UserDoc = dao_adapter:user_doc(UserId),
        #db_document{record = #user{spaces = USpaces} = User} = UserDoc,
        NewUser = User#user{spaces = lists:delete(SpaceId, USpaces)},
        dao_adapter:save(UserDoc#db_document{record = NewUser}),
        {ok, [{providers, UserProviders}]} = user_logic:get_providers(UserId),
        op_channel_logic:user_modified(UserProviders, UserId, NewUser)
    end, Users),

    lists:foreach(fun({GroupId, _}) ->
        GroupDoc = dao_adapter:group_doc(GroupId),
        #db_document{record = #user_group{spaces = GSpaces} = Group} = GroupDoc,
        NewGroup = Group#user_group{spaces = lists:delete(SpaceId, GSpaces)},
        dao_adapter:save(GroupDoc#db_document{record = NewGroup})
    end, Groups),

    lists:foreach(fun(ProviderId) ->
        ProviderDoc = dao_adapter:provider_doc(ProviderId),
        #db_document{record = #provider{spaces = PSpaces} = Provider} = ProviderDoc,
        NewProvider = Provider#provider{spaces = lists:delete(SpaceId, PSpaces)},
        dao_adapter:save(ProviderDoc#db_document{record = NewProvider})
    end, Providers),

    op_channel_logic:space_removed(Providers, SpaceId),
    dao_adapter:space_remove(SpaceId).


%% remove_user/2
%% ====================================================================
%% @doc Removes user from the Space.
%% Throws exception when call to dao fails, or space/user doesn't exist.
%% @end
%% ====================================================================
-spec remove_user(SpaceId :: binary(), UserId :: binary()) ->
    true.
%% ====================================================================
remove_user(SpaceId, UserId) ->
    {ok, [{providers, UserProviders}]} = user_logic:get_providers(UserId),

    UserDoc = dao_adapter:user_doc(UserId),
    #db_document{record = #user{spaces = Spaces} = User} = UserDoc,
    UserNew = User#user{spaces = lists:delete(SpaceId, Spaces)},

    SpaceDoc = dao_adapter:space_doc(SpaceId),
    #db_document{record = #space{users = Users, providers = SpaceProviders} = Space} = SpaceDoc,
    SpaceNew = Space#space{users = lists:keydelete(UserId, 1, Users)},

    dao_adapter:save(UserDoc#db_document{record = UserNew}),
    dao_adapter:save(SpaceDoc#db_document{record = SpaceNew}),
    cleanup(SpaceId),

    op_channel_logic:space_modified(SpaceProviders, SpaceId, SpaceNew),
    op_channel_logic:user_modified(UserProviders, UserId, UserNew),
    true.


%% remove_group/2
%% ====================================================================
%% @doc Removes group from the Space.
%% Throws exception when call to dao fails, or space/group doesn't exist.
%% @end
%% ====================================================================
-spec remove_group(SpaceId :: binary(), GroupId :: binary()) ->
    true.
%% ====================================================================
remove_group(SpaceId, GroupId) ->
    GroupDoc = dao_adapter:group_doc(GroupId),
    #db_document{record = #user_group{spaces = Spaces} = Group} = GroupDoc,
    GroupNew = Group#user_group{spaces = lists:delete(SpaceId, Spaces)},

    SpaceDoc = dao_adapter:space_doc(SpaceId),
    #db_document{record = #space{groups = Groups, providers = Providers} = Space} = SpaceDoc,
    SpaceNew = Space#space{groups = lists:keydelete(GroupId, 1, Groups)},

    dao_adapter:save(GroupDoc#db_document{record = GroupNew}),
    dao_adapter:save(SpaceDoc#db_document{record = SpaceNew}),
    cleanup(SpaceId),

    op_channel_logic:space_modified(Providers, SpaceId, SpaceNew),
    true.


%% remove_provider/2
%% ====================================================================
%% @doc Removes provider from the Space.
%% Throws exception when call to dao fails, or space/provider doesn't exist.
%% @end
%% ====================================================================
-spec remove_provider(SpaceId :: binary(), ProviderId :: binary()) ->
    true.
%% ====================================================================
remove_provider(SpaceId, ProviderId) ->
    ProviderDoc = dao_adapter:provider_doc(ProviderId),
    #db_document{record = #provider{spaces = Spaces} = Provider} = ProviderDoc,
    ProviderNew = Provider#provider{spaces = lists:delete(SpaceId, Spaces)},

    SpaceDoc = dao_adapter:space_doc(SpaceId),
    #db_document{record = #space{providers = Providers, size = Size} = Space} = SpaceDoc,
    SpaceNew = Space#space{providers = lists:delete(ProviderId, Providers), size = proplists:delete(ProviderId, Size)},

    dao_adapter:save(ProviderDoc#db_document{record = ProviderNew}),
    dao_adapter:save(SpaceDoc#db_document{record = SpaceNew}),

    op_channel_logic:space_modified(SpaceNew#space.providers, SpaceId, SpaceNew),
    op_channel_logic:space_removed([ProviderId], SpaceId),
    true.


%% create_with_provider/3
%% ====================================================================
%% @doc Creates a Space for a user or a group, with a preexisting provider.
%% Throws exception when call to dao fails, or user/group doesn't exist.
%% @end
%% ====================================================================
-spec create_with_provider({user | group, Id :: binary()}, Name :: binary(),
    Providers :: [binary()], Size :: [{Provider :: binary(), ProvidedSize :: pos_integer()}]) ->
    {ok, SpaceId :: binary()}.
%% ====================================================================
create_with_provider({user, UserId}, Name, Providers, Size) ->
    UserDoc = dao_adapter:user_doc(UserId),
    #db_document{record = #user{spaces = Spaces} = User} = UserDoc,

    Privileges = privileges:space_admin(),
    Space = #space{name = Name, size = Size, providers = Providers, users = [{UserId, Privileges}]},
    SpaceId = dao_adapter:save(Space),

    UserNew = User#user{spaces = [SpaceId | Spaces]},
    dao_adapter:save(UserDoc#db_document{record = UserNew}),

    op_channel_logic:space_modified(Providers, SpaceId, Space),
    op_channel_logic:user_modified(Providers, UserId, UserNew),
    {ok, SpaceId};
create_with_provider({group, GroupId}, Name, Providers, Size) ->
    GroupDoc = dao_adapter:group_doc(GroupId),
    #db_document{record = #user_group{users = Users, spaces = Spaces} = Group} = GroupDoc,

    Privileges = privileges:space_admin(),
    Space = #space{name = Name, size = Size, providers = Providers, groups = [{GroupId, Privileges}]},
    SpaceId = dao_adapter:save(Space),

    GroupNew = Group#user_group{spaces = [SpaceId | Spaces]},
    dao_adapter:save(GroupDoc#db_document{record = GroupNew}),

    op_channel_logic:space_modified(Providers, SpaceId, Space),
    op_channel_logic:group_modified(Providers, GroupId, Group),
    lists:foreach(fun({UserId, _}) ->
        op_channel_logic:user_modified(Providers, UserId, dao_adapter:user(UserId))
    end, Users),
    {ok, SpaceId}.


%% cleanup/1
%% ====================================================================
%% @doc Removes the space if empty.
%% Throws exception when call to dao fails, or space is already removed.
%% @end
%% ====================================================================
-spec cleanup(SpaceId :: binary()) -> ok | no_return().
%% ====================================================================
cleanup(SpaceId) ->
    #space{groups = Groups, users = Users} = dao_adapter:space(SpaceId),
    case {Groups, Users} of
        {[], []} -> remove(SpaceId);
        _ -> ok
    end,
    ok.


%% get_effective_privileges/2
%% ====================================================================
%% @doc Retrieves effective user privileges taking into account any groups
%% he is a member of that also are members of the Space.
%% Throws exception when call to dao fails, or space/user doesn't exist.
%% @end
%% ====================================================================
-spec get_effective_privileges(SpaceId :: binary(), UserId :: binary()) ->
    {ok, ordsets:ordset(privileges:space_privilege())}.
%% ====================================================================
get_effective_privileges(SpaceId, UserId) ->
    #user{groups = UGroups} = dao_adapter:user(UserId),
    #space{users = UserTuples, groups = SGroupTuples} = dao_adapter:space(SpaceId),

    UserGroups = sets:from_list(UGroups),

    PrivilegesSets = lists:filtermap(fun({GroupId, Privileges}) ->
        case sets:is_element(GroupId, UserGroups) of
            true -> {true, ordsets:from_list(Privileges)};
            false -> false
        end
    end, SGroupTuples),

    UserPrivileges =
        case lists:keyfind(UserId, 1, UserTuples) of
            {UserId, Privileges} -> ordsets:from_list(Privileges);
            false -> ordsets:new()
        end,

    {ok, ordsets:union([UserPrivileges | PrivilegesSets])}.


%% set_privileges_aux/3
%% ====================================================================
%% @doc Transforms a space to include new privileges.
%% ====================================================================
-spec set_privileges_aux(Space :: space_info(), {user | group, Id :: binary()},
                         Privileges :: [privileges:space_privilege()]) ->
    space_info().
%% ====================================================================
set_privileges_aux(#space{users = Users} = Space, {user, UserId}, Privileges) ->
    UsersNew = lists:keyreplace(UserId, 1, Users, {UserId, Privileges}),
    Space#space{users = UsersNew};
set_privileges_aux(#space{groups = Groups} = Space, {group, GroupId}, Privileges) ->
    GroupsNew = lists:keyreplace(GroupId, 1, Groups, {GroupId, Privileges}),
    Space#space{groups = GroupsNew}.
