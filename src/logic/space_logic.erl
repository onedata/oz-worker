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


%% API
-export([exists/1, has_provider/2, has_user/2, has_group/2, has_privilege/3]).
-export([create/2, create/3, modify/2, set_privileges/3, join/2, support/2]).
-export([get_data/2, get_users/2, get_groups/1, get_providers/2, get_user/3,
    get_group/2, get_provider/3, get_privileges/2]).
-export([remove/1, remove_user/2, remove_group/2, remove_provider/2, cleanup/1]).


%% exists/1
%% ====================================================================
%% @doc Returns whether a Space exists.
%% ====================================================================
-spec exists(SpaceId :: binary()) -> boolean().
%% ====================================================================
exists(SpaceId) ->
    logic_helper:space_exists(SpaceId).


%% has_provider/2
%% ====================================================================
%% @doc Returns whether the provider identified by ProviderId supports the
%% Space. Shall return false in any other case (Space doesn't exist, etc).
%% @end
%% ====================================================================
-spec has_provider(SpaceId :: binary(), ProviderId :: binary()) -> boolean().
%% ====================================================================
has_provider(SpaceId, ProviderId) ->
    case exists(SpaceId) of
        false -> false;
        true ->
            #space{providers = Providers} = logic_helper:space(SpaceId),
            lists:member(ProviderId, Providers)
    end.


%% has_user/2
%% ====================================================================
%% @doc Returns whether the user identified by UserId is a member of the Space.
%% Shall return false in any other case (Space doesn't exist, etc).
%% @end
%% ====================================================================
-spec has_user(SpaceId :: binary(), UserId :: binary()) -> boolean().
%% ====================================================================
has_user(SpaceId, UserId) ->
    case exists(SpaceId) of
        false -> false;
        true ->
            #space{users = Users} = logic_helper:space(SpaceId),
            lists:keymember(UserId, 1, Users)
    end.


%% has_group/2
%% ====================================================================
%% @doc Returns whether the group identified by GroupId is a member of the
%% Space. Shall return false in any other case (Space doesn't exist, etc).
%% @end
%% ====================================================================
-spec has_group(SpaceId :: binary(), GroupId :: binary()) -> boolean().
%% ====================================================================
has_group(SpaceId, GroupId) ->
    case exists(SpaceId) of
        false -> false;
        true ->
            #space{groups = Groups} = logic_helper:space(SpaceId),
            lists:keymember(GroupId, 1, Groups)
    end.


%% has_privilege/3
%% ====================================================================
%% @doc Returns whether the Space's user identified by UserId has privilege
%% in the Space. Shall return false in any other case (Space doesn't exist,
%% user is not Space's member, etc).
%% @end
%% ====================================================================
-spec has_privilege(SpaceId :: binary(), UserId :: binary(),
    Privilege :: privileges:space_privilege()) -> boolean().
%% ====================================================================
has_privilege(SpaceId, UserId, Privilege) ->
    case has_user(SpaceId, UserId) of
        false -> false;
        true ->
            UserPrivileges = get_effective_privileges(SpaceId, UserId),
            ordsets:is_element(Privilege, UserPrivileges)
    end.


%% create/2
%% ====================================================================
%% @doc Creates a Space for a user.
%% ====================================================================
-spec create({user | group, Id :: binary()}, Name :: binary()) ->
    {ok, SpaceId :: binary()} | no_return().
%% ====================================================================
create(Member, Name) ->
    create_with_provider(Member, Name, []).


%% create/3
%% ====================================================================
%% @doc Creates a Space for a user, by a provider that will support it.
%% ====================================================================
-spec create({provider, ProviderId :: binary()}, Name :: binary(),
             Token :: binary()) ->
    {ok, SpaceId :: binary()} | no_return().
%% ====================================================================
create({provider, ProviderId}, Name, Token) ->
    {ok, Member} = token_logic:consume(Token, space_create_token),
    create_with_provider(Member, Name, [ProviderId]).


%% modify/2
%% ====================================================================
%% @doc Modifies Space's data.
%% ====================================================================
-spec modify(SpaceId :: binary(), Name :: binary()) ->
    ok | no_return().
%% ====================================================================
modify(SpaceId, Name) ->
    #veil_document{record = Space} = Doc = logic_helper:space_doc(SpaceId),
    SpaceNew = Space#space{name = Name},
    logic_helper:save(Doc#veil_document{record = SpaceNew}),
    ok.


%% set_privileges/3
%% ====================================================================
%% @doc Sets privileges for a member of the Space.
%% ====================================================================
-spec set_privileges(SpaceId :: binary(), {user | group, Id :: binary()},
                     Privileges :: [privileges:space_privilege()]) ->
    ok | no_return().
%% ====================================================================
set_privileges(SpaceId, Member, Privileges) ->
    #veil_document{record = Space} = Doc = logic_helper:space_doc(SpaceId),
    PrivilegesNew = ordsets:from_list(Privileges),
    SpaceNew = set_privileges_aux(Space, Member, PrivilegesNew),
    logic_helper:save(Doc#veil_document{record = SpaceNew}),
    ok.


%% join/2
%% ====================================================================
%% @doc Adds a new member to a Space identified by a token.
%% ====================================================================
-spec join({group | user, Id :: binary()}, Token :: binary()) ->
    {ok, SpaceId :: binary()} | no_return().
%% ====================================================================
join({user, UserId}, Token) ->
    {ok, {space, SpaceId}} = token_logic:consume(Token, space_invite_user_token),
    case has_user(SpaceId, UserId) of
        true -> ok;
        false ->
            Privileges = privileges:space_user(),
            SpaceDoc = logic_helper:space_doc(SpaceId),
            #veil_document{record = #space{users = Users} = Space} = SpaceDoc,
            SpaceNew = Space#space{users = [{UserId, Privileges} | Users]},

            UserDoc = logic_helper:user_doc(UserId),
            #veil_document{record = #user{spaces = Spaces} = User} = UserDoc,
            UserNew = User#user{spaces = [SpaceId | Spaces]},

            logic_helper:save(SpaceDoc#veil_document{record = SpaceNew}),
            logic_helper:save(UserDoc#veil_document{record = UserNew})
    end,
    {ok, SpaceId};
join({group, GroupId}, Token) ->
    {ok, {space, SpaceId}} = token_logic:consume(Token, space_invite_group_token),
    case has_group(SpaceId, GroupId) of
        true -> ok;
        false ->
            Privileges = privileges:space_user(),
            SpaceDoc = logic_helper:space_doc(SpaceId),
            #veil_document{record = #space{groups = Groups} = Space} = SpaceDoc,
            SpaceNew = Space#space{groups = [{GroupId, Privileges} | Groups]},

            GroupDoc = logic_helper:group_doc(GroupId),
            #veil_document{record = #user_group{spaces = Spaces} = Group} = GroupDoc,
            GroupNew = Group#user_group{spaces = [SpaceId | Spaces]},

            logic_helper:save(SpaceDoc#veil_document{record = SpaceNew}),
            logic_helper:save(GroupDoc#veil_document{record = GroupNew})
    end,
    {ok, SpaceId}.


%% support/2
%% ====================================================================
%% @doc Adds a new supporting provider to a Space identified by a token.
%% ====================================================================
-spec support(ProviderId :: binary(), Token :: binary()) ->
    {ok, SpaceId :: binary()} | no_return().
%% ====================================================================
support(ProviderId, Token) ->
    {ok, {space, SpaceId}} = token_logic:consume(Token, space_support_token),
    case has_provider(SpaceId, ProviderId) of
        true -> ok;
        false ->
            SpaceDoc = logic_helper:space_doc(SpaceId),
            #veil_document{record = #space{providers = Providers} = Space} = SpaceDoc,
            SpaceNew = Space#space{providers = [ProviderId | Providers]},

            ProviderDoc = logic_helper:provider_doc(ProviderId),
            #veil_document{record = #provider{spaces = Spaces} = Provider} = ProviderDoc,
            ProviderNew = Provider#provider{spaces = [SpaceId | Spaces]},

            logic_helper:save(SpaceDoc#veil_document{record = SpaceNew}),
            logic_helper:save(ProviderDoc#veil_document{record = ProviderNew})
    end,
    {ok, SpaceId}.


%% get_data/2
%% ====================================================================
%% @doc Returns details about the Space.
%% ====================================================================
-spec get_data(SpaceId :: binary(), Client :: user | provider) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_data(SpaceId, _Client) ->
    #space{name = Name} = logic_helper:space(SpaceId),
    {ok, [
        {spaceId, SpaceId},
        {name, Name}
    ]}.


%% get_users/2
%% ====================================================================
%% @doc Returns details about Space's users.
%% ====================================================================
-spec get_users(SpaceId :: binary(), Client :: user | provider) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_users(SpaceId, user) ->
    #space{users = Users} = logic_helper:space(SpaceId),
    {UserIds, _} = lists:unzip(Users),
    {ok, [{users, UserIds}]};
get_users(SpaceId, provider) ->
    #space{users = SpaceUserTuples, groups = GroupTuples} = logic_helper:space(SpaceId),

    GroupUsersSets = lists:map(fun({GroupId, _}) ->
        #user_group{users = GroupUserTuples} = logic_helper:group(GroupId),
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
%% ====================================================================
-spec get_groups(SpaceId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_groups(SpaceId) ->
    #space{groups = GroupTuples} = logic_helper:space(SpaceId),
    {Groups, _} = lists:unzip(GroupTuples),
    {ok, [{groups, Groups}]}.


%% get_providers/2
%% ====================================================================
%% @doc Returns details about Space's providers.
%% ====================================================================
-spec get_providers(SpaceId :: binary(), Client :: user | provider) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_providers(SpaceId, _Client) ->
    #space{providers = Providers} = logic_helper:space(SpaceId),
    {ok, [{providers, Providers}]}.


%% get_user/3
%% ====================================================================
%% @doc Returns details about Space's user.
%% ====================================================================
-spec get_user(SpaceId :: binary(), Client :: user | provider,
               UserId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_user(_SpaceId, _Client, UserId) ->
    %% @todo: we don't want to give out every bit of data once clients have more data stored
    user_logic:get_data(UserId).


%% get_group/2
%% ====================================================================
%% @doc Returns details about Space's group.
%% ====================================================================
-spec get_group(SpaceId :: binary(), GroupId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_group(_SpaceId, GroupId) ->
    %% @todo: we don't want to give out every bit of data once groups have more data stored
    group_logic:get_data(GroupId).


%% get_provider/3
%% ====================================================================
%% @doc Returns details about Space's provider.
%% ====================================================================
-spec get_provider(SpaceId :: binary(), Client :: user | provider,
                   UserId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_provider(_SpaceId, _Client, ProviderId) ->
    %% @todo: we don't want to give out every bit of data once providers have more data stored
    provider_logic:get_data(ProviderId).


%% get_privileges/2
%% ====================================================================
%% @doc Returns list of Space's member privileges.
%% ====================================================================
-spec get_privileges(SpaceId :: binary(), {user | group, Id :: binary()}) ->
    {ok, [privileges:space_privilege()]} | no_return().
%% ====================================================================
get_privileges(SpaceId, {user, UserId}) ->
    #space{users = Users} = logic_helper:space(SpaceId),
    {_, Privileges} = lists:keyfind(UserId, 1, Users),
    {ok, Privileges};
get_privileges(SpaceId, {group, GroupId}) ->
    #space{groups = Groups} = logic_helper:space(SpaceId),
    {_, Privileges} = lists:keyfind(GroupId, 1, Groups),
    {ok, Privileges}.


%% remove/1
%% ====================================================================
%% @doc Removes the Space.
%% ====================================================================
-spec remove(SpaceId :: binary()) -> true | no_return().
%% ====================================================================
remove(SpaceId) ->
    Space = logic_helper:space(SpaceId),
    #space{users = Users, groups = Groups, providers = Providers} = Space,

    lists:foreach(fun({UserId, _}) ->
        UserDoc = logic_helper:user_doc(UserId),
        #veil_document{record = #user{spaces = USpaces} = User} = UserDoc,
        NewUser = User#user{spaces = lists:delete(SpaceId, USpaces)},
        logic_helper:save(UserDoc#veil_document{record = NewUser})
    end, Users),

    lists:foreach(fun({GroupId, _}) ->
        GroupDoc = logic_helper:group_doc(GroupId),
        #veil_document{record = #user_group{spaces = GSpaces} = Group} = GroupDoc,
        NewGroup = Group#user_group{spaces = lists:delete(SpaceId, GSpaces)},
        logic_helper:save(GroupDoc#veil_document{record = NewGroup})
    end, Groups),

    lists:foreach(fun(ProviderId) ->
        ProviderDoc = logic_helper:provider_doc(ProviderId),
        #veil_document{record = #provider{spaces = PSpaces} = Provider} = ProviderDoc,
        NewProvider = Provider#provider{spaces = lists:delete(SpaceId, PSpaces)},
        logic_helper:save(ProviderDoc#veil_document{record = NewProvider})
    end, Providers),

    logic_helper:space_remove(SpaceId).


%% remove_user/2
%% ====================================================================
%% @doc Removes user from the Space.
%% ====================================================================
-spec remove_user(SpaceId :: binary(), UserId :: binary()) -> true | no_return().
%% ====================================================================
remove_user(SpaceId, UserId) ->
    UserDoc = logic_helper:user_doc(UserId),
    #veil_document{record = #user{spaces = Spaces} = User} = UserDoc,
    UserNew = User#user{spaces = lists:delete(SpaceId, Spaces)},

    SpaceDoc = logic_helper:space_doc(SpaceId),
    #veil_document{record = #space{users = Users} = Space} = SpaceDoc,
    SpaceNew = Space#space{users = lists:keydelete(UserId, 1, Users)},

    logic_helper:save(UserDoc#veil_document{record = UserNew}),
    logic_helper:save(SpaceDoc#veil_document{record = SpaceNew}),
    cleanup(SpaceId),
    true.


%% remove_group/2
%% ====================================================================
%% @doc Removes group from the Space.
%% ====================================================================
-spec remove_group(SpaceId :: binary(), GroupId :: binary()) -> true | no_return().
%% ====================================================================
remove_group(SpaceId, GroupId) ->
    GroupDoc = logic_helper:group_doc(GroupId),
    #veil_document{record = #user_group{spaces = Spaces} = Group} = GroupDoc,
    GroupNew = Group#user_group{spaces = lists:delete(SpaceId, Spaces)},

    SpaceDoc = logic_helper:space_doc(SpaceId),
    #veil_document{record = #space{groups = Groups} = Space} = SpaceDoc,
    SpaceNew = Space#space{groups = lists:keydelete(GroupId, 1, Groups)},

    logic_helper:save(GroupDoc#veil_document{record = GroupNew}),
    logic_helper:save(SpaceDoc#veil_document{record = SpaceNew}),
    cleanup(SpaceId),
    true.


%% remove_provider/2
%% ====================================================================
%% @doc Removes provider from the Space.
%% ====================================================================
-spec remove_provider(SpaceId :: binary(), ProviderId :: binary()) -> true | no_return().
%% ====================================================================
remove_provider(SpaceId, ProviderId) ->
    ProviderDoc = logic_helper:provider_doc(ProviderId),
    #veil_document{record = #provider{spaces = Spaces} = Provider} = ProviderDoc,
    ProviderNew = Provider#provider{spaces = lists:delete(SpaceId, Spaces)},

    SpaceDoc = logic_helper:space_doc(SpaceId),
    #veil_document{record = #space{providers = Providers} = Space} = SpaceDoc,
    SpaceNew = Space#space{providers = lists:delete(ProviderId, Providers)},

    logic_helper:save(ProviderDoc#veil_document{record = ProviderNew}),
    logic_helper:save(SpaceDoc#veil_document{record = SpaceNew}),
    true.


%% create_with_provider/3
%% ====================================================================
%% @doc Creates a Space for a user or a group, with a preexisting provider.
%% ====================================================================
-spec create_with_provider({user | group, Id :: binary()}, Name :: binary(),
                           Providers :: [binary()]) ->
    {ok, SpaceId :: binary()} | no_return().
%% ====================================================================
create_with_provider({user, UserId}, Name, Providers) ->
    UserDoc = logic_helper:user_doc(UserId),
    #veil_document{record = #user{spaces = Spaces} = User} = UserDoc,

    Privileges = privileges:space_admin(),
    Space = #space{name = Name, providers = Providers, users = [{UserId, Privileges}]},
    SpaceId = logic_helper:save(Space),

    UserNew = User#user{spaces = [SpaceId | Spaces]},
    logic_helper:save(UserDoc#veil_document{record = UserNew}),

    {ok, SpaceId};
create_with_provider({group, GroupId}, Name, Providers) ->
    GroupDoc = logic_helper:group_doc(GroupId),
    #veil_document{record = #user_group{spaces = Spaces} = Group} = GroupDoc,

    Privileges = privileges:space_admin(),
    Space = #space{name = Name, providers = Providers, groups = [{GroupId, Privileges}]},
    SpaceId = logic_helper:save(Space),

    GroupNew = Group#user_group{spaces = [SpaceId | Spaces]},
    logic_helper:save(GroupDoc#veil_document{record = GroupNew}),

    {ok, SpaceId}.


%% cleanup/1
%% ====================================================================
%% @doc Removes the space if empty.
%% ====================================================================
-spec cleanup(SpaceId :: binary()) -> ok.
%% ====================================================================
cleanup(SpaceId) ->
    #space{groups = Groups, users = Users} = logic_helper:space(SpaceId),
    case {Groups, Users} of
        {[], []} -> remove(SpaceId);
        _ -> ok
    end,
    ok.


%% get_effective_privileges/2
%% ====================================================================
%% @doc Retrieves effective user privileges taking into account any groups
%% he is a member of that also are members of the Space.
%% @end
%% ====================================================================
-spec get_effective_privileges(SpaceId :: binary(), UserId :: binary()) ->
    ordsets:ordset(privileges:space_privilege()).
%% ====================================================================
get_effective_privileges(SpaceId, UserId) ->
    #user{groups = UGroups} = logic_helper:user(UserId),
    #space{users = UserTuples, groups = SGroupTuples} = logic_helper:space(SpaceId),

    UserGroups = sets:from_list(UGroups),
    PrivilegesSets = lists:filtermap(fun({GroupId, Privileges}) ->
        case sets:is_element(GroupId, UserGroups) of
            true -> {true, ordsets:from_list(Privileges)};
            false -> false
        end
    end, SGroupTuples),

    {_, UPrivileges} = lists:keyfind(UserId, 1, UserTuples),
    UserPrivileges = ordsets:from_list(UPrivileges),

    ordsets:union([UserPrivileges | PrivilegesSets]).


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
