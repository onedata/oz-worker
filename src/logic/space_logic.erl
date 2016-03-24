%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module implementing the business logic for spaces in the registry.
%%% This module serves as a buffer between the database and the REST API.
%%% @end
%%%-------------------------------------------------------------------
-module(space_logic).
-author("Konrad Zemek").

-include("datastore/oz_datastore_models_def.hrl").
-include("datastore/oz_datastore_models_def.hrl").

%% API
-export([exists/1, has_provider/2, has_user/2, has_effective_user/2, has_group/2,
    has_effective_privilege/3]).
-export([create/2, create/4, modify/2, set_privileges/3, join/2, support/3]).
-export([get_data/2, get_users/1, get_effective_users/1, get_groups/1,
    get_providers/2, get_user/3, get_group/2, get_provider/3, get_privileges/2,
    get_effective_privileges/2]).
-export([remove/1, remove_user/2, remove_group/2, remove_provider/2, cleanup/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns whether a Space exists.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec exists(SpaceId :: binary()) ->
    boolean().
exists(SpaceId) ->
    space:exists(SpaceId).

%%--------------------------------------------------------------------
%% @doc Returns whether the provider identified by ProviderId supports the
%% Space. Shall return false in any other case (Space doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_provider(SpaceId :: binary(), ProviderId :: binary()) ->
    boolean().
has_provider(SpaceId, ProviderId) ->
    case exists(SpaceId) of
        false -> false;
        true ->
            {ok, #document{value = #space{providers = Providers}}} = space:get(SpaceId),
            lists:member(ProviderId, Providers)
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the user identified by UserId is a member of the Space.
%% Shall return false in any other case (Space doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_user(SpaceId :: binary(), UserId :: binary()) ->
    boolean().
has_user(SpaceId, UserId) ->
    case exists(SpaceId) of
        false -> false;
        true ->
            {ok, #document{value = #space{users = Users}}} = space:get(SpaceId),
            lists:keymember(UserId, 1, Users)
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the user identified by UserId is a member of the Space,
%% either directly or through a group.
%% Shall return false in any other case (Space doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_effective_user(SpaceId :: binary(), UserId :: binary()) ->
    boolean().
has_effective_user(SpaceId, UserId) ->
    case exists(SpaceId) of
        false -> false;
        true ->
            {ok, #document{value = #space{users = Users, groups = SpaceGroups}}} = space:get(SpaceId),
            case lists:keymember(UserId, 1, Users) of
                true -> true;
                false ->
                    case user_logic:exists(UserId) of
                        false -> false;
                        true ->
                            {ok, #document{value = #onedata_user{groups = UserGroups}}} = onedata_user:get(UserId),
                            SpaceGroupsSet = ordsets:from_list([GroupId || {GroupId, _} <- SpaceGroups]),
                            UserGroupsSet = ordsets:from_list(UserGroups),
                            not ordsets:is_disjoint(SpaceGroupsSet, UserGroupsSet)
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the group identified by GroupId is a member of the
%% Space. Shall return false in any other case (Space doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_group(SpaceId :: binary(), GroupId :: binary()) ->
    boolean().
has_group(SpaceId, GroupId) ->
    case exists(SpaceId) of
        false -> false;
        true ->
            {ok, #document{value = #space{groups = Groups}}} = space:get(SpaceId),
            lists:keymember(GroupId, 1, Groups)
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the Space's user identified by UserId has privilege
%% in the Space. Shall return false in any other case (Space doesn't exist,
%% user is not Space's member, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_effective_privilege(SpaceId :: binary(), UserId :: binary(),
    Privilege :: privileges:space_privilege()) ->
    boolean().
has_effective_privilege(SpaceId, UserId, Privilege) ->
    case has_effective_user(SpaceId, UserId) of
        false -> false;
        true ->
            {ok, UserPrivileges} = get_effective_privileges(SpaceId, UserId),
            ordsets:is_element(Privilege, UserPrivileges)
    end.

%%--------------------------------------------------------------------
%% @doc Creates a Space for a user.
%% Throws exception when call to the datastore fails, or given member doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create({user | group, Id :: binary()}, Name :: binary()) ->
    {ok, SpaceId :: binary()} | no_return().
create(Member, Name) ->
    create_with_provider(Member, Name, [], []).

%%--------------------------------------------------------------------
%% @doc Creates a Space for a user, by a provider that will support it.
%% Throws exception when call to the datastore fails, or token/member_from_token doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create({provider, ProviderId :: binary()}, Name :: binary(),
    Macaroon :: macaroon:macaroon(), Size :: pos_integer()) ->
    {ok, SpaceId :: binary()}.
create({provider, ProviderId}, Name, Macaroon, Size) ->
    {ok, Member} = token_logic:consume(Macaroon),
    create_with_provider(Member, Name, [ProviderId], [{ProviderId, Size}]).

%%--------------------------------------------------------------------
%% @doc Modifies Space's data.
%% Throws exception when call to the datastore fails, or space doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec modify(SpaceId :: binary(), Name :: binary()) ->
    ok.
modify(SpaceId, Name) ->
    {ok, _} = space:update(SpaceId, fun(Space) ->
        {ok, Space#space{name = Name}}
    end),
    ok.

%%--------------------------------------------------------------------
%% @doc Sets privileges for a member of the Space.
%% Throws exception when call to the datastore fails, or space doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_privileges(SpaceId :: binary(), {user | group, Id :: binary()},
    Privileges :: [privileges:space_privilege()]) ->
    ok.
set_privileges(SpaceId, Member, Privileges) ->
    {ok, _} = space:update(SpaceId, fun(Space) ->
        PrivilegesNew = ordsets:from_list(Privileges),
        SpaceNew = set_privileges_aux(Space, Member, PrivilegesNew),
        {ok, SpaceNew}
    end),
    ok.

%%--------------------------------------------------------------------
%% @doc Adds a new member to a Space identified by a token.
%% Throws exception when call to the datastore fails, or member/token/space_from_token doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec join({group | user, Id :: binary()}, Macaroon :: macaroon:macaroon()) ->
    {ok, SpaceId :: binary()}.
join({user, UserId}, Macaroon) ->
    {ok, {space, SpaceId}} = token_logic:consume(Macaroon),
    case has_user(SpaceId, UserId) of
        true -> ok;
        false ->
            {ok, _} = space:update(SpaceId, fun(Space) ->
                Privileges = privileges:space_user(),
                #space{users = Users} = Space,
                {ok, Space#space{users = [{UserId, Privileges} | Users]}}
            end),
            {ok, _} = onedata_user:update(UserId, fun(User) ->
                #onedata_user{spaces = USpaces} = User,
                {ok, User#onedata_user{spaces = [SpaceId | USpaces]}}
            end)
    end,
    {ok, SpaceId};
join({group, GroupId}, Macaroon) ->
    {ok, {space, SpaceId}} = token_logic:consume(Macaroon),
    case has_group(SpaceId, GroupId) of
        true -> ok;
        false ->
            Privileges = privileges:space_user(),
            {ok, _} = space:update(SpaceId, fun(Space) ->
                #space{groups = Groups} = Space,
                {ok, Space#space{groups = [{GroupId, Privileges} | Groups]}}
            end),
            {ok, _} = user_group:update(GroupId, fun(Group) ->
                #user_group{spaces = Spaces} = Group,
                {ok, Group#user_group{spaces = [SpaceId | Spaces]}}
            end)
    end,
    {ok, SpaceId}.

%%--------------------------------------------------------------------
%% @doc Adds a new supporting provider to a Space identified by a token.
%% Throws exception when call to the datastore fails, or provider/token/space_from_token doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec support(ProviderId :: binary(), Macaroon :: macaroon:macaroon(),
    SupportedSize :: pos_integer()) ->
    {ok, SpaceId :: binary()}.
support(ProviderId, Macaroon, SupportedSize) ->
    {ok, {space, SpaceId}} = token_logic:consume(Macaroon),
    case has_provider(SpaceId, ProviderId) of
        true -> ok;
        false ->
            {ok, _} = space:update(SpaceId, fun(Space) ->
                #space{size = Size, providers = Providers} = Space,
                {ok, Space#space{
                    size = [{ProviderId, SupportedSize} | Size],
                    providers = [ProviderId | Providers]
                }}
            end),
            add_space_to_providers(SpaceId, [ProviderId])
    end,
    {ok, SpaceId}.


%%--------------------------------------------------------------------
%% @doc Returns details about the Space.
%% Throws exception when call to the datastore fails, or space doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_data(SpaceId :: binary(), Client :: user | provider) ->
    {ok, [proplists:property()]}.
get_data(SpaceId, _Client) ->
    {ok, #document{value = #space{name = Name, size = Size}}} = space:get(SpaceId),
    {ok, [
        {spaceId, SpaceId},
        {name, Name},
        {size, Size}
    ]}.

%%--------------------------------------------------------------------
%% @doc Returns details about Space's users.
%% Throws exception when call to the datastore fails, or space doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_users(SpaceId :: binary()) ->
    {ok, [proplists:property()]}.
get_users(SpaceId) ->
    {ok, #document{value = #space{users = Users}}} = space:get(SpaceId),
    {UserIds, _} = lists:unzip(Users),
    {ok, [{users, UserIds}]}.

%%--------------------------------------------------------------------
%% @doc Returns details about Space's users. The users may belong to the Space
%% directly or indirectly through their groups.
%% Throws exception when call to the datastore fails, or space doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_users(SpaceId :: binary()) ->
    {ok, [proplists:property()]}.
get_effective_users(SpaceId) ->
    {ok, #document{value = #space{users = SpaceUserTuples, groups = GroupTuples}}} = space:get(SpaceId),

    GroupUsersSets = lists:map(fun({GroupId, _}) ->
        {ok, #document{value = #user_group{users = GroupUserTuples}}} = user_group:get(GroupId),
        {GroupUsers, _} = lists:unzip(GroupUserTuples),
        ordsets:from_list(GroupUsers)
    end, GroupTuples),

    {SpaceUsers, _} = lists:unzip(SpaceUserTuples),
    SpaceUsersSet = ordsets:from_list(SpaceUsers),

    AllUserIds = ordsets:union([SpaceUsersSet | GroupUsersSets]),
    {ok, [{users, ordsets:to_list(AllUserIds)}]}.

%%--------------------------------------------------------------------
%% @doc Returns details about Space's groups.
%% Throws exception when call to the datastore fails, or space doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(SpaceId :: binary()) ->
    {ok, [proplists:property()]}.
get_groups(SpaceId) ->
    {ok, #document{value = #space{groups = GroupTuples}}} = space:get(SpaceId),
    {Groups, _} = lists:unzip(GroupTuples),
    {ok, [{groups, Groups}]}.

%%--------------------------------------------------------------------
%% @doc Returns details about Space's providers.
%% Throws exception when call to the datastore fails, or space doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_providers(SpaceId :: binary(), Client :: user | provider) ->
    {ok, [proplists:property()]}.
get_providers(SpaceId, _Client) ->
    {ok, #document{value = #space{providers = Providers}}} = space:get(SpaceId),
    {ok, [{providers, Providers}]}.

%%--------------------------------------------------------------------
%% @doc Returns details about Space's user.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_user(SpaceId :: binary(), Client :: user | provider,
    UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_user(_SpaceId, _Client, UserId) ->
    user_logic:get_data(UserId, provider).

%%--------------------------------------------------------------------
%% @doc Returns details about Space's group.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_group(SpaceId :: binary(), GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_group(_SpaceId, GroupId) ->
    %% @todo: we don't want to give out every bit of data once groups have more data stored
    group_logic:get_data(GroupId).

%%--------------------------------------------------------------------
%% @doc Returns details about Space's provider.
%% Throws exception when call to the datastore fails, or provider doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_provider(SpaceId :: binary(), Client :: user | provider,
    UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_provider(_SpaceId, _Client, ProviderId) ->
    %% @todo: we don't want to give out every bit of data once providers have more data stored
    provider_logic:get_data(ProviderId).

%%--------------------------------------------------------------------
%% @doc Returns list of Space's member privileges.
%% Throws exception when call to the datastore fails, or space doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_privileges(SpaceId :: binary(), {user | group, Id :: binary()}) ->
    {ok, [privileges:space_privilege()]}.
get_privileges(SpaceId, {user, UserId}) ->
    {ok, #document{value = #space{users = Users}}} = space:get(SpaceId),
    {_, Privileges} = lists:keyfind(UserId, 1, Users),
    {ok, Privileges};
get_privileges(SpaceId, {group, GroupId}) ->
    {ok, #document{value = #space{groups = Groups}}} = space:get(SpaceId),
    {_, Privileges} = lists:keyfind(GroupId, 1, Groups),
    {ok, Privileges}.

%%--------------------------------------------------------------------
%% @doc Removes the Space.
%% Throws exception when call to the datastore fails, or space is already removed.
%% @end
%%--------------------------------------------------------------------
-spec remove(SpaceId :: binary()) ->
    true.
remove(SpaceId) ->
    {ok, #document{value = Space}} = space:get(SpaceId),
    #space{users = Users, groups = Groups, providers = Providers} = Space,

    lists:foreach(fun({UserId, _}) ->
        {ok, _} = onedata_user:update(UserId, fun(User) ->
            #onedata_user{spaces = USpaces} = User,
            {ok, User#onedata_user{spaces = lists:delete(SpaceId, USpaces)}}
        end)
    end, Users),

    lists:foreach(fun({GroupId, _}) ->
        {ok, _} = user_group:update(GroupId, fun(Group) ->
            #user_group{spaces = GSpaces} = Group,
            {ok, Group#user_group{spaces = lists:delete(SpaceId, GSpaces)}}
        end)
    end, Groups),

    lists:foreach(fun(ProviderId) ->
        {ok, _} = provider:update(ProviderId, fun(Provider) ->
            #provider{spaces = PSpaces} = Provider,
            {ok, Provider#provider{spaces = lists:delete(SpaceId, PSpaces)}}
        end)
    end, Providers),

    case space:delete(SpaceId) of
        ok -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Removes user from the Space.
%% Throws exception when call to the datastore fails, or space/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(SpaceId :: binary(), UserId :: binary()) ->
    true.
remove_user(SpaceId, UserId) ->
    {ok, _} = onedata_user:update(UserId, fun(User) ->
        #onedata_user{spaces = USpaces} = User,
        {ok, User#onedata_user{spaces = lists:delete(SpaceId, USpaces)}}
    end),
    {ok, _} = space:update(SpaceId, fun(Space) ->
        #space{users = Users} = Space,
        {ok, Space#space{users = lists:keydelete(UserId, 1, Users)}}
    end),
    cleanup(SpaceId),
    true.

%%--------------------------------------------------------------------
%% @doc Removes group from the Space.
%% Throws exception when call to the datastore fails, or space/group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(SpaceId :: binary(), GroupId :: binary()) ->
    true.
remove_group(SpaceId, GroupId) ->
    {ok, _} = user_group:update(GroupId, fun(Group) ->
        #user_group{spaces = Spaces} = Group,
        {ok, Group#user_group{spaces = lists:delete(SpaceId, Spaces)}}
    end),
    {ok, _} = space:update(SpaceId, fun(Space) ->
        #space{groups = Groups} = Space,
        {ok, Space#space{groups = lists:keydelete(GroupId, 1, Groups)}}
    end),
    cleanup(SpaceId),
    true.

%%--------------------------------------------------------------------
%% @doc Removes provider from the Space.
%% Throws exception when call to the datastore fails, or space/provider doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec remove_provider(SpaceId :: binary(), ProviderId :: binary()) ->
    true.
remove_provider(SpaceId, ProviderId) ->
    {ok, _} = provider:update(ProviderId, fun(Provider) ->
        #provider{spaces = Spaces} = Provider,
        {ok, Provider#provider{spaces = lists:delete(SpaceId, Spaces)}}
    end),
    {ok, _} = space:update(SpaceId, fun(Space) ->
        #space{providers = Providers, size = Size} = Space,
        {ok, Space#space{
            providers = lists:delete(ProviderId, Providers),
            size = proplists:delete(ProviderId, Size)
        }}
    end),
    true.

%%--------------------------------------------------------------------
%% @doc Creates a Space for a user or a group, with a preexisting provider.
%% Throws exception when call to the datastore fails, or user/group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_with_provider({user | group, Id :: binary()}, Name :: binary(),
    Providers :: [binary()], Size :: [{Provider :: binary(), ProvidedSize :: pos_integer()}]) ->
    {ok, SpaceId :: binary()}.
create_with_provider({user, UserId}, Name, Providers, Size) ->
    Privileges = privileges:space_admin(),
    Space = #space{name = Name, size = Size, providers = Providers, users = [{UserId, Privileges}]},

    {ok, SpaceId} = space:save(#document{value = Space}),
    {ok, _} = onedata_user:update(UserId, fun(User) ->
        #onedata_user{spaces = USpaces} = User,
        {ok, User#onedata_user{spaces = [SpaceId | USpaces]}}
    end),

    add_space_to_providers(SpaceId, Providers),
    {ok, SpaceId};

create_with_provider({group, GroupId}, Name, Providers, Size) ->
    Privileges = privileges:space_admin(),
    Space = #space{name = Name, size = Size, providers = Providers, groups = [{GroupId, Privileges}]},
    {ok, SpaceId} = space:save(#document{value = Space}),

    {ok, _} = user_group:update(GroupId, fun(Group) ->
        #user_group{spaces = Spaces} = Group,
        {ok, Group#user_group{spaces = [SpaceId | Spaces]}}
    end),

    add_space_to_providers(SpaceId, Providers),
    {ok, SpaceId}.


%%--------------------------------------------------------------------
%% @doc Adds a space to providers' support list.
%% @end
%%--------------------------------------------------------------------
-spec add_space_to_providers(SpaceId :: binary(), ProviderIds :: [binary()])
        -> ok.
add_space_to_providers(_SpaceId, []) -> ok;
add_space_to_providers(SpaceId, [ProviderId | RestProviders]) ->
    {ok, _} = provider:update(ProviderId, fun(Provider) ->
        #provider{spaces = Spaces} = Provider,
        {ok, Provider#provider{spaces = [SpaceId | Spaces]}}
    end),
    add_space_to_providers(SpaceId, RestProviders).


%%--------------------------------------------------------------------
%% @doc Removes the space if empty.
%% Throws exception when call to the datastore fails, or space is already removed.
%% @end
%%--------------------------------------------------------------------
-spec cleanup(SpaceId :: binary()) -> boolean() | no_return().
cleanup(SpaceId) ->
    {ok, #document{value = #space{groups = Groups, users = Users}}} = space:get(SpaceId),
    case {Groups, Users} of
        {[], []} -> remove(SpaceId);
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Retrieves effective user privileges taking into account any groups
%% he is a member of that also are members of the Space.
%% Throws exception when call to the datastore fails, or space/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_privileges(SpaceId :: binary(), UserId :: binary()) ->
    {ok, ordsets:ordset(privileges:space_privilege())}.
get_effective_privileges(SpaceId, UserId) ->
    {ok, #document{value = #onedata_user{groups = UGroups}}} = onedata_user:get(UserId),
    {ok, #document{value = #space{users = UserTuples, groups = SGroupTuples}}} = space:get(SpaceId),

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

%%--------------------------------------------------------------------
%% @doc Transforms a space to include new privileges.
%%--------------------------------------------------------------------
-spec set_privileges_aux(Space :: space_info(), {user | group, Id :: binary()},
    Privileges :: [privileges:space_privilege()]) ->
    space_info().
set_privileges_aux(#space{users = Users} = Space, {user, UserId}, Privileges) ->
    UsersNew = lists:keyreplace(UserId, 1, Users, {UserId, Privileges}),
    Space#space{users = UsersNew};
set_privileges_aux(#space{groups = Groups} = Space, {group, GroupId}, Privileges) ->
    GroupsNew = lists:keyreplace(GroupId, 1, Groups, {GroupId, Privileges}),
    Space#space{groups = GroupsNew}.
