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
-export([remove/1, remove_user/2, remove_group/2, remove_provider/2]).


%% exists/1
%% ====================================================================
%% @doc Returns whether a Space exists.
%% ====================================================================
-spec exists(SpaceId :: binary()) -> boolean().
%% ====================================================================
exists(SpaceId) ->
    SSpaceId = binary:bin_to_list(SpaceId),
    {ok, Exists} = dao_lib:apply(dao_spaces, exist_space, [SSpaceId], 1),
    Exists.


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
            #veil_document{record = #space{providers = Providers}} = get_doc(SpaceId),
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
            #veil_document{record = #space{users = Users}} = get_doc(SpaceId),
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
            #veil_document{record = #space{groups = Groups}} = get_doc(SpaceId),
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
    case exists(SpaceId) of
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
    Doc = get_doc(SpaceId),
    #veil_document{record = Space} = Doc,
    DocNew = Doc#veil_document{record = Space#space{name = Name}},
    {ok, _} = dao_lib:apply(dao_spaces, save_space, [DocNew], 1),
    ok.


%% set_privileges/3
%% ====================================================================
%% @doc Sets privileges for a member of the Space.
%% ====================================================================
-spec set_privileges(SpaceId :: binary(), {user | group, Id :: binary()},
                     Privileges :: [privileges:space_privilege()]) ->
    ok | no_return().
%% ====================================================================
set_privileges(SpaceId, {MemberType, MemberId}, Privileges) ->
    SetPrivileges = ordsets:from_list(Privileges),
    Doc = get_doc(SpaceId),
    Space = Doc#veil_document.record,
    Members = case MemberType of
        user -> Space#space.users;
        group -> Space#space.groups
    end,
    Members2 = lists:keyreplace(MemberId, 1, Members, {MemberId, ordsets:to_list(SetPrivileges)}),
    Space2 = case MemberType of
        user -> Space#space{users = Members2};
        group -> Space#space{groups = Members2}
    end,
    Doc2 = Doc#veil_document{record = Space2},
    {ok, _} = dao_lib:apply(dao_spaces, save_space, [Doc2], 1),
    ok.


%% join/2
%% ====================================================================
%% @doc Adds a new member to a Space identified by a token.
%% ====================================================================
-spec join({group | user, Id :: binary()}, Token :: binary()) ->
    {ok, SpaceId :: binary()} | no_return().
%% ====================================================================
join({user, UserId}, Token) ->
    SUserId = binary:bin_to_list(UserId),
    {ok, {space, SpaceId}} = token_logic:consume(Token, space_invite_user_token),
    {ok, UserDoc} = dao_lib:apply(dao_users, get_user, [SUserId], 1),
    SpaceDoc = get_doc(SpaceId),

    #veil_document{record = #space{users = Users} = Space} = SpaceDoc,
    SpaceNew = Space#space{users = [{UserId, privileges:space_user()} | Users]},
    SpaceDocNew = SpaceDoc#veil_document{record = SpaceNew},

    #veil_document{record = #user{spaces = Spaces} = User} = UserDoc,
    UserNew = User#user{spaces = [SpaceId | Spaces]},
    UserDocNew = UserDoc#veil_document{record = UserNew},

    {ok, _} = dao_lib:apply(dao_spaces, save_space, [SpaceDocNew], 1),
    try
        {ok, _} = dao_lib:apply(dao_users, save_user, [UserDocNew], 1)
    catch
        error:Error ->
            dao_lib:apply(dao_spaces, save_space, [SpaceDoc], 1),
            dao_lib:apply(dao_users, save_space, [UserDoc], 1),
            error(Error)
    end,

    {ok, SpaceId};
join({group, GroupId}, Token) ->
    SGroupId = binary:bin_to_list(GroupId),
    {ok, {space, SpaceId}} = token_logic:consume(Token, space_invite_group_token),
    {ok, GroupDoc} = dao_lib:apply(dao_groups, get_group, [SGroupId], 1),
    SpaceDoc = get_doc(SpaceId),

    #veil_document{record = #space{groups = Groups} = Space} = SpaceDoc,
    SpaceNew = Space#space{groups = [{GroupId, privileges:space_user()} | Groups]},
    SpaceDocNew = SpaceDoc#veil_document{record = SpaceNew},

    #veil_document{record = #user_group{spaces = Spaces} = Group} = GroupDoc,
    GroupNew = Group#user_group{spaces = [SpaceId | Spaces]},
    GroupDocNew = GroupDoc#veil_document{record = GroupNew},

    {ok, _} = dao_lib:apply(dao_spaces, save_space, [SpaceDocNew], 1),
    try
        {ok, _} = dao_lib:apply(dao_groups, save_group, [GroupDocNew], 1)
    catch
        Type:Error ->
            dao_lib:apply(dao_spaces, save_space, [SpaceDoc], 1),
            dao_lib:apply(dao_groups, save_group, [GroupDoc], 1),
            error({Type, Error})
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
    SProviderId = binary:bin_to_list(ProviderId),
    {ok, {space, SpaceId}} = token_logic:consume(Token, space_support_token),
    {ok, ProviderDoc} = dao_lib:apply(dao_providers, get_provider, [SProviderId], 1),
    SpaceDoc = get_doc(SpaceId),

    #veil_document{record = #space{providers = Providers} = Space} = SpaceDoc,
    SpaceNew = Space#space{providers = [ProviderId | Providers]},
    SpaceDocNew = SpaceDoc#veil_document{record = SpaceNew},

    #veil_document{record = #provider{spaces = Spaces} = Provider} = ProviderDoc,
    ProviderNew = Provider#provider{spaces = [SpaceId | Spaces]},
    ProviderDocNew = ProviderDoc#veil_document{record = ProviderNew},

    {ok, _} = dao_lib:apply(dao_spaces, save_space, [SpaceDocNew], 1),
    try
        {ok, _} = dao_lib:apply(dao_providers, save_provider, [ProviderDocNew], 1)
    catch
        Type:Error ->
            dao_lib:apply(dao_spaces, save_space, [SpaceDoc], 1),
            dao_lib:apply(dao_providers, save_provider, [ProviderDoc], 1),
            error({Type, Error})
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
    #veil_document{record = #space{name = Name}} = get_doc(SpaceId),
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
    #veil_document{record = #space{users = Users}} = get_doc(SpaceId),
    {UserIds, _} = lists:unzip(Users),
    {ok, [{users, UserIds}]};
get_users(SpaceId, provider) ->
    #veil_document{record = #space{users = SpaceUsers, groups = Groups}} = get_doc(SpaceId),
    GrUIDs = lists:map(fun({GroupId, _}) ->
        SGroupId = binary:bin_to_list(GroupId),
        {ok, GroupDoc} = dao_lib:apply(dao_groups, get_group, [SGroupId], 1),
        #veil_document{record = #user_group{users = GroupUsers}} = GroupDoc,
        {Ids, _} = lists:unzip(GroupUsers),
        ordsets:from_list(Ids)
    end, Groups),
    {SpUserIds, _} = lists:unzip(SpaceUsers),

    SpaceUserIds = ordsets:from_list(SpUserIds),
    GroupUserIds = ordsets:union(GrUIDs),
    AllUserIds = ordsets:union(SpaceUserIds, GroupUserIds),

    {ok, [{users, ordsets:to_list(AllUserIds)}]}.


%% get_groups/1
%% ====================================================================
%% @doc Returns details about Space's groups.
%% ====================================================================
-spec get_groups(SpaceId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_groups(SpaceId) ->
    #veil_document{record = #space{groups = Groups}} = get_doc(SpaceId),
    {GroupIds, _} = lists:unzip(Groups),
    {ok, [{groups, GroupIds}]}.


%% get_providers/2
%% ====================================================================
%% @doc Returns details about Space's providers.
%% ====================================================================
-spec get_providers(SpaceId :: binary(), Client :: user | provider) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_providers(SpaceId, _Client) ->
    #veil_document{record = #space{providers = Providers}} = get_doc(SpaceId),
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
    #veil_document{record = #space{users = Users}} = get_doc(SpaceId),
    {_, Privileges} = lists:keyfind(UserId, 1, Users),
    {ok, Privileges};
get_privileges(SpaceId, {group, GroupId}) ->
    #veil_document{record = #space{groups = Groups}} = get_doc(SpaceId),
    {_, Privileges} = lists:keyfind(GroupId, 1, Groups),
    {ok, Privileges}.


%% remove/1
%% ====================================================================
%% @doc Removes the Space. Should return true if after the call the Space
%% doesn't exist; in particular if it never existed at all.
%% @end
%% ====================================================================
-spec remove(SpaceId :: binary()) -> boolean().
%% ====================================================================
remove(SpaceId) ->
    case exists(SpaceId) of
        false -> true;
        true ->
            SSpaceId = binary:bin_to_list(SpaceId),
            ok = dao_lib:apply(dao_spaces, remove_space, [SSpaceId], 1),
            true
    end.


%% remove_user/2
%% ====================================================================
%% @doc Removes user from the Space. Should return true if after the call the
%% user will no longer be a member of the Space; in particular if he never
%% was a member or the Space didn't exist.
%% @end
%% ====================================================================
-spec remove_user(SpaceId :: binary(), UserId :: binary()) -> boolean().
%% ====================================================================
remove_user(SpaceId, UserId) ->
    SUserId = binary:bin_to_list(UserId),
    SpaceDoc = get_doc(SpaceId),
    {ok, UserDoc} = dao_lib:apply(dao_users, get_user, [SUserId], 1),

    #veil_document{record = #space{users = Users} = Space} = SpaceDoc,
    SpaceNew = Space#space{users = lists:keydelete(UserId, 1, Users)},
    SpaceDocNew = SpaceDoc#veil_document{record = SpaceNew},

    #veil_document{record = #user{spaces = Spaces} = User} = UserDoc,
    UserNew = User#user{spaces = lists:delete(SpaceId, Spaces)},
    UserNewDoc = UserDoc#veil_document{record = UserNew},

    {ok, _} = dao_lib:apply(dao_spaces, save_space, [SpaceDocNew], 1),
    try
        {ok, _} = dao_lib:apply(dao_users, save_user, [UserNewDoc], 1)
    catch
        Type:Error ->
            dao_lib:apply(dao_spaces, save_space, [SpaceDoc], 1),
            dao_lib:apply(dao_users, save_user, [UserNew], 1),
            error({Type, Error})
    end,

    true.


%% remove_group/2
%% ====================================================================
%% @doc Removes group from the Space. Should return true if after the call the
%% group will no longer be a member of the Space; in particular if it never
%% was a member or the Space didn't exist.
%% @end
%% ====================================================================
-spec remove_group(SpaceId :: binary(), GroupId :: binary()) -> boolean().
%% ====================================================================
remove_group(SpaceId, GroupId) ->
    SGroupId = binary:bin_to_list(GroupId),
    SpaceDoc = get_doc(SpaceId),
    {ok, GroupDoc} = dao_lib:apply(dao_groups, get_group, [SGroupId], 1),

    #veil_document{record = #space{groups = Groups} = Space} = SpaceDoc,
    SpaceNew = Space#space{groups = lists:keydelete(GroupId, 1, Groups)},
    SpaceDocNew = SpaceDoc#veil_document{record = SpaceNew},

    #veil_document{record = #user_group{spaces = Spaces} = Group} = GroupDoc,
    GroupNew = Group#user_group{spaces = lists:delete(SpaceId, Spaces)},
    GroupNewDoc = GroupDoc#veil_document{record = GroupNew},

    {ok, _} = dao_lib:apply(dao_spaces, save_space, [SpaceDocNew], 1),
    try
        {ok, _} = dao_lib:apply(dao_groups, save_group, [GroupNewDoc], 1)
    catch
        Type:Error ->
            dao_lib:apply(dao_spaces, save_space, [SpaceDoc], 1),
            dao_lib:apply(dao_groups, save_group, [GroupNew], 1),
            error({Type, Error})
    end,

    true.


%% remove_provider/2
%% ====================================================================
%% @doc Removes provider from the Space. Should return true if after the call
%% the provider will no longer support the Space; in particular if he never
%% supported it or the Space didn't exist.
%% @end
%% ====================================================================
-spec remove_provider(SpaceId :: binary(), ProviderId :: binary()) -> boolean().
%% ====================================================================
remove_provider(SpaceId, ProviderId) ->
    SProviderId = binary:bin_to_list(ProviderId),
    SpaceDoc = get_doc(SpaceId),
    {ok, ProviderDoc} = dao_lib:apply(dao_providers, get_provider, [SProviderId], 1),

    #veil_document{record = #space{providers = Providers} = Space} = SpaceDoc,
    SpaceNew = Space#space{providers = lists:delete(ProviderId, Providers)},
    SpaceDocNew = SpaceDoc#veil_document{record = SpaceNew},

    #veil_document{record = #provider{spaces = Spaces} = Provider} = ProviderDoc,
    ProviderNew = Provider#provider{spaces = lists:delete(SpaceId, Spaces)},
    ProviderNewDoc = ProviderDoc#veil_document{record = ProviderNew},

    {ok, _} = dao_lib:apply(dao_spaces, save_space, [SpaceDocNew], 1),
    try
        {ok, _} = dao_lib:apply(dao_providers, save_provider, [ProviderNewDoc], 1)
    catch
        Type:Error ->
            dao_lib:apply(dao_spaces, save_space, [SpaceDoc], 1),
            dao_lib:apply(dao_providers, save_provider, [ProviderNew], 1),
            error({Type, Error})
    end,

    true.


%% create_with_provider/3
%% ====================================================================
%% @doc Creates a Space for a user or a group, with a preexisting provider.
%% ====================================================================
-spec create_with_provider({user | group, Id :: binary()}, Name :: binary(),
                           Providers :: [binary()]) ->
    {ok, SpaceId :: binary()} | no_return().
%% ====================================================================
create_with_provider(Member, Name, Providers) ->
    Privileges = privileges:space_admin(),
    Space = #space{name = Name, providers = Providers},
    Space2 = case Member of
        {user, UserId} -> Space#space{users = [{UserId, Privileges}]};
        {group, GroupId} -> Space#space{groups = [{GroupId, Privileges}]}
    end,
    {ok, SSpaceId} = dao_lib:apply(dao_spaces, save_space, [Space2], 1),
    {ok, <<SSpaceId>>}.


%% get_doc/1
%% ====================================================================
%% @doc Retrieves a space from the database.
%% ====================================================================
-spec get_doc(SpaceId :: binary) -> space_doc() | no_return().
%% ====================================================================
get_doc(SpaceId) ->
    SSpaceId = binary:bin_to_list(SpaceId),
    {ok, #veil_document{record = #space{}} = Doc} = dao_lib:apply(dao_spaces, get_space, [SSpaceId], 1),
    Doc.


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
    SUserId = binary:bin_to_list(UserId),
    {ok, #user{groups = UGroups}} = dao_lib:apply(dao_users, get_user, [SUserId], 1),
    #veil_document{record = #space{users = UserTuples, groups = SGroupTuples}} = get_doc(SpaceId),

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
