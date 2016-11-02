%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module implementing the business logic for handles in the registry.
%%% This module serves as a buffer between the database and the REST API.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_logic).
-author("Tomasz Lichon").

-include("datastore/oz_datastore_models_def.hrl").

%% API
-export([exists/1, has_user/2, has_effective_user/2, has_group/2, has_effective_privilege/3]).
-export([create/5, modify/4, set_user_privileges/3, set_group_privileges/3]).
-export([get_data/1, get_public_data/1, get_users/1, get_effective_users/1,
    get_groups/1, get_user_privileges/2, get_group_privileges/2,
    get_effective_user_privileges/2, get_metadata/1, list/0]).
-export([add_user/2, add_group/2]).
-export([remove/1, remove_user/2, remove_group/2, cleanup/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns whether a handle exists.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec exists(HandleId :: od_handle:id()) ->
    boolean().
exists(HandleId) ->
    od_handle:exists(HandleId).

%%--------------------------------------------------------------------
%% @doc Returns whether the user identified by UserId is a member of the handle.
%% Shall return false in any other case (handle doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_user(HandleId :: od_handle:id(), UserId :: od_user:id()) ->
    boolean().
has_user(HandleId, UserId) ->
    case od_handle:get(HandleId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #od_handle{users = Users}}} ->
            lists:keymember(UserId, 1, Users)
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the user identified by UserId is a member of the handle,
%% either directly or through a group.
%% Shall return false in any other case (handle doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_effective_user(HandleId :: od_handle:id(), UserId :: od_user:id()) ->
    boolean().
has_effective_user(HandleId, UserId) ->
    case od_handle:get(HandleId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #od_handle{users = Users, groups = HandleGroups}}} ->
            case lists:keymember(UserId, 1, Users) of
                true ->
                    true;
                false ->
                    case od_user:get(UserId) of
                        {error, {not_found, _}} ->
                            false;
                        {ok, #document{value = #od_user{groups = UserGroups}}} ->
                            HandleGroupsSet = ordsets:from_list([GroupId || {GroupId, _} <- HandleGroups]),
                            UserGroupsSet = ordsets:from_list(UserGroups),
                            not ordsets:is_disjoint(HandleGroupsSet, UserGroupsSet)
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the group identified by GroupId is a member of the handle.
%% Shall return false in any other case (handle doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_group(HandleId :: od_handle:id(), GroupId :: od_group:id()) ->
    boolean().
has_group(HandleId, GroupId) ->
    case od_handle:get(HandleId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #od_handle{groups = Groups}}} ->
            lists:keymember(GroupId, 1, Groups)
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the handle's user identified by UserId has privilege
%% in the handle. Shall return false in any other case (handle doesn't exist,
%% user is not handle's member, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_effective_privilege(HandleId :: od_handle:id(), UserId :: od_user:id(),
    Privilege :: privileges:handle_privilege()) ->
    boolean().
has_effective_privilege(HandleId, UserId, Privilege) ->
    case has_effective_user(HandleId, UserId) of
        false -> false;
        true ->
            {ok, UserPrivileges} = get_effective_user_privileges(HandleId, UserId),
            ordsets:is_element(Privilege, UserPrivileges)
    end.

%%--------------------------------------------------------------------
%% @doc Creates a handle for a user.
%% Throws exception when call to the datastore fails, or token/member_from_token doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create(od_user:id(), od_handle_service:id(), od_handle:resource_type(),
    od_handle:resource_id(), od_handle:metadata()) ->
    {ok, od_handle:id()}.
create(UserId, HandleServiceId, ResourceType, ResourceId, Metadata) ->
    {ok, PublicHandle} = handle_proxy:register_handle(HandleServiceId, ResourceType, ResourceId, Metadata),
    Privileges = privileges:handle_admin(),
    Handle = #od_handle{handle_service = HandleServiceId, resource_type = ResourceType,
        resource_id = ResourceId, public_handle = PublicHandle, metadata = Metadata,
        users = [{UserId, Privileges}]},

    {ok, HandleId} = od_handle:create(#document{value = Handle}),
    {ok, _} = od_user:update(UserId, fun(User = #od_user{handles = UHandles}) ->
        {ok, User#od_user{handles = [HandleId | UHandles]}}
    end),

    case ResourceType of
        <<"Share">> ->
            {ok, _} = od_share:update(ResourceId, fun(Share = #od_share{}) ->
                {ok, Share#od_share{handle = HandleId}}
            end);
        _ ->
            ok
    end,

    {ok, HandleId}.

%%--------------------------------------------------------------------
%% @doc Modifies handle's data.
%% Throws exception when call to the datastore fails, or handle doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec modify(od_handle:id(), od_handle:resource_type(), od_handle:resource_id(), od_handle:metadata()) -> ok.
modify(_HandleId, undefined, undefined, undefined) ->
    ok;
modify(HandleId, NewResourceType, NewResourceId, NewMetadata) ->
    ok = handle_proxy:modify_handle(HandleId, NewResourceType, NewResourceId, NewMetadata),
    {ok, _} = od_handle:update(HandleId,
        fun(Handle = #od_handle{resource_type = ResourceType, resource_id = ResourceId, metadata = Metadata}) ->
            FinalResourceType = utils:ensure_defined(NewResourceType, undefined, ResourceType),
            FinalResourceId = utils:ensure_defined(NewResourceId, undefined, ResourceId),
            FinalMetadata = utils:ensure_defined(NewMetadata, undefined, Metadata),
            {ok, Handle#od_handle{resource_type = FinalResourceType, resource_id = FinalResourceId,
                metadata = FinalMetadata, timestamp = od_handle:actual_timestamp()}}
        end),
    ok.

%%--------------------------------------------------------------------
%% @doc Sets privileges for an user member of the handle.
%% Throws exception when call to the datastore fails, or handle doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_user_privileges(HandleId :: od_handle:id(), UserId :: od_user:id(),
    Privileges :: [privileges:handle_privilege()]) ->
    ok.
set_user_privileges(HandleId, UserId, Privileges) ->
    {ok, _} = od_handle:update(HandleId,
        fun(Handle = #od_handle{users = Users}) ->
            PrivilegesNew = ordsets:from_list(Privileges),
            UsersNew = lists:keyreplace(UserId, 1, Users, {UserId, PrivilegesNew}),
            {ok, Handle#od_handle{users = UsersNew}}
        end),
    ok.

%%--------------------------------------------------------------------
%% @doc Sets privileges for a group member of the handle.
%% Throws exception when call to the datastore fails, or handle doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_group_privileges(HandleId :: od_handle:id(), GroupId :: od_group:id(),
    Privileges :: [privileges:handle_privilege()]) ->
    ok.
set_group_privileges(HandleId, GroupId, Privileges) ->
    {ok, _} = od_handle:update(HandleId,
        fun(Handle = #od_handle{groups = Groups}) ->
            PrivilegesNew = ordsets:from_list(Privileges),
            GroupsNew = lists:keyreplace(GroupId, 1, Groups, {GroupId, PrivilegesNew}),
            {ok, Handle#od_handle{groups = GroupsNew}}
        end),
    ok.

%%--------------------------------------------------------------------
%% @doc Adds a new user to a handle.
%% @end
%%--------------------------------------------------------------------
-spec add_user(HandleId :: od_handle:id(), UserId :: od_user:id()) ->
    {ok, HandleId :: od_handle:id()}.
add_user(HandleId, UserId) ->
    case has_user(HandleId, UserId) of
        true -> ok;
        false ->
            {ok, _} = od_handle:update(HandleId, fun(Handle) ->
                Privileges = privileges:handle_user(),
                #od_handle{users = Users} = Handle,
                {ok, Handle#od_handle{users = [{UserId, Privileges} | Users]}}
            end),
            {ok, _} = od_user:update(UserId, fun(User) ->
                #od_user{handles = UHandles} = User,
                {ok, User#od_user{handles = [HandleId | UHandles]}}
            end)
    end,
    {ok, HandleId}.

%%--------------------------------------------------------------------
%% @doc Adds a new group to a handle.
%% @end
%%--------------------------------------------------------------------
-spec add_group(HandleId :: od_handle:id(), GroupId :: od_group:id()) ->
    {ok, HandleId :: od_handle:id()}.
add_group(HandleId, GroupId) ->
    case has_group(HandleId, GroupId) of
        true -> ok;
        false ->
            {ok, _} = od_handle:update(HandleId, fun(Handle) ->
                Privileges = privileges:handle_user(),
                #od_handle{groups = Groups} = Handle,
                {ok, Handle#od_handle{groups = [{GroupId, Privileges} | Groups]}}
            end),
            {ok, _} = od_group:update(GroupId, fun(Group) ->
                #od_group{handles = Handles} = Group,
                {ok, Group#od_group{handles = [HandleId | Handles]}}
            end)
    end,
    {ok, HandleId}.


%%--------------------------------------------------------------------
%% @doc Returns details about the handle.
%% Throws exception when call to the datastore fails, or handle doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_data(HandleId :: od_handle:id()) -> {ok, [proplists:property()]}.
get_data(HandleId) ->
    {ok, #document{value = #od_handle{handle_service = HandleService, public_handle = Handle,
        resource_type = ResourceType, resource_id = ResourceId, metadata = Metadata,
        timestamp = Timestamp}}} =
        od_handle:get(HandleId),
    {ok, [
        {handleId, HandleId},
        {handleServiceId, HandleService},
        {handle, Handle},
        {resourceType, ResourceType},
        {resourceId, ResourceId},
        {metadata, Metadata},
        {timestamp, timestamp_utils:datetime_to_datestamp(Timestamp)}
    ]}.


%%--------------------------------------------------------------------
%% @doc Returns details about the handle that are publicly available.
%% Throws exception when call to the datastore fails, or handle doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_public_data(HandleId :: od_handle:id()) -> {ok, [proplists:property()]}.
get_public_data(HandleId) ->
    {ok, #document{value = #od_handle{
        public_handle = Handle,
        metadata = Metadata
    }}} = od_handle:get(HandleId),
    {ok, [
        {handleId, HandleId},
        {handle, Handle},
        {metadata, Metadata}
    ]}.


%%--------------------------------------------------------------------
%% @doc Returns handle metadata and timestamp
%% Throws exception when call to the datastore fails, or handle doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_metadata(HandleId :: od_handle:id()) -> {ok, [proplists:property()]}.
get_metadata(HandleId) ->
    {ok, #document{value = #od_handle{metadata = Metadata, timestamp = Timestamp}}} =
        od_handle:get(HandleId),
    {ok, [
        {metadata, Metadata},
        {timestamp, Timestamp}
    ]}.

%%--------------------------------------------------------------------
%% @doc Returns a list of all handles (their ids).
%%--------------------------------------------------------------------
-spec list() -> {ok, [binary()]}.
list() ->
    {ok, HandleDocs} = od_handle:list(),
    HandleIds = lists:map(fun(#document{key = HandleId}) ->
        HandleId
    end, HandleDocs),
    {ok, HandleIds}.

%%--------------------------------------------------------------------
%% @doc Returns details about handle's users.
%% Throws exception when call to the datastore fails, or handle doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_users(HandleId :: od_handle:id()) ->
    {ok, [proplists:property()]}.
get_users(HandleId) ->
    {ok, #document{value = #od_handle{users = Users}}} = od_handle:get(HandleId),
    {UserIds, _} = lists:unzip(Users),
    {ok, [{users, UserIds}]}.


%%--------------------------------------------------------------------
%% @doc Returns a list of all effective users of a handle_service.
%% Throws exception when call to the datastore fails, or handle_service doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_users(HandleId :: od_handle:id()) ->
    {ok, [proplists:property()]}.
get_effective_users(HandleId) ->
    {ok, #document{
        value = #od_handle{
            users = UserPerms,
            groups = GroupPerms
        }}} = od_handle:get(HandleId),
    {Users, _} = lists:unzip(UserPerms),
    UsersViaGroups = lists:foldl(
        fun({GroupId, _}, Acc) ->
            {ok, [{users, GroupUsers}]} = group_logic:get_effective_users(
                GroupId
            ),
            GroupUsers ++ Acc
        end, [], GroupPerms),
    {ok, [{users, ordsets:union(Users, UsersViaGroups)}]}.


%%--------------------------------------------------------------------
%% @doc Returns details about handle's groups.
%% Throws exception when call to the datastore fails, or handle doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(HandleId :: od_handle:id()) ->
    {ok, [proplists:property()]}.
get_groups(HandleId) ->
    {ok, #document{value = #od_handle{groups = GroupTuples}}} = od_handle:get(HandleId),
    {Groups, _} = lists:unzip(GroupTuples),
    {ok, [{groups, Groups}]}.

%%--------------------------------------------------------------------
%% @doc Returns list of handle's member privileges.
%% Throws exception when call to the datastore fails, or handle doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(HandleId :: od_handle:id(), UserId :: od_user:id()) ->
    {ok, [{privileges, [privileges:handle_privilege()]}]}.
get_user_privileges(HandleId, UserId) ->
    {ok, #document{value = #od_handle{users = Users}}} = od_handle:get(HandleId),
    {_, Privileges} = lists:keyfind(UserId, 1, Users),
    {ok, [{privileges, Privileges}]}.

%%--------------------------------------------------------------------
%% @doc Returns list of handle's member privileges.
%% Throws exception when call to the datastore fails, or handle doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(HandleId :: od_handle:id(), GroupId :: od_group:id()) ->
    {ok, [{privileges, [privileges:handle_privilege()]}]}.
get_group_privileges(HandleId, GroupId) ->
    {ok, #document{value = #od_handle{groups = Groups}}} = od_handle:get(HandleId),
    {_, Privileges} = lists:keyfind(GroupId, 1, Groups),
    {ok, [{privileges, Privileges}]}.

%%--------------------------------------------------------------------
%% @doc Removes the handle.
%% Throws exception when call to the datastore fails, or handle is already removed.
%% @end
%%--------------------------------------------------------------------
-spec remove(HandleId :: od_handle:id()) -> boolean().
remove(HandleId) ->
    ok = handle_proxy:unregister_handle(HandleId),
    {ok, #document{value = Handle}} = od_handle:get(HandleId),
    #od_handle{
        users = Users,
        groups = Groups,
        resource_id = ResourceId,
        resource_type = ResourceType
    } = Handle,

    lists:foreach(fun({UserId, _}) ->
        {ok, _} = od_user:update(UserId, fun(User) ->
            #od_user{handles = UHandles} = User,
            {ok, User#od_user{
                handles = lists:delete(HandleId, UHandles)
            }}
        end)
    end, Users),

    lists:foreach(fun({GroupId, _}) ->
        {ok, _} = od_group:update(GroupId, fun(Group) ->
            #od_group{handles = GHandles} = Group,
            {ok, Group#od_group{handles = lists:delete(HandleId, GHandles)}}
        end)
    end, Groups),

    case ResourceType of
        <<"Share">> ->
            od_share:update(ResourceId, fun(Share = #od_share{}) ->
                {ok, Share#od_share{handle = undefined}}
            end);
        _ ->
            ok
    end,

    case od_handle:delete(HandleId) of
        ok -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Removes user from the handle.
%% Throws exception when call to the datastore fails, or handle/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(HandleId :: od_handle:id(), UserId :: od_user:id()) ->
    true.
remove_user(HandleId, UserId) ->
    {ok, _} = od_user:update(UserId, fun(User) ->
        #od_user{handles = UHandles} = User,
        {ok, User#od_user{handles = lists:delete(HandleId, UHandles)}}
    end),
    {ok, _} = od_handle:update(HandleId, fun(Handle) ->
        #od_handle{users = Users} = Handle,
        {ok, Handle#od_handle{users = lists:keydelete(UserId, 1, Users)}}
    end),
    cleanup(HandleId),
    true.

%%--------------------------------------------------------------------
%% @doc Removes group from the handle.
%% Throws exception when call to the datastore fails, or handle/group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(HandleId :: od_handle:id(), GroupId :: od_group:id()) ->
    true.
remove_group(HandleId, GroupId) ->
    {ok, _} = od_group:update(GroupId, fun(Group) ->
        #od_group{handles = Handles} = Group,
        {ok, Group#od_group{handles = lists:delete(HandleId, Handles)}}
    end),
    {ok, _} = od_handle:update(HandleId, fun(Handle) ->
        #od_handle{groups = Groups} = Handle,
        {ok, Handle#od_handle{groups = lists:keydelete(GroupId, 1, Groups)}}
    end),

    cleanup(HandleId),
    true.

%%--------------------------------------------------------------------
%% @doc Removes the handle if empty.
%% Throws exception when call to the datastore fails, or handle is already removed.
%% @end
%%--------------------------------------------------------------------
-spec cleanup(HandleId :: od_handle:id()) -> boolean() | no_return().
cleanup(_HandleId) ->
%% Currently, handle with no users and groups are not deleted so it is
%% possible to restore it after accidentally leaving a handle.
%%    {ok, #document{value = #handle{groups = Groups, users = Users}}} = handle:get(HandleId),
%%    case {Groups, Users} of
%%        {[], []} -> remove(HandleId);
%%        _ -> false
%%    end.
    false.

%%--------------------------------------------------------------------
%% @doc Retrieves effective user privileges taking into account any groups
%% he is a member of that also are members of the handle.
%% Throws exception when call to the datastore fails, or handle/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_user_privileges(HandleId :: od_handle:id(), UserId :: od_user:id()) ->
    {ok, ordsets:ordset(privileges:handle_privilege())}.
get_effective_user_privileges(HandleId, UserId) ->
    {ok, #document{value = #od_user{groups = UGroups}}} = od_user:get(UserId),
    {ok, #document{value = #od_handle{users = UserTuples, groups = SGroupTuples}}} = od_handle:get(HandleId),

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

%%%===================================================================
%%% Internal functions
%%%===================================================================