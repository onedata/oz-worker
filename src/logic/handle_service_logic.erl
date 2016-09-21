%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module implementing the business logic for handle_services in the registry.
%%% This module serves as a buffer between the database and the REST API.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_service_logic).
-author("Tomasz Lichon").

-include("datastore/oz_datastore_models_def.hrl").

%% API
-export([exists/1, has_user/2, has_effective_user/2, has_group/2, has_effective_privilege/3]).
-export([create/4, modify/4, set_user_privileges/3, set_group_privileges/3]).
-export([get_data/1, get_users/1, get_groups/1, get_user_privileges/2, get_group_privileges/2, get_effective_user_privileges/2]).
-export([add_user/2, add_group/2]).
-export([remove/1, remove_user/2, remove_group/2, cleanup/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns whether a handle_service exists.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec exists(HandleServiceId :: handle_service:id()) ->
    boolean().
exists(HandleServiceId) ->
    handle_service:exists(HandleServiceId).

%%--------------------------------------------------------------------
%% @doc Returns whether the user identified by UserId is a member of the handle_service.
%% Shall return false in any other case (handle_service doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_user(HandleServiceId :: handle_service:id(), UserId :: onedata_user:id()) ->
    boolean().
has_user(HandleServiceId, UserId) ->
    case handle_service:get(HandleServiceId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #handle_service{users = Users}}} ->
            lists:keymember(UserId, 1, Users)
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the user identified by UserId is a member of the handle_service,
%% either directly or through a group.
%% Shall return false in any other case (handle_service doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_effective_user(HandleServiceId :: handle_service:id(), UserId :: onedata_user:id()) ->
    boolean().
has_effective_user(HandleServiceId, UserId) ->
    case handle_service:get(HandleServiceId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #handle_service{users = Users, groups = HandleServiceGroups}}} ->
            case lists:keymember(UserId, 1, Users) of
                true ->
                    true;
                false ->
                    case onedata_user:get(UserId) of
                        {error, {not_found, _}} ->
                            false;
                        {ok, #document{value = #onedata_user{groups = UserGroups}}} ->
                            HandleServiceGroupsSet = ordsets:from_list([GroupId || {GroupId, _} <- HandleServiceGroups]),
                            UserGroupsSet = ordsets:from_list(UserGroups),
                            not ordsets:is_disjoint(HandleServiceGroupsSet, UserGroupsSet)
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the group identified by GroupId is a member of the handle_service.
%% Shall return false in any other case (handle_service doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_group(HandleServiceId :: handle_service:id(), GroupId :: user_group:id()) ->
    boolean().
has_group(HandleServiceId, GroupId) ->
    case handle_service:get(HandleServiceId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #handle_service{groups = Groups}}} ->
            lists:keymember(GroupId, 1, Groups)
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the handle_service's user identified by UserId has privilege
%% in the handle_service. Shall return false in any other case (handle_service doesn't exist,
%% user is not handle_service's member, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_effective_privilege(HandleServiceId :: handle_service:id(), UserId :: onedata_user:id(),
    Privilege :: privileges:handle_service_privilege()) ->
    boolean().
has_effective_privilege(HandleServiceId, UserId, Privilege) ->
    case has_effective_user(HandleServiceId, UserId) of
        false -> false;
        true ->
            {ok, UserPrivileges} = get_effective_user_privileges(HandleServiceId, UserId),
            ordsets:is_element(Privilege, UserPrivileges)
    end.

%%--------------------------------------------------------------------
%% @doc Creates a handle_service for a user.
%% Throws exception when call to the datastore fails, or token/member_from_token doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create(UserId :: onedata_user:id(), Name :: handle_service:name(),
    ProxyEndpoint :: handle_service:proxy_endpoint(),
    ServiceProperties :: handle_service:service_properties()) ->
    {ok, HandleServiceId :: handle_service:id()}.
create(UserId, Name, ProxyEndpoint, ServiceProperties) ->
    Privileges = privileges:handle_service_admin(),
    HandleService = #handle_service{name = Name, proxy_endpoint = ProxyEndpoint, service_properties = ServiceProperties,  users = [{UserId, Privileges}]},

    {ok, HandleServiceId} = handle_service:create(#document{value = HandleService}),
    {ok, _} = onedata_user:update(UserId, fun(User) ->
        #onedata_user{handle_services = UHandleServices} = User,
        {ok, User#onedata_user{handle_services = [HandleServiceId | UHandleServices]}}
    end),

    {ok, HandleServiceId}.

%%--------------------------------------------------------------------
%% @doc Modifies handle_service's data.
%% Throws exception when call to the datastore fails, or handle_service doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec modify(HandleServiceId :: handle_service:id(), Name :: handle_service:name(),
    ProxyEndpoint :: handle_service:proxy_endpoint(),
    ServiceProperties :: handle_service:service_properties()) -> ok.
modify(HandleServiceId, NewName, NewProxyEndpoint, NewServiceProperties) ->
    {ok, _} = handle_service:update(HandleServiceId,
        fun(HandleService = #handle_service{name = Name, proxy_endpoint = ProxyEndpoint, service_properties = ServiceProperties}) ->
            FinalName = utils:ensure_defined(NewName, undefined, Name),
            FinalProxyEndpoint = utils:ensure_defined(NewProxyEndpoint, undefined, ProxyEndpoint),
            FinalServiceProperties = utils:ensure_defined(NewServiceProperties, undefined, ServiceProperties),
            {ok, HandleService#handle_service{name = FinalName, proxy_endpoint = FinalProxyEndpoint, service_properties = FinalServiceProperties}}
        end),
    ok.

%%--------------------------------------------------------------------
%% @doc Sets privileges for an user member of the handle_service.
%% Throws exception when call to the datastore fails, or handle_service doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_user_privileges(HandleServiceId :: handle_service:id(), UserId :: onedata_user:id(),
    Privileges :: [privileges:handle_service_privilege()]) ->
    ok.
set_user_privileges(HandleServiceId, UserId, Privileges) ->
    {ok, _} = handle_service:update(HandleServiceId,
        fun(HandleService = #handle_service{users = Users}) ->
            PrivilegesNew = ordsets:from_list(Privileges),
            UsersNew = lists:keyreplace(UserId, 1, Users, {UserId, PrivilegesNew}),
            {ok, HandleService#handle_service{users = UsersNew}}
        end),
    ok.

%%--------------------------------------------------------------------
%% @doc Sets privileges for a group member of the handle_service.
%% Throws exception when call to the datastore fails, or handle_service doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_group_privileges(HandleServiceId :: handle_service:id(), GroupId :: user_group:id(),
    Privileges :: [privileges:handle_service_privilege()]) ->
    ok.
set_group_privileges(HandleServiceId, GroupId, Privileges) ->
    {ok, _} = handle_service:update(HandleServiceId,
        fun(HandleService = #handle_service{groups = Groups}) ->
            PrivilegesNew = ordsets:from_list(Privileges),
            GroupsNew = lists:keyreplace(GroupId, 1, Groups, {GroupId, PrivilegesNew}),
            {ok, HandleService#handle_service{groups = GroupsNew}}
        end),
    ok.

%%--------------------------------------------------------------------
%% @doc Adds a new user to a handle_service.
%% @end
%%--------------------------------------------------------------------
-spec add_user(HandleServiceId :: handle_service:id(), UserId :: onedata_user:id()) ->
    {ok, HandleServiceId :: handle_service:id()}.
add_user(HandleServiceId, UserId) ->
    case has_user(HandleServiceId, UserId) of
        true -> ok;
        false ->
            {ok, _} = handle_service:update(HandleServiceId, fun(HandleService) ->
                Privileges = privileges:handle_service_user(),
                #handle_service{users = Users} = HandleService,
                {ok, HandleService#handle_service{users = [{UserId, Privileges} | Users]}}
            end),
            {ok, _} = onedata_user:update(UserId, fun(User) ->
                #onedata_user{handle_services = UHandleServices} = User,
                {ok, User#onedata_user{handle_services = [HandleServiceId | UHandleServices]}}
            end)
    end,
    {ok, HandleServiceId}.

%%--------------------------------------------------------------------
%% @doc Adds a new group to a handle_service.
%% @end
%%--------------------------------------------------------------------
-spec add_group(HandleServiceId :: handle_service:id(), GroupId :: user_group:id()) ->
    {ok, HandleServiceId :: handle_service:id()}.
add_group(HandleServiceId, GroupId) ->
    case has_group(HandleServiceId, GroupId) of
        true -> ok;
        false ->
            {ok, _} = handle_service:update(HandleServiceId, fun(HandleService) ->
                Privileges = privileges:handle_service_user(),
                #handle_service{groups = Groups} = HandleService,
                {ok, HandleService#handle_service{groups = [{GroupId, Privileges} | Groups]}}
            end),
            {ok, _} = user_group:update(GroupId, fun(Group) ->
                #user_group{handle_services = HandleServices} = Group,
                {ok, Group#user_group{handle_services = [HandleServiceId | HandleServices]}}
            end)
    end,
    {ok, HandleServiceId}.


%%--------------------------------------------------------------------
%% @doc Returns details about the handle_service.
%% Throws exception when call to the datastore fails, or handle_service doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_data(HandleServiceId :: handle_service:id()) -> {ok, [proplists:property()]}.
get_data(HandleServiceId) ->
    {ok, #document{value = #handle_service{name = Name, proxy_endpoint = Proxy, service_properties = Desc}}} =
        handle_service:get(HandleServiceId),
    {ok, [
        {handleServiceId, HandleServiceId},
        {name, Name},
        {proxyEndpoint, Proxy},
        {serviceProperties, Desc}
    ]}.

%%--------------------------------------------------------------------
%% @doc Returns details about handle_service's users.
%% Throws exception when call to the datastore fails, or handle_service doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_users(HandleServiceId :: handle_service:id()) ->
    {ok, [proplists:property()]}.
get_users(HandleServiceId) ->
    {ok, #document{value = #handle_service{users = Users}}} = handle_service:get(HandleServiceId),
    {UserIds, _} = lists:unzip(Users),
    {ok, [{users, UserIds}]}.

%%--------------------------------------------------------------------
%% @doc Returns details about handle_service's groups.
%% Throws exception when call to the datastore fails, or handle_service doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(HandleServiceId :: handle_service:id()) ->
    {ok, [proplists:property()]}.
get_groups(HandleServiceId) ->
    {ok, #document{value = #handle_service{groups = GroupTuples}}} = handle_service:get(HandleServiceId),
    {Groups, _} = lists:unzip(GroupTuples),
    {ok, [{groups, Groups}]}.

%%--------------------------------------------------------------------
%% @doc Returns list of handle_service's member privileges.
%% Throws exception when call to the datastore fails, or handle_service doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(HandleServiceId :: handle_service:id(), UserId :: onedata_user:id()) ->
    {ok, [{privileges, [privileges:handle_privilege()]}]}.
get_user_privileges(HandleServiceId, UserId) ->
    {ok, #document{value = #handle_service{users = Users}}} = handle_service:get(HandleServiceId),
    {_, Privileges} = lists:keyfind(UserId, 1, Users),
    {ok, [{privileges, Privileges}]}.

%%--------------------------------------------------------------------
%% @doc Returns list of handle_service's member privileges.
%% Throws exception when call to the datastore fails, or handle_service doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(HandleServiceId :: handle_service:id(), GroupId :: user_group:id()) ->
    {ok, [{privileges, [privileges:handle_privilege()]}]}.
get_group_privileges(HandleServiceId, GroupId) ->
    {ok, #document{value = #handle_service{groups = Groups}}} = handle_service:get(HandleServiceId),
    {_, Privileges} = lists:keyfind(GroupId, 1, Groups),
    {ok, [{privileges, Privileges}]}.

%%--------------------------------------------------------------------
%% @doc Removes the handle_service.
%% Throws exception when call to the datastore fails, or handle_service is already removed.
%% @end
%%--------------------------------------------------------------------
-spec remove(HandleServiceId :: handle_service:id()) -> boolean().
remove(HandleServiceId) ->
    {ok, #document{value = HandleService}} = handle_service:get(HandleServiceId),
    #handle_service{users = Users, groups = Groups} = HandleService,

    lists:foreach(fun({UserId, _}) ->
        {ok, _} = onedata_user:update(UserId, fun(User) ->
            #onedata_user{handle_services = UHandleServices} = User,
            {ok, User#onedata_user{
                handle_services = lists:delete(HandleServiceId, UHandleServices)
            }}
        end)
    end, Users),

    lists:foreach(fun({GroupId, _}) ->
        {ok, _} = user_group:update(GroupId, fun(Group) ->
            #user_group{handle_services = GHandleServices} = Group,
            {ok, Group#user_group{handle_services = lists:delete(HandleServiceId, GHandleServices)}}
        end)
    end, Groups),

    case handle_service:delete(HandleServiceId) of
        ok -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Removes user from the handle_service.
%% Throws exception when call to the datastore fails, or handle_service/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(HandleServiceId :: handle_service:id(), UserId :: onedata_user:id()) ->
    true.
remove_user(HandleServiceId, UserId) ->
    {ok, _} = onedata_user:update(UserId, fun(User) ->
        #onedata_user{handle_services = UHandleServices} = User,
        {ok, User#onedata_user{handle_services = lists:delete(HandleServiceId, UHandleServices)}}
    end),
    {ok, _} = handle_service:update(HandleServiceId, fun(HandleService) ->
        #handle_service{users = Users} = HandleService,
        {ok, HandleService#handle_service{users = lists:keydelete(UserId, 1, Users)}}
    end),
    cleanup(HandleServiceId),
    true.

%%--------------------------------------------------------------------
%% @doc Removes group from the handle_service.
%% Throws exception when call to the datastore fails, or handle_service/group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(HandleServiceId :: handle_service:id(), GroupId :: user_group:id()) ->
    true.
remove_group(HandleServiceId, GroupId) ->
    {ok, _} = user_group:update(GroupId, fun(Group) ->
        #user_group{handle_services = HandleServices} = Group,
        {ok, Group#user_group{handle_services = lists:delete(HandleServiceId, HandleServices)}}
    end),
    {ok, _} = handle_service:update(HandleServiceId, fun(HandleService) ->
        #handle_service{groups = Groups} = HandleService,
        {ok, HandleService#handle_service{groups = lists:keydelete(GroupId, 1, Groups)}}
    end),

    cleanup(HandleServiceId),
    true.

%%--------------------------------------------------------------------
%% @doc Removes the handle_service if empty.
%% Throws exception when call to the datastore fails, or handle_service is already removed.
%% @end
%%--------------------------------------------------------------------
-spec cleanup(HandleServiceId :: handle_service:id()) -> boolean() | no_return().
cleanup(_HandleServiceId) ->
%% Currently, handle_services with no users and groups are not deleted so it is
%% possible to restore it after accidentally leaving a handle_service.
%%    {ok, #document{value = #handle_service{groups = Groups, users = Users}}} = handle_service:get(HandleServiceId),
%%    case {Groups, Users} of
%%        {[], []} -> remove(HandleServiceId);
%%        _ -> false
%%    end.
    false.

%%--------------------------------------------------------------------
%% @doc Retrieves effective user privileges taking into account any groups
%% he is a member of that also are members of the handle_service.
%% Throws exception when call to the datastore fails, or handle_service/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_user_privileges(HandleServiceId :: handle_service:id(), UserId :: onedata_user:id()) ->
    {ok, ordsets:ordset(privileges:handle_service_privilege())}.
get_effective_user_privileges(HandleServiceId, UserId) ->
    {ok, #document{value = #onedata_user{groups = UGroups}}} = onedata_user:get(UserId),
    {ok, #document{value = #handle_service{users = UserTuples, groups = SGroupTuples}}} = handle_service:get(HandleServiceId),

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