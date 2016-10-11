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

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([exists/1, has_user/2, has_effective_user/2, has_effective_privilege/3,
    has_nested_group/2, can_view_public_data/2]).
-export([create/3, modify/2, add_user/2, add_group/2, join/2, join_group/2, set_privileges/3]).
-export([get_data/1, get_public_data/1, get_users/1, get_effective_users/1, get_spaces/1, get_providers/1,
    get_user/2, get_privileges/2, get_effective_privileges/2, get_nested_groups/1,
    get_nested_group/2, get_nested_group_privileges/2, set_nested_group_privileges/3,
    get_parent_groups/1, get_parent_group/2, get_effective_user/2]).
-export([remove/1, remove_user/2, cleanup/1, remove_nested_group/2]).
-export([create_predefined_groups/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns whether a group exists.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec exists(GroupId :: binary()) -> boolean().
exists(GroupId) ->
    od_group:exists(GroupId).

%%--------------------------------------------------------------------
%% @doc Returns whether the user identified by UserId is a member of the group.
%% Shall return false in any other case (group doesn't exist, etc).
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec has_user(GroupId :: binary(), UserId :: binary()) ->
    boolean().
has_user(GroupId, UserId) ->
    case od_group:get(GroupId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #od_group{users = Users}}} ->
            lists:keymember(UserId, 1, Users)
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the given group is a member of the given parent group.
%% Shall return false in any other case (group doesn't exist, etc).
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec has_nested_group(ParentGroupId :: binary(), GroupId :: binary()) ->
    boolean().
has_nested_group(ParentGroupId, GroupId) ->
    case od_group:get(ParentGroupId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #od_group{children = Groups}}} ->
            lists:keymember(GroupId, 1, Groups)
    end.


%%--------------------------------------------------------------------
%% @doc Returns whether given user can view public data in a group, i.e.
%% belongs to the group (doesn't need view data priv for public data), or
%% belongs to any space of this group and has view privileges there.
%% Shall return false in any other case (group doesn't exist, etc).
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec can_view_public_data(GroupId :: od_group:id(),
    UserId :: od_user:id()) -> boolean().
can_view_public_data(GroupId, UserId) ->
    case od_group:get(GroupId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #od_group{spaces = Spaces}}} ->
            case has_effective_user(GroupId, UserId) of
                true ->
                    true;
                false ->
                    {ok, #document{
                        value = #od_user{
                            space_aliases = UserSpaces
                        }}} = od_user:get(UserId),
                    lists:any(
                        fun(SpaceId) ->
                            case maps:is_key(SpaceId, UserSpaces) of
                                false ->
                                    false;
                                true ->
                                    space_logic:has_effective_privilege(
                                        SpaceId,
                                        UserId,
                                        space_view_data
                                    )
                            end
                        end, Spaces)
            end
    end.


%%--------------------------------------------------------------------
%% @doc Returns whether the given group is a ancestor of the other given group.
%% Shall return false in any other case (group doesn't exist, etc).
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec has_effective_group(GroupId :: binary(), EffectiveId :: binary()) ->
    boolean().
has_effective_group(GroupId, EffectiveId) ->
    case od_group:get(GroupId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #od_group{eff_children = Groups}}} ->
            lists:member(EffectiveId, Groups)
    end.

%%--------------------------------------------------------------------
%% @doc Returns whether the group's member identified by UserId has privilege
%% in the group. Shall return false in any other case (group doesn't exist,
%% user is not group's member, etc).
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec has_effective_privilege(GroupId :: binary(), UserId :: binary(),
    Privilege :: privileges:group_privilege()) -> boolean().
has_effective_privilege(GroupId, UserId, Privilege) ->
    case has_user(GroupId, UserId) of
        false -> false;
        true ->
            {ok, #document{value = #od_group{users = Users,
                eff_users = EUsers}}} = od_group:get(GroupId),

            %% 'effective_users' do contain 'users' of the group
            %% but are 'users' are checked for privileges regardless
            %% as 'users' may be slightly more up-to-date
            lists:member(Privilege, proplists:get_value(UserId, Users, [])) orelse
                lists:member(Privilege, proplists:get_value(UserId, EUsers, []))
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
    case od_group:get(GroupId) of
        {error, {not_found, _}} ->
            false;
        {ok, Doc} ->
            #document{
                value = #od_group{
                    users = Users,
                    eff_users = EffectiveUsers
                }} = Doc,
            lists:keymember(UserId, 1, Users ++ EffectiveUsers)
    end.

%%--------------------------------------------------------------------
%% @doc Creates a group for a user.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create(UserId :: binary(), Name :: binary(), Type :: od_group:type()) ->
    {ok, GroupId :: binary()}.
create(UserId, Name, Type) ->
    {ok, UserDoc} = od_user:get(UserId),
    #document{value = #od_user{groups = Groups} = User} = UserDoc,

    Privileges = privileges:group_admin(),
    Group = #od_group{name = Name, type = Type, users = [{UserId, Privileges}]},
    {ok, GroupId} = od_group:save(#document{value = Group}),

    UserNew = User#od_user{groups = [GroupId | Groups]},
    od_user:save(UserDoc#document{value = UserNew}),

    {ok, GroupId}.

%%--------------------------------------------------------------------
%% @doc Modifies group's data.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec modify(GroupId :: binary(), Data :: maps:map()) ->
    ok.
modify(GroupId, Data) ->
    {ok, _} = od_group:update(GroupId, Data),
    ok.

%%--------------------------------------------------------------------
%% @doc Adds a user to a group. Does not check authorization - use join/2 for
%% user adding based on authorization!
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec add_user(GroupId :: binary(), UserId :: binary()) ->
    {ok, GroupId :: onedata_group:id()}.
add_user(GroupId, UserId) ->
    case has_user(GroupId, UserId) of
        true ->
            {ok, GroupId};
        false ->
            Privileges = privileges:group_user(),
            {ok, _} = od_group:update(GroupId, fun(Group) ->
                #od_group{users = Users} = Group,
                {ok, Group#od_group{users = [{UserId, Privileges} | Users]}}
            end),
            {ok, _} = od_user:update(UserId, fun(User) ->
                #od_user{groups = Groups} = User,
                {ok, User#od_user{groups = [GroupId | Groups]}}
            end),
            % Update user's space name mapping for every space that
            % the group belongs to
            {ok, #document{
                value = #od_group{
                    spaces = Spaces
                }}} = od_group:get(GroupId),
            lists:foreach(
                fun(SpaceId) ->
                    {ok, #document{
                        value = #od_space{
                            name = Name
                        }}} = od_space:get(SpaceId),
                    user_logic:set_space_name_mapping(
                        UserId, SpaceId, Name, false
                    )
                end, Spaces),
            {ok, GroupId}
    end.

%%--------------------------------------------------------------------
%% @doc Adds a group to a group. Does not check authorization - use join/2 for
%% group adding based on authorization!
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec add_group(ParentGroupId :: binary(), ChildGroupId :: binary()) ->
    {ok, GroupId :: onedata_group:id()} | {error, cycle_averted}.
add_group(ParentGroupId, ChildGroupId) ->
    case has_nested_group(ParentGroupId, ChildGroupId) of
        true ->
            {ok, ParentGroupId};
        false ->
            case has_effective_group(ParentGroupId, ChildGroupId) of
                true ->
                    {error, cycle_averted};
                false ->
                    Privileges = privileges:group_user(),
                    {ok, _} = od_group:update(ParentGroupId, fun(Group) ->
                        #od_group{children = Groups} = Group,
                        {ok, Group#od_group{children = [
                            {ChildGroupId, Privileges} | Groups
                        ]}}
                    end),
                    {ok, _} = od_group:update(ChildGroupId, fun(Group) ->
                        #od_group{parents = Groups} = Group,
                        {ok, Group#od_group{parents = [
                            ParentGroupId | Groups
                        ]}}
                    end),
                    {ok, ParentGroupId}
            end
    end.


%%--------------------------------------------------------------------
%% @doc Adds user to a group identified by a token.
%% Throws exception when call to the datastore fails, or
%% token/user/group_from_token doesn't exist in db.
%% @end
%%--------------------------------------------------------------------
-spec join(UserId :: binary(), Macaroon :: macaroon:macaroon()) ->
    {ok, GroupId :: onedata_group:id()}.
join(UserId, Macaroon) ->
    {ok, {group, GroupId}} = token_logic:consume(Macaroon),
    add_user(GroupId, UserId).


%%--------------------------------------------------------------------
%% @doc Adds group as nested to a group identified by the given token.
%% Throws exception when call to the datastore fails, or token/group_from_token
%% doesn't exist in db.
%% @end
%%--------------------------------------------------------------------
-spec join_group(GroupId :: binary(), Macaroon :: macaroon:macaroon()) ->
    {ok, GroupId :: onedata_group:id()} | {error, cycle_averted}.
join_group(GroupId, Macaroon) ->
    {ok, {group, ParentGroupId}} = token_logic:consume(Macaroon),
    add_group(ParentGroupId, GroupId).


%%--------------------------------------------------------------------
%% @doc Sets privileges for a member of the group.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_privileges(GroupId :: binary(), UserId :: binary(),
    Privileges :: [privileges:group_privilege()]) ->
    ok.
set_privileges(GroupId, UserId, Privileges) ->
    {ok, _} = od_group:update(GroupId, fun(Group) ->
        #od_group{users = Users} = Group,
        UsersNew = lists:keyreplace(UserId, 1, Users, {UserId, Privileges}),
        {ok, Group#od_group{users = UsersNew}}
    end),
    ok.

%%--------------------------------------------------------------------
%% @doc Sets privileges for a member of the group.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_nested_group_privileges(ParentGroupId :: binary(), GroupId :: binary(),
    Privileges :: [privileges:group_privilege()]) ->
    ok.
set_nested_group_privileges(ParentGroupId, GroupId, Privileges) ->
    {ok, _} = od_group:update(ParentGroupId, fun(Group) ->
        #od_group{children = Groups} = Group,
        GroupsNew = lists:keyreplace(GroupId, 1, Groups, {GroupId, Privileges}),
        {ok, Group#od_group{children = GroupsNew}}
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
    {ok, #document{value = #od_group{name = Name, type = Type}}} =
        od_group:get(GroupId),
    {ok, [
        {groupId, GroupId},
        {name, Name},
        {type, Type}
    ]}.

%%--------------------------------------------------------------------
%% @doc Returns the groupId and name of the group (data that users without
%% group view data privilege are allowed to see).
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_public_data(GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_public_data(GroupId) ->
    {ok, #document{value = #od_group{name = Name}}} =
        od_group:get(GroupId),
    {ok, [
        {groupId, GroupId},
        {name, Name},
        % This is an information that only public data is available.
        {type, public}
    ]}.

%%--------------------------------------------------------------------
%% @doc Returns details about group's members.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_users(GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_users(GroupId) ->
    {ok, #document{value = #od_group{users = UserTuples}}} = od_group:get(GroupId),
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
    {ok, #document{
        value = #od_group{
            users = UserTuples,
            eff_users = EffUserTuples
        }}} = od_group:get(GroupId),
    {Users, _} = lists:unzip(UserTuples),
    {EffUsers, _} = lists:unzip(EffUserTuples),
    {ok, [{users, ordsets:union(Users, EffUsers)}]}.

%%--------------------------------------------------------------------
%% @doc Returns details about group's nested groups members.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_nested_groups(GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_nested_groups(GroupId) ->
    {ok, #document{value = #od_group{children = GroupTuples}}}
        = od_group:get(GroupId),
    {Groups, _} = lists:unzip(GroupTuples),
    {ok, [{nested_groups, Groups}]}.

%%--------------------------------------------------------------------
%% @doc Returns details about group's nested groups members.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_parent_groups(GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_parent_groups(GroupId) ->
    {ok, #document{value = #od_group{parents = GroupIds}}}
        = od_group:get(GroupId),
    {ok, [{parent_groups, GroupIds}]}.

%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Returns details about group's spaces.
%% Throws exception when call to the datastore fails, or group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_spaces(GroupId) ->
    {ok, #document{value = #od_group{spaces = Spaces}}} = od_group:get(GroupId),
    {ok, [{spaces, Spaces}]}.

%%--------------------------------------------------------------------
%% @doc Returns providers of user's spaces.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_providers(GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_providers(GroupId) ->
    {ok, #document{value = #od_group{spaces = Spaces}}} = od_group:get(GroupId),
    GroupProviders = lists:foldl(fun(Space, Providers) ->
        {ok, #document{value = #od_space{providers_supports = ProvidersSupports}}}
            = od_space:get(Space),
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
%% @doc Returns details about group's member.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_user(GroupId :: binary(), UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_effective_user(_GroupId, UserId) ->
    user_logic:get_data(UserId, provider).

%%--------------------------------------------------------------------
%% @doc Returns details about group's group member.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_nested_group(ParentGroupId :: binary(), GroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_nested_group(_ParentGroupId, GroupId) ->
    get_data(GroupId).

%%--------------------------------------------------------------------
%% @doc Returns details about group's parent.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_parent_group(GroupId :: binary(), ParentGroupId :: binary()) ->
    {ok, [proplists:property()]}.
get_parent_group(_GroupId, ParentGroupId) ->
    get_data(ParentGroupId).

%%--------------------------------------------------------------------
%% @doc Returns list of group's member privileges.
%% Throws exception when call to the datastore fails, or group/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_privileges(GroupId :: binary(), UserId :: binary()) ->
    {ok, [privileges:group_privilege()]}.
get_privileges(GroupId, UserId) ->
    {ok, #document{value = #od_group{users = UserTuples}}} = od_group:get(GroupId),
    {_, Privileges} = lists:keyfind(UserId, 1, UserTuples),
    {ok, Privileges}.

%%--------------------------------------------------------------------
%% @doc Returns list of group's member privileges.
%% Throws exception when call to the datastore fails, or group/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_nested_group_privileges(ParentGroupId :: binary(), GroupId :: binary()) ->
    {ok, [privileges:group_privilege()]}.
get_nested_group_privileges(ParentGroupId, GroupId) ->
    {ok, #document{value = #od_group{children = GroupTuples}}} = od_group:get(ParentGroupId),
    {_, Privileges} = lists:keyfind(GroupId, 1, GroupTuples),
    {ok, Privileges}.

%%--------------------------------------------------------------------
%% @doc Returns list of group's member privileges.
%% Throws exception when call to the datastore fails, or group/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_privileges(GroupId :: binary(), UserId :: binary()) ->
    {ok, [privileges:group_privilege()]}.
get_effective_privileges(GroupId, UserId) ->
    {ok, #document{value = #od_group{
        users = UserTuples, eff_users = EffUserTuples
    }}} = od_group:get(GroupId),
    case lists:keyfind(UserId, 1, EffUserTuples) of
        {_, Privileges} ->
            {ok, Privileges};
        false ->
            % It is possible that effective users are not yet synchronized, in
            % this case look in direct users.
            {_, USerPrivs} = lists:keyfind(UserId, 1, UserTuples),
            {ok, USerPrivs}
    end.

%%--------------------------------------------------------------------
%% @doc Removes the group.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec remove(GroupId :: binary()) ->
    true.
remove(GroupId) ->
    {ok, #document{value = #od_group{users = Users, spaces = Spaces}}} = od_group:get(GroupId),
    lists:foreach(fun({UserId, _}) ->
        {ok, _} = od_user:update(UserId, fun(User) ->
            #od_user{groups = Groups} = User,
            {ok, User#od_user{groups = lists:delete(GroupId, Groups)}}
        end)
    end, Users),
    lists:foreach(fun(SpaceId) ->
        {ok, _} = od_space:update(SpaceId, fun(Space) ->
            #od_space{groups = SGroups} = Space,
            {ok, Space#od_space{groups = lists:keydelete(GroupId, 1, SGroups)}}
        end)
    end, Spaces),
    od_group:delete(GroupId),
    true.

%%--------------------------------------------------------------------
%% @doc Removes user from the group.
%% Throws exception when call to the datastore fails, or group/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(GroupId :: binary(), UserId :: binary()) ->
    true.
remove_user(GroupId, UserId) ->
    {ok, _} = od_group:update(GroupId, fun(Group) ->
        #od_group{users = Users} = Group,
        {ok, Group#od_group{users = lists:keydelete(UserId, 1, Users)}}
    end),
    {ok, _} = od_user:update(UserId, fun(User) ->
        #od_user{groups = Groups} = User,
        {ok, User#od_user{groups = lists:delete(GroupId, Groups)}}
    end),
    cleanup(GroupId),
    % Clean user's space name mapping for every space that the group belongs to
    {ok, #document{
        value = #od_group{
            spaces = Spaces
        }}} = od_group:get(GroupId),
    lists:foreach(
        fun(SpaceId) ->
            user_logic:clean_space_name_mapping(UserId, SpaceId)
        end, Spaces),
    true.

%%--------------------------------------------------------------------
%% @doc Removes nested group from the group.
%% Throws exception when call to the datastore fails, or group/user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec remove_nested_group(ParentGroupId :: binary(), GroupId :: binary()) ->
    true.
remove_nested_group(ParentGroupId, GroupId) ->
    {ok, _} = od_group:update(ParentGroupId, fun(Group) ->
        #od_group{children = Nested} = Group,
        {ok, Group#od_group{children = lists:keydelete(GroupId, 1, Nested)}}
    end),
    {ok, _} = od_group:update(GroupId, fun(Group) ->
        #od_group{parents = Parents} = Group,
        {ok, Group#od_group{parents = lists:delete(ParentGroupId, Parents)}}
    end),
    cleanup(ParentGroupId),
    true.

%%--------------------------------------------------------------------
%% @doc Removes the group if empty.
%% Throws exception when call to the datastore fails, or group is already removed.
%% @end
%%--------------------------------------------------------------------
-spec cleanup(GroupId :: binary()) -> boolean().
cleanup(_GroupId) ->
    false.
%% Currently, groups with no users and groups are not deleted so it is
%% possible to restore membership after accidentally leaving a group.
%%    {ok, #document{value = Group}} = od_group:get(GroupId),
%%    case Group of
%%        #od_group{users = [], nested_groups = []} -> remove(GroupId);
%%        _ -> false
%%    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates predefined groups in the system based on settings in app.config.
%% @end
%%--------------------------------------------------------------------
-spec create_predefined_groups() -> ok.
create_predefined_groups() ->
    {ok, PredefinedGroups} =
        application:get_env(?APP_Name, predefined_groups),
    lists:foreach(
        fun(GroupMap) ->
            Id = maps:get(id, GroupMap),
            Name = maps:get(name, GroupMap),
            % Privileges can be either a list of privileges or a module and
            % function to call that will return such list.
            Privs = case maps:get(oz_api_privileges, GroupMap) of
                List when is_list(List) ->
                    List;
                {Module, Function} ->
                    Module:Function()
            end,
            create_predefined_group(Id, Name, Privs)
        end, PredefinedGroups).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a predefined group in the system, if it does not exist, and grants
%% given privileges to it.
%% @end
%%--------------------------------------------------------------------
-spec create_predefined_group(Id :: binary(), Name :: binary(),
    Privileges :: [oz_api_privileges:privilege()]) -> ok | error.
create_predefined_group(Id, Name, Privileges) ->
    case od_group:exists(Id) of
        true ->
            ?info("Predefined group '~s' already exists, "
            "skipping.", [Name]),
            ok;
        false ->
            NewGroup = #document{
                key = Id,
                value = #od_group{
                    name = Name,
                    type = role
                }},
            case od_group:create(NewGroup) of
                {ok, Id} ->
                    ok = oz_api_privileges_logic:modify(
                        Id, od_group, Privileges),
                    ?info("Created predefined group '~s'", [Name]),
                    ok;
                Other ->
                    ?error("Cannot create predefined group '~s' - ~p",
                        [Id, Other]),
                    error
            end
    end.
