%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains functions used for development purposes,
%%% mostly setting up test environment using the env_up script.
%%% The input arguments for set_up_test_entities/3, destroy_test_entities/3 are as follows:
%%%
%%%     Users = [
%%%         {<<"u1">>, [{<<"default_space">>, <<"sp1">>}]},
%%%         {<<"u2">>, [{<<"default_space">>, <<"sp2">>}]},
%%%         {<<"u3">>, [{<<"default_space">>, <<"sp1">>}]}
%%%     ].
%%%     Groups = [
%%%         {<<"g1">>, [{<<"users">>, [<<"u1">>, <<"u2">>]}]},
%%%         {<<"g2">>, [{<<"users">>, [<<"u2">>, <<"u3">>]}]}
%%%     ].
%%%     Spaces = [
%%%         {<<"s1">>, [
%%%             {<<"displayed_name">>, <<"space 1">>},
%%%             {<<"users">>, [<<"u1">>, <<"u3">>]},
%%%             {<<"groups">>, [<<"g1">>]},
%%%             {<<"providers">>, [
%%%                 {<<"p2">> [
%%%                    {<<"storage">>, <<"/mnt/s1">>},
%%%                    {<<"supported_size">>, 1 * 1024 * 1024 * 1024}
%%%                 ]}
%%%             ]}
%%%         ]},
%%%         {<<"s2">>, [
%%%             {<<"users">>, [<<"u2">>]},
%%%             {<<"groups">>, [<<"g2">>]},
%%%             {<<"providers">>, [
%%%                 {<<"p1">>, [
%%%                    {<<"storage">>, <<"/mnt/s1">>},
%%%                    {<<"supported_size">>, 2 * 1024 * 1024 * 1024}
%%%                 ],
%%%                 {<<"p2">>, [
%%%                    {<<"storage">>, <<"/mnt/s2">>},
%%%                    {<<"supported_size">>, 3 * 1024 * 1024 * 1024}
%%%                 ]
%%%             ]}
%%%         ]}
%%%     ].
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dev_utils).

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([dev_provider_registration_route/0]).
-export([set_up_users/1, set_up_test_entities/3, destroy_test_entities/3]).
-export([create_user_with_uuid/2]).
-export([create_group_with_uuid/3]).
-export([create_space_with_uuid/3]).

%%%===================================================================
%%% API
%%%===================================================================

dev_provider_registration_route() ->
    {<<"/providers/dev">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_provider, aspect = instance_dev}
    }}.

%%--------------------------------------------------------------------
%% @doc
%% Creates users in Onezone database. Must be done before registering
%% providers so that they are linked to the provider clusters.
%% @end
%%--------------------------------------------------------------------
-spec set_up_users(Users :: term()) -> ok | error.
set_up_users(Users) ->
    try
        lists:foreach(fun({UserId, _Props}) ->
            UserInfo = #od_user{
                full_name = UserId,
                username = UserId,
                emails = [<<UserId/binary, "@gmail.com">>],
                linked_accounts = [
                    #linked_account{idp = google,
                        subject_id = <<UserId/binary, "#oauth_id">>,
                        username = <<UserId/binary, "#oauth_username">>,
                        full_name = UserId,
                        emails = [<<UserId/binary, "@gmail.com">>]
                    }
                ],
                spaces = [],
                groups = []
            },
            {ok, UserInfo2} = basic_auth:toggle_basic_auth(UserInfo, true),
            {ok, UserInfo3} = basic_auth:set_password(UserInfo2, <<"password">>),
            {ok, UserId} = create_user_with_uuid(UserInfo3, UserId)
        end, Users)
    catch
        T:M:Stacktrace ->
            ?error_stacktrace("Cannot set up users - ~tp:~tp", [T, M], Stacktrace),
            error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates given entities in Onezone database, with all the
%% dependencies (supports, group joining etc).
%% @end
%%--------------------------------------------------------------------
-spec set_up_test_entities(Users :: term(), Groups :: term(), Spaces :: term()) -> ok | error.
set_up_test_entities(Users, Groups, Spaces) ->
    try
        % Create groups
        lists:foldl(
            fun({GroupId, Props}, Acc) ->
                UserList = proplists:get_value(<<"users">>, Props),
                [GroupCreator | UsersToAdd] = UserList,
                {ok, GroupId} = create_group_with_uuid(GroupCreator, GroupId, GroupId),
                % Add all users to group
                lists:foreach(
                    fun(UserId) ->
                        {ok, UserId} = group_logic:add_user(?ROOT, GroupId, UserId)
                    end, UsersToAdd),
                maps:put(GroupId, GroupCreator, Acc)
            end, #{}, Groups),

        lists:foreach(fun({GroupId, Props}) ->
            NestedGroups = proplists:get_value(<<"groups">>, Props, []),
            lists:foreach(fun(NestedGroupId) ->
                {ok, NestedGroupId} = group_logic:add_group(?ROOT, GroupId, NestedGroupId)
            end, NestedGroups)
        % Mark group changed as normally it is done asynchronously
        % and it could occur after refreshing
        end, Groups),

        % Create spaces
        lists:foreach(
            fun({SpaceId, Props}) ->
                UserList = proplists:get_value(<<"users">>, Props),
                GroupList = proplists:get_value(<<"groups">>, Props),
                ProviderList = proplists:get_value(<<"providers">>, Props),
                {Member, UsersToAdd, GroupsToAdd} =
                    case GroupList of
                        [] -> {{od_user, hd(UserList)}, tl(UserList), []};
                        _ -> {{od_group, hd(GroupList)}, UserList, tl(GroupList)}
                    end,
                %% create space with given name; if name is not defined, set Id as a name
                {ok, SpaceId} = case proplists:get_value(<<"displayed_name">>, Props) of
                    undefined ->
                        create_space_with_uuid(Member, SpaceId, SpaceId);
                    SpaceName ->
                        create_space_with_uuid(Member, SpaceName, SpaceId)
                end,
                % Add all users to space
                lists:foreach(
                    fun(UserId) ->
                        space_logic:add_user(?ROOT, SpaceId, UserId)
                    end, UsersToAdd),
                % Add all groups to space
                lists:foreach(
                    fun(GroupId) ->
                        space_logic:add_user(?ROOT, SpaceId, GroupId)
                    end, GroupsToAdd),
                % Support the space by all providers
                lists:foreach(
                    fun({ProviderId, ProviderProps}) ->
                        FirstUser = hd(UserList),
                        SupportedSize = proplists:get_value(<<"supported_size">>, ProviderProps),
                        ok = space_logic:update_user_privileges(
                            ?ROOT, SpaceId, FirstUser, [?SPACE_ADD_SUPPORT], []
                        ),
                        {ok, Token} = space_logic:create_space_support_token(
                            ?USER(FirstUser), SpaceId
                        ),
                        {ok, ProviderId} = storage_logic:create(
                            ?PROVIDER(ProviderId), ProviderId, ?STORAGE_DEFAULT_NAME
                        ),
                        {ok, SpaceId} = storage_logic:support_space(
                            ?PROVIDER(ProviderId), ProviderId, Token, SupportedSize
                        )
                    end, ProviderList)
            end, Spaces),

        % Give all space perms to users that have it as default space
        lists:foreach(
            fun({UserId, Props}) ->
                DefaultSpace = proplists:get_value(<<"default_space">>, Props),
                space_logic:update_user_privileges(
                    ?ROOT, DefaultSpace, UserId, privileges:space_admin(), []
                )
            end, Users),
        ok
    catch
        T:M:Stacktrace ->
            ?error_stacktrace("Cannot set up test entities - ~tp:~tp", [T, M], Stacktrace),
            error
    end.


%%--------------------------------------------------------------------
%% @doc Removes all the given entities.
%% Used for development purposes to clean up the environment created using the env_up script.
%% @end
%%--------------------------------------------------------------------
-spec destroy_test_entities(Users :: term(), Groups :: term(), Spaces :: term()) -> ok.
destroy_test_entities(Users, Groups, Spaces) ->
    lists:foreach(
        fun({UserId, _}) ->
            try user_logic:delete(?ROOT, UserId) catch _:_ -> ok end
        end, Users),
    lists:foreach(
        fun({GroupId, _}) ->
            try group_logic:delete(?ROOT, GroupId) catch _:_ -> ok end
        end, Groups),
    lists:foreach(
        fun({SpaceId, _}) ->
            try space_logic:delete(?ROOT, SpaceId) catch _:_ -> ok end
        end, Spaces),
    ok.


%%--------------------------------------------------------------------
%% @doc Creates a user account with implicit UUId.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec create_user_with_uuid(User :: #od_user{}, UserId :: binary()) -> {ok, UserId :: binary()}.
create_user_with_uuid(User, UserId) ->
    {ok, _} = od_user:create(#document{key = UserId, value = User}),
    {ok, UserId}.


%%--------------------------------------------------------------------
%% @doc Creates a group for a user with implicit UUId.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_group_with_uuid(UserId :: binary(), Name :: binary(), UUId :: binary()) ->
    {ok, GroupId :: binary()}.
create_group_with_uuid(UserId, Name, GroupId) ->
    {ok, _} = od_group:create(
        #document{key = GroupId, value = #od_group{name = Name}}
    ),
    {ok, UserId} = group_logic:add_user(
        ?ROOT, GroupId, UserId, privileges:group_admin()
    ),
    {ok, GroupId}.


%%--------------------------------------------------------------------
%% @doc Creates a Space for a user or group with implicit UUId.
%% Throws exception when call to the datastore fails, or given member doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_space_with_uuid({od_user | od_group, Id :: binary()}, Name :: binary(), UUId :: binary()) ->
    {ok, SpaceId :: binary()} | no_return().
create_space_with_uuid({MemberType, MemberId}, Name, UUId) ->
    {ok, _} = od_space:create(
        #document{key = UUId, value = #od_space{name = Name}}
    ),
    AddFun = case MemberType of
        od_user -> add_user;
        od_group -> add_group
    end,
    {ok, MemberId} = space_logic:AddFun(
        ?ROOT, UUId, MemberId, privileges:space_admin()
    ),
    {ok, UUId}.
