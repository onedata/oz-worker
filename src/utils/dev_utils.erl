%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains functionas used for development purposes,
%%% mostly setting up test environment.
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

-include("entity_logic.hrl").
-include("http/handlers/rest_handler.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([set_up_test_entities/3, destroy_test_entities/3]).
-export([create_provider_with_uuid/6, create_provider_with_uuid/5]).
-export([create_user_with_uuid/2]).
-export([create_group_with_uuid/3]).
-export([create_space_with_uuid/3, create_space_with_uuid/5, create_space_with_provider/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates given entities in GR database, with all the dependencies (supports, group joining etc).
%% Used for development purposes to set up an environment using the env_up script.
%% @end
%%--------------------------------------------------------------------
-spec set_up_test_entities(Users :: term(), Groups :: term(), Spaces :: term()) -> ok | error.
set_up_test_entities(Users, Groups, Spaces) ->
    try
        % Create users
        lists:foreach(
            fun({UserId, Props}) ->
                DefaultSpace = proplists:get_value(<<"default_space">>, Props),
                UserInfo = #od_user{
                    name = UserId,
                    alias = UserId,
                    email_list = [<<UserId/binary, "@gmail.com">>],
                    connected_accounts = [
                        #oauth_account{provider_id = google,
                            user_id = <<UserId/binary, "#oauth_id">>,
                            login = <<UserId/binary, "#oauth_login">>,
                            name = UserId,
                            email_list = [<<UserId/binary, "@gmail.com">>]
                        }
                    ],
                    spaces = [],
                    default_space = DefaultSpace,
                    groups = [],
                    default_provider = undefined,
                    chosen_provider = undefined
                },
                {ok, UserId} = create_user_with_uuid(UserInfo, UserId)
            end, Users),

        % Create groups
        GroupCreators = lists:foldl(
            fun({GroupId, Props}, Acc) ->
                UserList = proplists:get_value(<<"users">>, Props),
                [GroupCreator | UsersToAdd] = UserList,
                {ok, GroupId} = create_group_with_uuid(GroupCreator, GroupId, GroupId),
                % Add all users to group
                lists:foreach(
                    fun(UserId) ->
                        {ok, UserId} = n_group_logic:add_user(?ROOT,GroupId, UserId )
                    end, UsersToAdd),
                maps:put(GroupId, GroupCreator, Acc)
            end, #{}, Groups),

        lists:foreach(fun({GroupId, Props}) ->
            NestedGroups = proplists:get_value(<<"groups">>, Props, []),
            GroupCreator = maps:get(GroupId, GroupCreators),
            lists:foreach(fun(NestedGroupId) ->
                {ok, NestedGroupId} = n_group_logic:add_group(?ROOT, GroupId, NestedGroupId)
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
                {{_, MemberId} = Member, UsersToAdd, GroupsToAdd} =
                    case GroupList of
                        [] -> {{user, hd(UserList)}, tl(UserList), []};
                        _ -> {{group, hd(GroupList)}, UserList, tl(GroupList)}
                    end,
                %% create space with given name; if name is not defined, set Id as a name
                {ok, SpaceId} = case proplists:get_value(<<"displayed_name">>, Props) of
                    undefined ->
                        create_space_with_uuid(Member, SpaceId, SpaceId);
                    SpaceName ->
                        create_space_with_uuid(Member, SpaceName, SpaceId)
                end,
                % Support the space by all providers
                lists:foreach(
                    fun({ProviderId, ProviderProps}) ->
                        SupportedSize = proplists:get_value(<<"supported_size">>, ProviderProps),
                        {ok, Token} = n_space_logic:create_provider_invite_token(?ROOT, SpaceId),
                        {ok, SpaceId} = n_provider_logic:support_space(?ROOT, ProviderId, Token, SupportedSize)
                    end, ProviderList),
                % Add all users to space
                lists:foreach(
                    fun(UserId) ->
                        n_space_logic:add_user(?ROOT, SpaceId, UserId)
                    end, UsersToAdd),
                % Add all groups to space
                lists:foreach(
                    fun(GroupId) ->
                        n_space_logic:add_user(?ROOT, SpaceId, GroupId)
                    end, GroupsToAdd)
            end, Spaces),

        % Give all space perms to users that have it as default space
        lists:foreach(
            fun({UserId, Props}) ->
                DefaultSpace = proplists:get_value(<<"default_space">>, Props),
                n_space_logic:update_user_privileges(
                    ?ROOT, DefaultSpace, UserId, set, privileges:space_admin()
                )
            end, Users),
        ok
    catch
        T:M ->
            ?error_stacktrace("Cannot set up test entities - ~p:~p", [T, M]),
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
            try n_user_logic:delete(?ROOT, UserId) catch _:_ -> ok end
        end, Users),
    lists:foreach(
        fun({GroupId, _}) ->
            try n_group_logic:delete(?ROOT, GroupId) catch _:_ -> ok end
        end, Groups),
    lists:foreach(
        fun({SpaceId, _}) ->
            try n_space_logic:delete(?ROOT, SpaceId) catch _:_ -> ok end
        end, Spaces),
    ok.

%%--------------------------------------------------------------------
%% @doc Create a provider's account with implicit UUId.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_with_uuid(ClientName :: binary(), URLs :: [binary()],
    RedirectionPoint :: binary(), CSR :: binary(), UUId :: binary()) ->
    {ok, ProviderId :: binary(), ProviderCertPem :: binary()}.
create_provider_with_uuid(ClientName, URLs, RedirectionPoint, CSRBin, UUId) ->
    create_provider_with_uuid(ClientName, URLs, RedirectionPoint, CSRBin, UUId, #{}).

%%--------------------------------------------------------------------
%% @doc Create a provider's account with implicit UUId.
%% Throws exception when call to the datastore fails.
%% Accepts optional arguments map (which currently supports 'latitude' and
%% 'longitude' keys)
%% @end
%%--------------------------------------------------------------------
-spec create_provider_with_uuid(ClientName :: binary(), URLs :: [binary()],
    RedirectionPoint :: binary(), CSR :: binary(), UUId :: binary(),
    OptionalArgs :: #{atom() => term()}) ->
    {ok, ProviderId :: binary(), ProviderCertPem :: binary()}.
create_provider_with_uuid(ClientName, URLs, RedirectionPoint, CSRBin, UUId, OptionalArgs) ->
    {ok, {ProviderCertPem, Serial}} = ozpca:sign_provider_req(UUId, CSRBin),
    Latitude = maps:get(latitude, OptionalArgs, undefined),
    Longitude = maps:get(longitude, OptionalArgs, undefined),

    Provider = #od_provider{name = ClientName, urls = URLs,
        redirection_point = RedirectionPoint, serial = Serial,
        latitude = Latitude, longitude = Longitude},

    od_provider:save(#document{key = UUId, value = Provider}),
    {ok, UUId, ProviderCertPem}.


%%--------------------------------------------------------------------
%% @doc Creates a user account with implicit UUId.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec create_user_with_uuid(User :: #od_user{}, UUId :: binary()) -> {ok, UserId :: binary()}.
create_user_with_uuid(User, UUId) ->
    {ok, _} = od_user:save(#document{key = UUId, value = User}).


%%--------------------------------------------------------------------
%% @doc Creates a group for a user with implicit UUId.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_group_with_uuid(UserId :: binary(), Name :: binary(), UUId :: binary()) ->
    {ok, GroupId :: binary()}.
create_group_with_uuid(UserId, Name, UUId) ->
    {ok, UserDoc} = od_user:get(UserId),
    #document{value = #od_user{groups = Groups} = User} = UserDoc,

    Privileges = privileges:group_admin(),
    Group = #od_group{name = Name, users = #{UserId => Privileges}},
    {ok, GroupId} = od_group:save(#document{key = UUId, value = Group}),
    UserNew = User#od_user{groups = [GroupId | Groups]},
    od_user:save(UserDoc#document{value = UserNew}),

    {ok, GroupId}.


%%--------------------------------------------------------------------
%% @doc Creates a Space for a user or group with implicit UUId.
%% Throws exception when call to the datastore fails, or given member doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_space_with_uuid({user | group, Id :: binary()}, Name :: binary(), UUId :: binary()) ->
    {ok, SpaceId :: binary()} | no_return().
create_space_with_uuid(Member, Name, UUId) ->
    create_space_with_provider(Member, Name, #{}, UUId).


%%--------------------------------------------------------------------
%% @doc Creates a Space for a user or group with implicit UUId, by a provider that will support it.
%% Throws exception when call to the datastore fails, or token/member_from_token doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_space_with_uuid({provider, ProviderId :: binary()}, Name :: binary(),
    Token :: binary(), Support :: pos_integer(), UUId :: binary()) ->
    {ok, SpaceId :: binary()}.
create_space_with_uuid({provider, ProviderId}, Name, Token, Support, UUId) ->
    {ok, Macaroon} = token_utils:deserialize(Token),
    {ok, Member} = token_logic:consume(Macaroon),
    create_space_with_provider(Member, Name, #{ProviderId => Support}, UUId).


%%--------------------------------------------------------------------
%% @doc Creates a Space for a user or a group with implicit UUId, with a preexisting provider.
%% Throws exception when call to the datastore fails, or user/group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_space_with_provider({user | group, Id :: binary()}, Name :: binary(),
    Support :: [{Provider :: binary(), ProvidedSize :: pos_integer()}], UUId :: binary()) ->
    {ok, SpaceId :: binary()}.
create_space_with_provider({user, UserId}, Name, Support, UUId) ->
    {ok, UserDoc} = od_user:get(UserId),
    #document{value = #od_user{spaces = Spaces} = User} = UserDoc,

    Privileges = privileges:space_admin(),
    Space = #od_space{name = Name, providers = Support, users = #{UserId => Privileges}},
    {ok, SpaceId} = od_space:save(#document{key = UUId, value = Space}),
    UserNew = User#od_user{spaces = [SpaceId | Spaces]},
    od_user:save(UserDoc#document{value = UserNew}),

    {ok, SpaceId};
create_space_with_provider({group, GroupId}, Name, Support, UUId) ->
    {ok, GroupDoc} = od_group:get(GroupId),
    #document{value = #od_group{spaces = Spaces} = Group} = GroupDoc,

    Privileges = privileges:space_admin(),
    Space = #od_space{name = Name, providers = Support,
        groups = #{GroupId => Privileges}},
    {ok, SpaceId} = od_space:save(#document{key = UUId, value = Space}),
    GroupNew = Group#od_group{spaces = [SpaceId | Spaces]},
    od_group:save(GroupDoc#document{value = GroupNew}),

    {ok, SpaceId}.
