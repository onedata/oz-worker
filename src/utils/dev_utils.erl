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
%%%             {<<"users">>, [<<"u1">>, <<"u3">>]},
%%%             {<<"groups">>, [<<"g1">>]},
%%%             {<<"providers">>, [
%%%                 {<<"provider">>, <<"p2">>}, {<<"supported_size">>, 1 * 1024 * 1024 * 1024}
%%%             ]}
%%%         ]},
%%%         {<<"s2">>, [
%%%             {<<"sers, [<<"u2">>]},
%%%             {<<"groups, [<<"g2">>]},
%%%             {<<"providers, [
%%%                 {<<"provider">>, <<"p1">>}, {<<"supported_size">>, 2 * 1024 * 1024 * 1024},
%%%                 {<<"provider">>, <<"p2">>}, {<<"supported_size">>, 3 * 1024 * 1024 * 1024}
%%%             ]}
%%%         ]}
%%%     ].
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dev_utils).

-include_lib("ctool/include/logging.hrl").
-include("dao/dao_types.hrl").

%% API
-export([set_up_test_entities/3, destroy_test_entities/3]).
-export([create_provider_with_uuid/5]).
-export([create_user_with_uuid/2]).
-export([create_group_with_uuid/3]).
-export([create_space_with_uuid/3, create_space_with_uuid/5, create_space_with_provider/5]).

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
            fun({UserID, Props}) ->
                DefaultSpace = proplists:get_value(<<"default_space">>, Props),
                UserInfo = #user{
                    name = UserID,
                    alias = UserID,
                    email_list = [<<UserID/binary, "@email.com">>],
                    connected_accounts = [],
                    spaces = [],
                    default_space = DefaultSpace,
                    groups = [],
                    first_space_support_token = <<"">>,
                    default_provider = <<"">>
                },
                {ok, UserID} = create_user_with_uuid(UserInfo, UserID)
            end, Users),

        % Create groups
        lists:foreach(
            fun({GroupID, Props}) ->
                UserList = proplists:get_value(<<"users">>, Props),
                [FirstUser | UsersToAdd] = UserList,
                {ok, GroupID} = create_group_with_uuid(FirstUser, GroupID, GroupID),
                % Add all users to group
                lists:foreach(
                    fun(UserID) ->
                        {ok, GroupToken} = token_logic:create(group_invite_token, {group, GroupID}),
                        group_logic:join(UserID, GroupToken)
                    end, UsersToAdd)
            end, Groups),

        % Create spaces
        lists:foreach(
            fun({SpaceID, Props}) ->
                UserList = proplists:get_value(<<"users">>, Props),
                GroupList = proplists:get_value(<<"groups">>, Props),
                ProviderList = proplists:get_value(<<"providers">>, Props),
                {Member, UsersToAdd, GroupsToAdd} =
                    case GroupList of
                        [] -> {{user, hd(UserList)}, tl(UserList), []};
                        _ -> {{group, hd(GroupList)}, UserList, tl(GroupList)}
                    end,
                {ok, SpaceID} = create_space_with_uuid(Member, SpaceID, SpaceID),
                % Support the space by all providers
                lists:foreach(
                    fun(ProviderProps) ->
                        ProviderID = proplists:get_value(<<"provider">>, ProviderProps),
                        SupportedSize = proplists:get_value(<<"supported_size">>, ProviderProps),
                        {ok, SpaceToken} = token_logic:create(space_support_token, {space, SpaceID}),
                        space_logic:support(ProviderID, SpaceToken, SupportedSize)
                    end, ProviderList),
                % Add all users to space
                lists:foreach(
                    fun(UserID) ->
                        {ok, SpaceToken} = token_logic:create(space_invite_user_token, {space, SpaceID}),
                        space_logic:join({user, UserID}, SpaceToken)
                    end, UsersToAdd),
                % Add all groups to space
                lists:foreach(
                    fun(GroupID) ->
                        {ok, SpaceToken} = token_logic:create(space_invite_group_token, {space, SpaceID}),
                        space_logic:join({group, GroupID}, SpaceToken)
                    end, GroupsToAdd)
            end, Spaces),
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
-spec set_up_test_entities(Users :: term(), Groups :: term(), Spaces :: term()) -> ok | error.
destroy_test_entities(Users, Groups, Spaces) ->
    lists:foreach(
        fun({UserID, _}) ->
            try user_logic:remove(UserID) catch _:_ -> ok end
        end, Users),
    lists:foreach(
        fun({GroupID, _}) ->
            try group_logic:remove(GroupID) catch _:_ -> ok end
        end, Groups),
    lists:foreach(
        fun({SpaceID, _}) ->
            try space_logic:remove(SpaceID) catch _:_ -> ok end
        end, Spaces),
    ok.


%%--------------------------------------------------------------------
%% @doc Create a provider's account with implicit UUID.
%% Throws exception when call to dao fails.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_with_uuid(ClientName :: binary(), URLs :: [binary()],
    RedirectionPoint :: binary(), CSR :: binary(), UUID :: binary()) ->
    {ok, ProviderId :: binary(), ProviderCertPem :: binary()}.
create_provider_with_uuid(ClientName, URLs, RedirectionPoint, CSRBin, UUID) ->
    {ok, ProviderCertPem, Serial} = grpca:sign_provider_req(UUID, CSRBin),
    dao_adapter:save(#db_document{uuid = binary:bin_to_list(UUID), record =
    #provider{client_name = ClientName, urls = URLs,
        redirection_point = RedirectionPoint, serial = Serial}}),
    {ok, UUID, ProviderCertPem}.


%%--------------------------------------------------------------------
%% @doc Creates a user account with implicit UUID.
%% Throws exception when call to dao fails.
%% @end
%%--------------------------------------------------------------------
-spec create_user_with_uuid(User :: #user{}, UUID :: binary()) -> {ok, UserId :: binary()}.
create_user_with_uuid(User, UUID) ->
    UserIDStr = binary:bin_to_list(UUID),
    UserID = dao_adapter:save(#db_document{uuid = UserIDStr, record = User}),
    {ok, UserID}.


%%--------------------------------------------------------------------
%% @doc Creates a group for a user with implicit UUID.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_group_with_uuid(UserId :: binary(), Name :: binary(), UUID :: binary()) ->
    {ok, GroupId :: binary()}.
create_group_with_uuid(UserId, Name, UUID) ->
    {ok, [{providers, UserProviders}]} = user_logic:get_providers(UserId),

    UserDoc = dao_adapter:user_doc(UserId),
    #db_document{record = #user{groups = Groups} = User} = UserDoc,

    Privileges = privileges:group_admin(),
    Group = #user_group{name = Name, users = [{UserId, Privileges}]},
    GroupId = dao_adapter:save(#db_document{uuid = binary:bin_to_list(UUID), record = Group}),
    UserNew = User#user{groups = [GroupId | Groups]},
    dao_adapter:save(UserDoc#db_document{record = UserNew}),

    op_channel_logic:user_modified(UserProviders, UserId, UserNew),
    {ok, GroupId}.


%%--------------------------------------------------------------------
%% @doc Creates a Space for a user or group with implicit UUID.
%% Throws exception when call to dao fails, or given member doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_space_with_uuid({user | group, Id :: binary()}, Name :: binary(), UUID :: binary()) ->
    {ok, SpaceId :: binary()} | no_return().
create_space_with_uuid(Member, Name, UUID) ->
    create_space_with_provider(Member, Name, [], [], UUID).


%%--------------------------------------------------------------------
%% @doc Creates a Space for a user or group with implicit UUID, by a provider that will support it.
%% Throws exception when call to dao fails, or token/member_from_token doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_space_with_uuid({provider, ProviderId :: binary()}, Name :: binary(),
    Token :: binary(), Size :: pos_integer(), UUID :: binary()) ->
    {ok, SpaceId :: binary()}.
create_space_with_uuid({provider, ProviderId}, Name, Token, Size, UUID) ->
    {ok, Member} = token_logic:consume(Token, space_create_token),
    create_space_with_provider(Member, Name, [ProviderId], [{ProviderId, Size}], UUID).


%%--------------------------------------------------------------------
%% @doc Creates a Space for a user or a group with implicit UUID, with a preexisting provider.
%% Throws exception when call to dao fails, or user/group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_space_with_provider({user | group, Id :: binary()}, Name :: binary(), Providers :: [binary()],
    Size :: [{Provider :: binary(), ProvidedSize :: pos_integer()}], UUID :: binary()) ->
    {ok, SpaceId :: binary()}.
create_space_with_provider({user, UserId}, Name, Providers, Size, UUID) ->
    UserDoc = dao_adapter:user_doc(UserId),
    #db_document{record = #user{spaces = Spaces} = User} = UserDoc,

    Privileges = privileges:space_admin(),
    Space = #space{name = Name, size = Size, providers = Providers, users = [{UserId, Privileges}]},
    SpaceId = dao_adapter:save(#db_document{uuid = binary:bin_to_list(UUID), record = Space}),
    UserNew = User#user{spaces = [SpaceId | Spaces]},
    dao_adapter:save(UserDoc#db_document{record = UserNew}),

    op_channel_logic:space_modified(Providers, SpaceId, Space),
    op_channel_logic:user_modified(Providers, UserId, UserNew),
    {ok, SpaceId};
create_space_with_provider({group, GroupId}, Name, Providers, Size, UUID) ->
    GroupDoc = dao_adapter:group_doc(GroupId),
    #db_document{record = #user_group{users = Users, spaces = Spaces} = Group} = GroupDoc,

    Privileges = privileges:space_admin(),
    Space = #space{name = Name, size = Size, providers = Providers, groups = [{GroupId, Privileges}]},
    SpaceId = dao_adapter:save(#db_document{uuid = binary:bin_to_list(UUID), record = Space}),
    GroupNew = Group#user_group{spaces = [SpaceId | Spaces]},
    dao_adapter:save(GroupDoc#db_document{record = GroupNew}),

    op_channel_logic:space_modified(Providers, SpaceId, Space),
    op_channel_logic:group_modified(Providers, GroupId, Group),
    lists:foreach(fun({UserId, _}) ->
        op_channel_logic:user_modified(Providers, UserId, dao_adapter:user(UserId))
    end, Users),
    {ok, SpaceId}.