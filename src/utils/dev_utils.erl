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
%%%                 {<<"provider">>, <<"p2">>}, {<<"supported_size">>, 1 * 1024 * 1024 * 1024}
%%%             ]}
%%%         ]},
%%%         {<<"s2">>, [
%%%             {<<"users">>, [<<"u2">>]},
%%%             {<<"groups">>, [<<"g2">>]},
%%%             {<<"providers">>, [
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
-include("datastore/gr_datastore_models_def.hrl").
-include("handlers/rest_handler.hrl").

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
                UserInfo = #onedata_user{
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
                        {ok, SerializedGroupToken} = token_logic:create(#client{type = user, id = UserID}, group_invite_token, {group, GroupID}),
                        {ok, GroupToken} = macaroon:deserialize(SerializedGroupToken),
                        group_logic:join(UserID, GroupToken)
                    end, UsersToAdd)
            end, Groups),

        % Create spaces
        lists:foreach(
            fun({SpaceID, Props}) ->
                UserList = proplists:get_value(<<"users">>, Props),
                GroupList = proplists:get_value(<<"groups">>, Props),
                ProviderList = proplists:get_value(<<"providers">>, Props),
                {{_, MemberId} = Member, UsersToAdd, GroupsToAdd} =
                    case GroupList of
                        [] -> {{user, hd(UserList)}, tl(UserList), []};
                        _ -> {{group, hd(GroupList)}, UserList, tl(GroupList)}
                    end,
                %% create space with given name; if name is not defined, set ID as a name
                {ok, SpaceID} = case proplists:get_value(<<"displayed_name">>, Props) of
                    undefined -> create_space_with_uuid(Member, SpaceID, SpaceID);
                    SpaceName -> create_space_with_uuid(Member, SpaceName, SpaceID)
                end,
                % Support the space by all providers
                lists:foreach(
                    fun(ProviderProps) ->
                        ProviderID = proplists:get_value(<<"provider">>, ProviderProps),
                        SupportedSize = proplists:get_value(<<"supported_size">>, ProviderProps),
                        {ok, SerializedSpaceToken} = token_logic:create(#client{type = user, id = MemberId}, space_support_token, {space, SpaceID}),
                        {ok, SpaceToken} = macaroon:deserialize(SerializedSpaceToken),
                        space_logic:support(ProviderID, SpaceToken, SupportedSize)
                    end, ProviderList),
                % Add all users to space
                lists:foreach(
                    fun(UserID) ->
                        {ok, SerializedSpaceToken} = token_logic:create(#client{type = user, id = MemberId}, space_invite_user_token, {space, SpaceID}),
                        {ok, SpaceToken} = macaroon:deserialize(SerializedSpaceToken),
                        space_logic:join({user, UserID}, SpaceToken)
                    end, UsersToAdd),
                % Add all groups to space
                lists:foreach(
                    fun(GroupID) ->
                        {ok, SerializedSpaceToken} = token_logic:create(#client{type = user, id = MemberId}, space_invite_group_token, {space, SpaceID}),
                        {ok, SpaceToken} = macaroon:deserialize(SerializedSpaceToken),
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
-spec destroy_test_entities(Users :: term(), Groups :: term(), Spaces :: term()) -> ok.
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
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_with_uuid(ClientName :: binary(), URLs :: [binary()],
    RedirectionPoint :: binary(), CSR :: binary(), UUID :: binary()) ->
    {ok, ProviderId :: binary(), ProviderCertPem :: binary()}.
create_provider_with_uuid(ClientName, URLs, RedirectionPoint, CSRBin, UUID) ->
    {ok, ProviderCertPem, Serial} = grpca:sign_provider_req(UUID, CSRBin),
    Provider = #provider{client_name = ClientName, urls = URLs, redirection_point = RedirectionPoint, serial = Serial},
    provider:save(#document{key = binary:bin_to_list(UUID), value = Provider}),
    {ok, UUID, ProviderCertPem}.


%%--------------------------------------------------------------------
%% @doc Creates a user account with implicit UUID.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec create_user_with_uuid(User :: #onedata_user{}, UUID :: binary()) -> {ok, UserId :: binary()}.
create_user_with_uuid(User, UUID) ->
    {ok, _} = onedata_user:save(#document{key = UUID, value = User}).


%%--------------------------------------------------------------------
%% @doc Creates a group for a user with implicit UUID.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_group_with_uuid(UserId :: binary(), Name :: binary(), UUID :: binary()) ->
    {ok, GroupId :: binary()}.
create_group_with_uuid(UserId, Name, UUID) ->
    {ok, UserDoc} = onedata_user:get(UserId),
    #document{value = #onedata_user{groups = Groups} = User} = UserDoc,

    Privileges = privileges:group_admin(),
    Group = #user_group{name = Name, users = [{UserId, Privileges}]},
    {ok, GroupId} = user_group:save(#document{key = UUID, value = Group}),
    UserNew = User#onedata_user{groups = [GroupId | Groups]},
    onedata_user:save(UserDoc#document{value = UserNew}),

    {ok, GroupId}.


%%--------------------------------------------------------------------
%% @doc Creates a Space for a user or group with implicit UUID.
%% Throws exception when call to the datastore fails, or given member doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_space_with_uuid({user | group, Id :: binary()}, Name :: binary(), UUID :: binary()) ->
    {ok, SpaceId :: binary()} | no_return().
create_space_with_uuid(Member, Name, UUID) ->
    create_space_with_provider(Member, Name, [], [], UUID).


%%--------------------------------------------------------------------
%% @doc Creates a Space for a user or group with implicit UUID, by a provider that will support it.
%% Throws exception when call to the datastore fails, or token/member_from_token doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_space_with_uuid({provider, ProviderId :: binary()}, Name :: binary(),
    Token :: binary(), Size :: pos_integer(), UUID :: binary()) ->
    {ok, SpaceId :: binary()}.
create_space_with_uuid({provider, ProviderId}, Name, Token, Size, UUID) ->
    {ok, Macaroon} = macaroon:deserialize(Token),
    {ok, Member} = token_logic:consume(Macaroon),
    create_space_with_provider(Member, Name, [ProviderId], [{ProviderId, Size}], UUID).


%%--------------------------------------------------------------------
%% @doc Creates a Space for a user or a group with implicit UUID, with a preexisting provider.
%% Throws exception when call to the datastore fails, or user/group doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create_space_with_provider({user | group, Id :: binary()}, Name :: binary(), Providers :: [binary()],
    Size :: [{Provider :: binary(), ProvidedSize :: pos_integer()}], UUID :: binary()) ->
    {ok, SpaceId :: binary()}.
create_space_with_provider({user, UserId}, Name, Providers, Size, UUID) ->
    {ok, UserDoc} = onedata_user:get(UserId),
    #document{value = #onedata_user{spaces = Spaces} = User} = UserDoc,

    Privileges = privileges:space_admin(),
    Space = #space{name = Name, size = Size, providers = Providers, users = [{UserId, Privileges}]},
    {ok, SpaceId} = space:save(#document{key = UUID, value = Space}),
    UserNew = User#onedata_user{spaces = [SpaceId | Spaces]},
    onedata_user:save(UserDoc#document{value = UserNew}),

    {ok, SpaceId};
create_space_with_provider({group, GroupId}, Name, Providers, Size, UUID) ->
    {ok, GroupDoc} = user_group:get(GroupId),
    #document{value = #user_group{spaces = Spaces} = Group} = GroupDoc,

    Privileges = privileges:space_admin(),
    Space = #space{name = Name, size = Size, providers = Providers, groups = [{GroupId, Privileges}]},
    {ok, SpaceId} = space:save(#document{key = UUID, value = Space}),
    GroupNew = Group#user_group{spaces = [SpaceId | Spaces]},
    user_group:save(GroupDoc#document{value = GroupNew}),

    {ok, SpaceId}.
