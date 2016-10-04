%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module resolves which providers are eligible for receiving updates.
%%% @end
%%%-------------------------------------------------------------------
-module(eligible).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([providers/2]).

%%--------------------------------------------------------------------
%% @doc
%% Returns providers eligible for receiving given update.
%% @end
%%--------------------------------------------------------------------
-spec providers(Doc :: datastore:document(), Model :: subscriptions:model())
        -> [ProviderID :: binary()].
providers(Doc, space) ->
    #document{value = #od_space{users = SpaceUserTuples, groups = GroupTuples,
        providers_supports = ProvidersSupports}} = Doc,
    {SpaceProviders, _} = lists:unzip(ProvidersSupports),

    GroupUsersSets = lists:flatmap(fun({GroupId, _}) ->
        {ok, #document{value = #od_group{users = GroupUserTuples}}} =
            od_group:get(GroupId),
        {GroupUsers, _} = lists:unzip(GroupUserTuples),
        GroupUsers
    end, GroupTuples),

    {SpaceUsers, _} = lists:unzip(SpaceUserTuples),
    SpaceUsersSet = ordsets:from_list(SpaceUsers),

    SpaceProviders ++ through_users(SpaceUsersSet ++ GroupUsersSets);

% For share, the eligible providers are the same as for its parent space.
providers(Doc, share) ->
    #document{value = #od_share{parent_space = ParentId}} = Doc,
    {ok, ParentDoc} = od_space:get(ParentId),
    providers(ParentDoc, space);

providers(Doc, user_group) ->
    #document{
        value = #od_group{
            users = UsersWithPrivileges,
            eff_users = EUsersWithPrivileges,
            eff_children = EGroups}} = Doc,
    {Users, _} = lists:unzip(UsersWithPrivileges),
    {EUsers, _} = lists:unzip(EUsersWithPrivileges),
    AncestorsUsers = lists:foldl(
        fun(AncestorID, Acc) ->
            case od_group:get(AncestorID) of
                {ok, #document{
                    value = #od_group{eff_users = AncUsersAndPerms}}} ->
                    {AncestorUsers, _} = lists:unzip(AncUsersAndPerms),
                    Acc ++ AncestorUsers;
                {error, Reason} ->
                    ?warning("Refferenced group ~p not found due to ~p",
                        [AncestorID, Reason]),
                    Acc
            end
        end, [], EGroups -- [Doc#document.key]),
    through_users(Users ++ EUsers ++ AncestorsUsers);

providers(Doc, onedata_user) ->
    through_users([Doc#document.key]);

providers(Doc, provider) ->
    [Doc#document.key];

providers(Doc, handle) ->
    {ok, [{users, Users}]} = handle_logic:get_effective_users(Doc#document.key),
    through_users(Users);

providers(Doc, handle_service) ->
    {ok, [{users, Users}]} = handle_service_logic:get_effective_users(
        Doc#document.key
    ),
    through_users(Users);

providers(_Doc, _Type) ->
    [].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns providers who are eligible thanks to users declared in
%% current subscription.
%% @end
%%--------------------------------------------------------------------
-spec through_users(UserIDs :: [binary()]) -> ProviderIDs :: [binary()].
through_users(UserIDs) ->
    UsersSet = ordsets:from_list(UserIDs),
    lists:filtermap(fun(SubDoc) ->
        #document{value = #provider_subscription{users = Users,
            provider = ProviderID}} = SubDoc,
        case ordsets:is_disjoint(UsersSet, ordsets:from_list(Users)) of
            false -> {true, ProviderID};
            true -> false
        end
    end, subscriptions:all()).

