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
        -> [ProviderId :: binary()].
providers(Doc, od_space) ->
    #document{value = #od_space{users = UserPrivileges, groups = GroupPrivileges,
        providers = ProvidersSupports}} = Doc,
    SpaceProviders = maps:keys(ProvidersSupports),

    GroupUsersSets = lists:flatmap(fun({GroupId, _}) ->
        {ok, #document{value = #od_group{users = GroupUserTuples}}} =
            od_group:get(GroupId),
        maps:keys(GroupUserTuples)
    end, maps:to_list(GroupPrivileges)),

    SpaceUsers = maps:keys(UserPrivileges),
    SpaceUsersSet = ordsets:from_list(SpaceUsers),

    SpaceProviders ++ through_users(SpaceUsersSet ++ GroupUsersSets);

% For share, the eligible providers are the same as for its parent space.
providers(Doc, od_share) ->
    #document{value = #od_share{space = ParentId}} = Doc,
    {ok, ParentDoc} = od_space:get(ParentId),
    providers(ParentDoc, od_space);

providers(Doc, od_group) ->
    #document{
        value = #od_group{
            users = UsersWithPrivileges,
            eff_users = EUsersWithPrivileges,
            eff_children = EChildrenWithPrivileges
        }} = Doc,
    EGroups = maps:keys(EChildrenWithPrivileges),
    Users = maps:keys(UsersWithPrivileges),
    EUsers = maps:keys(EUsersWithPrivileges),
    AncestorsUsers = lists:foldl(
        fun(AncestorId, Acc) ->
            case od_group:get(AncestorId) of
                {ok, #document{
                    value = #od_group{eff_users = AncUsersAndPerms}}} ->
                    AncestorUsers = maps:keys(AncUsersAndPerms),
                    Acc ++ AncestorUsers;
                {error, Reason} ->
                    ?warning("Referenced group ~p not found due to ~p",
                        [AncestorId, Reason]),
                    Acc
            end
        end, [], EGroups -- [Doc#document.key]),
    through_users(Users ++ EUsers ++ AncestorsUsers);

providers(Doc, od_user) ->
    through_users([Doc#document.key]);

providers(Doc, od_provider) ->
    [Doc#document.key];

providers(Doc, od_handle_service) ->
    #document{value = #od_handle_service{eff_users = EffUsers}} = Doc,
    through_users(maps:keys(EffUsers));

providers(Doc, od_handle) ->
    #document{value = #od_handle{eff_users = EffUsers}} = Doc,
    through_users(maps:keys(EffUsers));

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
-spec through_users(UserIds :: [binary()]) -> ProviderIds :: [binary()].
through_users(UserIds) ->
    UsersSet = ordsets:from_list(UserIds),
    lists:filtermap(fun(SubDoc) ->
        #document{value = #provider_subscription{users = Users,
            provider = ProviderId}} = SubDoc,
        case ordsets:is_disjoint(UsersSet, ordsets:from_list(Users)) of
            false -> {true, ProviderId};
            true -> false
        end
    end, subscriptions:all()).
