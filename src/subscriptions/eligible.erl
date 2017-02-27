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
    #document{value = #od_space{
        users = Users, eff_users = EffUsers, providers = ProvidersSupports
    }} = Doc,
    SpaceProviders = maps:keys(ProvidersSupports),
    SpaceEffUsers = ordsets:union(maps:keys(Users), maps:keys(EffUsers)),
    SpaceProviders ++ through_users(SpaceEffUsers);

% For share, the eligible providers are the same as for its parent space.
providers(Doc, od_share) ->
    #document{value = #od_share{space = ParentId}} = Doc,
    case ParentId of
        undefined ->
            [];
        _ ->
            {ok, ParentDoc} = od_space:get(ParentId),
            providers(ParentDoc, od_space)
    end;

providers(Doc, od_group) ->
    #document{value = #od_group{users = Users, eff_users = EffUsers}} = Doc,
    through_users(ordsets:union(maps:keys(Users), maps:keys(EffUsers)));

providers(Doc, od_user) ->
    through_users([Doc#document.key]);

providers(Doc, od_provider) ->
    [Doc#document.key];

providers(Doc, od_handle_service) ->
    #document{value = #od_handle_service{users = Users, eff_users = EffUsers}} = Doc,
    through_users(ordsets:union(maps:keys(Users), maps:keys(EffUsers)));

providers(Doc, od_handle) ->
    #document{value = #od_handle{users = Users, eff_users = EffUsers}} = Doc,
    through_users(ordsets:union(maps:keys(Users), maps:keys(EffUsers)));

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
