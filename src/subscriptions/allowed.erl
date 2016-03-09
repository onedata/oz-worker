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
-module(allowed).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([providers/3]).

%%--------------------------------------------------------------------
%% @doc
%% Returns providers eligible for receiving given update.
%% @end
%%--------------------------------------------------------------------

-spec providers(Doc :: datastore:document(), Model :: atom(),
    fun((ProviderID :: term())-> boolean())) -> [ProviderID :: term()].

providers(Doc, Model, Filter) ->
    lists:filter(Filter, lists:usort(providers(Doc, Model))).

providers(Doc, space) ->
    #document{value = Value, key = SpaceID} = Doc,
    #space{providers = SpaceProviders} = Value,

    {ok, [{users, Users}]} = space_logic:get_effective_users(SpaceID),
    UserProviders = get_providers(Users),

    SpaceProviders ++ UserProviders;

providers(Doc, user_group) ->
    #document{value = #user_group{users = UsersWithPriv, spaces = Spaces}} = Doc,
    {Users, _} = lists:unzip(UsersWithPriv),
    ProvidesThroughUsers = get_providers(Users),

    ProvidesThroughSpaces = lists:flatmap(fun(SpaceID) ->
        {ok, #document{value = #space{providers = Providers}}} = space:get(SpaceID),
        Providers
    end, Spaces),

    ProvidesThroughUsers ++ ProvidesThroughSpaces;

providers(Doc, onedata_user) ->
    get_providers([Doc#document.key]);

providers(_Doc, _Type) ->
    [].

get_providers(UserIDs) ->
    UsersSet = ordsets:from_list(UserIDs),
    lists:filtermap(fun(SubDoc) ->
        #document{value = #provider_subscription{users = Users,
            provider = ProviderID}} = SubDoc,
        case ordsets:is_disjoint(UsersSet, ordsets:from_list(Users)) of
            false -> {true, ProviderID};
            true -> false
        end
    end, subscriptions:subscriptions()).

