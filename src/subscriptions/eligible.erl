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

-spec providers(Doc :: datastore:document(), Model :: atom())
        -> [ProviderID :: term()].
providers(Doc, space) ->
    #document{value = Value, key = SpaceID} = Doc,
    #space{providers = SpaceProviders} = Value,

    {ok, [{users, Users}]} = space_logic:get_effective_users(SpaceID),
    UserProviders = through_users(Users),

    SpaceProviders ++ UserProviders;

providers(Doc, user_group) ->
    #document{value = #user_group{users = UsersWithPriv, spaces = Spaces}} = Doc,
    {Users, _} = lists:unzip(UsersWithPriv),
    ProvidesThroughUsers = through_users(Users),

    ProvidesThroughSpaces = lists:flatmap(fun(SpaceID) ->
        {ok, #document{value = #space{providers = Providers}}} = space:get(SpaceID),
        Providers
    end, Spaces),

    ProvidesThroughUsers ++ ProvidesThroughSpaces;

providers(Doc, onedata_user) ->
    through_users([Doc#document.key]);

providers(_Doc, _Type) ->
    [].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns providers eligible for receiving given update.
%% @end
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns providers who are eligible thanks to users declared in
%% current subscription.
%% @end
%%--------------------------------------------------------------------

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

