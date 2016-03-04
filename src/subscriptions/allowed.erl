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

providers(Doc, space, Filter) ->
    #document{value = Value, key = SpaceID} = Doc,
    #space{providers = SpaceProviders} = Value,
    {ok, UserSubscriptions} = user_subscription:non_expired(),

    UserProviders = lists:filtermap(fun(#document{value = Value}) ->
        #user_subscription{user = User, provider = Provider} = Value,
        case Filter(Provider) of
            false -> false;
            true ->
                {ok, Set} = space_logic:get_effective_privileges(SpaceID, User),
                case ordsets:size(Set) > 0 of
                    true -> {true, Provider};
                    false -> false
                end
        end
    end, UserSubscriptions),
    SpaceProviders ++ UserProviders;

providers(_Doc, user_group, _Filter) ->
    [];

providers(Doc, onedata_user, _Filter) ->
    Now = erlang:system_time(seconds),
    #document{key = UserID} = Doc,
    case user_subscription:get(UserID) of
        {ok, #document{value = #user_subscription{
            expires = E, provider = P}}} when Now < E -> [P];
        _ -> []
    end;

providers(_Doc, _Type, _ProviderFilterFun) ->
    [].

