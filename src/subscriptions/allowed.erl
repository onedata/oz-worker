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
    #space{providers = ProvidersWithSupport} = Value,
    {SpaceProviders, _} = lists:unzip(ProvidersWithSupport),
    UserProviders = [], % todo
    SpaceProviders ++ UserProviders;

providers(_Doc, user_group, _Filter) ->
    [];

providers(Doc, onedata_user, _Filter) ->
    [];

providers(_Doc, _Type, _ProviderFilterFun) ->
    [].

