%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements data_backend_behaviour and is used to synchronize
%%% the `provider` model used in Ember application.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_data_backend).
-author("Lukasz Opiola").
-behaviour(data_backend_behaviour).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% data_backend_behaviour callbacks
-export([init/0]).
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).
%% API
-export([provider_record/3]).


%%%===================================================================
%%% data_backend_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback init/0.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find/2.
%% @end
%%--------------------------------------------------------------------
-spec find(ResourceType :: binary(), Id :: binary()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find(<<"provider">>, ProviderId) ->
    UserId = g_session:get_user_id(),
    {ok, DefaultProvider} = user_logic:get_default_provider(UserId),
    {ok, UserSpaces} = user_logic:get_spaces(UserId),
    SpaceIds = proplists:get_value(spaces, UserSpaces),
    Res = provider_record(ProviderId, DefaultProvider, SpaceIds),
    {ok, Res}.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find_all/1.
%% @end
%%--------------------------------------------------------------------
-spec find_all(ResourceType :: binary()) ->
    {ok, [proplists:proplist()]} | gui_error:error_result().
find_all(<<"provider">>) ->
    UserId = g_session:get_user_id(),
    {ok, [{providers, ProviderIds}]} = user_logic:get_providers(UserId),
    {ok, DefaultProvider} = user_logic:get_default_provider(UserId),
    {ok, UserSpaces} = user_logic:get_spaces(UserId),
    SpaceIds = proplists:get_value(spaces, UserSpaces),
    Res = lists:map(
        fun(ProviderId) ->
            provider_record(ProviderId, DefaultProvider, SpaceIds)
        end, ProviderIds),
    {ok, Res}.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find_query/2.
%% @end
%%--------------------------------------------------------------------
-spec find_query(ResourceType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find_query(<<"provider">>, _Data) ->
    gui_error:report_error(<<"Not iplemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback create_record/2.
%% @end
%%--------------------------------------------------------------------
-spec create_record(RsrcType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
create_record(<<"provider">>, _Data) ->
    gui_error:report_error(<<"Not iplemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback update_record/3.
%% @end
%%--------------------------------------------------------------------
-spec update_record(RsrcType :: binary(), Id :: binary(),
    Data :: proplists:proplist()) ->
    ok | gui_error:error_result().
update_record(<<"provider">>, ProviderId, Data) ->
    UserId = g_session:get_user_id(),
    IsDefault = proplists:get_value(<<"isDefault">>, Data),
    user_logic:set_provider_as_default(UserId, ProviderId, IsDefault),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback delete_record/2.
%% @end
%%--------------------------------------------------------------------
-spec delete_record(RsrcType :: binary(), Id :: binary()) ->
    ok | gui_error:error_result().
delete_record(<<"provider">>, _Id) ->
    gui_error:report_error(<<"Not iplemented">>).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns a client-compliant space record.
%% @end
%%--------------------------------------------------------------------
-spec provider_record(ProviderId :: binary(), DefaultProvider :: binary(),
    UserSpaces :: [binary()]) -> proplists:proplist().
provider_record(ProviderId, DefaultProvider, UserSpaces) ->
    {ok, ProviderData} = provider_logic:get_data(ProviderId),
    Name = proplists:get_value(clientName, ProviderData),
    Latitude = proplists:get_value(latitude, ProviderData, 0.0),
    Longitude = proplists:get_value(longitude, ProviderData, 0.0),
    IsWorking = provider_logic:check_provider_connectivity(ProviderId),
    {ok, [{spaces, Spaces}]} = provider_logic:get_spaces(ProviderId),
    SpacesToDisplay = lists:filter(
        fun(Space) ->
            lists:member(Space, UserSpaces)
        end, Spaces),
    [
        {<<"id">>, ProviderId},
        {<<"name">>, Name},
        {<<"isDefault">>, ProviderId =:= DefaultProvider},
        {<<"isWorking">>, IsWorking},
        {<<"spaces">>, SpacesToDisplay},
        {<<"latitude">>, Latitude},
        {<<"longitude">>, Longitude}
    ].