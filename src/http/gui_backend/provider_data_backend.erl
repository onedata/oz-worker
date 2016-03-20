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

%% API
-export([init/0]).
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback init/0.
%% @end
%%--------------------------------------------------------------------
init() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find/2.
%% @end
%%--------------------------------------------------------------------
find(<<"provider">>, ProviderIds) ->
    UserId = g_session:get_user_id(),
    Res = lists:map(
        fun(ProviderId) ->
            {ok, ProviderData} = provider_logic:get_data(ProviderId),
            Name = proplists:get_value(clientName, ProviderData),
            IsWorking = provider_logic:check_provider_connectivity(ProviderId),
            {ok, [{spaces, Spaces}]} = provider_logic:get_spaces(ProviderId),
            {ok, #document{
                value = #onedata_user{
                    default_provider = DefaultProvider
                }
            }} = user_logic:get_user_doc(UserId),
            [
                {<<"id">>, ProviderId},
                {<<"name">>, Name},
                {<<"isDefault">>, ProviderId =:= DefaultProvider},
                {<<"isWorking">>, IsWorking},
                {<<"spaces">>, Spaces}
            ]
        end, ProviderIds),
    {ok, Res}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find_query/2.
%% @end
%%--------------------------------------------------------------------
find_query(<<"provider">>, _Data) ->
    {error, <<"Not implemented">>}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find_all/1.
%% @end
%%--------------------------------------------------------------------
find_all(<<"provider">>) ->
    UserId = g_session:get_user_id(),
    {ok, [{providers, ProviderIds}]} = user_logic:get_providers(UserId),
    {ok, _Res} = find(<<"provider">>, ProviderIds).


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback create_record/2.
%% @end
%%--------------------------------------------------------------------
create_record(<<"provider">>, _Data) ->
    {error, <<"Not implemented">>}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback update_record/3.
%% @end
%%--------------------------------------------------------------------
update_record(<<"provider">>, ProviderId, Data) ->
    UserId = g_session:get_user_id(),
    IsDefault = proplists:get_value(<<"isDefault">>, Data),
    case IsDefault of
        true ->
            user_logic:set_default_provider(UserId, ProviderId);
        false ->
            ok
    end,
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback delete_record/2.
%% @end
%%--------------------------------------------------------------------
delete_record(<<"provider">>, _Id) ->
    {error, <<"Not implemented">>}.
